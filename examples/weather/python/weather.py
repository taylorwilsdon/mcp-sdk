"""Weather MCP server: current conditions for a city from Open-Meteo.

Run over stdio with `uv run weather.py`, or over Streamable HTTP with
`uv run weather.py --http 8000` (served at http://127.0.0.1:8000/mcp).
"""

import argparse
import os
from collections.abc import AsyncIterator
from typing import Annotated, Any, Literal

import httpx
from fastmcp import Context, FastMCP
from fastmcp.exceptions import ToolError
from fastmcp.server.lifespan import lifespan
from fastmcp.tools import ToolResult
from pydantic import BaseModel, Field

GEOCODING_URL = os.environ.get(
    "OPEN_METEO_GEOCODING_URL", "http://geocoding-api.open-meteo.com"
)
FORECAST_URL = os.environ.get("OPEN_METEO_FORECAST_URL", "http://api.open-meteo.com")

CURRENT_FIELDS = (
    "temperature_2m,relative_humidity_2m,apparent_temperature,"
    "weather_code,wind_speed_10m"
)

WMO_CONDITIONS = {
    0: "Clear sky",
    1: "Mainly clear",
    2: "Partly cloudy",
    3: "Overcast",
    45: "Fog",
    48: "Fog",
    51: "Drizzle",
    53: "Drizzle",
    55: "Drizzle",
    56: "Freezing drizzle",
    57: "Freezing drizzle",
    61: "Rain",
    63: "Rain",
    65: "Rain",
    66: "Freezing rain",
    67: "Freezing rain",
    71: "Snow",
    73: "Snow",
    75: "Snow",
    77: "Snow grains",
    80: "Rain showers",
    81: "Rain showers",
    82: "Rain showers",
    85: "Snow showers",
    86: "Snow showers",
    95: "Thunderstorm",
    96: "Thunderstorm with hail",
    99: "Thunderstorm with hail",
}

Units = Literal["celsius", "fahrenheit"]


class Weather(BaseModel):
    city: str
    country: str
    latitude: float
    longitude: float
    temperature: float
    apparent_temperature: float
    humidity: float
    wind_speed_kmh: float
    conditions: str
    units: Units


@lifespan
async def http_client(server: FastMCP) -> AsyncIterator[dict[str, Any]]:
    """Share one pooled HTTP client across all tool calls."""
    async with httpx.AsyncClient() as client:
        yield {"http": client}


mcp = FastMCP(
    "weather",
    instructions="Use get_weather to look up current conditions for a city.",
    version="1.0.0",
    lifespan=http_client,
)


async def fetch_json(
    client: httpx.AsyncClient, url: str, params: dict[str, Any]
) -> dict[str, Any]:
    """GET a JSON document, turning upstream failures into tool errors."""
    try:
        response = await client.get(url, params=params)
        response.raise_for_status()
    except httpx.HTTPStatusError as exc:
        raise ToolError(f"Open-Meteo returned HTTP {exc.response.status_code}") from exc
    except httpx.HTTPError as exc:
        raise ToolError(f"Open-Meteo request failed: {type(exc).__name__}") from exc
    return response.json()


@mcp.tool(
    description="Current weather for a city, from Open-Meteo",
    output_schema=Weather.model_json_schema(),
    annotations={"readOnlyHint": True},
)
async def get_weather(
    city: Annotated[str, Field(description="City name, e.g. Berlin")],
    ctx: Context,
    units: Units = "celsius",
) -> ToolResult:
    client: httpx.AsyncClient = ctx.lifespan_context["http"]

    geo = await fetch_json(
        client,
        f"{GEOCODING_URL}/v1/search",
        {"name": city, "count": 1, "language": "en", "format": "json"},
    )
    if not geo.get("results"):
        raise ToolError(f"No location found for '{city}'")
    place = geo["results"][0]

    forecast = await fetch_json(
        client,
        f"{FORECAST_URL}/v1/forecast",
        {
            "latitude": place["latitude"],
            "longitude": place["longitude"],
            "current": CURRENT_FIELDS,
            "temperature_unit": units,
            "wind_speed_unit": "kmh",
        },
    )
    current = forecast["current"]

    weather = Weather(
        city=place["name"],
        country=place.get("country", ""),
        latitude=place["latitude"],
        longitude=place["longitude"],
        temperature=current["temperature_2m"],
        apparent_temperature=current["apparent_temperature"],
        humidity=current["relative_humidity_2m"],
        wind_speed_kmh=current["wind_speed_10m"],
        conditions=WMO_CONDITIONS.get(current["weather_code"], "Unknown"),
        units=units,
    )
    degree = "°C" if units == "celsius" else "°F"
    summary = (
        f"{weather.city}, {weather.country}: "
        f"{weather.temperature:g}{degree} "
        f"(feels like {weather.apparent_temperature:g}{degree}), "
        f"{weather.conditions}, humidity {weather.humidity:g}%, "
        f"wind {weather.wind_speed_kmh:g} km/h"
    )
    return ToolResult(content=summary, structured_content=weather)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--http",
        type=int,
        metavar="PORT",
        help="serve Streamable HTTP on 127.0.0.1:PORT instead of stdio",
    )
    args = parser.parse_args()

    if args.http is None:
        mcp.run(show_banner=False)
    else:
        mcp.run(
            transport="http",
            show_banner=False,
            host="127.0.0.1",
            port=args.http,
            path="/mcp",
            stateless_http=True,
            json_response=True,
        )


if __name__ == "__main__":
    main()
