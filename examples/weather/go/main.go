// Command weather is the get_weather server on the official Go SDK, the
// native baseline of the benchmark in ../README.md. It mirrors ../ocaml and
// ../python: the same tool, arguments, output schema and upstream calls.
//
//	weather              serve stdio
//	weather -http PORT   serve stateless Streamable HTTP on 127.0.0.1:PORT/mcp
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"

	"github.com/modelcontextprotocol/go-sdk/mcp"
)

type Input struct {
	City  string `json:"city" jsonschema:"City name, e.g. Berlin"`
	Units string `json:"units,omitempty" jsonschema:"celsius or fahrenheit"`
}

type Weather struct {
	City                string  `json:"city"`
	Country             string  `json:"country"`
	Latitude            float64 `json:"latitude"`
	Longitude           float64 `json:"longitude"`
	Temperature         float64 `json:"temperature"`
	ApparentTemperature float64 `json:"apparent_temperature"`
	Humidity            float64 `json:"humidity"`
	WindSpeedKmh        float64 `json:"wind_speed_kmh"`
	Conditions          string  `json:"conditions"`
	Units               string  `json:"units"`
}

// WMO weather interpretation codes.
var conditions = map[int]string{
	0: "Clear sky", 1: "Mainly clear", 2: "Partly cloudy", 3: "Overcast",
	45: "Fog", 48: "Fog", 51: "Drizzle", 53: "Drizzle", 55: "Drizzle",
	56: "Freezing drizzle", 57: "Freezing drizzle", 61: "Rain", 63: "Rain", 65: "Rain",
	66: "Freezing rain", 67: "Freezing rain", 71: "Snow", 73: "Snow", 75: "Snow",
	77: "Snow grains", 80: "Rain showers", 81: "Rain showers", 82: "Rain showers",
	85: "Snow showers", 86: "Snow showers", 95: "Thunderstorm",
	96: "Thunderstorm with hail", 99: "Thunderstorm with hail",
}

func env(name, fallback string) string {
	if v := os.Getenv(name); v != "" {
		return v
	}
	return fallback
}

var (
	geocodingURL = env("OPEN_METEO_GEOCODING_URL", "https://geocoding-api.open-meteo.com")
	forecastURL  = env("OPEN_METEO_FORECAST_URL", "https://api.open-meteo.com")
	// One pooled client for every call, as the other servers keep.
	client = &http.Client{Transport: &http.Transport{MaxIdleConnsPerHost: 256}}
)

func fetch(ctx context.Context, base, path string, query url.Values, into any) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, base+path+"?"+query.Encode(), nil)
	if err != nil {
		return err
	}
	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("Open-Meteo request failed: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("Open-Meteo returned HTTP %d", resp.StatusCode)
	}
	return json.NewDecoder(resp.Body).Decode(into)
}

func getWeather(ctx context.Context, _ *mcp.CallToolRequest, in Input) (*mcp.CallToolResult, Weather, error) {
	units := in.Units
	if units == "" {
		units = "celsius"
	}
	if units != "celsius" && units != "fahrenheit" {
		return nil, Weather{}, errors.New("units must be celsius or fahrenheit")
	}
	var geo struct {
		Results []struct {
			Name      string  `json:"name"`
			Country   string  `json:"country"`
			Latitude  float64 `json:"latitude"`
			Longitude float64 `json:"longitude"`
		} `json:"results"`
	}
	query := url.Values{"name": {in.City}, "count": {"1"}, "language": {"en"}, "format": {"json"}}
	if err := fetch(ctx, geocodingURL, "/v1/search", query, &geo); err != nil {
		return nil, Weather{}, err
	}
	if len(geo.Results) == 0 {
		return nil, Weather{}, fmt.Errorf("No location found for '%s'", in.City)
	}
	place := geo.Results[0]
	var forecast struct {
		Current struct {
			Temperature float64 `json:"temperature_2m"`
			Humidity    float64 `json:"relative_humidity_2m"`
			Apparent    float64 `json:"apparent_temperature"`
			Code        int     `json:"weather_code"`
			Wind        float64 `json:"wind_speed_10m"`
		} `json:"current"`
	}
	query = url.Values{
		"latitude":         {fmt.Sprint(place.Latitude)},
		"longitude":        {fmt.Sprint(place.Longitude)},
		"current":          {"temperature_2m,relative_humidity_2m,apparent_temperature,weather_code,wind_speed_10m"},
		"temperature_unit": {units},
		"wind_speed_unit":  {"kmh"},
	}
	if err := fetch(ctx, forecastURL, "/v1/forecast", query, &forecast); err != nil {
		return nil, Weather{}, err
	}
	c := forecast.Current
	described, ok := conditions[c.Code]
	if !ok {
		described = "Unknown"
	}
	w := Weather{
		City: place.Name, Country: place.Country, Latitude: place.Latitude, Longitude: place.Longitude,
		Temperature: c.Temperature, ApparentTemperature: c.Apparent, Humidity: c.Humidity,
		WindSpeedKmh: c.Wind, Conditions: described, Units: units,
	}
	symbol := "°C"
	if units == "fahrenheit" {
		symbol = "°F"
	}
	summary := fmt.Sprintf("%s, %s: %g%s (feels like %g%s), %s, humidity %g%%, wind %g km/h",
		w.City, w.Country, w.Temperature, symbol, w.ApparentTemperature, symbol, w.Conditions,
		w.Humidity, w.WindSpeedKmh)
	return &mcp.CallToolResult{Content: []mcp.Content{&mcp.TextContent{Text: summary}}}, w, nil
}

func main() {
	port := flag.Int("http", 0, "serve Streamable HTTP on 127.0.0.1:`PORT` instead of stdio")
	flag.Parse()

	server := mcp.NewServer(&mcp.Implementation{Name: "weather", Version: "1.0.0"},
		&mcp.ServerOptions{Instructions: "Use get_weather to look up the current weather for a city."})
	mcp.AddTool(server, &mcp.Tool{
		Name:        "get_weather",
		Description: "Current weather for a city, from Open-Meteo",
		Annotations: &mcp.ToolAnnotations{ReadOnlyHint: true},
	}, getWeather)

	if *port == 0 {
		if err := server.Run(context.Background(), &mcp.StdioTransport{}); err != nil {
			log.Fatal(err)
		}
		return
	}
	handler := mcp.NewStreamableHTTPHandler(func(*http.Request) *mcp.Server { return server },
		&mcp.StreamableHTTPOptions{Stateless: true, JSONResponse: true})
	mux := http.NewServeMux()
	mux.Handle("/mcp", handler)
	log.Fatal(http.ListenAndServe(fmt.Sprintf("127.0.0.1:%d", *port), mux))
}
