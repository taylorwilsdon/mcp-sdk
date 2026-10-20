#!/usr/bin/env bash
# Run the official conformance suite against this SDK, as a server and as a
# client, for each revision it implements, each at its own wire version.
# Scenarios a revision requires decide the exit code; those listed in
# baseline-<revision>.yml are expected to fail. Writes the suite's results
# and a markdown summary to $OUT (default _conformance).
#
#   examples/conformance/run.sh
#   DUNE="opam exec -- dune" examples/conformance/run.sh
#
# Needs Node 22 or newer.
set -euo pipefail

SUITE=@modelcontextprotocol/conformance@0.2.0-alpha.12
REVISIONS=(2025-11-25 2026-07-28)
PORT=${PORT:-3917}
DUNE=${DUNE:-dune}
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
OUT=${OUT:-$ROOT/_conformance}

cd "$ROOT"
$DUNE build examples/conformance/conformance.exe examples/conformance/client.exe
SERVER=$ROOT/_build/default/examples/conformance/conformance.exe
CLIENT=$ROOT/_build/default/examples/conformance/client.exe

rm -rf "$OUT"
mkdir -p "$OUT"
"$SERVER" --port "$PORT" &
server_pid=$!
trap 'kill $server_pid 2>/dev/null' EXIT
sleep 1

status=0
for revision in "${REVISIONS[@]}"; do
  baseline=$HERE/baseline-$revision.yml
  npx -y "$SUITE" list --requirements "$revision" >"$OUT/required-$revision.txt"
  npx -y "$SUITE" server --url "http://localhost:$PORT/mcp" --requirements "$revision" \
    --expected-failures "$baseline" -o "$OUT/server-$revision" \
    >"$OUT/server-$revision.log" 2>&1 || status=1
  npx -y "$SUITE" client --command "$CLIENT" --requirements "$revision" \
    --expected-failures "$baseline" -o "$OUT/client-$revision" \
    >"$OUT/client-$revision.log" 2>&1 || status=1
done

# Count the scenarios each revision requires: passed, failing as the
# baseline expects, and failing otherwise. Scenarios the suite runs without
# scoring them are left out, as they are of its verdict.
python3 - "$OUT" "$HERE" "${REVISIONS[@]}" >"$OUT/summary.md" <<'EOF'
import re, sys

out, here, revisions = sys.argv[1], sys.argv[2], sys.argv[3:]

def sections(path, header):
    names, section = {}, None
    for line in open(path):
        if m := re.match(header, line):
            section = m.group(1).lower()
        elif m := re.match(r"^  - (\S+)$", line):
            if section:
                names.setdefault(section, set()).add(m.group(1))
        elif line.strip() and not line.lstrip().startswith("#"):
            section = None
    return names

print("| Revision | Role | Required | Passed | Expected failures | Unexpected failures |")
print("| --- | --- | ---: | ---: | ---: | ---: |")
for revision in revisions:
    required = sections(f"{out}/required-{revision}.txt", r"^(Server|Client) scenarios")
    expected = sections(f"{here}/baseline-{revision}.yml", r"^(server|client):")
    for role in ("server", "client"):
        log = open(f"{out}/{role}-{revision}.log").read()
        results = dict((name, mark) for mark, name in re.findall(r"^([✓✗]) (\S+):", log, re.M))
        names = required.get(role, set())
        passed = sum(results.get(n) == "✓" for n in names)
        known = sum(results.get(n) != "✓" and n in expected.get(role, set()) for n in names)
        print(f"| {revision} | {role} | {len(names)} | {passed} | {known} | {len(names) - passed - known} |")
EOF

cat "$OUT/summary.md"
exit $status
