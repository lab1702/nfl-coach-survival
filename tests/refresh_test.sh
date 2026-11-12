#!/usr/bin/env bash
# =============================================================================
# tests/refresh_test.sh — run docker/refresh.sh against a fake clock
# =============================================================================
#
# Run from anywhere:   bash tests/refresh_test.sh
#
# docker/refresh.sh decides when the report is rebuilt, how a failed run is
# retried, and what the site shows meanwhile. This script runs the real
# refresh.sh with three commands replaced by stand-ins on PATH:
#   * date    reads the time from a file instead of the system clock
#   * sleep   records how long it was asked to wait, moves that clock forward
#             and returns at once; after a set number of waits it stops
#             refresh.sh the way `docker stop` does (SIGTERM)
#   * Rscript takes 30 seconds of fake time and succeeds or fails as the
#             test case says, writing a report that names the run
# So days of schedule are checked in under a second, with no Docker, no R
# and no network. It needs bash and GNU date, as refresh.sh itself does.
# =============================================================================
set -uo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd) || exit 1
WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT
STATE=$WORK/state
REAL_DATE=$(command -v date)
REAL_SLEEP=$(command -v sleep)
export STATE REAL_DATE REAL_SLEEP

mkdir -p "$WORK/bin"

cat > "$WORK/bin/date" <<'EOF'
#!/usr/bin/env bash
# `date` at the fake time: "today" and "tomorrow" are relative to it, and a
# call without -d reports it.
now=$(cat "$STATE/now")
args=(); has_d=0
while (( $# )); do
  if [[ $1 == -d ]]; then
    shift; has_d=1
    case "$1" in
      today\ *)    args+=(-d "$("$REAL_DATE" -d "@$now" +%F) ${1#today }") ;;
      tomorrow\ *) args+=(-d "$("$REAL_DATE" -d "@$(( now + 86400 ))" +%F) ${1#tomorrow }") ;;
      *)           args+=(-d "$1") ;;
    esac
  else
    args+=("$1")
  fi
  shift
done
(( has_d )) || args=(-d "@$now" "${args[@]}")
exec "$REAL_DATE" "${args[@]}"
EOF

cat > "$WORK/bin/sleep" <<'EOF'
#!/usr/bin/env bash
# Record the wait and what the site shows during it, then jump the clock.
if   grep -q 'content="generating"' "$SITE/index.html" 2>/dev/null; then page=placeholder
elif grep -q 'content="failed"' "$SITE/index.html" 2>/dev/null;     then page=failed
else page=$(cat "$SITE/index.html" 2>/dev/null || echo missing)
fi
echo "$1 $page" >> "$STATE/waits"
echo $(( $(cat "$STATE/now") + $1 )) > "$STATE/now"
if (( $(wc -l < "$STATE/waits") >= $(cat "$STATE/max_waits") )); then
  kill -TERM "$PPID"            # as `docker stop` would
  exec "$REAL_SLEEP" 5          # refresh.sh kills this on its way out
fi
EOF

cat > "$WORK/bin/Rscript" <<'EOF'
#!/usr/bin/env bash
# One run of the analysis: 30 seconds, then the result the test case lists
# for this run ("ok" if the list has run out).
n=$(( $(cat "$STATE/runs") + 1 )); echo "$n" > "$STATE/runs"
echo $(( $(cat "$STATE/now") + 30 )) > "$STATE/now"
result=$(sed -n "${n}p" "$STATE/results")
echo "fake run $n: ${result:-ok}"
[[ ${result:-ok} == ok ]] || exit 1
echo "report $n" > output/coach_tenure_report.html
echo "warning from run $n" > output/run_warnings.txt
EOF
chmod +x "$WORK/bin/"*

failed=0
check() {   # name, actual, expected
  if [[ $2 == "$3" ]]; then echo "  ok    $1"
  else echo "  FAIL  $1"; echo "        expected: $3"; echo "        got:      $2"; failed=$(( failed + 1 ))
  fi
}

# start_case "start time (UTC)" "results of the runs" max_waits [VAR=value ...]
# Runs refresh.sh until it has waited max_waits times; leaves its exit code
# in $rc, its output in $WORK/log and the waits in $STATE/waits.
start_case() {
  local start=$1 results=$2 max_waits=$3; shift 3
  rm -rf "$WORK/app" "$WORK/site" "$STATE"
  mkdir -p "$WORK/app/docker" "$WORK/app/output" "$WORK/site" "$STATE"
  cp "$ROOT"/docker/*.html "$WORK/app/docker/"
  TZ=UTC "$REAL_DATE" -d "$start" +%s > "$STATE/now"
  echo 0 > "$STATE/runs"
  : > "$STATE/waits"
  tr ' ' '\n' <<< "$results" > "$STATE/results"
  echo "$max_waits" > "$STATE/max_waits"
  env PATH="$WORK/bin:$PATH" TZ=UTC SITE="$WORK/site" APP_DIR="$WORK/app" "$@" \
    timeout 60 bash "$ROOT/docker/refresh.sh" > "$WORK/log" 2>&1
  rc=$?
}
waits() { tr '\n' ',' < "$STATE/waits" | sed 's/,$//'; }

echo "Daily schedule, retries and a failed run keeping the last report"
# 09:00 start (ok), 10:00 daily run fails, two retries fail, then the next
# two daily runs succeed.
start_case "2026-01-05 09:00:00" "ok fail fail fail ok ok" 6 \
  REFRESH_TIME=10:00 RETRY_MINUTES=60 MAX_RETRIES=2
check "waits and the page shown during each" "$(waits)" \
  "3570 report 1,3600 report 1,3600 report 1,79110 report 1,86370 report 5,86370 report 6"
check "stops on SIGTERM" "$rc" "143"
check "retries are counted in the log" "$(grep -c 'Retry [12] of 2' "$WORK/log")" "2"
check "the run's warnings are printed" "$(grep -c '^  - warning from run 1$' "$WORK/log")" "1"
check "a failed run's log is shown" "$(grep -c '^fake run 2: fail$' "$WORK/log")" "1"

echo "Before the first report: placeholder, then the failed page, then the report"
start_case "2026-01-05 12:00:00" "fail ok" 2 REFRESH_TIME=10:00 RETRY_MINUTES=60 MAX_RETRIES=6
check "waits and the page shown during each" "$(waits)" "3600 failed,75540 report 2"

echo "Retries stop when the next daily run comes first"
start_case "2026-01-05 09:30:00" "fail ok" 2 REFRESH_TIME=10:00 RETRY_MINUTES=60 MAX_RETRIES=6
check "waits for the daily run instead" "$(waits)" "1770 failed,86370 report 2"

echo "No retries when MAX_RETRIES is 0"
start_case "2026-01-05 12:00:00" "fail" 1 REFRESH_TIME=10:00 RETRY_MINUTES=60 MAX_RETRIES=0
check "waits for the next daily run" "$(waits)" "79170 failed"

echo "Settings"
start_case "2026-01-05 12:00:00" "fail" 1 REFRESH_TIME=10:00 RETRY_MINUTES=08 MAX_RETRIES=09
check "leading zeros are read as decimal" "$(waits)" "480 failed"
start_case "2026-01-05 23:50:00" "ok" 1 REFRESH_TIME=0:05
check "a time early tomorrow" "$(waits)" "870 report 1"
for bad in "REFRESH_TIME=25:00" "REFRESH_TIME=+1 hour" "REFRESH_TIME=10:60" \
           "RETRY_MINUTES=0" "RETRY_MINUTES=soon" "MAX_RETRIES=-1"; do
  start_case "2026-01-05 12:00:00" "ok" 1 "$bad"
  check "rejects $bad without running" "$rc $(cat "$STATE/runs")" "1 0"
done

echo
if (( failed == 0 )); then echo "All refresh checks passed."; else echo "$failed check(s) failed."; fi
exit $(( failed > 0 ))
