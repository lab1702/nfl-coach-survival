#!/usr/bin/env bash
# Runs the analysis now, then once a day at REFRESH_TIME (in the container's
# TZ). A successful run replaces /site/index.html atomically; a failed run
# leaves the previous report in place.
set -euo pipefail

SITE=/site
REFRESH_TIME="${REFRESH_TIME:-10:00}"
cd /app

# Something to serve before the first run finishes.
if [[ ! -f "$SITE/index.html" ]]; then
  cp docker/placeholder.html "$SITE/index.html"
fi

refresh() {
  echo "[$(date -Is)] Running coach_tenure.R"
  if Rscript coach_tenure.R > output/run_log.txt 2>&1; then
    cp output/coach_tenure_report.html "$SITE/.index.html.tmp"
    mv "$SITE/.index.html.tmp" "$SITE/index.html"
    echo "[$(date -Is)] Published the report"
  else
    echo "[$(date -Is)] Run failed; keeping the previous report. Last lines of the log:"
    tail -n 20 output/run_log.txt
  fi
}

# Validate REFRESH_TIME up front so a typo fails loudly instead of looping.
if ! date -d "today $REFRESH_TIME" > /dev/null 2>&1; then
  echo "REFRESH_TIME must look like HH:MM (got '$REFRESH_TIME')" >&2
  exit 1
fi

refresh
while true; do
  now=$(date +%s)
  next=$(date -d "today $REFRESH_TIME" +%s)
  if (( next <= now )); then
    next=$(date -d "tomorrow $REFRESH_TIME" +%s)
  fi
  echo "[$(date -Is)] Next refresh at $(date -d "@$next" -Is)"
  sleep $(( next - now ))
  refresh
done
