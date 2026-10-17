#!/usr/bin/env bash
# Runs the analysis now, then once a day at REFRESH_TIME (in the container's
# TZ). A successful run replaces /site/index.html atomically; a failed run
# leaves the previous report in place and is retried every RETRY_MINUTES,
# up to MAX_RETRIES times, before falling back to the daily schedule.
#
# Errors are handled explicitly (no `set -e`): a failed run or publish must
# never kill the scheduler, or the container's restart policy would turn it
# into a tight loop of re-runs and re-downloads.
set -uo pipefail

SITE=/site
REFRESH_TIME="${REFRESH_TIME:-10:00}"
RETRY_MINUTES="${RETRY_MINUTES:-60}"
MAX_RETRIES="${MAX_RETRIES:-6}"
cd /app

log() { echo "[$(date -Is)] $*"; }

# Stop promptly on `docker stop`: children run in the background and the
# script waits on them, so a TERM is handled at once (also when PID 1).
child=0
on_term() {
  log "Stopping"
  (( child > 0 )) && kill -TERM "$child" 2>/dev/null
  exit 143
}
trap on_term TERM INT

run_bg() { "$@" & child=$!; wait "$child"; local rc=$?; child=0; return "$rc"; }

if ! date -d "today $REFRESH_TIME" > /dev/null 2>&1; then
  echo "REFRESH_TIME must look like HH:MM (got '$REFRESH_TIME')" >&2
  exit 1
fi
if ! [[ "$RETRY_MINUTES" =~ ^[0-9]+$ && "$MAX_RETRIES" =~ ^[0-9]+$ ]]; then
  echo "RETRY_MINUTES and MAX_RETRIES must be whole numbers" >&2
  exit 1
fi
# Read as base 10: bash arithmetic treats a leading zero ("08") as octal.
RETRY_MINUTES=$(( 10#$RETRY_MINUTES ))
MAX_RETRIES=$(( 10#$MAX_RETRIES ))
if (( RETRY_MINUTES < 1 )); then
  echo "RETRY_MINUTES must be at least 1" >&2
  exit 1
fi

# Something to serve before the first run finishes.
if [[ ! -f "$SITE/index.html" ]]; then
  cp docker/placeholder.html "$SITE/index.html" || log "Couldn't write the placeholder page"
fi

# Returns 0 if a new report was published.
refresh() {
  log "Running coach_tenure.R"
  if ! run_bg Rscript coach_tenure.R > output/run_log.txt 2>&1; then
    log "Run failed; keeping the previous report. Last lines of the log:"
    tail -n 20 output/run_log.txt
    return 1
  fi
  if cp output/coach_tenure_report.html "$SITE/.index.html.tmp" &&
     mv "$SITE/.index.html.tmp" "$SITE/index.html"; then
    log "Published the report"
    return 0
  fi
  log "Run succeeded but publishing to $SITE failed; keeping the previous report"
  return 1
}

# retries: retries used since the last scheduled run. Each scheduled run
# (startup or the daily slot) gets a fresh budget.
retries=0
while true; do
  failed=0; refresh || failed=1

  now=$(date +%s)
  next=$(date -d "today $REFRESH_TIME" +%s)
  (( next > now )) || next=$(date -d "tomorrow $REFRESH_TIME" +%s)
  retry=$(( now + RETRY_MINUTES * 60 ))
  if (( failed && retries < MAX_RETRIES && retry < next )); then
    retries=$(( retries + 1 ))
    next=$retry
    log "Retry $retries of $MAX_RETRIES at $(date -d "@$next" -Is)"
  else
    retries=0
    log "Next refresh at $(date -d "@$next" -Is)"
  fi
  run_bg sleep $(( next - now ))
done
