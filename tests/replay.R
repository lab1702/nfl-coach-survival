# =============================================================================
# tests/replay.R — replay coach_tenure.R at chosen dates and check the result
# =============================================================================
#
# Run from the project root:   Rscript tests/replay.R
#
# Much of coach_tenure.R depends on the calendar: whether a season is in
# progress, whether its regular season is over, whether the playoff field is
# known, what the hot seat shows in opening week or the offseason. Those
# branches run for real only a few weeks a year, so this script replays them.
#
# Each scenario copies the script and the research files to a temporary
# directory, writes a games file cut off at a date (scores after it blanked,
# later playoff rounds or seasons removed, as the nflverse file would have
# looked that day), runs the whole script there, and checks what it decided.
# Nothing in the project's own data/, plots/ or output/ is touched, and
# nothing is downloaded.
#
# The last scenarios replay the script with mistakes planted in the research
# files (a duplicated row, a mistyped departure, an unknown team or reason,
# an empty cell) and check that each is warned about, that warnings raised
# by R itself are recorded, and that a bad download would not replace the
# cached games file.
#
# It needs data/raw/games.csv (run coach_tenure.R once first) covering the
# 2025 season and September 2026; any later copy works, because every
# scenario cuts the file off at its own date. The expectations are history
# (the 2025 playoff field, the January 2026 coaching changes) plus the
# research files as they stand, so they only need revisiting if a research
# row for those seasons is changed.
#
# Scenarios run in parallel where the platform allows; each takes about as
# long as one run of the script.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

games_file <- "data/raw/games.csv"
if (!file.exists("coach_tenure.R") || !file.exists(games_file)) {
  stop("Run this from the project root, after coach_tenure.R has downloaded ",
       games_file, ".")
}
games_now <- read_csv(games_file, col_types = cols(.default = col_character()))
if (!any(games_now$season == "2026" & games_now$gameday >= "2026-09-14" &
         !is.na(games_now$home_score))) {
  stop(games_file, " must include results through at least 14 September 2026.")
}

playoff_rounds <- c("WC", "DIV", "CON", "SB")

# The games file as of `date`: only seasons up to `last_season`, without the
# playoff rounds of that season that weren't on the schedule yet, and with no
# scores after `date`.
games_as_of <- function(date, last_season, unscheduled = character()) {
  g <- games_now |>
    filter(as.integer(season) <= last_season,
           !(as.integer(season) == last_season & game_type %in% unscheduled))
  later <- as.Date(g$gameday) > as.Date(date)
  for (col in intersect(c("home_score", "away_score", "result", "total", "overtime"), names(g))) {
    g[[col]][later] <- NA
  }
  g
}

# What each run reports back.
state_vars <- c("LAST_COMPLETE_SEASON", "CURRENT_SEASON", "season_in_progress",
                "regular_season_over", "playoff_field_known", "playoff_teams",
                "risk", "tenure", "interim_stints", "coach_seasons", "run_warnings",
                "through_text")

# Run one scenario in its own directory. `games` is the games file to use;
# `setup` may edit the copied files (it gets the directory) before the run.
# `probe` is R code (no single quotes) evaluated in the run's own session
# once the script has finished; its value comes back as state$probe.
replay <- function(games, setup = NULL, probe = "NULL") {
  dir <- tempfile("replay_")
  dir.create(file.path(dir, "data", "raw"), recursive = TRUE)
  file.copy("coach_tenure.R", dir)
  file.copy("data/research", file.path(dir, "data"), recursive = TRUE)
  write_csv(games, file.path(dir, games_file), na = "NA")
  if (!is.null(setup)) setup(dir)

  code <- sprintf('source("coach_tenure.R"); probe <- %s; saveRDS(mget(c(%s, "probe")), "state.rds")',
                  probe, paste0('"', state_vars, '"', collapse = ", "))
  old <- setwd(dir)
  on.exit(setwd(old))
  status <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(code)),
                    stdout = "log.txt", stderr = "log.txt",
                    env = "GAMES_MAX_AGE_HOURS=1e9")   # never download
  report <- "output/coach_tenure_report.html"
  list(status = status,
       state  = if (file.exists("state.rds")) readRDS("state.rds") else NULL,
       report = if (file.exists(report)) paste(readLines(report, warn = FALSE), collapse = "\n") else "",
       files  = list.files(c("output", "plots"), full.names = TRUE),
       warnings_file = if (file.exists("output/run_warnings.txt")) readLines("output/run_warnings.txt") else NULL,
       log    = tail(readLines("log.txt", warn = FALSE), 15))
}

remove_change <- function(pattern) function(dir) {
  f <- file.path(dir, "data/research/coach_changes.csv")
  rows <- readLines(f)
  stopifnot(sum(grepl(pattern, rows)) == 1)
  writeLines(rows[!grepl(pattern, rows)], f)
}

# Append rows to a research file (`rows` may be a function of its lines).
add_rows <- function(dir, file, rows) {
  f <- file.path(dir, "data/research", file)
  old <- readLines(f)
  writeLines(c(old, if (is.function(rows)) rows(old) else rows), f)
}

# For the download check: stand-ins for what a bad download can look like,
# judged against the run's own games file as the cached copy.
download_probe <- '{
  g <- readLines(games_file)
  as_file <- function(lines) { f <- tempfile(fileext = ".csv"); writeLines(lines, f); f }
  verdict <- function(lines) { p <- download_problem(as_file(lines)); if (is.null(p)) "accepted" else p }
  c(same      = verdict(g),
    cut_short = verdict(head(g, length(g) %/% 2)),
    no_games  = verdict(g[1]),
    not_csv   = verdict("<html><body>502 Bad Gateway</body></html>"))
}'

# The 14 teams of the 2025 playoffs; Denver and Seattle had the byes.
field_2025 <- c("BUF", "CAR", "CHI", "DEN", "GB", "HOU", "JAX", "LA", "LAC",
                "NE", "PHI", "PIT", "SEA", "SF")

# Each scenario: the games file, an optional setup step, and checks. A check
# is a named logical computed from the result `r` (state in r$state).
scenarios <- list(
  "Week 18 under way (3 Jan 2026)" = list(
    games  = games_as_of("2026-01-03", 2025, playoff_rounds),
    checks = \(r) with(r$state, c(
      "2025 is the season in progress" = season_in_progress && CURRENT_SEASON == 2025,
      "regular season not over"        = !regular_season_over,
      "both playoff scenarios shown"   = !playoff_field_known && all(is.na(risk$made_playoffs)),
      "data through week 18"           = through_text == "week 18",
      "latest departure shown"         = grepl("latest departure on record", r$report)))),

  "Regular season over, playoff games not listed (5 Jan 2026)" = list(
    games  = games_as_of("2026-01-05", 2025, playoff_rounds),
    checks = \(r) with(r$state, c(
      "regular season over"   = regular_season_over,
      "playoff field unknown" = !playoff_field_known && all(is.na(risk$p_fired))))),

  "Wild-card games listed, byes inferred (5 Jan 2026)" = list(
    games  = games_as_of("2026-01-05", 2025, c("DIV", "CON", "SB")),
    checks = \(r) with(r$state, c(
      "playoff field known"     = playoff_field_known,
      "all 14 teams, with byes" = setequal(playoff_teams, field_2025),
      "one number per coach"    = !anyNA(risk$p_fired),
      "playoff result applied"  = all(risk$made_playoffs == (risk$team %in% field_2025))))),

  "Wild-card round played, divisional round listed (13 Jan 2026)" = list(
    games  = games_as_of("2026-01-13", 2025, c("CON", "SB")),
    checks = \(r) with(r$state, c(
      "playoff field known"         = playoff_field_known && setequal(playoff_teams, field_2025),
      "data through the wild cards" = through_text == "the wild-card round",
      "hero names the round"        = grepl("Data through the wild-card round", r$report)))),

  "Offseason, next schedule not out (1 Mar 2026)" = list(
    games  = games_as_of("2026-03-01", 2025),
    setup  = \(dir) {      # ratings left over from the season just ended
      dir.create(file.path(dir, "output"))
      writeLines("stale", file.path(dir, "output/current_coach_risk_2025.csv"))
    },
    checks = \(r) with(r$state, c(
      "no season in progress"  = !season_in_progress && LAST_COMPLETE_SEASON == 2025,
      "no ratings"             = is.null(risk),
      "old ratings removed"    = !any(grepl("current_coach_risk|07_current", r$files)),
      "report says so"         = grepl("No season is in progress", r$report)))),

  "Opening week (10 Sep 2026)" = list(
    games  = games_as_of("2026-09-10", 2026),
    setup  = \(dir) {
      dir.create(file.path(dir, "output"))
      writeLines("stale", file.path(dir, "output/current_coach_risk_2025.csv"))
    },
    checks = \(r) with(r$state, c(
      "2026 is the season in progress" = season_in_progress && CURRENT_SEASON == 2026,
      "only teams that played rated"   = nrow(risk) >= 1 && nrow(risk) <= 4,
      "others listed as not played"    = grepl("no game yet this season", r$report),
      "only this season's ratings"     = identical(grep("^current_coach_risk_", basename(r$files), value = TRUE),
                                                   "current_coach_risk_2026.csv")))),

  "An offseason departure is missing (14 Sep 2026)" = list(
    games  = games_as_of("2026-09-14", 2026),
    setup  = remove_change("^2026,Baltimore Ravens,John Harbaugh,"),
    checks = \(r) {
      bal <- r$state$tenure |> filter(coach == "John Harbaugh", team == "BAL")
      c("stint kept as unknown, censored" = nrow(bal) == 1 && bal$event_type == "unknown" &&
          bal$status == 0 && !bal$observed && !bal$left_in_current_season,
        "warning names him"  = any(grepl("John Harbaugh \\(BAL, last season 2025\\)", r$state$run_warnings)),
        "report names him"   = grepl("Missing departure records", r$report))
    }),

  "A coach fired mid-season is still listed the next year (1 Oct 2026)" = list(
    # As nflverse did for Gannon, Morris and McDermott in 2026, but for a
    # coach fired mid-season: Tennessee's 2026 games credited to Callahan.
    games  = games_as_of("2026-10-01", 2026) |>
      mutate(home_coach = if_else(season == "2026" & home_team == "TEN", "Brian Callahan", home_coach),
             away_coach = if_else(season == "2026" & away_team == "TEN", "Brian Callahan", away_coach)),
    checks = \(r) {
      ten <- r$state$tenure |> filter(team == "TEN", last_season >= 2025)
      c("Callahan's stint ends in 2025, fired" =
          any(ten$coach == "Brian Callahan" & ten$last_season == 2025 & ten$event_type == "fired"),
        "2026 games go to Saleh" =
          any(ten$coach == "Robert Saleh" & ten$hire_season == 2026),
        "no departure reported missing" = !any(grepl("No departure record", r$state$run_warnings)))
    }),

  "Mistakes in the research files (14 Sep 2026)" = list(
    games  = games_as_of("2026-09-14", 2026),
    setup  = \(dir) {
      # A retread's birth date entered again, a departure under a name the
      # games data doesn't use, a GM row with a misspelled team, and four
      # firings with a cell wrong: reason_category capitalised, reason_category
      # empty, departing_was_interim empty, last_season_coached empty.
      add_rows(dir, "coach_birthdates.csv", \(rows) grep("^Mike McCarthy,", rows, value = TRUE))
      f <- file.path(dir, "data/research/coach_changes.csv")
      rows <- readLines(f)
      edit <- function(start, old, new) {
        at <- grep(start, rows, fixed = TRUE)
        stopifnot(length(at) == 1, grepl(old, rows[at], fixed = TRUE))
        rows[at] <<- sub(old, new, rows[at], fixed = TRUE)
      }
      edit("2026,Miami Dolphins,Mike McDaniel,",   ",Fired,fired,", ",Fired,Fired,")
      edit("2026,Atlanta Falcons,Raheem Morris,",  ",Fired,fired,", ",Fired,,")
      edit("2026,Cleveland Browns,Kevin Stefanski,", "Stefanski,FALSE,", "Stefanski,,")
      edit("2026,Las Vegas Raiders,Pete Carroll,", "Carroll,FALSE,2025,", "Carroll,FALSE,,")
      writeLines(rows, f)
      add_rows(dir, "coach_changes.csv",
               '2026,Washington Commanders,Daniel Quinn,FALSE,2026,TRUE,2026-09-13,NA,Nobody Real,NA,Fired,fired,"x",https://example.org,NA')
      add_rows(dir, "gm_tenures.csv",
               "Las Vegas Raider,Nobody Real,General manager,2026-02-01,NA,day,https://example.org,NA")
    },
    checks = \(r) with(r$state, c(
      "no stint counted twice"        = !any(duplicated(tenure[c("coach", "team", "hire_season")])),
      "McCarthy's seasons not doubled" = identical(tenure$seasons[tenure$coach == "Mike McCarthy" & tenure$team == "GB"], 13L),
      "duplicate birth date warned"   = any(grepl("coach_birthdates.csv: more than one row for Mike McCarthy", run_warnings, fixed = TRUE)),
      "unmatched departures warned"   = any(grepl("match no coaching stint: Pete Carroll (LV, last season NA), Daniel Quinn (WAS, last season 2026)", run_warnings, fixed = TRUE)),
      "Quinn still counts as active"  = any(tenure$coach == "Dan Quinn" & tenure$team == "WAS" & tenure$event_type == "active"),
      "unknown team warned"           = any(grepl('gm_tenures.csv: unrecognised team name "Las Vegas Raider"', run_warnings, fixed = TRUE)),
      "unknown and empty reason warned" = any(grepl('unrecognised reason_category for: Raheem Morris (ATL, last season 2025): (empty), Mike McDaniel (MIA, last season 2025): "Fired"', run_warnings, fixed = TRUE)),
      "empty interim flag warned"     = any(grepl("departing_was_interim is empty for: Kevin Stefanski (CLE, last season 2025)", run_warnings, fixed = TRUE)),
      "their departures are missing"  = any(grepl("No departure record in data/research/coach_changes.csv for: Kevin Stefanski (CLE, last season 2025), Pete Carroll (LV, last season 2025), Raheem Morris (ATL, last season 2025)", run_warnings, fixed = TRUE)),
      "nothing else warned"           = length(run_warnings) == 6))),

  "Warnings, corrections to the games data and the download check (14 Sep 2026)" = list(
    games  = games_as_of("2026-09-14", 2026),
    setup  = \(dir) {      # a warning R raises itself, early in the script
      f <- file.path(dir, "coach_tenure.R")
      lines <- readLines(f)
      at <- grep("^# ---- 1\\. Data", lines)
      stopifnot(length(at) == 1)
      writeLines(append(lines, 'invisible(as.integer("not a number"))', at), f)
    },
    probe  = download_probe,
    checks = \(r) with(r$state, c(
      "R's own warning is recorded"   = identical(run_warnings, "NAs introduced by coercion"),
      "and written to the file"       = identical(r$warnings_file, "NAs introduced by coercion"),
      "nflverse name corrected"       = "Jerry Rosburg" %in% interim_stints$coach && !"Jay Rosburg" %in% interim_stints$coach,
      # Petrino coached Atlanta on 10 December 2007 and resigned the next
      # day; nflverse credits that game to his interim.
      "interim's early game goes back" = identical(coach_seasons$g_reg[coach_seasons$coach == "Bobby Petrino"], 13L),
      # Miami's GM change of 7 September 2010 and Seattle's sale of
      # 3 September 2026 both came days before the season's first game.
      "change before the opener counts that season" =
        identical(coach_seasons$new_gm[coach_seasons$team == "MIA" & coach_seasons$season %in% 2010:2011], c(TRUE, FALSE)) &&
        isTRUE(coach_seasons$new_owner[coach_seasons$team == "SEA" & coach_seasons$season == 2026]),
      "same file accepted"            = probe[["same"]] == "accepted",
      "file cut short rejected"       = grepl("stop earlier", probe[["cut_short"]]),
      "file with no games rejected"   = grepl("no played games", probe[["no_games"]]),
      "error page rejected"           = grepl("isn't the games file", probe[["not_csv"]]))))
)

run_scenario <- function(name) {
  s <- scenarios[[name]]
  r <- replay(s$games, s$setup, if (is.null(s$probe)) "NULL" else s$probe)
  checks <- if (r$status != 0 || is.null(r$state)) c("script ran to the end" = FALSE) else
    tryCatch(s$checks(r), error = function(e) setNames(FALSE, paste("checks ran:", conditionMessage(e))))
  list(name = name, checks = checks, log = r$log)
}

cores <- if (.Platform$OS.type == "unix") max(1, min(length(scenarios), parallel::detectCores())) else 1
results <- parallel::mclapply(names(scenarios), run_scenario, mc.cores = cores)

failed <- 0
for (res in results) {
  if (inherits(res, "try-error") || is.null(res$checks)) {
    cat("\nA scenario could not be run:\n"); print(res); failed <- failed + 1; next
  }
  cat("\n", res$name, "\n", sep = "")
  cat(paste0("  ", if_else(res$checks %in% TRUE, "ok  ", "FAIL"), "  ", names(res$checks)), sep = "\n")
  if (!all(res$checks %in% TRUE)) {
    failed <- failed + sum(!res$checks %in% TRUE)
    cat("  Last lines of the run's log:\n", paste0("    ", res$log, "\n"), sep = "")
  }
}
cat("\n", if (failed == 0) "All replay checks passed." else paste(failed, "check(s) failed."), "\n", sep = "")
quit(status = if (failed == 0) 0 else 1)
