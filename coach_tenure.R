# =============================================================================
# coach_tenure.R — Survival analysis of NFL head-coach tenure (hires 2000+)
# =============================================================================
#
# Run from the project root:   Rscript coach_tenure.R
#
# What this script does, top to bottom
#   0. Setup: check that every package used is already installed (no installs)
#   1. Data: download nflverse game results (who coached every game since 1999),
#      read the sourced research tables in data/research/, and build
#        * data/tenure.csv        one row per coach-team stint
#        * data/coach_seasons.csv one row per coach per season (time-varying)
#        * data/interim_stints.csv interim-only stints (excluded from models)
#   2. Kaplan-Meier curves (overall, and by prior NFL head-coaching experience)
#   3. Cox proportional-hazards model with baseline covariates
#   4. Proportional-hazards check (cox.zph)
#   5. Time-varying Cox model in counting-process (start, stop] format
#   6. Competing risks: cumulative incidence of firing vs. other exits
#   7. Model-based firing risk for current coaches after the 2026 season
#   8. A self-contained HTML report (output/coach_tenure_report.html) with
#      every plot embedded, ready to drop on any web server
#
# Data sources (nothing here is invented; every research row carries a URL)
#   * nflverse games file: https://github.com/nflverse/nfldata, downloaded to
#     data/raw/games.csv (refreshed when the copy is more than a day old)
#     – game scores plus the head coach credited for each team in each game.
#   * data/research/coach_changes.csv – every head-coach departure 2000–2026,
#     with the reason (fired / resigned / retired / ...) taken from the
#     Wikipedia "YYYY NFL season" articles, with a quote and URL per row.
#   * data/research/coach_birthdates.csv – birth dates from Wikidata.
#   * data/research/prior_hc_pre1999.csv – NFL head-coaching jobs before 1999
#     (the nflverse data starts in 1999), from each coach's Wikipedia infobox.
#   * data/research/gm_tenures.csv, owner_tenures.csv – GM and controlling-owner
#     tenures by team, from Wikipedia.
#
# Requirements: R >= 4.1 (native pipe, \(x) lambdas, raw strings), dplyr >= 1.1.0
# (slice_max(by =)), ggplot2 >= 3.4.0 (linewidth), readr >= 2.0.0
# (show_col_types). Section 0 checks these before anything runs.
#
# Package substitutions: none were needed. Everything used (survival,
# survminer, tidyverse, cmprsk, base64enc, htmltools) is installed. cmprsk
# computes the competing-risks cumulative incidence; base64enc and htmltools
# embed the plots and escape text in the HTML report (no pandoc needed).
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

if (getRversion() < "4.1.0") stop("This script needs R 4.1.0 or later.")

needed <- c("survival", "survminer", "tidyverse", "cmprsk", "base64enc", "htmltools")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  stop("These packages are not installed (and this script will not install ",
       "them): ", paste(missing_pkgs, collapse = ", "))
}
# Minimum versions for features the script relies on.
min_versions <- c(dplyr = "1.1.0", ggplot2 = "3.4.0", readr = "2.0.0")
too_old <- names(min_versions)[mapply(\(pkg, v) packageVersion(pkg) < v,
                                       names(min_versions), min_versions)]
if (length(too_old) > 0) {
  stop("These packages are older than this script needs: ",
       paste0(too_old, " (have ", sapply(too_old, \(p) format(packageVersion(p))),
              ", need >= ", min_versions[too_old], ")", collapse = ", "))
}

suppressPackageStartupMessages({
  library(survival)
  library(survminer)
  library(tidyverse)
  library(cmprsk)
})

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# Analysis window. The season settings are derived from the games data in
# section 1a, so the script runs at any time of year:
#   * LAST_COMPLETE_SEASON: the latest season whose Super Bowl has been played.
#     For model *fitting*, every stint is administratively censored at its
#     end: we know who survived it, nothing after.
#   * CURRENT_SEASON = LAST_COMPLETE_SEASON + 1, in progress once it has
#     results. The hot-seat ratings (section 7) only run then.
# The coaching research's coverage is read from coach_changes.csv (its
# latest article_season); when the data moves past it, the script warns that
# the research files need that offseason's changes.

# Colours (checked for colour-vision-deficiency separation): blue, orange, aqua.
pal <- c("#2a78d6", "#eb6834", "#1baf7a")
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title.position = "plot"))

# Map any full team name used in the research files (including old names
# such as "Oakland Raiders" or "Washington Redskins") to the franchise code.
team_abbr <- function(x) {
  key <- c("Cardinals" = "ARI", "Falcons" = "ATL", "Ravens" = "BAL",
           "Bills" = "BUF", "Panthers" = "CAR", "Bears" = "CHI",
           "Bengals" = "CIN", "Browns" = "CLE", "Cowboys" = "DAL",
           "Broncos" = "DEN", "Lions" = "DET", "Packers" = "GB",
           "Texans" = "HOU", "Colts" = "IND", "Jaguars" = "JAX",
           "Chiefs" = "KC", "Raiders" = "LV", "Chargers" = "LAC",
           "Rams" = "LA", "Dolphins" = "MIA", "Vikings" = "MIN",
           "Patriots" = "NE", "Saints" = "NO", "Giants" = "NYG",
           "Jets" = "NYJ", "Eagles" = "PHI", "Steelers" = "PIT",
           "49ers" = "SF", "Seahawks" = "SEA", "Buccaneers" = "TB",
           "Titans" = "TEN", "Washington" = "WAS", "Redskins" = "WAS",
           "Commanders" = "WAS")
  out <- rep(NA_character_, length(x))
  for (k in names(key)) out[is.na(out) & str_detect(x, fixed(k))] <- key[[k]]
  out
}

# Normalise coach names for joining across sources ("Jim Mora Jr." vs
# "Jim L. Mora" etc. are handled by also joining on team and season).
name_key <- function(x) {
  x |> str_to_lower() |> str_remove_all("\\b(jr|sr)\\.?$") |>
    str_remove_all("[^a-z ]") |> str_squish() |>
    recode("herman edwards" = "herm edwards")
}


# ---- 1. Data ----------------------------------------------------------------

## 1a. Game results with the credited head coach ------------------------------
# The file updates weekly in season, so a cached copy older than a day is
# re-downloaded. If the download fails, an existing copy is used instead.
games_url  <- "https://github.com/nflverse/nfldata/raw/master/data/games.csv"
games_file <- "data/raw/games.csv"
stale <- !file.exists(games_file) ||
  difftime(Sys.time(), file.mtime(games_file), units = "hours") > 24
if (stale) {
  tmp <- tempfile(fileext = ".csv")
  ok <- tryCatch(download.file(games_url, tmp, mode = "wb", quiet = TRUE) == 0,
                 error = function(e) FALSE, warning = function(w) FALSE)
  # Accept the download only if it looks like the games file (its header has
  # the columns this script uses); a proxy error page must not replace the
  # cache. read_csv copes with a byte-order mark or quoted column names.
  needed_cols <- c("season", "game_type", "gameday", "week", "home_team", "away_team",
                   "home_score", "away_score", "home_coach", "away_coach")
  header <- if (ok) tryCatch(names(read_csv(tmp, n_max = 0, show_col_types = FALSE)),
                             error = function(e) character()) else character()
  copied <- all(needed_cols %in% header) && file.copy(tmp, games_file, overwrite = TRUE)
  if (!copied) {
    if (!file.exists(games_file)) stop("Could not download ", games_url)
    warning("Could not refresh ", games_url, "; using the cached copy from ",
            format(file.mtime(games_file), "%Y-%m-%d"))
  }
}
games_all <- read_csv(games_file, show_col_types = FALSE)   # includes scheduled games
games <- games_all |> filter(!is.na(home_score))           # games already played

# Season settings (see section 0).
latest_season        <- max(games$season)
season_in_progress   <- !any(games$season == latest_season & games$game_type == "SB")
LAST_COMPLETE_SEASON <- if (season_in_progress) latest_season - 1 else latest_season
CURRENT_SEASON       <- LAST_COMPLETE_SEASON + 1
message("Last complete season: ", LAST_COMPLETE_SEASON, ". ",
        if (season_in_progress) paste(CURRENT_SEASON, "is in progress.")
        else paste("Offseason: no hot-seat ratings until", CURRENT_SEASON, "starts."))
relocated <- c(OAK = "LV", SD = "LAC", STL = "LA")   # one code per franchise
franchise <- function(x) unname(coalesce(relocated[x], x))

# One row per team per game.
team_games <- bind_rows(
  games |> transmute(game_id, season, game_type, gameday, team = home_team,
                     coach = home_coach, pf = home_score, pa = away_score),
  games |> transmute(game_id, season, game_type, gameday, team = away_team,
                     coach = away_coach, pf = away_score, pa = home_score)
) |>
  mutate(team    = franchise(team),
         gameday = as.Date(gameday),
         coach   = recode(coach, "Klint Kubliak" = "Klint Kubiak"),  # typo
         # Two different coaches are both "Jim Mora" in the source.
         coach   = case_when(coach == "Jim Mora" & team == "IND" ~ "Jim Mora Sr.",
                             coach == "Jim Mora" ~ "Jim Mora Jr.",
                             TRUE ~ coach)) |>
  arrange(team, gameday)

## 1b. Departures (reasons) from Wikipedia ------------------------------------
changes <- read_csv("data/research/coach_changes.csv", show_col_types = FALSE) |>
  mutate(team_code   = team_abbr(team),
         coach_key   = name_key(departing_coach),
         interim_key = name_key(interim_coach),
         departure_date = as.Date(departure_date))

# How far the coaching research goes. The GM and owner files can't be
# checked this way (a team with no change leaves no row), so the warning
# reminds about them too.
research_through <- max(changes$article_season)
if (CURRENT_SEASON > research_through) {
  warning("coach_changes.csv covers departures through the ", research_through,
          " offseason, but the data has reached ", CURRENT_SEASON, ". Add the ",
          CURRENT_SEASON, " offseason's departures, and check gm_tenures.csv and ",
          "owner_tenures.csv for that offseason's changes too.", call. = FALSE)
}

# nflverse credits one coach per game but occasionally misses a mid-season
# change (e.g. it credits Ken Whisenhunt with all of Tennessee's 2015 games,
# although Mike Mularkey coached the last nine as interim). For every
# in-season departure with a known date, games after that date are
# re-credited to the interim coach named on Wikipedia.
fixes <- changes |>
  filter(in_season, !is.na(departure_date), !is.na(interim_coach)) |>
  select(team = team_code, season = last_season_coached, coach_key,
         departure_date, interim_coach)

team_games <- team_games |>
  mutate(coach_key = name_key(coach)) |>
  left_join(fixes, by = c("team", "season", "coach_key")) |>
  mutate(recredited = !is.na(departure_date) & gameday > departure_date,
         coach = if_else(recredited, interim_coach, coach)) |>
  select(-coach_key, -departure_date, -interim_coach)

message("Games re-credited to an interim coach: ", sum(team_games$recredited))

# Likewise, the nflverse coach column for the 2026 season still lists three
# coaches who were fired in January 2026 (Gannon ARI, Morris ATL,
# McDermott BUF). If a coach left in the offseason after season s, any
# season s + 1 games still credited to him go to his named successor.
offseason_fixes <- changes |>
  filter(!in_season, !departing_was_interim, !is.na(incoming_coach)) |>
  transmute(team = team_code, season = last_season_coached + 1, coach_key,
            successor = incoming_coach)
team_games <- team_games |>
  mutate(coach_key = name_key(coach)) |>
  left_join(offseason_fixes, by = c("team", "season", "coach_key")) |>
  mutate(stale = !is.na(successor),
         coach = if_else(stale, successor, coach)) |>
  select(-coach_key, -successor)
message("Games re-credited from a departed coach to his successor: ",
        sum(team_games$stale), " (",
        paste(unique(team_games$team[team_games$stale]), collapse = ", "), ")")

## 1c. Coach-team-season summaries --------------------------------------------
# A coach "started" a season if he coached the team's first game; a coach who
# took over mid-season is an interim coach for that season.
cts <- team_games |>
  group_by(team, season) |>
  mutate(g_idx = row_number(), n_team_games = n()) |>
  group_by(team, season, coach) |>
  summarise(started  = min(g_idx) == 1,
            finished = max(g_idx) == first(n_team_games),
            g_reg    = sum(game_type == "REG"),
            w        = sum(game_type == "REG" & pf > pa),
            l        = sum(game_type == "REG" & pf < pa),
            t        = sum(game_type == "REG" & pf == pa),
            pf       = sum(pf[game_type == "REG"]),
            pa       = sum(pa[game_type == "REG"]),
            playoff  = any(game_type != "REG"),
            .groups  = "drop") |>
  # 2012 Saints: Payton was suspended for the season; Kromer and Vitt were
  # stand-ins, not permanent hires, and Payton's stint continues in 2013.
  mutate(started = if_else(team == "NO" & season == 2012, FALSE, started))

# Full-team regular-season win % (all coaches), for "before hire" and
# "change from previous season".
team_season <- team_games |>
  filter(game_type == "REG") |>
  group_by(team, season) |>
  summarise(team_win_pct = mean((pf > pa) + 0.5 * (pf == pa)), .groups = "drop")

## 1c'. Playoff qualification for the season in progress (used by section 7)
# A season's playoff games, scheduled or played, keeping only rows whose two
# teams also played that regular season, so placeholder rows for games whose
# teams aren't known yet are ignored.
post_games <- function(s, types = c("WC", "DIV", "CON", "SB")) {
  g   <- games_all |> filter(season == s)
  reg <- franchise(c(g$home_team[g$game_type == "REG"], g$away_team[g$game_type == "REG"]))
  g |>
    filter(game_type %in% types) |>
    mutate(home_team = franchise(home_team), away_team = franchise(away_team)) |>
    filter(home_team %in% reg, away_team %in% reg)
}
post_teams <- function(s, types = c("WC", "DIV", "CON", "SB")) {
  p <- post_games(s, types)
  unique(c(p$home_team, p$away_team))
}

# The regular season is over once every game on its last scheduled gameday
# has a score, or once wild-card games are on the schedule. (Only the last
# day is checked: a postponed game that never gets played mustn't hang this.)
cur_reg  <- games_all |> filter(season == CURRENT_SEASON, game_type == "REG")
wc       <- post_games(CURRENT_SEASON, "WC")
last_day <- if (nrow(cur_reg) > 0) max(as.Date(cur_reg$gameday)) else NA
regular_season_over <- season_in_progress && nrow(cur_reg) > 0 &&
  (all(!is.na(cur_reg$home_score[as.Date(cur_reg$gameday) == last_day])) || nrow(wc) > 0)

# Who qualified is known once every qualifier appears in a playoff game:
#   * Divisional round fully scheduled (every wild-card winner has a game):
#     all qualifiers have appeared, whatever the playoff format.
#   * Earlier, right after the regular season, only the wild-card games are
#     listed and the teams with byes are missing. Every division winner
#     qualifies, so a team that has its division's best record outright and
#     isn't playing a wild-card game must have a bye. If that finds as many
#     byes as last season's format had, the field is set; otherwise (a
#     division decided on tiebreakers, or a new format) the status waits for
#     the divisional schedule.
playoff_teams <- character()
playoff_field_known <- FALSE
if (regular_season_over) {
  playoff_teams <- post_teams(CURRENT_SEASON)
  wc_played  <- wc |> filter(!is.na(home_score))
  wc_winners <- if_else(wc_played$home_score > wc_played$away_score,
                        wc_played$home_team, wc_played$away_team)
  div_scheduled <- nrow(wc) > 0 && nrow(wc_played) == nrow(wc) &&
    all(wc_winners %in% post_teams(CURRENT_SEASON, c("DIV", "CON", "SB")))

  field_size <- length(post_teams(LAST_COMPLETE_SEASON))
  n_byes     <- field_size - length(post_teams(LAST_COMPLETE_SEASON, "WC"))
  wc_teams   <- post_teams(CURRENT_SEASON, "WC")

  # Divisions come from the data: every team plays all its division rivals
  # (div_game == 1), so a team plus its division opponents names its
  # division. The result is used only if it's consistent: every division the
  # same size (3+ teams) and every member listing exactly the same rivals.
  # A missing or wrong flag would break that, and then the bye inference is
  # skipped rather than trusting a bad grouping.
  division <- character()
  if ("div_game" %in% names(games_all)) {
    dg <- cur_reg |>
      filter(div_game == 1) |>
      transmute(a = franchise(home_team), b = franchise(away_team))
    keys <- bind_rows(dg, tibble(a = dg$b, b = dg$a)) |>
      summarise(key = paste(sort(unique(c(first(a), b))), collapse = "-"), .by = a)
    sizes <- table(keys$key)
    consistent <- length(sizes) > 0 && length(unique(sizes)) == 1 && all(sizes >= 3) &&
      all(sizes == lengths(strsplit(names(sizes), "-")))
    if (consistent) division <- setNames(keys$key, keys$a)
  }
  standings <- team_season |>
    filter(season == CURRENT_SEASON) |>
    mutate(div = unname(division[team]))
  if (!div_scheduled && n_byes > 0 && !anyNA(standings$div) &&   # unknown code: don't guess
      length(wc_teams) == field_size - n_byes) {
    byes <- standings |>
      filter(team_win_pct == max(team_win_pct), .by = div) |>
      filter(n() == 1, .by = div) |>                    # outright division leader
      filter(!team %in% wc_teams)
    if (nrow(byes) == n_byes) playoff_teams <- union(wc_teams, byes$team)
  }
  playoff_field_known <- div_scheduled ||
    length(playoff_teams) == field_size && length(wc_teams) == field_size - n_byes
  message("The ", CURRENT_SEASON, " regular season is over. Coaches who have ",
          "left since need a row in data/research/coach_changes.csv to drop ",
          "off the hot seat.",
          if (!playoff_field_known) " The playoff field isn't settled yet, so ratings still show both scenarios.")
}

## 1d. Stints -----------------------------------------------------------------
# A stint is a run of consecutive seasons by the same coach with the same
# team (Payton's 2012 suspension year doesn't break his run).
cts <- cts |>
  arrange(coach, team, season) |>
  group_by(coach, team) |>
  mutate(gap_ok = season - lag(season) == 1 |
           (coach == "Sean Payton" & team == "NO" & season == 2013),
         run = cumsum(!coalesce(gap_ok, FALSE))) |>
  ungroup() |>
  mutate(stint_id = paste(coach, team, run, sep = "|"))

# The coach of each team's most recent game, in whatever season that was (in
# a new season's opening week, some teams haven't played yet). Without a
# departure record, he's treated as still in the job (section 1e).
current_coaches <- team_games |>
  slice_max(gameday, by = team, with_ties = FALSE) |>
  select(team, coach, latest = season)

stints_all <- cts |>
  group_by(stint_id, coach, team) |>
  summarise(any_start      = any(started),
            first_season   = min(season),
            hire_season    = if (any(started)) min(season[started]) else NA_real_,
            last_season    = max(season),
            ended_midseason = !finished[which.max(season)],
            .groups = "drop") |>
  mutate(interim_before = any_start & first_season < hire_season,
         active = paste(coach, team, last_season) %in%
           paste(current_coaches$coach, current_coaches$team, current_coaches$latest))

# Interim-only stints are kept in their own table and excluded from models.
interim_stints <- stints_all |> filter(!any_start) |>
  select(coach, team, season = first_season, last_season)

# Hires from 2000 onward. Stints that began in 1999 or earlier are excluded
# (we would not know when they began).
stints <- stints_all |> filter(any_start, hire_season >= 2000)

## 1e. Event type: fired / resigned / retired / other / active ----------------
dep <- changes |>
  filter(!departing_was_interim) |>
  transmute(team = team_code, last_season = last_season_coached, coach_key,
            reason_category, reason_raw, evidence, source_url)

stints <- stints |>
  mutate(coach_key = name_key(coach)) |>
  left_join(dep, by = c("team", "last_season", "coach_key")) |>
  mutate(
    # "Fired" here means the club ended the job. Besides outright firings
    # that includes: "mutually agreed to part ways" (5 cases: Fox 2015,
    # Jim Harbaugh 2015, Mularkey 2018, Lewis 2019, Belichick 2024), the club
    # declining to renew an expiring contract (6: Gregg Williams, Tice,
    # Fox 2011, Cable, Garrett, McCarthy 2025), and Pete Carroll's 2024
    # "reassignment" to an advisor role. A stricter coding is checked in the
    # sensitivity analysis at the end of section 3.
    forced = reason_category %in% c("fired", "mutual", "contract_expired") |
      (reason_category == "other" & str_detect(reason_raw, regex("reassign", TRUE))),
    # A departure record always wins: a coach fired on Black Monday is still
    # the credited coach of his team's last game, but he isn't active.
    active = active & is.na(reason_category),
    event_type = case_when(
      forced                        ~ "fired",
      reason_category == "resigned" ~ "resigned",
      reason_category == "retired"  ~ "retired",
      !is.na(reason_category)       ~ "other",   # traded (Gruden 2002, Edwards 2006)
      active                        ~ "active",
      TRUE                          ~ NA_character_),
    strict_fired = reason_category == "fired")

# Every stint that ended by the last complete season needs a departure
# record. A coach who leaves during the current season may not have one yet;
# that's fine, because stints are censored at LAST_COMPLETE_SEASON anyway.
unmatched <- stints |> filter(is.na(event_type))
if (any(unmatched$last_season <= LAST_COMPLETE_SEASON)) {
  print(unmatched |> filter(last_season <= LAST_COMPLETE_SEASON) |>
          select(coach, team, hire_season, last_season))
  stop("Some ended stints have no departure record in coach_changes.csv")
}
if (nrow(unmatched) > 0) {
  # Either a real mid-season departure not yet in the research file, or a
  # coach-credit problem in the games file (e.g. a one-game fill-in). Both
  # leave the team without a rated coach, so say so loudly; the HTML report
  # also lists unrated teams.
  warning("No departure record for ", CURRENT_SEASON, " stint(s): ",
          paste0(unmatched$coach, " (", unmatched$team, ")", collapse = ", "),
          ". They are censored, and these teams get no hot-seat rating. Add the ",
          "departure to data/research/coach_changes.csv, or check the coach ",
          "credited in the games file.", call. = FALSE)
  stints <- stints |> mutate(event_type = coalesce(event_type, "unknown"))
}
# Reverse check: departures that match no stint should all be coaches hired
# before 2000 (outside the sample) or who never coached a game.
orphans <- dep |>
  anti_join(stints |> select(team, last_season, coach_key),
            by = c("team", "last_season", "coach_key")) |>
  anti_join(stints_all |> filter(hire_season < 2000 | is.na(hire_season)) |>
              mutate(coach_key = name_key(coach)) |> select(team, last_season, coach_key),
            by = c("team", "last_season", "coach_key")) |>
  filter(!is.na(last_season))  # Belichick's one-day 2000 Jets job: no games
if (nrow(orphans) > 0) {
  message("Departure records not matched to any stint (check these):")
  print(orphans |> select(team, last_season, coach_key, reason_category))
}

## 1f. Baseline covariates ----------------------------------------------------
births <- read_csv("data/research/coach_birthdates.csv", show_col_types = FALSE,
                   col_types = cols(birth_date = col_character())) |>
  select(coach, birth_date) |>
  # Wikidata gives only a birth year for Mike LaFleur ("1987-00-00"); use
  # mid-year, which is within ±0.5 years of his true age.
  mutate(birth_date = as.Date(str_replace(birth_date, "-00-00$", "-07-01")))

pre1999 <- read_csv("data/research/prior_hc_pre1999.csv", show_col_types = FALSE)

# "Previous NFL head coach" = held a *permanent* NFL head-coaching job
# (started a season as head coach) before this hire. Interim-only experience
# doesn't count; it is flagged separately.
perm_jobs <- bind_rows(
  stints_all |> filter(any_start) |> transmute(coach, job_start = hire_season),
  pre1999 |> transmute(coach, job_start = first_season))
interim_jobs <- interim_stints |> transmute(coach, job_start = season)

had_before <- function(coach, hire, jobs) {
  map2_lgl(coach, hire, \(c, h) any(jobs$coach == c & jobs$job_start < h))
}

stints <- stints |>
  left_join(births, by = "coach") |>
  left_join(team_season |> transmute(team, hire_season = season + 1,
                                     prev_win_pct = team_win_pct),
            by = c("team", "hire_season")) |>
  mutate(age_at_hire  = as.numeric(as.Date(paste0(hire_season, "-09-01")) -
                                     birth_date) / 365.25,
         prior_hc     = had_before(coach, hire_season, perm_jobs),
         prior_interim_only = !prior_hc &
           (had_before(coach, hire_season, interim_jobs) | interim_before),
         background   = factor(if_else(prior_hc, "Previous NFL head coach",
                                       "First-time NFL head coach"),
                               levels = c("First-time NFL head coach",
                                          "Previous NFL head coach")))

## 1g. Season-level table (time-varying covariates) ----------------------------
# GM and controlling-owner changes. A change dated Jan–Aug of year Y takes
# effect for season Y; one dated Sep–Dec (during season Y) counts for Y + 1.
# That way a GM fired *at the same time* as the coach can't leak into the
# season that led to the coach's firing.
first_season_of <- function(d) if_else(month(d) <= 8, year(d), year(d) + 1)
front_office <- bind_rows(
  read_csv("data/research/gm_tenures.csv", show_col_types = FALSE) |>
    transmute(team, start_date = as.Date(start_date), kind = "gm"),
  read_csv("data/research/owner_tenures.csv", show_col_types = FALSE) |>
    transmute(team, start_date = as.Date(start_date), kind = "owner")) |>
  mutate(team = team_abbr(team), season = first_season_of(start_date)) |>
  filter(!is.na(season))
new_fo <- front_office |>
  group_by(team, season) |>
  summarise(new_gm = any(kind == "gm"), new_owner = any(kind == "owner"),
            .groups = "drop")

coach_seasons <- cts |>
  semi_join(stints, by = "stint_id") |>
  left_join(stints |> select(stint_id, hire_season), by = "stint_id") |>
  filter(season >= hire_season) |>                 # drop interim lead-in year
  arrange(stint_id, season) |>
  group_by(stint_id) |>
  mutate(tenure_season = row_number()) |>          # 1 = first season, ...
  ungroup() |>
  left_join(team_season |> transmute(team, season = season + 1,
                                     prev_team_win_pct = team_win_pct),
            by = c("team", "season")) |>
  left_join(new_fo, by = c("team", "season")) |>
  mutate(win_pct        = (w + 0.5 * t) / g_reg,
         win_pct_change = win_pct - prev_team_win_pct,
         pd_per_game    = (pf - pa) / g_reg,
         new_gm         = coalesce(new_gm, FALSE),
         new_owner      = coalesce(new_owner, FALSE),
         new_gm_or_owner = new_gm | new_owner)

# Seasons coached, through the last complete season.
n_complete <- coach_seasons |>
  filter(season <= LAST_COMPLETE_SEASON) |>
  count(stint_id, name = "seasons")

tenure <- stints |>
  left_join(n_complete, by = "stint_id") |>
  mutate(seasons = coalesce(seasons, 0L),
         # The single rule for what the models count: an exit is observed only
         # if it happened by the end of the last complete season. A coach who
         # left during the current season keeps his event_type (fired,
         # resigned, ...) but is censored, flagged by left_in_current_season.
         observed = event_type != "active" & last_season <= LAST_COMPLETE_SEASON,
         left_in_current_season = event_type != "active" & !observed,
         # status = 1 only for an observed firing; everything else is censored
         status  = as.integer(observed & event_type == "fired")) |>
  arrange(hire_season, team)

# Event flag on the season row in which the firing happened (the stint's
# last season). Seasons after 2025 are not used for fitting.
coach_seasons <- coach_seasons |>
  left_join(tenure |> select(stint_id, status, last_season, event_type),
            by = "stint_id") |>
  mutate(fired_after = as.integer(status == 1 & season == last_season)) |>
  select(-status)

write_csv(tenure |> select(coach, team, hire_season, last_season, seasons,
                           event_type, status, left_in_current_season,
                           ended_midseason, background,
                           prior_hc, prior_interim_only, interim_before,
                           age_at_hire, prev_win_pct, reason_raw, source_url),
          "data/tenure.csv")
write_csv(coach_seasons |> select(coach, team, season, tenure_season, g_reg,
                                  w, l, t, win_pct, win_pct_change,
                                  pd_per_game, playoff, new_gm, new_owner,
                                  new_gm_or_owner, fired_after),
          "data/coach_seasons.csv")
write_csv(interim_stints, "data/interim_stints.csv")

fit_data <- tenure |> filter(seasons > 0)  # new hires have no completed season
cat("\nStints (hires 2000+):", nrow(tenure),
    "| used for fitting:", nrow(fit_data),
    "| interim-only stints excluded:", nrow(interim_stints), "\n")
print(count(tenure, event_type, left_in_current_season))
cat("Firings used in the models (status = 1):", sum(tenure$status), "\n")


# ---- 2. Kaplan–Meier --------------------------------------------------------
# Survival analysis models *time until an event* — here, seasons until a
# coach is fired. A coach still in the job, or one who resigned or retired,
# is CENSORED: we know he lasted at least that long without being fired, but
# not when (or whether) a firing would have come. Dropping those coaches, or
# treating them as fired, would bias tenure down; censoring uses exactly the
# information we have.
#
# The Kaplan–Meier (KM) estimator gives S(t), the probability of not having
# been fired after t seasons. At each season it multiplies by
# (1 − fired / at risk), where "at risk" counts coaches who reached that
# season still employed and uncensored.

km_all <- survfit(Surv(seasons, status) ~ 1, data = fit_data)
km_bg  <- survfit(Surv(seasons, status) ~ background, data = fit_data)

cat("\n== Kaplan–Meier median seasons until firing ==\n")
print(km_all)
print(km_bg)

save_km <- function(p, file, w = 9, h = 7) {
  png(file, width = w, height = h, units = "in", res = 150)
  suppressMessages(print(p))   # silence survminer's harmless label notes
  dev.off()
}

p_km_all <- ggsurvplot(
  km_all, data = fit_data, conf.int = TRUE, palette = pal[1],
  risk.table = TRUE, risk.table.height = 0.25, break.time.by = 2,
  risk.table.fontsize = 3.5,
  surv.median.line = "hv", legend = "none",
  xlab = "Seasons as head coach", ylab = "Probability not yet fired",
  title = paste0("NFL head coaches hired 2000–", LAST_COMPLETE_SEASON, ": time until firing"),
  subtitle = "Kaplan–Meier estimate with 95% CI; resignations, retirements and active coaches censored",
  ggtheme = theme_minimal(base_size = 12))
save_km(p_km_all, "plots/01_km_overall.png")

p_km_bg <- ggsurvplot(
  km_bg, data = fit_data, conf.int = TRUE, palette = pal[1:2],
  risk.table = TRUE, risk.table.height = 0.28, break.time.by = 2,
  risk.table.fontsize = 3.5,
  surv.median.line = "hv", pval = TRUE, pval.coord = c(10, 0.9),
  legend.title = "", legend.labs = levels(fit_data$background),
  xlab = "Seasons as head coach", ylab = "Probability not yet fired",
  title = "Time until firing, by background",
  subtitle = "Log-rank test p-value shown",
  ggtheme = theme_minimal(base_size = 12))
save_km(p_km_bg, "plots/02_km_by_background.png")


# ---- 3. Cox proportional-hazards model (baseline covariates) ----------------
# The HAZARD h(t) is the chance of being fired in season t given you've made
# it to season t. The Cox model says
#     h(t | x) = h0(t) * exp(b1 x1 + b2 x2 + ...)
# — every coach shares an unspecified baseline hazard h0(t), and covariates
# multiply it. exp(b) is the HAZARD RATIO (HR): HR = 1.3 means 30% higher
# firing risk in any given season, holding the other covariates fixed.
#
# Covariates (all known on the day of the hire):
#   age_at_hire (per 5 years), prior_hc (previous permanent NFL HC job),
#   prev_win_pct (team's win % the season before the hire, per 10 points).
# Ties: tenure is counted in whole seasons, so many coaches share an event
# time. survival uses Efron's approximation for ties by default.

cox_data <- fit_data |>
  mutate(age5 = age_at_hire / 5, prev_win10 = prev_win_pct * 10)
cox_base <- coxph(Surv(seasons, status) ~ age5 + prior_hc + prev_win10,
                  data = cox_data)
cat("\n== Cox model, baseline covariates ==\n")
print(summary(cox_base))
cat("(Houston 2002 is dropped: an expansion team has no previous season.)\n")

# Sensitivity: count only departures Wikipedia labels plainly "Fired"
# (mutual partings, non-renewals and Carroll's reassignment become censored).
cox_strict <- coxph(Surv(seasons, as.integer(status == 1 & strict_fired)) ~
                      age5 + prior_hc + prev_win10, data = cox_data)
cat("\n== Sensitivity: strict definition of fired ==\n")
print(round(cbind(HR_main = exp(coef(cox_base)), HR_strict = exp(coef(cox_strict))), 3))
print(survfit(Surv(seasons, as.integer(status == 1 & strict_fired)) ~ 1, data = cox_data))

png("plots/03_cox_baseline_forest.png", width = 9, height = 4.5, units = "in", res = 150)
print(ggforest(cox_base, data = cox_data,
               main = "Cox model: hazard ratios for being fired"))
dev.off()


# ---- 4. Checking proportional hazards --------------------------------------
# The Cox model assumes each HR is constant over tenure (e.g. that prior
# experience matters as much in season 1 as in season 6). cox.zph tests this
# using scaled Schoenfeld residuals: if a covariate's effect drifts with
# time, its residuals trend against time. A small p-value means the
# constant-HR assumption is doubtful for that covariate.

zph <- cox.zph(cox_base, transform = "km")
cat("\n== Proportional-hazards test (cox.zph) ==\n")
print(zph)

png("plots/04_cox_zph.png", width = 8, height = 10, units = "in", res = 150)
print(ggcoxzph(zph, font.main = 11, point.col = pal[1], point.alpha = 0.5,
               ggtheme = theme_minimal(base_size = 11)))
dev.off()


# ---- 5. Time-varying Cox model (counting-process format) --------------------
# Win % changes every season, so a single row per coach can't carry it.
# COUNTING-PROCESS FORMAT splits each coach's tenure into intervals
# (start, stop], one per season, each with that season's covariates and an
# event flag that is 1 only on the interval in which he was fired:
#
#   coach   start stop  win_pct  fired
#   A         0     1    0.35      0
#   A         1     2    0.25      1     <- fired after season 2
#
# Row k uses only what was known when the firing decision for season k was
# made: that season's results (for a mid-season firing, only the games he
# coached before being let go), and the change from the team's previous
# season. Front-office changes count only if they took effect before the
# season started (see 1g). Nothing from later seasons is used: no look-ahead.

tv_data <- coach_seasons |>
  filter(season <= LAST_COMPLETE_SEASON) |>
  left_join(stints |> select(stint_id, age_at_hire, prior_hc), by = "stint_id") |>
  mutate(tstart = tenure_season - 1, tstop = tenure_season,
         win10 = win_pct * 10, win_chg10 = win_pct_change * 10,
         age5 = age_at_hire / 5)

cox_tv <- coxph(Surv(tstart, tstop, fired_after) ~ win10 + win_chg10 +
                  playoff + pd_per_game + new_gm_or_owner + prior_hc + age5,
                data = tv_data, cluster = stint_id)
cat("\n== Time-varying Cox model ==\n")
print(summary(cox_tv))
cat("\n== PH check for the time-varying model ==\n")
print(cox.zph(cox_tv, transform = "km"))

# The change-in-win-% effect fails the PH check. A standard fix is to let
# the coefficient differ by period of tenure: seasons 1–2 vs. 3+. (Holding
# this season's win % fixed, a larger "change" means last season was worse,
# so this term measures "two bad years in a row".)
split_chg <- function(df) {
  mutate(df, win_chg10_s1_2 = win_chg10 * (tenure_season <= 2),
             win_chg10_s3plus = win_chg10 * (tenure_season > 2))
}
tv_data <- split_chg(tv_data)
cox_tv_split <- coxph(Surv(tstart, tstop, fired_after) ~ win10 +
                        win_chg10_s1_2 + win_chg10_s3plus +
                        playoff + pd_per_game + new_gm_or_owner + prior_hc + age5,
                      data = tv_data, cluster = stint_id)
cat("\n== Time-varying model with change-in-win-% split by tenure ==\n")
print(round(summary(cox_tv_split)$conf.int[, c(1, 3, 4)], 3))
print(cox.zph(cox_tv_split, transform = "km"))

# Forest plot built by hand: survminer::ggforest can't handle the cluster()
# term, and we want the robust (clustered) confidence intervals anyway.
tv_labels <- c(win10 = "Win % this season (per +10 pts)",
               win_chg10 = "Change in win % vs last season (per +10 pts)",
               playoffTRUE = "Made playoffs this season",
               pd_per_game = "Point differential per game (per +1)",
               new_gm_or_ownerTRUE = "New GM or owner this season",
               prior_hcTRUE = "Previous NFL head coach",
               age5 = "Age at hire (per +5 years)")
tv_ci <- summary(cox_tv)$conf.int |> as_tibble(rownames = "term") |>
  transmute(term = tv_labels[term], hr = `exp(coef)`,
            lo = `lower .95`, hi = `upper .95`) |>
  mutate(term = fct_rev(fct_inorder(term)))
p_forest_tv <- ggplot(tv_ci, aes(x = hr, y = term)) +
  geom_vline(xintercept = 1, colour = "grey60", linetype = "dashed") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.2, orientation = "y",
                colour = pal[1]) +
  geom_point(size = 2.8, colour = pal[1]) +
  geom_text(aes(label = sprintf("%.2f", hr)), vjust = -0.9, size = 3.3) +
  scale_x_log10() +
  labs(title = "Time-varying Cox model: hazard ratios for being fired after a season",
       subtitle = "Robust 95% CIs (clustered by coach stint). HR < 1 = lower firing risk. Log scale.",
       x = "Hazard ratio", y = NULL) +
  theme(panel.grid.major.y = element_blank())
ggsave("plots/05_cox_timevarying_forest.png", p_forest_tv, width = 9, height = 5, dpi = 150)


# ---- 6. Competing risks ------------------------------------------------------
# Coaches leave in more than one way. Treating resignations and retirements
# as censored (as above) answers "what if those exits didn't exist?". The
# CUMULATIVE INCIDENCE FUNCTION (CIF) answers the real-world question: by
# season t, what share of coaches have been fired, and what share have left
# some other way? (1 − KM overstates the firing share, because a coach who
# retired can no longer be fired.)

cr <- fit_data |>
  mutate(fstatus = case_when(!observed ~ 0L,               # censored
                             event_type == "fired" ~ 1L,
                             TRUE ~ 2L))  # resigned / retired / other
ci <- cuminc(ftime = cr$seasons, fstatus = cr$fstatus, cencode = 0)
names(ci) <- c("Fired", "Resigned, retired or other")[as.integer(str_sub(names(ci), -1))]
cat("\n== Cumulative incidence at 2, 4, 6, 8 seasons ==\n")
print(timepoints(ci, times = c(2, 4, 6, 8)))

# Plot the two cumulative incidence curves (step functions) with pointwise
# 95% CIs from cuminc's variance estimates.
ci_df <- imap_dfr(ci[names(ci) != "Tests"], \(x, nm)
  tibble(cause = nm, time = x$time, est = x$est, se = sqrt(x$var))) |>
  mutate(lo = pmax(est - 1.96 * se, 0), hi = pmin(est + 1.96 * se, 1),
         cause = factor(cause, levels = c("Fired", "Resigned, retired or other")))
end_labels <- ci_df |> group_by(cause) |> slice_max(time, n = 1, with_ties = FALSE)
p_ci <- ggplot(ci_df, aes(time, est, colour = cause, fill = cause)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA,
              stat = "identity", outline.type = "full") +
  geom_step(linewidth = 1) +
  geom_text(data = end_labels, aes(label = cause), hjust = 1, vjust = -0.8,
            size = 3.8, colour = "grey20") +
  scale_colour_manual(values = pal[c(2, 1)], name = NULL) +
  scale_fill_manual(values = pal[c(2, 1)], name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "How tenures end: firing vs. other exits",
       subtitle = paste0("Cumulative incidence (Aalen–Johansen), hires 2000–", LAST_COMPLETE_SEASON),
       x = "Seasons as head coach", y = "Share of coaches who have left this way") +
  theme(legend.position = "top", legend.justification = "left")
ggsave("plots/06_competing_risks.png", p_ci, width = 9, height = 5.5, dpi = 150)


# ---- 7. Current coaches: model risk of firing after the season in progress --
# Runs only while a season is in progress. For each current head coach,
# build the row for his tenure season k = CURRENT_SEASON using that season
# *to date* ("if the season ended today"). The playoff outcome isn't known
# yet, so we show two scenarios: misses / makes playoffs.
#
# P(fired after season k) = 1 − exp(−ΔH0(k) · exp(x'b)), where ΔH0(k) is the
# Cox baseline hazard for tenure season k. Few coaches reach long tenures, so
# ΔH0 is pooled (averaged) for seasons 8 and later. We use the split model
# from section 5, since it satisfies the proportional-hazards assumption.

if (season_in_progress) {
  bh <- basehaz(cox_tv_split, centered = FALSE) |>
    as_tibble() |>
    mutate(k = time, dH0 = hazard - lag(hazard, default = 0)) |>
    select(k, dH0)
  bh <- tibble(k = 1:40) |> left_join(bh, by = "k") |>
    mutate(dH0 = coalesce(dH0, 0))
  tail_rate <- mean(bh$dH0[bh$k >= 8 & bh$k <= max(tv_data$tstop)])
  bh <- bh |> mutate(dH0 = if_else(k >= 8, tail_rate, dH0))

  # Five or six games are a noisy sample, and the model was fit on full
  # seasons: plugging in a raw 0-5 would treat it like 0-17. So the inputs
  # are a *projected* full season: actual results so far, plus .500 football
  # and an even point differential over the remaining games of 17.
  SEASON_GAMES <- 17
  current <- coach_seasons |>
    filter(season == CURRENT_SEASON) |>
    semi_join(stints |> filter(active), by = "stint_id") |>   # still in the job
    left_join(stints |> select(stint_id, age_at_hire, prior_hc), by = "stint_id") |>
    mutate(win_pct_to_date = win_pct,
           win_pct        = (w + 0.5 * t + 0.5 * (SEASON_GAMES - g_reg)) / SEASON_GAMES,
           win_pct_change = win_pct - prev_team_win_pct,
           pd_per_game    = (pf - pa) / SEASON_GAMES,
           win10 = win_pct * 10, win_chg10 = win_pct_change * 10,
           age5 = age_at_hire / 5) |>
    split_chg() |>
    left_join(bh, by = c("tenure_season" = "k"))

  risk_for <- function(df, playoff_value) {
    nd <- df |> mutate(playoff = playoff_value)
    lp <- predict(cox_tv_split, newdata = nd, type = "lp", reference = "zero")
    1 - exp(-nd$dH0 * exp(lp))
  }

  # Until the playoffs start, show both scenarios. After that, playoff
  # qualification is known, so p_fired uses the team's actual result and
  # drives the ranking; the scenario columns are kept for reference.
  risk <- current |>
    mutate(record = paste0(w, "-", l, if_else(t > 0, paste0("-", t), "")),
           made_playoffs      = if (playoff_field_known) team %in% playoff_teams else NA,
           p_fired_if_miss    = risk_for(current, FALSE),
           p_fired_if_playoff = risk_for(current, TRUE),
           p_fired = if_else(made_playoffs, p_fired_if_playoff, p_fired_if_miss)) |>
    arrange(desc(coalesce(p_fired, p_fired_if_miss))) |>
    transmute(coach, team, season_number = tenure_season, record,
              games_played = g_reg,
              proj_win_pct = round(win_pct, 3),
              proj_win_pct_change = round(win_pct_change, 3),
              pd_to_date = pf - pa,
              new_gm_or_owner, prior_hc, made_playoffs,
              p_fired = round(p_fired, 3),
              p_fired_if_miss = round(p_fired_if_miss, 3),
              p_fired_if_playoff = round(p_fired_if_playoff, 3))

  write_csv(risk, paste0("output/current_coach_risk_", CURRENT_SEASON, ".csv"))
  cat("\n== Model-estimated probability of being fired after", CURRENT_SEASON, "==\n")
  cat(if (playoff_field_known) "(final regular-season records and actual playoff results; estimates, not forecasts)\n"
      else "(projected season: results to date + .500 the rest of the way; estimates, not forecasts)\n")
  print(risk, n = 32)

  risk_plot <- risk |>
    mutate(label = paste0(coach, " (", team, ", ", record, ")"),
           label = fct_reorder(label, coalesce(p_fired, p_fired_if_miss)))
  if (playoff_field_known) {
    p_risk <- ggplot(risk_plot, aes(x = p_fired, y = label,
                                    colour = if_else(made_playoffs, "Made playoffs", "Missed playoffs"))) +
      geom_point(size = 2.8) +
      scale_colour_manual(values = c("Made playoffs" = pal[1], "Missed playoffs" = pal[2]),
                          name = NULL) +
      labs(subtitle = paste("Time-varying Cox model, using each team's final", CURRENT_SEASON,
                            "record and playoff result. Model estimates, not predictions."))
  } else {
    p_risk <- ggplot(risk_plot, aes(y = label)) +
      geom_segment(aes(x = p_fired_if_playoff, xend = p_fired_if_miss, yend = label),
                   colour = "grey75", linewidth = 0.8) +
      geom_point(aes(x = p_fired_if_playoff, colour = "Makes playoffs"), size = 2.6) +
      geom_point(aes(x = p_fired_if_miss, colour = "Misses playoffs"), size = 2.6) +
      scale_colour_manual(values = c("Makes playoffs" = pal[1], "Misses playoffs" = pal[2]),
                          name = NULL) +
      labs(subtitle = paste("Time-varying Cox model.", CURRENT_SEASON,
                            "= results to date + .500 the rest of the way. Model estimates, not predictions."))
  }
  p_risk <- p_risk +
    scale_x_continuous(labels = scales::percent) +
    labs(title = paste("Model-estimated chance of being fired after the", CURRENT_SEASON, "season"),
         x = "Estimated probability", y = NULL) +
    theme(legend.position = "top", legend.justification = "left",
          panel.grid.major.y = element_blank())
  ggsave("plots/07_current_coach_risk.png", p_risk, width = 9, height = 9, dpi = 150)
} else {
  # No season in progress: clear ratings left over from an earlier run.
  risk <- NULL
  unlink(c("plots/07_current_coach_risk.png",
           Sys.glob("output/current_coach_risk_*.csv")))
}


# ---- 8. Self-contained HTML report -------------------------------------------
# One HTML file with the plots embedded as base64 PNGs and no external
# scripts, stylesheets or fonts, so it can be copied to any web server (or
# opened locally) as-is. All numbers below are computed from the models
# above, so the page stays in step with the data each time the script runs.

esc <- function(x) htmltools::htmlEscape(as.character(x))
# English month names regardless of the user's locale (format "%B" is localized).
long_date <- function(d) paste0(month.name[as.integer(format(d, "%m"))], " ",
                                as.integer(format(d, "%d")), ", ", format(d, "%Y"))
pct <- function(x, d = 0) sprintf(paste0("%.", d, "f%%"), 100 * x)
num <- function(x, d = 2) formatC(x, format = "f", digits = d)
pval <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))

img <- function(path, alt, caption = NULL) {
  sprintf('<figure class="card fig"><img src="data:image/png;base64,%s" alt="%s">%s</figure>',
          base64enc::base64encode(path), esc(alt),
          if (is.null(caption)) "" else paste0("<figcaption>", caption, "</figcaption>"))
}

# A data frame as an HTML table; `num_cols` are right-aligned.
html_table <- function(df, caption = NULL, num_cols = integer()) {
  cls <- ifelse(seq_along(df) %in% num_cols, ' class="num"', "")
  head <- paste0("<th", cls, ">", esc(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, \(r) paste0("<tr>", paste0("<td", cls, ">", esc(r), "</td>",
                                                  collapse = ""), "</tr>"))
  paste0('<div class="table-wrap card"><table>',
         if (!is.null(caption)) paste0("<caption>", esc(caption), "</caption>"),
         "<thead><tr>", head, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table></div>")
}

# Hazard-ratio table from a coxph fit (robust CIs/p-values when clustered).
hr_table <- function(fit, labels) {
  ci <- summary(fit)$conf.int
  pv <- summary(fit)$coefficients[, "Pr(>|z|)"]
  tibble(Covariate = coalesce(labels[rownames(ci)], rownames(ci)),
         `Hazard ratio` = num(ci[, "exp(coef)"]),
         `95% CI` = paste0(num(ci[, "lower .95"]), "–", num(ci[, "upper .95"])),
         p = pval(pv)) |>
    html_table(num_cols = 2:4)
}

zph_table <- function(z, labels) {
  tb <- z$table
  tibble(Term = coalesce(labels[rownames(tb)], rownames(tb)),
         `Chi-square` = num(tb[, "chisq"]), p = pval(tb[, "p"])) |>
    html_table(num_cols = 2:3)
}

base_labels <- c(age5 = "Age at hire (per +5 years)",
                 prior_hcTRUE = "Previous NFL head coach",
                 prior_hc = "Previous NFL head coach",
                 prev_win10 = "Team win % the season before hire (per +10 pts)",
                 GLOBAL = "Global test")
all_labels <- c(base_labels, tv_labels,
                win10 = "Win % this season (per +10 pts)",
                win_chg10 = "Change in win % vs last season (per +10 pts)",
                win_chg10_s1_2 = "Change in win % vs last season, seasons 1–2",
                win_chg10_s3plus = "Change in win % vs last season, season 3+",
                playoff = "Made playoffs this season",
                pd_per_game = "Point differential per game (per +1)",
                new_gm_or_owner = "New GM or owner this season")

## Numbers used in the text --------------------------------------------------
km_tab   <- summary(km_all)$table
km_bg_tb <- summary(km_bg)$table
lr       <- survdiff(Surv(seasons, status) ~ background, data = fit_data)
lr_p     <- pchisq(lr$chisq, df = length(lr$n) - 1, lower.tail = FALSE)
s_at     <- summary(km_all, times = c(1, 2, 3, 4, 5, 8, 10))
cif      <- timepoints(ci, times = c(2, 4, 6, 8))$est
zph_tv   <- cox.zph(cox_tv, transform = "km")
zph_spl  <- cox.zph(cox_tv_split, transform = "km")
hr_tv    <- summary(cox_tv)$conf.int
data_through <- max(team_games$gameday)
week_through <- if (season_in_progress)
  max(games$week[games$season == CURRENT_SEASON & games$game_type == "REG"]) else NA
n_fired  <- sum(fit_data$status)
n_fired_after_playoffs <- sum(tv_data$fired_after == 1 & tv_data$playoff)

flagged <- function(z) {
  tb <- z$table[rownames(z$table) != "GLOBAL", , drop = FALSE]
  hits <- rownames(tb)[tb[, "p"] < 0.05]
  if (length(hits) == 0) "none at the 5% level"
  else paste(esc(coalesce(all_labels[hits], hits)), collapse = "; ")
}

median_text <- function(row) {
  sprintf("%s seasons (95%% CI %s–%s)", row[["median"]], row[["0.95LCL"]], row[["0.95UCL"]])
}


## Layout pieces ----------------------------------------------------------------
section <- function(id, number, title, ...) {
  paste0('<section id="', id, '"><div class="sec-head"><span class="sec-num">', number,
         '</span><h2>', title, "</h2></div>", ..., "</section>")
}
takeaway <- function(text) paste0('<p class="takeaway">', text, "</p>")

## Hero: title + scoreboard -----------------------------------------------------
stat <- function(value, label) {
  sprintf('<div class="stat"><div class="stat-num">%s</div><div class="stat-label">%s</div></div>',
          value, label)
}
hero <- paste0(
  '<header class="hero"><div class="hero-inner">',
  sprintf('<p class="kicker">Survival analysis &middot; Head coaches hired %d&ndash;%d</p>',
          min(tenure$hire_season), max(tenure$hire_season)),
  "<h1>How long do NFL head coaches last?</h1>",
  if (season_in_progress) {
    sprintf('<p class="hero-sub">%d coaching stints, %d firings, and the model&rsquo;s read on who&rsquo;s on the hot seat after %d. Data through week %d (%s).</p>',
            nrow(tenure), n_fired, CURRENT_SEASON, week_through, long_date(data_through))
  } else {
    sprintf('<p class="hero-sub">%d coaching stints and %d firings. Data through the end of the %d season (%s).</p>',
            nrow(tenure), n_fired, LAST_COMPLETE_SEASON, long_date(data_through))
  },
  '<div class="scoreboard">',
  stat(km_tab[["median"]], "Median seasons until a coach is fired"),
  stat(pct(cif["Fired", "4"]), "Fired by the end of season 4"),
  stat(paste0("&minus;", pct(1 - hr_tv["win10", "exp(coef)"])), "Firing risk per +10 pts of win %"),
  stat(paste0("&minus;", pct(1 - hr_tv["playoffTRUE", "exp(coef)"])), "Firing risk after a playoff season"),
  "</div>",
  if (season_in_progress) '<a class="cta" href="#hot-seat">See the hot seat &rarr;</a>' else "",
  "</div></header>")

nav <- paste0(
  '<nav class="topnav"><div class="nav-inner">',
  '<a href="#data">Data</a><a href="#tenure">Tenure</a><a href="#hiring-day">Hiring day</a>',
  '<a href="#ph-check">PH check</a><a href="#season">Season by season</a>',
  '<a href="#exits">Exits</a><a href="#hot-seat" class="hot">Hot seat</a><a href="#limits">Limits</a>',
  "</div></nav>")

## Sections -------------------------------------------------------------------
sec_data <- section("data", "00", "The data",
  sprintf("<p>%d permanent head-coaching stints began in 2000&ndash;%d; %d have at least one completed season and are used for fitting. %d interim-only stints are excluded.</p>",
          nrow(tenure), max(tenure$hire_season), nrow(fit_data), nrow(interim_stints)),
  '<div class="grid2">',
  '<div class="card pad"><h3>Sources</h3><ul>',
  '<li><strong>Games and coaches:</strong> <a href="https://github.com/nflverse/nfldata">nflverse</a> results with the credited head coach, corrected for mid-season changes using Wikipedia departure dates.</li>',
  "<li><strong>Why each coach left:</strong> Wikipedia&rsquo;s yearly &ldquo;NFL season&rdquo; articles, with a quote and URL per departure.</li>",
  "<li><strong>Birth dates:</strong> Wikidata. <strong>Pre-1999 jobs:</strong> coach infoboxes. <strong>GMs and owners:</strong> Wikipedia.</li>",
  "</ul></div>",
  '<div class="card pad"><h3>Terms</h3><dl>',
  "<dt>Fired</dt><dd>The club ended the job: firings, &ldquo;mutual&rdquo; partings, non-renewals, and reassignment out of the role.</dd>",
  "<dt>Censoring</dt><dd>We know a coach lasted at least <em>t</em> seasons unfired, not what came after. Active coaches, resignations and retirements are censored.</dd>",
  "<dt>Hazard</dt><dd>The chance of being fired after season <em>t</em>, given he reached season <em>t</em>.</dd>",
  "<dt>Hazard ratio</dt><dd>How a covariate multiplies that per-season risk. 0.6 = 40% lower risk.</dd>",
  "<dt>Counting process</dt><dd>One row per coach-season, interval (<em>k</em>&minus;1, <em>k</em>], so covariates like win % can change every year.</dd>",
  "</dl></div></div>")

sec_km <- section("tenure", "01", "How long tenures last",
  takeaway(sprintf("Half of coaches are fired by the end of season %s. Only %s make it through season 8.",
                   km_tab[["median"]], pct(s_at$surv[s_at$time == 8]))),
  sprintf("<p>Kaplan&ndash;Meier median: <strong>%s</strong>. First-time NFL head coaches: %s. Previous NFL head coaches: %s. Log-rank test for a difference: p = %s.</p>",
          median_text(km_tab), median_text(km_bg_tb[1, ]), median_text(km_bg_tb[2, ]), pval(lr_p)),
  html_table(tibble(`Seasons` = s_at$time,
                    `At risk` = s_at$n.risk,
                    `Share not yet fired` = pct(s_at$surv),
                    `95% CI` = paste0(pct(s_at$lower), "–", pct(s_at$upper))),
             num_cols = 1:4),
  img("plots/01_km_overall.png", "Kaplan–Meier curve of time until firing, all coaches"),
  img("plots/02_km_by_background.png", "Kaplan–Meier curves by prior NFL head-coaching experience"))

sec_cox <- section("hiring-day", "02", "What&rsquo;s known on hiring day",
  takeaway(sprintf("Age, experience and the team&rsquo;s prior record barely predict tenure: concordance %s, where 0.50 is a coin flip.",
                   num(cox_base$concordance[["concordance"]]))),
  hr_table(cox_base, base_labels),
  img("plots/03_cox_baseline_forest.png", "Forest plot of baseline Cox hazard ratios"))

sec_ph <- section("ph-check", "03", "Checking proportional hazards",
  "<p>The Cox model assumes each hazard ratio is constant across tenure. <code>cox.zph</code> checks whether each covariate&rsquo;s scaled Schoenfeld residuals trend over time; a small p-value means the effect drifts.</p>",
  '<div class="grid2">',
  sprintf('<div><h3>Baseline model</h3><p class="small">Violations: %s.</p>%s</div>',
          flagged(zph), zph_table(zph, all_labels)),
  sprintf('<div><h3>Time-varying model</h3><p class="small">Violations: %s.</p>%s</div>',
          flagged(zph_tv), zph_table(zph_tv, all_labels)),
  "</div>",
  img("plots/04_cox_zph.png", "Scaled Schoenfeld residuals against time for the baseline Cox model"),
  takeaway("Holding this season&rsquo;s win % fixed, a bigger &ldquo;change from last season&rdquo; means last season was worse: the term measures two bad years in a row. Its effect differs early and late in a tenure, so the model splits it."),
  hr_table(cox_tv_split, all_labels),
  sprintf('<p class="small">Split model PH check: global p = %s; violations: %s. The hot-seat estimates use this model.</p>',
          pval(zph_spl$table["GLOBAL", "p"]), flagged(zph_spl)))

sec_tv <- section("season", "04", "Season by season",
  takeaway(sprintf("Win and you stay. Each +10 pts of win %% (about 1.7 wins) cuts that season&rsquo;s firing risk by %s; a playoff trip cuts it by %s. Only %d of %d firings came right after a playoff season.",
                   pct(1 - hr_tv["win10", "exp(coef)"]), pct(1 - hr_tv["playoffTRUE", "exp(coef)"]),
                   n_fired_after_playoffs, n_fired)),
  sprintf("<p>Time-varying Cox model on %d coach-seasons and %d firings, using only what was known when each decision was made. Standard errors clustered by stint. Concordance %s.</p>",
          cox_tv$n, cox_tv$nevent, num(cox_tv$concordance[["concordance"]])),
  hr_table(cox_tv, all_labels),
  img("plots/05_cox_timevarying_forest.png", "Forest plot of time-varying Cox hazard ratios"))

sec_cr <- section("exits", "05", "How tenures end",
  takeaway(sprintf("Firing is the dominant exit: by season 8, %s of coaches have been fired and %s have left another way.",
                   pct(cif["Fired", "8"]), pct(cif["Resigned, retired or other", "8"]))),
  "<p>Cumulative incidence gives the real-world share of coaches who have left each way by season <em>t</em>, allowing for the fact that a coach who retired can no longer be fired.</p>",
  html_table(tibble(`By end of season` = colnames(cif),
                    Fired = pct(cif["Fired", ]),
                    `Resigned, retired or other` = pct(cif["Resigned, retired or other", ])),
             num_cols = 1:3),
  img("plots/06_competing_risks.png", "Cumulative incidence of firing and of other exits"))

## Hot seat: spotlight cards + full board with risk bars -------------------------
ordinal <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th",
                                         c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]))
if (season_in_progress) {
  # Before the playoffs: two scenarios per coach. After: one number, using
  # the team's actual playoff result.
  headline <- coalesce(risk$p_fired, risk$p_fired_if_miss)
  spot <- risk |> mutate(headline = headline) |> slice_head(n = 3)
  spot_label <- if (playoff_field_known) {
    if_else(spot$made_playoffs, "estimated risk &middot; made the playoffs",
            "estimated risk &middot; missed the playoffs")
  } else rep("if they miss the playoffs", nrow(spot))
  spot_alt <- if (playoff_field_known) rep("", nrow(spot)) else
    sprintf('<div class="spot-alt">%s if they make it</div>', pct(spot$p_fired_if_playoff))
  spotlight <- paste0('<div class="spotlight">', paste0(
    sprintf('<div class="spot"><div class="spot-rank">#%d</div><div class="spot-name">%s</div><div class="spot-meta">%s &middot; %s season &middot; %s</div><div class="spot-num">%s</div><div class="spot-label">%s</div>%s</div>',
            seq_len(nrow(spot)), esc(spot$coach), esc(spot$team), ordinal(spot$season_number),
            esc(spot$record), pct(spot$headline), spot_label, spot_alt),
    collapse = ""), "</div>")

  bar <- sprintf('<td class="barcell"><div class="bar"><span style="width:%.1f%%"></span></div><b>%s</b></td>',
                 100 * headline / max(headline), pct(headline))
  if (playoff_field_known) {
    last_cols <- paste0('<td>', if_else(risk$made_playoffs, "Made", "Missed"), "</td>", bar)
    last_head <- '<th>Playoffs</th><th>Estimated risk</th>'
    win_head  <- "Win %"
  } else {
    last_cols <- paste0(bar, '<td class="num muted">', pct(risk$p_fired_if_playoff), "</td>")
    last_head <- '<th>If team misses playoffs</th><th class="num">If makes</th>'
    win_head  <- "Proj. win %"
  }
  board_rows <- sprintf(
    '<tr><td class="num rank">%d</td><td><strong>%s</strong></td><td><span class="team">%s</span></td><td class="num">%d</td><td class="num">%s</td><td class="num">%s</td>%s</tr>',
    seq_len(nrow(risk)), esc(risk$coach), esc(risk$team), risk$season_number,
    esc(risk$record), num(risk$proj_win_pct, 3), last_cols)
  board <- paste0(
    '<div class="table-wrap card"><table class="board"><thead><tr>',
    '<th class="num">#</th><th>Coach</th><th>Team</th><th class="num">Season</th><th class="num">Record</th>',
    '<th class="num">', win_head, '</th>', last_head,
    "</tr></thead><tbody>", paste(board_rows, collapse = ""), "</tbody></table></div>")

  # Teams whose latest-game coach has no rating, and why.
  unrated <- current_coaches |>
    anti_join(risk, by = c("team", "coach")) |>
    mutate(why = if_else(latest < CURRENT_SEASON, "no game yet this season",
                         "interim coach, or already left"))
  unrated_note <- if (nrow(unrated) == 0) "" else sprintf(
    '<p class="small">Not rated: %s. Coaches without a model row this season (and those whose predecessor left without a departure record yet) aren&rsquo;t rated.</p>',
    paste0(esc(unrated$team), " (", esc(unrated$coach), ": ", unrated$why, ")", collapse = ", "))

  intro <- if (playoff_field_known) {
    sprintf("<p>The %d regular season is over, so each estimate uses the team&rsquo;s final record and its actual playoff result.</p>",
            CURRENT_SEASON)
  } else {
    sprintf("<p>Each team&rsquo;s %d season is projected from results through week %d plus .500 football and an even point differential the rest of the way. Playoffs aren&rsquo;t decided, so each coach gets two numbers.</p>",
            CURRENT_SEASON, week_through)
  }
  sec_now <- section("hot-seat", "06", sprintf("The hot seat: risk of being fired after %d", CURRENT_SEASON),
    intro,
    spotlight,
    '<p class="note"><strong>Model estimates, not predictions.</strong> The model knows wins, playoffs, tenure, age, experience and front-office turnover. It knows nothing about Super Bowl rings, contracts, quarterback injuries or owner temperament, and the baseline risk for very long tenures is pooled and imprecise.</p>',
    board,
    unrated_note,
    img("plots/07_current_coach_risk.png", "Estimated chance each current coach is fired after the season"))
} else {
  sec_now <- section("hot-seat", "06", "The hot seat",
    sprintf('<div class="card pad"><p>No season is in progress. Ratings for the %d season return once it kicks off.</p></div>',
            CURRENT_SEASON))
}

sec_lim <- section("limits", "07", "Limitations",
  '<div class="card pad"><ul>',
  sprintf("<li>Small sample: %d firings, so intervals for anything beyond winning and playoffs are wide.</li>", n_fired),
  "<li>Seasons are a coarse time scale with many ties (Efron&rsquo;s method is used).</li>",
  "<li>Some coding is judgment: which partings count as firings, who held final personnel say without a conventional GM, and dates known only to the month or year. The research files keep the raw wording so these can be re-coded.</li>",
  "</ul></div>")

css <- r"---(
:root {
  --navy-950:#050c17; --navy-900:#081427; --navy-800:#0d1f3c; --navy-700:#16305a;
  --field:#0f3d2c; --gold:#d6a830; --gold-hi:#f3cf6b; --hot:#eb6834; --cool:#2a78d6;
  --bg:#f3f1ea; --card:#ffffff; --text:#121722; --muted:#5a6170; --border:#e1ddd1;
  --row:#f8f6f0; --note:#fbf3df;
  --display: "Oswald", "Bebas Neue", "Barlow Condensed", "Roboto Condensed",
             "Arial Narrow", "Liberation Sans Narrow", "Helvetica Neue", Arial, sans-serif;
  color-scheme: light;
}
@media (prefers-color-scheme: dark) {
  :root { --bg:#07111f; --card:#0d1c33; --text:#eef1f6; --muted:#a6b1c3;
          --border:#1e3354; --row:#102340; --note:#1c2a3d; color-scheme: dark; }
}
* { box-sizing: border-box; }
html { scroll-behavior: smooth; }
body { margin:0; background:var(--bg); color:var(--text);
       font: 16px/1.6 system-ui, -apple-system, "Segoe UI", Roboto, sans-serif; }
a { color: var(--cool); }
code { font-size: .9em; }

/* Hero: night-game navy over a striped field, gold rule underneath */
.hero { color:#fff; border-bottom: 6px solid var(--gold);
  background:
    radial-gradient(1200px 400px at 85% -10%, rgba(243,207,107,.18), transparent 60%),
    repeating-linear-gradient(90deg, transparent 0 calc(10% - 1px), rgba(255,255,255,.07) calc(10% - 1px) 10%),
    linear-gradient(180deg, var(--navy-900) 0%, var(--navy-800) 62%, var(--field) 100%); }
.hero-inner { max-width: 1040px; margin: 0 auto; padding: 56px 16px 40px; }
.kicker { font-family: var(--display); font-stretch: condensed; text-transform: uppercase;
  letter-spacing: .22em; color: var(--gold-hi); font-size: .85rem; margin: 0 0 10px; }
.hero h1 { font-family: var(--display); font-stretch: condensed; text-transform: uppercase;
  font-weight: 800; line-height: 1.02; letter-spacing: .01em;
  font-size: clamp(2.2rem, 6.5vw, 4.2rem); margin: 0 0 14px; max-width: 14ch; }
.hero-sub { color: rgba(255,255,255,.82); max-width: 62ch; margin: 0 0 28px; }
.scoreboard { display:grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 12px; }
.stat { background: rgba(3,8,16,.55); border: 1px solid rgba(255,255,255,.12);
  border-top: 3px solid var(--gold); border-radius: 10px; padding: 14px 16px; }
.stat-num { font-family: var(--display); font-stretch: condensed; font-weight: 700;
  font-size: 2.6rem; line-height: 1.05; color: var(--gold-hi); font-variant-numeric: tabular-nums; }
.stat-label { text-transform: uppercase; letter-spacing: .08em; font-size: .72rem;
  color: rgba(255,255,255,.75); margin-top: 4px; }
.cta { display:inline-block; margin-top: 24px; padding: 10px 18px; border-radius: 999px;
  background: var(--gold); color: var(--navy-950); font-weight: 700; text-decoration: none;
  text-transform: uppercase; letter-spacing: .06em; font-size: .85rem; }
.cta:hover { background: var(--gold-hi); }

/* Sticky section nav */
.topnav { position: sticky; top: 0; z-index: 10; background: var(--navy-950);
  border-bottom: 1px solid rgba(255,255,255,.08); }
.nav-inner { max-width: 1040px; margin: 0 auto; padding: 0 16px; display: flex; gap: 4px;
  overflow-x: auto; scrollbar-width: none; }
.topnav a { color: rgba(255,255,255,.78); text-decoration: none; white-space: nowrap;
  font-family: var(--display); font-stretch: condensed; text-transform: uppercase;
  letter-spacing: .1em; font-size: .82rem; padding: 12px 10px; border-bottom: 3px solid transparent; }
.topnav a:hover { color: #fff; border-bottom-color: var(--gold); }
.topnav a.hot { color: var(--gold-hi); }

main { max-width: 1040px; margin: 0 auto; padding: 8px 16px 64px; }
section { padding-top: 48px; scroll-margin-top: 48px; }
.sec-head { display:flex; align-items: center; gap: 14px; margin-bottom: 14px; }
.sec-num { font-family: var(--display); font-stretch: condensed; font-weight: 800;
  font-size: 3.2rem; line-height: 1; color: transparent; -webkit-text-stroke: 2px var(--gold);
  font-variant-numeric: tabular-nums; }
h2 { font-family: var(--display); font-stretch: condensed; text-transform: uppercase;
  font-weight: 700; letter-spacing: .02em; font-size: clamp(1.4rem, 3.4vw, 2rem);
  line-height: 1.1; margin: 0; }
h3 { font-family: var(--display); font-stretch: condensed; text-transform: uppercase;
  letter-spacing: .06em; font-size: 1rem; margin: 0 0 8px; color: var(--muted); }
.takeaway { font-size: 1.12rem; font-weight: 600; border-left: 5px solid var(--gold);
  padding: 4px 0 4px 14px; margin: 8px 0 18px; }
.small { font-size: .9rem; color: var(--muted); }
.card { background: var(--card); border: 1px solid var(--border); border-radius: 12px;
  box-shadow: 0 1px 2px rgba(0,0,0,.04), 0 8px 24px rgba(8,20,39,.06); }
.pad { padding: 16px 18px; }
.pad ul { margin: 0; padding-left: 20px; }
.grid2 { display:grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 16px; margin: 16px 0; }
.fig { margin: 20px 0; padding: 12px; background: #fff; }
.fig img { width: 100%; height: auto; display: block; }
figcaption { color: #5a6170; font-size: .85rem; padding-top: 8px; }
dl { margin: 0; } dt { font-weight: 700; margin-top: 10px; } dt:first-child { margin-top: 0; }
dd { margin: 2px 0 0; color: var(--muted); }
.note { background: var(--note); border-left: 5px solid var(--hot); border-radius: 8px; padding: 12px 14px; }

/* Tables */
.table-wrap { overflow-x: auto; margin: 16px 0; width: fit-content; max-width: 100%; }
.table-wrap:has(.board) { width: auto; }
table { border-collapse: collapse; width: 100%; font-size: .92rem; }
caption { text-align: left; color: var(--muted); padding: 10px 12px 0; }
th { background: var(--navy-800); color: #fff; font-family: var(--display); font-stretch: condensed;
  text-transform: uppercase; letter-spacing: .08em; font-weight: 600; font-size: .78rem; }
th, td { padding: 8px 12px; text-align: left; white-space: nowrap; }
td { border-bottom: 1px solid var(--border); }
tbody tr:nth-child(even) td { background: var(--row); }
tbody tr:last-child td { border-bottom: 0; }
td.num, th.num { text-align: right; font-variant-numeric: tabular-nums; }
.muted { color: var(--muted); }

/* Hot seat */
.spotlight { display:grid; grid-template-columns: repeat(auto-fit, minmax(230px, 1fr)); gap: 14px; margin: 20px 0; }
.spot { position: relative; overflow: hidden; color: #fff; border-radius: 14px; padding: 18px 18px 16px;
  background: linear-gradient(160deg, var(--navy-800), var(--navy-950));
  border: 1px solid rgba(255,255,255,.1); border-top: 4px solid var(--hot); }
.spot-rank { position: absolute; right: 14px; top: 6px; font-family: var(--display); font-stretch: condensed;
  font-weight: 800; font-size: 3.4rem; color: rgba(255,255,255,.08); }
.spot-name { font-family: var(--display); font-stretch: condensed; text-transform: uppercase;
  font-weight: 700; font-size: 1.45rem; line-height: 1.1; }
.spot-meta { color: rgba(255,255,255,.7); font-size: .85rem; margin: 2px 0 12px; }
.spot-num { font-family: var(--display); font-stretch: condensed; font-weight: 800;
  font-size: 3.2rem; line-height: 1; color: var(--hot); font-variant-numeric: tabular-nums; }
.spot-label { text-transform: uppercase; letter-spacing: .08em; font-size: .72rem; color: rgba(255,255,255,.75); }
.spot-alt { margin-top: 8px; font-size: .85rem; color: #9cc3f2; }
.board td.rank { color: var(--muted); width: 2.5em; }
.team { display: inline-block; min-width: 3.2em; text-align: center; font-weight: 700; font-size: .78rem;
  letter-spacing: .06em; padding: 2px 6px; border-radius: 4px; background: var(--navy-800); color: #fff;
  box-shadow: inset 0 0 0 1px rgba(214,168,48,.45); }
.barcell { display: flex; align-items: center; gap: 10px; min-width: 220px; }
.barcell b { font-variant-numeric: tabular-nums; min-width: 3em; text-align: right; }
.bar { flex: 1; height: 10px; border-radius: 999px; background: var(--border); overflow: hidden; }
.bar span { display: block; height: 100%; border-radius: 999px;
  background: linear-gradient(90deg, var(--gold), var(--hot)); }

footer { background: var(--navy-950); color: rgba(255,255,255,.6); border-top: 4px solid var(--gold); }
.foot-inner { max-width: 1040px; margin: 0 auto; padding: 20px 16px; font-size: .85rem; }
)---"

report <- paste0(
  '<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  "<title>NFL Coach Tenure</title>",
  '<meta name="description" content="Survival analysis of how long NFL head coaches hired since 2000 last before being fired, and who is on the hot seat now.">',
  '<meta name="theme-color" content="#081427">',
  "<style>", css, "</style></head><body>",
  hero, nav, "<main>",
  sec_data, sec_km, sec_cox, sec_ph, sec_tv, sec_cr, sec_now, sec_lim,
  "</main>",
  sprintf('<footer><div class="foot-inner">Generated %s by coach_tenure.R &middot; Sources: nflverse, Wikipedia, Wikidata &middot; Model estimates, not predictions. Not affiliated with the NFL or any team.</div></footer>',
          long_date(Sys.Date())),
  "</body></html>")

writeLines(report, "output/coach_tenure_report.html", useBytes = TRUE)
cat("\nHTML report: output/coach_tenure_report.html (",
    round(file.size("output/coach_tenure_report.html") / 1024), "KB )\n", sep = "")

cat("\nDone. Plots in plots/, tables in data/ and output/.\n")
