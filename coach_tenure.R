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
#   * nflverse games file: https://github.com/nflverse/nfldata (data/games.csv)
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
# Package substitutions: none were needed. Everything used (survival,
# survminer, tidyverse, cmprsk, base64enc, htmltools) is installed. cmprsk
# computes the competing-risks cumulative incidence; base64enc and htmltools
# embed the plots and escape text in the HTML report (no pandoc needed).
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

needed <- c("survival", "survminer", "tidyverse", "cmprsk", "base64enc", "htmltools")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  stop("These packages are not installed (and this script will not install ",
       "them): ", paste(missing_pkgs, collapse = ", "))
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

# Analysis window. The 2026 season is in progress, so for model *fitting*
# every stint is administratively censored at the end of the 2025 season:
# we know who survived 2025, but nobody has finished 2026 yet.
LAST_COMPLETE_SEASON <- 2025
CURRENT_SEASON       <- 2026

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
games_file <- "data/raw/games.csv"
if (!file.exists(games_file)) {
  download.file("https://github.com/nflverse/nfldata/raw/master/data/games.csv",
                games_file, mode = "wb", quiet = TRUE)
}
games <- read_csv(games_file, show_col_types = FALSE) |>
  filter(!is.na(home_score))                 # games already played

relocated <- c(OAK = "LV", SD = "LAC", STL = "LA")   # one code per franchise

# One row per team per game.
team_games <- bind_rows(
  games |> transmute(game_id, season, game_type, gameday, team = home_team,
                     coach = home_coach, pf = home_score, pa = away_score),
  games |> transmute(game_id, season, game_type, gameday, team = away_team,
                     coach = away_coach, pf = away_score, pa = home_score)
) |>
  mutate(team    = coalesce(relocated[team], team),
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

current_coaches <- team_games |>
  filter(season == CURRENT_SEASON) |>
  slice_max(gameday, by = team, with_ties = FALSE) |>
  select(team, coach)

stints_all <- cts |>
  group_by(stint_id, coach, team) |>
  summarise(any_start      = any(started),
            first_season   = min(season),
            hire_season    = if (any(started)) min(season[started]) else NA_real_,
            last_season    = max(season),
            ended_midseason = !finished[which.max(season)],
            .groups = "drop") |>
  mutate(interim_before = any_start & first_season < hire_season,
         active = paste(coach, team) %in% paste(current_coaches$coach, current_coaches$team) &
           last_season == CURRENT_SEASON)

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
    event_type = case_when(
      active                        ~ "active",
      forced                        ~ "fired",
      reason_category == "resigned" ~ "resigned",
      reason_category == "retired"  ~ "retired",
      !is.na(reason_category)       ~ "other",   # traded (Gruden 2002, Edwards 2006)
      TRUE                          ~ NA_character_),
    strict_fired = reason_category == "fired")

unmatched <- stints |> filter(is.na(event_type))
if (nrow(unmatched) > 0) {
  print(unmatched |> select(coach, team, hire_season, last_season))
  stop("Some ended stints have no departure record in coach_changes.csv")
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
         # status = 1 only for a firing; everything else is censored
         status  = as.integer(event_type == "fired" &
                                last_season <= LAST_COMPLETE_SEASON),
         # a stint that ended in 2026 (in progress) is censored at 2025
         event_type = if_else(last_season > LAST_COMPLETE_SEASON &
                                event_type != "active", "active", event_type)) |>
  arrange(hire_season, team)

# Event flag on the season row in which the firing happened (the stint's
# last season). Seasons after 2025 are not used for fitting.
coach_seasons <- coach_seasons |>
  left_join(tenure |> select(stint_id, status, last_season, event_type),
            by = "stint_id") |>
  mutate(fired_after = as.integer(status == 1 & season == last_season)) |>
  select(-status)

write_csv(tenure |> select(coach, team, hire_season, last_season, seasons,
                           event_type, status, ended_midseason, background,
                           prior_hc, prior_interim_only, interim_before,
                           age_at_hire, prev_win_pct, reason_raw, source_url),
          "data/tenure.csv")
write_csv(coach_seasons |> select(coach, team, season, tenure_season, g_reg,
                                  w, l, t, win_pct, win_pct_change,
                                  pd_per_game, playoff, new_gm, new_owner,
                                  new_gm_or_owner, fired_after),
          "data/coach_seasons.csv")
write_csv(interim_stints, "data/interim_stints.csv")

fit_data <- tenure |> filter(seasons > 0)  # 2026 hires have no completed season
cat("\nStints (hires 2000+):", nrow(tenure),
    "| used for fitting:", nrow(fit_data),
    "| interim-only stints excluded:", nrow(interim_stints), "\n")
print(count(tenure, event_type))


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
  title = "NFL head coaches hired 2000–2025: time until firing",
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
  mutate(fstatus = case_when(event_type == "fired" ~ 1L,
                             event_type == "active" ~ 0L,
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
       subtitle = "Cumulative incidence (Aalen–Johansen), hires 2000–2025",
       x = "Seasons as head coach", y = "Share of coaches who have left this way") +
  theme(legend.position = "top", legend.justification = "left")
ggsave("plots/06_competing_risks.png", p_ci, width = 9, height = 5.5, dpi = 150)


# ---- 7. Current coaches: model risk of firing after the 2026 season ---------
# For each current head coach, build the row for his tenure season k = 2026
# using the 2026 season *to date* ("if the season ended today"). The playoff
# outcome isn't known yet, so we show two scenarios: misses / makes playoffs.
#
# P(fired after season k) = 1 − exp(−ΔH0(k) · exp(x'b)), where ΔH0(k) is the
# Cox baseline hazard for tenure season k. Few coaches reach long tenures, so
# ΔH0 is pooled (averaged) for seasons 8 and later. We use the split model
# from section 5, since it satisfies the proportional-hazards assumption.

bh <- basehaz(cox_tv_split, centered = FALSE) |>
  as_tibble() |>
  mutate(k = time, dH0 = hazard - lag(hazard, default = 0)) |>
  select(k, dH0)
bh <- tibble(k = 1:40) |> left_join(bh, by = "k") |>
  mutate(dH0 = coalesce(dH0, 0))
tail_rate <- mean(bh$dH0[bh$k >= 8 & bh$k <= max(tv_data$tstop)])
bh <- bh |> mutate(dH0 = if_else(k >= 8, tail_rate, dH0))

# Five or six games are a noisy sample, and the model was fit on full
# seasons: plugging in a raw 0-5 would treat it like 0-17. So the 2026 inputs
# are a *projected* full season: actual results so far, plus .500 football
# and an even point differential over the remaining games of 17.
SEASON_GAMES <- 17
current <- coach_seasons |>
  filter(season == CURRENT_SEASON) |>
  semi_join(current_coaches, by = c("coach", "team")) |>
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

risk <- current |>
  mutate(record = paste0(w, "-", l, if_else(t > 0, paste0("-", t), "")),
         p_fired_if_miss    = risk_for(current, FALSE),
         p_fired_if_playoff = risk_for(current, TRUE)) |>
  arrange(desc(p_fired_if_miss)) |>
  transmute(coach, team, season_number = tenure_season, record_2026 = record,
            games_played = g_reg,
            proj_win_pct = round(win_pct, 3),
            proj_win_pct_change = round(win_pct_change, 3),
            pd_to_date = pf - pa,
            new_gm_or_owner, prior_hc,
            p_fired_if_miss = round(p_fired_if_miss, 3),
            p_fired_if_playoff = round(p_fired_if_playoff, 3))

write_csv(risk, "output/current_coach_risk_2026.csv")
cat("\n== Model-estimated probability of being fired after 2026 ==\n")
cat("(projected season: results to date + .500 the rest of the way; estimates, not forecasts)\n")
print(risk, n = 32)

p_risk <- risk |>
  mutate(label = paste0(coach, " (", team, ", ", record_2026, ")"),
         label = fct_reorder(label, p_fired_if_miss)) |>
  ggplot(aes(y = label)) +
  geom_segment(aes(x = p_fired_if_playoff, xend = p_fired_if_miss, yend = label),
               colour = "grey75", linewidth = 0.8) +
  geom_point(aes(x = p_fired_if_playoff, colour = "Makes playoffs"), size = 2.6) +
  geom_point(aes(x = p_fired_if_miss, colour = "Misses playoffs"), size = 2.6) +
  scale_colour_manual(values = c("Makes playoffs" = pal[1], "Misses playoffs" = pal[2]),
                      name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Model-estimated chance of being fired after the 2026 season",
       subtitle = "Time-varying Cox model. 2026 = results to date + .500 the rest of the way. Model estimates, not predictions.",
       x = "Estimated probability", y = NULL) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank())
ggsave("plots/07_current_coach_risk.png", p_risk, width = 9, height = 9, dpi = 150)



# ---- 8. Self-contained HTML report -------------------------------------------
# One HTML file with the plots embedded as base64 PNGs and no external
# scripts, stylesheets or fonts, so it can be copied to any web server (or
# opened locally) as-is. All numbers below are computed from the models
# above, so the page stays in step with the data each time the script runs.

esc <- function(x) htmltools::htmlEscape(as.character(x))
pct <- function(x, d = 0) sprintf(paste0("%.", d, "f%%"), 100 * x)
num <- function(x, d = 2) formatC(x, format = "f", digits = d)
pval <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))

img <- function(path, alt) {
  sprintf('<figure><img src="data:image/png;base64,%s" alt="%s"></figure>',
          base64enc::base64encode(path), esc(alt))
}

# A data frame as an HTML table; `num_cols` are right-aligned.
html_table <- function(df, caption = NULL, num_cols = integer()) {
  cls <- ifelse(seq_along(df) %in% num_cols, ' class="num"', "")
  head <- paste0("<th", cls, ">", esc(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, \(r) paste0("<tr>", paste0("<td", cls, ">", esc(r), "</td>",
                                                  collapse = ""), "</tr>"))
  paste0('<div class="table-wrap"><table>',
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
data_through <- max(team_games$gameday[team_games$season == CURRENT_SEASON])
week_through <- max(games$week[games$season == CURRENT_SEASON & games$game_type == "REG"])
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

## Sections -------------------------------------------------------------------
tiles <- paste0(
  '<div class="tiles">',
  sprintf('<div class="tile"><div class="tile-num">%s</div><div class="tile-label">median seasons until a coach is fired</div></div>',
          km_tab[["median"]]),
  sprintf('<div class="tile"><div class="tile-num">%s</div><div class="tile-label">of coaches fired by the end of season 4</div></div>',
          pct(cif["Fired", "4"])),
  sprintf('<div class="tile"><div class="tile-num">%s</div><div class="tile-label">lower firing risk per +10 pts of win %%</div></div>',
          pct(1 - hr_tv["win10", "exp(coef)"])),
  sprintf('<div class="tile"><div class="tile-num">%s</div><div class="tile-label">lower firing risk after a playoff season</div></div>',
          pct(1 - hr_tv["playoffTRUE", "exp(coef)"])),
  "</div>")

sec_data <- paste0(
  "<h2>Data</h2>",
  sprintf("<p>%d permanent head-coaching stints began in 2000–%d; %d have at least one completed season and are used for fitting (%d firings). %d interim-only stints are excluded. Game data runs through %s (week %d of the %d season).</p>",
          nrow(tenure), CURRENT_SEASON, nrow(fit_data), n_fired,
          nrow(interim_stints), format(data_through, "%B %-d, %Y"), week_through, CURRENT_SEASON),
  "<ul>",
  '<li><strong>Games and coaches:</strong> <a href="https://github.com/nflverse/nfldata">nflverse</a> game results with the credited head coach, corrected for mid-season changes using departure dates from Wikipedia.</li>',
  "<li><strong>Why each coach left:</strong> Wikipedia's yearly &ldquo;NFL season&rdquo; articles, with a quote and URL per departure.</li>",
  "<li><strong>Birth dates:</strong> Wikidata. <strong>Pre-1999 head-coaching jobs:</strong> coach Wikipedia infoboxes. <strong>GM and owner tenures:</strong> Wikipedia.</li>",
  "</ul>",
  "<h3>Terms</h3><dl>",
  "<dt>Fired</dt><dd>The club ended the job, including &ldquo;mutual&rdquo; partings, contracts the club declined to renew, and a reassignment out of the head-coach role.</dd>",
  "<dt>Censoring</dt><dd>We know a coach lasted at least <em>t</em> seasons without being fired, but not what came after. Active coaches, resignations and retirements are censored.</dd>",
  "<dt>Hazard</dt><dd>The chance of being fired after season <em>t</em>, given the coach reached season <em>t</em>.</dd>",
  "<dt>Hazard ratio (HR)</dt><dd>How a covariate multiplies that per-season risk. HR 0.6 means 40% lower risk in any given season, other things equal.</dd>",
  "<dt>Counting-process format</dt><dd>One row per coach per season, written as an interval (<em>k</em>&minus;1, <em>k</em>], carrying that season's covariates and an event flag that is 1 only in the season he was fired. This lets covariates such as win % change over time.</dd>",
  "</dl>")

sec_km <- paste0(
  "<h2>1. How long tenures last</h2>",
  sprintf("<p>Kaplan–Meier median: <strong>%s</strong>. First-time NFL head coaches: %s. Previous NFL head coaches: %s. Log-rank test for a difference: p = %s.</p>",
          median_text(km_tab), median_text(km_bg_tb[1, ]), median_text(km_bg_tb[2, ]), pval(lr_p)),
  html_table(tibble(`Seasons` = s_at$time,
                    `At risk` = s_at$n.risk,
                    `Share not yet fired` = pct(s_at$surv),
                    `95% CI` = paste0(pct(s_at$lower), "–", pct(s_at$upper))),
             num_cols = 1:4),
  img("plots/01_km_overall.png", "Kaplan–Meier curve of time until firing, all coaches"),
  img("plots/02_km_by_background.png", "Kaplan–Meier curves by prior NFL head-coaching experience"))

sec_cox <- paste0(
  "<h2>2. What's known on hiring day</h2>",
  sprintf("<p>Cox model with baseline covariates. Concordance %s (0.5 is a coin flip).</p>",
          num(cox_base$concordance[["concordance"]])),
  hr_table(cox_base, base_labels),
  img("plots/03_cox_baseline_forest.png", "Forest plot of baseline Cox hazard ratios"))

sec_ph <- paste0(
  "<h2>3. Checking proportional hazards</h2>",
  "<p>The Cox model assumes each hazard ratio is constant across tenure. <code>cox.zph</code> tests this by checking whether each covariate's scaled Schoenfeld residuals trend over time; a small p-value means the effect drifts.</p>",
  sprintf("<p><strong>Baseline model</strong> — violations: %s.</p>", flagged(zph)),
  zph_table(zph, all_labels),
  img("plots/04_cox_zph.png", "Scaled Schoenfeld residuals against time for the baseline Cox model"),
  sprintf("<p><strong>Time-varying model</strong> — violations: %s.</p>", flagged(zph_tv)),
  zph_table(zph_tv, all_labels),
  "<p>Holding this season's win % fixed, a bigger &ldquo;change from last season&rdquo; means last season was worse, so that term measures two bad years in a row. Letting its effect differ between seasons 1–2 (when last season was usually the previous coach's team) and season 3 onward:</p>",
  hr_table(cox_tv_split, all_labels),
  sprintf("<p>Proportional-hazards check for the split model: global p = %s; violations: %s. The current-coach estimates in section 6 use this split model.</p>",
          pval(zph_spl$table["GLOBAL", "p"]), flagged(zph_spl)))

sec_tv <- paste0(
  "<h2>4. Season by season</h2>",
  sprintf("<p>Time-varying Cox model on %d coach-seasons and %d firings, using only what was known when each decision was made. Standard errors are clustered by stint. Concordance %s.</p>",
          cox_tv$n, cox_tv$nevent, num(cox_tv$concordance[["concordance"]])),
  hr_table(cox_tv, all_labels),
  sprintf("<p>Each extra 10 points of win %% (about 1.7 wins) multiplies that season's firing risk by %s; a playoff appearance multiplies it by %s. Only %d of %d firings came right after a playoff season.</p>",
          num(hr_tv["win10", "exp(coef)"]), num(hr_tv["playoffTRUE", "exp(coef)"]),
          n_fired_after_playoffs, n_fired),
  img("plots/05_cox_timevarying_forest.png", "Forest plot of time-varying Cox hazard ratios"))

sec_cr <- paste0(
  "<h2>5. How tenures end</h2>",
  "<p>Cumulative incidence gives the real-world share of coaches who have left each way by season <em>t</em>, allowing for the fact that a coach who retired can no longer be fired.</p>",
  html_table(tibble(`By end of season` = colnames(cif),
                    Fired = pct(cif["Fired", ]),
                    `Resigned, retired or other` = pct(cif["Resigned, retired or other", ])),
             num_cols = 1:3),
  img("plots/06_competing_risks.png", "Cumulative incidence of firing and of other exits"))

sec_now <- paste0(
  sprintf("<h2>6. Current coaches: estimated risk of being fired after %d</h2>", CURRENT_SEASON),
  sprintf("<p>Each team's %d season is projected from results through week %d plus .500 football and an even point differential the rest of the way. Playoffs aren't decided yet, so each coach gets two numbers.</p>",
          CURRENT_SEASON, week_through),
  '<p class="note"><strong>Model estimates, not predictions.</strong> The model knows wins, playoffs, tenure, age, experience and front-office turnover. It knows nothing about Super Bowl rings, contracts, quarterback injuries or owner temperament, and the baseline risk for very long tenures is pooled and imprecise.</p>',
  img("plots/07_current_coach_risk.png", "Estimated chance each current coach is fired after the season"),
  html_table(risk |>
               transmute(Coach = coach, Team = team, Season = season_number,
                         Record = record_2026,
                         `Projected win %` = num(proj_win_pct, 3),
                         `If team misses playoffs` = pct(p_fired_if_miss),
                         `If team makes playoffs` = pct(p_fired_if_playoff)),
             num_cols = 3:7))

sec_lim <- paste0(
  "<h2>Limitations</h2><ul>",
  sprintf("<li>Small sample: %d firings, so intervals for anything beyond winning and playoffs are wide.</li>", n_fired),
  "<li>Seasons are a coarse time scale with many ties (Efron's method is used).</li>",
  "<li>Some coding is judgment: which partings count as firings, who held final personnel say in teams without a conventional GM, and dates known only to the month or year. The research files keep the raw wording so these can be re-coded.</li>",
  "</ul>")

css <- r"---(
:root { --bg:#fcfcfb; --surface:#ffffff; --text:#1f1f1e; --muted:#5f5e5a;
        --border:#e4e3df; --accent:#2a78d6; --note:#f6f1e7; color-scheme: light; }
@media (prefers-color-scheme: dark) {
  :root { --bg:#1a1a19; --surface:#242423; --text:#f0efec; --muted:#c3c2b7;
          --border:#3a3a38; --accent:#3987e5; --note:#2e2a22; color-scheme: dark; }
}
* { box-sizing: border-box; }
body { margin:0; background:var(--bg); color:var(--text);
       font: 16px/1.6 system-ui, -apple-system, "Segoe UI", Roboto, sans-serif; }
main { max-width: 900px; margin: 0 auto; padding: 32px 16px 64px; }
h1 { font-size: 1.9rem; line-height: 1.2; margin: 0 0 8px; }
h2 { font-size: 1.35rem; margin: 48px 0 12px; padding-top: 8px; border-top: 1px solid var(--border); }
h3 { font-size: 1.05rem; margin: 24px 0 8px; }
.lede { color: var(--muted); margin: 0 0 24px; }
a { color: var(--accent); }
code { font-size: 0.9em; }
.tiles { display:grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap:12px; margin: 24px 0; }
.tile { background:var(--surface); border:1px solid var(--border); border-radius:10px; padding:14px 16px; }
.tile-num { font-size: 1.8rem; font-weight: 650; font-variant-numeric: tabular-nums; }
.tile-label { color: var(--muted); font-size: 0.9rem; line-height: 1.35; }
figure { margin: 20px 0; }
figure img { width:100%; height:auto; display:block; border-radius:8px;
             border:1px solid var(--border); background:#fff; }
.table-wrap { overflow-x:auto; margin: 16px 0; }
table { border-collapse: collapse; width: 100%; font-size: 0.92rem; }
caption { text-align:left; color:var(--muted); padding-bottom:6px; }
th, td { padding: 6px 10px; border-bottom: 1px solid var(--border); text-align:left; white-space: nowrap; }
th { font-weight: 600; color: var(--muted); }
td.num, th.num { text-align: right; font-variant-numeric: tabular-nums; }
dl dt { font-weight: 600; margin-top: 10px; }
dl dd { margin: 2px 0 0 0; color: var(--muted); }
.note { background: var(--note); border-radius: 8px; padding: 12px 14px; }
footer { margin-top: 48px; color: var(--muted); font-size: 0.85rem; }
)---"

report <- paste0(
  '<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  "<title>NFL Coach Tenure</title>",
  '<meta name="description" content="Survival analysis of how long NFL head coaches hired since 2000 last before being fired.">',
  "<style>", css, "</style></head><body><main>",
  "<h1>How long do NFL head coaches last?</h1>",
  sprintf('<p class="lede">A survival analysis of head coaches hired %d–%d, with data through week %d of the %d season.</p>',
          min(tenure$hire_season), CURRENT_SEASON, week_through, CURRENT_SEASON),
  tiles, sec_data, sec_km, sec_cox, sec_ph, sec_tv, sec_cr, sec_now, sec_lim,
  sprintf("<footer>Generated %s by coach_tenure.R. Sources: nflverse, Wikipedia, Wikidata.</footer>",
          format(Sys.Date(), "%B %-d, %Y")),
  "</main></body></html>")

writeLines(report, "output/coach_tenure_report.html", useBytes = TRUE)
cat("\nHTML report: output/coach_tenure_report.html (",
    round(file.size("output/coach_tenure_report.html") / 1024), "KB )\n", sep = "")

cat("\nDone. Plots in plots/, tables in data/ and output/.\n")
