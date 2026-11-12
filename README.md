# NFL head coach tenure: a survival analysis

How long do NFL head coaches last before they're fired, what predicts it, and who's on the hot seat right now? This project answers those questions with survival analysis in R, for every permanent head coach hired since 2000.

One script, `coach_tenure.R`, does everything:

1. Builds a stint-level and a season-level dataset from game results and sourced research files.
2. Fits Kaplan–Meier curves, overall and by prior head-coaching experience.
3. Fits a Cox proportional-hazards model on hiring-day traits, then checks the proportional-hazards assumption with `cox.zph`.
4. Fits a time-varying Cox model in counting-process format (win %, playoffs, point differential, front-office changes), using no information from after the decision.
5. Treats resignations and retirements as competing risks (cumulative incidence).
6. Estimates each current coach's chance of being fired after the season in progress.
7. Writes a self-contained HTML report you can put on any web server.

The findings are written up in [WRITEUP.md](WRITEUP.md). Its numbers are a snapshot from the 2026 season through week 6.

## Running it

Requires R 4.1 or later and these packages: `survival` (one whose `predict.coxph()` supports `reference = "zero"`), `survminer`, `cmprsk`, `base64enc`, `htmltools`, and from the tidyverse `dplyr` (≥ 1.1.0), `readr` (≥ 2.0.0), `ggplot2` (≥ 3.4.0), `stringr`, `forcats`, `purrr`, `tibble` and `lubridate`. The full `tidyverse` install covers all of those. The script checks all of this before running and stops with a list of anything missing or too old; it never installs anything. Each run logs the R and package versions it used.

From the project root:

```bash
Rscript coach_tenure.R
```

It takes about 30 seconds. On first run, and whenever the cached copy is more than a day old, it downloads the nflverse games file into `data/raw/`. A download never replaces the cached copy if it fails, isn't the games file, or looks cut short next to the copy already there (its results stop earlier, or it has over 5% fewer games); the run carries on with the cached copy and warns.

It runs at any time of year. The season settings come from the data: the last complete season is the latest one whose Super Bowl has been played, and the next season counts as in progress once it has results. The hot-seat ratings only appear while a season is in progress; in the offseason the report says when they'll return. Until the playoff field is set, each coach gets two numbers (if his team misses or makes the playoffs). Once the regular season is over and the wild-card games are scheduled, the estimate uses the team's actual playoff result. Teams with first-round byes don't appear in the schedule until the divisional round. Until then, an outright division leader who isn't playing a wild-card game counts as having a bye, since every division winner qualifies. Divisions are read from the schedule's division games, so a realignment is picked up automatically; if those games don't form consistent divisions, the bye inference is skipped. If that doesn't account for all the byes in the previous season's format (a division decided on tiebreakers, or a new format), the status waits until the divisional round is scheduled. In a season's opening days, teams that haven't played yet are listed as not rated.

## Serving the report with Docker

`compose.yaml` runs two containers:
- **`refresh`** runs the script when it starts and then once a day, and publishes the report.
- **`web`** is nginx serving the latest report at `http://localhost:8080`.

```bash
docker compose up -d --build
```

- **Schedule.** The daily run time is `REFRESH_TIME` (HH:MM, default `10:00`) in the time zone `TZ` (default `UTC`). The host port is `PORT` (default `8080`). Set these in the environment or a `.env` file.
- **Fresh data.** Each run downloads the latest games file. If the download fails, it falls back to the cached copy.
- **Failed runs.** The report is replaced only when a run succeeds; a failed run leaves the previous one in place. It's retried every `RETRY_MINUTES` (default 60, at least 1), up to `MAX_RETRIES` times (default 6), before waiting for the next daily run. Check `docker compose logs refresh` for what happened. Before the first report exists, the site shows a placeholder that says whether a run is under way or the last one failed.
- **Warnings.** A run can succeed and still need attention: research files behind the season, a departure or birth date missing, a research row that is duplicated or matches nothing, a download that fell back to the cache. Every warning of the run, including any raised by R or a package, is printed in `docker compose logs refresh` after each run.
- **Research files.** `data/research/` is mounted read-only from this folder, so edits take effect on the next run without a rebuild. Changes to `coach_tenure.R` need `docker compose up -d --build`.
- **Volumes.** The games cache, the run log and warnings, and the published page live in named volumes. `docker compose down -v` removes them.

The image installs R and only the packages the script uses from Ubuntu's `r-cran-*` packages. Both base images (Ubuntu 26.04 and nginx 1.30) are pinned by digest, so a rebuild starts from the same images. Pinning also holds back their security fixes, so `.github/dependabot.yml` checks weekly and opens a pull request when a new image is published under either pinned tag. For Ubuntu it ignores minor and major tag upgrades (such as 26.04 to 26.10), because those change the R and package versions; move to a new tag by hand when you choose to. For nginx it also proposes minor upgrades (such as 1.30 to 1.31), since an nginx branch stops getting security fixes once it's superseded. Without GitHub, update by hand: pull the tags and replace the digests in `Dockerfile` and `compose.yaml`. Each run's log (`output/run_log.txt` in the `output` volume) starts with the R and package versions it used.

## Outputs

These are generated by the script and ignored by git:

| Path | Contents |
|---|---|
| `output/coach_tenure_report.html` | Self-contained report: every plot embedded, no external files |
| `output/current_coach_risk_<season>.csv` | Model-estimated firing risk for each current coach: both playoff scenarios, plus `made_playoffs` and `p_fired` once the playoff field is set. Only the season in progress has a file |
| `output/run_warnings.txt` | Every warning of the run, the script's own and R's, one per line (empty if there were none) |
| `plots/*.png` | Kaplan–Meier curves, forest plots, `cox.zph` residuals, cumulative incidence, current-coach risk |
| `data/tenure.csv` | One row per coach-team stint: hire season, last season, seasons coached, event type (`fired`, `resigned`, `retired`, `other`, `active`, or `unknown` if the coach has left but no departure is on record yet), `status`, `left_in_current_season`, covariates |
| `data/coach_seasons.csv` | One row per coach per season: the time-varying covariates |
| `data/interim_stints.csv` | Interim-only stints, which are excluded from the models |

## Data

| Source | Used for | Where |
|---|---|---|
| [nflverse/nfldata](https://github.com/nflverse/nfldata) `games.csv` | Scores and the credited head coach for every game since 1999 | Downloaded at run time |
| Wikipedia "YYYY NFL season" articles | Every head-coach departure 2000–2026 and its reason, with a short quote and URL per row | `data/research/coach_changes.csv` |
| Wikidata | Coach birth dates | `data/research/coach_birthdates.csv` |
| Coach Wikipedia articles | NFL head-coaching jobs before 1999 | `data/research/prior_hc_pre1999.csv` |
| Wikipedia team, GM and owner articles | GM and controlling-owner tenures for all 32 teams | `data/research/gm_tenures.csv`, `owner_tenures.csv` |
| Coach Wikipedia articles | Coach names the games file gets wrong, and the right ones | `data/research/coach_name_fixes.csv` |

Every research row carries its source URL. The research files are current as of mid-October 2026. Where sources conflicted, or where a judgment call was needed (such as who held final personnel say on teams without a conventional GM), the row's `notes` column says so.

## Updating for a new season

The season rolls forward on its own. What needs updating by hand is the research:

Add each offseason's head-coach departures to `coach_changes.csv` (with `article_season` set to the new season), and any GM or owner changes to their files. Each new hire also needs a birth date in `coach_birthdates.csv` (and, in the unlikely case he was an NFL head coach before 1999, a row in `prior_hc_pre1999.csv`). A hire without a birth date is left out of the models that use age and isn't rated on the hot seat; the script warns about it. The script reads how far the coaching research goes from the latest `article_season` and warns when the data has moved past it. It can't tell whether the GM and owner files are current, because a team with no change leaves no row, so check those by hand.

Departures matter most, and the script can't find them on its own. The games data doesn't show a firing: a coach fired after the season is still credited with his team's last game, and after a mid-season firing nflverse usually keeps crediting the fired coach for the rest of the season (it did for every mid-season firing of 2024 and 2025). So until his row is in `coach_changes.csv`, a coach who has left still counts as in the job and stays on the hot seat, with any games his interim coached added to his record. The hot-seat section shows the latest departure on record so a reader can tell how current it is. Once the row is there he drops off the hot seat, and the games after the change are credited to the right coach; the record always wins over the game data.

A coach who leaves during the season in progress is censored until that season ends (`left_in_current_season` is `TRUE` and `status` is 0).

If the games data shows a coach has gone but there's no departure row (his team started the next season under someone else, or nflverse credits an interim), the run doesn't stop. His stint is kept with event type `unknown` and censored, the script warns, and the report names him until the row is added. If he left during the season in progress, his team is also listed as not rated.

Mistakes in the research files are warned about rather than silently used or dropped:

- **A row entered twice** (a second birth date for a coach who already has one, say, when a former head coach is hired again): only the first is used.
- **A departure row that matches no stint** (the coach's name, the team or `last_season_coached` differs from the games data): it changes nothing, so the coach still counts as in the job.
- **A team name the script doesn't recognise:** rows with it are ignored. Use the team's full name.
- **A `reason_category` the script doesn't recognise** (`Fired` for `fired`, say): the departure is counted as a non-firing exit. Use one of the values already in the file.

If nflverse misspells a coach's name or lists two coaches under one name, add a row to `coach_name_fixes.csv`: the name as nflverse has it, the team code if the fix applies to one team only (such as `IND`; for a relocated team, its current code; otherwise `NA`), and the correct name.

## Tests

```bash
Rscript tests/replay.R
```

```bash
bash tests/refresh_test.sh
```

Much of the script depends on the calendar: whether a season is in progress, whether the playoff field is known, what opening week and the offseason look like. `tests/replay.R` replays the whole script against the games file cut off at chosen dates (the end of the 2025 regular season, the wild-card round, the offseason, opening week 2026) and checks what it decided. It also replays it with research rows removed, duplicated or mistyped, and checks the warnings, the name fixes and the download check. It runs in temporary directories, downloads nothing and leaves the project's outputs alone. It needs `data/raw/games.csv`, so run the script once first.

`tests/refresh_test.sh` runs the Docker scheduler, `docker/refresh.sh`, against a fake clock and a stand-in for the analysis: the daily run time, retries after a failed run, what the site shows before the first report, and rejected settings. It needs only bash and GNU `date`, and takes a couple of seconds.

## Caveats

The current-coach numbers are model estimates, not predictions. The model knows wins, playoffs, tenure, age, prior experience and front-office turnover. It knows nothing about Super Bowl rings, contracts, injuries or owner temperament.

This project isn't affiliated with the NFL or any team.

## License

The code and write-up are released under the [MIT License](LICENSE).

The research files in `data/research/` contain short excerpts from Wikipedia, which is licensed under [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/); each row links to its source article. Wikidata content is [CC0](https://creativecommons.org/publicdomain/zero/1.0/). The nflverse games file isn't redistributed here; see the [nflverse/nfldata](https://github.com/nflverse/nfldata) repository for its terms.
