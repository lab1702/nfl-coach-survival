# How long do NFL head coaches last? A survival analysis of hires since 2000

Run it with `Rscript coach_tenure.R`. It takes about 30 seconds. The full console output is saved in `output/run_log.txt`.

The script also writes `output/coach_tenure_report.html`, a single self-contained page (about 1 MB) with every plot embedded and no external files. Copy it to any web server as-is. Its numbers are computed on each run, so it stays current when the data updates; the tables below are a snapshot from the run through week 6 of 2026.

## Data

There are 190 permanent head-coaching stints that started in 2000–2026. 180 of them have at least one completed season and are used for model fitting. The 32 stints in progress include the 10 coaches hired for 2026.

| Source | What it gives |
|---|---|
| [nflverse `games.csv`](https://github.com/nflverse/nfldata) | Every game since 1999 with its score and the credited head coach. Stints, records, point differential and playoffs all come from this. |
| Wikipedia, "YYYY NFL season" articles | Why each coach left. All 230 departures have a quote and a URL (`data/research/coach_changes.csv`). |
| Wikidata | Birth dates (`coach_birthdates.csv`). |
| Coach Wikipedia infoboxes | NFL head-coaching jobs before 1999 (`prior_hc_pre1999.csv`). |
| Wikipedia team, GM and owner pages | GM and controlling-owner tenures for all 32 teams (`gm_tenures.csv`, `owner_tenures.csv`). |

Pro Football Reference blocks automated access, so I didn't use it. Nothing was hand-entered from memory: every research row points to its source.

**Corrections applied to nflverse:**
- Its coach column misses some mid-season changes. For example, it credits Ken Whisenhunt with all of Tennessee's 2015 games. 73 games are re-credited to the interim coach named on Wikipedia, using the departure date.
- For 2026 it still lists Gannon (ARI), Morris (ATL) and McDermott (BUF), all fired in January 2026. Their 15 games are re-credited to LaFleur, Stefanski and Brady.

**Definitions:**
- **Seasons coached:** seasons in which the coach coached at least one game for the team. A mid-season firing counts that season.
- **Fired (`status = 1`):** the club ended the job. This includes 5 "mutual partings", 6 contracts the club didn't renew (e.g. Garrett, McCarthy) and Carroll's 2024 reassignment. Treating these as censored instead barely moves the estimates (see the sensitivity check in section 3 of the script).
- **Resigned, retired or traded:** censored for the main models. The 2 trades are Gruden in 2002 and Herm Edwards in 2006.
- **Interim coaches:** the 42 interim-only stints are in `data/interim_stints.csv` and are excluded from the models. An interim later made permanent (e.g. Jason Garrett) starts his tenure with his first full season.
- **Previous NFL head coach:** held a permanent NFL head-coaching job before this hire (63 of 190 stints). Interim-only experience is flagged separately.
- **New GM or owner:** the change took effect before that season's opener. A change made during season *s* counts for *s + 1*, so a GM fired alongside the coach can't leak into the season that got the coach fired.

## Terms used below

- **Censoring:** we know a coach lasted at least *t* seasons without being fired, but not what happened after. Active coaches, resignations and retirements are censored. Treating them as fired, or dropping them, would bias tenure downward.
- **Hazard:** the chance of being fired after season *t*, given the coach has reached season *t*.
- **Hazard ratio (HR):** how a covariate multiplies that per-season risk. HR 0.6 means 40% lower risk in any given season, holding the other covariates fixed.
- **Counting-process format:** one row per coach per season, written as an interval (start, stop] = (*k*−1, *k*]. The row carries that season's covariates and an event flag that is 1 only on the interval in which he was fired. This is what lets covariates such as win % change over time.

## 1. Kaplan–Meier: how long tenures last (`plots/01`, `plots/02`)

**The median coach is fired by the end of his 4th season (95% CI 3–4).**

| Seasons | Share not yet fired |
|---|---|
| 1 | 89% |
| 2 | 73% |
| 3 | 52% |
| 4 | 36% |
| 8 | 17% |
| 10 | 11% |

The biggest drops come after seasons 2–4. Owners rarely fire after one year, and rarely wait past four.

**Previous NFL head coaches vs. first-timers: no meaningful difference.** The medians are 3 vs. 4 seasons and the curves overlap throughout (log-rank p = 0.57). Retreads get fired slightly sooner early on, but the gap is within noise.

## 2. Cox model with baseline covariates (`plots/03`)

| Covariate (known on the day of the hire) | HR | 95% CI |
|---|---|---|
| Age at hire, per +5 years | 1.13 | 0.98–1.30 |
| Previous NFL head coach | 0.84 | 0.53–1.33 |
| Team win % the season before, per +10 pts | 1.05 | 0.95–1.16 |

None of these predict much. The concordance is 0.54, where 0.5 is a coin flip. Older hires lean toward shorter tenures (p = 0.08), but nothing is significant.

What a coach brings in on hiring day doesn't predict how long he lasts. What he does on the field does (section 4).

## 3. Proportional-hazards check (`plots/04`)

The Cox model assumes each HR stays constant across tenure. `cox.zph` tests this by checking whether each covariate's residuals trend over time.

**Baseline model:** no violation at the 5% level (global p = 0.07). Prior experience is borderline (p = 0.07). Its plotted effect drifts downward: roughly neutral in seasons 1–3, mildly protective later. Plainly put, a retread who survives the first few years may be a bit safer than a first-timer at the same stage. The evidence is weak.

**Time-varying model:** one clear violation, for change in win % from last season (p = 0.005). Holding this season's win % fixed, a bigger "change" means last season was worse, so this term measures two bad years in a row. Splitting it by tenure fixes the violation (global p = 0.41):

- **Seasons 1–2:** HR 0.94 (0.80–1.10). Last year doesn't matter, because last year was usually the previous coach's team.
- **Season 3 onward:** HR 1.26 per 10 pts (1.11–1.42). A coach coming off a good year gets some slack; two bad years in a row is what ends it.

## 4. Time-varying Cox model (`plots/05`)

734 coach-seasons and 138 firings. Each row uses only information available when the decision was made: that season's results and the change from the team's previous season. For a mid-season firing, the row uses only the games he coached before being let go. Standard errors are clustered by stint.

| Covariate | HR | 95% CI |
|---|---|---|
| Win % this season, per +10 pts | **0.59** | 0.47–0.74 |
| Change in win % vs last season, per +10 pts | **1.14** | 1.02–1.26 (see section 3) |
| Made the playoffs | **0.18** | 0.07–0.51 |
| Point differential per game, per +1 | 0.97 | 0.92–1.03 |
| New GM or owner this season | 1.09 | 0.72–1.66 |
| Previous NFL head coach | 1.13 | 0.77–1.65 |
| Age at hire, per +5 years | 1.10 | 0.97–1.24 |

The concordance is 0.87, so the season-by-season model predicts far better than the baseline one.

- **Winning matters most.** Each extra 10 points of win % (about 1.7 more wins) cuts that season's firing risk by about 40%.
- **Making the playoffs cuts it by about 80%** on top of that. Only 4 of the 138 firings came right after a playoff season: Schottenheimer 2006, Fox 2014, Mularkey 2017 and McDermott 2025.
- **Point differential adds nothing once wins are known.** Owners fire on the record, not on "deserved" wins.
- **A new GM or owner doesn't show a measurable effect** in that same season.

## 5. Competing risks (`plots/06`)

A **cumulative incidence function (CIF)** gives the real-world share of coaches who have left by each route by season *t*. Unlike 1 − KM, it accounts for the fact that a coach who retired can no longer be fired.

| By end of season | Fired | Resigned, retired or other |
|---|---|---|
| 2 | 27% | 4% |
| 4 | 61% | 7% |
| 6 | 73% | 10% |
| 8 | 77% | 10% |

Firing is by far the dominant exit, roughly 8 to 1. Voluntary exits are rare and spread evenly over tenure, so ignoring them (treating them as censored) changes little.

## Current coaches: highest model-estimated risk of being fired after 2026 (`plots/07`, `output/current_coach_risk_2026.csv`)

The 2026 season is through week 6. Six games are a noisy sample, and the model was fit on full seasons, so each team's season is projected: actual results so far, plus .500 football and an even point differential the rest of the way. Playoffs aren't decided, so each coach gets two numbers: one if his team misses, one if it makes it. These come from the split model in section 3.

| Coach | Team | Season | 2026 so far | P(fired) if miss | P(fired) if make |
|---|---|---|---|---|---|
| Dan Quinn | WAS | 3rd | 1-3-1 | 50% | 11% |
| Jim Harbaugh | LAC | 3rd | 0-5 | 38% | 8% |
| Shane Steichen | IND | 4th | 2-3 | 32% | 6% |
| Todd Bowles | TB | 5th | 1-4 | 31% | 6% |
| Dan Campbell | DET | 6th | 3-2 | 25% | 5% |
| DeMeco Ryans | HOU | 4th | 1-4 | 24% | 5% |
| Dave Canales | CAR | 3rd | 2-2 | 24% | 5% |
| Nick Sirianni | PHI | 6th | 2-3 | 23% | 4% |

The pattern behind the list: a coach in season 3 or later, with a losing record, after a bad previous season. Quinn (Washington was 5-12 in 2025) and Harbaugh (0-5 now) fit it most clearly.

First-year coaches with bad starts, such as Hafley (0-5), Saleh (0-5) and Mike LaFleur (1-4), rate low. Since 2000 almost nobody has been fired after one season.

**Read these as model estimates, not predictions.**
- The model knows wins, playoffs, tenure, age, experience and front-office turnover. It knows nothing about Super Bowl rings, contracts, quarterback injuries, scandals or owner temperament. That is why Campbell, Sirianni and Andy Reid (20% "if miss", 14th season) show up higher than anyone following the league would put them.
- Few coaches reach season 8 or later, so the baseline risk for long tenures is pooled and imprecise.
- "If miss" is the relevant number for losing teams. For 5-0 teams the "if make" column is the realistic one.

## Limitations

- **Small sample:** 138 firings, so confidence intervals for anything other than winning and playoffs are wide.
- **Seasons are a coarse time scale.** Many coaches tie on tenure; the Cox models use Efron's method for ties.
- **Judgment calls in the research data:**
  - Which mutual partings count as firings.
  - Who held final personnel say in teams without a conventional GM, e.g. Belichick, Andy Reid in Philadelphia, Pete Carroll in Seattle, and Jerry Jones.
  - Some start dates known only to the month or year.

  The research CSVs keep the raw wording and notes so these can be re-coded.
- **One row dropped:** Houston's 2002 expansion season has no previous-season record, so it's excluded from models that need one.
