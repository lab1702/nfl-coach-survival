# Image for the daily refresh job: runs coach_tenure.R and publishes the
# HTML report to /site, which the web container serves (see compose.yaml).
#
# R and its packages come from Ubuntu's r-cran-* system packages, the same
# way the analysis was developed; nothing is installed from CRAN at run time.
FROM ubuntu:26.04

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    TZ=UTC

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      r-base-core \
      r-cran-survival r-cran-survminer r-cran-tidyverse r-cran-cmprsk \
      r-cran-base64enc r-cran-htmltools \
      ca-certificates fonts-dejavu-core tzdata \
 && rm -rf /var/lib/apt/lists/*

# Run as an unprivileged user. The script writes its tables to data/, so
# that directory is the user's (data/research stays read-only). The writable
# directories are created here so named volumes mounted over them start out
# owned by this user.
RUN useradd --create-home --uid 10001 app
WORKDIR /app
COPY coach_tenure.R ./
COPY data/research/ data/research/
COPY docker/ docker/
RUN mkdir -p data/raw output plots /site \
 && chown app:app data \
 && chown -R app:app data/raw output plots /site
USER app

# REFRESH_TIME: daily run time (HH:MM, in TZ). GAMES_MAX_AGE_HOURS=0 makes
# every run download the latest games file (falling back to the cache).
ENV REFRESH_TIME=10:00 \
    GAMES_MAX_AGE_HOURS=0

CMD ["bash", "/app/docker/refresh.sh"]
