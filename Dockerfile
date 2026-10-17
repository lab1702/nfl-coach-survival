# Image for the daily refresh job: runs coach_tenure.R and publishes the
# HTML report to /site, which the web container serves (see compose.yaml).
#
# R and its packages come from Ubuntu's r-cran-* system packages, the same
# way the analysis was developed; nothing is installed from CRAN at run time.
# The base image is pinned by digest so rebuilds get the same starting point;
# each run also logs the R and package versions it used. To update, pull
# ubuntu:26.04 and replace the digest.
FROM ubuntu:26.04@sha256:c098726dd9b9733f3e885f8de0ef375206a311b0a317eadc40d347b1115fb944

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    TZ=UTC

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      r-base-core \
      r-cran-survival r-cran-survminer r-cran-cmprsk \
      r-cran-base64enc r-cran-htmltools \
      r-cran-dplyr r-cran-readr r-cran-ggplot2 r-cran-stringr \
      r-cran-forcats r-cran-purrr r-cran-tibble r-cran-lubridate \
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

# REFRESH_TIME: daily run time (HH:MM, in TZ). A failed run is retried every
# RETRY_MINUTES, up to MAX_RETRIES times. GAMES_MAX_AGE_HOURS=0 makes every
# run download the latest games file (falling back to the cache).
ENV REFRESH_TIME=10:00 \
    RETRY_MINUTES=60 \
    MAX_RETRIES=6 \
    GAMES_MAX_AGE_HOURS=0

CMD ["bash", "/app/docker/refresh.sh"]
