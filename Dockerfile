FROM rocker/r-ver:4.4.1

ENV DEBIAN_FRONTEND=noninteractive \
    MIMICWIZARD_PORT=3838 \
    MIMICWIZARD_CACHE_DIR=/var/cache/mimicwizard

RUN apt-get update \
    && apt-get install --no-install-recommends -y \
        build-essential \
        ca-certificates \
        curl \
        gfortran \
        libbz2-dev \
        libcairo2-dev \
        libcurl4-openssl-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libfribidi-dev \
        libharfbuzz-dev \
        libicu-dev \
        libjpeg-dev \
        liblzma-dev \
        libnode-dev \
        libpng-dev \
        libpq-dev \
        libssl-dev \
        libtiff5-dev \
        libxml2-dev \
        libzstd-dev \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN useradd --create-home --shell /bin/bash shiny \
    && install --directory --owner=shiny --group=shiny /app /var/cache/mimicwizard

WORKDIR /app

COPY --chown=shiny:shiny renv.lock ./
COPY --chown=shiny:shiny renv/activate.R renv/settings.json ./renv/

RUN R --quiet -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

USER shiny

RUN R --quiet -e "source('renv/activate.R'); renv::restore(lockfile = 'renv.lock', prompt = FALSE)"

COPY --chown=shiny:shiny . ./

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 \
    CMD curl --fail --silent "http://127.0.0.1:${MIMICWIZARD_PORT}/" || exit 1

CMD ["R", "--quiet", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('MIMICWIZARD_PORT', '3838')))"]