FROM rocker/r-ver:4.5.3

ENV DEBIAN_FRONTEND=noninteractive \
    MIMICWIZARD_PORT=3838 \
    MIMICWIZARD_CACHE_DIR=/var/cache/mimicwizard 
    #RENV_PATHS_CACHE=/var/cache/mimicwizard/renv

# System dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
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


# # Create user and directories
 RUN useradd --create-home --shell /bin/bash shiny && \
     mkdir -p /var/cache/mimicwizard && \
     chown -R shiny:shiny /var/cache/mimicwizard


COPY --chown=shiny:shiny . /app
USER shiny
WORKDIR /app
# RUN R -e "options(timeout = 1800); \
#           install.packages('renv', repos = 'https://packagemanager.posit.co/cran/latest')"
RUN R -e "options(timeout = 1800); \
          options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest')); \
          source('renv/activate.R');\
          renv::restore(prompt = FALSE,repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'))"

COPY --chown=shiny:shiny renv/ renv/
COPY --chown=shiny:shiny renv.lock .

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 \
    CMD curl --fail --silent http://127.0.0.1:${MIMICWIZARD_PORT}/ || exit 1

CMD ["R", "--quiet", "-e", "source('renv/activate.R');shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('MIMICWIZARD_PORT', '3838')))"]
