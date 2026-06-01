FROM rocker/r-ver:4.4.0

# System libraries required by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
        libmagick++-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libfontconfig1-dev \
        libuv1 \
        libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages from Posit Package Manager
RUN R -e "install.packages( \
      c('shiny', 'bslib', 'jsonlite', 'magick'), \
      repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest' \
    )"

WORKDIR /app

# Copy only the Shiny app
COPY shiny_app/ .

# Render.com injects PORT at runtime; default 8080 for local Docker testing
EXPOSE 8080

CMD R -e "shiny::runApp('.', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '8080')))"
