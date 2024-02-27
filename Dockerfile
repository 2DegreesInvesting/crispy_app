FROM rocker/shiny:4.1.0

ENV DEBIAN_FRONTEND=noninteractive
ENV CRISPY_APP_ENV="prod" 

# Install system dependencies
RUN apt-get update -qq \
  && apt-get install --yes \
    curl \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server 
RUN rm -rf *

# Install R dependencies
COPY --chown=shiny:shiny .Rprofile renv.lock ./
COPY --chown=shiny:shiny renv/activate.R renv/
RUN sudo -u shiny Rscript -e 'renv::restore()'


# Copy app
COPY --chown=shiny:shiny app.R ./
COPY --chown=shiny:shiny config.yml ./
COPY --chown=shiny:shiny rhino.yml ./
COPY --chown=shiny:shiny app app/

# Expose the port your app runs on
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R', host = '127.0.0.1', port = 3838)"]
