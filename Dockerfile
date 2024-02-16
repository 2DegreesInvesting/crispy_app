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
RUN sudo -u shiny Rscript -e 'renv::restore(clean = TRUE)'

ENV CRISPY_APP_ENV="prod"
ENV TRISK_API_ENDPOINT=NULL

# Copy app
COPY --chown=shiny:shiny app.R ./
COPY --chown=shiny:shiny config.yml ./
COPY --chown=shiny:shiny rhino.yml ./
COPY --chown=shiny:shiny app app/

COPY --chown=shiny:shiny docker/shiny-server.conf /etc/shiny-server/
RUN mkdir -p /var/run/s6 && chown -R shiny:shiny /var/run/s6
USER shiny