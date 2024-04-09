FROM rocker/shiny:4.3.0


ENV DEBIAN_FRONTEND=noninteractive
ENV CRISPY_APP_ENV="cloud" 

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libpq-dev \
    libxml2-dev \
    cmake \ 
    && rm -rf /var/lib/apt/lists/*


RUN addgroup --system shiny \
    && adduser --system --home /home/app --ingroup shiny shiny

# Set the working directory to /home/app
WORKDIR /home/app

# Ensure shiny user has proper permissions to install packages
RUN chown -R shiny:shiny /home/app && \
    chmod -R 755 /home/app

# Install R dependencies
COPY --chown=shiny:shiny .Rprofile renv.lock .renvignore dependencies.R ./
COPY --chown=shiny:shiny renv/activate.R renv/
# RUN R -e "install.packages('withr', repos = 'http://cran.rstudio.com'); withr::with_envvar(c(NOT_CRAN = 'true'), renv::install('arrow'))"
RUN sudo -u shiny Rscript -e 'renv::restore(clean=T)'

# Copy app
COPY --chown=shiny:shiny app.R ./
COPY --chown=shiny:shiny config.yml ./
COPY --chown=shiny:shiny rhino.yml ./
COPY --chown=shiny:shiny app app/

USER shiny 

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
