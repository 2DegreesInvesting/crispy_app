FROM rocker/r-ver:latest

# Install system dependencies required by some R packages and renv
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libpq-dev \
    libsodium-dev
#     libssl-dev \
#     libxml2-dev \
#     liblzma-dev \
#     libbz2-dev \ 
#     libpcre2-dev \
    


# Set the working directory in the container
WORKDIR /app

# Initialize the R environment with renv and restore dependencies
# Ensure your project includes an renv.lock file
# Install R dependencies
COPY .Rprofile renv.lock ./
COPY renv/activate.R renv/
RUN Rscript -e 'renv::restore(clean = TRUE)'


# Copy the R project files
COPY ./main.R /app/main.R
COPY ./trisk_compute.R /app/trisk_compute.R
COPY ./st_inputs/* /app/st_inputs/

# Define env variables
ENV ST_POSTGRES_DB=NULL
ENV ST_POSTGRES_USER=NULL
ENV ST_POSTGRES_PASSWORD=NULL
ENV ST_POSTGRES_HOST=NULL
ENV ST_POSTGRES_PORT=NULL


# Command to run the app
# Expose the port your app will run on
EXPOSE 8080

# Use plumber to run your app
CMD ["Rscript", "/app/main.R"]