FROM rocker/r-ver:4.3.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    # Install system dependencies required by some R packages and renv
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    liblzma-dev \
    libbz2-dev \ 
    libpcre2-dev \
    libpq-dev \  
    # Python installation
    python3.11 \
    python3-pip \
    python3-setuptools \
    python3-dev

# Install Poetry for Python dependency management
# RUN pip3 install poetry

# Set the working directory in the container
WORKDIR /app

# Initialize the R environment with renv and restore dependencies
# Ensure your project includes an renv.lock file
# Install R dependencies
COPY .Rprofile renv.lock ./
COPY renv/activate.R renv/
RUN Rscript -e 'renv::restore(clean = TRUE)'


# Copy the Python project files and install Python dependencies
# COPY pyproject.toml poetry.lock* /app/
# RUN poetry config virtualenvs.create false && poetry install --no-interaction --no-ansi

RUN pip3 install --upgrade pip setuptools wheel
RUN pip3 install fastapi uvicorn pydantic psycopg2
RUN pip3 install rpy2


# Copy the R project files
COPY ./main.py /app/main.py
COPY ./trisk_compute.R /app/trisk_compute.R
COPY ./st_inputs/* /app/st_inputs/

ENV ST_POSTGRES_DB=NULL
ENV ST_POSTGRES_USER=NULL
ENV ST_POSTGRES_PASSWORD=NULL
ENV ST_POSTGRES_HOST=NULL
ENV ST_POSTGRES_PORT=NULL


# Expose the port your app runs on
EXPOSE 8000


# Command to run the app
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]