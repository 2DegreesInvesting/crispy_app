# Crispy App
This app is built with [Rhino](https://github.com/Appsilon/rhino).

Documentation about the folders structure : https://appsilon.github.io/rhino/articles/explanation/application-structure.html 

## Prerequisites
This is an application built in [Shiny](https://shiny.rstudio.com/).
To run it, make sure you have R (>= 4.0.0) installed.

The following env variables must be set, for example in an .Renviron file at the root of this project : 

```sh
# ./.Renviron

# 'cloud' or 'local'
CRISPY_APP_ENV="local" 

# IP to the kubernetes endpoint running trisk api
TRISK_API_SERVICE="0.0.0.0" 

# backend DB creds
POSTGRES_DB=""
POSTGRES_HOST=""
POSTGRES_PORT=""
POSTGRES_USERNAME=""
POSTGRES_PASSWORD=""


```

## Dependencies
Run `renv::restore(clean = TRUE)` to synchronize the project library with the lockfile
when you initially clone the repo or switch branches.

## Data
Application-ready data is included in `app/data`.
However, if you want to generate this data from raw sources, run `source("./scripts/generate_data.R")`

## Running
To run the app, use `Rscript -e 'shiny::runApp(launch.browser = TRUE)'`.

## Deployment
You can use the RStudio GUI to deploy the app to RStudio Connect or shinyapps.io.
You only need to include the following files:
`.Rprofile`, `dependencies.R`, `app.R`, and `app/` directory.

### Docker
The application can also be packaged as a Docker container using `rocker/shiny` as a
base image.

To build the image locally, execute:
```bash
docker build --tag crispy-app .
```

To run the container, execute:
```bash
docker run --rm -it -p 3838:3838 crispy-app
```

The application should be available under `localhost:3838` on your local
workstation.

## Development
This project uses [renv](https://rstudio.github.io/renv/) to manage R package dependencies.
To add/remove packages, edit the `dependencies.R` file and run the following commands:
```r
renv::install() # Install added packages
renv::snapshot() # Update the lockfile
renv::restore(clean = TRUE) # Uninstall removed packages
```


## Cloud Deployment

The app can be deployed on kubernetes by following those instructions : 

- First, deploy the TRISK api in the scripts/trisk_k8s_api folder
- pull the crispy app dockerfile, and verify its tag url matches the one in crispy-app-deployment.yaml
- run those commands : 

kubectl apply -f crispy-app-deployment.yaml
kubectl apply -f crispy-app-service.yaml

### set the secrets:

```bash
export POSTGRES_DB=""
export POSTGRES_HOST=""
export POSTGRES_PORT=""
export POSTGRES_USERNAME=""
export POSTGRES_PASSWORD=""



./deploy_secrets.sh "$POSTGRES_USERNAME" "$POSTGRES_PASSWORD" "$POSTGRES_HOST" "$POSTGRES_PORT" "$POSTGRES_DB"

```


# for cloud deployment mode, set env var : 

```{r}

# trisk kubernetes api endpoint url
# TRISK_API_SERVICE <- "http://trisk-api-service.default.svc.cluster.local:8000"
# trisk docker compose endpoint url
# TRISK_API_SERVICE <- "http://trisk-api-service:8000"
# trisk local api endpoint url
# TRISK_API_SERVICE <- "http://0.0.0.0:8000"

```