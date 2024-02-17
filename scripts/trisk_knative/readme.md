# to test api : 

in rconsole:

    source("api.R")

in shell:

    curl -X 'POST' \
    'http://0.0.0.0:8080/compute_trisk' \
    -H 'Content-Type: application/json' \
    -d '{
        "trisk_run_params": {
        "baseline_scenario": "WEO2021_APS",
        "shock_scenario": "WEO2021_SDS",
        "scenario_geography": "Global",
        "shock_year": 2025,
        "discount_rate": 0.02,
        "risk_free_rate": 0.01,
        "growth_rate": 0.01,
        "div_netprofit_prop_coef": 0.8,
        "carbon_price_model": "no_carbon_tax",
        "market_passthrough": 0
        }
    }'  


# with docker

start local server :

docker run \
  -e ST_POSTGRES_USERNAME=$ST_POSTGRES_USERNAME \
  -e ST_POSTGRES_PASSWORD=$ST_POSTGRES_PASSWORD \
  -e ST_POSTGRES_HOST=$ST_POSTGRES_HOST \
  -e ST_POSTGRES_PORT=$ST_POSTGRES_PORT \
  -e ST_POSTGRES_DB=$ST_POSTGRES_DB \
  -p 8080:8080 registry.digitalocean.com/theia-1in1000-shinyapps/trisk_api:latest


curl -X 'POST' \
  'http://0.0.0.0:8080/compute_trisk' \
  -H 'Content-Type: application/json' \
  -d '{
    "trisk_run_params": {
      "baseline_scenario": "WEO2021_APS",
      "shock_scenario": "WEO2021_SDS",
      "scenario_geography": "Global",
      "shock_year": 2025,
      "discount_rate": 0.02,
      "risk_free_rate": 0.01,
      "growth_rate": 0.01,
      "div_netprofit_prop_coef": 0.8,
      "carbon_price_model": "no_carbon_tax",
      "market_passthrough": 0
    }
  }'
