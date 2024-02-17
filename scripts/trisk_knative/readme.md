to test api : 

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