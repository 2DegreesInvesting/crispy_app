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


# Deploy and test deployment

deploy:
    kubectl apply -f trisk-api-service.yaml

get service hostname:
    kubectl get ksvc trisk-api -o=jsonpath='{.status.url}'


test api in vpc:
    curl -X POST http://SERVICE-HOSTNAME -H "Host: SERVICE-HOSTNAME" -H "Content-Type: application/json" -d '{"key":"value"}'

get external ip:
    kubectl get svc -n kourier-system

test api from the web : 
    curl -X POST http://EXTERNAL-IP -H "Host: SERVICE-HOSTNAME" -H "Content-Type: application/json" -d '{"key":"value"}'



debug: 
    kubectl logs -l serving.knative.dev/service=trisk-api-service -c user-container -n default




# DEPLOY

https://docs.digitalocean.com/products/kubernetes/how-to/set-up-autoscaling/

docker push registry.digitalocean.com/theia-1in1000-shinyapps/trisk_api:latest

## kubectl commands

kubectl get pods

<!-- Viewing Logs of a Pod -->
kubectl logs <pod-name>


kubectl apply -f app-deployment.yaml
kubectl apply -f app-service.yaml

kubectl autoscale deployment trisk-api --cpu-percent=50 --min=2 --max=10
kubectl get hpa


<!-- Listing Services -->
kubectl get svc
kubectl get deployment

<!-- restart deployment -->
kubectl rollout restart deployment <deployment-name>

kubectl delete deployment <deployment-name>


# To create db-credentials.yaml


echo -n DB_PASSWORD | base64

kubectl apply -f db-credentials.yaml





### test internally:
kubectl run curl --image=curlimages/curl --restart=Never --rm -ti -- -v -X POST http://trisk-api.default.svc.cluster.local/compute_trisk -H 'Content-Type: application/json' -d '{
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


curl -v -X POST 'http://159.223.251.127/compute_trisk' \
  -H 'Host: trisk-api.default.159.223.251.127' \
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