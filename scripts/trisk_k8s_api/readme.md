# TEST LOCALLY

docker build -t registry.digitalocean.com/theia-1in1000-shinyapps/trisk_api:latest .

docker run \
  -e ST_POSTGRES_USERNAME=$ST_POSTGRES_USERNAME \
  -e ST_POSTGRES_PASSWORD=$ST_POSTGRES_PASSWORD \
  -e ST_POSTGRES_HOST=$ST_POSTGRES_HOST \
  -e ST_POSTGRES_PORT=$ST_POSTGRES_PORT \
  -e ST_POSTGRES_DB=$ST_POSTGRES_DB \
  -p 8000:8000 registry.digitalocean.com/theia-1in1000-shinyapps/trisk_api:latest



curl -X 'POST' \                 
  'http://0.0.0.0:8000/compute_trisk/' \                                                    
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

