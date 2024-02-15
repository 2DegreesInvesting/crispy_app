from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import rpy2.robjects as robjects
from rpy2.robjects.vectors import ListVector
from typing import Dict, Union

TRISK_INPUT_PATH = './st_inputs'

app = FastAPI()


# Define your Pydantic model to validate incoming data
class RParams(BaseModel):
    trisk_run_params: Dict[str, Union[str, float]] # TODO expand in detailed params

@app.post("/compute_trisk/")
async def compute_r_function(params: RParams):
    try:
        # Load your R script or directly use the R function
        r_source = robjects.r['source']
        r_source('./trisk_compute.R')
        r_run_trisk_and_upload_results = robjects.globalenv['run_trisk_and_upload_results']
        # robjects.r('options(error = function(e) { print(e); cat("Call: ", deparse(e$call), "\\n"); traceback() })')

        # Call your R function with parameters from the request
        run_id = r_run_trisk_and_upload_results(
            trisk_run_params=ListVector(params.trisk_run_params), 
            trisk_input_path=TRISK_INPUT_PATH
            )
        
        return {"trisk_run_id": run_id[0]}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

       