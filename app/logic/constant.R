# CLOUD SERVICES CONSTANT


# TRISK API SERVICE ONLY USED IN "cloud" MODE
TRISK_API_SERVICE <- Sys.getenv("TRISK_API_SERVICE")
CRISPY_MODE <- Sys.getenv("CRISPY_MODE")

DBNAME <- Sys.getenv("POSTGRES_DB")
HOST_DB <- Sys.getenv("POSTGRES_HOST")
DB_PORT <- Sys.getenv("POSTGRES_PORT")
DB_USER <- Sys.getenv("POSTGRES_USERNAME")
DB_PASSWORD <- Sys.getenv("POSTGRES_PASSWORD")


# PROJECT CONSTANTS ====================================

trisk_input_path <- file.path("app", "data", "st_inputs")
backend_trisk_run_folder <- file.path("app", "data", "backend_db")

# Filter outliers in crispy when generating the analysis data
# see stress.test.plot.report:::load_input_plots_data_from_tibble documentation for more details
FILTER_CRISPY_OUTLIERS <- TRUE

# 1st january of the next year is the default expiration date for the equity portfolio
# in order to just pick 1 row out of the crispy data
DEFAULT_ASSET_EXPIRATION_DATE <- paste0(round(seq(from = 2024, to = 2034, length.out = 5)), "-01-01")

# Must be ordered from "less granular" to "more granular"
max_trisk_granularity <- list(
  "ald_sector" = 1,
  "ald_business_unit" = 2
  # ,"company_id" = 3
)


# # TRISK CONFIG

available_vars <- list(
  available_risk_free_rate = c(
    0,
    0.01,
    0.02,
    0.03,
    0.04,
    0.05
  ),
  available_growth_rate = c(
    0.01,
    0.03,
    0.05,
    0.07,
    0.09
  ),
  available_discount_rate = c(
    0.02,
    0.04,
    0.06,
    0.08,
    0.1
  ),
  available_shock_year = c(
    2025,
    2027,
    # 2029,
    2030,
    2032,
    # 2034,
    2035
    # 2040
  ),
  available_carbon_price_model = c("no_carbon_tax", "NZ2050", "NDC", "DN0", "B2DS"),
  available_market_passthrough = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  available_dividend_rate = c(0.8, 0.9, 1)
)


hide_vars <- list(
  hide_baseline_scenario = c(
    # "IPR2021_baseline",
    # "Oxford2021_base",
    # "NGFS2023_MESSAGE_CP",
    # "NGFS2023_GCAM_CP",
    # "NGFS2023_REMIND_CP",
    # "WEO2021_STEPS",
    # "NGFS2023_GCAM_NDC",
    # "NGFS2023_REMIND_NDC",
    # "NGFS2023_MESSAGE_NDC",
    # "NGFS2023_MESSAGE_FW",
    # "NGFS2023_REMIND_FW",
    # "NGFS2023_GCAM_FW"
  ),
  hide_shock_scenario = c(
    # "NGFS2023_REMIND_NZ2050",
    # "NGFS2023_MESSAGE_NZ2050",
    # "NGFS2023_GCAM_NZ2050",
    "NGFS2023_REMIND_DT",
    "NGFS2023_MESSAGE_DT",
    "NGFS2023_GCAM_DT"
    # "NGFS2023_REMIND_DN0",
    # "NGFS2023_MESSAGE_DN0",
    # "NGFS2023_GCAM_DN0",
    # "NGFS2023_GCAM_LD",
    # "NGFS2023_MESSAGE_LD",
    # "NGFS2023_REMIND_LD",
    # "NGFS2023_REMIND_B2DS",
    # "NGFS2023_MESSAGE_B2DS",
    # "NGFS2023_GCAM_B2DS",
    # "Oxford2021_fast",
    # "IPR2021_RPS",
    # "IPR2021_FPS",
    # "WEO2021_SDS",
    # "WEO2021_NZE_2050"
  ),
  hide_scenario_geography = c(
    # "Global",
    # "EmergingMarketAndDevelopingEconomies",
    # "OecdAndEu",
    # "UnitedStates",
    "SoutheastAsia",
    "China",
    "India",
    # "MiddleEast",
    "Non-OECD"
  )
)
