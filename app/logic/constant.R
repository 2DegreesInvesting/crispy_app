# export constant values from here


# ======================================================
# ======================================================
# TODO move to .RProfile env var

# INPUT DATA
trisk_input_path <- fs::path("app", "data", "st_inputs")
trisk_output_path <- fs::path("app", "data", "st_outputs")
backend_trisk_run_folder <- fs::path("app", "data", "backend_db")

# ANALYSIS DATA CREATION
trisk_start_year <- 2022
maturity_month_term_bridge_fp <- file.path("app", "data", "maturity_month_term_bridge.csv")
# ======================================================
# ======================================================


max_portfolio_granularity <-
  c(
    # "portfolio_id",
    # "asset_type",
    "ald_sector"
    # "ald_business_unit",
    # "term"
  )
max_crispy_granularity <- c(
  "run_id",
  "scenario_geography",
  "ald_sector",
  # "ald_business_unit",
  "term"
)
portfolio_crispy_merge_cols <-
  c(
    "ald_sector"
    # , "ald_business_unit"
    # , "term"
  )



# # TRISK CONFIG

use_ald_sector <- c(
  "Oil&Gas",
  "Power",
  "Coal"
)

available_discount_rate <- c(
  0.02,
  0.04,
  0.07,
  0.1
)
available_risk_free_rate <- c(
  0,
  0.02,
  0.05
)
available_growth_rate <- c(
  0.01,
  0.02,
  0.095
)
available_shock_year <- c(
  2025,
  2027,
  2027,
  # 2029,
  2030,
  2032,
  2032,
  # 2034,
  2035
  # 2040
)

available_carbon_price_model <- c("no_carbon_tax", "NZ2050", "NDC", "DN0", "B2DS")
available_market_passthrough <- c(0, 0.5, 1)

available_baseline_scenario <- c(
  "IPR2021_baseline",
  "Oxford2021_base",
  # "NGFS2022_MESSAGE_CP",
  # "NGFS2022_GCAM_CP",
  # "NGFS2022_REMIND_CP",
  "WEO2021_STEPS"
  # "NGFS2022_GCAM_NDC",
  # "NGFS2022_REMIND_NDC",
  # "NGFS2022_MESSAGE_NDC"
)
available_shock_scenario <- c(
  # "IPR2021_RPS",
  # "NGFS2022_REMIND_NZ2050", "NGFS2022_MESSAGE_NZ2050", "NGFS2022_GCAM_NZ2050",
  "WEO2021_NZE_2050",
  # "NGFS2022_REMIND_DT", "NGFS2022_MESSAGE_DT", "NGFS2022_GCAM_DT",
  # "NGFS2022_REMIND_DN0", "NGFS2022_MESSAGE_DN0", "NGFS2022_GCAM_DN0",
  # "NGFS2022_REMIND_B2DS",
  "Oxford2021_fast",
  # "NGFS2022_MESSAGE_B2DS",
  # "NGFS2022_GCAM_B2DS",
  "IPR2021_FPS",
  "WEO2021_SDS"
)


available_scenario_geography <- c(
  "Global",
  "EmergingMarketAndDevelopingEconomies",
  "OecdAndEu",
  "UnitedStates",
  "SoutheastAsia",
  "China",
  "India",
  "MiddleEast"
)
