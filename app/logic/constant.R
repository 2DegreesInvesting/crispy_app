# export constant values from here


portfolio_crispy_merge_cols <-
  c(
    "ald_sector"
    # , "ald_business_unit"
    # , "term"
  )
max_crispy_granularity <- c(
  "run_id",
  "scenario_geography",
  "term",
  portfolio_crispy_merge_cols
)


# # TRISK CONFIG

use_ald_sector <- c(
  "Oil&Gas",
  "Power",
  "Coal"
)

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
  available_financial_stimulus = c(1, 2, 3, 4, 5),
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
    # "NGFS2023_REMIND_DT",
    # "NGFS2023_MESSAGE_DT",
    # "NGFS2023_GCAM_DT",
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
    # "SoutheastAsia",
    # "China",
    # "India",
    # "MiddleEast"
  )
)
