RENAMING_SCENARIOS <- c(
  "NGFS2023REMIND_NDC" = "REMIND NDC",
  "NGFS2023MESSAGE_NDC" = "MESSAGE NDC",
  "NGFS2023GCAM_NDC" = "GCAM NDC",
  "NGFS2023MESSAGE_CP" = "MESSAGE CP",
  "NGFS2023GCAM_CP" = "GCAM CP",
  "NGFS2023REMIND_CP" = "REMIND CP",
  "NGFS2023MESSAGE_B2DS" = "MESSAGE B2DS",
  "NGFS2023REMIND_B2DS" = "REMIND B2DS",
  "NGFS2023GCAM_B2DS" = "GCAM B2DS",
  "NGFS2023MESSAGE_DT" = "MESSAGE DT",
  "NGFS2023REMIND_DT" = "REMIND DT",
  "NGFS2023GCAM_DT" = "GCAM DT",
  "NGFS2023REMIND_NZ2050" = "REMIND NZ2050",
  "NGFS2023MESSAGE_NZ2050" = "MESSAGE NZ2050",
  "NGFS2023GCAM_NZ2050" = "GCAM NZ2050",
  "NGFS2023REMIND_DN0" = "REMIND DN0",
  "NGFS2023GCAM_DN0" = "GCAM DN0",
  "NGFS2023MESSAGE_DN0" = "MESSAGE DN0",
  "NGFS2023MESSAGE_FW" = "MESSAGE FW",
  "NGFS2023REMIND_FW" = "REMIND FW",
  "NGFS2023GCAM_FW" = "GCAM FW",
  "NGFS2023GCAM_LD" = "GCAM LD",
  "NGFS2023MESSAGE_LD" = "MESSAGE LD",
  "NGFS2023REMIND_LD" = "REMIND LD",
  "IPR2023_RPS" = "IPR RPS",
  "IPR2023_FPS" = "IPR FPS",
  "IPR2023_baseline" = "IPR BASELINE",
  "WEO2021_STEPS" = "WEO STEPS",
  "WEO2021_APS" = "WEO APS",
  "WEO2021_NZE_2050" = "WEO NZ2050",
  "WEO2021_SDS" = "WEO B2DS",
  "Oxford2021_base" = "OXFORD BASELINE",
  "Oxford2021_fast" = "OXFORD B2DS"
)
REV_RENAMING_SCENARIOS <- stats::setNames(names(RENAMING_SCENARIOS), RENAMING_SCENARIOS)

RENAMING_ANALYSIS_COLUMNS <- c(
  "ald_sector" = "Sector",
  "exposure_value_usd" = "Exposure (USD)",
  "pd_portfolio" = "Probability of Default",
  "loss_given_default" = "Loss Given Default",
  "run_id" = "Run ID",
  "scenario_geography" = "Scenario Geography",
  "term" = "Term",
  # "roll_up_type" ,
  "baseline_scenario" = "Baseline Scenario",
  "shock_scenario" = "Target Scenario",
  "risk_free_rate" = "Risk Free Rate",
  "discount_rate" = "Discount Rate",
  "div_netprofit_prop_coef" = "Dividend Rate",
  "growth_rate" = "Growth Rate",
  "shock_year" = "Shock Year",
  "net_present_value_baseline" = "Net Present Value (Baseline)",
  "net_present_value_shock" = "Net Present Value (Shock)",
  "pd_baseline" = "Probability of Default (Baseline)",
  "pd_shock" = "Probability of Default (Shock)",
  "net_present_value_difference" = "Difference in NPV",
  "crispy_perc_value_change" = "Crispy Value Change (%)",
  "crispy_value_loss" = "Crispy Value Loss (USD)",
  "exposure_at_default" = "Exposure at Default",
  "expected_loss_portfolio" = "Expected Loss",
  "expected_loss_baseline" = "Expected Loss (Baseline)",
  "expected_loss_shock" = "Expected Loss (Shock)",
  "pd_difference" = "Difference in PD"
)


rename_tibble_columns <- function(table_to_rename, class) {
  if (class == "analysis_columns") {
    names(table_to_rename) <- RENAMING_ANALYSIS_COLUMNS[names(table_to_rename)]
    return(table_to_rename)
  } else {
    stop("Class not handled for renaming")
  }
}
