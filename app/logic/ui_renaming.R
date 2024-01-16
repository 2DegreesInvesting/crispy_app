RENAMING_SCENARIOS <- c(
  "NGFS2022_REMIND_NDC" = "REMIND NDC",
  "NGFS2022_MESSAGE_NDC" = "MESSAGE NDC",
  "NGFS2022_GCAM_NDC" = "GCAM NDC",
  "WEO2021_STEPS" = "WEO STEPS",
  "Oxford2021_base" = "OXFORD BASELINE",
  "IPR2021_baseline" = "IPR BASELINE",
  "Oxford2021_fast" = "OXFORD B2DS",
  "NGFS2022_MESSAGE_B2DS" = "MESSAGE B2DS",
  "NGFS2022_MESSAGE_DT" = "MESSAGE DT",
  "NGFS2022_REMIND_B2DS" = "REMIND B2DS",
  "NGFS2022_REMIND_DT" = "REMIND DT",
  "NGFS2022_REMIND_NZ2050" = "REMIND NZ2050",
  "NGFS2022_MESSAGE_DN0" = "MESSAGE DN0",
  "NGFS2022_GCAM_B2DS" = "GCAM B2DS",
  "NGFS2022_MESSAGE_NZ2050" = "MESSAGE NZ2050",
  "WEO2021_NZE_2050" = "WEO NZ2050",
  "NGFS2022_REMIND_DN0" = "REMIND DN0",
  "WEO2021_SDS" = "WEO B2DS",
  "IPR2021_RPS" = "IPR NZ2050",
  "IPR2021_FPS" = "IPR B2DS",
  "NGFS2022_GCAM_NZ2050" = "GCAM NZ2050",
  "NGFS2022_GCAM_DN0" = "GCAM DN0",
  "NGFS2022_GCAM_DT" = "GCAM DT"
)
REV_RENAMING_SCENARIOS <- stats::setNames(names(RENAMING_SCENARIOS), RENAMING_SCENARIOS)

RENAMING_ANALYSIS_COLUMNS <- c(
  "portfolio.ald_sector" = "Sector",
  "portfolio.exposure_value_usd" = "Exposure (USD)",
  "portfolio.pd_portfolio" = "Probability of Default",
  "portfolio.loss_given_default" = "Loss Given Default",
  "crispy.run_id" = "Run ID",
  "crispy.scenario_geography" = "Scenario Geography",
  "crispy.term" = "Term",
  # "crispy.roll_up_type" ,
  "crispy.baseline_scenario" = "Baseline Scenario",
  "crispy.shock_scenario" = "Target Scenario",
  "crispy.risk_free_rate" = "Risk Free Rate",
  "crispy.discount_rate" = "Discount Rate",
  "crispy.dividend_rate" = "Dividend Rate",
  "crispy.growth_rate" = "Growth Rate",
  "crispy.shock_year" = "Shock Year",
  "crispy.net_present_value_baseline" = "Net Present Value (Baseline)",
  "crispy.net_present_value_shock" = "Net Present Value (Shock)",
  "crispy.pd_baseline" = "Probability of Default (Baseline)",
  "crispy.pd_shock" = "Probability of Default (Shock)",
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
