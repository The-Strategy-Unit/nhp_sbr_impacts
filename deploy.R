deploy <- function(server_name, app_id) {
  rsconnect::deployDoc(
    server = server_name,
    appId = app_id,
    doc = "Measuring_the_impact_of_single_bedroom_Causal_Impact_Analysis.html",
    appName = "nhp_sbr_impacts",
    appTitle = "Measuring impact of single bedroom hospitals",
    lint = FALSE,
    forceUpdate = TRUE
  )
}

deploy(server_name = "connect.su.mlcsu.org", app_id = 129) ##new server
deploy(server_name = "connect.strategyunitwm.nhs.uk", app_id = 380) ##old server
