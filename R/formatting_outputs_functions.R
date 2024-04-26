format_controls_table <- function(data, org_code){
  
  table <- data |>
    filter(organisation_code == org_code) |>
    select(
      "Provider" = trust_name,
      "Provider code" = matching_organisation_code,
      "Site name" = site_name,
      "Site code" = site_code
    ) |>
    as_flextable(
      hide_grouplabel = TRUE,
      max_row = 20,
      show_coltype = FALSE
    ) |>
    align(part = "header", align = "center") |>
    bg(bg = "#f9bf07", part = "header") |>
    bold(bold = TRUE, part = "header") |>
    fontsize(size = 12, part = "all") |>
    padding(
      padding = 2,
      part = "all",
      padding.top = NULL
    ) |>
    autofit()
  
  return(table)
  
}



