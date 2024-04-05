## Map functions

# To create the map of 7 sites (leaflet):
map_all <- function(title,
                    hospitals,
                    ods_sites) {
  colour_sites <- "blue"
  
  sites <- ods_sites |>
    filter(is.na(effective_to)) |>
    inner_join(hospitals, by = "site_code")
  
  map <- leaflet(data = sites,
                 options = leafletOptions(zoomControl = FALSE)) |>
    addProviderTiles(providers$CartoDB) |>
    setView(lng = -1.75,
            lat = 52.5,
            zoom = 7) |>
    addCircleMarkers(
      data = sites,
      lng = ~ long,
      lat = ~ lat,
      color = colour_sites,
      label = sites$name,
      labelOptions = labelOptions(
        noHide = T,
        direction = 'top',
        textOnly = T,
        textsize = "11px",
        style = list("font-weight" = "bold")
      )
    )
  
  map <- oceanis::add_titre(map = map, titre = title)
  
  map
  
}

# To get a map with control locations for an intervention site:
map_controls <- function(hospital_of_interest
                         ,
                         hospitals
                         ,
                         controls
                         ,
                         ods_sites) {
  colour_sites <- "blue"
  colour_controls <- "orange"
  
  site_code_of_interest <- hospitals |>
    dplyr::filter(alias == hospital_of_interest) |>
    left_join(ods_sites |> select(site_code, long, lat), by = 'site_code')
  
  controls <- controls |>
    dplyr::filter(organisation_code == site_code_of_interest$organisation_code) |>
    select(matching_organisation_code, site_code, site_name) |>
    left_join(ods_sites |> select(site_code, long, lat), by = 'site_code')
  
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
    addProviderTiles(providers$CartoDB) |>
    setView(lng = -1.75,
            lat = 52.5,
            zoom = 6) |>
    addCircleMarkers(
      data = controls,
      lng = ~ long,
      lat = ~ lat,
      label = ~ htmlEscape(site_name),
      color = colour_controls,
      radius = 6,
      fillOpacity = 0.6,
      stroke = F
    ) |>
    addCircleMarkers(
      data = site_code_of_interest,
      lng = ~ long,
      lat = ~ lat,
      color = colour_sites,
      label = site_code_of_interest$name,
      labelOptions = labelOptions(
        noHide = T,
        direction = 'top',
        textOnly = T,
        textsize = "11px",
        style = list("font-weight" = "bold")
      )
    )
  
  map <-
    oceanis::add_titre(
      map = map,
      titre = paste0(
        "Map of ",
        site_code_of_interest$name,
        " and similar control sites"
      )
    )
  
  map
  
}
