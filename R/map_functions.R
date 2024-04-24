## Map functions

# To add label for sites with option to change offset
add_site_markers <- function(map, data, offset = c(0, 0)) {
  
  map <- map |> 
    addCircleMarkers(
      data = data,
      lng = ~ long,
      lat = ~ lat,
      color = "blue",
      label = data |> pull(name),
      labelOptions = labelOptions(
        noHide = T,
        direction = "top",
        textOnly = T,
        textsize = "11px",
        style = list("font-weight" = "bold"),
        offset = offset
      )
    )
  
  return(map)
}

# To create the map of 7 sites (leaflet):
map_all <- function(title,
                    hospitals,
                    ods_sites) {
  sites <- ods_sites |>
    filter(is.na(effective_to)) |>
    inner_join(hospitals, by = "site_code")
  
  sites_not_clatterbridge <- sites |> filter(site_code != "REN22")
  sites_clatterbridge <- sites |> filter(site_code == "REN22")
  
  map <- leaflet(data = sites,
                 options = leafletOptions(zoomControl = FALSE)) |>
    addTiles(urlTemplate ="https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png")|>
   # addProviderTiles(providers$CartoDB) |>
    setView(lng = -1.75,
            lat = 52.5,
            zoom = 7) |>
    add_site_markers(sites_not_clatterbridge) |>
    add_site_markers(sites_clatterbridge, c(0, 15))
  
  map <- oceanis::add_titre(map = map, titre = title)
  
  map
  
}

# To get a map with control locations for an intervention site:
map_controls <- function(org_code_of_interest,
                         hospitals,
                         controls,
                         ods_sites) {
  site_code_of_interest <- hospitals |>
    dplyr::filter(organisation_code == org_code_of_interest) |>
    left_join(ods_sites |> select(site_code, long, lat), by = 'site_code')
  
  controls <- controls |>
    dplyr::filter(organisation_code == site_code_of_interest$organisation_code
                  & rank_of_ranks <= 20) |>
    select(matching_organisation_code, site_code, site_name) |>
    left_join(ods_sites |> select(site_code, long, lat), by = 'site_code')
  
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
    addTiles(urlTemplate ="https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png")|>
    # addProviderTiles(providers$CartoDB) |>
    setView(lng = -1.75,
            lat = 52.5,
            zoom = 6) |>
    addCircleMarkers(
      data = controls,
      lng = ~ long,
      lat = ~ lat,
      label = ~ htmlEscape(site_name),
      color = "orange",
      radius = 6,
      fillOpacity = 0.6,
      stroke = F
    ) |>
    add_site_markers(site_code_of_interest)
  
  map <- oceanis::add_titre(
    map = map,
    titre = paste0(
      "Map of ",
      site_code_of_interest$name,
      " and similar control sites"
    )
  )
  
  map
  
}
