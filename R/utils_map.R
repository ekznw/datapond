# Public interactive basemaps: no account, token or provider API key.
# Keep attribution visible and let the browser honour tile caching headers.
# This is an online background; dataset overlays remain local to the app.
datapond_basemap_groups <- function() c("Street map", "Satellite (EOX 2016)")

add_datapond_basemap <- function(map, overlay_groups = character()) {
  groups <- datapond_basemap_groups()
  map <- leaflet::addTiles(
    map,
    urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
    group = groups[1],
    options = leaflet::tileOptions(maxZoom = 19)
  )
  # The yearless service identifier is explicitly the 2016 mosaic in EOX's
  # WMTS capabilities. Do not replace it with a newer, differently licensed year.
  map <- leaflet::addTiles(
    map,
    urlTemplate = "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless_3857/default/g/{z}/{y}/{x}.jpg",
    attribution = paste0(
      '<a href="https://s2maps.eu">Sentinel-2 cloudless</a> by ',
      '<a href="https://eox.at">EOX IT Services GmbH</a> ',
      '(Contains modified Copernicus Sentinel data 2016 &amp; 2017) | ',
      '<a href="https://creativecommons.org/licenses/by/4.0/">CC BY 4.0</a> | ',
      '<a href="https://maps.eox.at">EOX Maps</a>'
    ),
    group = groups[2],
    options = leaflet::tileOptions(maxNativeZoom = 14, maxZoom = 19)
  )
  map <- leaflet::hideGroup(map, groups[2])
  leaflet::addLayersControl(
    map,
    baseGroups = groups,
    overlayGroups = overlay_groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
}
