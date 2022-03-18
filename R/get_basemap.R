
get_basemap <- function(x,
                        map = "physical",
                        size = c(16000,9000),
                        dpi = 900,
                        imageSR = 4326,
                        native = FALSE){

  x <- st_bbox(x)

  old_ratio <- (x[["xmax"]] - x[["xmin"]]) / (x[["ymax"]] - x[["ymin"]])
  new_ratio <- (size[[1]] / size[[2]])

  if (!all.equal(old_ratio, new_ratio)) {

    msg <- paste0(
      "Extent of image (size) differs from extent of x (bbox). ",
      "Map may be warped."
    )

    warning(msg, call. = FALSE)

  }

  base_url <- "http://services.arcgisonline.com/arcgis/rest/services/"

  map_name <- switch(
    map,
    "hillshade"= "Elevation/World_Hillshade",
    "dark"     = "Elevation/World_Hillshade_Dark",
    "natgeo"   = "NatGeo_World_Map",
    "usa"      = "USA_Topo_Maps",
    "imagery"  = "World_Imagery",
    "physical" = "World_Physical_Map",
    "shaded"   = "World_Shaded_Relief",
    "street"   = "World_Street_Map",
    "terrain"  = "World_Terrain_Base",
    "topo"     = "World_Topo_map",
    stope(paste0("The ", map, " map is not supported."), call. = FALSE)
  )

  map_service <- paste0(base_url, map_name, "/MapServer/export?")

  query <- list(
    bbox    = paste(x, collapse = ","),
    bboxSR  = st_crs(x)$epsg,
    imageSR = imageSR,
    f       = "image",
    format  = "png",
    size    = paste(size, collapse = ","),
    dpi     = dpi
  )

  request <- httr::GET(
    map_service,
    query = query
  )

  result <- httr::content(request)

  if (native) {

    result <- grDevices::as.raster(result, native = TRUE)

  }

  return(result)

}
