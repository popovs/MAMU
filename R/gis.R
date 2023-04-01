# GIS functions

#' Rasterize VRI polygons
#'
#' [rasterize_vri()] will quickly rasterize a given VRI (multi)polygon using [fasterize::fasterize()]
#' under the hood at a default resolution of 10x10 m per pixel. By default, the function
#' filters out any polygons occuring >900m elevation as reported in the VRI 'Elevation' column.
#'
#' @param path Path to either .gdb directory, .shp file, or .gpkg containing the VRI polygons.
#' @param layer If path is a Geodatabase or Geopackage, layer name of the VRI polygons.
#' @param below_900m Logical (T/F). Should the function only rasterize polygons below 900m? Default `TRUE`. If `FALSE`, the function will ignore the 'Elevation' column and rasterize all polygons.
#' @param res Resolution in meters of the resultant raster. Default 10 m^2.
#'
#' @return
#' @export
#'
#' @examples
#' path <- "~/Documents/path/to/VRI.gdb"
#' layer <- "VRI_Algorithm_Jan11_2023"
#' vri <- rasterize_vri(path = path, layer = layer)
rasterize_vri <- function(path,
                          layer,
                          below_900m = TRUE,
                          res = 10) {
  if (grepl(".gdb$", path)) {
    # Check that `layer` param provided
    stopifnot(".gdb path provided. Please provide the layer name of the VRI to extract from the geodatabase." = !missing(layer))
    # Extract layer from Esri geodatabase
    gdb <- sf::st_layers(path)
    # Read
    message("Reading VRI layer...")
    vri <- sf::st_read(dsn = path, layer = layer)
  } else if (grepl(".shp$|.gpkg$", path)) {
    message("Reading VRI layer...")
    vri <- sf::st_read(path)
  }

  # Extract only polygons below 900m
  if (below_900m) vri <- vri[vri$Elevation == "Below 900m", ]
  # Keep only the shape
  vri <- vri[,"SHAPE"]

  # Set raster grid at provided resolution
  r <- raster::raster(vri, res = res)
  # Rasterize
  message("Rasterizing VRI layer...")
  vri <- fasterize::fasterize(vri, r)

  return(vri)
}
