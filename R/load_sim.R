#' Example feature raster
#'
#' Load the example feature raster shipped with the package.
#'
#' @return A `terra::SpatRaster`.
#' @export
load_sim_features_raster <- function() {
  path <- system.file("extdata", "sim_features_raster.tif", package = "multiscape")
  terra::rast(path)
}


