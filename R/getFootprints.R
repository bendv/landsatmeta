#' @title Get Footprints
#'
#' @description Get individual scene footprints from several EarthExplorer csv metadata files
#'
#' @param files Character. Vector of input csv filenames (from EarthExplorer)
#' @param method Function/Character. Function (e.g. mean, median) to calculate summary of footprint vertices. "union" (as character) is the default and computes vertices as the outermost bounding box of all footprints. Other built-in summary functions (e.g. mean, median) do not require quotes.
#'
#' @import sp
#' @export
#'
#' @examples
#' fl <- list.files('inst/extdata/', pattern = glob2rx('*.csv'), full.names = TRUE)
#' fp <- getFootprints(fl)
#' plot(fp)
#'
#' # or if you want to get fancy....
#' library(mapview)
#' mapView(fp)


getFootprints <- function(files, method = "union") {

  df <- parsedata(files)
  df$pr <- sprintf("%s_%s", df$path, df$row)
  
  if(method == "union") {
    method = list(min, max)
  } else {
    method = list(method, method)
  }

  footprints <- data.frame(
    pr = sprintf("PR_%s", unique(df$pr)),
    ul_lat = tapply(df$upper_left_lat, df$pr, method[[1]]),
    ul_long = tapply(df$upper_left_long, df$pr, method[[1]]),
    ur_lat = tapply(df$upper_right_lat, df$pr, method[[2]]),
    ur_long = tapply(df$upper_right_long, df$pr, method[[2]]),
    lr_lat = tapply(df$lower_right_lat, df$pr, method[[2]]),
    lr_long = tapply(df$lower_right_long, df$pr, method[[2]]),
    ll_lat = tapply(df$lower_left_lat, df$pr, method[[1]]),
    ll_long = tapply(df$lower_left_long, df$pr, method[[1]]))

  makePolygons <- function(dfrow) {
    id <- dfrow[1]
    dfrow <- dfrow[-1]
    coords <- matrix(as.numeric(dfrow), nc=2, byrow = TRUE)
    coords <- rbind(coords, coords[1,])
    coords <- cbind(coords[,2], coords[,1])
    poly <- Polygons(list(Polygon(coords)), ID = id)
    return(poly)
  }

  polys <- SpatialPolygons(apply(footprints, 1, makePolygons),
                           proj4string = CRS("+proj=longlat +ellps=WGS84"))

  polys <- SpatialPolygonsDataFrame(polys,
                                    data = data.frame(ID = as.character(footprints$pr),
                                                      row.names = as.character(footprints$pr)))
  return(polys)
}
