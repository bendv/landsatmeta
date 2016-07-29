#' @title Get Footprints
#'
#' @descriptions Get individual scene footprints from several EarthExplorer csv metadata files
#'
#' @param files Character. Vector of input csv filenames (from EarthExplorer)
#' @param method Function. Function to use to calculate vertices (default is median)
#'
#' @import sp
#'
#' @examples
#' fl <- list.files('inst/extdata/', pattern = glob2rx('*.csv'), full.names = TRUE)
#' fp <- getFootprints(fl)
#' plot(fp)
#'
#' # or if you want to get fancy....
#' library(mapview)
#' mapView(fp)


getFootprints <- function(files, method = max) {

  df <- parsedata(files)
  df$pr <- sprintf("%s_%s", df$path, df$row)

  footprints <- data.frame(
    pr = sprintf("PR_%s", unique(df$pr)),
    ul_lat = tapply(df$upper_left_lat, df$pr, method),
    ul_long = tapply(df$upper_left_long, df$pr, method),
    ur_lat = tapply(df$upper_right_lat, df$pr, method),
    ur_long = tapply(df$upper_right_long, df$pr, method),
    lr_lat = tapply(df$lower_right_lat, df$pr, method),
    lr_long = tapply(df$lower_right_long, df$pr, method),
     ll_lat = tapply(df$lower_left_lat, df$pr, method),
     ll_long = tapply(df$lower_left_long, df$pr, method))

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
