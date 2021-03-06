#' @title Parse metadata
#'
#' @description Parse select attributes from metadata csv files
#'
#' @param files Character. Vector of input csv filenames (from EarthExplorer)
#'
#' @return \code{data.frame} with cloud cover per date per image
#'
#' @import stringr
#' @export
#'
#' @examples
#' fl <- list.files('inst/extdata/', pattern = glob2rx('*.csv'), full.names = TRUE)
#' df <- parsedata(fl)
#' head(df)
#' tail(df)
#'
#' # plot cloud cover per DOY per year per path/row
#' library(magrittr)
#' library(ggplot2)
#' df$year <- format(df$date, format = "%Y") %>% as.numeric()
#' df$doy <- format(df$date, format = "%j") %>% as.numeric()
#' df$pr <- sprintf("%s-%s", df$path, df$row) %>% factor()
#' p <- ggplot(data = df, aes(x = doy, y = year)) +
#'    geom_point(aes(size = cloud_cover, colour = sensor), alpha = 0.5) +
#'    facet_wrap(~ pr, nc = 1)
#' p

parsedata <- function(files) {

  df <- vector('list', length(files))
  for(i in 1:length(files)) {

    patt <- str_extract(files[i], 'LANDSAT_(.*?)_')
    patt <- substr(patt, 1, nchar(patt) - 1)

    sensor <- switch(patt,
                     LANDSAT_TM = "TM",
                     LANDSAT_ETM = "ETM+",
                     LANDSAT_8 = "OLI")

    df[[i]] <- read.csv(files[i], stringsAsFactors = FALSE, quote = "", fileEncoding = 'latin1')

    if(patt == "LANDSAT_8") {
      df[[i]] <- subset(df[[i]], select = c(Landsat.Scene.Identifier,
                                            Date.Acquired,
                                            WRS.Path,
                                            WRS.Row,
                                            Scene.Cloud.Cover,
                                            NW.Corner.Lat.dec,
                                            NW.Corner.Long.dec,
                                            NE.Corner.Lat.dec,
                                            NE.Corner.Long.dec,
                                            SW.Corner.Lat.dec,
                                            SW.Corner.Long.dec,
                                            SE.Corner.Lat.dec,
                                            SE.Corner.Long.dec
))
    } else {
      df[[i]] <- subset(df[[i]], select = c(Landsat.Scene.Identifier,
                                            Date.Acquired,
                                            WRS.Path,
                                            WRS.Row,
                                            Cloud.Cover,
                                            Corner.Upper.Left.Lat.dec,
                                            Corner.Upper.Left.Long.dec,
                                            Corner.Upper.Right.Lat.dec,
                                            Corner.Upper.Right.Long.dec,
                                            Corner.Lower.Left.Lat.dec,
                                            Corner.Lower.Left.Long.dec,
                                            Corner.Lower.Right.Lat.dec,
                                            Corner.Lower.Right.Long.dec
))
    }
    names(df[[i]]) <- c("sceneID", "date", "path", "row", "cloud_cover", "upper_left_lat", "upper_left_long", "upper_right_lat", "upper_right_long", "lower_left_lat", "lower_left_long", "lower_right_lat", "lower_right_long")

    df[[i]]$sensor <- sapply(df[[i]]$date, FUN=function(x) {
      if(sensor == "ETM+") {
        if(x < as.Date("2003-03-31")) {
          "ETM+ SLC-on"
        } else {
          "ETM+ SLC-off"
        }
      } else {
        sensor
      }
    })
  }

  df <- do.call("rbind", df)
  df$date <- as.Date(df$date, format = "%Y/%m/%d")
  return(df)
}

