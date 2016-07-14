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
#'

parsedata <- function(files) {

  df <- vector('list', length(files))
  for(i in 1:length(files)) {

    patt <- str_extract(files[i], 'LANDSAT_(.*?)_')
    patt <- substr(patt, 1, nchar(patt) - 1)

    df[[i]] <- read.csv(file[i], stringsAsFactors = FALSE)

    if(patt == "LANDSAT_8") {
      df[[i]] <- subset(df[[i]], select = c(Landsat.Scene.Identifier,
                                            Date.Acquired,
                                            WRS.Path,
                                            WRS.Row,
                                            Scene.Cloud.Cover))
    } else {
      df[[i]] <- subset(df[[i]], select = c(Landsat.Scene.Identifier,
                                            Date.Acquired,
                                            WRS.Path,
                                            WRS.Row,
                                            Cloud.Cover))
    }
    names(df[[i]]) <- c("sceneID", "date", "path", "row", "cloud_cover")
  }

  df <- do.call("rbind", df)
  df$date <- as.Date(df$date, format = "%Y/%m/%d")
  return(df)
}
