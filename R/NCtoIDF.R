if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("ncdf4", quietly = TRUE)) {
  install.packages("ncdf4")
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

if (!requireNamespace("CoSMoS", quietly = TRUE)) {
  install.packages("CoSMoS")
}

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(terra)
library(data.table)
source("R/idf.R")
source("R/nc.R")

#' Generates and plots IDF curves for provided .nc data
#'
#' @param folderPath A path to folder containing .nc files
#' @param randomValues Toogle random cellIDs
#' @param cellIDs IDs to be used to select .nc data
#' @examples
#' NCtoIDF("C:/folderPath", cellIDs = c(296, 263, 264, 265, 295, 297, 327, 328, 329))
NCtoIDF <- function(folderPath,
                    randomValues = FALSE,
                    cellIDs = c(296, 263, 264, 265, 295, 297, 327, 328, 329)) {

  dta_all <- ncRead(folderPath, cellIDs)

  dta_all <- rbindlist(l = dta_all)
  dta_all_m <- melt(data = dta_all,
                    id.vars = "time",
                    variable.name = "cell_id")

  dta_all_m$time <- as.POSIXct(dta_all_m$time)

  mx <- dta_all_m[, .(mx = max(value)),
                  by = .(cell_id, year(x = time))]

  spl_dta <- split(x = dta_all_m,
                   f = dta_all_m$cell_id)

  idf_dta <- lapply(X = spl_dta,
                      FUN = calculateIdf)

  idf_dta <- rbindlist(l = idf_dta,
                       idcol = "cell_id")

  ggplot(data = idf_dta,
         mapping = aes(x = as.numeric(x = dur),
                       y = value,
                       colour = rp)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Return\nperion",
                        values = c("yellow4", "steelblue", "red4",
                                   "darkgreen", "pink", "magenta4")) +
    labs(x = "Duration (hours)",
         y = "Intensity (mm/h)",
         title = "IDF curve") +
    theme_bw() +
    facet_wrap(facets = ~cell_id)
}
