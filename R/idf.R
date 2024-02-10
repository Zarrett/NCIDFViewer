library(CoSMoS)
library(magrittr)
library(dplyr)

#' Returns list of IDF data generated from provided raster data
#'
#' @param x Raster data
#' @param rp list of time periods in years
#' @param dur list of time durations in hours
#' @param aggfun selection method string
#' @param dist distribution method string
#' @return list of nc objects
#' @examples
#' calculateIdf(rastrData, rp = c(2, 5, 10, 25, 50, 100), dur = c(1, 2, 5, 10, 24, 48), aggfun = "mean", dist = "gev")
calculateIdf <- function(x,
                rp = c(2, 5, 10, 25, 50, 100),
                dur = c(1, 2, 5, 10, 24, 48),
                aggfun = "mean",
                dist = "gev", ...) {

  agg <- lapply(
    X = dur,
    FUN = function(d) {

      out <- x[, .(time = time,
                   val = do.call(what = paste0("froll", aggfun),
                                 args = list(x = value,
                                             n = d,
                                             align = "center",
                                             fill = 0)))]
      out
    }
  )

  quant <- lapply(
    X = agg,
    FUN = function(a) {

      mx <- a[, .(mx = max(x = val,
                           na.rm = TRUE)),
              by = year(x = time)]

      para <- fitDist(data = mx$mx,
                      dist = dist,
                      n.points = 10,
                      norm = "N4",
                      constrain = FALSE)

      prob <- 1 - 1/rp

      q <- qgev(p = prob,
                loc = para$loc,
                scale = para$scale,
                shape = para$shape)

      names(x = q) <- rp

      as.list(x = q)
    }
  )

  names(x = quant) <- dur

  quant_all <- rbindlist(l = quant,
                         idcol = "dur")
  quant_idf <- melt(data = quant_all,
                    id.vars = "dur",
                    variable.name = "rp")

  return(quant_idf)
}
