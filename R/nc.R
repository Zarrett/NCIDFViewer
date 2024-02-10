# install.packages(c("ncdf4", "ncdf4.helpers"))
# install.packages(c("terra", "data.table"))

## data available @ "https://owncloud.cesnet.cz/index.php/s/HyKD3KXSOontoKX/download"

library(ncdf4)
library(ncdf4.helpers)
library(terra)
library(data.table)
library(dplyr)

#' Returns list of nc objects
#'
#' @param folderPath A path to folder containing .nc files
#' @param ids IDs to be used to select .nc data
#' @return list of nc objects
#' @examples
#' ncRead("C:/folderPath", cellIDs = c(296, 263, 264, 265, 295, 297, 327, 328, 329))
ncRead <- function(folderPath, ids) {
    fls <- list.files(path = folderPath,
                      recursive = TRUE,
                      pattern = ".nc",
                      full.names = TRUE,
                      all.files = TRUE)

    if(length(fls) < 1) {
      stop("Folder doesn't contain .nc files")
    }

    if(length(ids) < 1) {
      stop("Folder doesn't contain .nc files")
    }

    dta_all <- lapply(
      X = fls,
      FUN = function(i) {

        ex <- try(
          expr = {

            nc <- ncdf4::nc_open(filename = i)

            lon <- ncdf4::ncvar_get(nc = nc,
                             varid = "lon")
            lat <- ncdf4::ncvar_get(nc = nc,
                             varid = "lat")
            pr <- ncdf4::ncvar_get(nc = nc,
                            varid = "pr")
            time <- nc.get.time.series(f = nc)

            ncdf4::nc_close(nc = nc)

            r <- terra::rast(x = pr)
            ext(x = r) <- c(range(lon),
                            range(lat))
            crs(x = r) <- "epsg:4326"

            xy <- terra::xyFromCell(object = r,
                             cell = ids)
            val <- t(x = raster::extract(x = r,
                                 y = xy))

            dta <- data.table(time = time, val)
          },
          silent = TRUE
        )

        if (inherits(x = ex,
                     what = "try-error")) {
          return(NULL)
        } else {
          return(dta)
        }
      }
    )

    return(dta_all)
}
