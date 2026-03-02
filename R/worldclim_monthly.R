worldclim_monthly <- function(poly){ ## Input is polygon for which to crop

  ## All available periods
  periods <- c("1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2021")
  ## Variables to download
  vars <- c("tmin", "tmax", "prec")
  ## Base url to download files from
  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m"

  ## Go over variables
  output_tifs <- lapply(vars, function(var){
    ## Go over periods
    lapply(periods, function(period){

      output_tif <- paste0("./output/worldclim/", var, "-", period, ".tif")

      if(!file.exists(paste0("./output/worldclim/", var, "-", period, ".tif"))){

        zip <- paste0("./data/worldclim/", var, "-", period, ".zip")

        download.file(paste0(base_url, "_", var, "_", period, ".zip"),
                      destfile = zip,
                      method = "wget", extra = "-r -p --random-wait")

        unzip(zip, exdir = "./data/worldclim/")

        tifs <- list.files("./data/worldclim", pattern = ".tif", full.names = T)

        ## crop and mask based on polygon
        r <- mask(crop(rast(tifs), poly), poly)
        start_end <- unlist(strsplit(period, "-"))
        names(r) <- paste(var, rep(start_end[1]:start_end[2], each = 12), 1:12, sep = "_")

        writeRaster(r, output_tif, overwrite = T)

        file.remove(c(zip,tifs))
        gc()

      }
      return(output_tif)
    })
  })

  return(unlist(output_tifs))

}
