calc_bioclim <- function(monthly_wc, history_length){ ## monthly Worldclim file and desired length of climate period

  clim <- rast(monthly_wc)

  ## All years present in Worldclim data
  years <- unique(as.numeric(unlist(lapply(names(clim), function(name){
    strsplit(name,"_")[[1]][2]
  }))))

  ## Calculate bioclimatic variables for each history length in years
  bioclim_files <- lapply(years[history_length]:years[length(years)], function(year){

    bioclim_file <- paste0("./output/bioclim/bioclim-",history_length, "-yr-", year, ".tif")

    if(!file.exists(bioclim_file)){

      year_pattern <- paste((year-history_length+1):year, collapse = "|")

      ## Calculate mean minimum temperature, maximum temperature and precipitation for each month (jan-dec)
      prec <- tapp(subset(clim,
                          grepl("prec", names(clim)) &
                          grepl(year_pattern, names(clim))),
                   rep(1:12, history_length),
                   mean)
      tmin <- tapp(subset(clim,
                          grepl("tmin", names(clim)) &
                          grepl(year_pattern, names(clim))),
                   rep(1:12, history_length),
                   mean)
      tmax <- tapp(subset(clim,
                          grepl("tmax", names(clim)) &
                          grepl(year_pattern, names(clim))),
                   rep(1:12, history_length),
                   mean)

      ## Calcualte bioclimatic variables
      bioclim <- biovars(prec = stack(prec),
                         tmin = stack(tmin),
                         tmax = stack(tmax))

      writeRaster(rast(bioclim), bioclim_file, overwrite = T)

      gc()
    }

    return(bioclim_file)

  })

  return(unlist(bioclim_files))

}

# crop_bioclim <- function(history_bioclim){
# 
#   filename <- gsub("bioclim/", "bioclim-crop/", history_bioclim)
# 
#   writeRaster(crop(rast(history_bioclim), crop_extent),
#                     filename, overwrite = T)
# 
#   return(filename)
# 
# }
