get_scen_presence_absence <- function(random_sp_PA_years, ## Yearly PA rasters needed
                                      yearly_n, ## n per year
                                      years,
                                      scen){ # species behaviour scenarios

  if(scen ==  "static"){

    ## Static: sample based on pa raster in first year
    static_occs <- lapply(1:length(years), function(i){
      tibble(sampleOccurrences(random_sp_PA_years[[1]],
                               n = yearly_n[i],
                               type = "presence-absence",
                               plot = F)$sample.points) %>%
        mutate(year = years[i])
    })
    occ <- do.call(rbind, static_occs)

  } else if(scen == "lead_track"){

    ## Only leading edge is tracking cc: take cumulative maximum of pa rasters
    lead_track_dummy <- random_sp_PA_years
    lead_track_occs <- list()
    for(i in 1:length(years)){

      if(i>1){
        lead_track_dummy[[i]]$suitab.raster <- max(c(lead_track_dummy[[i-1]]$suitab.raster,
                                                                 random_sp_PA_years[[i]]$suitab.raster))
        lead_track_dummy[[i]] <- convertToPA(lead_track_dummy[[i]],
                                             PA.method = random_sp_PA_years[[i]]$PA.conversion[1],
                                             prob.method = random_sp_PA_years[[i]]$PA.conversion[2],
                                             alpha = as.numeric(random_sp_PA_years[[i]]$PA.conversion["alpha"]),
                                             beta = as.numeric(random_sp_PA_years[[i]]$PA.conversion["beta"]),
                                             plot = F)
     }
      lead_track_occs[[i]] <- tibble(sampleOccurrences(lead_track_dummy[[i]],
                                                       n = yearly_n[i],
                                                       type = "presence-absence",
                                                       plot = F)$sample.points) %>%
        mutate(year = years[i])

    }
    occ <- do.call(rbind, lead_track_occs)

  } else if(scen == "trail_track"){

    ## Only trailing edge is tracking cc: cumulative minimum of pa.rasters
    trail_track_dummy <- random_sp_PA_years
    trail_track_occs <- list()
    for(i in 1:length(years)){

      if(i>1){
      trail_track_dummy[[i]]$suitab.raster <- min(c(trail_track_dummy[[i-1]]$suitab.raster,
                                                      random_sp_PA_years[[i]]$suitab.raster))
        trail_track_dummy[[i]] <- convertToPA(trail_track_dummy[[i]],
                                             prob.method = random_sp_PA_years[[i]]$PA.conversion[2],
                                             alpha = as.numeric(random_sp_PA_years[[i]]$PA.conversion["alpha"]),
                                             beta = as.numeric(random_sp_PA_years[[i]]$PA.conversion["beta"]),
                                             PA.method = random_sp_PA_years[[i]]$PA.conversion[1],
                                             plot = F)
      }
      trail_track_occs[[i]] <- tibble(sampleOccurrences(trail_track_dummy[[i]],
                                                         n = yearly_n[i],
                                                         type = "presence-absence",
                                                         plot = T)$sample.points) %>%
        mutate(year = years[i])

    }
    occ <- do.call(rbind, trail_track_occs)

  } else if(scen == "both_track") {
    
    ## Both edges are tracking cc: use pa.raster for each year
    both_track_occs <- lapply(1:length(years), function(i){
      tibble(sampleOccurrences(random_sp_PA_years[[i]],
                               n = yearly_n[i],
                               type = "presence-absence",
                               plot = F)$sample.points) %>%
        mutate(year = years[i])
    })
    occ <- do.call(rbind, both_track_occs)

  }

  ## output in flexsdm-form
  return(occ %>% rename(lon = x, lat = y, pr_ab = Observed) %>%
         mutate(sp = "random") %>%
         select(-Real))

}
