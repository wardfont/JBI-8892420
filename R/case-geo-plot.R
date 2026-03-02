make_case_geo_bi_period_plot <- function(case_gams,
                                         env_files,
                                         crs = "EPSG:3035",
                                         years = 2013:2017,
                                         species,
                                         plot_species,
                                         predict_models = c("first", "sliding"),
                                         palette = "BlueGold",
                                         models) {

  # env in 4326
  eu_WGS84 <- crop(crop(vect(getEuropeCountries(mergeCountry = FALSE)),
    rast(env_files[[1]])[[1]],
    ext = TRUE
  ), as.polygons(rast(env_files[[1]])[[1]]))

  eu <- project(
    eu_WGS84,
    crs
  )

  order <- tibble(expand.grid(models, species)) %>%
    setNames(c("models", "species")) %>%
    mutate(n = 1:length(models))

  sp_maps <- lapply(plot_species, function(sp) {
    mods_pr <- lapply(predict_models, function(mod) {
      i <- order %>%
        filter(species == sp, models == mod) %>%
        pull(n)

      years_pr <- lapply(years, function(year) {
        if (!is.null(case_gams[[i]])) {
          pr <- sdm_predict(
            models = case_gams[[i]],
            pred = rast(grep(year, env_files, value = T))
          )[[1]]

          names(pr) <- mod
        } else {
          pr <- NULL
        }

        return(pr)
      })

      mod_pr <- mean(rast(years_pr))
      names(mod_pr) <- mod

      writeRaster(mod_pr, paste0("./output/predict/", sp, "_", mod, "_", years[1], "_johnston.tif"), overwrite = T)
      
      return(mod_pr)
    })


    mods_pr <- list(rast(paste0("./output/predict/", sp, "_first_", 2060, "_johnston.tif")),
                    rast(paste0("./output/predict/", sp, "_sliding_", 2060, "_johnston.tif")))

    if (sum(sapply(mods_pr, is.null)) == 0) {
      sp_pr <- project(rast(mods_pr), crs)

      ## https://web.natur.cuni.cz/~langhamr/lectures/vtfg1/mapinfo_2/barvy/colors.html
      ## https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html

      ## Choose palette: https://cran.r-project.org/web/packages/biscale/vignettes/bivariate_palettes.html

      bi_sp_pr <- as.data.frame(sp_pr * 100, xy = TRUE)


      if (identical(predict_models, c("first", "sliding"))) {
        bi_sp_pr <- bi_class(bi_sp_pr, x = "first", y = "sliding", style = "equal", dim = 4)
        break_vals <- bi_class_breaks(bi_sp_pr, x = "first", y = "sliding", style = "equal", dim = 4)
      } else if (identical(predict_models, c("early", "late"))) {
        bi_sp_pr <- bi_class(bi_sp_pr, x = "early", y = "late", style = "equal", dim = 4)
        break_vals <- bi_class_breaks(bi_sp_pr, x = "early", y = "late", style = "equal", dim = 4)
      }

      map <- ggplot() +
        geom_raster(data = bi_sp_pr, aes(x = x, y = y, fill = bi_class)) +
        bi_scale_fill(pal = palette, dim = 4) +
        coord_quickmap() +
        labs(
          x = sp,
          y = "",
          caption = ""
        ) +
        geom_spatvector(data = eu, fill = NA) +
        bi_theme(base_size = 16) +
        theme(legend.position = "none")

      ## return(list(
      ##   map = map,
      ##   break_vals = break_vals
      ## ))
    } else {
      map <- NULL
    }

    return(map)
  })

}

