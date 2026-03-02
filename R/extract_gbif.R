extract_gbif <- function(gbif_sblocked, ## gbif with blocked or random cross-validation folds, environment
                         envs, ## Environment files
                         model, ## Name of model
                         model_year = NULL, ## Year of model
                         vars) { ## Which variables to include
  
  ## Matched model, use all observations, couple observation with environment of same year
  if (model == "sliding") {
    da_extracted_yrs <- lapply(sort(unique(gbif_sblocked$year)), function(yr) {
      env_rast <- rast(grep(yr, envs, value = T))

      da <- gbif_sblocked %>%
        filter(year == yr)

      extracted <- as_tibble(
        extract(
          env_rast,
          vect(da,
            geom = c("lon", "lat"),
            crs = crs(env_rast)
          ),
          ID = F
        )
      )

      da_extracted_yr <- as_tibble(cbind(da, extracted))
    })

    gbif_extracted <- do.call(rbind, da_extracted_yrs)
  } else if (model %in% c("first", "early", "late")) { ## static model, couple observation with environment of the year of the model
    env_rast <- rast(grep(model_year, envs, value = T))

    extracted <- as_tibble(
      extract(
        env_rast,
        vect(gbif_sblocked,
          geom = c("lon", "lat"),
          crs = crs(env_rast)
        ),
        ID = FALSE
      )
    )

    gbif_extracted <- as_tibble(cbind(gbif_sblocked, extracted))
  } else if (grepl("later", model)) { ## Couple with only observations from after model year
    env_rast <- rast(grep(model_year, envs, value = T))

    extracted <- as_tibble(
      extract(
        env_rast,
        vect(gbif_sblocked,
          geom = c("lon", "lat"),
          crs = crs(env_rast)
        ),
        ID = FALSE
      )
    )

    gbif_extracted <- as_tibble(cbind(gbif_sblocked, extracted)) %>%
      filter(year >= (model_year - 19))
    
  } else if (grepl("matched", model)) { ## !! is not machted approach in paper, but matching observations to static period!! Couple with only observations from within 20-year period
    env_rast <- rast(grep(model_year, envs, value = T))

    extracted <- as_tibble(
      extract(
        env_rast,
        vect(gbif_sblocked,
          geom = c("lon", "lat"),
          crs = crs(env_rast)
        ),
        ID = FALSE
      )
    )

    gbif_extracted <- as_tibble(cbind(gbif_sblocked, extracted)) %>%
      filter(year %in% (model_year - 19):model_year)
  }

  return(gbif_extracted %>% select(lon, lat, pr_ab, year, sp, .part, all_of(vars)))
}


case_extract_gbif <- function(gbif_sblocked, envs, model, model_year = NULL, vars) {
  if (model == "sliding") {
    da_extracted_yrs <- lapply(sort(unique(gbif_sblocked$year)), function(yr) {
      env_rast <- rast(grep(yr, envs, value = T))

      da <- gbif_sblocked %>%
        filter(year == yr)

      extracted <- as_tibble(
        extract(
          env_rast,
          vect(da,
            geom = c("lon", "lat"),
            crs = crs(env_rast)
          ),
          ID = F
        )
      )

      da_extracted_yr <- as_tibble(cbind(da, extracted))
    })

    gbif_extracted <- do.call(rbind, da_extracted_yrs)
  } else if (model %in% c("first", "early", "late")) {
    env_rast <- rast(grep(model_year, envs, value = T))

    extracted <- as_tibble(
      extract(
        env_rast,
        vect(gbif_sblocked,
          geom = c("lon", "lat"),
          crs = crs(env_rast)
        ),
        ID = FALSE
      )
    )

    gbif_extracted <- as_tibble(cbind(gbif_sblocked, extracted))
  } else if (grepl("later", model)) {
    env_rast <- rast(grep(model_year, envs, value = T))

    extracted <- as_tibble(
      extract(
        env_rast,
        vect(gbif_sblocked,
          geom = c("lon", "lat"),
          crs = crs(env_rast)
        ),
        ID = FALSE
      )
    )

    gbif_extracted <- as_tibble(cbind(gbif_sblocked, extracted)) %>%
      filter(year >= (model_year - 19))
  } else if (grepl("matched", model)) {
    env_rast <- rast(grep(model_year, envs, value = T))

    extracted <- as_tibble(
      extract(
        env_rast,
        vect(gbif_sblocked,
          geom = c("lon", "lat"),
          crs = crs(env_rast)
        ),
        ID = FALSE
      )
    )

    gbif_extracted <- as_tibble(cbind(gbif_sblocked, extracted)) %>%
      filter(year %in% (model_year - 19):model_year)
  }

  return(gbif_extracted %>% select(lon, lat, pr_ab, year, .part, all_of(vars)))
}
