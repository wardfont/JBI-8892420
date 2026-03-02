get_ebird <- function(ebd_file, envs, months_incl) {
  env <- rast(envs[1])

  ebd_output_file <- "./output/ebird/ebd_filtered.txt"
  sed_output_file <- "./output/ebird/sed_filtered.txt"

  reduced_sed_output_file <- "./output/ebird/reduced_sed.txt"

  ## Extract data
  ebird_data <- ebd_file %>%
    auk_ebd(file_sampling = reduced_sed_output_file) %>%
    auk_bbox(bbox = c(-15, 25, 60, 75)) %>% ## bounding box for analysis
    auk_year(1979:2023) %>% ## Years in analysis
    auk_date(months_incl) %>%
    auk_complete() %>%
    auk_filter(
      file = ebd_output_file,
      file_sampling = sed_output_file,
      overwrite = TRUE
    )

  ## Zero fill data
  ebd_zf <- auk_zerofill(ebd_output_file, sampling_events = sed_output_file)

  ## Filter zerofilled data
  ebd_zf_df <- collapse_zerofill(ebd_zf) %>%
    mutate(
      observation_count = if_else(observation_count == "X",
        NA, observation_count
      ),
      observation_count = as.integer(observation_count),
      effort_distance_km = if_else(protocol_type == "Stationary",
        0, effort_distance_km
      ),
      species = scientific_name
    ) %>%
    filter(
      duration_minutes <= 60,
      effort_distance_km <= 1,
      number_observers <= 1
    )

  ## Keep only one record per year and environmental raster cell, presences if any presences was recorded
  ebd_zf_df_geo <- ebd_zf_df %>%
    mutate(
      cell_id = cells(env, vect(ebd_zf_df %>% select(longitude, latitude), geom = c("longitude", "latitude")))[, "cell"],
      year = year(observation_date)
    ) %>%
    group_by(species, cell_id, year) %>%
    summarise(pr_ab = max(species_observed)) %>%
    ungroup() %>%
    mutate(
      lon = xFromCell(env, cell_id),
      lat = yFromCell(env, cell_id)
    ) %>%
    select(species, lon, lat, year, pr_ab) %>%
    drop_na()

  return(ebd_zf_df_geo)
}

get_johnston_ebird <- function(ebd_file, envs, months_incl) {
  env <- rast(envs[1])
  
  ebd_output_file <- "./output/ebird/ebd_filtered.txt"
  sed_output_file <- "./output/ebird/sed_filtered.txt"
  
  reduced_sed_output_file <- "./output/ebird/reduced_sed.txt"
  
  ## Extract data
  ebird_data <- ebd_file %>%
    auk_ebd(file_sampling = reduced_sed_output_file) %>%
    auk_bbox(bbox = c(-15, 25, 60, 75)) %>% ## bounding box for analysis
    auk_year(1979:2023) %>% ## Years in analysis
    auk_date(months_incl) %>%
    auk_complete() %>%
    auk_filter(
      file = ebd_output_file,
      file_sampling = sed_output_file,
      overwrite = TRUE
    )
  
  ## Zero fill data
  ebd_zf <- auk_zerofill(ebd_output_file, sampling_events = sed_output_file)
  
  ## Filter zerofilled data
  ebd_zf_df <- collapse_zerofill(ebd_zf) %>%
    mutate(
      observation_count = if_else(observation_count == "X",
                                  NA, observation_count
      ),
      observation_count = as.integer(observation_count),
      effort_distance_km = if_else(protocol_type == "Stationary",
                                   0, effort_distance_km
      ),
      species = scientific_name
    ) %>%
    filter(
      duration_minutes <= 60,
      effort_distance_km == 0,
      number_observers == 1
    )
  
  set.seed(1996)
  ## Keep only one record per year and environmental raster cell, presences if any presences was recorded
  ebd_zf_df_geo <- ebd_zf_df %>%
    mutate(
      cell_id = cells(env, vect(ebd_zf_df %>% select(longitude, latitude), geom = c("longitude", "latitude")))[, "cell"],
      year = year(observation_date),
      pr_ab =  species_observed
    ) %>%
    group_by(species, cell_id, year) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(
      lon = xFromCell(env, cell_id),
      lat = yFromCell(env, cell_id)
    ) %>%
    select(species, lon, lat, year, pr_ab) %>%
    drop_na()
  
  return(ebd_zf_df_geo)
}