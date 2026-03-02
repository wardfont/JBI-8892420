centroid_vs_range <- function(){
  
  ## For supporting information: 
  
  tar_load(bird_occ)
  tar_load(history_bioclim)
  tar_load(bird_species)
  tar_load(plot_bird_species)
  
  ## Establish background raster layer for in plots
  env <- rast(history_bioclim)[[1]] %>% project("EPSG:3035") %>%
    mutate(land = !is.na(bio1)) %>% select(land)
  
  ## Which indices are the birds species of focus
  is <- which(bird_species %in% c(plot_bird_species))
  end_years <- seq(1984, 2024, 10)
  
  ## For each bird speices of focus, caluclate centroid and 95 percentile convex hull
  plots <- lapply(bird_occ[is], function(b_o){
    
    ## Convert to vector
    b_v <- b_o %>% filter(pr_ab == 1) %>%  vect(geom = c("lon", "lat"), crs = "EPSG:4326") %>% project("EPSG:3035")

    ## Calculate centroids for periods
    centroids <- lapply(end_years, function(e_y){
      
      if(e_y != 1984){
        data <- b_v %>% 
          filter(year %in% (e_y-9):e_y) %>%
          mutate(year = paste0((e_y-9),"-", e_y))
      } else {
        data <- b_v %>% 
          filter(year <= e_y) %>%
          mutate(year = "1979-1984")
      }
      
      
      if(length(data) > 0){
        centroid <- data %>%
          as_tibble(geom = "XY") %>%
          summarise(x = mean(x),
                    y = mean(y),
                    n = n()) %>%
          mutate(year =  data$year[1])
      } else {
        centroid <- tibble()
      }
      
    }) %>% reduce(rbind) %>% vect(geom = c("x", "y"), crs= "EPSG:3035")
    
    ## Calculate 95 percentile convex hull for periods
    hulls <- lapply(end_years, function(e_y){
      
      if(e_y != 1984){
        data <- b_v %>% 
          filter(year %in% (e_y-9):e_y) %>%
          mutate(year = paste0((e_y-9),"-", e_y))
      } else {
        data <- b_v %>% 
          filter(year <= e_y) %>%
          mutate(year = "1979-1984")
      }
      
      if(length(data) > 2){
        # range_envelope <- convHull(data)
        
        range_envelope <- sf::st_as_sf(data) %>%
          summarize(.groups = "keep") %>% 
          mutate(cent = sf::st_centroid(geometry)) %>%
          sf::st_cast(to = "POINT") %>%  
          mutate(dist = sf::st_distance(geometry, cent, by_element = TRUE)) %>% 
          filter(dist <= quantile(dist, .95)) %>% 
          summarize(.groups = "keep")  %>%
          sf::st_convex_hull() %>%
          st_sf() %>%
          vect()
        
        range_envelope$year <- data$year[1]
        
      } else {
        range_envelope <- NULL
      }
      
      return(range_envelope)
    }) 
    ## Gather hulls in one vector
    hull <- vect(compact(hulls))
    
    ## Plot centroids and hull for each bird species
    ggplot() +
      geom_spatraster(data = env) +
      scale_fill_manual(values = c(NA, "grey75"), na.value = NA) +
      geom_spatvector(data = centroids, aes(col = as.factor(year)), size = 2) +
      geom_spatvector(data = hull, aes(col = as.factor(year)), fill = NA, lwd = 0.75) +
      scale_colour_viridis_d() +
      labs(title = b_o$species[1], colour = "Timeframe") +
      guides(fill="none")
    
  })
  
  ## Gather plots
  ggarrange(plotlist = plots[c(3,5,1,2,4,6)], common.legend = T, ncol = 2, nrow = 3)+ theme_pubr()
}