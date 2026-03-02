gams_check <- function(){

  ## from documentation k.check:
  ## Low p-values may indicate that the basis dimension, k, has been set too low,
  ## especially if the reported edf is close to k\', the maximum possible EDF for the term.
  ## Doubling a suspect k and re-fitting is sensible:
  ## if the reported edf increases substantially then you may have been missing something in the first fit. 
  ## Of course p-values can be low for reasons other than a too low k.
  ##
  ## But what if a variable is not relevant? 
  ## Should it then return a high p-value because there does not need to be a lot of detail?
  ## Or can it be a low p-value that we disregard. IS this "other reasons" as above?
  ##
  ## 1d with k=10: only a few that are efd close to 9 and low p value
  ## 1d with k=20: no low p-values and high edf
  ## 1D --> 10 is fine
  ## pca with k=10: more towards high edf, only few really pushing up to 9 and low p-value
  ## pca with k=20: most are kept under 9, not a lot with low p-value and high edf
  ## PCA --> 10 is (borderline) fine
  
  ## do k.checks for all models and make a graph representation.
  
  tar_load(gam_scen)
  k.checks <- lapply(gam_scen, function(gam){
    k.check(gam[["model"]]) %>%
      as_tibble()
  })
  ks_1d <- reduce(k.checks[1:((24*28))], rbind)
  plot(ks_1d$edf, ks_1d$`p-value`)
  ks_pca <- reduce(k.checks[((24*28)+1):length(k.checks)], rbind) 
  plot(ks_pca$edf, ks_pca$`p-value`)
  
  tar_load(gam_scen_k20)
  k20.checks <- lapply(gam_scen_k20, function(gam){
    k.check(gam[["model"]]) %>%
      as_tibble()
  })
  k20s_1d <- reduce(k20.checks[1:((24*28))], rbind)
  plot(k20s_1d$edf, k20s_1d$`p-value`)
  k20s_pca <- reduce(k20.checks[((24*28)+1):length(k.checks)], rbind) 
  plot(k20s_pca$edf, k20s_pca$`p-value`)
  
  
  # tar_load(gam_bird)
  # k.checks.birds <- lapply(gam_bird, function(gam){
  #   
  #   k.check(gam[["model"]]) %>%
  #     as_tibble()
  #   
  # })
  # 
  # #ks_bird <- reduce(k.checks.birds, rbind)
  # ks_bird <- lapply(1:length(bird_species), function(i){
  #   k <- k.checks.birds[[i]]
  #   o <- order[i,]
  #   out <- k %>% mutate(sp = o$bird_species,
  #                       model = o$model)
  # }) %>% reduce(rbind) %>%
  #   filter(sp %in% plot_bird_species)
  # plot(ks_bird$edf, ks_bird$`p-value`)
  # 
  # tar_load(gam_bird_k20)
  # k.checks.birdk20s <- lapply(gam_bird_k20, function(gam){
  #   
  #   k.check(gam[["model"]]) %>%
  #     as_tibble()
  #   
  # })
  # 
  # #k20s_bird <- reduce(k.checks.birdk20s, rbind)
  # k20s_bird <- lapply(1:length(bird_species), function(i){
  #   
  #   k <- k.checks.birdk20s[[i]]
  #   o <- order[i,]
  #   
  #   out <- k %>% mutate(sp = o$bird_species,
  #                       model = o$model)
  #   
  # }) %>% reduce(rbind) %>%
  #   filter(sp %in% plot_bird_species)
  # plot(k20s_bird$edf, k20s_bird$`p-value`)
  # 
  # tar_load(gam_bird_k40)
  # k.checks.birdk40s <- lapply(gam_bird_k40, function(gam){
  #   
  #   k.check(gam[["model"]]) %>%
  #     as_tibble()
  #   
  # })
  # 
  # #k40s_bird <- reduce(k.checks.birdk40s, rbind)
  # k40s_bird <- lapply(1:length(bird_species), function(i){
  #   
  #   k <- k.checks.birdk40s[[i]]
  #   o <- order[i,]
  #   
  #   out <- k %>% mutate(sp = o$bird_species,
  #                       model = o$model)
  #   
  # }) %>% reduce(rbind) %>%
  #   filter(sp %in% plot_bird_species)
  # plot(k40s_bird$edf, k40s_bird$`p-value`)
  # 
  
  tar_load(bird_johnston_species)
  tar_load(plot_bird_species)
  tar_load(case_model_grid)
  
  order_johnston <- expand_grid(
    bird_johnston_species,
    case_model_grid
  )
  
  tar_load(gam_bird_johnston_k20)
  k.checks.bird_johnstonk20s <- lapply(gam_bird_johnston_k20, function(gam){
    
    k.check(gam[["model"]]) %>%
      as_tibble()
    
  })
  
  k20s_bird_johnston <- lapply(1:length(bird_johnston_species), function(i){
    
    k <- k.checks.bird_johnstonk20s[[i]]
    o <- order_johnston[i,]
    
    out <- k %>% mutate(sp = o$bird_johnston_species,
                        model = o$model)
    
  }) %>% reduce(rbind) %>%
    filter(sp %in% plot_bird_species)
  plot(k20s_bird_johnston$edf, k20s_bird_johnston$`p-value`)
  
  
  tar_load(gam_bird_johnston_k20_ncv)
  k.checks.bird_johnstonk20s_ncv <- lapply(gam_bird_johnston_k20_ncv, function(gam){
    
    k.check(gam[["model"]]) %>%
      as_tibble()
    
  })
  
  k20s_bird_johnston_ncv <- lapply(1:length(bird_johnston_species), function(i){
    
    k <- k.checks.bird_johnstonk20s_ncv[[i]]
    o <- order_johnston[i,]
    
    out <- k %>% mutate(sp = o$bird_johnston_species,
                        model = o$model)
    
  }) %>% reduce(rbind) %>%
    filter(sp %in% plot_bird_species)
  plot(k20s_bird_johnston_ncv$edf, k20s_bird_johnston_ncv$`p-value`)
  
  tar_load(gam_bird_johnston_k10)
  k.checks.bird_johnstonk10s <- lapply(gam_bird_johnston_k10, function(gam){
    
    k.check(gam[["model"]]) %>%
      as_tibble()
    
  })
  
  k10s_bird_johnston <- lapply(1:length(bird_johnston_species), function(i){
    
    k <- k.checks.bird_johnstonk10s[[i]]
    o <- order_johnston[i,]
    
    out <- k %>% mutate(sp = o$bird_johnston_species,
                        model = o$model)
    
  }) %>% reduce(rbind) %>%
    filter(sp %in% plot_bird_species)
  plot(k10s_bird_johnston$edf, k10s_bird_johnston$`p-value`)
  
  
  
  tar_load(gam_scen_slope)
  
  kslope.checks <- lapply(gam_scen_slope, function(gam){
    k.check(gam[["model"]]) %>%
      as_tibble()
  })
  
  kslopes_1d <- reduce(kslope.checks[1:((24*28))], rbind)
  plot(kslopes_1d$edf,kslopes_1d$`p-value`)
  
  tar_load(gam_scen_slope_k20)
  
  k20slope.checks <- lapply(gam_scen_slope_k20, function(gam){
    k.check(gam[["model"]]) %>%
      as_tibble()
  })
  
  k20slopes_1d <- reduce(k20slope.checks[1:((24*28))], rbind)
  plot(k20slopes_1d$edf,k20slopes_1d$`p-value`)
  
  
  tar_load(gam_scen_skew)
  
  kskew.checks <- lapply(gam_scen_skew, function(gam){
    k.check(gam[["model"]]) %>%
      as_tibble()
  })
  
  kskews_1d <- reduce(kskew.checks[1:((24*28))], rbind)
  plot(kskews_1d$edf,kskews_1d$`p-value`)
  
  data <- rbind(ks_1d %>% mutate(k = 10, case = "One-dimensional virtual species", sp = NA, model = NA),
        k20s_1d %>% mutate(k = 20, case = "One-dimensional virtual species", sp = NA, model = NA),
        ks_pca %>% mutate(k = 10, case = "PCA virtual species", sp = NA, model = NA),
        k20s_pca %>% mutate(k = 20, case = "PCA virtual species", sp = NA, model = NA),
        kslopes_1d %>% mutate(k = 10, case = "Gradual slope one-dimensional virtual species", sp = NA, model = NA),
        k20slopes_1d %>% mutate(k = 20, case = "Gradual slope one-dimensional virtual species", sp = NA, model = NA),
        k10s_bird_johnston %>% mutate(k = 10, case = "Bird species"),
        k20s_bird_johnston %>% mutate(k = 20, case = "Bird species")#,
        # k40s_bird %>% mutate(k = 40, case = "Bird species")
        ) %>%
    mutate(case = ordered(case, levels = c("One-dimensional virtual species", "PCA virtual species",
                                           "Gradual slope one-dimensional virtual species", "Bird species")))
  
  # ## Testing ncv
  # data <- rbind(k20s_bird_johnston %>% mutate(k = 20, case = "Bird species"),#,,
  #               k20s_bird_johnston_ncv %>% mutate(k = "20_ncv", case = "Bird species")#,
  #               # k40s_bird %>% mutate(k = 40, case = "Bird species")
  # ) %>%
  #   mutate(case = ordered(case, levels = c("One-dimensional virtual species", "PCA virtual species",
  #                                          "Gradual slope one-dimensional virtual species", "Bird species")))
  # 
  ggplot(data) +
    geom_point(aes(x = edf, y = `p-value`, col = as.factor(k)), alpha = 0.5) +
    labs(col = "Number of basis\nfunctions k") +
    facet_wrap(~case, ncol = 1) +
    lims(x = c(0,20))
  
  output(data)
}


## No real difference between k-check metrics for birds between normal or ncv metric used in gam --> that is not the cause .
# ns <- tar_read(bird_extracted) %>% purrr::reduce(rbind) %>% group_by(species) %>% summarise(n = n())
# 
# k.checks.normal10 <- lapply(tar_read(gam_bird) , function(gam){
# 
# 
#   k.check(gam[["model"]]) %>%
#     as_tibble() %>% mutate(n = nrow(gam$data_ens))
# 
# 
# })
# 
# a <- lapply(1:length(gam_scen), function(i){
# 
#   tibble(og_pred = gam_scen[[1]]$data_ens$pred, og_pr_ab = gam_scen[[1]]$data_ens$pr_ab,
#          new_pred = gam_scen_k20[[1]]$data_ens$pred, new_pr_ab = gam_scen_k20[[1]]$data_ens$pr_ab)
# }) %>% purrr::reduce(rbind)
# 
# a <- lapply(1:length(gam_scen), function(i){
# 
#   tibble(og_pred = gam_scen[[1]]$performance$AUC_mean,
#          new_pred = gam_scen_k20[[1]]$performance$AUC_mean)
# }) %>% purrr::reduce(rbind)
# 
# ggplot(a, aes(x = og_pred, y = new_pred, col = og_pred)) +geom_point()
# 
# k.checks.ncv10 <- lapply(tar_read(gam_bird_k10_ncv), function(gam){
# 
#   k.check(gam[["model"]]) %>%
#     as_tibble()  %>% mutate(n = nrow(gam$data_ens))
# 
# })
# # 
# data <- rbind(k.checks.normal10  %>% purrr::reduce(rbind) %>% mutate(type = "normal"),
#       k.checks.ncv10 %>% purrr::reduce(rbind)  %>% mutate(type = "ncv"))
# 
# 
# ggplot(data , aes(x = edf, y = `p-value`, col = type)) +geom_point() +facet_wrap(~n)
