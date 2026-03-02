sp_to_pa <- function(random_sp, ## virtual species
                     envs, # environment files
                     years, # Years in which 20-year periods end
                     alpha = NULL,
                     beta = NULL){

  ## Presence-absence raster based on environment of for each year separately
  pas <- lapply(years, function(year){

    env <- grep(year, envs, value = T)

    ## Different code needed for 1d and pca species
    if(!is.null(random_sp$details$parameters)){

      random_sp_year <- generateSpFromFun(raster.stack = subset(rast(env),random_sp$details$variables),
                                          plot = F,
                                          parameters = random_sp$details$parameters,
                                          rescale = random_sp$details$rescale,
                                          rescale.each.response =  random_sp$details$rescale.each.response,
                                          formula = random_sp$details$formula)

    } else {

      random_sp_year <- generateSpFromPCA(raster.stack = subset(rast(env),random_sp$details$variables),
                                          plot = F,
                                          pca = random_sp$details$pca,
                                          means = random_sp$details$means,
                                          sds = random_sp$details$sds)

    }

    ## generate PA raster
    random_sp_PA_year <- convertToPA(random_sp_year,
                                     PA.method = random_sp$PA.conversion["conversion.method"],
                                     prob.method = random_sp$PA.conversion["probabilistic.method"],
                                     alpha = ifelse(is.null(alpha),as.numeric(random_sp$PA.conversion["alpha"]), alpha),
                                     beta = ifelse(is.null(beta),as.numeric(random_sp$PA.conversion["beta"]), beta),
                                     plot = F)

    random_sp_PA_year$details$pca <- NULL
    gc()

    return(random_sp_PA_year)

  })

  return(pas)

}
