library(targets)
library(tarchetypes)

tar_source()

tar_plan(

  #### Data ####

  ## Define British Isles and Europe up to 35 degrees east as study area
  eu_poly_wkt = "POLYGON ((25.280527 74.385866, 34 74.516506, 34 35.773364, 32.861584 35.588421, 26.375071 34.609982, 22.816449 34.504241, 13.627633 36.690862, 10.001544 38.719154, -0.356862 36.404548, -9.323257 35.627079, -12.292937 57.408982, 25.280527 74.385866))",
  eu_poly_crs = "epsg:4326",
  eu_poly = wrap(vect(eu_poly_wkt, crs = eu_poly_crs)),

  ## Get monthly Worldclim for Europe and tmin, tmax, prec
  ## If needs to be rerun, delete Worldclim output files
  monthly_wc = worldclim_monthly(unwrap(eu_poly)),

  ## For which periods to calculate bioclimatic variables?
  history_length = 20, # consistent with Worldclim 20-year spans for future projections

  ## Calculate bioclimatic variables for each history length period possible in data
  tar_target(
    history_bioclim,
    calc_bioclim(monthly_wc, history_length)
  ),

  ## Climate for future projection SSP370 in 2041-2060, file donwloaded seperately
  tar_target(
    moderate_future_bioclim,
    {
      writeRaster(
        crop(rast(grep("IPSL-CM6A-LR",
          grep("370_2041-2060", list.files("./data/worldclim/", full.names = T), value = T),
          value = T
        )), rast(history_bioclim[1])) %>%
          setNames(paste0("bio", 1:19)),
        "./output/future/SSP370-2060.tif",
        overwrite = T
      )
      return("./output/future/SSP370-2060.tif")
    }
  ),

  #### Virtual Species ####

  # Year for which to generate virtual species
  virt_year = 1979,
  ## Years to simulate
  years = 1979:2021,
  ## gir dof mean and sd for virtual species Gaussian response to bio1
  grid_1d = expand.grid(
    mean_bio1 = seq(0, 14, 2),
    sd_bio1 = c(0.75, 1, 2)
  ),

  ## Generate one dimensional virtual species using virtualspecies package
  tar_target(random_sp_1d,
    generateSpFromFun(
      raster.stack = subset(
        rast(grep(virt_year, history_bioclim, value = T)),
        "bio1"
      ),
      parameters = formatFunctions(bio1 = c(
        fun = "dnorm",
        mean = grid_1d$mean_bio1,
        sd = grid_1d$sd_bio1
      ))
    ) %>%
      convertToPA(
        PA.method = "probability",
        alpha = -0.05,
        beta = 0.5
      ),
    pattern = map(grid_1d),
    iteration = "list",
    garbage_collection = TRUE
  ),

  ## Define amount of PCA species to generate
  pca_reps = 1:10,
  ## Generate PCA virtual species based on 6 climate variables using virtualspecies package
  tar_target(random_sp_pca,
    generateRandomSp(
      raster.stack = subset(
        rast(grep(virt_year, history_bioclim, value = T)),
        paste0("bio", c(1, 5, 6, 12, 13, 14))
      ), # 3 temp, 3 prec vairables
      approach = "pca",
      convert.to.PA = TRUE,
      PA.method = "probability",
      niche.breadth = "narrow",
      alpha = -0.05,
      beta = 0.5
    ),
    pattern = map(pca_reps),
    iteration = "list",
    garbage_collection = TRUE
  ),

  ## Bundle 1d and pca virtual species
  tar_target(random_sp,
    append(random_sp_1d, random_sp_pca),
    iteration = "list"
  ),

  ## Generate suitability and probability of occurrence for each year based on simulated niche
  tar_target(random_sp_PA_years,
    sp_to_pa(random_sp,
      envs = history_bioclim,
      years
    ),
    pattern = map(random_sp),
    garbage_collection = T,
    iteration = "list"
  ),

  ## For which years to generate presences and absences
  occ_years = 1979:2020,
  ## Desired number of total presences and absences
  n_target = 1000,
  ## How many observations per year to generate
  n_per_year = round(n_target / length(occ_years)),
  tar_target(yearly_n,
    list(
      rep(n_per_year, length(occ_years))
    ),
    iteration = "list"
  ),

  ## Four species migration behaviour scenarios
  scens = c("both_track", "lead_track", "static", "trail_track"),
  ## Generate presences and absences based on certain species migration behaviour scenarios and yearly pa rasters
  tar_target(scen_occ,
    get_scen_presence_absence(
      random_sp_PA_years,
      ifelse(length(random_sp_PA_years[[1]]$details$variables) > 1,
        list(yearly_n * 5), ## 5 times more observations for pca species
        list(yearly_n)
      )[[1]],
      occ_years,
      scens
    ),
    pattern = cross(random_sp_PA_years, yearly_n, scens), ## yearly_n is only one option
    iteration = "list",
    garbage_collection = TRUE
  ),

  ## Generate random cross validation folds
  tar_target(scen_kfold,
    part_random(
      data = scen_occ,
      pr_ab = "pr_ab",
      method = c(method = "kfold", folds = 5)
    ),
    pattern = map(scen_occ),
    iteration = "list"
  ),

  ## Which models to calculate and which environment they correspond to
  model_grid = tibble(
    model = c(
      "first", # 1979 model
      "early", # 2000 model all observations
      "early_later", # 2000 model only use observations from 1981 onwards
      "early_matched", # 2000 model only use observations from within period 1981-2000
      "late", # 2020 model all observations
      "late_matched", # 2020 model only use observations from within period 2001-2020
      "sliding" ## Matched model
    ),
    model_year = c(
      1979,
      2000,
      2000,
      2000,
      2020,
      2020,
      NA
    )
  ),

  ## Which variables to extract from environment raster
  tar_target(vars_incl,
    random_sp_PA_years[[1]]$details$variables,
    cross(random_sp_PA_years, yearly_n, scens, model_grid),
    iteration = "list"
  ),

  ## Extract climate variables for each observation according to model approach
  tar_target(scen_extracted,
    extract_gbif(scen_kfold, ## Gbif is only remnant of earlier analysis, function name was not changed
      envs = history_bioclim,
      model = model_grid$model,
      model_year = model_grid$model_year,
      vars = vars_incl
    ),
    pattern = map(cross(scen_kfold, model_grid), vars_incl),
    iteration = "list"
  ),

  ## GAM model
  tar_target(gam_scen,
    fit_gam(
      data = scen_extracted %>% filter(year <= 2020), ## only use data from 2020 or before
      response = "pr_ab",
      predictors = vars_incl,
      partition = ".part",
      thr = "max_sens_spec",
      fit_formula = NULL,
      k = -1
    ),
    pattern = map(vars_incl, scen_extracted),
    iteration = "list"
  ),

  ## Summarise results for 1d species
  tar_target(scen_info_1d,
    get_info_scen_1d(gam_scen,
      random_sp_PA_years,
      envs = history_bioclim,
      years,
      predict_years = 2021, ## project to 2021 environment
      yearly_n = yearly_n,
      scen = scens,
      model = model_grid$model
    ),
    pattern = map(
      slice(gam_scen, index = 1:(24 * 28)), ## only for 1d species
      cross(
        map(slice(random_sp_PA_years, index = 1:24)), ## only for 1d species
        yearly_n, scens, model_grid
      )
    )
  ),

  ## Make summarising figure for one-dimensional virtual species
  plot_1d_data = make_plot_1d(scen_info_1d),

  ## Which species are pca?
  tar_target(sp_i_all,
    list(25, 26, 27, 28, 29, 30, 31, 32, 33, 34),
    iteration = "list"
  ),

  ## Summary for PCA virtual species
  tar_target(scen_info_2d_summary,
    get_info_scen_2d_summary(gam_scen,
      random_sp,
      random_sp_PA_years,
      envs = history_bioclim,
      years,
      predict_years = 2021,
      cc_years = c(1979, 2020, 2021),
      yearly_n = yearly_n,
      scen = scens,
      model = model_grid$model,
      sp_i = sp_i_all
    ),
    pattern = map(
      slice(gam_scen, index = (((25 - 1) * 28) + 1):(34 * 28)),
      cross(
        map(
          slice(random_sp, index = 25:34), # 34
          slice(random_sp_PA_years, index = 25:34),
          sp_i_all
        ),
        yearly_n, scens, model_grid
      )
    ),
    garbage_collection = T
  ),

  ## all species
  sp_i_both = 1:34,

  ## Performance summmary packaging for plot in paper
  tar_target(scen_performance,
    if (!is.null(random_sp$details$parameters$bio1$args["mean"])) {
      tibble(gam_scen$performance,
        yearly_n = ifelse(length(unique(yearly_n)) == 1, "uniform", "realistic"),
        scen = scens,
        model_type = model_grid$model,
        mean_bio1 = random_sp$details$parameters$bio1$args["mean"],
        sd_bio1 = random_sp$details$parameters$bio1$args["sd"],
        sp_i = sp_i_both
      )
    } else {
      tibble(gam_scen$performance,
        yearly_n = ifelse(length(unique(yearly_n)) == 1, "uniform", "realistic"),
        scen = scens,
        model_type = model_grid$model,
        mean_pca1 = random_sp$details$means[1],
        mean_pca2 = random_sp$details$means[2],
        sd_pca1 = random_sp$details$sds[1],
        sd_pca2 = random_sp$details$sds[2],
        sp_i = sp_i_both
      )
    },
    pattern = map(
      gam_scen,
      cross(
        map(random_sp, sp_i_both),
        yearly_n, scens, model_grid
      )
    )
  ),

  #### Appendix ####

  ### Same analysis as 1d species above but 20 basis functions
  ## GAM model
  tar_target(gam_scen_k20,
    fit_gam(
      data = scen_extracted %>% filter(year <= 2020), ## only use data from 2020 or before
      response = "pr_ab",
      predictors = vars_incl,
      partition = ".part",
      thr = "max_sens_spec",
      fit_formula = NULL,
      k = 20
    ),
    pattern = map(vars_incl, scen_extracted),
    iteration = "list"
  ),


  ### Same analysis as 1d species above but observations are skewed in time, recency bias, only 1d for brevity ###
  ## generate how many observations per year
  tar_target(yearly_n_skew,
    list(
      round(93 / 1.1^(length(years):1 - 1)) ## exponential distribution of observations summing to about 1000
    ),
    iteration = "list"
  ),
  ## get presences and absences based on certain species migration behavrious scenario
  tar_target(scen_occ_skew,
    get_scen_presence_absence(
      random_sp_PA_years,
      ifelse(length(random_sp_PA_years[[1]]$details$variables) > 1,
        list(yearly_n_skew * 5),
        list(yearly_n_skew)
      )[[1]],
      occ_years,
      scens
    ),
    pattern = cross(slice(random_sp_PA_years, index = 1:24), yearly_n_skew, scens),
    iteration = "list",
    garbage_collection = TRUE
  ),

  ## Number of clusters for cross-valdation
  tar_target(scen_kfold_skew,
    part_random(
      data = scen_occ_skew,
      pr_ab = "pr_ab",
      method = c(method = "kfold", folds = 5)
    ),
    pattern = map(scen_occ_skew),
    iteration = "list"
  ),

  ## Extract climate variables for each obsevation according to model approach
  tar_target(scen_extracted_skew,
    extract_gbif(scen_kfold_skew,
      envs = history_bioclim,
      model = model_grid$model,
      model_year = model_grid$model_year,
      vars = vars_incl
    ),
    pattern = map(cross(scen_kfold_skew, model_grid), slice(vars_incl, index = 1:(24 * 28))),
    iteration = "list"
  ),

  ## GAM model
  tar_target(gam_scen_skew,
    fit_gam(
      data = scen_extracted_skew %>% filter(year <= 2020),
      response = "pr_ab",
      predictors = vars_incl,
      partition = ".part",
      thr = "max_sens_spec",
      fit_formula = NULL,
      k = -1
    ),
    pattern = map(slice(vars_incl, index = 1:(24 * 28)), scen_extracted_skew),
    iteration = "list"
  ),
  ## Summarise results for 1d species
  tar_target(scen_info_1d_skew,
    get_info_scen_1d(gam_scen_skew,
      random_sp_PA_years,
      envs = history_bioclim,
      years,
      predict_years = 2021,
      yearly_n = yearly_n_skew,
      scen = scens,
      model = model_grid$model
    ),
    pattern = map(
      gam_scen_skew,
      cross(
        map(slice(random_sp_PA_years, index = 1:24)),
        yearly_n_skew, scens, model_grid
      )
    )
  ),
  ## Make summarising figure for one-dimensional virtual species
  plot_1d_data_skew = make_plot_1d(scen_info_1d_skew),
  sp_i_perf_skew = 1:24,

  ## Performance summmary packaging for plot in paper
  tar_target(scen_performance_skew,
    if (!is.null(random_sp$details$parameters$bio1$args["mean"])) {
      tibble(gam_scen_skew$performance,
        yearly_n = ifelse(length(unique(yearly_n_skew)) == 1, "uniform", "realistic"),
        scen = scens,
        model_type = model_grid$model,
        mean_bio1 = random_sp$details$parameters$bio1$args["mean"],
        sd_bio1 = random_sp$details$parameters$bio1$args["sd"],
        sp_i = sp_i_perf_skew
      )
    } else {
      tibble(gam_scen_skew$performance,
        yearly_n = ifelse(length(unique(yearly_n_skew)) == 1, "uniform", "realistic"),
        scen = scens,
        model_type = model_grid$model,
        mean_pca1 = random_sp$details$means[1],
        mean_pca2 = random_sp$details$means[2],
        sd_pca1 = random_sp$details$sds[1],
        sd_pca2 = random_sp$details$sds[2],
        sp_i = sp_i_perf_skew
      )
    },
    pattern = map(
      gam_scen_skew,
      cross(
        map(slice(random_sp, index = 1:24), sp_i_perf_skew),
        yearly_n_skew, scens, model_grid
      )
    )
  ),

  ### Sensitivity analysis for logistic function slope, same analysis as 1d above ###
  ## Generate one dimensional virtual species
  tar_target(random_sp_1d_slope,
    generateSpFromFun(
      raster.stack = subset(
        rast(grep(virt_year, history_bioclim, value = T)),
        "bio1"
      ),
      parameters = formatFunctions(bio1 = c(
        fun = "dnorm",
        mean = grid_1d$mean_bio1,
        sd = grid_1d$sd_bio1
      ))
    ) %>%
      convertToPA(
        PA.method = "probability",
        alpha = -0.5,
        beta = 0.5
      ),
    pattern = map(grid_1d),
    iteration = "list",
    garbage_collection = TRUE
  ),

  ## Generate suitability and probability of occurrence for each year based on simulated niche
  tar_target(random_sp_PA_years_slope,
    sp_to_pa(random_sp_1d_slope,
      envs = history_bioclim,
      years
    ),
    pattern = map(random_sp_1d_slope),
    garbage_collection = T,
    iteration = "list"
  ),

  ## get presences and absences based on certain species migration behaviour scenario
  tar_target(scen_occ_slope,
    get_scen_presence_absence(
      random_sp_PA_years_slope,
      ifelse(length(random_sp_PA_years_slope[[1]]$details$variables) > 1,
        list(yearly_n * 5),
        list(yearly_n)
      )[[1]],
      occ_years,
      scens
    ),
    pattern = cross(random_sp_PA_years_slope, yearly_n, scens),
    iteration = "list",
    garbage_collection = TRUE
  ),

  ## Number of clusters for cross-valdation
  tar_target(scen_kfold_slope,
    part_random(
      data = scen_occ_slope,
      pr_ab = "pr_ab",
      method = c(method = "kfold", folds = 5)
    ),
    pattern = map(scen_occ_slope),
    iteration = "list"
  ),

  ## Extract climate variables for each observation according to model approach
  tar_target(scen_extracted_slope,
    extract_gbif(scen_kfold_slope,
      envs = history_bioclim,
      model = model_grid$model,
      model_year = model_grid$model_year,
      vars = vars_incl
    ),
    pattern = map(cross(scen_kfold_slope, model_grid), slice(vars_incl, index = 1:(24 * 28))),
    iteration = "list"
  ),

  ## GAM model
  tar_target(gam_scen_slope,
    fit_gam(
      data = scen_extracted_slope %>% filter(year <= 2020),
      response = "pr_ab",
      predictors = vars_incl,
      partition = ".part",
      thr = "max_sens_spec",
      fit_formula = NULL,
      k = -1
    ),
    pattern = map(slice(vars_incl, index = 1:(24 * 28)), scen_extracted_slope),
    iteration = "list"
  ),

  ## GAM model
  tar_target(gam_scen_slope_k20,
    fit_gam(
      data = scen_extracted_slope %>% filter(year <= 2020),
      response = "pr_ab",
      predictors = vars_incl,
      partition = ".part",
      thr = "max_sens_spec",
      fit_formula = NULL,
      k = 20
    ),
    pattern = map(slice(vars_incl, index = 1:(24 * 28)), scen_extracted_slope),
    iteration = "list"
  ),

  ## Summarise results for 1d species
  tar_target(scen_info_1d_slope,
    get_info_scen_1d(gam_scen_slope_k20,
      random_sp_PA_years_slope,
      envs = history_bioclim,
      years,
      predict_years = 2021,
      yearly_n = yearly_n,
      scen = scens,
      model = model_grid$model
    ),
    pattern = map(
      gam_scen_slope_k20,
      cross(
        map(slice(random_sp_PA_years_slope, index = 1:24)),
        yearly_n, scens, model_grid
      )
    )
  ),
  ## Make summarising figure for one-dimensional virtual species
  plot_1d_data_slope = make_plot_1d(scen_info_1d_slope),
  sp_i_perf_slope = 1:24,

  ## Performance summmary packaging for plot in paper
  tar_target(scen_performance_slope,
    tibble(gam_scen_slope_k20$performance,
      yearly_n = ifelse(length(unique(yearly_n)) == 1, "uniform", "realistic"),
      scen = scens,
      model_type = model_grid$model,
      mean_bio1 = random_sp_1d_slope$details$parameters$bio1$args["mean"],
      sd_bio1 = random_sp_1d_slope$details$parameters$bio1$args["sd"],
      sp_i = sp_i_perf_slope
    ),
    pattern = map(
      gam_scen_slope_k20,
      cross(
        map(slice(random_sp_1d_slope, index = 1:24), sp_i_perf_slope),
        yearly_n, scens, model_grid
      )
    )
  ),



  #### Illustrative example using bird species ####

  case_model_grid = tibble(
    model = c(
      "first",
      "sliding"
    ),
    model_year = c(
      1979,
      NA
    )
  ),

  ## Use all 19 bioclimatic variables
  case_vars = paste0("bio", 1:19),

  ## eBird data files
  tar_target(ebd_files,
    list.files("./data/ebird/ebd/",
      pattern = "*2024.txt$",
      recursive = T, full.names = T
    ),
    ## force = TRUE, ## If files change, update the tar_target to tar_force and uncomment this line
    iteration = "list"
  ),

  ## Convert eBird data files to zero-filled occurrences
  tar_target(bird_occ,
    get_ebird(ebd_files,
      envs = history_bioclim,
      months_incl = c("*-04-01", "*-09-30") ## Only half of year
    ),
    pattern = map(ebd_files),
    iteration = "list"
  ),

  ## Which bird species to plot
  tar_target(plot_bird_species,
    c(
      "Cettia cetti",
      "Carduelis carduelis",
      "Passer montanus",
      "Curruca communis",
      "Merops apiaster",
      "Turdus iliacus"
    ),
    iteration = "vector"
  ),

  ## Follow guidelines from Johnston et al to process eBird data
  tar_target(bird_johnston_occ,
    get_johnston_ebird(ebd_files,
      envs = history_bioclim,
      months_incl = c("*-04-01", "*-09-30") ## Only half of year
    ),
    pattern = slice(ebd_files, index = c(5, 8, 10, 12, 7, 17)), # ,8,10,12,7,17)),
    iteration = "list"
  ),

  ## which bird species in data
  tar_target(bird_johnston_species,
    bird_johnston_occ$species[1],
    pattern = map(bird_johnston_occ),
    iteration = "vector"
  ),

  ## Balance bird occurrences per year
  tar_target(bird_johnston_balanced,
    balance(bird_johnston_occ),
    pattern = map(bird_johnston_occ),
    iteration = "list"
  ),

  ## spatial block cross-validation folds using cv_spatial
  tar_target(bird_johnston_sblock,
    bird_johnston_balanced %>%
      filter(year %in% 1979:2021) %>% ## Restrict to years in environmental data
      mutate(.part = cv_spatial(
        x = vect(.,
          geom = c("lon", "lat"),
          crs = "EPSG:4326"
        ),
        column = "pr_ab",
        k = 5,
        rows_cols = 5
      )$folds_ids),
    pattern = map(bird_johnston_balanced),
    iteration = "list"
  ),

  ## Extract relevant variables according to model approach and year
  tar_target(bird_johnston_extracted,
    case_extract_gbif(bird_johnston_sblock,
      envs = history_bioclim,
      model = case_model_grid$model,
      model_year = case_model_grid$model_year,
      vars = case_vars
    ),
    pattern = cross(bird_johnston_sblock, case_model_grid),
    iteration = "list"
  ),



  ## GAM model
  tar_target(gam_bird_johnston_k20,
    fit_gam(
      data = bird_johnston_extracted,
      response = "pr_ab",
      predictors = case_vars,
      partition = ".part",
      thr = "max_sens_spec",
      fit_formula = NULL,
      k = 20
    ),
    pattern = map(bird_johnston_extracted),
    iteration = "list",
    garbage_collection = T,
    deployment = "worker"
  ),
  tar_target(gam_bird_johnston_k10,
    fit_gam(
      data = bird_johnston_extracted,
      response = "pr_ab",
      predictors = case_vars,
      partition = ".part",
      thr = "max_sens_spec",
      fit_formula = NULL,
      k = 10
    ),
    pattern = map(bird_johnston_extracted),
    iteration = "list",
    garbage_collection = T,
    deployment = "worker"
  ),

  ## Summarise bird model gam performance
  tar_target(bird_johnston_performance_k20,
    tibble(gam_bird_johnston_k20$performance,
      aic = gam_bird_johnston_k20$model$aic,
      model_type = case_model_grid$model,
      species = bird_johnston_species
    ),
    pattern = map(
      gam_bird_johnston_k20,
      cross(
        bird_johnston_species,
        case_model_grid
      )
    )
  ),


  ## Make geographic projection plot using biscale for 2015 with 1979 and matched models
  tar_target(
    each_bird_johnston_geo_bi_2015_k20_plot,
    make_case_geo_bi_period_plot(
      case_gams = gam_bird_johnston_k20,
      env_files = history_bioclim,
      years = 2015,
      crs = "EPSG:3035",
      species = bird_johnston_species,
      plot_species = plot_bird_species,
      predict_models = c("first", "sliding"),
      palette = "BlueGold",
      models = case_model_grid$model
    ),
    pattern = map(plot_bird_species)
  ),


  ## Make geographic projection plot using biscale for 2060 with 1979 and matched models
  tar_target(
    each_bird_johnston_geo_bi_2060_k20_plot,
    make_case_geo_bi_period_plot(
      case_gams = gam_bird_johnston_k20,
      env_files = moderate_future_bioclim,
      years = 2060,
      crs = "EPSG:3035",
      species = bird_johnston_species,
      plot_species = plot_bird_species,
      predict_models = c("first", "sliding"),
      palette = "BlueGold",
      models = case_model_grid$model
    ),
    pattern = map(plot_bird_species)
  ),
)
