get_info_scen_1d <- function(model_scen, ## model for scenarios
                             random_sp_PA_years, ## year pa raster
                             envs, ## environment files
                             years,
                             predict_years,
                             yearly_n,
                             scen,
                             model) {
  
  ## This function processes the model results in a form that can be used to make the plot in the paper
  ## It calculated the correct and estimated probability of occurrence for bio1 values normalised using mean and sd of gaussian response of virutal species
  if (!is.null(model_scen) & !is.null(model_scen$model) & (length(random_sp_PA_years[[1]]$details$variables) == 1)) {

    ns <- lapply(predict_years, function(pr_yr) {
      i <- which(years == pr_yr)

      real <- unwrap(random_sp_PA_years[[i]]$probability.of.occurrence)
      names(real) <- "real"

      env_rast <- rast(grep(pr_yr, envs, value = T))

      sdm <- sdm_predict(
        models = list(model_scen),
        pred = env_rast,
        clamp = T,
        pred_type = "cloglog"
      )[[1]]
      names(sdm) <- c("sdm")

      as_tibble(c(
        env_rast[["bio1"]],
        real,
        sdm[[1]]
      )) %>%
        drop_na() %>%
        mutate(year = pr_yr)
    })

    data <- do.call(rbind, ns) %>%
      mutate(
        yearly_n = ifelse(length(unique(yearly_n)) == 1, "uniform", "realistic"),
        scen = scen,
        model = model,
        mean_bio1 = random_sp_PA_years[[1]]$details$parameters$bio1$args["mean"],
        sd_bio1 = random_sp_PA_years[[1]]$details$parameters$bio1$args["sd"],
        type = "bio1"
      )
  } else {
    data <- tibble(
      cellID = double(), real = double(), sdm = double(),
      yearly_n = character(),
      scen = character(), model = character(),
      bio1 = double(),
      mean_bio1 = double(),
      sd_bio1 = double()
    )
  }

  data_round <- data %>%
    mutate(
      bio1_scaled = (bio1 - mean_bio1) / sd_bio1,
      bio1_round = round(bio1_scaled, digits = 1)
    ) %>%
    group_by(bio1_round, yearly_n, scen, model, mean_bio1, sd_bio1) %>%
    summarise(
      sdm_mean = mean(sdm, na.rm = T),
      real_mean = mean(real, na.rm = T)
    ) %>%
    drop_na()

  return(data_round)
}

## PCA equivalent of function above but for two dimensions: selects two PCA axis and also calculates direction of climate change in those two axis.
get_info_scen_2d_summary <- function(model_scen,
                                     random_sp,
                                     random_sp_PA_years,
                                     envs,
                                     years,
                                     predict_years,
                                     cc_years,
                                     yearly_n,
                                     scen,
                                     model,
                                     sp_i) {
  if (!is.null(model_scen) & !is.null(model_scen$model) & (length(random_sp_PA_years[[1]]$details$variables) > 1)) {
    pca <- random_sp$details$pca
    yr_pca_rasts <- lapply(cc_years, function(yr) {
      yr_env_rast <- rast(grep(yr, envs, value = T))

      yr_pca_axes <- as_tibble(suprow(
        pca,
        as_tibble(as.data.frame(yr_env_rast, na.rm = F)) %>%
          select(all_of(random_sp_PA_years[[1]]$details$variables))
      )$lisup)
      yr_pca1_rast <- yr_env_rast[[1]]
      yr_pca2_rast <- yr_env_rast[[1]]
      values(yr_pca1_rast) <- (yr_pca_axes[, 1] - random_sp$details$means[1]) / random_sp$details$sds[1]
      values(yr_pca2_rast) <- (yr_pca_axes[, 2] - random_sp$details$means[2]) / random_sp$details$sds[2]

      yr_pca_rast <- c(yr_pca1_rast, yr_pca2_rast)
      names(yr_pca_rast) <- paste0(yr, "_pca", 1:2)

      return(yr_pca_rast)
    })

    pca_rast <- rast(yr_pca_rasts)

    ns <- lapply(predict_years, function(pr_yr) {
      i <- which(years == pr_yr)

      real_first <- unwrap(random_sp_PA_years[[1]]$probability.of.occurrence)
      names(real_first) <- "real_first"

      real <- unwrap(random_sp_PA_years[[i]]$probability.of.occurrence)
      names(real) <- "real"

      env_rast <- rast(grep(pr_yr, envs, value = T))

      sdm <- sdm_predict(
        models = list(model_scen),
        pred = env_rast,
        clamp = T,
        pred_type = "cloglog"
      )[[1]]
      names(sdm) <- c("sdm")

      all <- c(
        real_first, real, sdm[[1]],
        pca_rast
      )

      as_tibble(all) %>%
        drop_na() %>%
        mutate(year = pr_yr)
    })

    data <- do.call(rbind, ns) %>%
      mutate(
        yearly_n = ifelse(length(unique(yearly_n)) == 1, "uniform", "realistic"),
        scen = scen,
        model = model,
        type = "pca",
        mean_pca1 = random_sp$details$means[1],
        mean_pca2 = random_sp$details$means[2],
        sd_pca1 = random_sp$details$sds[1],
        sd_pca2 = random_sp$details$sds[2],
        sp_i = sp_i
      )

    round1_factor <- 7

    plot_data <- data %>%
      mutate(
        pca1_round1 = round(`2021_pca1` / round1_factor, digits = 1) * round1_factor,
        pca2_round1 = round(`2021_pca2` / round1_factor, digits = 1) * round1_factor,
        pca1_round2 = round(`2021_pca1`, digits = 1),
        pca2_round2 = round(`2021_pca2`, digits = 1)
      ) %>%
      mutate(model = case_when(
        model == "first" ~ "1979 - All",
        model == "early" ~ "2000 - All",
        model == "early_later" ~ "2000 - During and after",
        model == "early_matched" ~ "2000 - During",
        model == "sliding" ~ "Matched - All",
        model == "late" ~ "2020 - All",
        model == "late_matched" ~ "2020 - During"
      )) %>%
      mutate(
        scen = case_when(
          scen == "static" ~ "Static",
          scen == "lead_track" ~ "Leading edge tracking",
          scen == "trail_track" ~ "Trailing edge tracking",
          scen == "both_track" ~ "Both edges tracking"
        ),
        yearly_n = str_to_title(yearly_n)
      ) %>%
      mutate(
        scen = factor(scen,
          ordered = T,
          levels = c(
            "Static", "Leading edge tracking",
            "Trailing edge tracking", "Both edges tracking"
          )
        ),
        model = factor(model, ordered = T, levels = c(
          "1979 - All",
          "2000 - All",
          "2000 - During",
          "2020 - All",
          "2020 - During",
          "Matched - All"
        )),
        yearly_n = factor(yearly_n, ordered = T, levels = c("Uniform", "Realistic"))
      )

    cc_dir_data <- plot_data %>%
      select(contains("pca"), -contains("round2"), -contains("mean"), -contains("sd")) %>%
      distinct() %>%
      group_by(pca1_round1, pca2_round1) %>%
      summarise(across(everything(), .f = list(mean = mean)))
    
    contour_data <- plot_data %>%
      group_by(pca1_round2, pca2_round2, yearly_n, scen, model) %>%
      summarise(
        sdm = mean(sdm),
        real = mean(real)
      )

    contour_summary <- plot_data %>%
      group_by(pca1_round2, pca2_round2, model, scen, yearly_n) %>%
      summarise(across(c("real", "sdm"), .f = list(mean = mean))) %>%
      mutate(
        prediction = case_when(
          real_mean >= 0.5 & sdm_mean < 0.5 ~ "Underprediction",
          real_mean < 0.5 & sdm_mean >= 0.5 ~ "Overprediction",
          real_mean >= 0.5 & sdm_mean >= 0.5 ~ "Overlap",
          T ~ "Outside"
        )
      ) %>%
      group_by(model, scen, yearly_n) %>%
      summarise(
        pearson = cor(sdm_mean, real_mean, method = "pearson"),
        Full_real = sum(prediction == "Overlap" | prediction == "Underprediction"),
        Overlap = sum(prediction == "Overlap") / Full_real,
        Underprediction = sum(prediction == "Underprediction") / Full_real,
        Overprediction = sum(prediction == "Overprediction") / Full_real
      ) %>%
      mutate(sp_i = sp_i)
  } else {
    data <- tibble(sp_i = sp_i)
    cc_dir_data <- NULL
    contour_data <- NULL
    contour_summary <- tibble(
      model = model,
      scen = scen,
      yearly_n = ifelse(length(unique(yearly_n)) == 1, "Uniform", "Realistic"),
      pearson = NA,
      Full_real = NA,
      Overlap = NA,
      Underprediction = NA,
      Overprediction = NA,
      sp_i = sp_i
    )
  }

  return(list(
    cc_dir_data,
    contour_data,
    contour_summary
  ))
}
