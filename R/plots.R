make_plot_1d <- function(scen_info_1d) {
  ## Better if all close to zero also here for mean and se calculations?

  plot_data_ind <- scen_info_1d %>%
    ## filter(model %in% c("first", "early", "sliding", "late")) %>%
    ## mutate(timing = case_when(
    ##   model %in% c("first", "early", "late", "sliding") ~ "All",
    ##   model == "early_later" ~ "During and after",
    ##   model %in% c("early_matched", "late_matched") ~ "During"
    ## )) %>%
    ## mutate(model = case_when(
    ##   model == "first" ~ "1979",
    ##   model %in% c("early", "early_later", "early_matched") ~ "2000",
    ##   model == "sliding" ~ "Matched",
    ##   model %in% c("late", "late_matched") ~ "2020"
    ## )) %>%
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
      ## model = factor(model, ordered = T, levels = c(
      ##   "1979",
      ##   "2000",
      ##   "2020",
      ##   "Matched"
      ## )),
      model = factor(model, ordered = T, levels = c(
        "1979 - All",
        "2000 - All",
        ## "2000 - During and after",
        "2000 - During",
        "2020 - All",
        "2020 - During",
        "Matched - All"
      )),
      yearly_n = factor(yearly_n, ordered = T, levels = c("Uniform", "Realistic"))
    )

  plot_data <- plot_data_ind %>%
    group_by(bio1_round, yearly_n, scen, model) %>%
    summarise(
      sdm_mean = mean(sdm_mean, na.rm = T),
      real_mean = mean(real_mean, na.rm = T)
    ) %>%
    drop_na()

  #########

  ## plot_data_ind <- plot_data_ind %>% filter(yearly_n == "Uniform")
  ## plot_data <- plot_data %>% filter(yearly_n == "Uniform")

  ## minmax_ind <- plot_data_ind %>%
  ##   filter(sdm_mean >= 0.5) %>%
  ##   group_by(yearly_n, scen, model, mean_bio1, sd_bio1) %>%
  ##   summarise(
  ##     bio1_min = min(bio1_round),
  ##     bio1_max = max(bio1_round)
  ##   ) %>%
  ##   mutate(dodge = case_when(
  ##     model == "1979 - All" ~ rnorm(n = 1, mean = -0.06, sd = 0.005),
  ##     model == "2000 - All" ~ rnorm(n = 1, mean = -0.04, sd = 0.005),
  ##     ## model == "2000 - During and after" ~ rnorm(n = 1, mean = -0.02, sd = 0.005),
  ##     model == "2000 - During" ~ rnorm(n = 1, mean = -0.02, sd = 0.005),
  ##     model == "2020 - All" ~ rnorm(n = 1, mean = 0.0, sd = 0.005),
  ##     model == "2020 - During" ~ rnorm(n = 1, mean = 0.02, sd = 0.005),
  ##     model == "Matched - All" ~ rnorm(n = 1, mean = 0.04, sd = 0.005),
  ##     T ~ NA
  ##   ))

  ## minmax <- plot_data %>%
  ##   filter(sdm_mean >= 0.5) %>%
  ##   group_by(yearly_n, scen, model) %>%
  ##   summarise(
  ##     bio1_min = min(bio1_round),
  ##     bio1_max = max(bio1_round)
  ##   ) %>%
  ##   mutate(dodge = case_when(
  ##     model == "1979 - All" ~ -0.06,
  ##     model == "2000 - All" ~ -0.04,
  ##     ## model == "2000 - During and after" ~ -0.02,
  ##     model == "2000 - During" ~ -0.02,
  ##     model == "2020 - All" ~ 0.00,
  ##     model == "2020 - During" ~ 0.02,
  ##     model == "Matched - All" ~ 0.04,
  ##     T ~ NA
  ##   ))

  ## minmax_real <- plot_data %>%
  ##   filter(real_mean >= 0.5) %>%
  ##   group_by(yearly_n, scen, model) %>%
  ##   summarise(
  ##     bio1_min = min(bio1_round),
  ##     bio1_max = max(bio1_round)
  ##   )

  ## scen_labs <- c("Static", "Leading edge tracking - Expansion", "Trailing edge tracking - Contraction", "Both edges tracking - Range shift")
  ## names(scen_labs) <- c("Static", "Leading edge tracking", "Trailing edge tracking", "Both edges tracking")

  ## ## plot_one
  ## ggplot(
  ##   data = plot_data, # %>% filter(model != "Matched - All"),
  ##   aes(
  ##     x = bio1_round,
  ##     y = sdm_mean,
  ##     col = model,
  ##     ## linetype = model,
  ##     group = paste0(yearly_n, model, scen)
  ##   )
  ## ) +
  ##   geom_line(
  ##     data = plot_data_ind,
  ##     aes(group = paste0(yearly_n, model, scen, mean_bio1, sd_bio1)),
  ##     alpha = 0.05
  ##   ) +
  ##   geom_line(size = 1) +
  ##   geom_line(aes(y = real_mean), col = "black", alpha = 0.25) +
  ##   geom_segment(
  ##     data = minmax_ind,
  ##     aes(
  ##       x = bio1_min, xend = bio1_max,
  ##       y = -0.08 + dodge, yend = -0.08 + dodge,
  ##       col = model,
  ##       group = paste0(yearly_n, model, scen, mean_bio1, sd_bio1)
  ##     ),
  ##     alpha = 0.05
  ##   ) +
  ##   geom_segment(
  ##     data = minmax, # %>% filter(model != "Matched - All"),
  ##     aes(
  ##       x = bio1_min, xend = bio1_max,
  ##       y = -0.08 + dodge, yend = -0.08 + dodge,
  ##       col = model,
  ##       group = paste0(yearly_n, model, scen)
  ##     )
  ##   ) +
  ##   scale_color_okabe_ito(order = c(7, 6, 1, 5, 2, 3)) +
  ##   labs(col = "Model") +
  ##   new_scale_colour() +
  ##   geom_segment(
  ##     data = minmax_real,
  ##     aes(
  ##       x = bio1_min, xend = bio1_max,
  ##       y = -0.16, yend = -0.16, col = "Virtual species"
  ##     )
  ##   ) +
  ##   scale_color_okabe_ito(order = c(9), labels = NULL) +
  ##   labs(col = "Virtual species") +
  ##   xlim(-3, 4) +
  ##   ylim(-0.5, 1) +
  ##   scale_y_continuous(
  ##     labels = scales::number_format(
  ##       accuracy = 0.1,
  ##       decimal.mark = "."
  ##     )
  ##   ) +
  ##   facet_grid(~scen, labeller = labeller(scen = scen_labs)) +
  ##   labs(x = "Standardised annual mean temperature", y = "Probability of occurrence") +
  ##   theme_pubr()


  return(list(
    plot_data_ind,
    plot_data
  ))
}


