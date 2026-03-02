balance <- function(occ, multiplier = 1){

  ## all presences
  pr <- occ %>%
    filter(pr_ab == 1)

  ## Number of presences per year
  n_per_year <- pr %>%
    group_by(year) %>%
    summarise(n = n())

  ## Filter absences to match number of presences in each year (multiplier is 1)
  ab <- apply(n_per_year, 1, function(n_yr){

    n_yr

    occ %>%
      filter(pr_ab == 0,
             year == n_yr["year"]) %>%
      sample_n(min(n_yr["n"] * multiplier, nrow(.)))

  })

  balanced <- rbind(do.call(rbind, ab),
                    pr)

  return(balanced)

}
