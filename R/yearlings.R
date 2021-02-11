#'@title Spring-Run Yearlings
yearling_growth <- function(year, yearlings, 
                            territory_size = c(0.05423379,0.14539419,0.48471474,0.48471474)) {
  
  growth_rates <- diag(1, nrow = 4, ncol = 4)
  growth_rates_floodplain <- replicate(4, diag(1, 4, 4))
  
  for (month in 5:10) {
    # only months 9, 10 experience growth
    if (month %in% 9:10) {
      growth_rates <- growth()
      growth_rates_floodplain <- growth_floodplain()  
    } 
    
    this_weeks_flooded <- weeks_flooded[, month, year]
    
    # we only care for floodplain and inchannel
    habitat <- get_habitat(year, month) 
    
    
    # we only care for floodplain and inchannel
    survival_rates <- get_rearing_survival_rates(year = year, 
                               month = month, 
                               scenario = NULL)
    
    ic_rearing <- matrix(0, nrow = nrow(yearlings), ncol = 4)
    fp_rearing <- matrix(0, nrow = nrow(yearlings), ncol = 4)
    floodplain_remaining <- habitat$floodplain
    
    for (w in 1:nrow(yearlings)) {
      for (s in 4:1) {
        # either the max the number of fish that fit or the total number of fish
        fp_rearing[w, s] <- min(floodplain_remaining[w]/territory_size[s], yearlings[w, s])
        # update floodplain remaining 
        floodplain_remaining[w] <- 
          max(floodplain_remaining[w] - fp_rearing[w, s]*territory_size[s], 0)
      }  
    }
    
    fp_rearing <- round(pmax(fp_rearing, 0)) # everything must be greater than 0
    yearlings <- pmax(yearlings - fp_rearing, 0) # remove from yearlings and set min 0
    
    inchannel_remaining <- habitat$inchannel 
    
    for (w in 1:nrow(yearlings)) {
      for (s in 4:1) {
        # either the max the number of fish that fit or the total number of fish
        ic_rearing[w, s] <- min(inchannel_remaining[w]/territory_size[s], yearlings[w, s])
        # update inchannel remaining 
        inchannel_remaining[w] <- 
          max(inchannel_remaining[w] - ic_rearing[w, s]*territory_size[s], 0)
      } 
    }
    
    ic_rearing <- round(pmax(ic_rearing, 0))
    yearlings <- pmax(yearlings - ic_rearing, 0)
    
    # apply habitat specific mortality
    fp_rearing <- t(sapply(1:nrow(fp_rearing), function(i) {
      rbinom(4, size=fp_rearing[i, ], prob = survival_rates$floodplain[i, ])
    }))
    
    ic_rearing <- t(sapply(1:nrow(ic_rearing), function(i) {
      rbinom(4, size=ic_rearing[i, ], prob = survival_rates$inchannel[i, ])
    }))
    
    # apply growth 
    ic_rearing <- round(ic_rearing %*% growth_rates)
    
    
    fp_rearing <- t(sapply(1:length(this_weeks_flooded), function(i) {
      if (this_weeks_flooded[i] == 0) 
        fp_rearing[i, ]
      else 
        fp_rearing[i, ] %*% growth_rates_floodplain[,,this_weeks_flooded[i]]
    }))
    
    yearlings <- round(fp_rearing + ic_rearing)
  
  }
  
  return(yearlings)
}


