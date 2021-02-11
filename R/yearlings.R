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


#' @title Yearling Migration
yearling_migration <- function(year, yearlings, migrants, north_delta_fish, south_delta_fish, 
                               juveniles_at_chipps, growth_rates, annual_migrants, avg_ocean_transition_month, month = 11) {

  # all remaining fish outmigrate
  yearling_habitat <- get_habitat(year, month) 
  yearling_migratory_survival <- get_migratory_survival_rates(year, month) 
  yearling_rearing_survival <- get_rearing_survival_rates(year = year, 
                                                        month = month, 
                                                        scenario = NULL)
  upper_sac_trib_fish <-  route(year = year,
                                month = month,
                                juveniles = yearlings[1:15, ],
                                inchannel_habitat = yearling_habitat$inchannel[1:15],
                                floodplain_habitat = yearling_habitat$floodplain[1:15],
                                prop_pulse_flows =  prop_pulse_flows[1:15, ],
                                detour = 'sutter')
  
  # route migrant fish into Upper-mid Sac Region (fish from watersheds 1:15)
  # regional fish migrate further downstream or in sutter bypass
  sutter_fish <- route_bypass(bypass_fish = upper_sac_trib_fish$detoured,
                              bypass_habitat = yearling_habitat$sutter,
                              migration_survival_rate = yearling_migratory_survival$sutter)
  
  upper_mid_sac_fish <- route_regional(month = month,
                                       migrants =  upper_sac_trib_fish$migrants,
                                       inchannel_habitat = yearling_habitat$inchannel[16],
                                       floodplain_habitat = yearling_habitat$floodplain[16],
                                       prop_pulse_flows = prop_pulse_flows[16, , drop = FALSE],
                                       migration_survival_rate = yearling_migratory_survival$uppermid_sac)
  
  migrants[1:15, ] <- upper_mid_sac_fish$migrants + sutter_fish$migrants
  
  # route migrant fish into Lower-mid Sac Region (fish from watersheds 18:20, and migrants from Upper-mid Sac Region)
  # regional fish stay and rear
  # or migrate further downstream  or in yolo bypass
  lower_mid_sac_trib_fish <- route(year = year,
                                   month = month,
                                   juveniles = yearlings[18:20, ],
                                   inchannel_habitat = yearling_habitat$inchannel[18:20],
                                   floodplain_habitat = yearling_habitat$floodplain[18:20],
                                   prop_pulse_flows =  prop_pulse_flows[18:20, ],
                                   detour = 'yolo')
  
  yolo_fish <- route_bypass(bypass_fish = lower_mid_sac_trib_fish$detoured,
                            bypass_habitat = yearling_habitat$yolo,
                            migration_survival_rate = yearling_migratory_survival$yolo)
  
  migrants[18:20, ] <- lower_mid_sac_trib_fish$migrants + yolo_fish$migrants
  
  lower_mid_sac_fish <- route_regional(month = month,
                                       migrants = migrants,
                                       inchannel_habitat = yearling_habitat$inchannel[21],
                                       floodplain_habitat = yearling_habitat$floodplain[21],
                                       prop_pulse_flows = prop_pulse_flows[21, , drop = FALSE],
                                       migration_survival_rate = yearling_migratory_survival$lowermid_sac)
  
  migrants <- lower_mid_sac_fish$migrants
  
  # route migrant fish into Lower Sac Region (fish from watershed 23, and migrants from Lower-mid Sac Region)
  # regional fish stay and rear
  # or migrate north delta
  lower_sac_trib_fish <- route(year = year,
                               month = month,
                               juveniles = yearlings[23, , drop = FALSE],
                               inchannel_habitat = yearling_habitat$inchannel[23],
                               floodplain_habitat = yearling_habitat$floodplain[23],
                               prop_pulse_flows =  prop_pulse_flows[23, , drop = FALSE])
  
  migrants[23, ] <- lower_sac_trib_fish$migrants
  
  lower_sac_fish <- route_regional(month = month,
                                   migrants = migrants,
                                   inchannel_habitat = yearling_habitat$inchannel[24],
                                   floodplain_habitat = yearling_habitat$floodplain[24],
                                   prop_pulse_flows = prop_pulse_flows[24, , drop = FALSE],
                                   migration_survival_rate = yearling_migratory_survival$lower_sac)
  
  migrants <- lower_sac_fish$migrants
  # route southern natal fish stay and rear or migrate downstream ------
  
  # route migrant fish into South Delta Region (fish from watersheds 25:27)
  # regional fish stay and rear
  # or migrate to south delta
  south_delta_trib_fish <- route(year = year,
                                 month = month,
                                 juveniles = yearlings[25:27, ],
                                 inchannel_habitat = yearling_habitat$inchannel[25:27],
                                 floodplain_habitat = yearling_habitat$floodplain[25:27],
                                 prop_pulse_flows =  prop_pulse_flows[25:27, ])
  
  migrants[25:27, ] <- south_delta_trib_fish$migrants
  
  # route migrant fish into San Joquin River (fish from watersheds 28:30)
  # regional fish stay and rear
  # or migrate to south delta
  san_joaquin_trib_fish <- route(year = year,
                                 month = month,
                                 juveniles = yearlings[28:30, ],
                                 inchannel_habitat = yearling_habitat$inchannel[28:30],
                                 floodplain_habitat = yearling_habitat$floodplain[28:30],
                                 prop_pulse_flows =  prop_pulse_flows[28:30, ])
  
  san_joaquin_fish <- route_regional(month = month,
                                     migrants = san_joaquin_trib_fish$migrants,
                                     inchannel_habitat = yearling_habitat$inchannel[31],
                                     floodplain_habitat = yearling_habitat$floodplain[31],
                                     prop_pulse_flows = prop_pulse_flows[31, , drop = FALSE],
                                     migration_survival_rate = yearling_migratory_survival$san_joaquin)
  
  migrants[28:30, ] <- san_joaquin_fish$migrants
  
  delta_fish <- route_and_rear_deltas(year = year, month = month,
                                      migrants = round(migrants),
                                      north_delta_fish = north_delta_fish,
                                      south_delta_fish = south_delta_fish,
                                      north_delta_habitat = yearling_habitat$north_delta,
                                      south_delta_habitat = yearling_habitat$south_delta,
                                      rearing_survival_delta = yearling_rearing_survival$delta,
                                      migratory_survival_delta = yearling_migratory_survival$delta,
                                      migratory_survival_sac_delta = yearling_migratory_survival$sac_delta,
                                      migratory_survival_bay_delta = yearling_migratory_survival$bay_delta,
                                      juveniles_at_chipps = juveniles_at_chipps,
                                      growth_rates = growth_rates)
  
  yearling_migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate
  north_delta_fish <- delta_fish$north_delta_fish
  south_delta_fish <- delta_fish$south_delta_fish
  juveniles_at_chipps <- delta_fish$juveniles_at_chipps
  
  yearlings_in_ocean <- ocean_entry_success(migrants = yearling_migrants_at_golden_gate,
                                            month = month,
                                            avg_ocean_transition_month = avg_ocean_transition_month)
  annual_migrants <- annual_migrants + yearling_migrants_at_golden_gate
}



