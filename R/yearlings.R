#'@title Spring-Run Yearlings
yearling_growth <- function(year, yearlings) {
  # do stuff here
  # 1. get growthn and transition matricse
  # 2. data (monthly temp, diversions, predation)
  # 3. get rearing survival values (inchannel and floodplain)
  # 4. apply survival 
  
  growth_rates <- diag(1, ncol=4)
  growth_rates_floodplain <- replicate(4, diag(1, 4, 4))
  
  for (month in 5:10) {
    # calculate growth rates, only months 9, 10 experience growth
    if (month %in% 9:10) {
      growth_rates <- growth()
      growth_rates_floodplain <- growth_floodplain()  
    } 
    
    yearling_habitat <- get_habitat(year, month) 
    
    
    # get survival rates
    yearling_survival_rates <- get_rearing_survival_rates(year = year, 
                               month = month, 
                               scenario = NULL)
    
    # TODO confirm the method above, the OG model has scaling in this part of the run
    yearlings_filled <- fill_natal(juveniles = yearlings, 
               inchannel_habitat = yearling_habitat$inchannel, 
               floodplain_habitat = yearling_habitat$floodplain, 
               territory_size = c(0.05423379,0.14539419,0.48471474,0.00000000))
    
    yearlings_reared <- rear(juveniles = yearlings_filled$inchannel, 
                             survival_rate = yearling_survival_rates$inchannel, 
                             growth = growth_rates, 
                             floodplain_juveniles = yearlings_filled$floodplain, 
                             floodplain_survival_rate = yearling_survival_rates$floodplain, 
                             floodplain_growth = growth_rates_floodplain, 
                             weeks_flooded = weeks_flooded[, month, year])
    
    yearlings <- yearlings_reared$inchannel + yearlings_reared$floodplain
    
  }
  
  return(yearlings)
}


#' @title Yearling Migration
yearling_migration <- function() {
  # all remaining fish outmigrate
  
  sutter_fish <- migrate(sutter_fish, migratory_survival$sutter)
  upper_mid_sac_fish <- migrate(upper_mid_sac_fish + juveniles[1:15, ], migratory_survival$uppermid_sac)
  migrants[1:15, ] <- upper_mid_sac_fish + sutter_fish
  yolo_fish <- migrate(yolo_fish, migratory_survival$yolo)
  migrants[18:20, ] <- juveniles[18:20, ] + yolo_fish
  lower_mid_sac_fish <- migrate(lower_mid_sac_fish + migrants, migratory_survival$lowermid_sac)
  migrants <- lower_mid_sac_fish
  migrants[23, ] <- juveniles[23, ]
  lower_sac_fish <- migrate(lower_sac_fish + migrants, migratory_survival$lower_sac)
  migrants[25:27, ] <- juveniles[25:27, ]
  san_joaquin_fish <- migrate(juveniles[28:30, ] + san_joaquin_fish, migratory_survival$san_joaquin)
  migrants[18:20, ] <- migrants[18:20, ] + yolo_fish
  migrants[28:30, ] <- san_joaquin_fish
  
  delta_fish <- route_and_rear_deltas(year = juv_dynamics_year, month = month,
                                      migrants = round(migrants),
                                      north_delta_fish = north_delta_fish,
                                      south_delta_fish = south_delta_fish,
                                      north_delta_habitat = habitat$north_delta,
                                      south_delta_habitat = habitat$south_delta,
                                      rearing_survival_delta = rearing_survival$delta,
                                      migratory_survival_delta = migratory_survival$delta,
                                      migratory_survival_sac_delta = migratory_survival$sac_delta,
                                      migratory_survival_bay_delta = migratory_survival$bay_delta,
                                      juveniles_at_chipps = juveniles_at_chipps,
                                      growth_rates = growth_rates)
  
  
  migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate
  
  annual_migrants <- annual_migrants + migrants_at_golden_gate
}



