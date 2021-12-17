spring_run_fitness <- function(
  known_adults,
  seeds,
  params,
  surv_adult_enroute_int,
  surv_adult_prespawn_int,
  surv_egg_to_fry_int,
  surv_juv_rear_int_default,
  surv_juv_rear_int_battle_clear,
  surv_juv_rear_int_butte,
  surv_juv_rear_int_deer,
  surv_juv_rear_int_mill,
  surv_juv_rear_int_sac,
  surv_juv_rear_int_feather,
  surv_juv_rear_int_yuba,
  surv_juv_rear_int_san_joaq,
  surv_juv_rear_contact_points,
  surv_juv_rear_prop_diversions,
  surv_juv_rear_total_diversions,
  surv_juv_bypass_int,
  surv_juv_delta_int,
  surv_juv_delta_contact_points,
  surv_juv_delta_total_diverted,
  surv_juv_outmigration_sj_int,
  ocean_entry_success_int_default,
  ocean_entry_success_int_battle_clear,
  ocean_entry_success_int_butte,
  ocean_entry_success_int_deer,
  ocean_entry_success_int_mill,
  ocean_entry_success_int_bear_feather,
  ocean_entry_success_int_yuba
) {
  params_init <- params
  
  params_init$..surv_adult_enroute_int = surv_adult_enroute_int
  params_init$..surv_adult_prespawn_int = surv_adult_prespawn_int # they hard coded from fall run
  params_init$..surv_egg_to_fry_int = surv_egg_to_fry_int  # they hard coded from fall run
  
  # Juvenile rearing survival coefficients and variables
  params_init$..surv_juv_rear_int = c(`Upper Sacramento River` = surv_juv_rear_int_default, 
                                      `Antelope Creek` = surv_juv_rear_int_default, 
                                      `Battle Creek` = surv_juv_rear_int_battle_clear,
                                      `Bear Creek` = surv_juv_rear_int_default, 
                                      `Big Chico Creek` = surv_juv_rear_int_default, 
                                      `Butte Creek` = surv_juv_rear_int_butte,
                                      `Clear Creek` = 	surv_juv_rear_int_battle_clear, 
                                      `Cottonwood Creek` = surv_juv_rear_int_default, 
                                      `Cow Creek` = surv_juv_rear_int_default,
                                      `Deer Creek` = surv_juv_rear_int_deer, 
                                      `Elder Creek` = surv_juv_rear_int_default, 
                                      `Mill Creek` = surv_juv_rear_int_mill,
                                      `Paynes Creek` = surv_juv_rear_int_default, 
                                      `Stony Creek` = surv_juv_rear_int_default, 
                                      `Thomes Creek` = surv_juv_rear_int_default,
                                      `Upper-mid Sacramento River` = surv_juv_rear_int_sac, 
                                      `Sutter Bypass` = surv_juv_rear_int_default,
                                      `Bear River` = surv_juv_rear_int_default, 
                                      `Feather River` = surv_juv_rear_int_feather, 
                                      `Yuba River` = surv_juv_rear_int_yuba,
                                      `Lower-mid Sacramento River` = 	surv_juv_rear_int_sac, 
                                      `Yolo Bypass` = surv_juv_rear_int_default, 
                                      `American River` = surv_juv_rear_int_default,
                                      `Lower Sacramento River` = surv_juv_rear_int_sac, 
                                      `Calaveras River` = surv_juv_rear_int_default, 
                                      `Cosumnes River` = surv_juv_rear_int_default,
                                      `Mokelumne River` = surv_juv_rear_int_default, 
                                      `Merced River` = surv_juv_rear_int_default, 
                                      `Stanislaus River` = surv_juv_rear_int_default,
                                      `Tuolumne River` = surv_juv_rear_int_default, 
                                      `San Joaquin River` = surv_juv_rear_int_san_joaq)
  
  params_init$..surv_juv_rear_contact_points = surv_juv_rear_contact_points
  params_init$..surv_juv_rear_prop_diversions = surv_juv_rear_prop_diversions
  params_init$..surv_juv_rear_total_diversions = surv_juv_rear_total_diversions
  params_init$..surv_juv_bypass_int = surv_juv_bypass_int
  params_init$..surv_juv_delta_int = surv_juv_delta_int
  params_init$..surv_juv_delta_contact_points = surv_juv_delta_contact_points
  params_init$..surv_juv_delta_total_diverted = surv_juv_delta_total_diverted
  params_init$..surv_juv_outmigration_sj_int = surv_juv_outmigration_sj_int
  
  # Ocean entry success coefficient and variable
  params_init$..ocean_entry_success_int = c(
    `Upper Sacramento River` = ocean_entry_success_int_default,
    `Antelope Creek` = ocean_entry_success_int_default,
    `Battle Creek` = ocean_entry_success_int_battle_clear,
    `Bear Creek` = ocean_entry_success_int_default,
    `Big Chico Creek` = ocean_entry_success_int_default,
    `Butte Creek` = ocean_entry_success_int_butte,
    `Clear Creek` = ocean_entry_success_int_battle_clear,
    `Cottonwood Creek` = ocean_entry_success_int_default,
    `Cow Creek` = ocean_entry_success_int_default,
    `Deer Creek` = ocean_entry_success_int_deer,
    `Elder Creek` = ocean_entry_success_int_default,
    `Mill Creek` = ocean_entry_success_int_mill,
    `Paynes Creek` = ocean_entry_success_int_default,
    `Stony Creek` = ocean_entry_success_int_default,
    `Thomes Creek` = ocean_entry_success_int_default,
    `Upper-mid Sacramento River` = ocean_entry_success_int_default,
    `Sutter Bypass` = ocean_entry_success_int_default,
    `Bear River` = ocean_entry_success_int_bear_feather,
    `Feather River` = ocean_entry_success_int_bear_feather,
    `Yuba River` = ocean_entry_success_int_yuba,
    `Lower-mid Sacramento River` = ocean_entry_success_int_default,
    `Yolo Bypass` = ocean_entry_success_int_default,
    `American River` = ocean_entry_success_int_default,
    `Lower Sacramento River` = ocean_entry_success_int_default,
    `Calaveras River` = ocean_entry_success_int_default,
    `Cosumnes River` = ocean_entry_success_int_default,
    `Mokelumne River` = ocean_entry_success_int_default,
    `Merced River` = ocean_entry_success_int_default,
    `Stanislaus River` = ocean_entry_success_int_default,
    `Tuolumne River` = ocean_entry_success_int_default,
    `San Joaquin River` = ocean_entry_success_int_default
  )
  
  keep <- c(2, 3, 6, 7, 10, 12, 19, 20)
  num_obs <- rowSums(!is.na(known_adults[keep, 6:19]))
  total_obs <- sum(!is.na(known_adults[keep, 6:19]))
  weights <- num_obs / total_obs
  
  
  tryCatch({
    preds <- spring_run_model(mode = "calibrate",
                              seeds = seeds,
                              stochastic = FALSE,
                              ..params = params_init)
    
    known_nats <- known_adults[keep, 6:19] * (1 - params_init$proportion_hatchery[keep])
    mean_escapent <-rowMeans(known_nats, na.rm = TRUE)
    
    sse <- sum(((preds[keep,] - known_nats)^2 * weights)/mean_escapent, na.rm = TRUE)
    
    return(sse)
  },
  error = function(e) return(1e12),
  warning = function(w) return(1e12)
  )
}

# TODO for debugging purposes
# x <- runif(27)
# print(spring_run_fitness(
#   known_adults = DSMCalibrationData::grandtab_observed$spring,
#   seeds = DSMCalibrationData::grandtab_imputed$spring,
#   params = params,
#   x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
#   x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
#   x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27]
# ))
