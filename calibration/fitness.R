spring_run_fitness <- function(
  known_adults,
  seeds,
  params,
  surv_adult_enroute_int,
  surv_adult_prespawn_int,
  surv_egg_to_fry_int,
  default_surv_rear_int,
  battle_clear_surv_rear_int,
  butte_surv_rear_int,
  deer_surv_rear_int,
  mill_surv_rear_int,
  sac_surv_rear_int,
  feather_surv_rear_int,
  yuba_surv_rear_int,
  sj_surv_rear_int,
  surv_juv_rear_contact_points,
  surv_juv_rear_prop_diversions,
  surv_juv_rear_total_diversions,
  surv_juv_bypass_int,
  surv_juv_delta_int,
  surv_juv_delta_contact_points,
  surv_juv_delta_total_diverted,
  surv_juv_outmigration_sj_int,
  surv_juv_outmigration_sac_delta_intercept_one,
  surv_juv_outmigration_sac_delta_intercept_two,
  surv_juv_outmigration_sac_delta_intercept_three,
  default_ocean_int,
  butte_ocean_int,
  deer_battle_clear_ocean_int,
  mill_ocean_int,
  bear_feather_ocean_int,
  yuba_ocean_int
) {
  params_init <- params
  
  # Juvenile rearing survival coefficients and variables
  params_init$..surv_juv_rear_int = c(`Upper Sacramento River` = default_surv_rear_int, 
                                      `Antelope Creek` = default_surv_rear_int, 
                                      `Battle Creek` = battle_clear_surv_rear_int,
                                      `Bear Creek` = default_surv_rear_int, 
                                      `Big Chico Creek` = default_surv_rear_int, 
                                      `Butte Creek` = butte_surv_rear_int,
                                      `Clear Creek` = 	battle_clear_surv_rear_int, 
                                      `Cottonwood Creek` = default_surv_rear_int, 
                                      `Cow Creek` = default_surv_rear_int,
                                      `Deer Creek` = deer_surv_rear_int, 
                                      `Elder Creek` = default_surv_rear_int, 
                                      `Mill Creek` = mill_surv_rear_int,
                                      `Paynes Creek` = default_surv_rear_int, 
                                      `Stony Creek` = default_surv_rear_int, 
                                      `Thomes Creek` = default_surv_rear_int,
                                      `Upper-mid Sacramento River` = sac_surv_rear_int, 
                                      `Sutter Bypass` = default_surv_rear_int,
                                      `Bear River` = default_surv_rear_int, 
                                      `Feather River` = feather_surv_rear_int, 
                                      `Yuba River` = yuba_surv_rear_int,
                                      `Lower-mid Sacramento River` = 	sac_surv_rear_int, 
                                      `Yolo Bypass` = default_surv_rear_int, 
                                      `American River` = default_surv_rear_int,
                                      `Lower Sacramento River` = sac_surv_rear_int, 
                                      `Calaveras River` = default_surv_rear_int, 
                                      `Cosumnes River` = default_surv_rear_int,
                                      `Mokelumne River` = default_surv_rear_int, 
                                      `Merced River` = default_surv_rear_int, 
                                      `Stanislaus River` = default_surv_rear_int,
                                      `Tuolumne River` = default_surv_rear_int, 
                                      `San Joaquin River` = sj_surv_rear_int)
  
  params_init$..surv_adult_enroute_int = surv_adult_enroute_int
  params_init$..surv_adult_prespawn_int = surv_adult_prespawn_int # they hard coded from fall run
  params_init$..surv_egg_to_fry_int = surv_egg_to_fry_int  # they hard coded from fall run
  params_init$..surv_juv_rear_contact_points = surv_juv_rear_contact_points
  params_init$..surv_juv_rear_prop_diversions = surv_juv_rear_prop_diversions
  params_init$..surv_juv_rear_total_diversions = surv_juv_rear_total_diversions
  params_init$..surv_juv_bypass_int = surv_juv_bypass_int
  params_init$..surv_juv_delta_int = surv_juv_delta_int
  params_init$..surv_juv_delta_contact_points = surv_juv_delta_contact_points
  params_init$..surv_juv_delta_total_diverted = surv_juv_delta_total_diverted
  params_init$..surv_juv_outmigration_sj_int = surv_juv_outmigration_sj_int
  
  params_init$..surv_juv_outmigration_sac_delta_intercept_one = surv_juv_outmigration_sac_delta_intercept_one
  params_init$..surv_juv_outmigration_sac_delta_intercept_two = surv_juv_outmigration_sac_delta_intercept_two
  params_init$..surv_juv_outmigration_sac_delta_intercept_three = surv_juv_outmigration_sac_delta_intercept_three
  
  # Ocean entry success coefficient and variable
  params_init$..ocean_entry_success_int = c(
    `Upper Sacramento River` = default_ocean_int,
    `Antelope Creek` = default_ocean_int,
    `Battle Creek` = deer_battle_clear_ocean_int,
    `Bear Creek` = default_ocean_int,
    `Big Chico Creek` = default_ocean_int,
    `Butte Creek` = butte_ocean_int,
    `Clear Creek` = deer_battle_clear_ocean_int,
    `Cottonwood Creek` = default_ocean_int,
    `Cow Creek` = default_ocean_int,
    `Deer Creek` = deer_battle_clear_ocean_int,
    `Elder Creek` = default_ocean_int,
    `Mill Creek` = mill_ocean_int,
    `Paynes Creek` = default_ocean_int,
    `Stony Creek` = default_ocean_int,
    `Thomes Creek` = default_ocean_int,
    `Upper-mid Sacramento River` = default_ocean_int,
    `Sutter Bypass` = default_ocean_int,
    `Bear River` = bear_feather_ocean_int,
    `Feather River` = bear_feather_ocean_int,
    `Yuba River` = yuba_ocean_int,
    `Lower-mid Sacramento River` = default_ocean_int,
    `Yolo Bypass` = default_ocean_int,
    `American River` = default_ocean_int,
    `Lower Sacramento River` = default_ocean_int,
    `Calaveras River` = default_ocean_int,
    `Cosumnes River` = default_ocean_int,
    `Mokelumne River` = default_ocean_int,
    `Merced River` = default_ocean_int,
    `Stanislaus River` = default_ocean_int,
    `Tuolumne River` = default_ocean_int,
    `San Joaquin River` = default_ocean_int
  )
  
  keep <- c(3L, 6L, 10L, 12L, 19L, 20L)
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
    
    # watershed_cor <- sapply(1:length(keep), function(i) {
    #   cor(preds[i,], known_nats[i,], use = "pairwise.complete.obs")
    # })
    sse <- sum(((preds[keep,] - known_nats)^2 * weights)/mean_escapent, na.rm = TRUE)
    
    return(sse)
  },
  error = function(e) return(1e12),
  warning = function(w) return(1e12)
  )
}



# x <- runif(29)
# print(spring_run_fitness(
#   known_adults = known_adults,
#   seeds = calibration_seeds,
#   params = params,
#   x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
#   x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
#   x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
#   x[29]
# ))
