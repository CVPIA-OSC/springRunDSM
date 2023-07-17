library(tidyverse)
# 2022

# start with old params
params_2022_raw <- springRunDSM::params
# add new decay multiplier
params_2022_raw$spawn_decay_multiplier <- DSMhabitat::spawning_decay_multiplier$biop_itp_2018_2019$sr
# updates based on latest calibration
source("calibration/update-params.R")
calib_results_2022 <- readr::read_rds("calibration/calibration-results-2023.rds")@solution[1,]
params_2022 <- update_params(x = calib_results_2022, params = params_2022_raw)

usethis::use_data(params_2022, overwrite = TRUE)


calibration_solution <- read_rds('calibration/calibrated-results.rds')

x <- calibration_solution@solution[1,]
names(x) <- NULL

params <- list(
  
  # Data from DSMscenarios
  spawn_decay_rate = DSMscenario::spawn_decay_rate,
  rear_decay_rate = DSMscenario::rear_decay_rate,
  
  adult_territory_size = 1,
  # Yearling
  yearling_territory_size = c(0.05423379, 0.14539419, 0.48471474, 0.48471474),
  ## Variable from load baseline data
  # DSMflow variables -----
  freeport_flows = DSMflow::freeport_flow$biop_itp_2018_2019,
  vernalis_flows = DSMflow::vernalis_flow$biop_itp_2018_2019,
  stockton_flows = DSMflow::stockton_flow$biop_itp_2018_2019,
  CVP_exports = DSMflow::cvp_exports$biop_itp_2018_2019,
  SWP_exports = DSMflow::swp_exports$biop_itp_2018_2019,
  proportion_diverted = DSMflow::proportion_diverted$biop_itp_2018_2019,
  total_diverted = DSMflow::total_diverted$biop_itp_2018_2019,
  delta_proportion_diverted = DSMflow::delta_proportion_diverted$biop_itp_2018_2019,
  delta_total_diverted = DSMflow::delta_total_diverted$biop_itp_2018_2019,
  prop_pulse_flows = DSMflow::proportion_pulse_flows$biop_itp_2018_2019,
  prop_flow_natal = DSMflow::proportion_flow_natal$biop_itp_2018_2019,
  upper_sacramento_flows = DSMflow::upper_sacramento_flows$biop_itp_2018_2019,
  delta_inflow = DSMflow::delta_inflow$biop_itp_2018_2019,
  cc_gates_days_closed = DSMflow::delta_cross_channel_closed$biop_itp_2018_2019["count", ],
  cc_gates_prop_days_closed = DSMflow::delta_cross_channel_closed$biop_itp_2018_2019["proportion", ],
  proportion_flow_bypass = DSMflow::proportion_flow_bypasses$biop_itp_2018_2019,
  gates_overtopped = DSMflow::gates_overtopped$biop_itp_2018_2019,
  
  # DSMtemperature variables -----
  vernalis_temps = DSMtemperature::vernalis_temperature,
  prisoners_point_temps = DSMtemperature::prisoners_point_temperature,
  degree_days = DSMtemperature::degree_days$biop_itp_2018_2019,
  mean_egg_temp_effect = DSMtemperature::egg_temperature_effect$spring_run,
  avg_temp = DSMtemperature::stream_temperature$biop_itp_2018_2019,
  avg_temp_delta = DSMtemperature::delta_temperature,
  migratory_temperature_proportion_over_20 = DSMtemperature::migratory_temperature_proportion_over_20,
  
  # DSMhabitat variables -----
  spawning_habitat = DSMhabitat::sr_spawn$biop_itp_2018_2019,
  inchannel_habitat_fry = DSMhabitat::sr_fry$biop_itp_2018_2019, # vary by run
  inchannel_habitat_juvenile = DSMhabitat::sr_juv$biop_itp_2018_2019, # vary by run
  floodplain_habitat = DSMhabitat::sr_fp$biop_itp_2018_2019, # vary by run
  weeks_flooded = DSMhabitat::weeks_flooded$biop_itp_2018_2019,
  delta_habitat = DSMhabitat::delta_habitat,
  sutter_habitat = DSMhabitat::sutter_habitat$biop_itp_2018_2019,
  yolo_habitat = DSMhabitat::yolo_habitat$biop_itp_2018_2019,
  tisdale_bypass_watershed = DSMhabitat::tisdale_bypass_watershed,
  yolo_bypass_watershed = DSMhabitat::yolo_bypass_watershed,
  south_delta_routed_watersheds = DSMhabitat::south_delta_routed_watersheds,
  prop_high_predation = DSMhabitat::prop_high_predation,
  contact_points = DSMhabitat::contact_points,
  delta_contact_points = DSMhabitat::delta_contact_points,
  delta_prop_high_predation = DSMhabitat::delta_prop_high_predation,
  prob_strand_early = DSMhabitat::prob_strand_early,
  prob_strand_late = DSMhabitat::prob_strand_late,
  prob_nest_scoured = DSMhabitat::prob_nest_scoured,
  spring_run_pools = ifelse(is.na(DSMhabitat::pools$SR_pools_sq_meters), 0, DSMhabitat::pools$SR_pools_sq_meters),
  
  # Data from springRunDSM cache-data (values vary by run)
  hatchery_allocation = springRunDSM::hatchery_allocation,
  natural_adult_removal_rate = springRunDSM::natural_adult_removal_rate,
  proportion_hatchery = springRunDSM::proportion_hatchery,
  month_return_proportions = springRunDSM::month_return_proportions,
  growth_rates = springRunDSM::growth_rates_inchannel,
  growth_rates_floodplain = springRunDSM::growth_rates_floodplain,
  mass_by_size_class = springRunDSM::mass_by_size_class,
  cross_channel_stray_rate = springRunDSM::cross_channel_stray_rate,
  stray_rate = springRunDSM::stray_rate,
  adult_harvest_rate = springRunDSM::adult_harvest_rate,
  diversity_group = springRunDSM::diversity_group,
  
  
  prey_density = springRunDSM::prey_density,
  prey_density_delta = springRunDSM::prey_density_delta,
  
  # Coefficients for adult submodules
  # stray
  .adult_stray_intercept = 3,
  .adult_stray_wild = -5.5,
  .adult_stray_natal_flow = -1.99,
  .adult_stray_cross_channel_gates_closed = -0.174,
  .adult_stray_prop_bay_trans = 2.09,
  .adult_stray_prop_delta_trans = 2.89,
  # Enroute survival
  .adult_en_route_migratory_temp = -0.26,
  .adult_en_route_bypass_overtopped = -0.019,
  .adult_en_route_adult_harvest_rate = springRunDSM::adult_harvest_rate, # varies by run
  # Prespawn Survival
  .adult_prespawn_deg_day = -0.000669526,
  
  # Routing coefficients and variables
  .pulse_movement_intercept = -7.70744,
  .pulse_movement_proportion_pulse = 0.26579,
  .pulse_movement_medium = 1.66845,
  .pulse_movement_large = 0.5706,
  .pulse_movement_vlarge = -4.305,
  .pulse_movement_medium_pulse = -0.25477,
  .pulse_movement_large_pulse = -0.44778,
  .pulse_movement_very_large_pulse = 0.329,
  territory_size = c(0.0498944803729701, 0.138941944739835, 0.471083652829798, 0),
  
  # Spawn success variables
  spawn_success_sex_ratio = 0.5,
  spawn_success_redd_size = 9.29,
  spawn_success_fecundity = 5522,
  
  # Egg to fry survival calubrated parameters and coefficents
  .surv_egg_to_fry_proportion_natural = 0.533,
  .surv_egg_to_fry_scour = -0.655,
  
  .surv_juv_rear_contact_points = -0.189, # from literature
  .surv_juv_rear_prop_diversions = -3.51, # from literature
  .surv_juv_rear_total_diversions = -0.0021, # from literature
  
  .surv_juv_rear_avg_temp_thresh = -0.717,
  .surv_juv_rear_high_predation = -0.122,
  .surv_juv_rear_stranded = -1.939,
  .surv_juv_rear_medium = 1.48,
  .surv_juv_rear_large = 2.223,
  .surv_juv_rear_floodplain = 0.47,
  min_survival_rate = 0.0001,
  
  # Juvenile bypass survival calibrated parameters and coefficients
  .surv_juv_bypass_avg_temp_thresh = -0.717,
  .surv_juv_bypass_high_predation = -0.122,
  .surv_juv_bypass_medium = 1.48,
  .surv_juv_bypass_large = 2.223,
  .surv_juv_bypass_floodplain = 0.47,
  
  # Juvenile delta survival coefficients and variables
  .surv_juv_delta_contact_points = -0.189, # from literature
  .surv_juv_delta_total_diverted = -0.0021, # from literature
  .surv_juv_delta_avg_temp_thresh = -0.717,
  .surv_juv_delta_high_predation = -0.122,
  .surv_juv_delta_prop_diverted = -3.51,
  .surv_juv_delta_medium = 1.48,
  .surv_juv_delta_large = 2.223,
  
  # San joaquin outmigration calibrated intercept and coefficents
  .surv_juv_outmigration_san_joaquin_medium = 1.48,
  .surv_juv_outmigration_san_joaquin_large = 2.223,
  

  .ocean_entry_success_length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
  .ocean_entry_success_months = 0.35,
  
  # Calibrated values
  ..surv_adult_enroute_int = x[1],
  ..surv_adult_prespawn_int = x[2], 
  ..surv_egg_to_fry_int = x[3],
  ..surv_juv_rear_int = c(`Upper Sacramento River` = x[4], 
                          `Antelope Creek` = x[4], 
                          `Battle Creek` = x[5],
                          `Bear Creek` = x[4], 
                          `Big Chico Creek` = x[4], 
                          `Butte Creek` = x[6],
                          `Clear Creek` = 	x[5], 
                          `Cottonwood Creek` = x[4], 
                          `Cow Creek` = x[4],
                          `Deer Creek` = x[7], 
                          `Elder Creek` = x[4], 
                          `Mill Creek` = x[8],
                          `Paynes Creek` = x[4], 
                          `Stony Creek` = x[4], 
                          `Thomes Creek` = x[4],
                          `Upper-mid Sacramento River` = x[9], 
                          `Sutter Bypass` = x[4],
                          `Bear River` = x[4], 
                          `Feather River` = x[10], 
                          `Yuba River` = x[11],
                          `Lower-mid Sacramento River` = 	x[9], 
                          `Yolo Bypass` = x[4], 
                          `American River` = x[4],
                          `Lower Sacramento River` = x[9], 
                          `Calaveras River` = x[4], 
                          `Cosumnes River` = x[4],
                          `Mokelumne River` = x[4], 
                          `Merced River` = x[4], 
                          `Stanislaus River` = x[4],
                          `Tuolumne River` = x[4], 
                          `San Joaquin River` = x[12]),
  ..surv_juv_rear_contact_points = x[13],
  ..surv_juv_rear_prop_diversions = x[14],
  ..surv_juv_rear_total_diversions = x[15],
  ..surv_juv_bypass_int = x[16],
  ..surv_juv_delta_int = x[17],
  ..surv_juv_delta_contact_points = x[18],
  ..surv_juv_delta_total_diverted = x[19],
  ..surv_juv_outmigration_sj_int = x[20],
  # Ocean entry success coefficient and variable
  ..ocean_entry_success_int = c(
    `Upper Sacramento River` = x[21],
    `Antelope Creek` = x[21],
    `Battle Creek` = x[22],
    `Bear Creek` = x[21],
    `Big Chico Creek` = x[21],
    `Butte Creek` = x[23],
    `Clear Creek` = x[22],
    `Cottonwood Creek` = x[21],
    `Cow Creek` = x[21],
    `Deer Creek` = x[24],
    `Elder Creek` = x[21],
    `Mill Creek` = x[25],
    `Paynes Creek` = x[21],
    `Stony Creek` = x[21],
    `Thomes Creek` = x[21],
    `Upper-mid Sacramento River` = x[21],
    `Sutter Bypass` = x[21],
    `Bear River` = x[26],
    `Feather River` = x[26],
    `Yuba River` = x[27],
    `Lower-mid Sacramento River` = x[21],
    `Yolo Bypass` = x[21],
    `American River` = x[21],
    `Lower Sacramento River` = x[21],
    `Calaveras River` = x[21],
    `Cosumnes River` = x[21],
    `Mokelumne River` = x[21],
    `Merced River` = x[21],
    `Stanislaus River` = x[21],
    `Tuolumne River` = x[21],
    `San Joaquin River` = x[21])
)

usethis::use_data(params, overwrite = TRUE)




