library(tidyverse)

calibration_solution <- read_rds('calibration/calibration_best_fit.rds')

x <- calibration_solution@solution

params <- list(
  
  # Data from DSMscenarios
  spawn_decay_rate = DSMscenario::spawn_decay_rate,
  rear_decay_rate = DSMscenario::rear_decay_rate,
  
  adult_territory_size = 1,
  # Yearling
  yearling_territory_size = c(0.05423379, 0.14539419, 0.48471474, 0.48471474),
  ## Variable from load baseline data
  # DSMflow variables -----
  freeport_flows = DSMflow::freeport_flow,
  vernalis_flows = DSMflow::vernalis_flow,
  stockton_flows = DSMflow::stockton_flow,
  CVP_exports = DSMflow::cvp_exports,
  SWP_exports = DSMflow::swp_exports,
  proportion_diverted = DSMflow::proportion_diverted,
  total_diverted = DSMflow::total_diverted,
  delta_proportion_diverted = DSMflow::delta_proportion_diverted,
  delta_total_diverted = DSMflow::delta_total_diverted,
  prop_pulse_flows = DSMflow::proportion_pulse_flows,
  prop_flow_natal = DSMflow::proportion_flow_natal,
  upper_sacramento_flows = DSMflow::upper_sacramento_flows,
  delta_inflow = DSMflow::delta_inflow,
  cc_gates_days_closed = DSMflow::delta_cross_channel_closed["count", ],
  cc_gates_prop_days_closed = DSMflow::delta_cross_channel_closed["proportion", ],
  proportion_flow_bypass = DSMflow::proportion_flow_bypasses,
  gates_overtopped = DSMflow::gates_overtopped,
  
  # DSMtemperature variables -----
  vernalis_temps = DSMtemperature::vernalis_temperature,
  prisoners_point_temps = DSMtemperature::prisoners_point_temperature,
  degree_days = DSMtemperature::degree_days,
  mean_egg_temp_effect = DSMtemperature::egg_temperature_effect$spring_run,
  avg_temp = DSMtemperature::stream_temperature,
  avg_temp_delta = DSMtemperature::delta_temperature,
  migratory_temperature_proportion_over_20 = DSMtemperature::migratory_temperature_proportion_over_20,
  
  # DSMhabitat variables -----
  spawning_habitat = DSMhabitat::sr_spawn,
  inchannel_habitat_fry = DSMhabitat::sr_fry, # vary by run
  inchannel_habitat_juvenile = DSMhabitat::sr_juv, # vary by run
  floodplain_habitat = DSMhabitat::sr_fp, # vary by run
  weeks_flooded = DSMhabitat::weeks_flooded,
  delta_habitat = DSMhabitat::delta_habitat,
  sutter_habitat = DSMhabitat::sutter_habitat,
  yolo_habitat = DSMhabitat::yolo_habitat,
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
  
  # Juvenile rearing survival coefficients and variables
  ..surv_juv_rear_int = c(`Upper Sacramento River` = x[1], 
                          `Antelope Creek` = x[1], 
                          `Battle Creek` = x[2],
                          `Bear Creek` = x[1], 
                          `Big Chico Creek` = x[1], 
                          `Butte Creek` = x[3],
                          `Clear Creek` = 	x[2], 
                          `Cottonwood Creek` = x[1], 
                          `Cow Creek` = x[1],
                          `Deer Creek` = x[4], 
                          `Elder Creek` = x[1], 
                          `Mill Creek` = x[5],
                          `Paynes Creek` = x[1], 
                          `Stony Creek` = x[1], 
                          `Thomes Creek` = x[1],
                          `Upper-mid Sacramento River` = x[6], 
                          `Sutter Bypass` = x[1],
                          `Bear River` = x[1], 
                          `Feather River` = x[7], 
                          `Yuba River` = x[8],
                          `Lower-mid Sacramento River` = 	x[6], 
                          `Yolo Bypass` = x[1], 
                          `American River` = x[1],
                          `Lower Sacramento River` = x[6], 
                          `Calaveras River` = x[1], 
                          `Cosumnes River` = x[1],
                          `Mokelumne River` = x[1], 
                          `Merced River` = x[1], 
                          `Stanislaus River` = x[1],
                          `Tuolumne River` = x[1], 
                          `San Joaquin River` = x[9]),
  ..surv_adult_enroute_int = x[10],
  ..surv_adult_prespawn_int = x[11], # they hard coded from fall run
  ..surv_egg_to_fry_int = x[12],  # they hard coded from fall run
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
    `Upper Sacramento River` = x[24],
    `Antelope Creek` = x[24],
    `Battle Creek` = x[25],
    `Bear Creek` = x[24],
    `Big Chico Creek` = x[24],
    `Butte Creek` = x[29],
    `Clear Creek` = x[25],
    `Cottonwood Creek` = x[24],
    `Cow Creek` = x[24],
    `Deer Creek` = x[25],
    `Elder Creek` = x[24],
    `Mill Creek` = x[26],
    `Paynes Creek` = x[24],
    `Stony Creek` = x[24],
    `Thomes Creek` = x[24],
    `Upper-mid Sacramento River` = x[24],
    `Sutter Bypass` = x[24],
    `Bear River` = x[27],
    `Feather River` = x[27],
    `Yuba River` = x[28],
    `Lower-mid Sacramento River` = x[24],
    `Yolo Bypass` = x[24],
    `American River` = x[24],
    `Lower Sacramento River` = x[24],
    `Calaveras River` = x[24],
    `Cosumnes River` = x[24],
    `Mokelumne River` = x[24],
    `Merced River` = x[24],
    `Stanislaus River` = x[24],
    `Tuolumne River` = x[24],
    `San Joaquin River` = x[24])
  
)

usethis::use_data(params, overwrite = TRUE)




