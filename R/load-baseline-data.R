#' @title Load Baseline Data
#' @description loads baseline data from DSMflow, DSMhabitat and DSMtemperature
#' @details
#' More details for each item in the list available below.
#'
#' \strong{Flow Inputs}
#' \itemize{
#'   \item freeport_flows - \link[DSMflow]{freeport_flow}
#'   \item vernalis_flows - \link[DSMflow]{vernalis_flow}
#'   \item stockton_flows - \link[DSMflow]{stockton_flow}
#'   \item CVP_exports - \link[DSMflow]{cvp_exports}
#'   \item SWP_exports - \link[DSMflow]{swp_exports}
#'   \item proportion_diverted - \link[DSMflow]{proportion_diverted}
#'   \item total_diverted - \link[DSMflow]{total_diverted}
#'   \item delta_proportion_diverted - \link[DSMflow]{delta_proportion_diverted}
#'   \item delta_total_diverted - \link[DSMflow]{delta_total_diverted}
#'   \item prop_pulse_flows - \link[DSMflow]{proportion_pulse_flows}
#'   \item prop_flow_natal - \link[DSMflow]{proportion_flow_natal}
#'   \item upper_sacramento_flows - \link[DSMflow]{upper_sacramento_flows}
#'   \item delta_inflow - \link[DSMflow]{delta_inflow}
#'   \item cc_gates_days_closed - \link[DSMflow]{delta_cross_channel_closed}
#'   \item cc_gates_prop_days_closed - \link[DSMflow]{delta_cross_channel_closed}
#'   \item proportion_flow_bypass - \link[DSMflow]{proportion_flow_bypasses}
#'   \item gates_overtopped -\link[DSMflow]{gates_overtopped}
#' }
#'
#' \strong{Temperature Inputs}
#' \itemize{
#'   \item vernalis_temps - \link[DSMTemperature]{vernalis_temperature}
#'   \item prisoners_point_temps - \link[DSMTemperature]{prisoners_point_temperature}
#'   \item degree_days - \link[DSMTemperature]{degree_days}
#'   \item mean_egg_temp_effect - \link[DSMTemperature]{egg_temperature_effect}
#'   \item avg_temp - \link[DSMTemperature]{stream_temperature}
#'   \item avg_temp_delta - \link[DSMTemperature]{delta_temperature}
#'   \item migratory_temperature_proportion_over_20 - \link[DSMTemperature]{migratory_temperature_proportion_over_20}
#' }
#'
#' \strong{Habitat Inputs}
#' \itemize{
#'   \item spawning_habitat - \link[DSMhabitat]{fr_spawn}
#'   \item inchannel_habitat_fry - \link[DSMhabitat]{fr_fry}
#'   \item inchannel_habitat_juvenile - \link[DSMhabitat]{fr_juv}
#'   \item floodplain_habitat - \link[DSMhabitat]{fr_fp}
#'   \item weeks_flooded - \link[DSMhabitat]{weeks_flooded}
#'   \item delta_habitat - \link[DSMhabitat]{delta_habitat}
#'   \item sutter_habitat - \link[DSMhabitat]{sutter_habitat}
#'   \item yolo_habitat - \link[DSMhabitat]{yolo_habitat}
#'   \item tisdale_bypass_watershed - \link[DSMhabitat]{tisdale_bypass_watershed}
#'   \item yolo_bypass_watershed - \link[DSMhabitat]{yolo_bypass_watershed}
#'   \item south_delta_routed_watersheds - \link[DSMhabitat]{south_delta_routed_watersheds}
#'   \item prop_high_predation - \link[DSMhabitat]{prop_high_predation}
#'   \item contact_points - \link[DSMhabitat]{contact_points}
#'   \item delta_contact_points - \link[DSMhabitat]{delta_contact_points}
#'   \item delta_prop_high_predation - \link[DSMhabitat]{delta_prop_high_predation}
#'   \item prob_strand_early - \link[DSMhabitat]{prob_strand_early}
#'   \item prob_strand_late - \link[DSMhabitat]{prob_strand_late}
#'   \item prob_nest_scoured - \link[DSMhabitat]{prob_nest_scoured}
#' }
#'
#' @export
load_baseline_data <- function() {
  # DSMflow variables -----
  freeport_flows <- DSMflow::freeport_flow
  vernalis_flows <- DSMflow::vernalis_flow
  stockton_flows <- DSMflow::stockton_flow
  CVP_exports <- DSMflow::cvp_exports
  SWP_exports <- DSMflow::swp_exports
  proportion_diverted <- DSMflow::proportion_diverted
  total_diverted <- DSMflow::total_diverted
  delta_proportion_diverted <- DSMflow::delta_proportion_diverted
  delta_total_diverted <- DSMflow::delta_total_diverted
  prop_pulse_flows <- DSMflow::proportion_pulse_flows
  prop_flow_natal <- DSMflow::proportion_flow_natal
  upper_sacramento_flows <- DSMflow::upper_sacramento_flows
  delta_inflow <- DSMflow::delta_inflow
  cc_gates_days_closed <- DSMflow::delta_cross_channel_closed["count", ]
  cc_gates_prop_days_closed <- DSMflow::delta_cross_channel_closed["proportion", ]
  proportion_flow_bypass <- DSMflow::proportion_flow_bypasses
  gates_overtopped <- DSMflow::gates_overtopped

  # DSMtemperature variables -----
  vernalis_temps <- DSMtemperature::vernalis_temperature
  prisoners_point_temps <- DSMtemperature::prisoners_point_temperature
  degree_days <- DSMtemperature::degree_days
  mean_egg_temp_effect <- DSMtemperature::egg_temperature_effect$spring_run
  avg_temp <- DSMtemperature::stream_temperature
  avg_temp_delta <- DSMtemperature::delta_temperature
  migratory_temperature_proportion_over_20 <- DSMtemperature::migratory_temperature_proportion_over_20

  # DSMhabitat variables -----
  spawning_habitat <- DSMhabitat::sr_spawn
  inchannel_habitat_fry <- DSMhabitat::sr_fry
  inchannel_habitat_juvenile <- DSMhabitat::sr_juv
  floodplain_habitat <- DSMhabitat::sr_fp
  weeks_flooded <- DSMhabitat::weeks_flooded
  delta_habitat <- DSMhabitat::delta_habitat
  sutter_habitat <- DSMhabitat::sutter_habitat
  yolo_habitat <- DSMhabitat::yolo_habitat
  tisdale_bypass_watershed <- DSMhabitat::tisdale_bypass_watershed
  yolo_bypass_watershed <- DSMhabitat::yolo_bypass_watershed
  south_delta_routed_watersheds <- DSMhabitat::south_delta_routed_watersheds
  prop_high_predation <- DSMhabitat::prop_high_predation
  contact_points <- DSMhabitat::contact_points
  delta_contact_points <- DSMhabitat::delta_contact_points
  delta_prop_high_predation <- DSMhabitat::delta_prop_high_predation
  prob_strand_early <- DSMhabitat::prob_strand_early
  prob_strand_late <- DSMhabitat::prob_strand_late
  prob_nest_scoured <- DSMhabitat::prob_nest_scoured
  spring_run_pools <- ifelse(is.na(DSMhabitat::pools$SR_pools_sq_meters), 0, DSMhabitat::pools$SR_pools_sq_meters)

  list(
    freeport_flows = freeport_flows,
    vernalis_flows = vernalis_flows,
    stockton_flows = stockton_flows,
    CVP_exports = CVP_exports,
    SWP_exports = SWP_exports,
    proportion_diverted = proportion_diverted,
    total_diverted = total_diverted,
    delta_proportion_diverted = delta_proportion_diverted,
    delta_total_diverted = delta_total_diverted,
    prop_pulse_flows = prop_pulse_flows,
    prop_flow_natal = prop_flow_natal,
    upper_sacramento_flows = upper_sacramento_flows,
    delta_inflow = delta_inflow,
    cc_gates_days_closed = cc_gates_days_closed,
    cc_gates_prop_days_closed = cc_gates_prop_days_closed,
    proportion_flow_bypass = proportion_flow_bypass,
    gates_overtopped = gates_overtopped,
    vernalis_temps = vernalis_temps,
    prisoners_point_temps = prisoners_point_temps,
    degree_days = degree_days,
    mean_egg_temp_effect = mean_egg_temp_effect,
    avg_temp = avg_temp,
    avg_temp_delta = avg_temp_delta,
    migratory_temperature_proportion_over_20 = migratory_temperature_proportion_over_20,
    spawning_habitat = spawning_habitat,
    inchannel_habitat_fry = inchannel_habitat_fry,
    inchannel_habitat_juvenile = inchannel_habitat_juvenile,
    floodplain_habitat = floodplain_habitat,
    weeks_flooded = weeks_flooded,
    delta_habitat = delta_habitat,
    sutter_habitat = sutter_habitat,
    yolo_habitat = yolo_habitat,
    tisdale_bypass_watershed = tisdale_bypass_watershed,
    yolo_bypass_watershed = yolo_bypass_watershed,
    south_delta_routed_watersheds = south_delta_routed_watersheds,
    prop_high_predation = prop_high_predation,
    contact_points = contact_points,
    delta_contact_points = delta_contact_points,
    delta_prop_high_predation = delta_prop_high_predation,
    prob_strand_early = prob_strand_early,
    prob_strand_late = prob_strand_late,
    prob_nest_scoured = prob_nest_scoured,
    spring_run_pools = spring_run_pools
  )
}







#' @export
load_calibrated_data <- function() {
  base_data <- load_baseline_data()
  vect2<-c(1.4416309,1.9379344,1.3706987,1.6449355,1.4556516,0.5481166,0.7098337,0.7279391,0.8681320,
           1.3761102,1.0039699,1.9759963,1.7591008,1.4374917,0.8327904,0.6907140,1.0503587,1.3019928,
           1.1983915)
  
  # DSMflow variables -----
  freeport_flows <- DSMflow::freeport_flow
  vernalis_flows <- DSMflow::vernalis_flow
  stockton_flows <- DSMflow::stockton_flow
  CVP_exports <- DSMflow::cvp_exports
  SWP_exports <- DSMflow::swp_exports
  proportion_diverted <- DSMflow::proportion_diverted
  total_diverted <- DSMflow::total_diverted
  delta_proportion_diverted <- DSMflow::delta_proportion_diverted
  delta_total_diverted <- DSMflow::delta_total_diverted
  prop_pulse_flows <- DSMflow::proportion_pulse_flows
  prop_flow_natal <- DSMflow::proportion_flow_natal
  upper_sacramento_flows <- DSMflow::upper_sacramento_flows
  delta_inflow <- DSMflow::delta_inflow
  cc_gates_days_closed <- DSMflow::delta_cross_channel_closed["count", ]
  cc_gates_prop_days_closed <- DSMflow::delta_cross_channel_closed["proportion", ]
  proportion_flow_bypass <- DSMflow::proportion_flow_bypasses
  
  # DSMtemperature variables -----
  vernalis_temps <- DSMtemperature::vernalis_temperature
  prisoners_point_temps <- DSMtemperature::prisoners_point_temperature
  degree_days <- DSMtemperature::degree_days
  mean_egg_temp_effect <- DSMtemperature::egg_temperature_effect
  avg_temp <- DSMtemperature::stream_temperature
  avg_temp_delta <- DSMtemperature::delta_temperature
  migratory_temperature_proportion_over_20 <- DSMtemperature::migratory_temperature_proportion_over_20
  
  # DSMhabitat variables -----
  # surv.adj<-rep(1,31)
  # surv.adj[c(2,3,6,7,10,12,19,20)]<-1 
  
  spawning_habitat <- DSMhabitat::sr_spawn
  spawning_habitat[6,,] <- spawning_habitat[6,,]*vect2[1] #Butte
  spawning_habitat[10,,]<- spawning_habitat[10,,]*vect2[2] # Deer
  spawning_habitat[12,,]<- spawning_habitat[12,,]*vect2[3] # Mill
  spawning_habitat[19,,]<- spawning_habitat[19,,]*vect2[4] # Feather
  spawning_habitat[20,,]<- spawning_habitat[20,,]*vect2[5]# Yuba
  
  inchannel_habitat_fry <- DSMhabitat::sr_fry
  inchannel_habitat_fry[6,,] <- inchannel_habitat_fry[6,,]*vect2[6] # Butte
  inchannel_habitat_fry[10,,] <- inchannel_habitat_fry[10,,]*vect2[7] # Deer
  inchannel_habitat_fry[12,,] <- inchannel_habitat_fry[12,,]*vect2[8] # Mill
  inchannel_habitat_fry[16,,] <- inchannel_habitat_fry[16,,]*vect2[9] # Upper-mid Sac (corridor for above)
  inchannel_habitat_fry[19,,] <- inchannel_habitat_fry[19,,]*vect2[10] # Feather 
  inchannel_habitat_fry[20,,] <- inchannel_habitat_fry[20,,]*vect2[11] # Yuba
  inchannel_habitat_fry[21,,] <- inchannel_habitat_fry[21,,]*vect2[12] # Lower-mid Sac (corridor for above)
  inchannel_habitat_fry[24,,] <- inchannel_habitat_fry[24,,]*vect2[13] # Lower Sac (corridor for above)
  inchannel_habitat_fry[2,,]<-inchannel_habitat_fry[2,,]*vect2[14] # Antelope
  inchannel_habitat_fry[7,,]<-inchannel_habitat_fry[7,,]*vect2[15] # Clear
  
  inchannel_habitat_juvenile <- DSMhabitat::sr_juv
  inchannel_habitat_juvenile[6,,]<-inchannel_habitat_juvenile[6,,]*vect2[6] # Butte
  inchannel_habitat_juvenile[10,,]<-inchannel_habitat_juvenile[10,,]*vect2[7] # Deer
  inchannel_habitat_juvenile[12,,]<-inchannel_habitat_juvenile[12,,]*vect2[8] # Mill
  inchannel_habitat_juvenile[16,,]<-inchannel_habitat_juvenile[16,,]*vect2[9] # Upper-mid Sac (corridor for above)
  inchannel_habitat_juvenile[19,,]<-inchannel_habitat_juvenile[19,,]*vect2[10] # Feather 
  inchannel_habitat_juvenile[20,,]<-inchannel_habitat_juvenile[20,,]*vect2[11] # Yuba
  inchannel_habitat_juvenile[21,,]<-inchannel_habitat_juvenile[21,,]*vect2[12] # Lower-mid Sac (corridor for above)
  inchannel_habitat_juvenile[24,,]<-inchannel_habitat_juvenile[24,,]*vect2[13] # Lower Sac (corridor for above)
  inchannel_habitat_juvenile[2,,]<-inchannel_habitat_juvenile[2,,]*vect2[14] # Antelope
  inchannel_habitat_juvenile[7,,]<-inchannel_habitat_juvenile[7,,]*vect2[15] # Clear
  
  floodplain_habitat <- DSMhabitat::sr_fp
  weeks_flooded <- DSMhabitat::weeks_flooded
  delta_habitat <- DSMhabitat::delta_habitat
  delta_habitat[,,1]<-delta_habitat[,,1]*vect2[18]
  delta_habitat[,,2]<-delta_habitat[,,2]*vect2[19]
  
  sutter_habitat <- DSMhabitat::sutter_habitat * vect2[16]
  yolo_habitat <- DSMhabitat::yolo_habitat * vect2[17]
  tisdale_bypass_watershed <- DSMhabitat::tisdale_bypass_watershed
  yolo_bypass_watershed <- DSMhabitat::yolo_bypass_watershed
  south_delta_routed_watersheds <- DSMhabitat::south_delta_routed_watersheds
  prop_high_predation <- DSMhabitat::prop_high_predation
  contact_points <- DSMhabitat::contact_points
  delta_contact_points <- DSMhabitat::delta_contact_points
  delta_prop_high_predation <- DSMhabitat::delta_prop_high_predation
  prob_strand_early <- DSMhabitat::prob_strand_early
  prob_strand_late <- DSMhabitat::prob_strand_late
  prob_nest_scoured <- DSMhabitat::prob_nest_scoured
  spring_run_pools <- ifelse(is.na(DSMhabitat::pools$SR_pools_sq_meters), 0, DSMhabitat::pools$SR_pools_sq_meters)
  
  list(
    freeport_flows = freeport_flows,
    vernalis_flows = vernalis_flows,
    stockton_flows = stockton_flows,
    CVP_exports = CVP_exports,
    SWP_exports = SWP_exports,
    proportion_diverted = proportion_diverted,
    total_diverted = total_diverted,
    delta_proportion_diverted = delta_proportion_diverted,
    delta_total_diverted = delta_total_diverted,
    prop_pulse_flows = prop_pulse_flows,
    prop_flow_natal = prop_flow_natal,
    upper_sacramento_flows = upper_sacramento_flows,
    delta_inflow = delta_inflow,
    cc_gates_days_closed = cc_gates_days_closed,
    cc_gates_prop_days_closed = cc_gates_prop_days_closed,
    proportion_flow_bypass = proportion_flow_bypass,
    vernalis_temps = vernalis_temps,
    prisoners_point_temps = prisoners_point_temps,
    degree_days = degree_days,
    mean_egg_temp_effect = mean_egg_temp_effect,
    avg_temp = avg_temp,
    avg_temp_delta = avg_temp_delta,
    migratory_temperature_proportion_over_20 = migratory_temperature_proportion_over_20,
    spawning_habitat = spawning_habitat,
    inchannel_habitat_fry = inchannel_habitat_fry,
    inchannel_habitat_juvenile = inchannel_habitat_juvenile,
    floodplain_habitat = floodplain_habitat,
    weeks_flooded = weeks_flooded,
    delta_habitat = delta_habitat,
    sutter_habitat = sutter_habitat,
    yolo_habitat = yolo_habitat,
    tisdale_bypass_watershed = tisdale_bypass_watershed,
    yolo_bypass_watershed = yolo_bypass_watershed,
    south_delta_routed_watersheds = south_delta_routed_watersheds,
    prop_high_predation = prop_high_predation,
    contact_points = contact_points,
    delta_contact_points = delta_contact_points,
    delta_prop_high_predation = delta_prop_high_predation,
    prob_strand_early = prob_strand_early,
    prob_strand_late = prob_strand_late,
    prob_nest_scoured = prob_nest_scoured,
    spring_run_pools = spring_run_pools
  )
}


# create a load data for the inputs that were used in the OG model
#' @export
load_2019_baseline_data <- function() {
  vect2<-c(1.4416309,1.9379344,1.3706987,1.6449355,1.4556516,0.5481166,0.7098337,0.7279391,0.8681320,
           1.3761102,1.0039699,1.9759963,1.7591008,1.4374917,0.8327904,0.6907140,1.0503587,1.3019928,
           1.1983915)
  
  current_data <- load_baseline_data()
  
  current_data$spawning_habitat <- baseline_2019$IChab.spawn
  current_data$inchannel_habitat_fry <- baseline_2019$IChab.fry
  current_data$inchannel_habitat_juvenile <- baseline_2019$IChab.juv
  
  current_data$spawning_habitat[6,,] <- current_data$spawning_habitat[6,,]*vect2[1] #Butte
  current_data$spawning_habitat[10,,]<- current_data$spawning_habitat[10,,]*vect2[2] # Deer
  current_data$spawning_habitat[12,,]<- current_data$spawning_habitat[12,,]*vect2[3] # Mill
  current_data$spawning_habitat[19,,]<- current_data$spawning_habitat[19,,]*vect2[4] # Feather
  current_data$spawning_habitat[20,,]<- current_data$spawning_habitat[20,,]*vect2[5]# Yuba

  current_data$inchannel_habitat_fry[6,,] <- current_data$inchannel_habitat_fry[6,,]*vect2[6] # Butte
  current_data$inchannel_habitat_fry[10,,] <- current_data$inchannel_habitat_fry[10,,]*vect2[7] # Deer
  current_data$inchannel_habitat_fry[12,,] <- current_data$inchannel_habitat_fry[12,,]*vect2[8] # Mill
  current_data$inchannel_habitat_fry[16,,] <- current_data$inchannel_habitat_fry[16,,]*vect2[9] # Upper-mid Sac (corridor for above)
  current_data$inchannel_habitat_fry[19,,] <- current_data$inchannel_habitat_fry[19,,]*vect2[10] # Feather 
  current_data$inchannel_habitat_fry[20,,] <- current_data$inchannel_habitat_fry[20,,]*vect2[11] # Yuba
  current_data$inchannel_habitat_fry[21,,] <- current_data$inchannel_habitat_fry[21,,]*vect2[12] # Lower-mid Sac (corridor for above)
  current_data$inchannel_habitat_fry[24,,] <- current_data$inchannel_habitat_fry[24,,]*vect2[13] # Lower Sac (corridor for above)
  current_data$inchannel_habitat_fry[2,,]<- current_data$inchannel_habitat_fry[2,,]*vect2[14] # Antelope
  current_data$inchannel_habitat_fry[7,,]<- current_data$inchannel_habitat_fry[7,,]*vect2[15] # Clear
  
  current_data$inchannel_habitat_juvenile[6,,] <- current_data$inchannel_habitat_juvenile[6,,]*vect2[6] # Butte
  current_data$inchannel_habitat_juvenile[10,,] <- current_data$inchannel_habitat_juvenile[10,,]*vect2[7] # Deer
  current_data$inchannel_habitat_juvenile[12,,] <- current_data$inchannel_habitat_juvenile[12,,]*vect2[8] # Mill
  current_data$inchannel_habitat_juvenile[16,,] <- current_data$inchannel_habitat_juvenile[16,,]*vect2[9] # Upper-mid Sac (corridor for above)
  current_data$inchannel_habitat_juvenile[19,,] <- current_data$inchannel_habitat_juvenile[19,,]*vect2[10] # Feather 
  current_data$inchannel_habitat_juvenile[20,,] <- current_data$inchannel_habitat_juvenile[20,,]*vect2[11] # Yuba
  current_data$inchannel_habitat_juvenile[21,,] <- current_data$inchannel_habitat_juvenile[21,,]*vect2[12] # Lower-mid Sac (corridor for above)
  current_data$inchannel_habitat_juvenile[24,,] <- current_data$inchannel_habitat_juvenile[24,,]*vect2[13] # Lower Sac (corridor for above)
  current_data$inchannel_habitat_juvenile[2,,] <- current_data$inchannel_habitat_juvenile[2,,]*vect2[14] # Antelope
  current_data$inchannel_habitat_juvenile[7,,] <- current_data$inchannel_habitat_juvenile[7,,]*vect2[15] # Clear
  
  return(current_data)
}























