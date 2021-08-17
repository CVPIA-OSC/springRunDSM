#' @title Spring Run Chinook Model
#' @description Spring Run Chinook life cycle model used for CVPIA's Structured
#' Decision Making Process
#' @param scenario Model inputs, can be modified to test management actions
#' @param mode The mode to run model in. Can be \code{"seed"}, \code{"simulate"}, \code{"calibrate"}
#' @param seeds The default value is NULL runs the model in seeding mode,
#' returning a 31 by 25 matrix with the first four years of seeded adults. This
#' returned value can be fed into the model again as the value for the seeds argument
#' @param ..params Parameters for model and submodels
#' @source IP-117068
#' @examples
#' spring_run_seeds <- springRunDSM::spring_run_model(mode = "seed")
#' springRunDSM::spring_run_model(scenario = DSMscenario::scenarios$ONE,
#'                            mode = "simulate",
#'                            seeds = spring_run_seeds)
#' @export
spring_run_model <- function(scenario = NULL, mode = c("seed", "simulate", "calibrate"),
                             seeds = NULL, ..params = springRunDSM::params, stochastic = FALSE){
  
  mode <- match.arg(mode)
  
  if (mode == "simulate") {
    if (is.null(scenario)) {
      # the do nothing scenario to force habitat degradation
      scenario <- DSMscenario::scenarios$NO_ACTION
    }
    
    habitats <- list(
      spawning_habitat = ..params$spawning_habitat,
      inchannel_habitat_fry = ..params$inchannel_habitat_fry,
      inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
      floodplain_habitat = ..params$floodplain_habitat,
      weeks_flooded = ..params$weeks_flooded
    )
    
    scenario_data <- DSMscenario::load_scenario(scenario,
                                                habitat_inputs = habitats,
                                                species = DSMscenario::species$SPRING_RUN)
    
    ..params$spawning_habitat <- scenario_data$spawning_habitat
    ..params$inchannel_habitat_fry <- scenario_data$inchannel_habitat_fry
    ..params$inchannel_habitat_juvenile <- scenario_data$inchannel_habitat_juvenile
    ..params$floodplain_habitat <- scenario_data$floodplain_habitat
    ..params$weeks_flooded <- scenario_data$weeks_flooded
    
  }
  
  if (mode == "calibrate") {
    scenario_data <- list(
      survival_adjustment = matrix(1, nrow = 31, ncol = 21,
                                   dimnames = list(DSMscenario::watershed_labels,
                                                   1980:2000)))
  }
  
  output <- list(
    
    # SIT METRICS
    spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(springRunDSM::watershed_labels, 1:20)),
    juvenile_biomass = matrix(0, nrow = 31, ncol = 20, dimnames = list(springRunDSM::watershed_labels, 1:20)),
    proportion_natural = matrix(NA_real_, nrow = 31, ncol = 20, dimnames = list(springRunDSM::watershed_labels, 1:20))
  )
  
  if (mode == 'calibrate') {
    calculated_adults <- matrix(0, nrow = 31, ncol = 30)
  }
  
  adults <- switch (mode,
                    "seed" = springRunDSM::adult_seeds,
                    "simulate" = seeds,
                    "calibrate" = seeds,
  )
  
  simulation_length <- switch(mode,
                              "seed" = 5,
                              "simulate" = 20,
                              "calibrate" = 19)
  
  yearlings <- matrix(0, ncol = 4, nrow = 31, dimnames = list(springRunDSM::watershed_labels, size_class_labels))
  
  for (year in 1:simulation_length) {
    
    adults_in_ocean <- numeric(31)
    lower_mid_sac_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(springRunDSM::watershed_labels[1:20], springRunDSM::size_class_labels))
    lower_sac_fish <- matrix(0, nrow = 27, ncol = 4, dimnames = list(springRunDSM::watershed_labels[1:27], springRunDSM::size_class_labels))
    upper_mid_sac_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(springRunDSM::watershed_labels[1:15], springRunDSM::size_class_labels))
    sutter_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(springRunDSM::watershed_labels[1:15], springRunDSM::size_class_labels))
    yolo_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(springRunDSM::watershed_labels[1:20], springRunDSM::size_class_labels))
    san_joaquin_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(springRunDSM::watershed_labels[28:30], springRunDSM::size_class_labels))
    north_delta_fish <- matrix(0, nrow = 23, ncol = 4, dimnames = list(springRunDSM::watershed_labels[1:23], springRunDSM::size_class_labels))
    south_delta_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(springRunDSM::watershed_labels, springRunDSM::size_class_labels))
    juveniles_at_chipps <- matrix(0, nrow = 31, ncol = 4, dimnames = list(springRunDSM::watershed_labels, springRunDSM::size_class_labels))
    
    avg_ocean_transition_month <- ocean_transition_month(stochastic = stochastic) # 2
    
    hatch_adults <- if (stochastic) {
      rmultinom(1, size = round(runif(1, 4588.097,8689.747)), prob = ..params$hatchery_allocation)[ , 1]
    } else {
      mean(c(4829.885,4588.097,8689.747)) * ..params$hatchery_allocation
    }

    # if (year == 10) {
    #   # print('before')
    #   x <- adults
    #   # print('----')
    #   # print('after')
    #   adults <- structure(c(0, 0.412206818021207, 339.840205578755, 0, 0, 12986.9512027071,
    #                         788.565428394236, 0, 0, 490.66567240937, 0, 691.261776039144,
    #                         0, 0, 0, 0, 0, 0, 1441.37510273575, 78.8704216451964, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0.353758008394228, 266.285294303453,
    #                         0, 0, 11826.9920127831, 731.507223700282, 0, 0, 379.015044535337,
    #                         0, 609.216493146242, 0, 0, 0, 0, 0, 0, 2053.54409903886, 76.1843323880321,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0249678219094437, 1.77757998134365,
    #                         262.980291636751, 0, 0.0302795493496409, 9824.94524392232, 517.907403454582,
    #                         0.0497810823450009, 0, 326.215902261902, 0, 472.04095165171,
    #                         0, 0, 0.0034054213410213, 0, 0, 0, 943.280397324885, 60.9896304356331,
    #                         0, 0, 0, 0, 0, 0, 4.84738359352476e-09, 0, 0.000108418777840923,
    #                         0.000221295584015599, 0, 0.0717950294018173, 5.92181595860771,
    #                         343.170645291502, 0, 0.0958273023903902, 9255.69966094654, 418.37823423485,
    #                         0.33384635412495, 0, 345.700906276215, 0, 675.449913169659, 0,
    #                         0, 0.0197219492324818, 0, 0, 0, 243.897603877153, 53.1565624074762,
    #                         0, 0, 0, 0, 0, 0, 2.49466583536748e-06, 0, 0.000281408719487314,
    #                         0.000597127361157015, 0, 0.097526445258113, 8.6487247615538,
    #                         415.749549874421, 0, 0.144501184540779, 11421.1531587621, 629.9250377396,
    #                         0.539517151863436, 0, 408.771296622658, 0, 915.011253255, 0,
    #                         0, 0.0330265421192939, 0, 0, 0, 740.94506712251, 77.5065265557278,
    #                         0, 0, 0, 0, 0, 0, 1.04244730070789e-05, 0, 0.0021662372932061,
    #                         0.00206379127074066, 0, 0.0989938866607274, 7.35955041062589,
    #                         328.855458812173, 0, 0.135395518574438, 13254.6624081894, 777.904453967467,
    #                         0.402771674513092, 0, 345.751166083255, 0, 817.585737817533,
    #                         0, 0, 0.0266449203476908, 0, 0, 0, 2034.02659387455, 90.5062653877437,
    #                         0, 0, 0, 0, 0, 0, 1.55083144019444e-05, 0, 0.00543402512349716,
    #                         0.00412331291562988, 0, 0.0712045872073951, 6.58057833750118,
    #                         245.943607476619, 0, 0.0790054188243461, 12593.2189135428, 713.089592506401,
    #                         0.277075086543319, 0, 281.38048376744, 0, 747.970887632527, 0,
    #                         0, 0.0169335694083625, 0, 0, 0, 2763.49148818956, 87.5205771463118,
    #                         0, 0, 0, 0, 0, 0, 1.24057478518488e-05, 0, 0.00538667662072991,
    #                         0.00343220921307866, 0, 0.0613417723605576, 7.72921041214768,
    #                         230.473617035424, 0, 0.0923843460409767, 11068.1899783342, 599.775255605578,
    #                         0.159938742082891, 0, 271.204552693231, 0, 587.716308157684,
    #                         0, 0, 0.0135326813598808, 0, 0, 0, 1278.0034398998, 118.840044887515,
    #                         0, 0, 0, 0, 0, 0, 3.21718846869839e-05, 0, 0.00300606456329619,
    #                         0.00126817364885796, 0, 0.0796893937039662, 8.36612435398589,
    #                         222.936433993791, 0, 0.147390852145772, 10210.3088246216, 548.573343656489,
    #                         0.131241240238088, 0, 238.818413570468, 0, 524.717941981775,
    #                         0, 0, 0.0138793999195346, 0, 0, 0, 80.2609773836419, 157.939767372466,
    #                         0, 0, 0, 0, 0, 0, 5.19779051020262e-05, 0, 0.00317501463605501,
    #                         0.000846091513995383, 0, 0.059448118040562, 7.47945087973858,
    #                         240.85939646228, 0, 0.102444525733284, 9939.06056898501, 531.393011476826,
    #                         0.263212438950806, 0, 191.838883392242, 0, 613.055450766982,
    #                         0, 0, 0.0110832895231136, 0, 0, 0, 55.7341666445423, 129.944540818217,
    #                         0, 0, 0, 0, 0, 0, 2.46449331505261e-05, 0, 0.00454707321018138,
    #                         0.00100866903039233, 0, 0.034021746327719, 6.48866157058948,
    #                         232.090483365504, 0, 0.044455067953902, 9773.26114624425, 496.894071651839,
    #                         0.25404721131521, 0, 164.09856311948, 0, 504.147450779261, 0,
    #                         0, 0.00683029641442697, 0, 0, 0, 27.484407720225, 66.6548519146172,
    #                         0, 0, 0, 0, 0, 0, 7.3033878473891e-09, 0, 0.00438621553168578,
    #                         0.000506408166413379, 0, 0.0197526847090924, 5.10668283291934,
    #                         147.786094913801, 0, 0.0318413683202997, 7053.24953694085, 365.100410390096,
    #                         0.096108005217115, 0, 130.944527705102, 0, 244.325540182907,
    #                         0, 0, 0.0041208984268143, 0, 0, 0, 15.0379389142453, 28.0159032855585,
    #                         0, 0, 0, 0, 0, 0, 5.55969042409515e-10, 0, 0.00296423866913814,
    #                         0.00010951167807249, 0, 0.00392149676611984, 1.91411812248536,
    #                         42.8151479235746, 0, 0.0122609882449482, 2220.78022706505, 123.733479437457,
    #                         0.00421544258380007, 0, 48.7226708981606, 0, 50.6222605079392,
    #                         0, 0, 0.00102850968734212, 0, 0, 0, 2.25392696129825, 7.60177746375126,
    #                         0, 0, 0, 0, 0, 0, 7.73114695514036e-11, 0, 0.00101024742608245,
    #                         3.69988858659869e-05, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    #                         0, 0, 0), .Dim = c(31L, 25L))
    #   # print(adults)
    # }

    spawners <- get_spawning_adults(year, adults, hatch_adults, mode = mode,
                                    month_return_proportions = ..params$month_return_proportions,
                                    prop_flow_natal = ..params$prop_flow_natal,
                                    south_delta_routed_watersheds = ..params$south_delta_routed_watersheds,
                                    cc_gates_days_closed = ..params$cc_gates_days_closed,
                                    gates_overtopped = ..params$gates_overtopped,
                                    tisdale_bypass_watershed = ..params$tisdale_bypass_watershed,
                                    yolo_bypass_watershed = ..params$yolo_bypass_watershed,
                                    migratory_temperature_proportion_over_20 = ..params$migratory_temperature_proportion_over_20,
                                    ..surv_adult_enroute_int = ..params$..surv_adult_enroute_int,
                                    .adult_stray_intercept = ..params$.adult_stray_intercept,
                                    .adult_stray_wild = ..params$.adult_stray_wild,
                                    .adult_stray_natal_flow = ..params$.adult_stray_natal_flow,
                                    .adult_stray_cross_channel_gates_closed = ..params$.adult_stray_cross_channel_gates_closed,
                                    .adult_stray_prop_bay_trans = ..params$.adult_stray_prop_bay_trans,
                                    .adult_stray_prop_delta_trans = ..params$.adult_stray_prop_delta_trans,
                                    .adult_en_route_migratory_temp = ..params$.adult_en_route_migratory_temp,
                                    .adult_en_route_bypass_overtopped = ..params$.adult_en_route_bypass_overtopped,
                                    .adult_en_route_adult_harvest_rate = ..params$.adult_en_route_adult_harvest_rate,
                                    stochastic = stochastic)
    
    init_adults <- spawners$init_adults

    output$spawners[ , year] <- init_adults
    output$proportion_natural[ , year] <- spawners$proportion_natural
    
    egg_to_fry_surv <- surv_egg_to_fry(
      proportion_natural = spawners$proportion_natural,
      scour = ..params$prob_nest_scoured,
      temperature_effect = ..params$mean_egg_temp_effect,
      .proportion_natural = ..params$.surv_egg_to_fry_proportion_natural,
      .scour = ..params$.surv_egg_to_fry_scour,
      ..surv_egg_to_fry_int = ..params$..surv_egg_to_fry_int
    )
    
    min_spawn_habitat <- apply(..params$spawning_habitat[ , 3:6, year], 1, min)
    
    #TODO double check this calculation compared to theirs
    accumulated_degree_days <- cbind(march = rowSums(..params$degree_days[ , 3:6, year]),
                                     april = rowSums(..params$degree_days[ , 4:6, year]),
                                     may = rowSums(..params$degree_days[ , 5:6, year]),
                                     june = ..params$degree_days[ , 6, year])
    
    average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, ..params$month_return_proportions)
    
    prespawn_survival <- surv_adult_prespawn(average_degree_days,
                                             ..surv_adult_prespawn_int = ..params$..surv_adult_prespawn_int,
                                             .deg_day = ..params$.adult_prespawn_deg_day)
    
    # Apply SR pools logic
    init_adults <- if (stochastic) {
      rbinom(31, round(init_adults), prespawn_survival)
    } else {
      init_adults * prespawn_survival
    }
    
    # spring run above capacity die, capacity based on spring run pools
    # TODO capacity applied incorrectly here
    init_adults <- ifelse(init_adults >= ..params$spring_run_pools, 
                          ..params$spring_run_pools, 
                          init_adults)
    
    # Holding period for spring run
    # apply degree days and prespawn survival
    holding_split <- if (stochastic) {
      rbinom(31, init_adults, 0.5) / init_adults
    } else {
      init_adults * 0.5 / init_adults
    }
    
    average_degree_days <- rowSums(params$degree_days[ , 7:10, year]) * (1 - holding_split) + 
      rowSums(params$degree_days[ , 7:9, year]) * holding_split
    
    prespawn_survival <- surv_adult_prespawn(average_degree_days,
                                             ..surv_adult_prespawn_int = ..params$..surv_adult_prespawn_int,
                                             .deg_day = ..params$.adult_prespawn_deg_day)
    
    juveniles <- spawn_success(escapement = init_adults,
                               adult_prespawn_survival = prespawn_survival,
                               egg_to_fry_survival = egg_to_fry_surv,
                               prob_scour = ..params$prob_nest_scoured,
                               spawn_habitat = min_spawn_habitat,
                               sex_ratio = ..params$spawn_success_sex_ratio,
                               redd_size = ..params$spawn_success_redd_size,
                               fecundity = ..params$spawn_success_fecundity,
                               stochastic = stochastic)
    
    for (month in c(11, 12, 1:5)) {
      if (month %in% 1:5) juv_dynamics_year <- year + 1 else juv_dynamics_year <- year
      habitat <- get_habitat(juv_dynamics_year, month,
                             inchannel_habitat_fry = ..params$inchannel_habitat_fry,
                             inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
                             floodplain_habitat = ..params$floodplain_habitat,
                             sutter_habitat = ..params$sutter_habitat,
                             yolo_habitat = ..params$yolo_habitat,
                             sutter_floodplain_habitat = ..params$sutter_floodplain_habitat,
                             yolo_floodplain_habitat = ..params$yolo_floodplain_habitat,
                             delta_habitat = ..params$delta_habitat)
      
      rearing_survival <- get_rearing_survival(juv_dynamics_year, month,
                                               survival_adjustment = scenario_data$survival_adjustment,
                                               mode = mode,
                                               avg_temp = ..params$avg_temp,
                                               avg_temp_delta = ..params$avg_temp_delta,
                                               prob_strand_early = ..params$prob_strand_early,
                                               prob_strand_late = ..params$prob_strand_late,
                                               proportion_diverted = ..params$proportion_diverted,
                                               total_diverted = ..params$total_diverted,
                                               delta_proportion_diverted = ..params$delta_proportion_diverted,
                                               delta_total_diverted = ..params$delta_total_diverted,
                                               weeks_flooded = ..params$weeks_flooded,
                                               prop_high_predation = ..params$prop_high_predation,
                                               contact_points = ..params$contact_points,
                                               delta_contact_points = ..params$delta_contact_points,
                                               delta_prop_high_predation = ..params$delta_prop_high_predation,
                                               ..surv_juv_rear_int = ..params$..surv_juv_rear_int,
                                               .surv_juv_rear_contact_points = ..params$.surv_juv_rear_contact_points,
                                               ..surv_juv_rear_contact_points = ..params$..surv_juv_rear_contact_points,
                                               .surv_juv_rear_prop_diversions = ..params$.surv_juv_rear_prop_diversions,
                                               ..surv_juv_rear_prop_diversions = ..params$..surv_juv_rear_prop_diversions,
                                               .surv_juv_rear_total_diversions = ..params$.surv_juv_rear_total_diversions,
                                               ..surv_juv_rear_total_diversions = ..params$..surv_juv_rear_total_diversions,
                                               ..surv_juv_bypass_int = ..params$..surv_juv_bypass_int,
                                               ..surv_juv_delta_int = ..params$..surv_juv_delta_int,
                                               .surv_juv_delta_contact_points = ..params$.surv_juv_delta_contact_points,
                                               ..surv_juv_delta_contact_points = ..params$..surv_juv_delta_contact_points,
                                               .surv_juv_delta_total_diverted = ..params$.surv_juv_delta_total_diverted,
                                               ..surv_juv_delta_total_diverted = ..params$..surv_juv_delta_total_diverted,
                                               .surv_juv_rear_avg_temp_thresh = ..params$.surv_juv_rear_avg_temp_thresh,
                                               .surv_juv_rear_high_predation = ..params$.surv_juv_rear_high_predation,
                                               .surv_juv_rear_stranded = ..params$.surv_juv_rear_stranded,
                                               .surv_juv_rear_medium = ..params$.surv_juv_rear_medium,
                                               .surv_juv_rear_large = ..params$.surv_juv_rear_large,
                                               .surv_juv_rear_floodplain = ..params$.surv_juv_rear_floodplain,
                                               .surv_juv_bypass_avg_temp_thresh = ..params$.surv_juv_bypass_avg_temp_thresh,
                                               .surv_juv_bypass_high_predation = ..params$.surv_juv_bypass_high_predation,
                                               .surv_juv_bypass_medium = ..params$.surv_juv_bypass_medium,
                                               .surv_juv_bypass_large = ..params$.surv_juv_bypass_large,
                                               .surv_juv_bypass_floodplain = ..params$.surv_juv_bypass_floodplain,
                                               .surv_juv_delta_avg_temp_thresh = ..params$.surv_juv_delta_avg_temp_thresh,
                                               .surv_juv_delta_high_predation = ..params$.surv_juv_delta_high_predation,
                                               .surv_juv_delta_prop_diverted = ..params$.surv_juv_delta_prop_diverted,
                                               .surv_juv_delta_medium = ..params$.surv_juv_delta_medium,
                                               .surv_juv_delta_large = ..params$.surv_juv_delta_large,
                                               min_survival_rate = ..params$min_survival_rate,
                                               stochastic = stochastic)
      
      migratory_survival <- get_migratory_survival(juv_dynamics_year, month,
                                                   cc_gates_prop_days_closed = ..params$cc_gates_prop_days_closed,
                                                   freeport_flows = ..params$freeport_flows,
                                                   vernalis_flows = ..params$vernalis_flows,
                                                   stockton_flows = ..params$stockton_flows,
                                                   vernalis_temps = ..params$vernalis_temps,
                                                   prisoners_point_temps = ..params$prisoners_point_temps,
                                                   CVP_exports = ..params$CVP_exports,
                                                   SWP_exports = ..params$SWP_exports,
                                                   upper_sacramento_flows = ..params$upper_sacramento_flows,
                                                   delta_inflow = ..params$delta_inflow,
                                                   avg_temp_delta = ..params$avg_temp_delta,
                                                   avg_temp = ..params$avg_temp,
                                                   total_diverted = ..params$total_diverted,
                                                   proportion_diverted = ..params$proportion_diverted,
                                                   delta_proportion_diverted = ..params$delta_proportion_diverted,
                                                   .surv_juv_outmigration_sac_delta_intercept_one = ..params$.surv_juv_outmigration_sac_delta_intercept_one,
                                                   .surv_juv_outmigration_sac_delta_intercept_two = ..params$.surv_juv_outmigration_sac_delta_intercept_two,
                                                   .surv_juv_outmigration_sac_delta_intercept_three = ..params$.surv_juv_outmigration_sac_delta_intercept_three,
                                                   .surv_juv_outmigration_sac_delta_delta_flow = ..params$.surv_juv_outmigration_sac_delta_delta_flow,
                                                   .surv_juv_outmigration_sac_delta_avg_temp = ..params$.surv_juv_outmigration_sac_delta_avg_temp,
                                                   .surv_juv_outmigration_sac_delta_perc_diversions = ..params$.surv_juv_outmigration_sac_delta_perc_diversions,
                                                   .surv_juv_outmigration_sac_delta_medium = ..params$.surv_juv_outmigration_sac_delta_medium,
                                                   .surv_juv_outmigration_sac_delta_large = ..params$.surv_juv_outmigration_sac_delta_large,
                                                   ..surv_juv_outmigration_sj_int = ..params$..surv_juv_outmigration_sj_int,
                                                   ..surv_juv_outmigration_sac_int_one = ..params$..surv_juv_outmigration_sac_int_one,
                                                   ..surv_juv_outmigration_sac_prop_diversions = ..params$..surv_juv_outmigration_sac_prop_diversions,
                                                   ..surv_juv_outmigration_sac_total_diversions = ..params$..surv_juv_outmigration_sac_total_diversions,
                                                   ..surv_juv_outmigration_sac_int_two = ..params$..surv_juv_outmigration_sac_int_two,
                                                   .surv_juv_outmigration_san_joaquin_medium = ..params$.surv_juv_outmigration_san_joaquin_medium,
                                                   .surv_juv_outmigration_san_joaquin_large = ..params$.surv_juv_outmigration_san_joaquin_large,
                                                   min_survival_rate = ..params$min_survival_rate,
                                                   surv_juv_outmigration_sac_delta_model_weights = ..params$surv_juv_outmigration_sac_delta_model_weights,
                                                   stochastic = stochastic)
      
      migrants <- matrix(0, nrow = 31, ncol = 4, dimnames = list(springRunDSM::watershed_labels, springRunDSM::size_class_labels))
      ## TODO check/refactor yearling dynamics
      if (month == 5) {
        # yearling logic here
        # 1 - 15, 18-20, 23, 25:30
        yearlings[c(1:15, 18:20, 23, 25:30), 1:2] <- juveniles[c(1:15, 18:20, 23, 25:30), 1:2]
        juveniles[c(1:15, 18:20, 23, 25:30), 1:2] <- 0 # set all to zero since they are yearlings now
        # all remaining fish outmigrate
        if(any(yearlings > 0)) browser()
        
        migrants <- juveniles
        
        sutter_fish <- migrate(sutter_fish, migratory_survival$sutter, stochastic = stochastic)
        upper_mid_sac_fish <- migrate(upper_mid_sac_fish + migrants[1:15, ], migratory_survival$uppermid_sac, stochastic = stochastic)
        migrants[1:15, ] <- upper_mid_sac_fish + sutter_fish
        
        lower_mid_sac_fish <- migrate(lower_mid_sac_fish + migrants[1:20, ], migratory_survival$lowermid_sac, stochastic = stochastic)
        yolo_fish <- migrate(yolo_fish, migratory_survival$yolo, stochastic = stochastic)
        migrants[1:20, ] <- lower_mid_sac_fish + yolo_fish
        
        lower_sac_fish <- migrate(lower_sac_fish + migrants[1:27, ], migratory_survival$lower_sac, stochastic = stochastic)
        
        san_joaquin_fish <- migrate(migrants[28:30, ] + san_joaquin_fish, migratory_survival$san_joaquin, stochastic = stochastic)
        migrants[28:30, ] <- san_joaquin_fish
        
        delta_fish <- route_and_rear_deltas(year = juv_dynamics_year, month = month,
                                            migrants = round(migrants),
                                            north_delta_fish = north_delta_fish,
                                            south_delta_fish = south_delta_fish,
                                            north_delta_habitat = habitat$north_delta,
                                            south_delta_habitat = habitat$south_delta,
                                            freeport_flows = ..params$freeport_flows,
                                            cc_gates_days_closed = ..params$cc_gates_days_closed,
                                            rearing_survival_delta = rearing_survival$delta,
                                            migratory_survival_delta = migratory_survival$delta,
                                            migratory_survival_sac_delta = migratory_survival$sac_delta,
                                            migratory_survival_bay_delta = migratory_survival$bay_delta,
                                            juveniles_at_chipps = juveniles_at_chipps,
                                            growth_rates = ..params$growth_rates,
                                            territory_size = ..params$territory_size,
                                            stochastic = stochastic)
        
        migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate
        
      } else {
        
        if (month == 11 & year > 1) {
          # applying summer year to the yearlings and send them out to the ocean
          yearlings <- yearling_growth(year = juv_dynamics_year,
                                       yearlings = yearlings,
                                       mode = mode,
                                       survival_adjustment = scenario_data$survival_adjustment,
                                       yearling_territory_size = ..params$yearling_territory_size,
                                       inchannel_habitat_fry = ..params$inchannel_habitat_fry,
                                       inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
                                       floodplain_habitat = ..params$floodplain_habitat,
                                       avg_temp = ..params$avg_temp,
                                       prob_strand_early = ..params$prob_strand_early,
                                       prob_strand_late = ..params$prob_strand_late,
                                       proportion_diverted = ..params$proportion_diverted,
                                       total_diverted = ..params$total_diverted,
                                       weeks_flooded = ..params$weeks_flooded,
                                       prop_high_predation = ..params$prop_high_predation,
                                       contact_points = ..params$contact_points,
                                       growth_rates = ..params$growth_rates,
                                       growth_rates_floodplain = ..params$growth_rates_floodplain,
                                       ..surv_juv_rear_int = ..params$..surv_juv_rear_int,
                                       .surv_juv_rear_contact_points = ..params$.surv_juv_rear_contact_points,
                                       ..surv_juv_rear_contact_points = ..params$..surv_juv_rear_contact_points,
                                       .surv_juv_rear_prop_diversions = ..params$.surv_juv_rear_prop_diversions,
                                       ..surv_juv_rear_prop_diversions = ..params$..surv_juv_rear_prop_diversions,
                                       .surv_juv_rear_total_diversions = ..params$.surv_juv_rear_total_diversions,
                                       ..surv_juv_rear_total_diversions = ..params$..surv_juv_rear_total_diversions,
                                       .surv_juv_rear_avg_temp_thresh = ..params$.surv_juv_rear_avg_temp_thresh,
                                       .surv_juv_rear_high_predation = ..params$.surv_juv_rear_high_predation,
                                       .surv_juv_rear_stranded = ..params$.surv_juv_rear_stranded,
                                       .surv_juv_rear_medium = ..params$.surv_juv_rear_medium,
                                       .surv_juv_rear_large = ..params$.surv_juv_rear_large,
                                       .surv_juv_rear_floodplain = ..params$.surv_juv_rear_floodplain,
                                       min_survival_rate = ..params$min_survival_rate, 
                                       stochastic = stochastic)
          
          sutter_detoured <- t(sapply(1:nrow(yearlings[1:15, ]), function(i) {
            rbinom(n = 4,
                   size = round(yearlings[i, ]),
                   prob = ..params$proportion_flow_bypass[month, juv_dynamics_year, 1])
          }))
          
          yearlings_at_uppermid <- rbind(
            migrate(yearlings[1:15, ] - sutter_detoured, migratory_survival$uppermid_sac, 
                    stochastic = stochastic),
            matrix(0, ncol = 4, nrow = 2)
          )
          
          yearlings_at_sutter <- rbind(
            migrate(sutter_detoured, migratory_survival$sutter, 
                    stochastic = stochastic),
            matrix(0, ncol = 4, nrow = 2)
          )
          
          yearlings_at_uppermid <- yearlings_at_sutter + yearlings_at_uppermid
          
          yearlings_at_lowermid <- rbind(yearlings_at_uppermid, yearlings[18:20, ])
          
          yolo_detoured <- t(sapply(1:nrow(yearlings_at_lowermid), function(i) {
            rbinom(n = 4,
                   size = round(yearlings_at_lowermid[i, ]),
                   prob = ..params$proportion_flow_bypass[month, juv_dynamics_year, 2])
          }))
          
          yearlings_at_lowersac <- rbind(
            migrate(yearlings_at_lowermid - yolo_detoured, migratory_survival$lowermid_sac, 
                    stochastic = stochastic),
            matrix(0, ncol = 4, nrow = 3)
          )
          
          yearlings_at_lowersac[23, ] <- yearlings[23, ]
          
          yearlings_at_lowersac <- migrate(yearlings_at_lowersac, migratory_survival$lower_sac, 
                                           stochastic = stochastic)
          
          prop_delta_fish_entrained <- route_south_delta(freeport_flow = ..params$freeport_flows[[month, juv_dynamics_year]] * 35.3147,
                                                         dcc_closed = ..params$cc_gates_days_closed[month],
                                                         month = month)
          
          sac_not_entrained <- t(sapply(1:nrow(yearlings_at_lowersac), function(i) {
            rbinom(n = 4, yearlings_at_lowersac[i, ], prob = 1 - prop_delta_fish_entrained)
          }))
          
          yearlings_at_north_delta <- sac_not_entrained +
            rbind(migrate(yolo_detoured, migratory_survival$yolo, stochastic = stochastic), 
                  matrix(0, ncol = 4, nrow = 3))
          
          yearlings_at_north_delta <- rbind(yearlings_at_north_delta,
                                            matrix(0, ncol = 4, nrow = 8))
          
          yearlings_at_south_delta <- rbind(
            matrix(0, ncol = 4, nrow = 24), # 24 rows for north delta/sac origin fish
            yearlings[25:27, ], # delta tribs
            migrate(yearlings[28:30,], migratory_survival$san_joaquin, 
                    stochastic = stochastic),
            matrix(0, ncol = 4, nrow = 1) # SJR
          ) +
            rbind(
              yearlings_at_lowersac - sac_not_entrained,
              matrix(0, ncol = 4, nrow = 8)
            )
          
          # estimate fish at Golden Gate Bridge and Chipps Island
          yearling_holding_south_delta <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
          
          yearling_holding_south_delta[1:24, ] <- t(sapply(1:24, function(i) {
            rbinom(n = 4, size = round(yearlings_at_south_delta[i, ]), prob = migratory_survival$delta[1, ])
          }))
          
          yearling_holding_south_delta[26:27, ] <- t(sapply(26:27, function(i) {
            rbinom(n = 4, size = round(yearlings_at_south_delta[i, ]), prob = migratory_survival$delta[2, ])
          }))
          
          yearling_holding_south_delta[25, ] <- rbinom(n = 4,
                                                       yearlings_at_south_delta[25, , drop = F],
                                                       prob = migratory_survival$delta[3, ])
          
          yearling_holding_south_delta[28:31, ] <- t(sapply(28:31, function(i) {
            rbinom(n = 4, size = round(yearlings_at_south_delta[i, ]), prob = migratory_survival$delta[4, ])
          }))
          
          yearlings_out <- t(sapply(1:nrow(yearlings_at_north_delta), function(i) {
            rbinom(n = 4,
                   size = round(yearlings_at_north_delta[i, ]),
                   prob = migratory_survival$sac_delta[1, ])
          }))
          
          survived_yearlings_out <- t(sapply(1:nrow(yearlings_out), function(i) {
            rbinom(n = 4,
                   size = round(yearlings_out[i, ]),
                   prob = migratory_survival$bay_delta)
          }))
          
          survived_yearling_holding_south_delta <- t(sapply(1:nrow(yearling_holding_south_delta), function(i) {
            rbinom(n = 4,
                   size = round(yearling_holding_south_delta[i, ]),
                   prob = migratory_survival$bay_delta)
          }))
          
          yearlings_at_golden_gate <- survived_yearlings_out + survived_yearling_holding_south_delta
          
          juveniles_at_chipps <- juveniles_at_chipps + yearlings_out + yearling_holding_south_delta
          
          adults_in_ocean <- adults_in_ocean + ocean_entry_success(migrants = yearlings_at_golden_gate,
                                                                   month = 11,
                                                                   avg_ocean_transition_month = avg_ocean_transition_month, 
                                                                   stochastic = stochastic)
          
          yearlings <- matrix(0, ncol = 4, nrow = 31, dimnames = list(springRunDSM::watershed_labels, size_class_labels))
        }
        # if month < 8
        # route northern natal fish stay and rear or migrate downstream ------
        upper_sac_trib_fish <-  route(year = juv_dynamics_year,
                                      month = month,
                                      juveniles = juveniles[1:15, ],
                                      inchannel_habitat = habitat$inchannel[1:15],
                                      floodplain_habitat = habitat$floodplain[1:15],
                                      prop_pulse_flows = ..params$prop_pulse_flows[1:15, ],
                                      .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                      .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                      .pulse_movement_medium = ..params$.pulse_movement_medium,
                                      .pulse_movement_large = ..params$.pulse_movement_large,
                                      .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                      .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                      .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                      .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                      territory_size = ..params$territory_size,
                                      stochastic = stochastic)
        
        upper_sac_trib_rear <- rear(juveniles = upper_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[1:15, ],
                                    growth = ..params$growth_rates,
                                    floodplain_juveniles = upper_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[1:15, ],
                                    floodplain_growth = ..params$growth_rates_floodplain,
                                    weeks_flooded = ..params$weeks_flooded[1:15, month, juv_dynamics_year], 
                                    stochastic = stochastic)
        
        juveniles[1:15, ] <- upper_sac_trib_rear$inchannel + upper_sac_trib_rear$floodplain
        
        # route migrant fish into Upper-mid Sac Region (fish from watersheds 1:15)
        # regional fish stay and rear
        # or migrate further downstream or in sutter bypass
        
        upper_mid_sac_fish <- route_regional(month = month,
                                             year = juv_dynamics_year,
                                             migrants = upper_mid_sac_fish + upper_sac_trib_fish$migrants,
                                             inchannel_habitat = habitat$inchannel[16],
                                             floodplain_habitat = habitat$floodplain[16],
                                             prop_pulse_flows = ..params$prop_pulse_flows[16, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$uppermid_sac,
                                             proportion_flow_bypass = ..params$proportion_flow_bypass,
                                             detour = 'sutter',
                                             territory_size = ..params$territory_size,
                                             stochastic = stochastic)
        
        
        sutter_fish <- route_bypass(bypass_fish = sutter_fish + upper_mid_sac_fish$detoured,
                                    bypass_habitat = habitat$sutter,
                                    flood_habitat = habitat$floodplain_habitat_sutter,
                                    migration_survival_rate = migratory_survival$sutter,
                                    territory_size = ..params$territory_size,
                                    stochastic = stochastic)
        
        migrants[1:15, ] <- upper_mid_sac_fish$migrants + sutter_fish$migrants
        
        upper_mid_sac_fish <- rear(juveniles = upper_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[16, ],
                                   growth = ..params$growth_rates,
                                   floodplain_juveniles = upper_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[16, ],
                                   floodplain_growth = ..params$growth_rates_floodplain,
                                   weeks_flooded = rep(..params$weeks_flooded[16, month, juv_dynamics_year], nrow(upper_mid_sac_fish$inchannel)),
                                   stochastic = stochastic)
        
        upper_mid_sac_fish <- upper_mid_sac_fish$inchannel + upper_mid_sac_fish$floodplain
        
        sutter_fish <- rear(juveniles = sutter_fish$inchannel,
                            floodplain_juveniles = sutter_fish$floodplain,
                            survival_rate = rearing_survival$sutter[1,],
                            floodplain_survival_rate = rearing_survival$sutter[1,],
                            growth = ..params$growth_rates,
                            floodplain_growth = ..params$growth_rates_floodplain,
                            weeks_flooded = rep(..params$weeks_flooded[17, month, year], nrow(sutter_fish$inchannel)),
                            stochastic = stochastic)
        
        sutter_fish <- sutter_fish$inchannel + sutter_fish$floodplain
        
        # route migrant fish into Lower-mid Sac Region (fish from watersheds 18:20, and migrants from Upper-mid Sac Region)
        # regional fish stay and rear
        # or migrate further downstream  or in yolo bypass
        lower_mid_sac_trib_fish <- route(year = juv_dynamics_year,
                                         month = month,
                                         juveniles = juveniles[18:20, ],
                                         inchannel_habitat = habitat$inchannel[18:20],
                                         floodplain_habitat = habitat$floodplain[18:20],
                                         prop_pulse_flows =  ..params$prop_pulse_flows[18:20, ],
                                         .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                         .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                         .pulse_movement_medium = ..params$.pulse_movement_medium,
                                         .pulse_movement_large = ..params$.pulse_movement_large,
                                         .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                         .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                         .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                         .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                         territory_size = ..params$territory_size,
                                         stochastic = stochastic)
        
        lower_mid_sac_trib_rear <- rear(juveniles = lower_mid_sac_trib_fish$inchannel,
                                        survival_rate = rearing_survival$inchannel[18:20, ],
                                        growth = ..params$growth_rates,
                                        floodplain_juveniles = lower_mid_sac_trib_fish$floodplain,
                                        floodplain_survival_rate = rearing_survival$floodplain[18:20, ],
                                        floodplain_growth = ..params$growth_rates_floodplain,
                                        weeks_flooded = ..params$weeks_flooded[18:20, month, juv_dynamics_year],
                                        stochastic = stochastic)
        
        juveniles[18:20, ] <- lower_mid_sac_trib_rear$inchannel + lower_mid_sac_trib_rear$floodplain
        migrants[18:20, ] <- lower_mid_sac_trib_fish$migrants
        
        lower_mid_sac_fish <- route_regional(month = month,
                                             year = juv_dynamics_year,
                                             migrants = lower_mid_sac_fish + migrants[1:20, ],
                                             inchannel_habitat = habitat$inchannel[21],
                                             floodplain_habitat = habitat$floodplain[21],
                                             prop_pulse_flows = ..params$prop_pulse_flows[21, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$lowermid_sac,
                                             proportion_flow_bypass = ..params$proportion_flow_bypass,
                                             detour = 'yolo',
                                             territory_size = ..params$territory_size,
                                             stochastic = stochastic)
        
        yolo_fish <- route_bypass(bypass_fish = yolo_fish + lower_mid_sac_fish$detoured,
                                  bypass_habitat = habitat$yolo,
                                  flood_habitat = habitat$floodplain_habitat_yolo,
                                  migration_survival_rate = migratory_survival$yolo,
                                  territory_size = ..params$territory_size,
                                  stochastic = stochastic)
        
        migrants[1:20, ] <- lower_mid_sac_fish$migrants + yolo_fish$migrants
        
        lower_mid_sac_fish <- rear(juveniles = lower_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[21, ],
                                   growth = ..params$growth_rates,
                                   floodplain_juveniles = lower_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[21, ],
                                   floodplain_growth = ..params$growth_rates_floodplain,
                                   weeks_flooded = rep(..params$weeks_flooded[21, month, juv_dynamics_year], nrow(lower_mid_sac_fish$inchannel)),
                                   stochastic = stochastic)
        
        lower_mid_sac_fish <- lower_mid_sac_fish$inchannel + lower_mid_sac_fish$floodplain
        
        yolo_fish <- rear(juveniles = yolo_fish$inchannel,
                          floodplain_juveniles = yolo_fish$floodplain,
                          survival_rate = rearing_survival$yolo[1,],
                          floodplain_survival_rate = rearing_survival$yolo[1,],
                          growth = ..params$growth_rates,
                          floodplain_growth = ..params$growth_rates_floodplain,
                          weeks_flooded = rep(..params$weeks_flooded[22, month, year], nrow(yolo_fish$inchannel)),
                          stochastic = stochastic)
        yolo_fish <- yolo_fish$inchannel + yolo_fish$floodplain
        
        # route migrant fish into Lower Sac Region (fish from watershed 23, and migrants from Lower-mid Sac Region)
        # regional fish stay and rear
        # or migrate north delta
        lower_sac_trib_fish <- route(year = juv_dynamics_year,
                                     month = month,
                                     juveniles = juveniles[23, , drop = FALSE],
                                     inchannel_habitat = habitat$inchannel[23],
                                     floodplain_habitat = habitat$floodplain[23],
                                     prop_pulse_flows =  ..params$prop_pulse_flows[23, , drop = FALSE],
                                     .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                     .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                     .pulse_movement_medium = ..params$.pulse_movement_medium,
                                     .pulse_movement_large = ..params$.pulse_movement_large,
                                     .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                     .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                     .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                     .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                     territory_size = ..params$territory_size,
                                     stochastic = stochastic)
        
        lower_sac_trib_rear <- rear(juveniles = lower_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[23, , drop = FALSE],
                                    growth = ..params$growth_rates,
                                    floodplain_juveniles = lower_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[23, , drop = FALSE],
                                    floodplain_growth = ..params$growth_rates_floodplain,
                                    weeks_flooded = ..params$weeks_flooded[23, month, juv_dynamics_year],
                                    stochastic = stochastic)
        
        juveniles[23, ] <- lower_sac_trib_rear$inchannel + lower_sac_trib_rear$floodplain
        
        migrants[23, ] <- lower_sac_trib_fish$migrants
        
        lower_sac_fish <- route_regional(month = month,
                                         year = juv_dynamics_year,
                                         migrants = lower_sac_fish + migrants[1:27, ],
                                         inchannel_habitat = habitat$inchannel[24],
                                         floodplain_habitat = habitat$floodplain[24],
                                         prop_pulse_flows = ..params$prop_pulse_flows[24, , drop = FALSE],
                                         migration_survival_rate = migratory_survival$lower_sac,
                                         territory_size = ..params$territory_size,
                                         stochastic = stochastic)
        
        migrants[1:27, ] <- lower_sac_fish$migrants
        
        lower_sac_fish <- rear(juveniles = lower_sac_fish$inchannel,
                               survival_rate = rearing_survival$inchannel[24, ],
                               growth = ..params$growth_rates,
                               floodplain_juveniles = lower_sac_fish$floodplain,
                               floodplain_survival_rate = rearing_survival$floodplain[24, ],
                               floodplain_growth = ..params$growth_rates_floodplain,
                               weeks_flooded = rep(..params$weeks_flooded[24, month, juv_dynamics_year], nrow(lower_sac_fish$inchannel)),
                               stochastic = stochastic)
        
        lower_sac_fish <- lower_sac_fish$inchannel + lower_sac_fish$floodplain
        
        # route southern natal fish stay and rear or migrate downstream ------
        
        # route migrant fish into South Delta Region (fish from watersheds 25:27)
        # regional fish stay and rear
        # or migrate to south delta
        south_delta_trib_fish <- route(year = juv_dynamics_year,
                                       month = month,
                                       juveniles = juveniles[25:27, ],
                                       inchannel_habitat = habitat$inchannel[25:27],
                                       floodplain_habitat = habitat$floodplain[25:27],
                                       prop_pulse_flows =  ..params$prop_pulse_flows[25:27, ],
                                       .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                       .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                       .pulse_movement_medium = ..params$.pulse_movement_medium,
                                       .pulse_movement_large = ..params$.pulse_movement_large,
                                       .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                       .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                       .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                       .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                       territory_size = ..params$territory_size,
                                       stochastic = stochastic)
        
        south_delta_trib_rear <- rear(juveniles = south_delta_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[25:27, ],
                                      growth = ..params$growth_rates,
                                      floodplain_juveniles = south_delta_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[25:27, ],
                                      floodplain_growth = ..params$growth_rates_floodplain,
                                      weeks_flooded = ..params$weeks_flooded[25:27, month, juv_dynamics_year],
                                      stochastic = stochastic)
        
        juveniles[25:27, ] <- south_delta_trib_rear$inchannel + south_delta_trib_rear$floodplain
        
        migrants[25:27, ] <- south_delta_trib_fish$migrants
        
        # route migrant fish into San Joquin River (fish from watersheds 28:30)
        # regional fish stay and rear
        # or migrate to south delta
        
        san_joaquin_trib_fish <- route(year = juv_dynamics_year,
                                       month = month,
                                       juveniles = juveniles[28:30, ],
                                       inchannel_habitat = habitat$inchannel[28:30],
                                       floodplain_habitat = habitat$floodplain[28:30],
                                       prop_pulse_flows =  ..params$prop_pulse_flows[28:30, ],
                                       .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                       .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                       .pulse_movement_medium = ..params$.pulse_movement_medium,
                                       .pulse_movement_large = ..params$.pulse_movement_large,
                                       .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                       .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                       .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                       .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                       territory_size = ..params$territory_size,
                                       stochastic = stochastic)
        
        san_joaquin_trib_rear <- rear(juveniles = san_joaquin_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[28:30, ],
                                      growth = ..params$growth_rates,
                                      floodplain_juveniles = san_joaquin_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[28:30, ],
                                      floodplain_growth = ..params$growth_rates_floodplain,
                                      weeks_flooded = ..params$weeks_flooded[28:30, month, juv_dynamics_year],
                                      stochastic = stochastic)
        
        juveniles[28:30, ] <- san_joaquin_trib_rear$inchannel + san_joaquin_trib_rear$floodplain
        
        san_joaquin_fish <- route_regional(month = month,
                                           year = juv_dynamics_year,
                                           migrants = san_joaquin_fish + san_joaquin_trib_fish$migrants,
                                           inchannel_habitat = habitat$inchannel[31],
                                           floodplain_habitat = habitat$floodplain[31],
                                           prop_pulse_flows = ..params$prop_pulse_flows[31, , drop = FALSE],
                                           migration_survival_rate = migratory_survival$san_joaquin,
                                           territory_size = ..params$territory_size,
                                           stochastic = stochastic)
        
        migrants[28:30, ] <- san_joaquin_fish$migrants
        
        san_joaquin_fish <- rear(juveniles = san_joaquin_fish$inchannel,
                                 survival_rate = rearing_survival$inchannel[31, ],
                                 growth = ..params$growth_rates,
                                 floodplain_juveniles = san_joaquin_fish$floodplain,
                                 floodplain_survival_rate = rearing_survival$floodplain[31, ],
                                 floodplain_growth = ..params$growth_rates_floodplain,
                                 weeks_flooded = rep(..params$weeks_flooded[31, month, juv_dynamics_year], nrow(san_joaquin_fish$inchannel)),
                                 stochastic = stochastic)
        
        san_joaquin_fish <- san_joaquin_fish$inchannel + san_joaquin_fish$floodplain
        
        delta_fish <- route_and_rear_deltas(year = juv_dynamics_year, month = month,
                                            migrants = round(migrants),
                                            north_delta_fish = north_delta_fish,
                                            south_delta_fish = south_delta_fish,
                                            north_delta_habitat = habitat$north_delta,
                                            south_delta_habitat = habitat$south_delta,
                                            freeport_flows = ..params$freeport_flows,
                                            cc_gates_days_closed = ..params$cc_gates_days_closed,
                                            rearing_survival_delta = rearing_survival$delta,
                                            migratory_survival_delta = migratory_survival$delta,
                                            migratory_survival_sac_delta = migratory_survival$sac_delta,
                                            migratory_survival_bay_delta = migratory_survival$bay_delta,
                                            juveniles_at_chipps = juveniles_at_chipps,
                                            growth_rates = ..params$growth_rates,
                                            territory_size = ..params$territory_size,
                                            stochastic = stochastic)
        
        migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate
        
        north_delta_fish <- delta_fish$north_delta_fish
        south_delta_fish <- delta_fish$south_delta_fish
        juveniles_at_chipps <- delta_fish$juveniles_at_chipps
      }
      
      adults_in_ocean <- adults_in_ocean + ocean_entry_success(migrants = migrants_at_golden_gate,
                                                               month = month,
                                                               avg_ocean_transition_month = avg_ocean_transition_month,
                                                               .ocean_entry_success_length = ..params$.ocean_entry_success_length,
                                                               ..ocean_entry_success_int = ..params$..ocean_entry_success_int,
                                                               .ocean_entry_success_months = ..params$.ocean_entry_success_months,
                                                               stochastic = stochastic)
      
    } # end month loop
    
    output$juvenile_biomass[ , year] <- juveniles_at_chipps %*% springRunDSM::params$mass_by_size_class
    
    adults_returning <- t(sapply(1:31, function(i) {
      if (stochastic) {
        rmultinom(1, adults_in_ocean[i], prob = c(.25, .5, .25))
      } else {
        round(adults_in_ocean[i] * c(.25, .5, .25))
      }
    }))
    
    # distribute returning adults for future spawning
    if (mode == "calibrate") {
      calculated_adults[1:31, (year + 2):(year + 4)] <- calculated_adults[1:31, (year + 2):(year + 4)] + adults_returning
    } else {
      adults[1:31, (year + 2):(year + 4)] <- adults[1:31, (year + 2):(year + 4)] + adults_returning
    }
    
  } # end year for loop
  
  if (mode == "seed") {
    return(adults[ , 6:30])
  } else if (mode == "calibrate") {
    return(calculated_adults[, 6:19])
  }
  
  spawn_change <- sapply(1:19, function(year) {
    output$spawners[ , year] / (output$spawners[ , year + 1] + 1)
  })
  
  viable <- spawn_change >= 1 & output$proportion_natural[ , -1] >= 0.9 & output$spawners[ , -1] >= 833
  
  output$viability_metrics <- sapply(1:4, function(group) {
    colSums(viable[which(springRunDSM::params$diversity_group == group), ])
  })
  
  return(output)
  
}





