#' @title Spring Run Chinook Model
#' @description Spring Run Chinook life cycle model used for CVPIA's Structured
#' Decision Making Process
#' @param scenario Model inputs, can be modified to test management actions
#' @param seeds The default value is NULL runs the model in seeding mode,
#' returning a 31 by 25 matrix with the first four years of seeded adults. This
#' returned value can be fed into the model again as the value for the seeds argument
#' @source IP-117068
#' @export
spring_run_model <- function(scenario = NULL, seeds = NULL){
  
  watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                        "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                        "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                        "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                        "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                        "Feather River", "Yuba River", "Lower-mid Sacramento River",
                        "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                        "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                        "Tuolumne River", "San Joaquin River")
  
  size_class_labels <- c('s', 'm', 'l', 'vl')
  
  output <- list(
    
    # SIT METRICS
    spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(watershed_labels, 1:20)),
    natural_spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(watershed_labels, 1:20)),
    juvenile_biomass = matrix(0, nrow = 31, ncol = 20, dimnames = list(watershed_labels, 1:20))
  )
  
  # initialise 31 x 4 matrices for natal fish, migrants, and ocean fish
  lower_mid_sac_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
  lower_sac_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
  upper_mid_sac_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(watershed_labels[1:15], size_class_labels))
  sutter_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(watershed_labels[1:15], size_class_labels))
  yolo_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(watershed_labels[18:20], size_class_labels))
  san_joaquin_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(watershed_labels[28:30], size_class_labels))
  north_delta_fish <- matrix(0, nrow = 23, ncol = 4, dimnames = list(watershed_labels[1:23], size_class_labels))
  south_delta_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
  juveniles_at_chipps <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
  proportion_natural <- matrix(NA_real_, nrow = 31, ncol = 20)
  
  
  # calculate growth rates
  growth_rates <- growth()
  growth_rates_floodplain <- growth_floodplain()
  
  adults <- if(is.null(seeds)) adult_seeds else seeds
  simulation_length <- ifelse(is.null(seeds), 5, 20)
  
  for (year in 1:simulation_length) {
    adults_in_ocean <- numeric(31)
    annual_migrants <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
    avg_ocean_transition_month <- ocean_transition_month() # 2
    # TODO confirm this works as expected
    
    
    
    hatch_adults <- rmultinom(1, size = round(runif(1, 4588.097,8689.747)), prob = hatchery_allocation)[ , 1]
    spawners <- get_spawning_adults(year, round(adults[ , year]), hatch_adults, seeds=seeds)
    init_adults <- spawners$init_adults
    
    output$spawners[ , year] <- init_adults
    proportion_natural[ , year] <- spawners$proportion_natural
    output$natural_spawners[ , year] <- spawners$natural_adults
    
    egg_to_fry_surv <- surv_egg_to_fry(
      proportion_natural = 1 - proportion_hatchery,
      scour = prob_nest_scoured,
      temperature_effect = mean_egg_temp_effect
    )
    
    min_spawn_habitat <- apply(spawning_habitat[ , 3:6, year], 1, min)
    
    average_degree_days <- (
      spawners$init_adults_by_month[,1]*rowSums(degree_days[,3:6,year]) +
        spawners$init_adults_by_month[,2]*rowSums(degree_days[,4:6,year]) +           
        spawners$init_adults_by_month[,3]*rowSums(degree_days[,5:6,year]) + 
        spawners$init_adults_by_month[,4]*(degree_days[,6,year])
    )/(init_adults)    
    average_degree_days <- ifelse(is.nan(average_degree_days), 0, average_degree_days)
    
    # calculate and apply pre-spawn survival to adults
    prespawn_survival <- surv_adult_prespawn(average_degree_days)
    init_adults <- rbinom(31, round(init_adults), prespawn_survival)
    
    # spring run above capacity die, capacity based on spring run pools
    init_adults <- ifelse(init_adults >= spring_run_pools, spring_run_pools, init_adults)
    
    # Holding period for spring run
    # apply degree days and prespawn survival 
    init_spawn_holding <- matrix(0, ncol = 2, nrow = 31)
    
    # Add split popoulation in half using binom and 
    init_spawn_holding[, 1] <- rbinom(31, init_adults, 0.5)
    init_spawn_holding[, 2] <- pmax(init_adults - init_spawn_holding[, 1], 0)
    
    average_degree_days <- ((init_spawn_holding[, 1] * rowSums(degree_days[, 7:10, year])) +
                              (init_spawn_holding[, 2] * rowSums(degree_days[, 7:9, year])))/init_adults
    
    average_degree_days <- ifelse(is.nan(average_degree_days), 0, average_degree_days)
    
    prespawn_survival <- surv_adult_prespawn(average_degree_days)
    
    juveniles <- round(rspawn_success(escapement = init_adults,
                               adult_prespawn_survival = prespawn_survival,
                               egg_to_fry_survival = egg_to_fry_surv,
                               prob_scour = prob_nest_scoured,
                               spawn_habitat = min_spawn_habitat, 
                               sex_ratio = 0.5, 
                               redd_size = 9.29, 
                               fecundity = 5522))
    
    
    # TODO flood activation based on scenarios
    
    for (month in c(11, 12, 1:5)) {
      if (month %in% 1:5) juv_dynamics_year <- year + 1 else juv_dynamics_year <- year
      # TODO confirm aproach works in edge cases (i.e when year = 25)
      
      habitat <- get_habitat(juv_dynamics_year, month) # habitat$yolo
      rearing_survival <- get_rearing_survival_rates(juv_dynamics_year, month, scenario) # rearing_survival$inchannel
      migratory_survival <- get_migratory_survival_rates(juv_dynamics_year, month) #migratory_survival$uppermid_sac
      migrants <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
      
      
      
      if (month == 5) {
        # yearling logic here
        # 1 - 15, 18-20, 23, 25:30
        yearlings <- matrix(0, ncol = 4, nrow = 31, 
                            dimnames = list(watershed_labels, size_class_labels))
        
        yearlings[c(1:15, 18:20, 23, 25:30), 1:2] <- 
          juveniles[c(1:15, 18:20, 23, 25:30), 1:2]
        juveniles[c(1:15, 18:20, 23, 25:30), 1:2] <- 0 # set all to zero since they are yearlings now
        
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
        
      } else {
        
        if (month == 11 & year > 1) {
          # applying summer year to the yearlings and send them out to the ocean
          yearlings <- yearling_growth(year, round(yearlings))
          
          # detoured.fish<-rbinMatObject(yearlings[1:15,],prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),1],stochastic)
          sutter_detoured <- t(sapply(1:nrow(yearlings[1:15, ]), function(i) {
            rbinom(n = 4,
                   size = round(yearlings[i, ]),
                   prob = proportion_flow_bypass[month, year, 1])
          }))
          
          # yearlingsUM<-(rbind(rbin2MatSpec(yearlings[1:15,]-detoured.fish,UM.Sac.S,stochastic),matrix(0,ncol=4,nrow=2))*stochastic)+(rbind((yearlings[1:15,]*(1-prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),1]))%z%UM.Sac.S,matrix(0,ncol=4,nrow=2)))*(1-stochastic)
          yearlings_at_uppermid <- rbind(
            migrate(yearlings[1:15, ] - sutter_detoured, migratory_survival$uppermid_sac), 
            matrix(0, ncol = 4, nrow = 2)
          )
          
          # yearlingsSut<-(rbind(rbin2MatSpec(detoured.fish,Sut.S,stochastic),matrix(0,ncol=4,nrow=2))*stochastic)+(rbind(detoured.fish%z%Sut.S,matrix(0,ncol=4,nrow=2)))*(1-stochastic)
          yearlings_at_sutter <- rbind(
            migrate(sutter_detoured, migratory_survival$sutter), 
            matrix(0, ncol = 4, nrow = 2)
          )
          
          # yearlingsUM<-yearlingsSut+yearlingsUM
          yearlings_at_uppermid <- yearlings_at_sutter + yearlings_at_uppermid
          
          # yearlingsLM<-rbind(yearlingsUM,yearlings[18:20,])
          yearlings_at_lowermid <- rbind(yearlings_at_uppermid, yearlings[18:20, ]) 
          
          # detoured.fish<-rbinMatObject(yearlingsLM,prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),5],stochastic)
          # yearlingsyolo<-detoured.fish
          yolo_detoured <- t(sapply(1:nrow(yearlings_at_lowermid), function(i) {
            rbinom(n = 4,
                   size = round(yearlings_at_lowermid[i, ]),
                   prob = proportion_flow_bypass[month, year, 2])
          }))
          
          # yearlingsLM<-(stochastic*(yearlingsLM-detoured.fish))+((1-stochastic)*(yearlingsLM*(1-prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),5])))
          # detoured.fish<-NULL
          # yearlingsLSac<-(rbind(rbin2MatSpec(yearlingsLM,LM.Sac.S,stochastic),matrix(0,ncol=4,nrow=3))*stochastic)+(rbind(yearlingsLM%z%LM.Sac.S,matrix(0,ncol=4,nrow=3)))*(1-stochastic)
          yearlings_at_lowersac <- rbind(
            migrate(yearlings_at_lowermid - yolo_detoured, migratory_survival$lowermid_sac), 
            matrix(0, ncol = 4, nrow = 3)
          )
          
          # yearlingsLSac[23,]<-yearlings[23,]
          yearlings_at_lowersac[23, ] <- yearlings[23, ]
          
          # yearlingsLSac<-(rbin2MatSpec(yearlingsLSac,LL.Sac.S,stochastic)*stochastic)+((yearlingsLSac%z%LL.Sac.S)*(1-stochastic))
          yearlings_at_lowersac <- migrate(yearlings_at_lowersac, migratory_survival$lower_sac)
          
          # sac.not.entrained<-rbinMatObject(yearlingsLSac,(1-prop.dlt.entrain),stochastic)
          prop_delta_fish_entrained <- route_south_delta(freeport_flow = freeport_flows[[month, year]] * 35.3147,
                                                         dcc_closed = cc_gates_days_closed[month],
                                                         month = month)
          
          sac_not_entrained <- t(sapply(1:nrow(yearlings_at_lowersac), function(i) {
            
            rbinom(n = 4, yearlings_at_lowersac[i, ], prob = 1 - prop_delta_fish_entrained)
          }))
          
          # yearlingsNDl<-sac.not.entrained + 
          #   (rbind(rbin2MatSpec(yearlingsyolo,Yolo.S,stochastic),matrix(0,ncol=4,nrow=3))*stochastic)+(rbind(yearlingsyolo%z%Yolo.S,matrix(0,ncol=4,nrow=3)))*(1-stochastic)
          yearlings_at_north_delta <- sac_not_entrained + 
            rbind(migrate(yolo_detoured, migratory_survival$yolo), matrix(0, ncol = 4, nrow = 3))
          
          # yearlingsNDl<-rbind(yearlingsNDl,matrix(0,ncol=4,nrow=8))
          yearlings_at_north_delta <- rbind(yearlings_at_north_delta, 
                                            matrix(0, ncol = 4, nrow = 8))
          
          
          # yearlingsSDl<-rbind(
          #   matrix(0,ncol=4,nrow=24),
          #   yearlings[25:27,],
          #   ((rbin2MatSpec(yearlings[28:30,],SJ.S,stochastic)*stochastic)+(yearlings[28:30,]%z%SJ.S)*(1-stochastic)),
          #   matrix(0,ncol=4,nrow=1)
          # ) +
          #   rbind((((yearlingsLSac-sac.not.entrained)*stochastic)+((yearlingsLSac*prop.dlt.entrain)*(1-stochastic))),matrix(0,ncol=4,nrow=8))
          # 
          yearlings_at_south_delta <- rbind(
            matrix(0, ncol = 4, nrow = 24), # 24 rows for north delta/sac origin fish
            yearlings[25:27, ], # delta tribs
            migrate(yearlings[28:30,], migratory_survival$san_joaquin), 
            matrix(0, ncol = 4, nrow = 1) # SJR
          ) + 
            rbind(
              yearlings_at_lowersac - sac_not_entrained, 
              matrix(0, ncol = 4, nrow = 8)
            )
          
          
          # estimate fish at Golden Gate Bridge and Chipps Island
          # holdSdelta<-array(0,dim=dim(S.delt.fsh$out2ocean))
          yearling_holding_south_delta <- matrix(0, nrow = 31, ncol = 4, dimnames = list(watershed_labels, size_class_labels))
          
          # holdSdelta[1:24,]<-rbinMatVector(S.delt.fsh$out2ocean[1:24,],newDsurv[1,],stochastic)
          yearling_holding_south_delta[1:24, ] <- t(sapply(1:24, function(i) {
            rbinom(n = 4, size = round(yearlings_at_south_delta[i, ]), prob = migratory_survival$delta[1, ])
          }))
          
          # holdSdelta[26:27,]<-rbinMatVector(S.delt.fsh$out2ocean[26:27,],newDsurv[2,],stochastic)
          yearling_holding_south_delta[26:27, ] <- t(sapply(26:27, function(i) {
            rbinom(n = 4, size = round(yearlings_at_south_delta[i, ]), prob = migratory_survival$delta[2, ])
          }))
          
          # holdSdelta[25,]<-rbin2Vectors(S.delt.fsh$out2ocean[25,],newDsurv[3,],stochastic)
          yearling_holding_south_delta[25, ] <- rbinom(n = 4, 
                                              yearlings_at_south_delta[25, , drop = F], 
                                              prob = migratory_survival$delta[3, ])
          
          # holdSdelta[28:31,]<-rbinMatVector(S.delt.fsh$out2ocean[28:31,],newDsurv[4,],stochastic)
          yearling_holding_south_delta[28:31, ] <- t(sapply(28:31, function(i) {
            rbinom(n = 4, size = round(yearlings_at_south_delta[i, ]), prob = migratory_survival$delta[4, ])
          }))
          
          # migrants.out<-rbinMatVector(N.delt.fsh$out2ocean,Sac.Delt.S[1,],stochastic)
          yearlings_out <- t(sapply(1:nrow(yearlings_at_north_delta), function(i) {
            rbinom(n = 4, 
                   size = round(yearlings_at_north_delta[i, ]), 
                   prob = migratory_survival$sac_delta[1, ])
          }))
          
          
          # migrants.at.GG<-rbinMatObject(migrants.out,Bay.S,stochastic)+rbinMatObject(holdSdelta,Bay.S,stochastic)
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
                                                                   avg_ocean_transition_month = avg_ocean_transition_month)
        }
        
        # if month < 8
        # route northern natal fish stay and rear or migrate downstream ------
        upper_sac_trib_fish <-  route(year = juv_dynamics_year,
                                      month = month,
                                      juveniles = juveniles[1:15, ],
                                      inchannel_habitat = habitat$inchannel[1:15],
                                      floodplain_habitat = habitat$floodplain[1:15],
                                      prop_pulse_flows =  prop_pulse_flows[1:15, ],
                                      detour = 'sutter')
        
        upper_sac_trib_rear <- rear(juveniles = upper_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[1:15, ],
                                    growth = growth_rates,
                                    floodplain_juveniles = upper_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[1:15, ],
                                    floodplain_growth = growth_rates_floodplain,
                                    weeks_flooded = weeks_flooded[1:15, month, juv_dynamics_year])
        
        juveniles[1:15, ] <- upper_sac_trib_rear$inchannel + upper_sac_trib_rear$floodplain
        
        # route migrant fish into Upper-mid Sac Region (fish from watersheds 1:15)
        # regional fish stay and rear
        # or migrate further downstream or in sutter bypass
        sutter_fish <- route_bypass(bypass_fish = sutter_fish + upper_sac_trib_fish$detoured,
                                    bypass_habitat = habitat$sutter,
                                    migration_survival_rate = migratory_survival$sutter)
        
        upper_mid_sac_fish <- route_regional(month = month,
                                             migrants = upper_mid_sac_fish + upper_sac_trib_fish$migrants,
                                             inchannel_habitat = habitat$inchannel[16],
                                             floodplain_habitat = habitat$floodplain[16],
                                             prop_pulse_flows = prop_pulse_flows[16, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$uppermid_sac)
        
        
        migrants[1:15, ] <- upper_mid_sac_fish$migrants + sutter_fish$migrants
        
        sutter_fish <- rear(juveniles = sutter_fish$inchannel,
                            survival_rate = matrix(rep(rearing_survival$sutter, nrow(sutter_fish$inchannel)), ncol = 4, byrow = TRUE),
                            growth = growth_rates)
        
        
        upper_mid_sac_fish <- rear(juveniles = upper_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[16, ],
                                   growth = growth_rates,
                                   floodplain_juveniles = upper_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[16, ],
                                   floodplain_growth = growth_rates_floodplain,
                                   weeks_flooded = rep(weeks_flooded[16, month, juv_dynamics_year], nrow(upper_mid_sac_fish$inchannel)))
        
        upper_mid_sac_fish <- upper_mid_sac_fish$inchannel + upper_mid_sac_fish$floodplain
        
        # route migrant fish into Lower-mid Sac Region (fish from watersheds 18:20, and migrants from Upper-mid Sac Region)
        # regional fish stay and rear
        # or migrate further downstream  or in yolo bypass
        lower_mid_sac_trib_fish <- route(year = juv_dynamics_year,
                                         month = month,
                                         juveniles = juveniles[18:20, ],
                                         inchannel_habitat = habitat$inchannel[18:20],
                                         floodplain_habitat = habitat$floodplain[18:20],
                                         prop_pulse_flows =  prop_pulse_flows[18:20, ],
                                         detour = 'yolo')
        
        lower_mid_sac_trib_rear <- rear(juveniles = lower_mid_sac_trib_fish$inchannel,
                                        survival_rate = rearing_survival$inchannel[18:20, ],
                                        growth = growth_rates,
                                        floodplain_juveniles = lower_mid_sac_trib_fish$floodplain,
                                        floodplain_survival_rate = rearing_survival$floodplain[18:20, ],
                                        floodplain_growth = growth_rates_floodplain,
                                        weeks_flooded = weeks_flooded[18:20, month, juv_dynamics_year])
        
        juveniles[18:20, ] <- lower_mid_sac_trib_rear$inchannel + lower_mid_sac_trib_rear$floodplain
        
        yolo_fish <- route_bypass(bypass_fish = yolo_fish + lower_mid_sac_trib_fish$detoured,
                                  bypass_habitat = habitat$yolo,
                                  migration_survival_rate = migratory_survival$yolo)
        
        migrants[18:20, ] <- lower_mid_sac_trib_fish$migrants + yolo_fish$migrants
        
        lower_mid_sac_fish <- route_regional(month = month,
                                             migrants = lower_mid_sac_fish + migrants,
                                             inchannel_habitat = habitat$inchannel[21],
                                             floodplain_habitat = habitat$floodplain[21],
                                             prop_pulse_flows = prop_pulse_flows[21, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$lowermid_sac)
        
        migrants <- lower_mid_sac_fish$migrants
        
        # rear
        yolo_fish <- rear(juveniles = yolo_fish$inchannel,
                          survival_rate = matrix(rep(rearing_survival$yolo, nrow(yolo_fish$inchannel)), ncol = 4, byrow = TRUE),
                          growth = growth_rates)
        
        lower_mid_sac_fish <- rear(juveniles = lower_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[21, ],
                                   growth = growth_rates,
                                   floodplain_juveniles = lower_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[21, ],
                                   floodplain_growth = growth_rates_floodplain,
                                   weeks_flooded = rep(weeks_flooded[21, month, juv_dynamics_year], nrow(lower_mid_sac_fish$inchannel)))
        
        lower_mid_sac_fish <- lower_mid_sac_fish$inchannel + lower_mid_sac_fish$floodplain
        
        # route migrant fish into Lower Sac Region (fish from watershed 23, and migrants from Lower-mid Sac Region)
        # regional fish stay and rear
        # or migrate north delta
        lower_sac_trib_fish <- route(year = juv_dynamics_year,
                                     month = month,
                                     juveniles = juveniles[23, , drop = FALSE],
                                     inchannel_habitat = habitat$inchannel[23],
                                     floodplain_habitat = habitat$floodplain[23],
                                     prop_pulse_flows =  prop_pulse_flows[23, , drop = FALSE])
        
        lower_sac_trib_rear <- rear(juveniles = lower_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[23, , drop = FALSE],
                                    growth = growth_rates,
                                    floodplain_juveniles = lower_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[23, , drop = FALSE],
                                    floodplain_growth = growth_rates_floodplain,
                                    weeks_flooded = weeks_flooded[23, month, juv_dynamics_year])
        
        juveniles[23, ] <- lower_sac_trib_rear$inchannel + lower_sac_trib_rear$floodplain
        
        migrants[23, ] <- lower_sac_trib_fish$migrants
        
        lower_sac_fish <- route_regional(month = month,
                                         migrants = lower_sac_fish + migrants,
                                         inchannel_habitat = habitat$inchannel[24],
                                         floodplain_habitat = habitat$floodplain[24],
                                         prop_pulse_flows = prop_pulse_flows[24, , drop = FALSE],
                                         migration_survival_rate = migratory_survival$lower_sac)
        
        migrants <- lower_sac_fish$migrants
        
        lower_sac_fish <- rear(juveniles = lower_sac_fish$inchannel,
                               survival_rate = rearing_survival$inchannel[24, ],
                               growth = growth_rates,
                               floodplain_juveniles = lower_sac_fish$floodplain,
                               floodplain_survival_rate = rearing_survival$floodplain[24, ],
                               floodplain_growth = growth_rates_floodplain,
                               weeks_flooded = rep(weeks_flooded[24, month, juv_dynamics_year], nrow(lower_sac_fish$inchannel)))
        
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
                                       prop_pulse_flows =  prop_pulse_flows[25:27, ])
        
        south_delta_trib_rear <- rear(juveniles = south_delta_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[25:27, ],
                                      growth = growth_rates,
                                      floodplain_juveniles = south_delta_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[25:27, ],
                                      floodplain_growth = growth_rates_floodplain,
                                      weeks_flooded = weeks_flooded[25:27, month, juv_dynamics_year])
        
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
                                       prop_pulse_flows =  prop_pulse_flows[28:30, ])
        
        san_joaquin_trib_rear <- rear(juveniles = san_joaquin_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[28:30, ],
                                      growth = growth_rates,
                                      floodplain_juveniles = san_joaquin_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[28:30, ],
                                      floodplain_growth = growth_rates_floodplain,
                                      weeks_flooded = weeks_flooded[28:30, month, juv_dynamics_year])
        
        juveniles[28:30, ] <- san_joaquin_trib_rear$inchannel + san_joaquin_trib_rear$floodplain
        
        san_joaquin_fish <- route_regional(month = month,
                                           migrants = san_joaquin_fish + san_joaquin_trib_fish$migrants,
                                           inchannel_habitat = habitat$inchannel[31],
                                           floodplain_habitat = habitat$floodplain[31],
                                           prop_pulse_flows = prop_pulse_flows[31, , drop = FALSE],
                                           migration_survival_rate = migratory_survival$san_joaquin)
        
        migrants[28:30, ] <- san_joaquin_fish$migrants
        
        san_joaquin_fish <- rear(juveniles = san_joaquin_fish$inchannel,
                                 survival_rate = rearing_survival$inchannel[31, ],
                                 growth = growth_rates,
                                 floodplain_juveniles = san_joaquin_fish$floodplain,
                                 floodplain_survival_rate = rearing_survival$floodplain[31, ],
                                 floodplain_growth = growth_rates_floodplain,
                                 weeks_flooded = rep(weeks_flooded[31, month, juv_dynamics_year], nrow(san_joaquin_fish$inchannel)))
        
        san_joaquin_fish <- san_joaquin_fish$inchannel + san_joaquin_fish$floodplain
        
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
        
        north_delta_fish <- delta_fish$north_delta_fish
        south_delta_fish <- delta_fish$south_delta_fish
        juveniles_at_chipps <- delta_fish$juveniles_at_chipps
      }
      
      adults_in_ocean <- adults_in_ocean + ocean_entry_success(migrants = migrants_at_golden_gate,
                                                               month = month,
                                                               avg_ocean_transition_month = avg_ocean_transition_month)
      
    } # end month loop
    
    output$juvenile_biomass[ , year] <- juveniles_at_chipps %*% mass_by_size_class
    
    adults_returning <- t(sapply(1:31, function(i) {
      rmultinom(1, adults_in_ocean[i], prob = c(.25, .5, .25))
    }))
    
    # distribute returning adults for future spawning
    adults[1:31, (year + 2):(year + 4)] <- adults[1:31, (year + 2):(year + 4)] + adults_returning
    
  } # end year for loop
  
  if (is.null(seeds)) {
    return(adults[ , 6:30])
  }
  
  spawn_change <- sapply(1:19, function(year) {
    output$spawners[ , year] / (output$spawners[ , year + 1] + 1)
  })
  
  viable <- spawn_change >= 1 & proportion_natural[ , -1] >= 0.9 & output$spawners[ , -1] >= 833
  
  output$viability_metrics <- sapply(1:4, function(group) {
    colSums(viable[which(diversity_group == group), ])
  })
  
  output
}











