update_params <- function(x, params) {
  
  params$..surv_adult_enroute_int = x[1]
  params$..surv_adult_prespawn_int = x[2]
  params$..surv_egg_to_fry_int = x[3]
  params$..surv_juv_rear_int = c(`Upper Sacramento River` = x[4], 
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
                                 `San Joaquin River` = x[12])
  params$..surv_juv_rear_contact_points = x[13]
  params$..surv_juv_rear_prop_diversions = x[14]
  params$..surv_juv_rear_total_diversions = x[15]
  params$..surv_juv_bypass_int = x[16]
  params$..surv_juv_delta_int = x[17]
  params$..surv_juv_delta_contact_points = x[18]
  params$..surv_juv_delta_total_diverted = x[19]
  params$..surv_juv_outmigration_sj_int = x[20]
  # Ocean entry success coefficient and variable
  params$..ocean_entry_success_int = c(
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
  
  return(params)
  
}
