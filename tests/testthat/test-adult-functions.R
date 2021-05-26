library(testthat)
library(springRunDSM)
# tests for adult functions
# Lists inputs to use in testing
test_data <- springRunDSM::load_baseline_data()
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(test_data$tisdale_bypass_watershed + test_data$yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(test_data$migratory_temperature_proportion_over_20[ , 10:12])
average_degree_days <- c(`Upper Sacramento River` = 0, `Antelope Creek` = 1233.77816060903, 
                         `Battle Creek` = 1292.65340515059, `Bear Creek` = 0, `Big Chico Creek` = 0, 
                         `Butte Creek` = 1230.30444092125, `Clear Creek` = 717.252853041589, 
                         `Cottonwood Creek` = 0, `Cow Creek` = 0, `Deer Creek` = 1234.5169260865, 
                         `Elder Creek` = 0, `Mill Creek` = 1247.07973637408, `Paynes Creek` = 0, 
                         `Stony Creek` = 0, `Thomes Creek` = 0, `Upper-mid Sacramento River` = 0, 
                         `Sutter Bypass` = 0, `Bear River` = 0, `Feather River` = 587.940732577516, 
                         `Yuba River` = 1171.26105941192, `Lower-mid Sacramento River` = 0, 
                         `Yolo Bypass` = 0, `American River` = 0, `Lower Sacramento River` = 0, 
                         `Calaveras River` = 0, `Cosumnes River` = 0, `Mokelumne River` = 0, 
                         `Merced River` = 0, `Stanislaus River` = 0, `Tuolumne River` = 0, 
                         `San Joaquin River` = 0)

# Tests adult straying function
expected_straying_output <- c(`Upper Sacramento River` = 0.0179218144440285, `Antelope Creek` = 0.0740104504838898,
                              `Battle Creek` = 0.0678754718644023, `Bear Creek` = 0.0755029971698723,
                              `Big Chico Creek` = 0.0748474212008828, `Butte Creek` = 0.0730690238859734,
                              `Clear Creek` = 0.0709270693771585, `Cottonwood Creek` = 0.0709270693771585,
                              `Cow Creek` = 0.0704102088441598, `Deer Creek` = 0.0722674057922913,
                              `Elder Creek` = 0.0755511108365283, `Mill Creek` = 0.0717077263560781,
                              `Paynes Creek` = 0.0755029971698723, `Stony Creek` = 0.0756043218542417,
                              `Thomes Creek` = 0.0739657235650295, `Upper-mid Sacramento River` = 0.0110961379974576,
                              `Sutter Bypass` = 0.0758581800212435, `Bear River` = 0.0736293343892286,
                              `Feather River` = 0.0409341987251872, `Yuba River` = 0.0526037516322295,
                              `Lower-mid Sacramento River` = 0.0110961379974576, `Yolo Bypass` = 0.0758581800212435,
                              `American River` = 0.0579554510917732, `Lower Sacramento River` = 0.0110961379974576,
                              `Calaveras River` = 0.0110961379974576, `Cosumnes River` = 0.0110961379974576,
                              `Mokelumne River` = 0.0110961379974576, `Merced River` = 0.0429069359657317,
                              `Stanislaus River` = 0.0293554482561996, `Tuolumne River` = 0.0468862825202343,
                              `San Joaquin River` = 0.0110961379974576)

test_that('The straying function returns the expected values for year 1', {
  expect_equal(adult_stray(wild = 1,
                           natal_flow = test_data$prop_flow_natal[ , year],
                           south_delta_watershed = test_data$south_delta_routed_watersheds,
                           cross_channel_gates_closed = test_data$cc_gates_days_closed[10]),
               expected_straying_output)
})

#tests adult surv_en_route
expected_surv_en_route <- c(`Upper Sacramento River` = 0.951708351611047, `Antelope Creek` = 0.951708351611047, 
                            `Battle Creek` = 0.951708351611047, `Bear Creek` = 0.951708351611047, 
                            `Big Chico Creek` = 0.951708351611047, `Butte Creek` = 0.951708351611047, 
                            `Clear Creek` = 0.951708351611047, `Cottonwood Creek` = 0.951708351611047, 
                            `Cow Creek` = 0.951708351611047, `Deer Creek` = 0.951708351611047, 
                            `Elder Creek` = 0.951708351611047, `Mill Creek` = 0.951708351611047, 
                            `Paynes Creek` = 0.951708351611047, `Stony Creek` = 0.951708351611047, 
                            `Thomes Creek` = 0.951708351611047, `Upper-mid Sacramento River` = 0.951708351611047, 
                            `Sutter Bypass` = 0.952574126822433, `Bear River` = 0.951708351611047, 
                            `Feather River` = 0.951708351611047, `Yuba River` = 0.951708351611047, 
                            `Lower-mid Sacramento River` = 0.952574126822433, `Yolo Bypass` = 0.952574126822433, 
                            `American River` = 0.952574126822433, `Lower Sacramento River` = 0.952574126822433, 
                            `Calaveras River` = 0.952320885972597, `Cosumnes River` = 0.952320885972597, 
                            `Mokelumne River` = 0.952320885972597, `Merced River` = 0.952130112901253, 
                            `Stanislaus River` = 0.952130112901253, `Tuolumne River` = 0.952130112901253, 
                            `San Joaquin River` = 0.952574126822433)

test_that('The adult enroute survival function returns the expected values for year 1', {
  expect_equal(surv_adult_enroute(migratory_temp = avg_migratory_temp,
                                  bypass_overtopped = bypass_is_overtopped,
                                  adult_harvest = adult_harvest_rate),
               expected_surv_en_route)
})

# Tests prespawn survival
expected_prespawn_surv <- c(`Upper Sacramento River` = 0.952574126822433, `Antelope Creek` = 0.897886015404489, 
                            `Battle Creek` = 0.8942147569172, `Bear Creek` = 0.952574126822433, 
                            `Big Chico Creek` = 0.952574126822433, `Butte Creek` = 0.89809905814968, 
                            `Clear Creek` = 0.925516929510014, `Cottonwood Creek` = 0.952574126822433, 
                            `Cow Creek` = 0.952574126822433, `Deer Creek` = 0.897840656146627, 
                            `Elder Creek` = 0.952574126822433, `Mill Creek` = 0.897066578095631, 
                            `Paynes Creek` = 0.952574126822433, `Stony Creek` = 0.952574126822433, 
                            `Thomes Creek` = 0.952574126822433, `Upper-mid Sacramento River` = 0.952574126822433, 
                            `Sutter Bypass` = 0.952574126822433, `Bear River` = 0.952574126822433, 
                            `Feather River` = 0.931269675631335, `Yuba River` = 0.901660321075316, 
                            `Lower-mid Sacramento River` = 0.952574126822433, `Yolo Bypass` = 0.952574126822433, 
                            `American River` = 0.952574126822433, `Lower Sacramento River` = 0.952574126822433, 
                            `Calaveras River` = 0.952574126822433, `Cosumnes River` = 0.952574126822433, 
                            `Mokelumne River` = 0.952574126822433, `Merced River` = 0.952574126822433, 
                            `Stanislaus River` = 0.952574126822433, `Tuolumne River` = 0.952574126822433, 
                            `San Joaquin River` = 0.952574126822433)

test_that('The prespawn survival function returns the expected values for year 1', {
  expect_equal(surv_adult_prespawn(average_degree_days),
               expected_prespawn_surv)
})

# Tests egg to fry surv
expected_egg_surv <- c(`Upper Sacramento River` = 0.55126648754873, `Antelope Creek` = 0.557465511034345, 
                       `Battle Creek` = 0.555249545010465, `Bear Creek` = 0.534371105617283, 
                       `Big Chico Creek` = 0.537367315773288, `Butte Creek` = 0.569124433535414, 
                       `Clear Creek` = 0.569292247931292, `Cottonwood Creek` = 0.506799804331753, 
                       `Cow Creek` = 0.570696629269701, `Deer Creek` = 0.569325901735189, 
                       `Elder Creek` = 0.539482360132187, `Mill Creek` = 0.514366344433048, 
                       `Paynes Creek` = 0.534371105617283, `Stony Creek` = 0.528387818867039, 
                       `Thomes Creek` = 0.535652339961507, `Upper-mid Sacramento River` = 0, 
                       `Sutter Bypass` = 0, `Bear River` = 0.512991581615581, `Feather River` = 0.437030519553356, 
                       `Yuba River` = 0.508044472640408, `Lower-mid Sacramento River` = 0, 
                       `Yolo Bypass` = 0, `American River` = 0.539767947824993, `Lower Sacramento River` = 0, 
                       `Calaveras River` = 0.511435418526615, `Cosumnes River` = 0.520695295226667, 
                       `Mokelumne River` = 0.545634566287998, `Merced River` = 0.388654548325646, 
                       `Stanislaus River` = 0.551201577688213, `Tuolumne River` = 0.564926181881676, 
                       `San Joaquin River` = 0)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(surv_egg_to_fry(proportion_natural = 1 - proportion_hatchery,
                               scour = test_data$prob_nest_scoured,
                               temperature_effect = test_data$mean_egg_temp_effect),
               expected_egg_surv)
})
