library(testthat)
library(springRunDSM)
# tests for adult functions
# Lists inputs to use in testing
list2env(load_baseline_data(), envir = .GlobalEnv)
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(tisdale_bypass_watershed + yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(migratory_temperature_proportion_over_20[ , 10:12])
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
                           natal_flow = prop_flow_natal[ , year],
                           south_delta_watershed = south_delta_routed_watersheds,
                           cross_channel_gates_closed = cc_gates_days_closed[10]),
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
mean_egg_temp_effect <- c(0.900631877, 0.879803496, 0.876306213, 0.843355432, 0.846055568, 
                          0.896491346, 0.900631877, 0.7941397695, 0.900685118, 0.900685118, 
                          0.843355432, 0.8098417915, 0.843355432, 0.835920241, 0.843355432, 
                          0, 0, 0.809613829, 0.87965336, 0.884765615, 0, 0, 0.8457998405, 
                          0, 0.8091012, 0.8178536045, 0.8611316565, 0.6133825745, 0.869917628, 
                          0.893724672, 0)
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
                               scour = prob_nest_scoured,
                               temperature_effect = mean_egg_temp_effect),
               expected_egg_surv)
})

# Test get_spawning_adults
adults <- structure(c(22012, 72, 12626, 12, 12, 885, 8555, 1251, 1649,
                      569, 12, 1332, 51, 12, 12, 0, 0, 12, 52408, 7184, 0, 0, 24959,
                      0, 12, 499, 4514, 2145, 5405, 984, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 31:30, .Dimnames = list(c("Upper Sacramento River",
                                                                                   "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek",
                                                                                   "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek",
                                                                                   "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek",
                                                                                   "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass",
                                                                                   "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River",
                                                                                   "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                                                                                   "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                                                                                   "Tuolumne River", "San Joaquin River"), NULL))
hatch_adults <- c(`Upper Sacramento River` = 6926L, `Antelope Creek` = 13L, `Battle Creek` = 16007L,
                  `Bear Creek` = 15L, `Big Chico Creek` = 18L, `Butte Creek` = 80L,
                  `Clear Creek` = 1082L, `Cottonwood Creek` = 914L, `Cow Creek` = 274L,
                  `Deer Creek` = 132L, `Elder Creek` = 15L, `Mill Creek` = 66L,
                  `Paynes Creek` = 25L, `Stony Creek` = 16L, `Thomes Creek` = 13L,
                  `Upper-mid Sacramento River` = 0L, `Sutter Bypass` = 0L, `Bear River` = 21L,
                  `Feather River` = 50417L, `Yuba River` = 8136L, `Lower-mid Sacramento River` = 0L,
                  `Yolo Bypass` = 0L, `American River` = 14083L, `Lower Sacramento River` = 0L,
                  `Calaveras River` = 28L, `Cosumnes River` = 14L, `Mokelumne River` = 2926L,
                  `Merced River` = 1402L, `Stanislaus River` = 1506L, `Tuolumne River` = 475L,
                  `San Joaquin River` = 0L)
seeds <- NULL

expected_spawners <- list(init_adults = c(0, 4, 439, 0, 0, 8896, 179, 0, 0, 574, 0, 
                                          478, 0, 0, 0, 0, 0, 0, 3748, 641, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                          0, 0), 
                          proportion_natural = c(`Upper Sacramento River` = 1, `Antelope Creek` = 1, 
                                                 `Battle Creek` = 1, `Bear Creek` = 1, `Big Chico Creek` = 1, 
                                                 `Butte Creek` = 0.9975, `Clear Creek` = 1, `Cottonwood Creek` = 1, 
                                                 `Cow Creek` = 1, `Deer Creek` = 1, `Elder Creek` = 1, `Mill Creek` = 1, 
                                                 `Paynes Creek` = 1, `Stony Creek` = 1, `Thomes Creek` = 1, `Upper-mid Sacramento River` = 1, 
                                                 `Sutter Bypass` = 1, `Bear River` = 1, `Feather River` = 0.145, 
                                                 `Yuba River` = 0.50875, `Lower-mid Sacramento River` = 1, `Yolo Bypass` = 1, 
                                                 `American River` = 1, `Lower Sacramento River` = 1, `Calaveras River` = 1, 
                                                 `Cosumnes River` = 1, `Mokelumne River` = 1, `Merced River` = 1, 
                                                 `Stanislaus River` = 1, `Tuolumne River` = 1, `San Joaquin River` = 1),
                          natural_adults = c(0, 4, 439, 0, 0, 8896, 179, 0, 0, 574, 
                                             0, 478, 0, 0, 0, 0, 0, 0, 4811, 641, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0), 
                          init_adults_by_month = structure(c(0L, 0L, 57L, 0L, 
                                                             0L, 1109L, 30L, 0L, 0L, 57L, 0L, 54L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                             480L, 77L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 
                                                             157L, 0L, 0L, 3321L, 70L, 0L, 0L, 226L, 0L, 171L, 0L, 0L, 0L, 
                                                             0L, 0L, 0L, 1339L, 253L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                             0L, 0L, 0L, 1L, 159L, 0L, 0L, 3313L, 59L, 0L, 0L, 205L, 0L, 181L, 
                                                             0L, 0L, 0L, 0L, 0L, 0L, 1439L, 234L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                                             0L, 0L, 0L, 0L, 0L, 0L, 1L, 66L, 0L, 0L, 1153L, 20L, 0L, 0L, 
                                                             86L, 0L, 72L, 0L, 0L, 0L, 0L, 0L, 0L, 490L, 77L, 0L, 0L, 0L, 
                                                             0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), .Dim = c(31L, 4L)))
test_that("Get spawning adults returns the expected values", {
  
  set.seed(2021)
  spawning_adults <- get_spawning_adults(year = year, adults = adults, hatch_adults = hatch_adults, seeds = seeds)
  expect_equal(spawning_adults, expected_spawners)
})


# Tests spawn success function
init_adults <- expected_spawners$init_adults
min_spawn_habitat <- apply(spawning_habitat[ , 10:12, year], 1, min)

expected_juveniles <- structure(c(0, 5306.85036682699, 577739.689748696, 0, 0, 12177675.2184342, 
                                  247378.914905372, 0, 0, 769594.78175614, 0, 590694.950715528, 
                                  0, 0, 0, 0, 0, 0, 3369327.32347569, 794502.377058433, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                                .Dim = c(31L, 4L), 
                                .Dimnames = list(c("Upper Sacramento River", "Antelope Creek", 
                                                   "Battle Creek", "Bear Creek", "Big Chico Creek", "Butte Creek", 
                                                   "Clear Creek", "Cottonwood Creek", "Cow Creek", "Deer Creek", 
                                                   "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek", 
                                                   "Upper-mid Sacramento River", "Sutter Bypass", "Bear River", 
                                                   "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                                                   "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River", 
                                                   "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River", 
                                                   "Tuolumne River", "San Joaquin River"), c("fry", "", "", "")))
test_that("spawn success function returns the expected value", {
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat)
  expect_equal(juveniles, expected_juveniles)
})
