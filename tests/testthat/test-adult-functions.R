library(testthat)
library(springRunDSM)
# tests for adult functions
# Lists inputs to use in testing
# list2env(load_baseline_data(), envir = .GlobalEnv)
year <- 1
month <- 9
bypass_is_overtopped <- as.logical(params$tisdale_bypass_watershed + params$yolo_bypass_watershed)
avg_migratory_temp <- rowMeans(params$migratory_temperature_proportion_over_20[ , 10:12])
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
                           natal_flow = params$prop_flow_natal[ , year],
                           south_delta_watershed = params$south_delta_routed_watersheds,
                           cross_channel_gates_closed = params$cc_gates_days_closed[10]),
               expected_straying_output)
})

#tests adult surv_en_route
expected_surv_en_route <- c(`Upper Sacramento River` = 0.956116465919529, `Antelope Creek` = 0.956116465919529, 
                            `Battle Creek` = 0.956116465919529, `Bear Creek` = 0.956116465919529, 
                            `Big Chico Creek` = 0.956116465919529, `Butte Creek` = 0.956116465919529, 
                            `Clear Creek` = 0.956116465919529, `Cottonwood Creek` = 0.956116465919529, 
                            `Cow Creek` = 0.956116465919529, `Deer Creek` = 0.956116465919529, 
                            `Elder Creek` = 0.956116465919529, `Mill Creek` = 0.956116465919529, 
                            `Paynes Creek` = 0.956116465919529, `Stony Creek` = 0.956116465919529, 
                            `Thomes Creek` = 0.956116465919529, `Upper-mid Sacramento River` = 0.956116465919529, 
                            `Sutter Bypass` = 0.956906790642622, `Bear River` = 0.956116465919529, 
                            `Feather River` = 0.956116465919529, `Yuba River` = 0.956116465919529, 
                            `Lower-mid Sacramento River` = 0.956906790642622, `Yolo Bypass` = 0.956906790642622, 
                            `American River` = 0.956906790642622, `Lower Sacramento River` = 0.956906790642622, 
                            `Calaveras River` = 0.956675632780601, `Cosumnes River` = 0.956675632780601, 
                            `Mokelumne River` = 0.956675632780601, `Merced River` = 0.956501487996017, 
                            `Stanislaus River` = 0.956501487996017, `Tuolumne River` = 0.956501487996017, 
                            `San Joaquin River` = 0.956906790642622)

test_that('The adult enroute survival function returns the expected values for year 1', {
  expect_equal(surv_adult_enroute(migratory_temp = avg_migratory_temp,
                                  bypass_overtopped = bypass_is_overtopped,
                                  adult_harvest = adult_harvest_rate),
               expected_surv_en_route)
})

# Tests prespawn survival
expected_prespawn_surv <- c(`Upper Sacramento River` = 0.420646421660919, `Antelope Creek` = 0.241189912273981, 
                            `Battle Creek` = 0.234049413310684, `Bear Creek` = 0.420646421660919, 
                            `Big Chico Creek` = 0.420646421660919, `Butte Creek` = 0.241615820225701, 
                            `Clear Creek` = 0.309952843827822, `Cottonwood Creek` = 0.420646421660919, 
                            `Cow Creek` = 0.420646421660919, `Deer Creek` = 0.241099399333482, 
                            `Elder Creek` = 0.420646421660919, `Mill Creek` = 0.239563764315645, 
                            `Paynes Creek` = 0.420646421660919, `Stony Creek` = 0.420646421660919, 
                            `Thomes Creek` = 0.420646421660919, `Upper-mid Sacramento River` = 0.420646421660919, 
                            `Sutter Bypass` = 0.420646421660919, `Bear River` = 0.420646421660919, 
                            `Feather River` = 0.328768122131017, `Yuba River` = 0.248933189120103, 
                            `Lower-mid Sacramento River` = 0.420646421660919, `Yolo Bypass` = 0.420646421660919, 
                            `American River` = 0.420646421660919, `Lower Sacramento River` = 0.420646421660919, 
                            `Calaveras River` = 0.420646421660919, `Cosumnes River` = 0.420646421660919, 
                            `Mokelumne River` = 0.420646421660919, `Merced River` = 0.420646421660919, 
                            `Stanislaus River` = 0.420646421660919, `Tuolumne River` = 0.420646421660919, 
                            `San Joaquin River` = 0.420646421660919)

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
expected_egg_surv <- c(`Upper Sacramento River` = 0.864901519822763, `Antelope Creek` = 0.847846700197009, 
                       `Battle Creek` = 0.844476447788732, `Bear Creek` = 0.812722526524745, 
                       `Big Chico Creek` = 0.81551797501442, `Butte Creek` = 0.864091733769654, 
                       `Clear Creek` = 0.867711421455577, `Cottonwood Creek` = 0.765835779675094, 
                       `Cow Creek` = 0.867969846317653, `Deer Creek` = 0.867762716357489, 
                       `Elder Creek` = 0.813486630429588, `Mill Creek` = 0.780611301273453, 
                       `Paynes Creek` = 0.812722526524745, `Stony Creek` = 0.805365165352234, 
                       `Thomes Creek` = 0.812915297925267, `Upper-mid Sacramento River` = 0, 
                       `Sutter Bypass` = 0, `Bear River` = 0.780206508012689, `Feather River` = 0.825175411671839, 
                       `Yuba River` = 0.843971415812829, `Lower-mid Sacramento River` = 0, 
                       `Yolo Bypass` = 0, `American River` = 0.81565463055227, `Lower Sacramento River` = 0, 
                       `Calaveras River` = 0.779526430590034, `Cosumnes River` = 0.788519744938551, 
                       `Mokelumne River` = 0.829853071416654, `Merced River` = 0.591102892989836, 
                       `Stanislaus River` = 0.838319913135477, `Tuolumne River` = 0.861056692901221, 
                       `San Joaquin River` = 0)

test_that('The egg_to_fry survival function returns the expected values for year 1', {
  expect_equal(surv_egg_to_fry(proportion_natural = 1 - proportion_hatchery,
                               scour = params$prob_nest_scoured,
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