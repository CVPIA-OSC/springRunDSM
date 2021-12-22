library(testthat)
library(springRunDSM)

#tests for adult functions
#set time
year <- 1
month <- 9
#set function variables
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

# Tests adult straying function ------------------------------------------------
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

#tests adult surv_en_route -----------------------------------------------------
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

# Tests prespawn survival ------------------------------------------------------
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

# Tests egg to fry surv --------------------------------------------------------
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

# Test get_spawning_adults ----------------------------------------------------
# this needs to be tested in both seed and sim mode since
# there are different code paths for each

#Seed Mode
#Deterministic
adults <- structure(c(12, 12, 286, 0, 12, 5746, 171, 12, 0, 471, 0, 377, 
                      0, 0, 0, 0, 0, 0, 12, 12, 0, 0, 0, 0, 0, 0, 12, 0, 12, 12, 0, 
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
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 31:30, .Dimnames = list(
                        c("Upper Sacramento River", "Antelope Creek", "Battle Creek", 
                          "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek", 
                          "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek", 
                          "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek", 
                          "Upper-mid Sacramento River", "Sutter Bypass", "Bear River", 
                          "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                          "Yolo Bypass", "American River", "Lower Sacramento River", 
                          "Calaveras River", "Cosumnes River", "Mokelumne River", "Merced River", 
                          "Stanislaus River", "Tuolumne River", "San Joaquin River"
                        ), NULL))

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

expected_get_spawning_adults_seed_mode_det <- list(init_adults = c(`Upper Sacramento River` = 12, `Antelope Creek` = 12, 
                                                             `Battle Creek` = 286, `Bear Creek` = 0, `Big Chico Creek` = 12, 
                                                             `Butte Creek` = 5746, `Clear Creek` = 170, `Cottonwood Creek` = 12, 
                                                             `Cow Creek` = 0, `Deer Creek` = 472, `Elder Creek` = 0, `Mill Creek` = 376, 
                                                             `Paynes Creek` = 0, `Stony Creek` = 0, `Thomes Creek` = 0, `Upper-mid Sacramento River` = 0, 
                                                             `Sutter Bypass` = 0, `Bear River` = 0, `Feather River` = 10, 
                                                             `Yuba River` = 12, `Lower-mid Sacramento River` = 0, `Yolo Bypass` = 0, 
                                                             `American River` = 0, `Lower Sacramento River` = 0, `Calaveras River` = 0, 
                                                             `Cosumnes River` = 0, `Mokelumne River` = 12, `Merced River` = 0, 
                                                             `Stanislaus River` = 12, `Tuolumne River` = 12, `San Joaquin River` = 0
), proportion_natural = c(`Upper Sacramento River` = 1, `Antelope Creek` = 1, 
                          `Battle Creek` = 1, `Bear Creek` = 1, `Big Chico Creek` = 1, 
                          `Butte Creek` = 0.9975, `Clear Creek` = 1, `Cottonwood Creek` = 1, 
                          `Cow Creek` = 1, `Deer Creek` = 1, `Elder Creek` = 1, `Mill Creek` = 1, 
                          `Paynes Creek` = 1, `Stony Creek` = 1, `Thomes Creek` = 1, `Upper-mid Sacramento River` = 1, 
                          `Sutter Bypass` = 1, `Bear River` = 1, `Feather River` = 0.145, 
                          `Yuba River` = 0.50875, `Lower-mid Sacramento River` = 1, `Yolo Bypass` = 1, 
                          `American River` = 1, `Lower Sacramento River` = 1, `Calaveras River` = 1, 
                          `Cosumnes River` = 1, `Mokelumne River` = 1, `Merced River` = 1, 
                          `Stanislaus River` = 1, `Tuolumne River` = 1, `San Joaquin River` = 1
), init_adults_by_month = structure(c(2, 2, 36, 0, 2, 718, 21, 
                                      2, 0, 59, 0, 47, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 2, 
                                      0, 2, 2, 0, 4, 4, 107, 0, 4, 2155, 64, 4, 0, 177, 0, 141, 0, 
                                      0, 0, 0, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 4, 0, 4, 4, 0, 4, 4, 107, 
                                      0, 4, 2155, 64, 4, 0, 177, 0, 141, 0, 0, 0, 0, 0, 0, 3, 4, 0, 
                                      0, 0, 0, 0, 0, 4, 0, 4, 4, 0, 2, 2, 36, 0, 2, 718, 21, 2, 0, 
                                      59, 0, 47, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 2, 0, 2, 
                                      2, 0), .Dim = c(31L, 4L), .Dimnames = list(c("Upper Sacramento River", 
                                                                                   "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek", 
                                                                                   "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek", 
                                                                                   "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek", 
                                                                                   "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass", 
                                                                                   "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                                                                                   "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River", 
                                                                                   "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River", 
                                                                                   "Tuolumne River", "San Joaquin River"), NULL)))

test_that("Get spawning (seed mode) returns the expected values (deterministic)", {
  res <- get_spawning_adults(year = 1, 
                             adults = adults, 
                             hatch_adults = hatch_adults, 
                             mode = "seed",
                             month_return_proportions = params$month_return_proportions,
                             prop_flow_natal = params$prop_flow_natal,
                             south_delta_routed_watersheds = params$south_delta_routed_watersheds,
                             cc_gates_days_closed = params$cc_gates_days_closed,
                             gates_overtopped = params$gates_overtopped,
                             tisdale_bypass_watershed = params$tisdale_bypass_watershed,
                             yolo_bypass_watershed = params$yolo_bypass_watershed,
                             migratory_temperature_proportion_over_20 = params$migratory_temperature_proportion_over_20,
                             natural_adult_removal_rate = params$natural_adult_removal_rate,
                             cross_channel_stray_rate = params$cross_channel_stray_rate,
                             stray_rate = params$stray_rate,
                             ..surv_adult_enroute_int = params$..surv_adult_enroute_int,
                             .adult_stray_intercept = params$.adult_stray_intercept,
                             .adult_stray_wild = params$.adult_stray_wild,
                             .adult_stray_natal_flow = params$.adult_stray_natal_flow,
                             .adult_stray_cross_channel_gates_closed = params$.adult_stray_cross_channel_gates_closed,
                             .adult_stray_prop_bay_trans = params$.adult_stray_prop_bay_trans,
                             .adult_stray_prop_delta_trans = params$.adult_stray_prop_delta_trans,
                             .adult_en_route_migratory_temp = params$.adult_en_route_migratory_temp,
                             .adult_en_route_bypass_overtopped = params$.adult_en_route_bypass_overtopped,
                             .adult_en_route_adult_harvest_rate = params$.adult_en_route_adult_harvest_rate,
                             stochastic = FALSE)
  
  expect_equal(res, expected_get_spawning_adults_seed_mode_det)
  
})

#Stochastic
expected_get_spawning_adults_seed_mode <- list(init_adults = c(12, 12, 286, 0, 12, 5746, 171, 12, 0, 471, 
                                                               0, 377, 0, 0, 0, 0, 0, 0, 10, 12, 0, 0, 0, 0, 0, 0, 12, 0, 12, 
                                                               12, 0), proportion_natural = c(`Upper Sacramento River` = 1, 
                                                                                              `Antelope Creek` = 1, `Battle Creek` = 1, `Bear Creek` = 1, `Big Chico Creek` = 1, 
                                                                                              `Butte Creek` = 0.9975, `Clear Creek` = 1, `Cottonwood Creek` = 1, 
                                                                                              `Cow Creek` = 1, `Deer Creek` = 1, `Elder Creek` = 1, `Mill Creek` = 1, 
                                                                                              `Paynes Creek` = 1, `Stony Creek` = 1, `Thomes Creek` = 1, `Upper-mid Sacramento River` = 1, 
                                                                                              `Sutter Bypass` = 1, `Bear River` = 1, `Feather River` = 0.145, 
                                                                                              `Yuba River` = 0.50875, `Lower-mid Sacramento River` = 1, `Yolo Bypass` = 1, 
                                                                                              `American River` = 1, `Lower Sacramento River` = 1, `Calaveras River` = 1, 
                                                                                              `Cosumnes River` = 1, `Mokelumne River` = 1, `Merced River` = 1, 
                                                                                              `Stanislaus River` = 1, `Tuolumne River` = 1, `San Joaquin River` = 1
                                                               ), init_adults_by_month = structure(c(1L, 1L, 26L, 0L, 1L, 750L, 
                                                                                                     27L, 3L, 0L, 53L, 0L, 42L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 3L, 0L, 
                                                                                                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 3L, 0L, 6L, 5L, 100L, 0L, 7L, 
                                                                                                     2051L, 73L, 6L, 0L, 152L, 0L, 139L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 
                                                                                                     4L, 0L, 0L, 0L, 0L, 0L, 0L, 5L, 0L, 5L, 5L, 0L, 3L, 4L, 112L, 
                                                                                                     0L, 1L, 2219L, 60L, 1L, 0L, 184L, 0L, 152L, 0L, 0L, 0L, 0L, 0L, 
                                                                                                     0L, 7L, 3L, 0L, 0L, 0L, 0L, 0L, 0L, 6L, 0L, 6L, 4L, 0L, 2L, 2L, 
                                                                                                     48L, 0L, 3L, 726L, 11L, 2L, 0L, 82L, 0L, 44L, 0L, 0L, 0L, 0L, 
                                                                                                     0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L), .Dim = c(31L, 
                                                                                                                                                                           4L)))

test_that("Get spawning (seed mode) returns the expected values (stochastic)", {
  set.seed(2021)
  res <- get_spawning_adults(year = 1, 
                             adults = adults, 
                             hatch_adults = hatch_adults, 
                             mode = "seed",
                             month_return_proportions = params$month_return_proportions,
                             prop_flow_natal = params$prop_flow_natal,
                             south_delta_routed_watersheds = params$south_delta_routed_watersheds,
                             cc_gates_days_closed = params$cc_gates_days_closed,
                             gates_overtopped = params$gates_overtopped,
                             tisdale_bypass_watershed = params$tisdale_bypass_watershed,
                             yolo_bypass_watershed = params$yolo_bypass_watershed,
                             migratory_temperature_proportion_over_20 = params$migratory_temperature_proportion_over_20,
                             natural_adult_removal_rate = params$natural_adult_removal_rate,
                             cross_channel_stray_rate = params$cross_channel_stray_rate,
                             stray_rate = params$stray_rate,
                             ..surv_adult_enroute_int = params$..surv_adult_enroute_int,
                             .adult_stray_intercept = params$.adult_stray_intercept,
                             .adult_stray_wild = params$.adult_stray_wild,
                             .adult_stray_natal_flow = params$.adult_stray_natal_flow,
                             .adult_stray_cross_channel_gates_closed = params$.adult_stray_cross_channel_gates_closed,
                             .adult_stray_prop_bay_trans = params$.adult_stray_prop_bay_trans,
                             .adult_stray_prop_delta_trans = params$.adult_stray_prop_delta_trans,
                             .adult_en_route_migratory_temp = params$.adult_en_route_migratory_temp,
                             .adult_en_route_bypass_overtopped = params$.adult_en_route_bypass_overtopped,
                             .adult_en_route_adult_harvest_rate = params$.adult_en_route_adult_harvest_rate,
                             stochastic = TRUE)
  
  expect_equal(res, expected_get_spawning_adults_seed_mode)
  
})
#Sim mode
#Deterministic
expected_get_spawning_adults_sim_mode_det <- 
  list(init_adults = c(`Upper Sacramento River` = 6623, `Antelope Creek` = 64, 
                       `Battle Creek` = 15534, `Bear Creek` = 16, `Big Chico Creek` = 47, 
                       `Butte Creek` = 5188, `Clear Creek` = 1219, `Cottonwood Creek` = 899, 
                       `Cow Creek` = 261, `Deer Creek` = 580, `Elder Creek` = 16, `Mill Creek` = 436, 
                       `Paynes Creek` = 24, `Stony Creek` = 16, `Thomes Creek` = 32, 
                       `Upper-mid Sacramento River` = 0, `Sutter Bypass` = 0, `Bear River` = 22, 
                       `Feather River` = 48038, `Yuba River` = 7796, `Lower-mid Sacramento River` = 0, 
                       `Yolo Bypass` = 0, `American River` = 13419, `Lower Sacramento River` = 0, 
                       `Calaveras River` = 46, `Cosumnes River` = 32, `Mokelumne River` = 2821, 
                       `Merced River` = 1355, `Stanislaus River` = 1466, `Tuolumne River` = 481, 
                       `San Joaquin River` = 0), proportion_natural = c(`Upper Sacramento River` = 0.00437868029022074, 
                                                                        `Antelope Creek` = 0.781248779298782, `Battle Creek` = 0.0188618513012627, 
                                                                        `Bear Creek` = 0, `Big Chico Creek` = 0.617019963787311, `Butte Creek` = 0.985350790567641, 
                                                                        `Clear Creek` = 0.155045106230918, `Cottonwood Creek` = 0.0322580609279131, 
                                                                        `Cow Creek` = 0, `Deer Creek` = 0.784482623365065, `Elder Creek` = 0, 
                                                                        `Mill Creek` = 0.853210813483758, `Paynes Creek` = 0, `Stony Creek` = 0, 
                                                                        `Thomes Creek` = 0.562498242192993, `Upper-mid Sacramento River` = 0, 
                                                                        `Sutter Bypass` = 0, `Bear River` = 0, `Feather River` = 0.000791040424682459, 
                                                                        `Yuba River` = 0.00641354532563436, `Lower-mid Sacramento River` = 0, 
                                                                        `Yolo Bypass` = 0, `American River` = 0, `Lower Sacramento River` = 0, 
                                                                        `Calaveras River` = 0.39130349716631, `Cosumnes River` = 0.562498242192993, 
                                                                        `Mokelumne River` = 0.0102800421736958, `Merced River` = 0.0132841318609497, 
                                                                        `Stanislaus River` = 0.0197817176137983, `Tuolumne River` = 0.0602910477565389, 
                                                                        `San Joaquin River` = 0), init_adults_by_month = structure(c(832, 
                                                                                                                                     9, 1950, 2, 6, 652, 153, 113, 33, 72, 2, 55, 3, 2, 4, 0, 0, 3, 
                                                                                                                                     6030, 979, 0, 0, 1684, 0, 6, 4, 354, 169, 184, 60, 0, 2494, 23, 
                                                                                                                                     5850, 6, 18, 1954, 459, 339, 98, 219, 6, 164, 9, 6, 12, 0, 0, 
                                                                                                                                     8, 18090, 2935, 0, 0, 5053, 0, 17, 12, 1061, 510, 552, 181, 0, 
                                                                                                                                     2474, 23, 5805, 6, 17, 1938, 455, 335, 98, 217, 6, 163, 9, 6, 
                                                                                                                                     12, 0, 0, 8, 17954, 2913, 0, 0, 5016, 0, 17, 12, 1056, 508, 548, 
                                                                                                                                     180, 0, 823, 9, 1929, 2, 6, 644, 152, 112, 32, 72, 2, 54, 3, 
                                                                                                                                     2, 4, 0, 0, 3, 5964, 969, 0, 0, 1666, 0, 6, 4, 350, 168, 182, 
                                                                                                                                     60, 0), .Dim = c(31L, 4L), .Dimnames = list(c("Upper Sacramento River", 
                                                                                                                                                                                   "Antelope Creek", "Battle Creek", "Bear Creek", "Big Chico Creek", 
                                                                                                                                                                                   "Butte Creek", "Clear Creek", "Cottonwood Creek", "Cow Creek", 
                                                                                                                                                                                   "Deer Creek", "Elder Creek", "Mill Creek", "Paynes Creek", "Stony Creek", 
                                                                                                                                                                                   "Thomes Creek", "Upper-mid Sacramento River", "Sutter Bypass", 
                                                                                                                                                                                   "Bear River", "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                                                                                                                                                                                   "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River", 
                                                                                                                                                                                   "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River", 
                                                                                                                                                                                   "Tuolumne River", "San Joaquin River"), NULL)))
test_that("Get spawning (sim mode) returns the expected values (deterministic)", {
  res <- get_spawning_adults(year = 1, 
                             adults = adults, 
                             hatch_adults = hatch_adults, 
                             mode = "simulate",
                             month_return_proportions = params$month_return_proportions,
                             prop_flow_natal = params$prop_flow_natal,
                             south_delta_routed_watersheds = params$south_delta_routed_watersheds,
                             cc_gates_days_closed = params$cc_gates_days_closed,
                             gates_overtopped = params$gates_overtopped,
                             tisdale_bypass_watershed = params$tisdale_bypass_watershed,
                             yolo_bypass_watershed = params$yolo_bypass_watershed,
                             migratory_temperature_proportion_over_20 = params$migratory_temperature_proportion_over_20,
                             natural_adult_removal_rate = params$natural_adult_removal_rate,
                             cross_channel_stray_rate = params$cross_channel_stray_rate,
                             stray_rate = params$stray_rate,
                             ..surv_adult_enroute_int = params$..surv_adult_enroute_int,
                             .adult_stray_intercept = params$.adult_stray_intercept,
                             .adult_stray_wild = params$.adult_stray_wild,
                             .adult_stray_natal_flow = params$.adult_stray_natal_flow,
                             .adult_stray_cross_channel_gates_closed = params$.adult_stray_cross_channel_gates_closed,
                             .adult_stray_prop_bay_trans = params$.adult_stray_prop_bay_trans,
                             .adult_stray_prop_delta_trans = params$.adult_stray_prop_delta_trans,
                             .adult_en_route_migratory_temp = params$.adult_en_route_migratory_temp,
                             .adult_en_route_bypass_overtopped = params$.adult_en_route_bypass_overtopped,
                             .adult_en_route_adult_harvest_rate = params$.adult_en_route_adult_harvest_rate,
                             stochastic = FALSE)
  
  expect_equal(res, expected_get_spawning_adults_sim_mode_det)
  
})

#Stochastic
expected_get_spawning_adults_sim_mode <- list(init_adults = c(6629, 55, 15562, 14, 46, 5145, 1222, 908, 
                                                              258, 567, 15, 438, 25, 16, 31, 0, 0, 20, 48030, 7823, 0, 0, 13409, 
                                                              0, 50, 39, 2815, 1373, 1460, 485, 0), proportion_natural = c(0.00377130783268505, 
                                                                                                                           0.818180330581217, 0.0192134685823579, 0, 0.630433412101278, 
                                                                                                                           0.984645267548197, 0.160392785565239, 0.0275330366153043, 0, 
                                                                                                                           0.779541308722873, 0, 0.863013501595091, 0, 0, 0.580643288247457, 
                                                                                                                           0, 0, 0, 0.000832812823583567, 0.007030550849667, 0, 0, 0, 0, 
                                                                                                                           0.47999904000192, 0.641023997374366, 0.0117229125498077, 0.019664965792792, 
                                                                                                                           0.0198630123381498, 0.0680412230842839, 0), init_adults_by_month = structure(c(861L, 
                                                                                                                                                                                                          5L, 1893L, 2L, 7L, 688L, 153L, 96L, 42L, 67L, 2L, 43L, 1L, 2L, 
                                                                                                                                                                                                          5L, 0L, 0L, 3L, 5960L, 1038L, 0L, 0L, 1708L, 0L, 5L, 6L, 334L, 
                                                                                                                                                                                                          189L, 178L, 59L, 0L, 2451L, 19L, 5776L, 5L, 18L, 1841L, 479L, 
                                                                                                                                                                                                          322L, 92L, 193L, 5L, 165L, 10L, 7L, 10L, 0L, 0L, 6L, 18114L, 
                                                                                                                                                                                                          2934L, 0L, 0L, 5007L, 0L, 17L, 16L, 1098L, 505L, 565L, 190L, 
                                                                                                                                                                                                          0L, 2485L, 24L, 5956L, 4L, 12L, 1972L, 437L, 360L, 99L, 211L, 
                                                                                                                                                                                                          6L, 184L, 12L, 5L, 11L, 0L, 0L, 10L, 18052L, 2826L, 0L, 0L, 4992L, 
                                                                                                                                                                                                          0L, 22L, 13L, 1038L, 497L, 549L, 176L, 0L, 832L, 7L, 1937L, 3L, 
                                                                                                                                                                                                          9L, 644L, 153L, 130L, 25L, 96L, 2L, 46L, 2L, 2L, 5L, 0L, 0L, 
                                                                                                                                                                                                          1L, 5904L, 1025L, 0L, 0L, 1702L, 0L, 6L, 4L, 345L, 182L, 168L, 
                                                                                                                                                                                                          60L, 0L), .Dim = c(31L, 4L)))

test_that("Get spawning (sim mode) returns the expected values (stochastic)", {
  set.seed(2021)
  res <- get_spawning_adults(year = 1, 
                             adults = adults, 
                             hatch_adults = hatch_adults, 
                             mode = "simulate",
                             month_return_proportions = params$month_return_proportions,
                             prop_flow_natal = params$prop_flow_natal,
                             south_delta_routed_watersheds = params$south_delta_routed_watersheds,
                             cc_gates_days_closed = params$cc_gates_days_closed,
                             gates_overtopped = params$gates_overtopped,
                             tisdale_bypass_watershed = params$tisdale_bypass_watershed,
                             yolo_bypass_watershed = params$yolo_bypass_watershed,
                             migratory_temperature_proportion_over_20 = params$migratory_temperature_proportion_over_20,
                             natural_adult_removal_rate = params$natural_adult_removal_rate,
                             cross_channel_stray_rate = params$cross_channel_stray_rate,
                             stray_rate = params$stray_rate,
                             ..surv_adult_enroute_int = params$..surv_adult_enroute_int,
                             .adult_stray_intercept = params$.adult_stray_intercept,
                             .adult_stray_wild = params$.adult_stray_wild,
                             .adult_stray_natal_flow = params$.adult_stray_natal_flow,
                             .adult_stray_cross_channel_gates_closed = params$.adult_stray_cross_channel_gates_closed,
                             .adult_stray_prop_bay_trans = params$.adult_stray_prop_bay_trans,
                             .adult_stray_prop_delta_trans = params$.adult_stray_prop_delta_trans,
                             .adult_en_route_migratory_temp = params$.adult_en_route_migratory_temp,
                             .adult_en_route_bypass_overtopped = params$.adult_en_route_bypass_overtopped,
                             .adult_en_route_adult_harvest_rate = params$.adult_en_route_adult_harvest_rate,
                             stochastic = TRUE)
  
  expect_equal(res, expected_get_spawning_adults_sim_mode)
  
})
# Tests spawn success function -------------------------------------------------
init_adults <- expected_get_spawning_adults_sim_mode$init_adults
min_spawn_habitat <- apply(params$spawning_habitat[ , 10:12, year], 1, min)

#Deterministic
expected_juveniles_det <- structure(c(5459334, 31462, 2433643, 0, 43682, 2878846, 540474, 
                                      799651, 0, 309549, 0, 217423, 0, 0, 30480, 0, 0, 0, 28779603, 
                                      4448455, 0, 0, 0, 0, 44982, 34137, 2604297, 905584, 1364319, 
                                      460736, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(31L, 4L), .Dimnames = list(
                                        c("Upper Sacramento River", "Antelope Creek", "Battle Creek", 
                                          "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek", 
                                          "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek", 
                                          "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek", 
                                          "Upper-mid Sacramento River", "Sutter Bypass", "Bear River", 
                                          "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                                          "Yolo Bypass", "American River", "Lower Sacramento River", 
                                          "Calaveras River", "Cosumnes River", "Mokelumne River", "Merced River", 
                                          "Stanislaus River", "Tuolumne River", "San Joaquin River"
                                        ), c("fry", "", "", "")))

test_that("spawn success function returns the expected value (deterministic)", {
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = params$prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat, 
                             stochastic = FALSE)
  expect_equal(juveniles, expected_juveniles_det)
})

#Stochastic
expected_juveniles <-structure(c(5410448, 35966, 2432508, 0, 48011, 2776943, 540878, 
                                 807142, 0, 250009, 0, 188376, 0, 0, 30356, 0, 0, 0, 28205687, 
                                 4592908, 0, 0, 0, 0, 61181, 34003, 2489851, 874485, 1284392, 
                                 529131, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(31L, 4L), .Dimnames = list(
                                   NULL, c("fry", "", "", "")))

test_that("spawn success function returns the expected value (stochastic)", {
  set.seed(2021)
  juveniles <- spawn_success(escapement = init_adults,
                             adult_prespawn_survival = expected_prespawn_surv,
                             egg_to_fry_survival = expected_egg_surv,
                             prob_scour = params$prob_nest_scoured,
                             spawn_habitat = min_spawn_habitat, 
                             stochastic = TRUE)
  expect_equal(juveniles, expected_juveniles)
})

