library(testthat)
library(springRunDSM)
# tests for survival functions
# Lists inputs to use in testing
year <- 1
month <- 9
aveT20 <- c(0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L,
            0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 1L)
maxT25 <- c(0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L,
            1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L)
aveT20D <- c(1L, 1L)
maxT25D <- 0:1
high_predation <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
                    0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L)
ws_strand <- c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
               0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L)

mean_egg_temp_effect <- c(0.900631877, 0.879803496, 0.876306213, 0.843355432, 0.846055568, 
                          0.896491346, 0.900631877, 0.7941397695, 0.900685118, 0.900685118, 
                          0.843355432, 0.8098417915, 0.843355432, 0.835920241, 0.843355432, 
                          0, 0, 0.809613829, 0.87965336, 0.884765615, 0, 0, 0.8457998405, 
                          0, 0.8091012, 0.8178536045, 0.8611316565, 0.6133825745, 0.869917628, 
                          0.893724672, 0)

# Tests surv_juv_rear survival function
expected_surv_juv_rear <- list(inchannel = structure(c(0.00714123269831683, 0.0306289144194922,
                                                       0.0622863095352219, 1),
                                                     .Dim = c(1L, 4L),
                                                     .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
                               floodplain = structure(c(0.0304552063499971, 0.110137287888533, 0.189997758240063, 1),
                                                      .Dim = c(1L, 4L),
                                                      .Dimnames = list("Upper Sacramento River", c("s", "m", "l", "vl"))))

test_that('The surv_juv_rear function returns the expected values for year 1 month 9 watershed 1', {
  expect_equal(surv_juv_rear(max_temp_thresh = maxT25[1],
                             avg_temp_thresh = aveT20[1],
                             high_predation = high_predation[1],
                             contact_points = params$contact_points[1],
                             prop_diversions = params$proportion_diverted[1],
                             total_diversions = params$total_diverted[1],
                             stranded = ws_strand[1],
                             weeks_flooded = params$weeks_flood[1],
                             ..surv_juv_rear_int = params$..surv_juv_rear_int[1],
                             .surv_juv_rear_contact_points = params$.surv_juv_rear_contact_points,
                             ..surv_juv_rear_contact_points = params$..surv_juv_rear_contact_points,
                             .surv_juv_rear_prop_diversions = params$.surv_juv_rear_prop_diversions,
                             ..surv_juv_rear_prop_diversions = params$..surv_juv_rear_prop_diversions,
                             .surv_juv_rear_total_diversions = params$.surv_juv_rear_total_diversions,
                             ..surv_juv_rear_total_diversions = params$..surv_juv_rear_total_diversions,
                             .avg_temp_thresh = params$.surv_juv_rear_avg_temp_thresh,
                             .high_predation = params$.surv_juv_rear_high_predation,
                             .stranded = params$.surv_juv_rear_stranded,
                             .medium = params$.surv_juv_rear_medium,
                             .large = params$.surv_juv_rear_large,
                             .floodplain = params$.surv_juv_rear_floodplain,
                             min_survival_rate = params$min_survival_rate,
                             stochastic = FALSE),
               expected_surv_juv_rear)
})
# 
# # Tests surv_juv_delta survival function
# expected_delta_juv_surv <- structure(c(2.28347300347935e-06, 1e-04, 1.00310951502147e-05, 
#                                        1e-04, 2.10874637097035e-05, 1e-04, 1, 1), 
#                                      .Dim = c(2L, 4L), 
#                                      .Dimnames = list(NULL, c("s", "m", "l", "vl")))
# 
# test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
#   expect_equal(surv_juv_delta(max_temp_thresh = maxT25D,
#                               avg_temp_thresh = aveT20D,
#                               high_predation = delta_prop_high_predation,
#                               contact_points = delta_contact_points,
#                               prop_diverted = delta_proportion_diverted,
#                               total_diverted = delta_total_diverted),
#                expected_delta_juv_surv)
# })
# 
# # Tests surv_juv_bypass survival function
# expected_bypass_juv_surv <- structure(c(1e-04, 1e-04, 1e-04, 1), 
#                                       .Dim = c(1L, 4L), 
#                                       .Dimnames = list(NULL, c("s", "m", "l", "vl")))
# 
# test_that('The bypass_juv_surv function returns the expected values for year 1 month 9', {
#   expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
#                                avg_temp_thresh = aveT20[22],
#                                high_predation = 0),
#                expected_bypass_juv_surv)
# })
# 
# # Tests migratory survival for lower mid sac fish survival function
# expected_lms_mig_surv <-structure(c(0.98682029951278, 0.996937462511772, 0.998538504859141, 
#                                     0.998538504859141), 
#                                   .Dim = c(1L, 4L), 
#                                   .Dimnames = list(NULL, c("s", "m", "l", "vl")))
# 
# test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
#   expect_equal(surv_juv_outmigration_sac(flow_cms = upper_sacramento_flows[month, year],
#                                          avg_temp = avg_temp[21, month, year],
#                                          total_diversions = total_diverted[21],
#                                          prop_diversions = proportion_diverted[21]),
#                expected_lms_mig_surv)
# })
# 
# 
# # Tests migratory survival for san joaquin fish survival function
# expected_lms_mig_surv <- structure(c(0.0293122307513563, 0.117118990875781, 0.218061322644411,
#                                      0.218061322644411), .Dim = c(1L, 4L),
#                                    .Dimnames = list(NULL,
#                                                     c("s", "m", "l", "vl")))
# 
# test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
#   expect_equal(surv_juv_outmigration_san_joaquin(),
#                expected_lms_mig_surv)
# })
# 
# # Tests migratory survival for sac delta outmigration survival function
# expected_sac_delta_mig_surv <- c(s = 0.362285441652534, m = 0.44305372307621, l = 0.526441379341886,
#                                  vl = 0.526441379341886)
# test_that('The migratory_juv_surv function for sac delta returns the expected values for row one of year 1 month 9', {
#   expect_equal(surv_juv_outmigration_sac_delta(delta_flow = delta_inflow[month, year, ],
#                                                avg_temp = avg_temp_delta[month, year, ],
#                                                perc_diversions = delta_proportion_diverted * 100)[1,],
#                expected_sac_delta_mig_surv)
# })
# 
# # Test egg to fry survival function 
# expected_egg_to_fry_surv <- c(`Upper Sacramento River` = 0.55126648754873, `Antelope Creek` = 0.557465511034345, 
#                               `Battle Creek` = 0.555249545010465, `Bear Creek` = 0.534371105617283, 
#                               `Big Chico Creek` = 0.537367315773288, `Butte Creek` = 0.569124433535414, 
#                               `Clear Creek` = 0.569292247931292, `Cottonwood Creek` = 0.506799804331753, 
#                               `Cow Creek` = 0.570696629269701, `Deer Creek` = 0.569325901735189, 
#                               `Elder Creek` = 0.539482360132187, `Mill Creek` = 0.514366344433048, 
#                               `Paynes Creek` = 0.534371105617283, `Stony Creek` = 0.528387818867039, 
#                               `Thomes Creek` = 0.535652339961507, `Upper-mid Sacramento River` = 0, 
#                               `Sutter Bypass` = 0, `Bear River` = 0.512991581615581, `Feather River` = 0.437030519553356, 
#                               `Yuba River` = 0.508044472640408, `Lower-mid Sacramento River` = 0, 
#                               `Yolo Bypass` = 0, `American River` = 0.539767947824993, `Lower Sacramento River` = 0, 
#                               `Calaveras River` = 0.511435418526615, `Cosumnes River` = 0.520695295226667, 
#                               `Mokelumne River` = 0.545634566287998, `Merced River` = 0.388654548325646, 
#                               `Stanislaus River` = 0.551201577688213, `Tuolumne River` = 0.564926181881676, 
#                               `San Joaquin River` = 0)
# 
# 
# proportion_natural <- c(`Upper Sacramento River` = 1, `Antelope Creek` = 1, `Battle Creek` = 1, 
#                         `Bear Creek` = 1, `Big Chico Creek` = 1, `Butte Creek` = 0.9975, 
#                         `Clear Creek` = 1, `Cottonwood Creek` = 1, `Cow Creek` = 1, `Deer Creek` = 1, 
#                         `Elder Creek` = 1, `Mill Creek` = 1, `Paynes Creek` = 1, `Stony Creek` = 1, 
#                         `Thomes Creek` = 1, `Upper-mid Sacramento River` = 1, `Sutter Bypass` = 1, 
#                         `Bear River` = 1, `Feather River` = 0.145, `Yuba River` = 0.50875, 
#                         `Lower-mid Sacramento River` = 1, `Yolo Bypass` = 1, `American River` = 1, 
#                         `Lower Sacramento River` = 1, `Calaveras River` = 1, `Cosumnes River` = 1, 
#                         `Mokelumne River` = 1, `Merced River` = 1, `Stanislaus River` = 1, 
#                         `Tuolumne River` = 1, `San Joaquin River` = 1) 
# 
# test_that('The egg_to_fry survival function returns the expected values for year 1 month 9', {
#   expect_equal(surv_egg_to_fry(proportion_natural = proportion_natural,
#                                scour = prob_nest_scoured,
#                                temperature_effect = mean_egg_temp_effect),
#                expected_egg_to_fry_surv)
# })
# 
# # tests the surv_juv_outmigration_delta function'
# expected_surv_juv_outmigration <- structure(c(0.266668614822945, 2.26283033759458e-26, 1.49657237445669e-25,
#                                               3.67469661043849e-14, 0.266668614822945, 2.26283033759458e-26,
#                                               1.49657237445669e-25, 3.67469661043849e-14, 0.266668614822945,
#                                               2.26283033759458e-26, 1.49657237445669e-25, 3.67469661043849e-14,
#                                               0.373914118050784, 4.49218800782043e-26, 2.2667851513676e-25,
#                                               8.17576203365024e-14), .Dim = c(4L, 4L),
#                                             .Dimnames = list(c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"),
#                                                              c("s", "m", "l", "vl")))
# test_that('tests that the surv_juv_outmigration_delta function returns the correct value', {
#   expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = cc_gates_prop_days_closed[month],
#                                            hor_barr = 0,
#                                            freeport_flow = freeport_flows[month, year],
#                                            vernalis_flow = vernalis_flows[month, year],
#                                            stockton_flow = stockton_flows[month, year],
#                                            vernalis_temperature = vernalis_temps[month, year],
#                                            prisoners_point_temperature = prisoners_point_temps[month, year],
#                                            CVP_exp = CVP_exports[month, year],
#                                            SWP_exp = SWP_exports[month, year],
#                                            trap_trans = 0),
#                expected_surv_juv_outmigration)
# })
# 
# ## Tests survival functions with randomness (set.seed() for testing these)
# # Tests the rearing survival rates function
# expected_survival <- list(inchannel = structure(c(0.0474146321204507, 1e-04, 1e-04, 
#                                                   1e-04, 1e-04, 1e-04, 0.922791599706058, 1e-04, 1e-04, 1e-04, 
#                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.00968704285689289, 
#                                                   0.0224804459355071, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.00297660049574533, 
#                                                   1e-04, 1e-04, 1e-04, 0.00168068616550728, 1e-04, 1e-04, 0.179424877539889, 
#                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.981309915041057, 1e-04, 
#                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                   0.0412004913871866, 0.0917566459522768, 1e-04, 1e-04, 1e-04, 
#                                                   1e-04, 1e-04, 0.0129453039222223, 1e-04, 1e-04, 1e-04, 0.00734129944151178, 
#                                                   1e-04, 1e-04, 0.314913124015171, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                   1e-04, 0.991021426691332, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.082850568755141, 0.175176926673678, 
#                                                   1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0268311947312388, 1e-04, 
#                                                   1e-04, 1e-04, 0.0153092405722624, 1e-04, 1e-04, 1, 1, 1, 1, 1, 
#                                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
#                                                   1, 1, 1, 1, 1), .Dim = c(31L, 4L)), 
#                           floodplain = structure(c(0.0784415434949796, 
#                                                    1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.940480614983144, 1e-04, 
#                                                    1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                    0.00968704285689289, 0.44862428487003, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                    1e-04, 0.00297660049574533, 1e-04, 1e-04, 1e-04, 0.00168068616550728, 
#                                                    1e-04, 1e-04, 0.266490336879093, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                    1e-04, 0.985766000781812, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                    1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0412004913871866, 0.781387307076476, 
#                                                    1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.0129453039222223, 1e-04, 
#                                                    1e-04, 1e-04, 0.00734129944151178, 1e-04, 1e-04, 0.427128095808306, 
#                                                    1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 0.993175648244178, 1e-04, 
#                                                    1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                    0.082850568755141, 0.882546534408116, 1e-04, 1e-04, 1e-04, 1e-04, 
#                                                    1e-04, 0.0268311947312388, 1e-04, 1e-04, 1e-04, 0.0153092405722624, 
#                                                    1e-04, 1e-04, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
#                                                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 4L)), 
#                           sutter = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
#                           yolo = structure(c(0.01,  0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
#                           delta = structure(c(2.04287534292905e-06,  1e-04, 8.9741782110702e-06, 1e-04, 1.88656248371699e-05, 1e-04, 1, 1), .Dim = c(2L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))))
# 
# test_that("get_rearing_survival returns the expected result", {
#   set.seed(2021)
#   survival <- get_rearing_survival_rates(year = year, month = month, scenario = 0)
#   expect_equal(survival, expected_survival)
# })
# 
# expected_migratory_survival <- list(delta = structure(c(0.266668614822945, 2.26283033759458e-26, 
#                                                         1.49657237445669e-25, 3.67469661043849e-14, 0.266668614822945, 
#                                                         2.26283033759458e-26, 1.49657237445669e-25, 3.67469661043849e-14, 
#                                                         0.266668614822945, 2.26283033759458e-26, 1.49657237445669e-25, 
#                                                         3.67469661043849e-14, 0.373914118050784, 4.49218800782043e-26, 
#                                                         2.2667851513676e-25, 8.17576203365024e-14), 
#                                                       .Dim = c(4L, 4L), .Dimnames = list(
#                                                         c("northern_fish", "cosumnes_mokelumne_fish", "calaveras_fish", 
#                                                           "southern_fish"), c("s", "m", "l", "vl"))), 
#                                     san_joaquin = structure(c(0.0293122307513563, 0.117118990875781, 0.218061322644411, 0.218061322644411), .Dim = c(1L,  4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
#                                     uppermid_sac = structure(c(0.991452069104508, 0.998008222596834, 0.999049070161138, 0.999049070161138), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
#                                     lowermid_sac = structure(c(0.993388292417814,  0.998467557065212, 0.999268985238279, 0.999268985238279), .Dim = c(1L,  4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
#                                     lower_sac = structure(c(0.993061995384178, 0.998390487063658, 0.999232112134422, 0.999232112134422), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))), 
#                                     sutter = structure(c(0.01,  0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))),
#                                     yolo = structure(c(0.01, 0.01, 0.01, 1), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", "m", "l", "vl"))))
# 
# test_that("get_migratory_survival returns the expected result", {
#   set.seed(2021)
#   migratory_survival <- get_migratory_survival_rates(year = year, month = month)[1:7]
#   expect_equal(migratory_survival, expected_migratory_survival)
# })