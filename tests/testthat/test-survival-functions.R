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
expected_surv_juv_rear <- list(inchannel = structure(c(0.00820006173903215, 0.0350473263273152, 
                                                       0.0709373073656477, 1), .Dim = c(1L, 4L), .Dimnames = list("Upper Sacramento River", 
                                                                                                                  c("s", "m", "l", "vl"))), floodplain = structure(c(0.031628440645925, 
                                                                                                                                                                     0.11525567525299, 0.199668187475351, 1), .Dim = c(1L, 4L), .Dimnames = list(
                                                                                                                                                                       "Upper Sacramento River", c("s", "m", "l", "vl"))))

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

# Tests surv_juv_delta survival function
expected_delta_juv_surv <- structure(c(0.0932457862245425, 1e-04, 0.0932457862245425, 1e-04, 
                                       0.0932457862245425, 1e-04, 1, 1), .Dim = c(2L, 4L), .Dimnames = list(
                                         c("North Delta", "South Delta"), c("s", "m", "l", "vl")))

test_that('The delta_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_delta(avg_temp = params$avg_temp_delta[month, year, "North Delta"],
                              max_temp_thresh = maxT25D,
                              avg_temp_thresh = aveT20D,
                              high_predation = params$delta_prop_high_predation,
                              contact_points = params$delta_contact_points,
                              prop_diverted = params$delta_proportion_diverted,
                              total_diverted = params$delta_total_diverted,
                              ..surv_juv_delta_int = params$..surv_juv_delta_int,
                              .surv_juv_delta_contact_points = params$.surv_juv_delta_contact_points,
                              ..surv_juv_delta_contact_points = params$..surv_juv_delta_contact_points,
                              .surv_juv_delta_total_diverted = params$.surv_juv_delta_total_diverted,
                              ..surv_juv_delta_total_diverted = params$..surv_juv_delta_total_diverted,
                              .avg_temp_thresh = params$.surv_juv_delta_avg_temp_thresh,
                              .high_predation = params$.surv_juv_delta_high_predation,
                              .prop_diverted = params$.surv_juv_delta_prop_diverted,
                              .medium = params$.surv_juv_delta_medium,
                              .large = params$.surv_juv_delta_large,
                              min_survival_rate = params$min_survival_rate,
                              stochastic = FALSE),
               expected_delta_juv_surv)
})

# Tests surv_juv_bypass survival function
expected_bypass_juv_surv <- structure(c(1e-04, 1e-04, 1e-04, 1),
                                      .Dim = c(1L, 4L),
                                      .Dimnames = list(NULL, c("s", "m", "l", "vl")))

test_that('The bypass_juv_surv function returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_bypass(max_temp_thresh = maxT25[22],
                               avg_temp_thresh = aveT20[22],
                               high_predation = 0, 
                               stochastic = FALSE),
               expected_bypass_juv_surv)
})
# 
# Tests migratory survival for lower mid sac fish survival function
expected_lms_mig_surv <-c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189)

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_sac(flow_cms = params$upper_sacramento_flows[month, year]),
               expected_lms_mig_surv)
})


# Tests migratory survival for san joaquin fish survival function
expected_lms_mig_surv <- structure(c(0.94909764929578, 0.987938508316242, 0.99422601791841, 
                                     0.99422601791841), .Dim = c(1L, 4L), .Dimnames = list(NULL, c("s", 
                                                                                                   "m", "l", "vl")))

test_that('The migratory_juv_surv function for lower mid sac returns the expected values for year 1 month 9', {
  expect_equal(surv_juv_outmigration_san_joaquin(),
               expected_lms_mig_surv)
})

# Test egg to fry survival function
expected_egg_to_fry_surv <- c(`Upper Sacramento River` = 0.864901519822763, `Antelope Creek` = 0.847846700197009, 
                              `Battle Creek` = 0.844476447788732, `Bear Creek` = 0.812722526524745, 
                              `Big Chico Creek` = 0.81551797501442, `Butte Creek` = 0.864091733769654, 
                              `Clear Creek` = 0.867711421455577, `Cottonwood Creek` = 0.765835779675094, 
                              `Cow Creek` = 0.867969846317653, `Deer Creek` = 0.867762716357489, 
                              `Elder Creek` = 0.813486630429588, `Mill Creek` = 0.780611301273453, 
                              `Paynes Creek` = 0.812722526524745, `Stony Creek` = 0.805365165352234, 
                              `Thomes Creek` = 0.812915297925268, `Upper-mid Sacramento River` = 0, 
                              `Sutter Bypass` = 0, `Bear River` = 0.780206508012689, `Feather River` = 0.825175411671839, 
                              `Yuba River` = 0.843971415812829, `Lower-mid Sacramento River` = 0, 
                              `Yolo Bypass` = 0, `American River` = 0.81565463055227, `Lower Sacramento River` = 0, 
                              `Calaveras River` = 0.779526430590034, `Cosumnes River` = 0.788519744938551, 
                              `Mokelumne River` = 0.829853071416654, `Merced River` = 0.591102892989836, 
                              `Stanislaus River` = 0.838319913135477, `Tuolumne River` = 0.861056692901221, 
                              `San Joaquin River` = 0)


proportion_natural <- c(`Upper Sacramento River` = 1, `Antelope Creek` = 1, `Battle Creek` = 1,
                        `Bear Creek` = 1, `Big Chico Creek` = 1, `Butte Creek` = 0.9975,
                        `Clear Creek` = 1, `Cottonwood Creek` = 1, `Cow Creek` = 1, `Deer Creek` = 1,
                        `Elder Creek` = 1, `Mill Creek` = 1, `Paynes Creek` = 1, `Stony Creek` = 1,
                        `Thomes Creek` = 1, `Upper-mid Sacramento River` = 1, `Sutter Bypass` = 1,
                        `Bear River` = 1, `Feather River` = 0.145, `Yuba River` = 0.50875,
                        `Lower-mid Sacramento River` = 1, `Yolo Bypass` = 1, `American River` = 1,
                        `Lower Sacramento River` = 1, `Calaveras River` = 1, `Cosumnes River` = 1,
                        `Mokelumne River` = 1, `Merced River` = 1, `Stanislaus River` = 1,
                        `Tuolumne River` = 1, `San Joaquin River` = 1)

test_that('The egg_to_fry survival function returns the expected values for year 1 month 9', {
  expect_equal(surv_egg_to_fry(proportion_natural = proportion_natural,
                               scour = params$prob_nest_scoured,
                               temperature_effect = params$mean_egg_temp_effect),
               expected_egg_to_fry_surv)
})
# 
# tests the surv_juv_outmigration_delta function'
expected_surv_juv_outmigration <- structure(c(0.266668614822945, 0.000123932662831837, 0.000819655793037249, 
                                              0.00566155265467863, 0.266668614822945, 0.000123932662831837, 
                                              0.000819655793037249, 0.00566155265467863, 0.266668614822945, 
                                              0.000123932662831837, 0.000819655793037249, 0.00566155265467863, 
                                              0.373914118050784, 0.000245928323835351, 0.00124096914866476, 
                                              0.0110614050155086), .Dim = c(4L, 4L), .Dimnames = list(c("northern_fish", 
                                                                                                        "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"), 
                                                                                                      c("s", "m", "l", "vl")))
test_that('tests that the surv_juv_outmigration_delta function returns the correct value', {
  expect_equal(surv_juv_outmigration_delta(prop_DCC_closed = params$cc_gates_prop_days_closed[month],
                                           hor_barr = 0,
                                           freeport_flow = params$freeport_flows[month, year],
                                           vernalis_flow = params$vernalis_flows[month, year],
                                           stockton_flow = params$stockton_flows[month, year],
                                           vernalis_temperature = params$vernalis_temps[month, year],
                                           prisoners_point_temperature = params$prisoners_point_temps[month, year],
                                           CVP_exp = params$CVP_exports[month, year],
                                           SWP_exp = params$SWP_exports[month, year],
                                           trap_trans = 0),
               expected_surv_juv_outmigration)
})

## Tests survival functions with randomness (set.seed() for testing these)
# Tests the rearing survival rates function
expected_survival <- list(inchannel = structure(c(0.125441180055978, 0.974056662027463, 
                                                  0.0375329131799549, 0.00174762600818678, 0.000243749049632853, 
                                                  0.00192953891680829, 0.788628326849881, 0.000117439586626742, 
                                                  0.00011560588700464, 0.0313236690546031, 0.916313794657251, 0.0314138642944959, 
                                                  0.0701154582020285, 0.000140912769650905, 0.000252297462985282, 
                                                  0.0382380715432165, 0.00263260560674758, 0.210949595131945, 0.549611416886024, 
                                                  0.261281446824522, 0.000172376589322069, 0.000683629600942927, 
                                                  0.0887468731026543, 0.00299300627354314, 0.987826021426153, 0.000104984109951456, 
                                                  0.0498317421119726, 0.0438085806068452, 0.0328971869136023, 0.00130718522983631, 
                                                  0.000143114034039866, 0.384512429269215, 0.977564967692279, 0.0441213989423155, 
                                                  0.00597096414408703, 0.000624624499828506, 0.00192954926534693, 
                                                  0.941969837473876, 0.000165781770842705, 0.00015977061643463, 
                                                  0.031337353728176, 0.916360277856402, 0.0314249961534745, 0.245534762820155, 
                                                  0.000172152231592856, 0.000252310743741265, 0.0384082183449072, 
                                                  0.0089400249119576, 0.539846310711765, 0.842783014990221, 0.279963956039602, 
                                                  0.000304002271720472, 0.00217673132211202, 0.217556392678981, 
                                                  0.00371386591468184, 0.988263668369107, 0.000119229737736365, 
                                                  0.138326836744771, 0.0677202577932641, 0.129922286067928, 0.00437372300998651, 
                                                  0.000226515251440526, 0.564756266000544, 0.978109365331924, 0.0453521712253206, 
                                                  0.00981419815288415, 0.000987502702866018, 0.00192955086452738, 
                                                  0.971150110104616, 0.000214925906076156, 0.000206065336134224, 
                                                  0.0313394695044576, 0.916367461372249, 0.0314267170767421, 0.400314253932719, 
                                                  0.000181803458867741, 0.000252312796236341, 0.0384346468082225, 
                                                  0.0144610225935579, 0.711197353003192, 0.918493845122121, 0.283092044698709, 
                                                  0.000383693666807792, 0.00353184898153592, 0.280499842421754, 
                                                  0.00385858570292711, 0.988331332983295, 0.000134386144099697, 
                                                  0.190721250772555, 0.073964221467895, 0.238727817722131, 0.00713018871196758, 
                                                  0.000280420286324018, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 
                                                                                                                  4L)), floodplain = structure(c(0.126980944356866, 0.974056662027463, 
                                                                                                                                                 0.0375329131799549, 0.00174762600818678, 0.000243749049632853, 
                                                                                                                                                 0.00192953891680829, 0.831809783866848, 0.000123117954310951, 
                                                                                                                                                 0.00011560588700464, 0.0313236690546031, 0.916313794657251, 0.0314138642944959, 
                                                                                                                                                 0.0701154582020285, 0.000140912769650905, 0.000252297462985282, 
                                                                                                                                                 0.0376550421372517, 0.00263260560674758, 0.210949595131945, 0.376695661679863, 
                                                                                                                                                 0.261281446824522, 0.000298010872995005, 0.000683629600942927, 
                                                                                                                                                 0.0655814343799477, 0.00299300627354314, 0.987826021426153, 0.000104984109951456, 
                                                                                                                                                 0.0424861714848881, 0.0438085806068452, 0.0636391581245602, 0.00130718522983631, 
                                                                                                                                                 0.000264552675689124, 0.387787560226362, 0.977564967692279, 0.0441213989423155, 
                                                                                                                                                 0.00597096414408703, 0.000624624499828506, 0.00192954926534693, 
                                                                                                                                                 0.955194020186611, 0.000182126925134328, 0.00015977061643463, 
                                                                                                                                                 0.031337353728176, 0.916360277856402, 0.0314249961534745, 0.245534762820155, 
                                                                                                                                                 0.000172152231592856, 0.000252310743741265, 0.0382719068802503, 
                                                                                                                                                 0.0089400249119576, 0.539846310711765, 0.72639205998998, 0.279963956039602, 
                                                                                                                                                 0.000402779711139126, 0.00217673132211202, 0.179777024876694, 
                                                                                                                                                 0.00371386591468184, 0.988263668369107, 0.000119229737736365, 
                                                                                                                                                 0.124325443022557, 0.0677202577932641, 0.222421094167108, 0.00437372300998651, 
                                                                                                                                                 0.000349398430445447, 0.568104205841275, 0.978109365331924, 0.0453521712253206, 
                                                                                                                                                 0.00981419815288415, 0.000987502702866018, 0.00192955086452738, 
                                                                                                                                                 0.977765192255139, 0.000236697254938572, 0.000206065336134224, 
                                                                                                                                                 0.0313394695044576, 0.916367461372249, 0.0314267170767421, 0.400314253932719, 
                                                                                                                                                 0.000181803458867741, 0.000252312796236341, 0.0383695339600307, 
                                                                                                                                                 0.0144610225935579, 0.711197353003192, 0.84804906785186, 0.283092044698709, 
                                                                                                                                                 0.000453771752742799, 0.00353184898153592, 0.247455060305143, 
                                                                                                                                                 0.00385858570292711, 0.988331332983295, 0.000134386144099697, 
                                                                                                                                                 0.17737889856605, 0.073964221467895, 0.36654742668121, 0.00713018871196758, 
                                                                                                                                                 0.000370988884224001, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(31L, 
                                                                                                                                                                                                                 4L)), sutter = structure(c(0.00297377321675923, 0.00594563539714444, 
                                                                                                                                                                                                                                            0.00705725176223706, 1), .Dim = c(1L, 4L), .Dimnames = list("Yolo Bypass", 
                                                                                                                                                                                                                                                                                                        c("s", "m", "l", "vl"))), yolo = structure(c(0.00297377321675923, 
                                                                                                                                                                                                                                                                                                                                                     0.00594563539714444, 0.00705725176223706, 1), .Dim = c(1L, 4L
                                                                                                                                                                                                                                                                                                                                                     ), .Dimnames = list("Yolo Bypass", c("s", "m", "l", "vl"))), 
                          delta = structure(c(0.0932457862245425, 1.09977606108503e-07, 
                                              0.0932457862245425, 1.09977606108503e-07, 0.0932457862245425, 
                                              1.09977606108503e-07, 1, 1), .Dim = c(2L, 4L), .Dimnames = list(
                                                c("North Delta", "South Delta"), c("s", "m", "l", "vl"
                                                ))))
test_that("get_rearing_survival returns the expected result", {
  set.seed(2021)
  survival <- get_rearing_survival(year, month,
                                   survival_adjustment = scenario_data$survival_adjustment,
                                   mode = "seed",
                                   avg_temp = params$avg_temp,
                                   avg_temp_delta = params$avg_temp_delta,
                                   prob_strand_early = params$prob_strand_early,
                                   prob_strand_late = params$prob_strand_late,
                                   proportion_diverted = params$proportion_diverted,
                                   total_diverted = params$total_diverted,
                                   delta_proportion_diverted = params$delta_proportion_diverted,
                                   delta_total_diverted = params$delta_total_diverted,
                                   weeks_flooded = params$weeks_flooded,
                                   prop_high_predation = params$prop_high_predation,
                                   contact_points = params$contact_points,
                                   delta_contact_points = params$delta_contact_points,
                                   delta_prop_high_predation = params$delta_prop_high_predation,
                                   ..surv_juv_rear_int = params$..surv_juv_rear_int,
                                   .surv_juv_rear_contact_points = params$.surv_juv_rear_contact_points,
                                   ..surv_juv_rear_contact_points = params$..surv_juv_rear_contact_points,
                                   .surv_juv_rear_prop_diversions = params$.surv_juv_rear_prop_diversions,
                                   ..surv_juv_rear_prop_diversions = params$..surv_juv_rear_prop_diversions,
                                   .surv_juv_rear_total_diversions = params$.surv_juv_rear_total_diversions,
                                   ..surv_juv_rear_total_diversions = params$..surv_juv_rear_total_diversions,
                                   ..surv_juv_bypass_int = params$..surv_juv_bypass_int,
                                   ..surv_juv_delta_int = params$..surv_juv_delta_int,
                                   .surv_juv_delta_contact_points = params$.surv_juv_delta_contact_points,
                                   ..surv_juv_delta_contact_points = params$..surv_juv_delta_contact_points,
                                   .surv_juv_delta_total_diverted = params$.surv_juv_delta_total_diverted,
                                   ..surv_juv_delta_total_diverted = params$..surv_juv_delta_total_diverted,
                                   .surv_juv_rear_avg_temp_thresh = params$.surv_juv_rear_avg_temp_thresh,
                                   .surv_juv_rear_high_predation = params$.surv_juv_rear_high_predation,
                                   .surv_juv_rear_stranded = params$.surv_juv_rear_stranded,
                                   .surv_juv_rear_medium = params$.surv_juv_rear_medium,
                                   .surv_juv_rear_large = params$.surv_juv_rear_large,
                                   .surv_juv_rear_floodplain = params$.surv_juv_rear_floodplain,
                                   .surv_juv_bypass_avg_temp_thresh = params$.surv_juv_bypass_avg_temp_thresh,
                                   .surv_juv_bypass_high_predation = params$.surv_juv_bypass_high_predation,
                                   .surv_juv_bypass_medium = params$.surv_juv_bypass_medium,
                                   .surv_juv_bypass_large = params$.surv_juv_bypass_large,
                                   .surv_juv_bypass_floodplain = params$.surv_juv_bypass_floodplain,
                                   .surv_juv_delta_avg_temp_thresh = params$.surv_juv_delta_avg_temp_thresh,
                                   .surv_juv_delta_high_predation = params$.surv_juv_delta_high_predation,
                                   .surv_juv_delta_prop_diverted = params$.surv_juv_delta_prop_diverted,
                                   .surv_juv_delta_medium = params$.surv_juv_delta_medium,
                                   .surv_juv_delta_large = params$.surv_juv_delta_large,
                                   min_survival_rate = params$min_survival_rate,
                                   stochastic = FALSE)
  expect_equal(survival, expected_survival)
})

expected_migratory_survival <- list(uppermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189
), lowermid_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189
), lower_sac = c(s = 0.189, m = 0.189, l = 0.189, vl = 0.189), 
sutter = structure(c(0.0545323135100578, 0.0771079463942883, 
                     0.0840074506352684, 1), .Dim = c(1L, 4L), .Dimnames = list(
                       "Yolo Bypass", c("s", "m", "l", "vl"))), yolo = structure(c(0.0545323135100578, 
                                                                                   0.0771079463942883, 0.0840074506352684, 1), .Dim = c(1L, 
                                                                                                                                        4L), .Dimnames = list("Yolo Bypass", c("s", "m", "l", "vl"
                                                                                                                                        ))), san_joaquin = structure(c(0.94909764929578, 0.987938508316242, 
                                                                                                                                                                       0.99422601791841, 0.99422601791841), .Dim = c(1L, 4L), .Dimnames = list(
                                                                                                                                                                         NULL, c("s", "m", "l", "vl"))), delta = structure(c(0.266668614822945, 
                                                                                                                                                                                                                             0.000123932662831837, 0.000819655793037249, 0.00566155265467863, 
                                                                                                                                                                                                                             0.266668614822945, 0.000123932662831837, 0.000819655793037249, 
                                                                                                                                                                                                                             0.00566155265467863, 0.266668614822945, 0.000123932662831837, 
                                                                                                                                                                                                                             0.000819655793037249, 0.00566155265467863, 0.373914118050784, 
                                                                                                                                                                                                                             0.000245928323835351, 0.00124096914866476, 0.0110614050155086
                                                                                                                                                                         ), .Dim = c(4L, 4L), .Dimnames = list(c("northern_fish", 
                                                                                                                                                                                                                 "cosumnes_mokelumne_fish", "calaveras_fish", "southern_fish"
                                                                                                                                                                         ), c("s", "m", "l", "vl"))), bay_delta = 0.358)

test_that("get_migratory_survival returns the expected result", {
  migratory_survival <- get_migratory_survival(year, month,
                                               cc_gates_prop_days_closed = params$cc_gates_prop_days_closed,
                                               freeport_flows = params$freeport_flows,
                                               vernalis_flows = params$vernalis_flows,
                                               stockton_flows = params$stockton_flows,
                                               vernalis_temps = params$vernalis_temps,
                                               prisoners_point_temps = params$prisoners_point_temps,
                                               CVP_exports = params$CVP_exports,
                                               SWP_exports = params$SWP_exports,
                                               upper_sacramento_flows = params$upper_sacramento_flows,
                                               delta_inflow = params$delta_inflow,
                                               avg_temp_delta = params$avg_temp_delta,
                                               avg_temp = params$avg_temp,
                                               delta_proportion_diverted = params$delta_proportion_diverted,
                                               ..surv_juv_outmigration_sj_int = params$..surv_juv_outmigration_sj_int,
                                               .surv_juv_outmigration_san_joaquin_medium = params$.surv_juv_outmigration_san_joaquin_medium,
                                               .surv_juv_outmigration_san_joaquin_large = params$.surv_juv_outmigration_san_joaquin_large,
                                               min_survival_rate = params$min_survival_rate,
                                               stochastic = FALSE)
  expect_equal(migratory_survival, expected_migratory_survival)
})
