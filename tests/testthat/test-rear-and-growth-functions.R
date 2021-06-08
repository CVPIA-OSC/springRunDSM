library(testthat)
library(springRunDSM)


survival_rate <- structure(c(0.970687769248644, 0.926162679369688, 0.0292171120559602,
                             0.993172867577862, 0.982175316206257, 0.116773215187387, 0.996740770796031,
                             0.991441080750038, 0.217490944447593, 1, 1, 1), .Dim = 3:4)

juveniles <- structure(c(0, 11573957.5840594, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 3:4)
growth <- structure(c(0.0030986844080139, 0, 0, 0, 0.996809996864753, 0.508311476301429,
                      0, 0, 9.13187272335581e-05, 0.491688523698443, 0.813818359404653,
                      0, 0, 1.27897692436818e-13, 0.186181640595347, 1), .Dim = c(4L,
                                                                                  4L), .Dimnames = list(c("s", "m", "l", "vl"), c("s", "m", "l",  "vl")))
# Tests rear fucntion
expected_rearing_output <- structure(c(0, 33215.9371435199, 0, 0, 10685172.7508171, 0, 0,
                                       978.879002963796, 0, 0, 0, 0), .Dim = 3:4, .Dimnames = list(NULL,
                                                                                                   c("s", "m", "l", "vl")))
test_that('The rearing function returns the expected values for year 1', {
  expect_equal(rear(juveniles = juveniles, survival_rate = survival_rate, growth = growth),
               expected_rearing_output)
})

# Tests growth functions
expected_growth <- structure(c(0.0045914028239995, 0, 0, 0, 0.995346079527392, 0.508197615704544, 
                               0, 0, 6.2517648608762e-05, 0.491802384294065, 0.750034293109025, 
                               0, 0, 1.39133149446025e-12, 0.249965706890975, 1), 
                             .Dim = c(4L,  4L), .Dimnames = list(c("s", "m", "l", "vl"), c("s", "m", "l", "vl")))

test_that('The growth() function returns the expected value', {
  set.seed(2021)
  growth <- growth()
  expect_equal(growth, expected_growth)
})

expected_growth_floodplain <- structure(c(0.00353734264565751, 0, 0, 0, 0.898316948523529, 
                                          0.389212293704814, 0, 0, 0.0980860690180065, 0.603923710077691, 
                                          0.579937237124007, 0, 5.96398128066e-05, 0.00686399621749566, 
                                          0.420062762875993, 1, 0.00248328246731551, 0, 0, 0, 0.801287817519667, 
                                          0.270226971705083, 0, 0, 0.196109620387404, 0.716045035861317, 
                                          0.409840181138989, 0, 0.0001192796256132, 0.0137279924336, 0.590159818861011, 
                                          1, 0.00142922228897352, 0, 0, 0, 0.704258686515805, 0.151241649705353, 
                                          0, 0, 0.294133171756802, 0.828166361644943, 0.239743125153972, 
                                          0, 0.0001789194384198, 0.0205919886497043, 0.760256874846028, 
                                          1, 0.000375162110631526, 0, 0, 0, 0.607229555511942, 0.0322563277056229, 
                                          0, 0, 0.3921567231262, 0.940287687428568, 0.0696460691689542, 
                                          0, 0.0002385592512264, 0.0274559848658086, 0.930353930831046, 
                                          1), .Dim = c(4L, 4L, 4L), .Dimnames = list(c("s", "m", "l", "vl"
                                          ), c("s", "m", "l", "vl"), c("1 week flooded", "2 weeks flooded", 
                                                                       "3 weeks flooded", "4 weeks flooded")))

test_that('The growth_floodplain() function returns the expected value', {
  set.seed(2021)
  growth_floodplain <- growth_floodplain()
  expect_equal(growth_floodplain, expected_growth_floodplain)
})