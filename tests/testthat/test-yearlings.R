test_that("errors when juvs/yearlings matrix has no rownames", {
  list2env(load_2019_baseline_data(), environment())
  sim_juvs <- matrix(c(1, 1, 1, 1,
                       1, 3, 4, 5, 
                       44, 5, 55, 23), 
                     ncol = 4, 
                     byrow = TRUE, 
                     dimnames = list(NULL,
                                     c("s", "m", "l", "vl"))
  )
  
  expect_error(yearling_growth(year = 1, yearlings = sim_juvs), 
               "yearlings/juvs data needs to be named by watershed, please confirm `rownames(yearlings)` returns the names of watersheds")
})

test_that("zero will always return zero", {
  list2env(load_2019_baseline_data(), environment())
  sim_juvs <- matrix(rep(0, 12), 
                     ncol = 4, 
                     byrow = TRUE, 
                     dimnames = list(c("Battle Creek", "Deer Creek", "Butte Creek"),
                                     c("s", "m", "l", "vl"))
  )
  
  d <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = 3:4, .Dimnames = list(
    NULL, c("s", "m", "l", "vl")))
  
  expect_equal(yearling_growth(year = 1, yearlings = sim_juvs), d)
})
