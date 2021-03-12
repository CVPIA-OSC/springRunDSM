test_that("errors when juvs/yearlings matrix has no rownames", {
  sim_juvs <- matrix(c(1, 1, 1, 1,
                       1, 3, 4, 5, 
                       44, 5, 55, 23), 
                     ncol = 4, 
                     byrow = TRUE, 
                     dimnames = list(NULL,
                                     c("s", "m", "l", "vl"))
  )
  
  expect_error(yearling_growth(year = 1, yearlings = sim_juvs))
})
