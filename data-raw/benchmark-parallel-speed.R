library(tictoc)
library(springRunDSM)
library(parallel)
list2env(load_2019_baseline_data(), envir = .GlobalEnv)

number_of_cores <- detectCores()

seeded_adults <- spring_run_model(seeds = NULL)
results <- spring_run_model(scenario = NULL, seeds = seeded_adults)

names(results)

dim(results$spawners)
results_df <- dplyr::tibble(
  watersheds = rep(watershed_attributes$watershed, 20),
  year = rep(1:20, 20)
)
# output <- fall_run_model(seeds = seeded_adults)

# 
# run_model <- function() {
#   seeded_adults <- fall_run_model()
#   output <- fall_run_model(seeds = seeded_adults)
#   return(output)
# }

# tic('one model run')
# r <- run_model()
# toc()

# tic('testing parallel run model')
# results <- parallel::mclapply(1:20, run_model, mc.cores = number_of_cores)
# toc()
