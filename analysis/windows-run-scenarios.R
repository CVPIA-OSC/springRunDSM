library(DSMscenario)
library(parallel)
library(doParallel)
library(purrr)
library(springRunDSM)
library(dplyr)

# set up for parallel processing ----------------------------------
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)


run_scenario <- function(scenario) {
  s <- spring_run_model(mode = 'seed', stochastic = TRUE)
  output <- spring_run_model(scenario = scenario, mode = 'simulate',
                           stochastic = TRUE, seed = s)

  # TODO pick metrics
  prop_nat <- colSums(output$spawners * output$proportion_natural, na.rm = TRUE)[20]
  juv_biomass <- colSums(output$juvenile_biomass)[20]

  return(list(
    prop_nat = prop_nat,
    juv_biomass = juv_biomass
  ))
}

clusterExport(cl, list('run_scenario', 'spring_run_model'))

run_scenarios_parallel <- function(scenario, number_of_runs = 5000) {
  parLapply(cl, 1:number_of_runs,
           fun = function(i) {
             run_scenario(scenario = scenario)
           })
}

number_of_runs <- 4000

baseline_results <- run_scenarios_parallel(DSMscenario::scenarios$NO_ACTION, number_of_runs)
scenario_1_results <- run_scenarios_parallel(DSMscenario::scenarios$ONE, number_of_runs)
scenario_2_results <- run_scenarios_parallel(DSMscenario::scenarios$TWO, number_of_runs)
scenario_3_results <- run_scenarios_parallel(DSMscenario::scenarios$THREE, number_of_runs)
scenario_4_results <- run_scenarios_parallel(DSMscenario::scenarios$FOUR, number_of_runs)
scenario_5_results <- run_scenarios_parallel(DSMscenario::scenarios$FIVE, number_of_runs)
scenario_6_results <- run_scenarios_parallel(DSMscenario::scenarios$SIX, number_of_runs)
scenario_7_results <- run_scenarios_parallel(DSMscenario::scenarios$SEVEN, number_of_runs)
scenario_8_results <- run_scenarios_parallel(DSMscenario::scenarios$EIGHT, number_of_runs)
scenario_9_results <- run_scenarios_parallel(DSMscenario::scenarios$NINE, number_of_runs)
scenario_10_results <- run_scenarios_parallel(DSMscenario::scenarios$TEN, number_of_runs)
scenario_11_results <- run_scenarios_parallel(DSMscenario::scenarios$ELEVEN, number_of_runs)
scenario_12_results <- run_scenarios_parallel(DSMscenario::scenarios$TWELVE, number_of_runs)
scenario_13_results <- run_scenarios_parallel(DSMscenario::scenarios$THIRTEEN, number_of_runs)

readr::write_rds(x =list(
  baseline_results,
  scenario_1_results,
  scenario_2_results,
  scenario_3_results,
  scenario_4_results,
  scenario_5_results,
  scenario_6_results,
  scenario_7_results,
  scenario_8_results,
  scenario_9_results,
  scenario_10_results,
  scenario_11_results,
  scenario_12_results,
  scenario_13_results
), "analysis/scenario-run-results-2023.rds")


baseline_prop_nat <- flatten_dbl(transpose(baseline_results)$prop_nat)
scenario_1_prop_nat <- flatten_dbl(transpose(scenario_1_results)$prop_nat)
scenario_2_prop_nat <- flatten_dbl(transpose(scenario_2_results)$prop_nat)
scenario_3_prop_nat <- flatten_dbl(transpose(scenario_3_results)$prop_nat)
scenario_4_prop_nat <- flatten_dbl(transpose(scenario_4_results)$prop_nat)
scenario_5_prop_nat <- flatten_dbl(transpose(scenario_5_results)$prop_nat)
scenario_6_prop_nat <- flatten_dbl(transpose(scenario_6_results)$prop_nat)
scenario_7_prop_nat <- flatten_dbl(transpose(scenario_7_results)$prop_nat)
scenario_8_prop_nat <- flatten_dbl(transpose(scenario_8_results)$prop_nat)
scenario_9_prop_nat <- flatten_dbl(transpose(scenario_9_results)$prop_nat)
scenario_10_prop_nat <- flatten_dbl(transpose(scenario_10_results)$prop_nat)
scenario_11_prop_nat <- flatten_dbl(transpose(scenario_11_results)$prop_nat)
scenario_12_prop_nat <- flatten_dbl(transpose(scenario_12_results)$prop_nat)
scenario_13_prop_nat <- flatten_dbl(transpose(scenario_13_results)$prop_nat)

plot(1:number_of_runs, cummean(baseline_prop_nat), main = "baseline")
plot(1:number_of_runs, cummean(scenario_1_prop_nat), main = "scenario 1")
plot(1:number_of_runs, cummean(scenario_2_prop_nat), main = "scenario 2")
plot(1:number_of_runs, cummean(scenario_3_prop_nat), main = "scenario 3")
plot(1:number_of_runs, cummean(scenario_4_prop_nat), main = "scenario 4")
plot(1:number_of_runs, cummean(scenario_5_prop_nat), main = "scenario 5")
plot(1:number_of_runs, cummean(scenario_6_prop_nat), main = "scenario 6")
plot(1:number_of_runs, cummean(scenario_7_prop_nat), main = "scenario 7")
plot(1:number_of_runs, cummean(scenario_8_prop_nat), main = "scenario 8")
plot(1:number_of_runs, cummean(scenario_9_prop_nat), main = "scenario 9")
plot(1:number_of_runs, cummean(scenario_10_prop_nat), main = "scenario 10")
plot(1:number_of_runs, cummean(scenario_11_prop_nat), main = "scenario 11")
plot(1:number_of_runs, cummean(scenario_12_prop_nat), main = "scenario 12")
plot(1:number_of_runs, cummean(scenario_13_prop_nat), main = "scenario 13")

(mean(scenario_1_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_2_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_3_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_4_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_5_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_6_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_7_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_8_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_9_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_10_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_11_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_12_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)
(mean(scenario_13_prop_nat) - mean(baseline_prop_nat)) / mean(baseline_prop_nat)

# close all cluster connections
closeAllConnections()
