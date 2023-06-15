library(springRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

source("calibration/fitness.R")
source("calibration/update-params.R")

params <- DSMCalibrationData::set_synth_years(springRunDSM::params)

best_previous_solution <- readr::read_rds("calibration/calibration-results-2023.rds")@solution

# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -spring_run_fitness(
              known_adults = DSMCalibrationData::grandtab_observed$spring,
              seeds = DSMCalibrationData::grandtab_imputed$spring,
              params = params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
              x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27]
            ),
          lower = c(2.5, rep(-3.5, 26)),
          upper = rep(3.5, 27),
          popSize = 150,
          maxiter = 10000,
          run = 50,
          parallel = TRUE,
          pmutation = .3, 
          suggestions = best_previous_solution) # <- remove this argument if wanting to start from zero
                                                # its a good idea to start from scractch when doing "annual" calibrations

readr::write_rds(res, paste0("calibration/res-", Sys.time(), ".rds"))

res <- readr::read_rds("calibration/res-2023-06-09.rds")

# Evaluate Results ------------------------------------

keep <- c(2, 3, 6, 7, 10, 12, 19, 20)
r1_solution <- res@solution[1, ]

r1_params <- update_params(x = r1_solution, springRunDSM::params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- spring_run_model(seeds = DSMCalibrationData::grandtab_imputed$spring, mode = "calibrate",
                           ..params = r1_params,
                           stochastic = FALSE)


r1_nat_spawners <- as_tibble(r1_sim[keep, ,drop = F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated",
         year = readr::parse_number(year) + 5)



r1_observed <- as_tibble((1 - springRunDSM::params$proportion_hatchery[keep]) * DSMCalibrationData::grandtab_observed$spring[keep,, drop=F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed", year = as.numeric(year) - 1997) %>%
  filter(!is.na(spawners),
         year > 5)



r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)


r1_eval_df %>%
  ggplot(aes(year, spawners, color = type)) + geom_line() + facet_wrap(~watershed, scales = "free_y")

r1_eval_df %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1)

x <- r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )


r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )

