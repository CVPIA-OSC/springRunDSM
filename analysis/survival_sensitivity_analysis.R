library(tidyverse)

set.seed(123119)

sensi_seeds <- springRunDSM::spring_run_model(mode = "seed", 
                                              which_surv = NA)


sensitivity_spring_run_model <- function(scenario, scenarios, sensi_seeds) {
  
  which_surv = scenarios[scenario, ]$which_surv
  location_surv = scenarios[scenario, ]$location_surv
  month_surv = scenarios[scenario, ]$month_surv
  
  model_results <- springRunDSM::spring_run_model(mode = "simulate",
                                              seeds = sensi_seeds,
                                              which_surv = which_surv,
                                              location_surv = location_surv,
                                              month_surv = month_surv)
  
  output <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
    dplyr::mutate(location = springRunDSM::watershed_labels,
                  survival_target = which_surv,
                  location_target = location_surv,
                  month_target = month_surv,
                  id = scenario) |>
    dplyr::select(id, location, survival_target, location_target, month_target, `1`:`20`)
  
  return(output)
  
}

# run scenarios in parallel -------
library(tictoc) # measure time to run
library(parallel)
library(doParallel)

# Spring Run :
# Upper Sacramento River
# Antelope Creek
# Battle Creek
# Big Chico Creek
# Butte Creek
# Clear Creek
# Cottonwood Creek
# Deer Creek
# Mill Creek
# Upper-mid Sacramento River
# Sutter Bypass
# Feather River
# Yuba River
# Lower-mid Sacramento River
# Yolo Bypass
# Lower Sacramento River
# Mokelumne River
# Stanislaus River
# Tuolumne River
# San Joaquin River
# Delta
# Bay Delta

# scenarios to evaluate
rearing_watersheds <- DSMhabitat::watershed_lengths %>%
     filter(lifestage == "rearing") %>%
     filter(species == "sr") %>%
     pull(watershed)

scenarios1 <- expand.grid(location_surv = rearing_watersheds,
                          month_surv = c(11, 12, 1:5),
                          which_surv = c("juv_rear"))

scenarios2 <- expand.grid(location_surv = c("Yolo Bypass", "Sutter Bypass"), 
                          month_surv = c(11, 12, 1:5),
                          which_surv = c("juv_rear"))

scenarios3  <- expand.grid(location_surv = c("Lower-mid Sacramento River", 
                                             "Lower Sacramento River", "Sutter Bypass", 
                                             "Yolo Bypass",
                                             "Delta", "Bay Delta", "San Joaquin River",
                                             "Upper-mid Sacramento River"),
                           month_surv = c(11, 12, 1:5),
                           which_surv = "juv_migratory")

# pull spawning names from watersheds that have spawning for spring Run
spawning_watersheds <- DSMhabitat::watershed_lengths %>%
  filter(lifestage == "spawning") %>%
  filter(species == "sr") %>%
  pull(watershed)

scenarios4 <- expand.grid(location_surv = spawning_watersheds,
                          month_surv = NA,
                          which_surv = "egg_to_fry")

scenarios5 <- expand.grid(location_surv = c("North Delta", "South Delta"), 
                          month_surv = c(11, 12, 1:5),
                          which_surv = c("juv_rear"))


# set up for running function in parallel
no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)

clusterExport(cl, list("sensitivity_spring_run_model", "sensi_seeds",
                       "scenarios1", "scenarios2", "scenarios3", "scenarios4", 'scenarios5'))


tic("parallel 1")
scenario_results_list1 <- parLapply(cl, 1:nrow(scenarios1),
                                    fun = function(scenario) {
                                      sensitivity_spring_run_model(scenario, scenarios1, sensi_seeds)
                                    })
toc()

tic("parallel 2")
scenario_results_list2 <- parLapply(cl, 1:nrow(scenarios2),
                                    fun = function(scenario) {
                                      sensitivity_spring_run_model(scenario, scenarios2, sensi_seeds)
                                    })
toc()

tic("parallel 3")
scenario_results_list3 <- parLapply(cl, 1:nrow(scenarios3),
                                    fun = function(scenario) {
                                      sensitivity_spring_run_model(scenario, scenarios3, sensi_seeds)
                                    })
toc()

tic("parallel 4")
scenario_results_list4 <- parLapply(cl, 1:nrow(scenarios4),
                                    fun = function(scenario) {
                                      sensitivity_spring_run_model(scenario, scenarios4, sensi_seeds)
                                    })
toc()

tic("parallel 5")
scenario_results_list5 <- parLapply(cl, 1:nrow(scenarios5),
                                    fun = function(scenario) {
                                      sensitivity_spring_run_model(scenario, scenarios5, sensi_seeds)
                                    })
toc()

# combine into one
r1 <- scenario_results_list1 |> dplyr::bind_rows()
r2 <- scenario_results_list2 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r1$id))
r3 <- scenario_results_list3 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r2$id))
r4 <- scenario_results_list4 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r3$id))
r5 <- scenario_results_list5 |> dplyr::bind_rows() |> dplyr::mutate(id = id + max(r4$id))

# do nothing
model_results <- springRunDSM::spring_run_model(mode = "simulate",
                                            seeds = sensi_seeds,
                                            which_surv = NA,
                                            location_surv = NA,
                                            month_surv = NA)

do_nothing <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
  dplyr::mutate(location = springRunDSM::watershed_labels,
                survival_target = NA,
                location_target = NA,
                month_target = NA,
                id = max(r5$id) + 1) |>
  dplyr::select(id, location, survival_target, location_target, month_target, `1`:`20`)

results <- dplyr::bind_rows(r1, r2, r3, r4, r5, do_nothing)

# identify watersheds where no spawning occurs and remove from dataframe
no_spawning_locations <- results  |>
  mutate(row_sum = rowSums(results[6:20]))  |>
  filter(row_sum == 0) |>
  pull(location) |> unique()
results_only_spawn_watersheds

final_results <- results |>
  filter(!location %in% no_spawning_locations) |>
  glimpse()

write_csv(final_results, "analysis/spring_run_survival_sensi_model_ouput.csv")


# Exploratory plots
results <- read_csv("analysis/spring_run_survival_sensi_model_ouput.csv") |> glimpse()

no_action <- results |> 
  filter(id == 233) |> 
  pivot_longer(`1`:`20`, names_to = "year", values_to = "nat_spawn",
               names_transform = list(year = as.numeric)) |> 
  group_by(year) |> 
  summarise(no_action_total_nat_spawn = sum(nat_spawn))

results_with_diff <- results %>% 
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', 
               names_to = "year", names_transform = list(year = as.numeric)) |> 
  mutate(scenario = paste(survival_target, month_target, 
                          tolower(gsub(" ", "_", location_target)), sep = "_")) |> 
  group_by(id, year, scenario) |>
  summarise(total_nat_spawn = sum(natural_spawners)) |> 
  ungroup() |> 
  filter(id != 233) |> 
  left_join(no_action) |> 
  mutate(diff = total_nat_spawn - no_action_total_nat_spawn)

ids_with_diff <- results_with_diff |> 
  group_by(id, scenario) |>
  summarise(total_diff = sum(diff)) |> 
  filter(total_diff > 0) |> 
  pull(id)

results_with_diff |> 
  filter(id %in% ids_with_diff) |> 
  group_by(year) |> 
  summarise(total_spawn_no_action = sum(no_action_total_nat_spawn),
            total_spawn_action = sum(total_nat_spawn)) |> 
  View()

results_with_diff |> 
  filter(id == 185) |> 
  pivot_longer(total_nat_spawn:no_action_total_nat_spawn,
               names_to = "type", values_to = "nat_spawn") |>
  ggplot(aes(year, nat_spawn, color = type)) +
  geom_line()


# Older plots 
# exploratory plots
# juv_rear:
results %>% 
  filter(is.na(survival_target)) %>% 
  bind_rows(results %>%
              filter(survival_target == "juv_rear",
                     location_target == "Battle Creek",
                     location == "Battle Creek", 
                     month_target == 11)) %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_point(aes(x = as.factor(year), y = row_sum, color = as.factor(id), alpha = 0.5, shape = as.factor(id))) +
  coord_flip()

# juv migratory:
results %>%
  filter(is.na(survival_target)) %>%
  bind_rows(results %>%
              filter(survival_target == "juv_migratory",
                     location_target == "San Joaquin River",
                     month_target == 11)) %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_point(aes(x = as.factor(year), y = natural_spawners, color = as.factor(id), alpha = 0.5, shape = as.factor(id))) +
  coord_flip()

# egg to fry:
results %>%
  filter(is.na(survival_target)) %>%
  bind_rows(results %>%
              filter(survival_target == "egg_to_fry",
                     location_target == "Clear Creek")) %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_point(aes(x = as.factor(year), y = natural_spawners, color = as.factor(id), alpha = 0.5, shape = as.factor(id))) +
  coord_flip()
