og_all_spawners <- read_rds("data-raw/og-all-spawn.rds")

deer_creek_og_spawners <- og_all_spawners[10, ,] %>% 
  as_tibble() %>% 
  mutate(
    year = 1:20
  ) %>% 
  gather("run", "og_spawners", -year) %>% 
  mutate(run = readr::parse_number(run)) %>% 
  group_by(year) %>% 
  mutate(avg_og_spawner = mean(og_spawners)) %>% ungroup()

butte_creek_og_spawners <- og_all_spawners[6, ,] %>% 
  as_tibble() %>% 
  mutate(
    year = 1:20
  ) %>% 
  gather("run", "og_spawners", -year) %>% 
  mutate(run = readr::parse_number(run)) %>% 
  group_by(year) %>% 
  mutate(avg_og_spawner = mean(og_spawners)) %>% ungroup()

p1 <- butte_creek_og_spawners %>% 
  ggplot(aes(year, og_spawners, group = run)) + geom_line(alpha=.3) + 
  geom_line(aes(year, avg_og_spawner), linetype=2, color="blue")

# Results Comparison ------------------------------------------------------
library(springRunDSM)
library(tidyverse)
library(parallel)
list2env(load_2019_baseline_data(), .GlobalEnv)
sr_results_seeds <- spring_run_model()
sr_results <- spring_run_model(seeds = sr_results_seeds)
full_run <- function() {
  s <- spring_run_model()
  spring_run_model(seeds=s)
}

iters <- 25
results <- replicate(iters, full_run())

results_spawners <- map_df(1:iters, ~results["spawners", ][[.]]["Butte Creek", ]) %>% 
  mutate(run = 1:iters) %>% 
  gather("year", "spawners", -run) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  mutate(avg_spawners = mean(spawners))



p2 <- results_spawners %>% 
  ggplot(aes(year, spawners, group = run)) + geom_line(alpha=0.3) + 
  geom_line(aes(year, avg_spawners), linetype = 2, color = "blue") 

gridExtra::grid.arrange(p1, p2, nrow=2)


results_juveniles <- map_df(1:50, ~results["juveniles", ][[.]][6, ]) %>% 
  mutate(run = 1:50) %>% 
  gather("year", "juveniles", -run) %>% 
  mutate(year = as.numeric(year))

results_juveniles %>% 
  ggplot(aes(year, juveniles, group = run)) + geom_line(alpha=0.01) + 
  scale_x_continuous(breaks = seq(1, 20, by = 1))


results_spawners %>% 
  ggplot(aes(year, adults_pre_survival, group = run)) + geom_line(alpha=.4) + 
  scale_y_continuous(breaks = seq(0, 60000, by=5000)) + 
  geom_hline(yintercept = 8896.4, linetype=2)

adults <- results_adult_post %>% left_join(results_adult_pre) %>% 
  mutate(difference = adults_pre_survival - init_adults)

adults %>% 
  ggplot() + 
  geom_line(aes(year, init_adults, group = run)) + 
  geom_line(aes(year, adults_pre_survival, group = run)) 

# look at a few seeding stages
iters <- 50
seedings <- replicate(iters, spring_run_model())


seedings_results <- map_df(1:iters, ~seedings["spawners", ][[.]][6, ]) %>% 
  mutate(run = 1:iters) %>% 
  gather("year", "spawners", -run) %>% 
  mutate(year = as.numeric(year))

seedings_results %>% 
  ggplot(aes(year, spawners, group = run)) + geom_line(alpha=.4)


rm(list = ls())





