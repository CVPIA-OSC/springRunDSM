
# Get Spawning Adults -----------------------------------------------------

result_adults <- replicate(100, get_spawning_adults(1, adults[, 1], hatch_adults, seeds = NULL)$init_adults_by_month)
dim(result_adults)
butte_adults <- result_adults[6, ,] %>% 
  as_tibble() %>% 
  mutate(month = 1:4) %>% 
  gather("run", "adults", -month)

butte_adults %>% 
  ggplot(aes(month, adults)) + geom_col()
