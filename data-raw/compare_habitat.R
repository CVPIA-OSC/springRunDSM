ics = IChab.spawn 
icf = IChab.fry
icj = IChab.juv
shd = spwn.hab.deg 
rhd = rear.hab.deg
stoc = stochastic

ics[trib,,yrs] <- ics[trib,,yrs] * shd[trib] #degrade future spawning habitat
icf[trib,,yrs] <- icf[trib,,yrs] * rhd[trib] #degrade future rearing habitat
icj[trib,,yrs] <- icj[trib,,yrs] * rhd[trib]  #degrade future rearing habitat

dim(old_habitats$spawning_habitat)
dim(new_habitats$spawning_habitat)

library(lubridate)
library(tidyverse)

hab_no_decay <- read_rds('data-raw/misc/habitat-no-decay.rds')
hab_with_decay <- read_rds('data-raw/misc/habitat-with-decay.rds')
old_habitats <- read_rds('data-raw/misc/old_habitats.rds')

spawning <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    og_no_scale = as.vector(hab_no_decay$spawn),
    og_with_scale = as.vector(hab_with_decay$spawn_hab_with_deg),
    new_no_scale = as.vector(old_habitats$spawning_habitat),
    new_with_scale = as.vector(new_habitats$spawning_habitat))

all(spawning$new_no_scale == spawning$og_no_scale)

spawning %>% 
  select(-new_no_scale) %>% 
  filter(!(watershed %in% c("Butte Creek", "Cottonwood Creek")),
         abs(round(og_with_scale - new_with_scale)) > 0) %>% pull(year) %>% table()

spawning %>% 
  transmute(watershed, date = ymd(paste(year, month, 1)),
            change_og = og_with_scale - og_no_scale,
            change_new = new_with_scale - og_no_scale) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = change_og), color = 'red') +
  geom_line(aes(y = change_new), color = 'blue') +
  facet_wrap(~watershed, scales = "free_y")

spawning %>% 
  filter(watershed == 'Merced River', new_with_scale != og_with_scale) %>% View
  

fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    og_no_scale = as.vector(hab_no_decay$fry),
    og_with_scale = as.vector(hab_with_decay$fry_hab_with_def),
    new_with_scale = as.vector(new_habitats$inchannel_habitat_fry))


fry %>% 
  transmute(watershed, date = ymd(paste(year, month, 1)),
            change_og = og_with_scale - og_no_scale,
            change_new = new_with_scale - og_no_scale) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = change_og), color = 'red') +
  geom_line(aes(y = change_new), color = 'blue') +
  facet_wrap(~watershed, scales = "free_y")

fry %>% 
  transmute(watershed, date = ymd(paste(year, month, 1)),
            change_og = og_with_scale - og_no_scale,
            change_new = new_with_scale - og_no_scale) %>% 
  filter(watershed == "Upper Sacramento River", abs(round(change_og - change_new)) > 0) %>% 
  gather(sim, value, -watershed, - date) %>% 
  ggplot(aes(date, value, color = sim)) +
  geom_line()

juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    og_no_scale = as.vector(hab_no_decay$juv),
    og_with_scale = as.vector(hab_with_decay$juv_hab_with_def),
    new_with_scale = as.vector(new_habitats$inchannel_habitat_juvenile))

juv %>% 
  transmute(watershed, date = ymd(paste(year, month, 1)),
            change_og = og_with_scale - og_no_scale,
            change_new = new_with_scale - og_no_scale) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = change_og), color = 'red') +
  geom_line(aes(y = change_new), color = 'blue') +
  facet_wrap(~watershed, scales = "free_y")


weeks_flooded <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    old_wkfld = as.vector(old_habitats$weeks_flooded),
    new_wkfld = as.vector(new_habitats$weeks_flooded))

weeks_flooded %>% 
  transmute(watershed, date = ymd(paste(year, month, 1)),
            change = new_wkfld - old_wkfld) %>% 
  ggplot(aes(x = date, y = change)) +
  geom_col() +
  facet_wrap(~watershed, scales = "free_y")
