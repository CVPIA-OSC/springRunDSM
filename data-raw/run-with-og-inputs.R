library(tidyverse)
source('data-raw/load-old-inputs.R')

sim <- spring_run_model(seeds = old_seeds, stochastic = FALSE,
                        mode = "simulate", ..params = params)


v1.1 <- sim$spawners[1,]
v1.0 <- c(42.6547296715519, 39.5235925127827, 31.8870317214397, 29.9170377952522, 
          38.1316802936261, 44.5025019675442, 44.146051341225, 35.0092800182692, 
          32.3904976371846, 31.8099408803782, 29.6250493394868, 28.2698404899983, 
          28.23305297572, 33.2068504891259, 37.1807139046197, 33.6552504199256, 
          29.3479110967424, 32.4460102150267, 33.7270698201146, 27.5098252390387
)

cbind(v1.1, v1.0, (v1.1-v1.0)/v1.0)
summary((v1.1-v1.0)/v1.0)

tibble(year = 1:20,
       v1.1, v1.0) %>% 
  gather(scenario, spawners, - year) %>% 
  ggplot(aes(year, spawners, color = scenario)) +
  geom_line()

mean(v1.1) - mean(v1.0)