library(springRunDSM)
library(tidyverse)
r2r_seeds <- springRunDSM::spring_run_model(scenario = NULL, mode = "seed",
                                            seeds = NULL, ..params = springRunDSM::r_to_r_baseline_params, stochastic = FALSE)

r2r_model_results <- springRunDSM::spring_run_model(mode = "simulate", ..params = springRunDSM::r_to_r_baseline_params,
                                                seeds = r2r_seeds)
r2r_model_results$spawners
r2r_model_results$phos

plot_total_spawners <- function(model_results,
                                result_type = c("Total Spawners", "Total Natural Spawners")) {
  if (result_type == "Total Natural Spawners") {
    spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  }
  else {
    spawn <- dplyr::as_tibble(model_results$spawners) |>
      dplyr::mutate(location = fallRunDSM::watershed_labels)
  }
  
  spawn %>%
    pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") |>
    group_by(year) |>
    summarize(total_spawners = sum(spawners)) |>
    mutate(year = as.numeric(year))  |> 
    ggplot(aes(year, total_spawners)) +
    geom_line() +
    theme_minimal() +
    labs(y = result_type,
         x = "Year") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = 1:20) +
    theme(text = element_text(size = 20))
}

plot_total_spawners(r2r_model_results,"Total Spawners")
