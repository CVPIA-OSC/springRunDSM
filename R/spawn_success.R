#' @title Spawning Success
#' @description Calculates the annual reproductive success.
#' @param escapement The number of returning adults
#' @param adult_prespawn_survival The adult prespawn survival rate
#' @param egg_to_fry_survival The egg to fry survival rate
#' @param prob_scour The probability of nest scouring
#' @param spawn_habitat The available spawning habitat in square meters
#' @param sex_ratio The female to male spawning ratio
#' @param redd_size The size of redds including defensible space
#' @param fecundity The number of eggs per female
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export

spawn_success <- function(escapement, 
                          proportion_natural, # R2R ADDS NEW PARAM
                          hatchery_age_distribution, # R2R ADDS NEW PARAM
                          natural_age_distribution, # R2R ADDS NEW PARAM
                          fecundity_lookup = fallRunDSM::params$fecundity_lookup, # R2R ADDS NEW PARAM
                          adult_prespawn_survival, egg_to_fry_survival,
                          prob_scour, spawn_habitat,
                          stochastic,
                          sex_ratio = springRunDSM::params$spawn_success_sex_ratio,
                          redd_size = springRunDSM::params$spawn_success_redd_size,
                          fecundity = springRunDSM::params$spawn_success_fecundity){

  capacity <- spawn_habitat / redd_size

  spawner_potential <- if(stochastic) {
    rbinom(31, round(escapement), (adult_prespawn_survival * sex_ratio))
  } else {
    round(escapement * adult_prespawn_survival * sex_ratio)
  }

  spawners <- pmin(spawner_potential, capacity)

  # caluclate natural fry
  # TODO remove dependencies and clean up code
  total_nat_spawn <- dplyr::tibble(watershed = fallRunDSM::watershed_labels, spawners = round(spawners * proportion_natural)) |>
    dplyr::left_join(natural_age_distribution, by = c("watershed" = "watershed")) |>
    dplyr::mutate(age_2_spawners = round(spawners * prop_2),
                  age_3_spawners = round(spawners * prop_3),
                  age_4_spawners = round(spawners * prop_4),
                  age_5_spawmers = round(spawners * prop_5)) |>
    dplyr::select(-c(prop_2, prop_3, prop_4, prop_5, spawners, watershed)) |>
    as.matrix()
  
  dimnames(total_nat_spawn) <- list(c(fallRunDSM::watershed_labels), c("2", "3", "4", "5"))
  fecundity_natural <- fecundity_by_age |>
    dplyr::filter(origin == "Wild") |>
    dplyr::pull(fecundity)
  
  # calculate natural fry
  natural_fry <- rowSums(sweep(total_nat_spawn * (1 - prob_scour), 2, fecundity_natural, "*") * egg_to_fry_survival)
  
  # nat_spawn_with_ages[1] <- total_nat_spawn * natural_age_distribution
  total_hatch_spawn <- dplyr::tibble(watershed = fallRunDSM::watershed_labels, spawners = round(spawners * (1 - proportion_natural))) |>
    dplyr::left_join(hatchery_age_distribution, by = c("watershed" = "watershed")) |>
    dplyr::mutate(age_2_spawners = round(spawners * prop_2),
                  age_3_spawners = round(spawners * prop_3),
                  age_4_spawners = round(spawners * prop_4)) |>
    dplyr::select(-c(prop_2, prop_3, prop_4, spawners, watershed)) |>
    as.matrix()
  
  dimnames(total_hatch_spawn) <- list(c(fallRunDSM::watershed_labels), c("2", "3", "4"))
  fecundity_hatch <- fecundity_by_age |>
    dplyr::filter(origin == "Hatchery") |>
    dplyr::pull(fecundity)
  
  # calculate hatchery fry
  hatchery_fry <- rowSums(sweep(total_hatch_spawn * (1 - prob_scour), 2, fecundity_hatch, "*") * egg_to_fry_survival)
  fry <- natural_fry + hatchery_fry

  fry <- if(stochastic) {
    pmax(round(rnorm(31, fry, (sqrt(fry) / 2))), 0)
  } else {
    round(fry)
  }

  zeros <- matrix(0, nrow = length(escapement), ncol = 3)
  cbind(fry, zeros)

}
