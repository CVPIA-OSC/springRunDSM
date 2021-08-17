#'@title Spring-Run Yearling Growth
#' @description Generates transition probability matrices for yearling growth
#' @details See \code{\link{params}} for details on parameter sources
#' @param year Current simulation year
#' @param yearlings Yearlings in tributaries
#' @param mode
#' @param yearling_territory_size Array of juvenile fish territory requirements
#' @param inchannel_habitat_fry 3 dimensional array [watersheds, months, years] representing fry inchannel habitat in square meters
#' @param inchannel_habitat_juvenile 3 dimensional array [watersheds, months, years] representing juvenile inchannel habitat in square meters
#' @param floodplain_habitat 3 dimensional array [watersheds, months, years] representing floodplain habitat in square meters
#' @param avg_temp More details at \code{\link[DSMtemperature]{stream_tempetature}}
#' @param prob_strand_early More details at \code{\link[DSMhabitat]{prop_strand_early}}
#' @param prob_strand_late More details at \code{\link[DSMhabitat]{prop_strand_late}}
#' @param proportion_diverted More details at \code{\link[DSMflow]{proportion_diverted}}
#' @param total_diverted More details at \code{\link[DSMflow]{total_diverted}}
#' @param weeks_flooded More details at \code{\link[DSMflow]{weeks_flooded}}
#' @param prop_high_predation More details at \code{\link[DSMhabitat]{prop_high_predation}}
#' @param contact_points More details at \code{\link[DSMhabitat]{contact_points}}
#' @param ..surv_juv_rear_int Intercept for \code{\link{surv_juv_rear}}
#' @param .surv_juv_rear_contact_points Coefficient for \code{\link{surv_juv_rear}} \code{contact_points} variable
#' @param ..surv_juv_rear_contact_points Calibrated coefficient for \code{\link{surv_juv_rear}} \code{contact_points} variable
#' @param .surv_juv_rear_prop_diversions Coefficient for \code{\link{surv_juv_rear}} \code{prop_diversions} variable
#' @param ..surv_juv_rear_prop_diversions Calibrated coefficient for \code{\link{surv_juv_rear}} \code{prop_diversions} variable
#' @param .surv_juv_rear_total_diversions Coefficient for \code{\link{surv_juv_rear}} \code{total_diversions} variable
#' @param ..surv_juv_rear_total_diversions Calibrated coefficient for \code{\link{surv_juv_rear}} \code{total_diversions} variable
#' @param .surv_juv_rear_avg_temp_thresh Coefficient for \code{\link{surv_juv_rear}} \code{avg_temp_thresh} variable
#' @param .surv_juv_rear_high_predation Coefficient for \code{\link{surv_juv_rear}} \code{high_predation} variable
#' @param .surv_juv_rear_stranded Coefficient for \code{\link{surv_juv_rear}} \code{stranded} variable
#' @param .surv_juv_rear_medium Size related intercept for \code{\link{surv_juv_rear}} medium sized fish
#' @param .surv_juv_rear_large Size related intercept for \code{\link{surv_juv_rear}} large sized fish
#' @param .surv_juv_rear_floodplain Additional intercept for \code{\link{surv_juv_rear}} floodplain rearing benefit
#' @param min_survival_rate estimated survival rate if temperature threshold is exceeded
#' @export
yearling_growth <- function(year,
                            yearlings,
                            mode,
                            survival_adjustment,
                            yearling_territory_size = springRunDSM::params$yearling_territory_size,
                            inchannel_habitat_fry = springRunDSM::params$inchannel_habitat_fry,
                            inchannel_habitat_juvenile = springRunDSM::params$inchannel_habitat_juvenile,
                            floodplain_habitat = springRunDSM::params$floodplain_habitat,
                            avg_temp = springRunDSM::params$avg_temp,
                            prob_strand_early = springRunDSM::params$prob_strand_early,
                            prob_strand_late = springRunDSM::params$prob_strand_late,
                            proportion_diverted = springRunDSM::params$proportion_diverted,
                            total_diverted = springRunDSM::params$total_diverted,
                            weeks_flooded = springRunDSM::params$weeks_flooded,
                            prop_high_predation = springRunDSM::params$prop_high_predation,
                            contact_points = springRunDSM::params$contact_points,
                            growth_rates = springRunDSM::params$growth_rates,
                            growth_rates_floodplain = springRunDSM::params$growth_rates_floodplain,
                            ..surv_juv_rear_int = springRunDSM::..surv_juv_rear_int,
                            .surv_juv_rear_contact_points = springRunDSM::.surv_juv_rear_contact_points,
                            ..surv_juv_rear_contact_points = springRunDSM::..surv_juv_rear_contact_points,
                            .surv_juv_rear_prop_diversions = springRunDSM::.surv_juv_rear_prop_diversions,
                            ..surv_juv_rear_prop_diversions = springRunDSM::..surv_juv_rear_prop_diversions,
                            .surv_juv_rear_total_diversions = springRunDSM::.surv_juv_rear_total_diversions,
                            ..surv_juv_rear_total_diversions = springRunDSM::..surv_juv_rear_total_diversions,
                            .surv_juv_rear_avg_temp_thresh = springRunDSM::.surv_juv_rear_avg_temp_thresh,
                            .surv_juv_rear_high_predation = springRunDSM::.surv_juv_rear_high_predation,
                            .surv_juv_rear_stranded = springRunDSM::.surv_juv_rear_stranded,
                            .surv_juv_rear_medium = springRunDSM::.surv_juv_rear_medium,
                            .surv_juv_rear_large = springRunDSM::.surv_juv_rear_large,
                            .surv_juv_rear_floodplain = springRunDSM::.surv_juv_rear_floodplain,
                            min_survival_rate = springRunDSM::min_survival_rate,
                            stochastic) {

  growth_rates_identity <- diag(1, nrow = 4, ncol = 4)
  growth_rates_floodplain_identity <- replicate(5, diag(1, 4, 4))

  for (month in 5:10) {
    # only months 9, 10 experience growth
    if (month %in% 9:10) {
      growth_rates_used <- growth_rates
      growth_rates_floodplain_used <- growth_rates_floodplain
    } else {
      growth_rates_used <- growth_rates_identity
      growth_rates_floodplain_used <- growth_rates_floodplain_identity
    }
    
    this_weeks_flooded <- weeks_flooded[, month, year]

    # we only care for floodplain and inchannel
    habitat <- get_habitat(year, month,
                           inchannel_habitat_fry = inchannel_habitat_fry,
                           inchannel_habitat_juvenile = inchannel_habitat_juvenile,
                           floodplain_habitat = floodplain_habitat)

    # we only care for floodplain and inchannel
    survival_rates <- get_rearing_survival(year = year,
                                           month = month,
                                           survival_adjustment = survival_adjustment,
                                           mode = mode,
                                           avg_temp = avg_temp,
                                           prob_strand_early = prob_strand_early,
                                           prob_strand_late = prob_strand_late,
                                           proportion_diverted = proportion_diverted,
                                           total_diverted = total_diverted,
                                           weeks_flooded = weeks_flooded,
                                           prop_high_predation = prop_high_predation,
                                           contact_points = contact_points,
                                           ..surv_juv_rear_int = ..surv_juv_rear_int,
                                           .surv_juv_rear_contact_points = .surv_juv_rear_contact_points,
                                           ..surv_juv_rear_contact_points = ..surv_juv_rear_contact_points,
                                           .surv_juv_rear_prop_diversions = .surv_juv_rear_prop_diversions,
                                           ..surv_juv_rear_prop_diversions = ..surv_juv_rear_prop_diversions,
                                           .surv_juv_rear_total_diversions = .surv_juv_rear_total_diversions,
                                           ..surv_juv_rear_total_diversions = ..surv_juv_rear_total_diversions,
                                           min_survival_rate = min_survival_rate, 
                                           stochastic = stochastic)

    floodplain_remaining <- habitat$floodplain

    yearlings <- fill_natal(juveniles = yearlings, inchannel_habitat = habitat$inchannel, 
                            floodplain_habitat = habitat$floodplain,
                            territory_size = yearling_territory_size, 
                            up_to_size_class = 4, yearlings = TRUE)
    
    yearlings <- rear(juveniles = yearlings$inchannel, survival_rate = survival_rates$inchannel, 
         growth = growth_rates_used, 
         floodplain_juveniles = yearlings$floodplain,
         floodplain_survival_rate = survival_rates$floodplain, 
         floodplain_growth = growth_rates_floodplain_used,
         weeks_flooded = weeks_flooded, 
         stochastic)
    
    yearlings <- round(yearlings$inchannel + yearlings$floodplain)

  }

  return(yearlings)
}
