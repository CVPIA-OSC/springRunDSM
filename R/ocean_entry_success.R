#' @title Ocean Entry Success
#' @description Calculates the number of juveniles that survive entering the ocean
#' @param migrants Variable representing the number of juveniles at golden gate bridge
#' @param month Variable representing the current simulation month
#' @param avg_ocean_transition_month Variable representing the average month juveniles transition to the ocean
#' @param length Variable representing the fork lengths for each size classes. \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @param ..ocean_entry_success_int Intercept, source: Calibration (Varies by tributary )
#' @param .month Coefficient for \code{month} variable, source: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
#' @source IP-117068
#' @export
ocean_entry_success <- function(migrants, month, avg_ocean_transition_month,
                                length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
                                ..ocean_entry_success_intercept =  c(`Upper Sacramento River` = -3.49954625, 
                                                                     `Antelope Creek` = -3.49954625, 
                                                                     `Battle Creek` = -2.59452699, 
                                                                     `Bear Creek` = -3.49954625, 
                                                                     `Big Chico Creek` = -3.49954625, 
                                                                     `Butte Creek` = -1.5380522, 
                                                                     `Clear Creek` = -2.59452699, 
                                                                     `Cottonwood Creek` = -3.49954625, 
                                                                     `Cow Creek` = -3.49954625, 
                                                                     `Deer Creek` = -1.49855839, 
                                                                     `Elder Creek` = -3.49954625, 
                                                                     `Mill Creek` = -3.22990407, 
                                                                     `Paynes Creek` = -3.49954625, 
                                                                     `Stony Creek` = -3.49954625, 
                                                                     `Thomes Creek` = -3.49954625, 
                                                                     `Upper-mid Sacramento River` = -3.49954625, 
                                                                     `Sutter Bypass` = -3.49954625, 
                                                                     `Bear River` = 2.49974122, 
                                                                     `Feather River` = 2.49974122, 
                                                                     `Yuba River` = -2.96201071, 
                                                                     `Lower-mid Sacramento River` = -3.49954625, 
                                                                     `Yolo Bypass` = -3.49954625, 
                                                                     `American River` = -3.49954625, 
                                                                     `Lower Sacramento River` = -3.49954625, 
                                                                     `Calaveras River` = -3.49954625, 
                                                                     `Cosumnes River` = -3.49954625, 
                                                                     `Mokelumne River` = -3.49954625, 
                                                                     `Merced River` = -3.49954625, 
                                                                     `Stanislaus River` = -3.49954625, 
                                                                     `Tuolumne River` = -3.49954625, 
                                                                     `San Joaquin River` = -3.49954625), 
                                .month = 0.35
                              ){
  
  month_since <- ifelse(month <= avg_ocean_transition_month, 0, max(1, month - avg_ocean_transition_month))

  survival_rate <- NULL
  for(i in 1:dim(migrants)[1]) {
    survival_rate <- rbind(survival_rate,
                           boot::inv.logit(..ocean_entry_success_intercept[i] + .month * month_since + length))
  }

  if (month_since == 0) rep(0, 31) else rowSums(round(survival_rate * migrants))

}

