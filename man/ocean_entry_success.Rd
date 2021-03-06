% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ocean_entry_success.R
\name{ocean_entry_success}
\alias{ocean_entry_success}
\title{Ocean Entry Success}
\source{
IP-117068
}
\usage{
ocean_entry_success(
  migrants,
  month,
  avg_ocean_transition_month,
  length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
  betas = c(`Upper Sacramento River` = -3.49954625, `Antelope Creek` = -3.49954625,
    `Battle Creek` = -2.59452699, `Bear Creek` = -3.49954625, `Big Chico Creek` =
    -3.49954625, `Butte Creek` = -1.5380522, `Clear Creek` = -2.59452699, `Cottonwood
    Creek` = -3.49954625, `Cow Creek` = -3.49954625, `Deer Creek` = -1.49855839, `Elder
    Creek` = -3.49954625, `Mill Creek` = -3.22990407, `Paynes Creek` = -3.49954625,
    `Stony Creek` = -3.49954625, `Thomes Creek` = -3.49954625, `Upper-mid Sacramento
    River` = -3.49954625,      `Sutter Bypass` = -3.49954625, `Bear River` = 2.49974122,
    `Feather River` = 2.49974122, `Yuba River` = -2.96201071, `Lower-mid Sacramento
    River` = -3.49954625, `Yolo Bypass` = -3.49954625, `American River` = -3.49954625,
    `Lower Sacramento River` = -3.49954625, `Calaveras River` = -3.49954625, `Cosumnes
    River` = -3.49954625, `Mokelumne River` = -3.49954625, `Merced River` = -3.49954625,
    `Stanislaus River` = -3.49954625, `Tuolumne River` = -3.49954625, `San Joaquin River`
    = -3.49954625, months = 0.35)
)
}
\arguments{
\item{migrants}{The number of juveniles at golden gate bridge}

\item{month}{The current simulation month}

\item{avg_ocean_transition_month}{The average month juveniles transition to the ocean}

\item{length}{Fork lengths for each size classes. \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}}

\item{betas}{Parameters estimated through calibration}
}
\description{
Calculates the number of juveniles that survive entering the ocean
}
\section{Parameters}{

Parameters from the model are obtained from either literature, calibration, export elicitation,
and meta-analysis. The source for each parameter in this function are detailed below.
\itemize{
\item intercept 1-31: calibration estimate; varies by tributary
\item months: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/Satterthwaite_et_al_2014.pdf}{Satterthwaite et al. (2014)}
}
}

