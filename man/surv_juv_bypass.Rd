% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survival.R
\name{surv_juv_bypass}
\alias{surv_juv_bypass}
\title{Juvenile Bypass Survival}
\source{
IP-117068
}
\usage{
surv_juv_bypass(
  max_temp_thresh,
  avg_temp_thresh,
  high_predation,
  betas = c(intercept = -3.5, `average temperature` = -0.717, predation = -0.122,
    medium = 1.48, large = 2.223, `floodplain habitat` = 0.47)
)
}
\arguments{
\item{max_temp_thresh}{The probability of exceeding the max temp threshold}

\item{avg_temp_thresh}{The probability of exceeding the average temperature}

\item{high_predation}{An indicator for high predation in watershed}

\item{betas}{The parameter estimates from calibration}
}
\description{
Calculates the juvenile rearing survival in the bypasses
}
\section{Parameters}{

Parameters from the model are obtained from either literature, calibration, export elicitation,
and meta-analysis. The source for each parameter in this function are detailed below.
\itemize{
\item intercept: calibration estimate; varies by tributary
\item average temperature: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/marine_cech_water_temp_effects.pdf}{Marine and Chech (2004)}
\item predation \href{https://pubag.nal.usda.gov/catalog/512123}{Cavallo et al. (2012)}
\item medium: \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
\item large" \href{https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/M02-161.1}{Connor et al. (2004)}
\item floodplain habitat: \href{https://dsm-docs.s3-us-west-2.amazonaws.com/SOMMER_T-SDWA+180+Floodplain+rearing+of+juvenile+chinook+salmon+evidence+of+enhanced+growth+and+survival+.pdf}{Sommer et al. (2001)}
}
}

