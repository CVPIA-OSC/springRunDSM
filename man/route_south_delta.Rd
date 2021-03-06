% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route.R
\name{route_south_delta}
\alias{route_south_delta}
\title{South Delta Routing}
\source{
IP-117068
}
\usage{
route_south_delta(
  freeport_flow,
  dcc_closed,
  month,
  mean_freeport_flow = 21546.19,
  sd_freeport_flow = 14375.9,
  betas = list(sss = c(intercept = 1.892235, `freeport discharge` = 2.170375, `upper
    asymptote` = 0.3512465), dcc = c(intercept = -1.48962, `freeport discharge` =
    -1.248865), gs = c(intercept = -2.948145, `freeport discharge` = -2.911835, `DCC
    effect on routing` = -0.554843, `lower asymptote` = 0.2729845))
)
}
\arguments{
\item{freeport_flow}{Monthly mean flow at freeport in cubic feet per second}

\item{dcc_closed}{Number of days the Delta Cross Channel gates are closed during the month}

\item{month}{Current simulation month as an integer for calculating number of days
the Delta Cross Channel gates are open}

\item{mean_freeport_flow}{Mean of flow at freeport for standardizing discharge}

\item{sd_freeport_flow}{Standard Deviation of flow at freeport for standardizing discharge}

\item{betas}{Parameters for Sutter and Steamboat Sloughs (sss),  Delta Cross
Channel (dcc), and Georgiana Slough (gs). All parameters are posterior medians}
}
\description{
Routes juveniles through the South Delta
}
\details{
This submodel is adapted from Perry et al. (2018) https://doi.org/10.1139/cjfas-2017-0310
}
