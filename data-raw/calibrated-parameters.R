library(tidyverse)

vect <- c(3.5000000 , #inchannel default intercept
          1.5000000, # upper sacramento intercept
          -2.5000000, # butte creek intercept
          -2.9000000, # deer creek intercept
          -1.1092908, # mill creek intercept
          -3.5000000, # sacramento sections 1,2,3 intercepts
          3.5000000, # middle sac tribs
          -3.5000000, # ?
          2.5000000, # american river intercept
          -1.2000000, # delta tribs, calaveras and cosumnes
          1.9999999, # moke int
          -0.2000000, # merced int
          -0.1081707, # stan intercept
          -3.4999959, # tuol intercept
          -0.4000000, # sj main intercept
          -3.5000000, # bypass intercept
          1.4000000, # delta intercapt
          -3.5000000,
          2.5000000,
          0.3000000,
          -3.5000000,
          0.3000000,
          -3.5000000,
          1.2000000,
          -0.5108849,
          -3.3233638,
          -3.2304288,
          -3.4148335,
          -3.5000000,
          -3.5000000,
          -1.3083410,
          -1.9841364,
          2.5000007,
          -3.5000000,
          -3.0000000,
          -0.9000000
)

vect=c(-2.25097915, # 1 default calibration int
       -2.31387713, # 2 int: watershed 10
       1.86641130, # 3 int: watershed 12
       -0.55423070, # 4 int: watershed 19
       -3.49999933, # 5 int: watershe 20
       -2.56726844, #6 int: watershed 16, 21, 24
       -2.52403823, # 7 int: bypass survival sutter and yolo
       1.42642277, # 8 int: delta survival 
       -2.79409568, # 9 migratory survival SJ
       2.04438137, # 10 outmigration surv sac: UM, LM, LL
       0.99985602, # 11 outmigration surv 2nd beta sac: UM, LM, LL
       -2.89141694, # 12 juv delta outmigration int 1
       0.75077947, # 13 juv delta outmigration int 2
       -3.10495582, # 14 juv delta outmigration int 3
       2.91518569, # 15 adult en-route survival intercept 
       -3.49954625, # 16 adult ocean entry surival intercept 1
       -1.49855839, # 17 adult ocean entry surival intercept 2
       -3.22990407, # 18 adult ocean entry surival intercept 3
       2.49974122, # 19 adult ocean entry surival intercept 4
       -2.96201071, # 20 adult ocean entry surival intercept 5
       0.09999992, # 21 contact point beta
       0.01000010, # 22 proportion diverted beta
       0.19126503, # 23 total diversion beta
       0.61104442, # 24 total diverson beta for delta
       -0.70866158, # 25 int watershed 6
       2.10420292, # 26 int watershed 31
       -2.59452699, # 27 ocean entry survival 6
       2.79189516, # 28 int watersheds 3, 7
       -1.53805220 # 29 ocean entry survival 7
      )


surv.adj <- rep(1, 31)
surv.adj[c(2, 4, 5, 8, 9, 11, 14, 13, 15, 18)] <- 0.025
surv.adj[c(8)] <- 0.50
surv.adj[c(9)] <- 0.25


# Create a dataframe that contains the watersheds and corresponding betas for each
index_ws_to_update <- c(3, 6, 7, 10, 12, 16, 19, 20, 21, 24, 31)
`2nd calibration adjustment` <- c(vect[28], vect[25], vect[28], vect[2], vect[3], 
                                  vect[6], vect[4], vect[5], vect[6], vect[6], vect[26])

beta_to_update <- tibble(
  order = index_ws_to_update,
  `2nd calibration adjustment`
)

survival_betas <- cvpiaData::watershed_ordering %>%
  left_join(beta_to_update) %>%
  mutate(`2nd calibration adjustment` = ifelse(is.na(`2nd calibration adjustment`),
                                               -2.25097915, `2nd calibration adjustment`)) %>%
  add_column(`average temperature` = -0.717,
             predation = -0.122,
             `contact points` = 0.09999992,
             `contact points scaler` = -0.189,
             `proportion diverted` = 0.01000010,
             `proportion diverted scaler` = -3.51,
             `total diverted` = 0.19126503,
             `total diverted scaler` = -0.0021,
             stranded = -1.939,
             medium = 1.48,
             large = 2.223,
             `floodplain habitat` = 0.47,
             `survival adjustments` = surv.adj)

usethis::use_data(survival_betas, overwrite = TRUE)
# write_csv(survival_betas, "data-raw/sr-survival-betas.csv")

# TODO make the model use these tibbles!

# delta_survival_betas <- tibble(
#   watershed = c("North Delta", "South Delta"),
#   intercept = 1.4, # vect[17]
#   `avg temp thresh` = -0.717,
#   predation = -0.122,
#   contact = 0.0358 * -0.189,
#   `prop diversions` = -3.51,
#   `total diversions` = 0.5 * -0.0021,
#   medium = 1.48,
#   large = 2.223
# )
#
# usethis::use_data(delta_survival_betas, overwrite = TRUE)
#
# outmigration_survival_betas <- tibble(
#   `intercept 1` = 2.5, flow = 0.0092,
#   `proportion diversion` = -3.51 * 0.05,
#   `total diversion` = -0.0021 * 0.215,
#   `intercept 2` = 0.3,
#   `average temperature` = 0.554,
#   `model weight` = .5,
#   medium = 1.48, large = 2.223
# )
