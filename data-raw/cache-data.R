library(tidyverse)
# Seeds differ based on run 
adult_seeds <- matrix(0, nrow = 31, ncol = 30)
# seeds are derived from "Simulation Seeds" file.
adult_seeds[ , 1] <- c(0, 4, 439.4, 0, 0, 8896.4, 179.2, 0, 0, 574.2, 0, 478.6, 0, 
                       0, 0, 0, 0, 0, 4811.2, 641.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)
rownames(adult_seeds) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(adult_seeds, overwrite = TRUE)

# proportion_hatchery differ based on run 
# Prop hatchery come from 2010-2013 CWI reports 
butte_creek_hatch = mean(c(0.01, 0, 0, 0))
feather_river_hatch = mean(c(0.78, 0.90, 0.90, 0.84))
yuba_river_hatch = mean(c(0.71, 0.495, 0.36, 0.40))
proportion_hatchery <- c(rep(0, 5), butte_creek_hatch, rep(0, 12), feather_river_hatch, yuba_river_hatch, rep(0, 11))
names(proportion_hatchery) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(proportion_hatchery, overwrite = TRUE)

# @title Hatchery Returns
# @export
# emanuel: not needed
# total_hatchery_returning <- round(stats::runif(1,83097.01, 532203.1)) # what is this

# @title Proportion of Adults Spawning March to June
# @export
# month_return_proportions differs based on run 
# Proportion adults spawners (including hatchery fish) across 4 months (March-June)
month_return_proportions <- c(0.125, 0.375, 0.375, 0.125)
names(month_return_proportions) <- month.abb[3:6]
usethis::use_data(month_return_proportions, overwrite = TRUE)

# Mass by size class
mass_by_size_class <- c(0.5, 1.8, 9.1, 31.4)
names(mass_by_size_class) <- c("s", "m", "l", "vl")
usethis::use_data(mass_by_size_class, overwrite = TRUE)

# TODO come up with better names
# stray rates differ based on run 
cross_channel_stray_rate <- c(1, 2, 2, 0, 1, 2, 2, 1, 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)/20
names(cross_channel_stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(cross_channel_stray_rate, overwrite = TRUE)

stray_rate <- c(1, 2, 2, 0, 1, 2, 2, 1, 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 2, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0)/26
names(stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(stray_rate, overwrite = TRUE)

# TODO confirm that these are not being used in the refactored model?
# TODO better names and refactor
cc.aloc <- c(1, 2, 2, 0, 1, 2, 2, 1, 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)/20
oth.aloc <- c(1, 2, 2, 0, 1, 2, 2, 1, 0, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 2, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0)/26

# differs based on run ------
# No adult harvest for spring run 
adult_harvest_rate <- rep(0, 31)
names(adult_harvest_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(adult_harvest_rate, overwrite = TRUE)

# Feather River(19) has the only non zero value for natural adult removal springrun 
natural_adult_removal_rate <- c(rep(0, 18), 0.22, rep(0, 12)) #differs based on run
names(natural_adult_removal_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(natural_adult_removal_rate, overwrite = TRUE)

hatchery_allocation <- c(0, 0.00012, 0.00445, 0, 0.00076, 0.00285, 0.00038, 
                         0.00009, 0, 0.00536, 0, 0, 0, 0, 0, 0, 0,0, 0.86531, 
                         0.12068, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) # differs based on run
names(hatchery_allocation) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(hatchery_allocation, overwrite = TRUE)

# Grouping data does not differ based on run 
original_groups <- read_csv("data-raw/misc/Grouping.csv")
diversity_group <- original_groups$DiversityGroup
names(diversity_group) <- original_groups$watershed
usethis::use_data(diversity_group, overwrite = TRUE)




# OG data for comparison 
baseline_2019 <- readr::read_rds("data-raw/og-model-inputs.rds")
usethis::use_data(baseline_2019)























