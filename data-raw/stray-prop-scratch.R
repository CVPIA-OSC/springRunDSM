# they call this spawnerbyMonth
adults_by_month <- t(sapply(1:31, function(watershed) {
  rmultinom(1, adult_seeds[watershed], month_return_proportions)
}))

hatch_adults

adults_natural <- sapply(1:4, function(month) {
  rbinom(n = 31, 
         size = round(adults_by_month[, month]), 
         prob = 1 - natural_adult_removal_rate)
})

stray_props_by_month <- sapply(3:6, function(month) {adult_stray(wild = 1,
            natal_flow = prop_flow_natal[ , year],
            south_delta_watershed = south_delta_routed_watersheds,
            cross_channel_gates_closed = cc_gates_days_closed[month])}
)

stray_by_month <- sapply(1:4, function(month) {
  rbinom(n = 31, adults, stray_props_by_month[, month])
})

south_delta_routed_watersheds
# strays CC
as.vector(rmultinom(1,round(sum(stray_by_month[, 2]*south_delta_routed_watersheds)),cross_channel_stray_rate))

# strays OTH
as.vector(rmultinom(1,round(sum(stray_by_month[, 1]*(1-south_delta_routed_watersheds))), stray_rate))
randstray3<-as.vector(rmultinom(1,round(sum(stray*(1-inps$SCDELT))),oth.aloc))

