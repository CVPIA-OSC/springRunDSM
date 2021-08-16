all_inputs <- read_rds('data-raw/misc/spring_run_all_old_inputs.rds')
params <- springRunDSM::params

all_inputs$floodp.sutter -> params$sutter_floodplain_habitat
all_inputs$floodp.yolo -> params$yolo_floodplain_habitat

all_inputs$territory_size -> params$territory_size
spawn_dimnames <- dimnames(params$spawning_habitat)
all_inputs$spawn_out -> params$spawning_habitat
dimnames(params$spawning_habitat) <- spawn_dimnames

rear_dimnames <- dimnames(params$inchannel_habitat_fry)
all_inputs$fry_out -> params$inchannel_habitat_fry
dimnames(params$inchannel_habitat_fry) <- rear_dimnames
all_inputs$juv_out -> params$inchannel_habitat_juvenile
dimnames(params$inchannel_habitat_juvenile) <- rear_dimnames

flood_dimnames <- dimnames(params$floodplain_habitat)
all_inputs$floodP -> params$floodplain_habitat
flood_dimnames -> dimnames(params$floodplain_habitat)

bypass_dimnames <- dimnames(params$sutter_habitat)
all_inputs$sutter_out -> params$sutter_habitat
dimnames(params$sutter_habitat) <- bypass_dimnames
all_inputs$yolo_out -> params$yolo_habitat
dimnames(params$yolo_habitat) <- bypass_dimnames


delta_dimnames <- dimnames(params$delta_habitat)
all_inputs$delta_out -> params$delta_habitat
dimnames(params$delta_habitat) <- delta_dimnames

all_inputs$cc.aloc -> params$cross_channel_stray_rate
names(params$cross_channel_stray_rate) <- springRunDSM::watershed_labels
all_inputs$oth.aloc -> params$stray_rate
names(params$stray_rate) <- springRunDSM::watershed_labels

all_inputs$returnProps -> params$month_return_proportions
names(params$month_return_proportions) <- month.abb[3:6]

all_inputs$prop.hatch -> params$proportion_hatchery
names(params$proportion_hatchery) <- springRunDSM::watershed_labels

retq_dimnames <- dimnames(params$prop_flow_natal)
as.matrix(all_inputs$retQ[,-1]) -> params$prop_flow_natal
dimnames(params$prop_flow_natal) <- retq_dimnames

overtop_dimnames <- dimnames(params$gates_overtopped)
all_inputs$gate.top -> params$gates_overtopped
dimnames(params$gates_overtopped) <- overtop_dimnames

mc_dimnames <- dimnames(params$migratory_temperature_proportion_over_20)
as.matrix(all_inputs$ptemp20mc[,c(-1,-2)]) -> params$migratory_temperature_proportion_over_20
mc_dimnames -> dimnames(params$migratory_temperature_proportion_over_20)

dimnames_31_12_22 <- dimnames(params$degree_days)
all_inputs$DegDay -> params$degree_days
dimnames(params$degree_days) <- dimnames_31_12_22

dimnames_31_12_21 <- dimnames(params$avg_temp)
all_inputs$juv.tmp -> params$avg_temp
dimnames(params$avg_temp) <- dimnames_31_12_21

all_inputs$p.diver -> params$proportion_diverted
dimnames(params$proportion_diverted) <- dimnames_31_12_21
all_inputs$t.diver -> params$total_diverted
dimnames(params$total_diverted) <- dimnames_31_12_21

dimnames_31_12 <- dimnames(params$prop_pulse_flows)
as.matrix(all_inputs$prop.pulse[,-1])/100 -> params$prop_pulse_flows
dimnames(params$prop_pulse_flows) <- dimnames_31_12

all_inputs$dlt.divers -> params$delta_proportion_diverted
dimnames(params$delta_proportion_diverted) <- delta_dimnames
all_inputs$dlt.divers.tot -> params$delta_total_diverted
dimnames(params$delta_total_diverted) <- delta_dimnames

all_inputs$fp.weeks -> params$weeks_flooded
dimnames(params$weeks_flooded) <- dimnames_31_12_21

upsq_dn <- dimnames(params$upper_sacramento_flows)
as.matrix(all_inputs$upSacQ) -> params$upper_sacramento_flows
upsq_dn -> dimnames(params$upper_sacramento_flows)

all_inputs$Dlt.inf -> params$delta_inflow
dimnames(params$delta_inflow) <- delta_dimnames
all_inputs$juv.tmp.dlt -> params$avg_temp_delta
dimnames(params$avg_temp_delta) <- delta_dimnames

pqb_dn <- dimnames(params$proportion_flow_bypass)
all_inputs$prop.Q.bypasses[,,1] -> params$proportion_flow_bypass[, ,1]
all_inputs$prop.Q.bypasses[,,5] -> params$proportion_flow_bypass[, ,2]
pqb_dn -> dimnames(params$proportion_flow_bypass)

all_inputs$inps$hatch.alloc -> params$hatchery_allocation
names(params$hatchery_allocation) <- springRunDSM::watershed_labels
all_inputs$inps$prop.nat.remov -> params$natural_adult_removal_rate
names(params$natural_adult_removal_rate) <- springRunDSM::watershed_labels
all_inputs$inps$SCDELT -> params$south_delta_routed_watersheds
names(params$south_delta_routed_watersheds) <- springRunDSM::watershed_labels
all_inputs$inps$TISD -> params$tisdale_bypass_watershed
names(params$tisdale_bypass_watershed) <- springRunDSM::watershed_labels
all_inputs$inps$YOLO -> params$yolo_bypass_watershed
names(params$yolo_bypass_watershed) <- springRunDSM::watershed_labels
all_inputs$inps$A.HARV -> params$adult_harvest_rate
names(params$adult_harvest_rate) <- springRunDSM::watershed_labels
all_inputs$inps$P.scour.nst -> params$prob_nest_scoured
names(params$prob_nest_scoured) <- springRunDSM::watershed_labels
all_inputs$inps$P.strand.early -> params$prob_strand_early
names(params$prob_strand_early) <- springRunDSM::watershed_labels
all_inputs$inps$P.strand.late -> params$prob_strand_late
names(params$prob_strand_late) <- springRunDSM::watershed_labels
all_inputs$inps$High.pred -> params$prop_high_predation
names(params$prop_high_predation) <- springRunDSM::watershed_labels
all_inputs$inps$contact -> params$contact_points
names(params$contact_points) <- springRunDSM::watershed_labels

all_inputs$Dlt.inp$contct.pts -> params$delta_contact_points
names(params$delta_contact_points) <- c("North Delta", "South Delta")
all_inputs$Dlt.inp$High.pred -> params$delta_prop_high_predation
names(params$delta_prop_high_predation) <- c("North Delta", "South Delta")

all_inputs$egg.tmp.eff$mean_temp_effect -> params$mean_egg_temp_effect

# synthetic shuffle these
all_inputs$Q_free -> params$freeport_flows
all_inputs$Q_vern -> params$vernalis_flows
all_inputs$Q_stck -> params$stockton_flows
all_inputs$Temp_vern -> params$vernalis_temps
all_inputs$Temp_pp -> params$prisoners_point_temps
all_inputs$CVP_exp -> params$CVP_exports
all_inputs$SWP_exp -> params$SWP_exports

# spawn_decay_rate = DSMscenario::spawn_decay_rate
all_inputs$states$spwnDecay -> params$spawn_decay_rate
# rear_decay_rate = DSMscenario::rear_decay_rate
all_inputs$states$rearDecay -> params$rear_decay_rate

# confirmed they are the same
# all_inputs$dlt.gates$days_closed <- params$cc_gates_prop_days_closed
# all_inputs$dlt.gates$days_closed/days_in_month(1:12)

# TODO this is wierd
gro_dn <- dimnames(params$growth_rates)
structure(c(0.0045914028239995, 0, 0, 0, 0.995346079527392, 0.508197615704544,
            0, 0, 6.2517648608762e-05, 0.491802384294065, 0.750034293109025,
            0, 0, 1.39133149446025e-12, 0.249965706890975, 1), .Dim = c(4L, 4L)) -> params$growth_rates
# growthMatrices<-Hab.growth(daily.rates=c(grow.ic,grow.fp),wks.fld=fp.weeks[,mnth,yr+1])
# trans_mat_river<-growthMatrices$T.mtx.ic
# trans_mat_flood<-growthMatrices$T.mtx.fp
gro_dn -> dimnames(params$growth_rates)

structure(c(0.0045914028239995, 0, 0, 0, 0.995346079527392, 0.508197615704544,
            0, 0, 6.2517648608762e-05, 0.491802384294065, 0.750034293109025,
            0, 0, 1.39133149446025e-12, 0.249965706890975, 1, 0.00353734264565751,
            0, 0, 0, 0.898316948523529, 0.389212293741988, 0, 0, 0.0980860690180065,
            0.603923710042661, 0.57993723717849, 0, 5.96398128066e-05, 0.00686399621535116,
            0.42006276282151, 1, 0.00248328246731551, 0, 0, 0, 0.801287817519667,
            0.270226971754649, 0, 0, 0.196109620387404, 0.71604503581461,
            0.409840181211634, 0, 0.0001192796256132, 0.0137279924307407,
            0.590159818788366, 1, 0.00142922228897352, 0, 0, 0, 0.704258686515805,
            0.151241649742527, 0, 0, 0.294133171756802, 0.828166361609913,
            0.239743125208455, 0, 0.0001789194384198, 0.0205919886475598,
            0.760256874791545, 1, 0.000375162110631526, 0, 0, 0, 0.607229555511942,
            0.0322563277056229, 0, 0, 0.3921567231262, 0.940287687428568,
            0.0696460691689542, 0, 0.0002385592512264, 0.0274559848658086,
            0.930353930831046, 1), .Dim = c(4L, 4L, 5L)) -> params$growth_rates_floodplain


old_seeds <- structure(c(0, 0, 340, 0, 0, 12987, 789, 0, 0, 491, 0, 691, 0, 
            0, 0, 0, 0, 0, 1441, 79, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 266, 0, 0, 11827, 732, 0, 0, 379, 0, 609, 0, 0, 0, 0, 0, 0, 
            2054, 76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 192, 0, 0, 8015, 
            478, 0, 0, 267, 0, 324, 0, 0, 0, 0, 0, 0, 933, 56, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 60, 0, 0, 2542, 140, 0, 0, 84, 0, 
            47, 0, 0, 0, 0, 0, 0, 2, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
), .Dim = c(31L, 25L))






