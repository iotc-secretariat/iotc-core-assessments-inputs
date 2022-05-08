# Builds the grids 1x1 & 5x5 to PS statistical areas mappings using the shapes of the corresponding irregular areas

G5_G1 = grid_intersections_by_grid_types(grid_5x5, grid_1x1)

PS_AREAS = c("IRS01ST", "IRS02ST", "IRS03ST", "IRS04ST", "IRS05ST", "IRS06ST", "IRS07ST", "IRS08ST", "IRS09ST", "IRS10ST")

FG_PS_1 = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(grid_1x1, PS_AREAS)[PROPORTION >= 0.0001]
FG_PS_1 = FG_PS_1[, .(GRID_1 = SOURCE_FISHING_GROUND_CODE, PS_AREA_CODE = TARGET_FISHING_GROUND_CODE)]

GRIDS_15_MAPPINGS = merge(FG_PS_1, G5_G1,
                          by.x = "GRID_1",
                          by.y = "TARGET_FISHING_GROUND_CODE",
                          all.x = TRUE,
                          allow.cartesian = TRUE)[, .(GRID_1, GRID_5 = SOURCE_FISHING_GROUND_CODE, PS_AREA_CODE)]

GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS01ST", `:=`(PS_AREA = "9100040", PS_AREA_NUMBER = 0)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS02ST", `:=`(PS_AREA = "9200020", PS_AREA_NUMBER = 2)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS03ST", `:=`(PS_AREA = "9200060", PS_AREA_NUMBER = 1)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS04ST", `:=`(PS_AREA = "9210020", PS_AREA_NUMBER = 3)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS05ST", `:=`(PS_AREA = "9200080", PS_AREA_NUMBER = 5)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS06ST", `:=`(PS_AREA = "9220060", PS_AREA_NUMBER = 4)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS07ST", `:=`(PS_AREA = "9120040", PS_AREA_NUMBER = 9)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS08ST", `:=`(PS_AREA = "9100080", PS_AREA_NUMBER = 8)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS09ST", `:=`(PS_AREA = "9100100", PS_AREA_NUMBER = 7)]
GRIDS_15_MAPPINGS[PS_AREA_CODE == "IRS10ST", `:=`(PS_AREA = "9200100", PS_AREA_NUMBER = 6)]

write.csv(GRIDS_15_MAPPINGS, file = "./references/GRIDS_1_5_PS_MAPPINGS.csv", row.names = FALSE)
save(list = "GRIDS_15_MAPPINGS", file = "./references/GRIDS_1_5_PS_MAPPINGS.RData")

# Builds the grids 5x5 to LL statistical areas mappings using the shapes of the corresponding irregular areas

G5_G1020 = grid_intersections_by_grid_types(grid_5x5, grid_10x20)

GRIDS_5_LL_MAPPINGS = G5_G1020[, .(GRID_5 = SOURCE_FISHING_GROUND_CODE, LL_AREA = TARGET_FISHING_GROUND_CODE)]

write.csv(GRIDS_5_LL_MAPPINGS, file = "./references/GRIDS_5_LL_MAPPINGS.csv", row.names = FALSE)
save(list = "GRIDS_5_LL_MAPPINGS", file = "./references/GRIDS_5_LL_MAPPINGS.RData")
