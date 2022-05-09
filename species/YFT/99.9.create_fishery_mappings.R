SA_AREA_CODES = c("IRYFT1A", "IRYFT1B", "IRYFT02", "IRYFT03", "IRYFT04", "IRYFT00")

SA_AREA_MAPPINGS = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(grid_5x5, SA_AREA_CODES)

CE = merge(CE_R_YQMFG, SA_AREA_MAPPINGS,
           by.x = "FISHING_GROUND_CODE",
           by.y = "SOURCE_FISHING_GROUND_CODE",
           all.x = TRUE)

CE[is.na(TARGET_FISHING_GROUND_CODE), .(EST_MT = sum(EST_MT, na.rm = TRUE)), keyby = .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE)]

CE_A_mappings = unique(CE[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, AREA_ORIG = TARGET_FISHING_GROUND_CODE)])

LAST_FI = fread("./species/YFT/YFT_FISHERIES_LAST.csv")

LAST_FI_mappings = unique(LAST_FI[, .(FLEET = DSFleetCode, GEAR_CODE = Gear, FISHERY = FisheryCode)])

CE_A_F_mappings = merge(CE_A_mappings, LAST_FI_mappings,
                        by = c("FLEET", "GEAR_CODE"),
                        all.x = TRUE)

# Adds missing mappings (from previous assessment)

CE_A_F_mappings[FLEET == "AUS" & GEAR_CODE == "RR",     FISHERY := "OT"]
CE_A_F_mappings[FLEET == "EUESP" & GEAR_CODE == "LLEX", FISHERY := "LL"]
CE_A_F_mappings[FLEET == "MOZ" & GEAR_CODE %in% c("BS", "HARP"), FISHERY := "OT"]
CE_A_F_mappings[FLEET == "PAK" & GEAR_CODE == "HAND",   FISHERY := "HD"]
CE_A_F_mappings[FLEET == "SYC" & GEAR_CODE == "FLL",    FISHERY := "LF"]
CE_A_F_mappings[FLEET %in% c("EUMYT", "SYC") & GEAR_CODE == "LLCO", FISHERY := "LF"]

# Subdivides the 'PS' fishery in its FS / LS components

CE_A_F_mappings[FISHERY == "PS" & SCHOOL_TYPE_CODE == "FS", FISHERY := "FS"]
CE_A_F_mappings[FISHERY == "PS" & SCHOOL_TYPE_CODE == "LS", FISHERY := "LS"]

# Initializes the "area mappings" to transfer catches from one fishery / area to another (area)

CE_A_F_mappings[FISHERY == "LL" & AREA_ORIG == "IRYFT1a", SA_AREA := "R1a"]
CE_A_F_mappings[FISHERY == "LL" & AREA_ORIG == "IRYFT1b", SA_AREA := "R1b"]
CE_A_F_mappings[FISHERY == "LL" & AREA_ORIG == "IRYFT02", SA_AREA := "R2"]
CE_A_F_mappings[FISHERY == "LL" & AREA_ORIG == "IRYFT03", SA_AREA := "R3"]
CE_A_F_mappings[FISHERY == "LL" & AREA_ORIG == "IRYFT04", SA_AREA := "R4"]
CE_A_F_mappings[FISHERY == "LF", SA_AREA := "R4"]
CE_A_F_mappings[FISHERY == "HD", SA_AREA := "R1a"]
CE_A_F_mappings[FISHERY == "BB", SA_AREA := "R1b"]
CE_A_F_mappings[FISHERY %in% c("FS", "LS", "TR") & AREA_ORIG %in% c("IRYFT1A", "IRYFT1B"), SA_AREA := "R1b"]
CE_A_F_mappings[FISHERY %in% c("FS", "LS", "TR") & AREA_ORIG %in% c("IRYFT02"), SA_AREA := "R2"]
CE_A_F_mappings[FISHERY %in% c("FS", "LS", "TR") & AREA_ORIG %in% c("IRYFT03", "IRYFT04"), SA_AREA := "R4"]
CE_A_F_mappings[FISHERY %in% c("GI", "OT") & AREA_ORIG %in% c("IRYFT1A", "IRYFT1B", "IRYFT02"), SA_AREA := "R1a"]
CE_A_F_mappings[FISHERY %in% c("GI", "OT") & AREA_ORIG %in% c("IRYFT03", "IRYFT04"), SA_AREA := "R4"]

CE_A_F_mappings[AREA_ORIG == "IRYFT00", SA_AREA := "R0"]

CE_A_F_mappings[AREA_ORIG == "IRYFT1A", AREA_ORIG := "R1a"]
CE_A_F_mappings[AREA_ORIG == "IRYFT1B", AREA_ORIG := "R1b"]
CE_A_F_mappings[AREA_ORIG == "IRYFT02", AREA_ORIG := "R2"]
CE_A_F_mappings[AREA_ORIG == "IRYFT03", AREA_ORIG := "R3"]
CE_A_F_mappings[AREA_ORIG == "IRYFT04", AREA_ORIG := "R4"]
CE_A_F_mappings[AREA_ORIG == "IRYFT00", AREA_ORIG := "R0"]

CE_A_F_mappings = CE_A_F_mappings[!is.na(AREA_ORIG)]

CE_A_F_mappings$AREA_ORIG = factor(
  CE_A_F_mappings$AREA_ORIG,
  levels = c("R1a", "R1b", "R2", "R3", "R4", "R0"),
  labels = c("R1a", "R1b", "R2", "R3", "R4", "R0"),
  ordered = TRUE
)

CE_A_F_mappings$SCHOOL_TYPE_CODE = "UNCL"

CE_A_F_mappings[GEAR_CODE == "PS" & FISHERY == "FS", SCHOOL_TYPE_CODE := "FS"]
CE_A_F_mappings[GEAR_CODE == "PS" & FISHERY == "LS", SCHOOL_TYPE_CODE := "LS"]                

CE_A_F_mappings_PIVOT =
  dcast.data.table(
    CE_A_F_mappings,
    value.var = "SA_AREA",
    FLEET + GEAR_CODE + SCHOOL_TYPE_CODE + FISHERY ~ AREA_ORIG,
    fill = NA,
    fun.aggregate = unique,
    drop = c(TRUE, FALSE)
  )

write.csv(CE_A_F_mappings_PIVOT, file = "./species/YFT/YFT_FISHERIES.csv", row.names = FALSE, na = "")