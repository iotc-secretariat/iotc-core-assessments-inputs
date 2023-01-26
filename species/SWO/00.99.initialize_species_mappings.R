l_warn("This file (00.99.initialize_species_mappings.R) shouldn't be sourced!")

CER = read.csv("./species/ALB/WP/WPTmT08-AS/input/CAS/CE_raised.csv")
CER = unique(as.data.table(CER)[, .(FLEET = str_trim(Fleet), GEAR_CODE = str_trim(Gear), SCHOOL_TYPE_CODE = str_trim(SchoolType), FISHING_GROUND_CODE = as.character(Grid))])
SF = unique(SF_YQMFG[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)])

CESF = unique(rbind(CER, SF))

FG_5_TO_SA_AREAS = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = c("IRALB01", "IRALB02", "IRALB03", "IRALB04"), 
                                                                                source_grid_type_code = grid_5x5)

CESF = merge(CESF, FG_5_TO_SA_AREAS,
             by.x = "FISHING_GROUND_CODE",
             by.y = "SOURCE_FISHING_GROUND_CODE",
             all.x = TRUE)

CESF_U = unique(CESF[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, AREA = TARGET_FISHING_GROUND_CODE)])

CESF_U[,                                                                             FISHERY := "OT"]
CESF_U[GEAR_CODE %in% c("LL", "LLOB", "FLL", "LLCO", "ELL", "ELLOB", "SLL", "LLEX"), FISHERY := "LL"]
CESF_U[GEAR_CODE %in% c("PS", "PSOB"),                                               FISHERY := "PS"]
CESF_U[GEAR_CODE %in% c("GILL") & FLEET == "TWN",                                    FISHERY := "DN"]

CESF_U$AREA = factor(
  CESF_U$AREA,
  levels = c("IRALB01", "IRALB02", "IRALB03", "IRALB04"),
  labels = c("A1", "A2", "A3", "A4"),
  ordered = TRUE
)

CESF_U$AREA_ORIG = CESF_U$AREA

CESF_U[FISHERY == "OT" & FLEET == "MDV", AREA := "A1"]
CESF_U[FISHERY == "OT" & FLEET == "AUS", AREA := "A4"]
CESF_U[FISHERY == "DN" & FLEET == "TWN" & AREA_ORIG == "A1",  AREA := "A3"]
CESF_U[FISHERY == "DN" & FLEET == "TWN" & AREA_ORIG == "A2",  AREA := "A4"]

CESF_U = unique(CESF_U)

CESF_P = 
  dcast.data.table(
    CESF_U,
    formula = FLEET + GEAR_CODE + SCHOOL_TYPE_CODE + FISHERY ~ AREA_ORIG,
    value.var = "AREA",
    fun.aggregate = min,
    drop = c(TRUE, FALSE)
  )

write.csv(CESF_P, file = "./species/ALB/FISHERY_AREA_MAPPINGS.csv", na = "", row.names = FALSE)