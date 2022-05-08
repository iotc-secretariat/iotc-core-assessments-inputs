BET_FISHERIES_AREA_MAPPINGS = fread(species_folder(SPECIES, "YFT_FISHERIES.csv"))

BET_FISHERIES_MAPPINGS = BET_FISHERIES_AREA_MAPPINGS[, .(FLEET = Fleet, GEAR_CODE = Gear, SCHOOL_TYPE_CODE = SchoolType, FISHERY = Fishery)]
BET_AREA_MAPPINGS = 
  melt.data.table(
    BET_FISHERIES_AREA_MAPPINGS,
    variable.name = "AREA_SRC",
    value.name = "AREA_DEST",
    id.vars = c("Fleet", "Gear", "SchoolType", "Fishery")
  )[, .(FLEET = Fleet, GEAR_CODE = Gear, SCHOOL_TYPE_CODE = SchoolType, FISHERY, AREA_SRC, AREA_DEST)]

BET_AREA_MAPPINGS$FISHERY = NULL

SA_AREA_CODES = c("IRYFT1A", "IRYFT1B", "IRYFT02", "IRYFT03", "IRYFT04")

SA_AREAS = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = SA_AREA_CODES, connection = IOTC)

SA_AREAS[CODE == "IRYFT1A", NAME_SHORT := "1a - Arabian sea"]
SA_AREAS[CODE == "IRYFT1B", NAME_SHORT := "1b - Western Indian ocean (tropical)"]
SA_AREAS[CODE == "IRYFT02", NAME_SHORT := "02 - Mozambique channel"]
SA_AREAS[CODE == "IRYFT03", NAME_SHORT := "03 - Southern Indian ocean"]
SA_AREAS[CODE == "IRYFT04", NAME_SHORT := "04 - Eastern Indian ocean (tropical)"]

SA_AREAS$CODE = factor(
  SA_AREAS$CODE,
  levels = SA_AREA_CODES,
  ordered = TRUE
)

SA_AREAS = SA_AREAS[order(CODE)]

# Area names
AREA_NAMES = SA_AREAS[order(CODE)]$NAME_SHORT

SA_AREA_ORIG_CODES = SA_AREA_CODES

SA_AREAS_ORIG = SA_AREAS

# Area names
AREA_ORIG_NAMES = AREA_NAMES
  
FG_5_TO_SA_AREAS_ORIG = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREA_ORIG_CODES, 
                                                                                source_grid_type_code = grid_5x5)

FG_1_TO_SA_AREAS_ORIG = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREA_ORIG_CODES, 
                                                                                source_grid_type_code = grid_1x1)

FG_TO_SA_AREAS_ORIG = rbind(FG_5_TO_SA_AREAS_ORIG, FG_1_TO_SA_AREAS_ORIG)  

assign_area = function(dataset) {
  dataset = merge(dataset, FG_TO_SA_AREAS_ORIG,
                  by.x = "FISHING_GROUND_CODE",
                  by.y = "SOURCE_FISHING_GROUND_CODE",
                  all.x = TRUE,
                  allow.cartesian = TRUE)

  unmapped_grid_codes = unique(dataset[is.na(TARGET_FISHING_GROUND_CODE)]$FISHING_GROUND_CODE)
  
  if(length(unmapped_grid_codes) >= 1) {
    print(paste0(length(unmapped_grid_codes), " grids have not been assigned to any SA area..."))
    print(unmapped_grid_codes)
    
    dataset = dataset[!is.na(TARGET_FISHING_GROUND_CODE)]
  }
  
  dataset$AREA_ORIG = dataset$TARGET_FISHING_GROUND_CODE
  
  delete_column(dataset, "PROPORTION")
  
  dataset = merge(dataset, BET_AREA_MAPPINGS,
                  by.x = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "TARGET_FISHING_GROUND_CODE"),
                  by.y = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "AREA_SRC"),
                  all.x = TRUE)
   
  delete_column(dataset, "TARGET_FISHING_GROUND_CODE")
  
  colnames(dataset)[which(colnames(dataset) == "AREA_DEST")] = "AREA"

  dataset$AREA = factor(
    dataset$AREA,
    levels = c(1, 2, 3, 0),
    labels = AREA_NAMES,
    ordered = TRUE
  )
  
  dataset$AREA_ORIG = factor(
    dataset$AREA_ORIG,
    levels = SA_AREA_ORIG_CODES,
    labels = AREA_ORIG_NAMES,
    ordered = TRUE
  )
  
  return(dataset)
}

FISHERY_CODES = c("PSFS", "PSLS", "LL", "FL", "BB", "LINE", "OTHER")

assign_fishery = function(dataset) {
  dataset = merge(dataset, BET_FISHERIES_MAPPINGS, 
                  by = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE"),
                  all.x = TRUE)
  
  missing_fishery = dataset[is.na(FISHERY)]
  num_missing_fishery = nrow(missing_fishery)
  
  if(num_missing_fishery > 0) {
    print(paste0(num_missing_fishery, " fleet / gear / school type mappings are missing"))
    print(unique(missing_fishery[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, YEAR)]))
  }
  
  dataset$FISHERY = factor(
    dataset$FISHERY,
    levels = FISHERY_CODES,
    labels = FISHERY_CODES,
    ordered = TRUE
  )
  
  return (dataset)
}

# STILL TO BE REFINED...

FISHERY_GROUP_CODES = c("PS - industrial purse seines", 
                        "LL - industrial longlines", 
                        "BB - pole-and-lines", 
                        "GN - ", 
                        "LI - handlines and troll lines", 
                        "OT - other gears")

update_fishery_groups = function(dataset) {
  dataset[,                               FISHERY_GROUP := "OT - other gears"]
  dataset[FISHERY %in% c("PSFS", "PSLS"), FISHERY_GROUP := "PS - industrial purse seines"]
  dataset[FISHERY %in% c("LL", "FL"),     FISHERY_GROUP := "LL - industrial longlines"]
  dataset[FISHERY == "BB",                FISHERY_GROUP := "BB - pole-and-lines and small seines"]
  dataset[FISHERY == "LINE",              FISHERY_GROUP := "LI - handlines and small longlines"]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_CODES,
    levels = FISHERY_GROUP_CODES,
    ordered = TRUE
  )
  
  return(dataset)
}