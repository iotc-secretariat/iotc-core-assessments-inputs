BET_FISHERIES_AREA_MAPPINGS = fread(species_folder(SPECIES, "BET_FISHERIES.csv"))

BET_FISHERIES_MAPPINGS = BET_FISHERIES_AREA_MAPPINGS[, .(FLEET = Fleet, GEAR_CODE = Gear, SCHOOL_TYPE_CODE = SchoolType, FISHERY = Fishery)]
BET_AREA_MAPPINGS = 
  melt.data.table(
    BET_FISHERIES_AREA_MAPPINGS,
    variable.name = "AREA_SRC",
    value.name = "AREA_DEST",
    id.vars = c("Fleet", "Gear", "SchoolType", "Fishery")
  )[, .(FLEET = Fleet, GEAR_CODE = Gear, SCHOOL_TYPE_CODE = SchoolType, FISHERY, AREA_SRC, AREA_DEST)]

BET_AREA_MAPPINGS$FISHERY = NULL

SA_AREA_CODES = c("IRBETNW", "IRBETNE",
                  "IRBETWE", "IRBETEA",
                  "IRBETSW", "IRBETSC", "IRBETSE",
                  "IRBETZW", "IRBETZC", "IRBETZE")

SA_AREAS = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = SA_AREA_CODES, connection = IOTC)

FG_5_TO_SA_AREAS = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREA_CODES, 
                                                                                source_grid_type_code = grid_5x5)

FG_1_TO_SA_AREAS = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREA_CODES, 
                                                                                source_grid_type_code = grid_1x1)

FG_TO_SA_AREAS = rbind(FG_5_TO_SA_AREAS, FG_1_TO_SA_AREAS)  

SA_AREAS[CODE == "IRBETNW", NAME_SHORT := "A0 - Northwest"]
SA_AREAS[CODE == "IRBETNE", NAME_SHORT := "A0 - Northeast"]
SA_AREAS[CODE == "IRBETWE", NAME_SHORT := "A1 - West"]
SA_AREAS[CODE == "IRBETEA", NAME_SHORT := "A2 - East"]
SA_AREAS[CODE == "IRBETSW", NAME_SHORT := "A3 - Southwest"]
SA_AREAS[CODE == "IRBETSC", NAME_SHORT := "A3 - South-central"]
SA_AREAS[CODE == "IRBETSE", NAME_SHORT := "A3 - Southeast"]
SA_AREAS[CODE == "IRBETZW", NAME_SHORT := "A0 - South-Southwest"]
SA_AREAS[CODE == "IRBETZC", NAME_SHORT := "A0 - South-South-central"]
SA_AREAS[CODE == "IRBETZE", NAME_SHORT := "A0 - South-Southeast"]

# Area and fishery group names
AREA_NAMES = c("1 - West", "2 - East", "3 - South")

assign_area = function(dataset) {
  dataset = merge(dataset, FG_TO_SA_AREAS,
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
  
  delete_column(dataset, "PROPORTION")
  
  dataset = merge(dataset, BET_AREA_MAPPINGS,
                  by.x = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "TARGET_FISHING_GROUND_CODE"),
                  by.y = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "AREA_SRC"),
                  all.x = TRUE)
   
  delete_column(dataset, "TARGET_FISHING_GROUND_CODE")
  
  colnames(dataset)[which(colnames(dataset) == "AREA_DEST")] = "AREA"

  dataset$AREA = factor(
    dataset$AREA,
    levels = c(1, 2, 3),
    labels = AREA_NAMES,
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

FISHERY_GROUP_CODES = c("PS - industrial purse seine", 
                        "LL - deep-freezing longlines", "FL - fresh tuna longlines", 
                        "BB - pole-and-lines and small seines", 
                        "LI - handlines and small longlines", 
                        "OT - other gears")

update_fishery_groups = function(dataset) {
  dataset[,                               FISHERY_GROUP := "OT - other gears"]
  dataset[FISHERY %in% c("PSFS", "PSLS"), FISHERY_GROUP := "PS - industrial purse seine"]
  dataset[FISHERY == "LL",                FISHERY_GROUP := "LL - deep-freezing longlines"]
  dataset[FISHERY == "FL",                FISHERY_GROUP := "FL - fresh tuna longlines"]
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