SA_AREA_CODES = c("IRBETNW", "IRBETNE",
                  "IRBETWE", "IRBETEA",
                  "IRBETSW", "IRBETSC", "IRBETSE",
                  "IRBETZW", "IRBETZC", "IRBETZE")

SA_AREAS = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = SA_AREA_CODES, connection = IOTC)
FG_5_TO_SA_AREAS = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREA_CODES, 
                                                                                source_grid_type_code = grid_5x5)
  
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
AREA_NAMES          = SA_AREAS$NAME_SHORT

FISHERY_GROUP_NAMES = c("PSFS - Industrial purse seines on free-swimming schools",
                        "PSLS - Industrial purse seines on associated schools (FAD)", 
                        "LL - [Deep-]freezing longline fisheries, especially Japan, Korea, Taiwan,China, Seychelles, and EU", 
                        "FL - Fresh-tuna longline fisheries, especially Indonesia and Taiwan,China", 
                        "BB - All pole-and-line and small seine fisheries (catching small fish)", 
                        "LINE - All fisheries using handlines, and small longlines, including the gillnet and longline combination fishery of Sri Lanka", 
                        "OTHER - Other fisheries including gillnet, trolling and other minor artisanal gears")

# For this to be of help, we need to break down SF grids as 5x5 grids first, considering that several records might indeed refer to larger (or smaller) grids
# IRALB01 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB01")
# IRALB02 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB02")
# IRALB03 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB03")
# IRALB04 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB04")
assign_area = function(dataset) {
  SOURCE_FGS = unique(dataset$FISHING_GROUND_CODE)

  FGS_TO_5 = iotc.core.gis.cwp.IO::grid_intersections_by_target_grid_type(source_grid_codes = SOURCE_FGs, 
                                                                          target_grid_type_code = grid_5x5)
  
  dataset = merge(dataset, 
                  FGS_TO_5, 
                  by.x = "FISHING_GROUND_CODE", 
                  by.y = "SOURCE_FISHING_GROUND_CODE")
  
  dataset$FISHING_GROUND_CODE = TARGET_FISHING_GROUND_CODE
  dataset$SOURCE_FISHING_GROUND_CODE = NULL
  dataset$PROPORTION = NULL
  
  dataset = merge(dataset, FG_5_TO_SA_AREAS,
                  by.x = "FISHING_GROUND_CODE",
                  by.y = "SOURCE_FISHING_GROUND_CODE")
    
  # WRONG DEFINITIONS (used for previous assessments)
  
  N = dataset[substr(FISHING_GROUND_CODE, 2, 4) <= "225"]
  S = dataset[substr(FISHING_GROUND_CODE, 2, 4)  > "225"]
  
  N[substr(FISHING_GROUND_CODE, 5, 7) <= "075", AREA := 1]
  N[substr(FISHING_GROUND_CODE, 5, 7)  > "075", AREA := 2]
  
  S[substr(FISHING_GROUND_CODE, 5, 7) <= "075", AREA := 3]
  S[substr(FISHING_GROUND_CODE, 5, 7)  > "075", AREA := 4]
  
  # CORRECT DEFINITIONS (to be used for forthcoming assessments)
  
  #N = dataset[substr(FISHING_GROUND_CODE, 2, 4)  < "225"]
  #S = dataset[substr(FISHING_GROUND_CODE, 2, 4) >= "225"]
  
  #N[substr(FISHING_GROUND_CODE, 5, 7)  < "075", AREA := 1]
  #N[substr(FISHING_GROUND_CODE, 5, 7) >= "075", AREA := 2]
  
  #S[substr(FISHING_GROUND_CODE, 5, 7)  < "075", AREA := 3]
  #S[substr(FISHING_GROUND_CODE, 5, 7) >= "075", AREA := 4]
  
  #dataset[substr(FISHING_GROUND_CODE, 2, 4)  < "225" & substr(FISHING_GROUND_CODE, 5, 7)  < "075", AREA := 1]
  #dataset[substr(FISHING_GROUND_CODE, 2, 4)  < "225" & substr(FISHING_GROUND_CODE, 5, 7) >= "075", AREA := 2]
  #dataset[substr(FISHING_GROUND_CODE, 2, 4) >= "225" & substr(FISHING_GROUND_CODE, 5, 7)  < "075", AREA := 3]
  #dataset[substr(FISHING_GROUND_CODE, 2, 4) >= "225" & substr(FISHING_GROUND_CODE, 5, 7) >= "075", AREA := 4]
  
  return(rbind(N, S))
}

assign_fishery = function(dataset) {
  dataset[, FISHERY_TYPE := "OT"]
  dataset[GEAR_CODE %in% c("LL", "LLOB", "FLL", "LLCO", "ELL", "ELLOB", "SLL", "LLEX"), FISHERY_TYPE := "LL"]
  dataset[GEAR_CODE %in% c("PS", "PSOB"),                                               FISHERY_TYPE := "PS"]
  dataset[GEAR_CODE %in% c("GILL") & FLEET == "TWN",                                    FISHERY_TYPE := "DN"]
  
  dataset[, FISHERY := paste0(FISHERY_TYPE, AREA)]
  
  dataset[FISHERY_TYPE == "PS",  FISHERY := paste0(FISHERY_TYPE, 1)]
  dataset[FISHERY_TYPE == "OT" & FLEET == "MDV", FISHERY := paste0(FISHERY_TYPE, 1)]
  dataset[FISHERY_TYPE == "OT" & FLEET == "AUS", FISHERY := paste0(FISHERY_TYPE, 4)]
  dataset[FISHERY_TYPE == "DN" & AREA %in% c(1, 3), FISHERY := paste0(FISHERY_TYPE, 3)]
  dataset[FISHERY_TYPE == "DN" & AREA %in% c(2, 4), FISHERY := paste0(FISHERY_TYPE, 4)]
  
  return(dataset)
}

update_fishery_groups = function(dataset) {
  dataset[, FISHERY_GROUP := "OT - Other gears"]
  dataset[GEAR_CODE == "PS", FISHERY_GROUP  := "PS - industrial purse seines"]
  dataset[GEAR_CODE == "FLL", FISHERY_GROUP := "FLL - fresh-tuna longliners"]
  dataset[GEAR_CODE == "GILL" & FLEET == "TWN", FISHERY_GROUP := "DN - Driftnets"]
  dataset[GEAR_CODE %in% c("LL", "LLCO", "ELL", "SLL", "LLEX"), FISHERY_GROUP := "LL - all other longliners"]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_NAMES,
    levels = FISHERY_GROUP_NAMES,
    ordered = TRUE
  )
  
  return(dataset)
}