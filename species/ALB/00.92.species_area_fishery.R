SA_AREAS = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = c("IRALB01", "IRALB02", "IRALB03", "IRALB04"), connection = IOTC)
SA_AREAS[CODE == "IRALB01", NAME_SHORT := "1 - Northwest"]
SA_AREAS[CODE == "IRALB02", NAME_SHORT := "2 - Northeast"]
SA_AREAS[CODE == "IRALB03", NAME_SHORT := "3 - Southwest"]
SA_AREAS[CODE == "IRALB04", NAME_SHORT := "4 - Southeast"]

# Area and fishery group names
AREA_NAMES          = SA_AREAS$NAME_SHORT
FISHERY_GROUP_NAMES = c("DN - Driftnets", 
                        "FLL - fresh-tuna longliners", 
                        "LL - all other longliners", 
                        "PS - industrial purse seines", 
                        "OT - Other gears")

# For this to be of help, we need to break down SF grids as 5x5 grids first, considering that several records might indeed refer to larger (or smaller) grids
# IRALB01 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB01")
# IRALB02 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB02")
# IRALB03 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB03")
# IRALB04 = grid_intersections_by_source_grid_type(grid_5x5, "IRALB04")
assign_area = function(dataset) {
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