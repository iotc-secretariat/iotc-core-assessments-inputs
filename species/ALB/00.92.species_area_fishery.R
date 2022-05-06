SA_AREA_CODES = c("IRALB01", "IRALB02", "IRALB03", "IRALB04")

SA_AREAS = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = SA_AREA_CODES, connection = IOTC)
FG_5_TO_SA_AREAS = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREA_CODES, 
                                                                                source_grid_type_code = grid_5x5)

# HERE IT SHOULD CHECK THAT EACH 5x5 GRID IS NOT ASSIGNED TO MORE THAN 1 SA AREA

SA_AREAS[CODE == "IRALB01", NAME_SHORT := "1 - Northwest"]
SA_AREAS[CODE == "IRALB02", NAME_SHORT := "2 - Northeast"]
SA_AREAS[CODE == "IRALB03", NAME_SHORT := "3 - Southwest"]
SA_AREAS[CODE == "IRALB04", NAME_SHORT := "4 - Southeast"]

# Area and fishery group names
AREA_NAMES = sort(SA_AREAS$NAME_SHORT)
AREA_CODES = sort(SA_AREAS$CODE)

SA_AREAS_ORIG   = SA_AREAS
AREA_ORIG_CODES = AREA_CODES
AREA_ORIG_NAMES = AREA_NAMES

FISHERY_GROUP_NAMES = c("DN - Driftnets", 
                        "FLL - fresh-tuna longliners", 
                        "LL - all other longliners", 
                        "PS - industrial purse seines", 
                        "OT - Other gears")

assign_area = function(dataset) {
  dataset = merge(dataset, FG_5_TO_SA_AREAS,
                  by.x = "FISHING_GROUND_CODE",
                  by.y = "SOURCE_FISHING_GROUND_CODE",
                  all.x = TRUE)
  
  dataset[, AREA_ORIG := TARGET_FISHING_GROUND_CODE]

  delete_column(dataset, c("TARGET_FISHING_GROUND_CODE", "PROPORTION"))

  unmapped_grid_codes = unique(dataset[is.na(AREA_ORIG)]$FISHING_GROUND_CODE)
  
  if(length(unmapped_grid_codes) >= 1) {
    print(paste0(length(unmapped_grid_codes), " grids have not been assigned to any SA area..."))
    print(unique(dataset[is.na(AREA_ORIG)]$FISHING_GROUND_CODE))
   
    dataset = dataset[!is.na(AREA_ORIG)]
  }
  
  dataset$AREA_ORIG = factor(
    dataset$AREA_ORIG,
    levels = AREA_CODES,
    labels = AREA_NAMES,
    ordered = TRUE
  )
  
  dataset$AREA = dataset$AREA_ORIG
  
  return(dataset)
}

assign_fishery = function(dataset) {
  dataset[,                                                                             FISHERY_TYPE := "OT"]
  dataset[GEAR_CODE %in% c("LL", "LLOB", "FLL", "LLCO", "ELL", "ELLOB", "SLL", "LLEX"), FISHERY_TYPE := "LL"]
  dataset[GEAR_CODE %in% c("PS", "PSOB"),                                               FISHERY_TYPE := "PS"]
  dataset[GEAR_CODE %in% c("GILL") & FLEET == "TWN",                                    FISHERY_TYPE := "DN"]
  
  dataset[AREA == "1 - Northwest", FISHERY := paste0(FISHERY_TYPE, 1)]
  dataset[AREA == "2 - Northeast", FISHERY := paste0(FISHERY_TYPE, 2)]
  dataset[AREA == "3 - Southwest", FISHERY := paste0(FISHERY_TYPE, 3)]
  dataset[AREA == "4 - Southeast", FISHERY := paste0(FISHERY_TYPE, 4)]
  
  dataset[FISHERY_TYPE == "PS",  `:=`(FISHERY = paste0(FISHERY_TYPE, 1), AREA = "1 - Northwest")]
  dataset[FISHERY_TYPE == "OT" & FLEET == "MDV", `:=`(FISHERY = paste0(FISHERY_TYPE, 1), AREA = "1 - Northwest")]
  dataset[FISHERY_TYPE == "OT" & FLEET == "AUS", `:=`(FISHERY = paste0(FISHERY_TYPE, 4), AREA = "4 - Southeast")]
  dataset[FISHERY_TYPE == "DN" & AREA_ORIG %in% c("1 - Northwest", "3 - Southwest"), `:=`(FISHERY = paste0(FISHERY_TYPE, 3), AREA = "3 - Southwest")]
  dataset[FISHERY_TYPE == "DN" & AREA_ORIG %in% c("2 - Northeast", "4 - Southeast"), `:=`(FISHERY = paste0(FISHERY_TYPE, 4), AREA = "4 - Southeast")]
  
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