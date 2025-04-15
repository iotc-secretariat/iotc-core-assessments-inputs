SA_FISHERY_AREA_MAPPINGS_FILE = "FISHERY_AREA_MAPPINGS.csv"

SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRALB01", "IRALB02", "IRALB03", "IRALB04"),
  AREA_CODE  = c("A1", "A2", "A3", "A4"),
  NAME_SHORT = c("Northwest",
                 "Northeast",
                 "Southwest",
                 "Southeast")
)

SA_AREAS_CONFIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]
SA_AREAS_CONFIG_ORIG = copy(SA_AREAS_CONFIG)

postprocess_fishery = function(dataset) {
  dataset[, SF_FISHERY := fifelse(FISHERY %in% c("PS", "DN"), "PSPLGI", "LLOT")]
  dataset[, FISHERY := paste0(FISHERY, str_sub(AREA, -1))]
  
  return(dataset)
}

FISHERY_CODES = c("DN1", "DN2", "DN3", "DN4", "LL1", "LL2", "LL3", "LL4", "OT1", "OT2", "OT3", "OT4", "PS1", "PS2", "PS3", "PS4")  # Updated to reflect the TWN GIOF fishery in the north

#FISHERY_CODES = c("DN3", "DN4", "LL1", "LL2", "LL3", "LL4", "OT1", "OT2", "OT3", "OT4", "PS1", "PS2", "PS3", "PS4")

FISHERY_GROUP_NAMES = c("DN - driftnets", 
                        "FLL - fresh-tuna longliners", 
                        "LL - all other longliners", 
                        "PS - industrial purse seines", 
                        "OT - other gears")

update_fishery_groups = function(dataset) {
  dataset[, FISHERY_GROUP := "OT - other gears"]
  dataset[GEAR_CODE == "PS", FISHERY_GROUP  := "PS - industrial purse seines"]
  dataset[GEAR_CODE == "FLL", FISHERY_GROUP := "FLL - fresh-tuna longliners"]
  dataset[GEAR_CODE == "GIOF" & FLEET %in% c("IDN", "LKA", "TWN"), FISHERY_GROUP := "DN - driftnets"]  # Changed from GILL to GIOF and "TWN" to c("IDN", "LKA", "TWN")
  dataset[GEAR_CODE %in% c("LL", "LLCO", "ELL", "SLL", "LLEX"), FISHERY_GROUP := "LL - all other longliners"]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_NAMES,
    levels = FISHERY_GROUP_NAMES,
    ordered = TRUE
  )
  
  return(dataset)
}
