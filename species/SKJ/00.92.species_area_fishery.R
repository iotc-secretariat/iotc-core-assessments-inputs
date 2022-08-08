SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRALLIO"),
  AREA_CODE  = c("A1"),
  NAME_SHORT = c("All Indian Ocean")
)

SA_AREAS_CONFIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

SA_AREAS_CONFIG_ORIG = data.table(
  IOTC_CODE  = c("IRALLIO"),
  AREA_CODE  = c("A1"),
  NAME_SHORT = c("All Indian Ocean")
)

SA_AREAS_CONFIG_ORIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

postprocess_fishery = function(dataset) {
  dataset[, SF_FISHERY := fifelse(FISHERY %in% c("PSOT", "PSFS", "PSLS", "BB", "GI"), "PSPLGI", "LLOT")]

  return(dataset)
}

FISHERY_CODES = c("PSFS", "PSLS", "BB", "OTHER")

# STILL TO BE REFINED...

FISHERY_GROUP_CODES = c("PS - industrial purse seines", 
                        "BB - pole-and-lines and small seines", 
                        "OT - all other gears")

update_fishery_groups = function(dataset) {
  dataset[,                               FISHERY_GROUP := "OT - all other gears"]
  dataset[FISHERY %in% c("PSFS", "PSLS"), FISHERY_GROUP := "PS - industrial purse seines"]
  dataset[FISHERY == "BB",                FISHERY_GROUP := "BB - pole-and-lines and small seines"]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_CODES,
    levels = FISHERY_GROUP_CODES,
    ordered = TRUE
  )
  
  return(dataset)
}