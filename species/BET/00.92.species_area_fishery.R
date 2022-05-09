SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRBET01", "IRBET02", "IRBET03", "IRBET00"),
  AREA_CODE  = c("A1", "A2", "A3", "A0"),
  NAME_SHORT = c("West",
                 "East",
                 "South",
                 "All other")
)

SA_AREAS_CONFIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

SA_AREAS_CONFIG_ORIG = data.table(
  IOTC_CODE  = c("IRBETNW", "IRBETNE", 
                 "IRBETWE", "IRBETEA",
                 "IRBETSW", "IRBETSC", "IRBETSE",
                 "IRBETZW", "IRBETZC", "IRBETZE"),
  AREA_CODE  = c("A0.1", "A0.2", 
                 "A1", "A2",
                 "A3.1", "A3.2", "A3.3",
                 "A0.3", "A0.4", "A0.5"),
  NAME_SHORT = c("Northwest", "Northeast",
                 "West", "East",
                 "Southwest", "South-central", "Southeast",
                 "South-Southwest", "South-South-central", "South-Southeast")
)

SA_AREAS_CONFIG_ORIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

postprocess_fishery = function(dataset) {
  return(dataset)
}

FISHERY_CODES = c("PSFS", "PSLS", "LL", "FL", "BB", "LINE", "OTHER")

# STILL TO BE REFINED...

FISHERY_GROUP_CODES = c("PS - industrial purse seines", 
                        "LL - industrial longlines", 
                        "BB - pole-and-lines and small seines", 
                        "LI - handlines and small longlines", 
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