### SINGLE AREA (ALL IO) 

SA_AREAS_CONFIG_SINGLE = data.table(
  IOTC_CODE  = c("IRALLIO"),
  AREA_CODE  = c("A1"),
  NAME_SHORT = c("All Indian Ocean")
)

SA_AREAS_CONFIG_SINGLE[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

SA_AREAS_CONFIG_SINGLE_ORIG = data.table(
  IOTC_CODE  = c("IRALLIO"),
  AREA_CODE  = c("A1"),
  NAME_SHORT = c("All Indian Ocean")
)

SA_AREAS_CONFIG_SINGLE_ORIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

### TWO AREAS (IO EAST + IO WEST - extended) ### STILL TO BE DONE ###

SA_AREAS_CONFIG_MULTIPLE = data.table(
  IOTC_CODE  = c("IRSKJWE", "IRSKJEA"),
  AREA_CODE  = c("A1", "A2"),
  NAME_SHORT = c("Western Indian ocean",
                 "Eastern Indian Ocean, Arabian sea and Maldives atolls")
)

SA_AREAS_CONFIG_MULTIPLE[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

SA_AREAS_CONFIG_MULTIPLE_ORIG = data.table(
  IOTC_CODE  = c("IRSKJWE", "IRSKJEA"),
  AREA_CODE  = c("A1", "A2"),
  NAME_SHORT = c("Western Indian ocean",
                 "Eastern Indian Ocean, Arabian sea and Maldives atolls")
)

SA_AREAS_CONFIG_MULTIPLE_ORIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]

## Set one of the two area configurations as the current one

SA_AREAS_CONFIG      = SA_AREAS_CONFIG_MULTIPLE
SA_AREAS_CONFIG_ORIG = SA_AREAS_CONFIG_MULTIPLE_ORIG

SA_AREA_COLORS       = SA_AREA_COLORS_MULTIPLE

##

postprocess_fishery = function(dataset) {
  dataset[, SF_FISHERY := fifelse(FISHERY %in% c("PSOT", "PSFS", "PSLS", "BB", "GL"), "PSPLGI", "LLOT")]

  return(dataset)
}

FISHERY_CODES = c("PSFS", "PSLS", "BB", "GL", "LI", "LL", "Other")

# STILL TO BE REFINED...

FISHERY_GROUP_CODES = c("PS - industrial purse seines", 
                        "BB - pole-and-lines and small seines", 
                        "GL - gillnets", 
                        "LI - lines", 
                        "LL - longlines", 
                        "Other - all other gears")

update_fishery_groups = function(dataset) {
  dataset[,                               FISHERY_GROUP := "Other - all other gears"]
  dataset[FISHERY %in% c("PSFS", "PSLS"), FISHERY_GROUP := "PS - industrial purse seines"]
  dataset[FISHERY == "BB",                FISHERY_GROUP := "BB - pole-and-lines and small seines"]
  dataset[FISHERY == "GL",                FISHERY_GROUP := "GL - gillnets"]
  dataset[FISHERY == "LI",                FISHERY_GROUP := "LI - lines"]
  dataset[FISHERY == "LL",                FISHERY_GROUP := "LL - longlines"]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_CODES,
    levels = FISHERY_GROUP_CODES,
    ordered = TRUE
  )
  
  return(dataset)
}