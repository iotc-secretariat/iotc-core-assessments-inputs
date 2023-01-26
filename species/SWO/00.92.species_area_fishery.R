SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRSWONE", "IRSWOSE", "IRSWOSW", "IRSWONW", "IRSWOOT"),
  AREA_CODE  = c("NE", "SE", "SW", "NW", "OT"),
  NAME_SHORT = c("Northeast",
                 "Southeast",
                 "Southwest",
                 "Northwest",
                 "All other")
)

SA_AREAS_CONFIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]
SA_AREAS_CONFIG_ORIG = copy(SA_AREAS_CONFIG)

postprocess_fishery = function(dataset) {
  dataset[, SF_FISHERY := fifelse(FISHERY %in% c("ALGI"), "PSPLGI", "LLOT")]
  #dataset[, FISHERY := paste0(FISHERY, str_sub(AREA, -1))] # This is only required for ALB
  
  return(dataset)
}

FISHERY_CODES = c("ALGI", "AUEL", "EUEL", "ISEL", "JPLL", "TWFL", "TWLL")

FISHERY_GROUP_NAMES = c("EL - swordfish longliners", 
                        "LL - deep-freezing longliners", 
                        "FL - fresh tuna longliners", 
                        "OT - Other gears")

update_fishery_groups = function(dataset) {
  dataset[,                                       FISHERY_GROUP := "OT - Other gears"             ]
  dataset[FISHERY %in% c("AUEL", "EUEL", "ISEL"), FISHERY_GROUP := "EL - Swordfish longliners"    ]
  dataset[FISHERY %in% c("JPLL", "TWLL"),         FISHERY_GROUP := "LL - Deep-freezing longliners"]
  dataset[FISHERY %in% c("TWFL"),                 FISHERY_GROUP := "FL - Fresh tuna longliners"   ]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_NAMES,
    levels = FISHERY_GROUP_NAMES,
    ordered = TRUE
  )
  
  return(dataset)
}