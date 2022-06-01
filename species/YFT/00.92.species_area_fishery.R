SA_AREAS_CONFIG = data.table(
  IOTC_CODE  = c("IRYFT1A", "IRYFT1B", "IRYFT02", "IRYFT03", "IRYFT04", "IRYFT00"),
  AREA_CODE  = c("R1a", "R1b", "R2", "R3", "R4", "R0"),
  NAME_SHORT = c("Arabian sea",
                 "Western Indian ocean (tropical)",
                 "Mozambique channel",
                 "Southern Indian ocean",
                 "Eastern Indian ocean (tropical)",
                 "All other areas")
)

SA_AREAS_CONFIG[, AREA_NAME := paste0(AREA_CODE, " - ", NAME_SHORT)]
SA_AREAS_CONFIG_ORIG = copy(SA_AREAS_CONFIG)

postprocess_fishery = function(dataset) {
  dataset[, SF_FISHERY := fifelse(FISHERY %in% c("FS", "LS", "BB", "GI"), "PSPLGI", "LLOT")]
  
  return(dataset)
}

FISHERY_CODES = c("FS", "LS", "LL", "LF", "BB", "GI", "HD", "TR", "OT")

# STILL TO BE REFINED...

FISHERY_GROUP_CODES = c("PS - industrial purse seines", 
                        "LL - industrial longlines", 
                        "BB - pole-and-lines", 
                        "GI - gillnets", 
                        "LI - handlines and troll lines", 
                        "OT - other gears")

update_fishery_groups = function(dataset) {
  dataset[,                               FISHERY_GROUP := "OT - other gears"]
  dataset[FISHERY %in% c("FS", "LS"),     FISHERY_GROUP := "PS - industrial purse seines"]
  dataset[FISHERY %in% c("LL", "LF"),     FISHERY_GROUP := "LL - industrial longlines"]
  dataset[FISHERY == "BB",                FISHERY_GROUP := "BB - pole-and-lines"]
  dataset[FISHERY == "GI",                FISHERY_GROUP := "GI - gillnets"]
  dataset[FISHERY %in% c("HD", "TR"),     FISHERY_GROUP := "LI - handlines and troll lines"]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_CODES,
    levels = FISHERY_GROUP_CODES,
    ordered = TRUE
  )
  
  return(dataset)
}