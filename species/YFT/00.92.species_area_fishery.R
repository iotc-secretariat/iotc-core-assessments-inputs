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

YFT_FISHERIES_AREA_MAPPINGS = fread(species_folder(SPECIES, "YFT_FISHERIES.csv"), na.strings = "")

YFT_FISHERIES_MAPPINGS = YFT_FISHERIES_AREA_MAPPINGS[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHERY)]

YFT_AREA_MAPPINGS = 
  melt.data.table(
    YFT_FISHERIES_AREA_MAPPINGS,
    variable.name = "AREA_SRC",
    value.name = "AREA_DEST",
    id.vars = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "FISHERY")
  )[, .(FLEET, GEAR_CODE, FISHERY, SCHOOL_TYPE_CODE, AREA_SRC, AREA_DEST)][!is.na(AREA_DEST)]

YFT_AREA_MAPPINGS$FISHERY = NULL

SA_AREAS = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = SA_AREAS_CONFIG$IOTC_CODE, connection = IOTC)
SA_AREAS = merge(SA_AREAS, SA_AREAS_CONFIG,
                 by.x = "CODE",
                 by.y = "IOTC_CODE",
                 all.x = TRUE)

SA_AREAS$CODE = factor(
  SA_AREAS$CODE,
  levels = SA_AREAS_CONFIG$IOTC_CODE,
  labels = SA_AREAS_CONFIG$AREA_NAME,
  ordered = TRUE
)

SA_AREAS = SA_AREAS[order(CODE)]

# Area names
AREA_NAMES = SA_AREAS_CONFIG$NAME_SHORT

SA_AREA_ORIG_CODES = SA_AREAS$CODE

SA_AREAS_ORIG = SA_AREAS

# Area names
AREA_ORIG_NAMES = AREA_NAMES
  
FG_5_TO_SA_AREAS_ORIG = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREAS_CONFIG$IOTC_CODE, 
                                                                                     source_grid_type_code = grid_5x5)

FG_1_TO_SA_AREAS_ORIG = iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = SA_AREAS_CONFIG$IOTC_CODE, 
                                                                                     source_grid_type_code = grid_1x1)

FG_TO_SA_AREAS_ORIG = rbind(FG_5_TO_SA_AREAS_ORIG, FG_1_TO_SA_AREAS_ORIG)

FG_TO_SA_AREAS_ORIG = merge(FG_TO_SA_AREAS_ORIG, SA_AREAS_CONFIG,
                            by.x = "TARGET_FISHING_GROUND_CODE",
                            by.y = "IOTC_CODE",
                            all.x = TRUE)

setindex(FG_TO_SA_AREAS_ORIG, SOURCE_FISHING_GROUND_CODE)

assign_area = function(dataset) {
  setindex(dataset, YEAR)
  
  dbg(paste0("Assigning area to dataset of ", nrow(dataset), " rows..."))
  
  ym = min(dataset$YEAR)
  yM = max(dataset$YEAR) 
  
  D = NULL
  
  for(y in ym:yM) {
    dbg(paste0("Processing year ", y))
    
    d = merge(dataset[YEAR == y], FG_TO_SA_AREAS_ORIG,
              by.x = "FISHING_GROUND_CODE",
              by.y = "SOURCE_FISHING_GROUND_CODE",
              all.x = TRUE,
              allow.cartesian = TRUE)
    
    if(is.null(D)) 
      D = d
    else
      D = rbind(D, d)
    
    dbg("GC() - START")
    gc()
    dbg("GC() - END")
  }
  
  dataset = D
  
  unmapped_grid_codes = unique(dataset[is.na(TARGET_FISHING_GROUND_CODE)]$FISHING_GROUND_CODE)
  
  if(length(unmapped_grid_codes) >= 1) {
    print(paste0(length(unmapped_grid_codes), " grids have not been assigned to any SA area..."))
    print(unmapped_grid_codes)
    
    dataset = dataset[!is.na(TARGET_FISHING_GROUND_CODE)]
  }
  
  dataset$AREA_ORIG = dataset$TARGET_FISHING_GROUND_CODE
  
  
  dataset = merge(dataset, YFT_AREA_MAPPINGS,
                  by.x = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "AREA_CODE"),
                  by.y = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "AREA_SRC"),
                  all.x = TRUE)
   
  colnames(dataset)[which(colnames(dataset) == "AREA_DEST")] = "AREA"

  dataset$AREA = factor(
    dataset$AREA,
    levels = SA_AREAS_CONFIG$AREA_CODE,
    labels = SA_AREAS_CONFIG$AREA_NAME,
    ordered = TRUE
  )
  
  dataset$AREA_ORIG = factor(
    dataset$AREA_ORIG,
    levels = SA_AREAS_CONFIG$IOTC_CODE,
    labels = SA_AREAS_CONFIG$AREA_NAME,
    ordered = TRUE
  )
  
  delete_column(dataset, c("PROPORTION", "NAME_SHORT", "AREA_NAME"))
  
  dbg(paste0("Assigned area to dataset of ", nrow(dataset), " rows!"))
  
  return(dataset)
}

FISHERY_CODES = c("FS", "LS", "LL", "LF", "BB", "GI", "HD", "TR", "OT")

assign_fishery = function(dataset) {
  dbg(paste0("Assigning fishery to dataset of ", nrow(dataset), " rows..."))
  
  dataset = merge(dataset, YFT_FISHERIES_MAPPINGS, 
                  by = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE"),
                  all.x = TRUE)
  
  missing_fishery = dataset[is.na(FISHERY)]
  missing_fishery = unique(missing_fishery[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, YEAR)])
  num_missing_fishery = nrow(missing_fishery)
  
  if(num_missing_fishery > 0) {
    print(paste0(num_missing_fishery, " fleet / gear / school type mappings are missing"))
    print(unique(missing_fishery[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE)]))
  }
  
  dataset$FISHERY = factor(
    dataset$FISHERY,
    levels = FISHERY_CODES,
    labels = FISHERY_CODES,
    ordered = TRUE
  )

  dbg(paste0("Assigned fishery to dataset of ", nrow(dataset), " rows!"))
  
  return (dataset)
}

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
  dataset[FISHERY == "BB",                FISHERY_GROUP := "BB - pole-and-lines and small seines"]
  dataset[FISHERY == "GI",                FISHERY_GROUP := "GI - gillnets"]
  dataset[FISHERY %in% c("HD", "TR"),     FISHERY_GROUP := "LI - handlines and small longlines"]
  
  dataset$FISHERY_GROUP = factor(
    dataset$FISHERY_GROUP,
    labels = FISHERY_GROUP_CODES,
    levels = FISHERY_GROUP_CODES,
    ordered = TRUE
  )
  
  return(dataset)
}