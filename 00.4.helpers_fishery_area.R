### AREA + FISHERY FUNCTIONS

initialize_species_specific_fishery_mappings = function(species) {
  species_specific_mappings = fread(species_folder(SPECIES, SA_FISHERY_AREA_MAPPINGS_FILE), na.strings = "")
  
  fishery_mappings = species_specific_mappings[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHERY)]
  
  return(
    unique(
      fishery_mappings
    )
  )
}  

initialize_species_specific_area_mappings = function(species) {
  species_specific_mappings = fread(species_folder(SPECIES, SA_FISHERY_AREA_MAPPINGS_FILE), na.strings = "")
 
  area_mappings = 
    melt.data.table(
      species_specific_mappings,
      variable.name = "AREA_CODE",
      value.name = "AREA_DEST_CODE",
      id.vars = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "FISHERY")
    )[, .(FLEET, GEAR_CODE, FISHERY, SCHOOL_TYPE_CODE, AREA_CODE, AREA_DEST_CODE)][!is.na(AREA_DEST_CODE)]
  
  area_mappings$AREA_CODE = factor(
    area_mappings$AREA_CODE,
    levels = SA_AREAS_CONFIG_ORIG$AREA_CODE,
    labels = SA_AREAS_CONFIG_ORIG$AREA_CODE,
    ordered = TRUE
  )
  
  area_mappings$AREA_DEST_CODE = factor(
    area_mappings$AREA_DEST_CODE,
    levels = SA_AREAS_CONFIG$AREA_CODE,
    labels = SA_AREAS_CONFIG$AREA_CODE,
    ordered = TRUE
  )
  
  area_mappings$FISHERY = NULL
  
  return(area_mappings)
}

retrieve_areas = function(sa_areas_configuration = SA_AREAS_CONFIG) {
  areas = iotc.core.gis.wkt::fishing_grounds_data(fishing_ground_codes = sa_areas_configuration$IOTC_CODE, connection = IOTC)
  areas = merge(areas, sa_areas_configuration,
                by.x = "CODE",
                by.y = "IOTC_CODE",
                all.x = TRUE)
  
  areas$CODE = factor(
    areas$CODE,
    levels = sa_areas_configuration$IOTC_CODE,
    labels = sa_areas_configuration$AREA_NAME,
    ordered = TRUE
  )
  
  areas = areas[order(CODE)]
  
  return(areas)
}

grid_to_area_mappings = function(sa_areas_configuration = SA_AREAS_CONFIG, grid_type_code = grid_5x5) {
  return(
    iotc.core.gis.cwp.IO::grid_intersections_by_source_grid_type(target_grid_codes = sa_areas_configuration$IOTC_CODE, 
                                                                 source_grid_type_code = grid_type_code)
  )
}

all_grid_to_area_mappings = function(sa_areas_configuration = SA_AREAS_CONFIG) {
  result =
    rbind(
      grid_to_area_mappings(sa_areas_configuration, grid_1x1),
      grid_to_area_mappings(sa_areas_configuration, grid_5x5)
    )
  
  result = 
    merge(
      result,
      sa_areas_configuration,
      by.x = "TARGET_FISHING_GROUND_CODE",
      by.y = "IOTC_CODE",
      all.x = TRUE
    )
  
  colnames(result)[which(colnames(result) == "SOURCE_FISHING_GROUND_CODE")] = "FISHING_GROUND_CODE"
  
  setindex(result, FISHING_GROUND_CODE)
  
  return(result)
}

assign_area_and_fishery = function(dataset) {
  dataset_FI = 
    assign_fishery(
      assign_area(
        copy(
          dataset
        )
      )
    )
  
  setcolorder(dataset_FI, c("FISHERY", "AREA", "AREA_ORIG"))
  
  return(dataset_FI)
}

SPECIES_SPECIFIC_FISHERY_MAPPINGS = initialize_species_specific_fishery_mappings(SPECIES)
SPECIES_SPECIFIC_AREA_MAPPINGS    = initialize_species_specific_area_mappings(SPECIES)

FG_TO_SA_AREAS_ORIG = all_grid_to_area_mappings(SA_AREAS_CONFIG_ORIG)

SA_AREAS      = retrieve_areas(SA_AREAS_CONFIG)
SA_AREAS_ORIG = retrieve_areas(SA_AREAS_CONFIG_ORIG)

assign_area = function(dataset, 
                       area_config                    = SA_AREAS_CONFIG,
                       original_area_config           = SA_AREAS_CONFIG_ORIG,
                       grid_to_area_mappings          = FG_TO_SA_AREAS_ORIG,
                       species_specific_area_mappings = SPECIES_SPECIFIC_AREA_MAPPINGS) {
  l_info(paste0("Assigning area to dataset of ", nrow(dataset), " rows..."))
  
  runGC()
  
  setindex(dataset, FISHING_GROUND_CODE)
  
  dataset = merge(dataset, grid_to_area_mappings,
                  by = "FISHING_GROUND_CODE",
                  all.x = TRUE,
                  allow.cartesian = TRUE)
  
  unmapped_grid_codes = unique(dataset[is.na(TARGET_FISHING_GROUND_CODE)]$FISHING_GROUND_CODE)
  
  if(length(unmapped_grid_codes) >= 1) {
    l_warn(paste0(length(unmapped_grid_codes), " grids have not been assigned to any SA area..."))
    l_warn(unmapped_grid_codes)
    
    dataset = dataset[!is.na(TARGET_FISHING_GROUND_CODE)]
  }
  
  dataset$AREA_ORIG = dataset$TARGET_FISHING_GROUND_CODE
  
  dataset = merge(dataset, species_specific_area_mappings,
                  by = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "AREA_CODE"),
                  all.x = TRUE)
  
  dataset[, AREA_ORIG := AREA_CODE]
  
  colnames(dataset)[which(colnames(dataset) == "AREA_DEST_CODE")] = "AREA"
  
  unmapped_areas = unique(dataset[is.na(AREA), .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, AREA_CODE)])
  
  if(nrow(unmapped_areas) >= 1) {
    l_warn(paste0(nrow(unmapped_areas), " records have not been mapped to any area..."))
    print(unmapped_areas)
    
    dataset = dataset[!is.na(AREA)]
  }
  
  dataset$AREA = factor(
    dataset$AREA,
    levels = area_config$AREA_CODE,
    #labels = area_config$AREA_NAME,
    ordered = TRUE
  )
  
  dataset$AREA_ORIG = factor(
    dataset$AREA_ORIG,
    levels = original_area_config$AREA_CODE,
    #labels = original_area_config$AREA_NAME,
    labels = original_area_config$AREA_CODE,
    ordered = TRUE
  )
  
  delete_column(dataset, c("PROPORTION", "NAME_SHORT", "AREA_NAME", "AREA_CODE", "TARGET_FISHING_GROUND_CODE"))
  
  runGC()
  
  l_info(paste0("Assigned area to dataset of ", nrow(dataset), " rows!"))
  
  return(dataset)
}

assign_fishery = function(dataset, 
                          species_specific_fishery_mappings = SPECIES_SPECIFIC_FISHERY_MAPPINGS,
                          fishery_codes = FISHERY_CODES) {
  l_info(paste0("Assigning fishery to dataset of ", nrow(dataset), " rows..."))
  
  runGC()
  
  dataset = merge(dataset, species_specific_fishery_mappings, 
                  by = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE"),
                  all.x = TRUE)
  
  missing_fishery = dataset[is.na(FISHERY)]
  missing_fishery = unique(missing_fishery[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, YEAR)])
  num_missing_fishery = nrow(missing_fishery)
  
  if(num_missing_fishery > 0) {
    l_error(paste0(num_missing_fishery, " fleet / gear / school type mappings are missing"))
    l_error(unique(missing_fishery[, .(FLEET, GEAR_CODE, SCHOOL_TYPE_CODE)]))
  }
  
  dataset = postprocess_fishery(dataset)
  
  dataset$FISHERY = factor(
    dataset$FISHERY,
    levels = fishery_codes,
    labels = fishery_codes,
    ordered = TRUE
  )
  
  runGC()
  
  l_info(paste0("Assigned fishery to dataset of ", nrow(dataset), " rows!"))
  
  return (dataset)
}