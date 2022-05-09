#################################################################################################################
#### Prepares 'Standard' CAS in numbers by Year, Quarter, Month, Fleet, Gear, School type and fishing ground ####
#################################################################################################################

merge_catches_and_quarterly_CAS = function(raised_catches, quarterly_CAS) {
  dbg("Merging CA & Q CAS...")
  
  merged = 
    merge(raised_catches, #sanitize_duplicated_SF_areas(raised_catches), 
          quarterly_CAS, 
          by = c("YEAR", "QUARTER", "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", "SF_AREA"),
          #all.x = TRUE,
          allow.cartesian = TRUE)#[, FISH_COUNT := EST_NO * FISH_COUNT_PROPORTION ]#[!is.na(FIRST_CLASS_LOW)]

  merged = merged[, TOTAL_SAMPLES_COUNT := sum(SAMPLES_COUNT), by = .(YEAR, QUARTER, MONTH, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)]
  merged = merged[, FISH_COUNT := EST_NO * SAMPLES_COUNT / TOTAL_SAMPLES_COUNT ]
  #merged = merged[, UNIQUE_SF_AREAS := .N, by = .(YEAR, QUARTER, MONTH, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, SIZE_BIN)]
  #merged = merged[, FISH_COUNT := FISH_COUNT / UNIQUE_SF_AREAS]
  
  #delete_column(merged, "UNIQUE_SF_AREAS")
  
  dbg("Finished merging CA & Q CAS!")
  
  return(merged)
}
 
extract_CAS_strata = function(strata_and_catches) {
  return(
    unique(strata_and_catches[, .(YEAR, QUARTER, MONTH, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, FIRST_CLASS_LOW, SIZE_INTERVAL)])
  )
}

extract_CAS_strata_and_catches = function(merged_data) {
  return(
    unique(merged_data[, .(YEAR, QUARTER, MONTH, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, FIRST_CLASS_LOW, SIZE_INTERVAL, EST_NO, EST_MT)])
  )
}

extract_CAS_data = function(merged_data, quantity = "FISH_COUNT") {
  colnames(merged_data)[which(colnames(merged_data) == quantity)] = "QUANTITY"
  merged_data = merged_data[, .(YEAR, QUARTER, MONTH, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, SIZE_BIN, SIZE_CLASS, QUANTITY)]
  colnames(merged_data)[which(colnames(merged_data) == "QUANTITY")] = quantity

  return(merged_data)  
  #return(
  #  merged_data[, .(YEAR, QUARTER, MONTH, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, SIZE_BIN, SIZE_CLASS, FISH_COUNT)]
  #)
}

prepare_SA_CAS_YQMFG = function(merged_catches_and_quarterly_CAS, quantity = "FISH_COUNT") {
  CAS = merged_catches_and_quarterly_CAS

  CAS_strata_and_catches = extract_CAS_strata_and_catches(CAS)
  CAS_strata             = extract_CAS_strata(CAS_strata_and_catches)
  CAS_data               = extract_CAS_data(CAS, quantity)
  
  CAS_data_pivoted = dcast.data.table(
    CAS_data, 
    YEAR + QUARTER + MONTH + FLEET + GEAR_CODE + SCHOOL_TYPE_CODE + FISHING_GROUND_CODE ~ SIZE_BIN,
    value.var = quantity, 
    fun = sum, 
    fill = 0.0,
    drop = c(TRUE, FALSE)
  )
  
  CAS_OUT = merge(CAS_strata_and_catches, 
                  CAS_data_pivoted,
                  by = c("YEAR", "QUARTER", "MONTH", 
                         "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", 
                         "FISHING_GROUND_CODE"),
                  all.x = TRUE)

  setcolorder(CAS_OUT, c("YEAR", "QUARTER", "MONTH",
                         "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", 
                         "FISHING_GROUND_CODE", 
                         "FIRST_CLASS_LOW", "SIZE_INTERVAL",
                         "EST_MT", "EST_NO"))

  return(CAS_OUT)
}

prepare_SA_CAS_FIA_Q = function(merged_catches_and_quarterly_CAS, samples_by_fishery_and_quarter, quantity = "FISH_COUNT") {
  dbg("Preparing CAS FIA Q...")
  
  CAS = merged_catches_and_quarterly_CAS
  
  CAS_strata_and_catches = assign_area_and_fishery(extract_CAS_strata_and_catches(CAS))
  #CAS_strata = assign_area_and_fishery(extract_CAS_strata(CAS_strata_and_catches))
  
  dbg("prepare_SA_CAS_FIA_Q - gc() - START")
  
  gc()
  
  dbg("prepare_SA_CAS_FIA_Q - gc() - END")
  
  CAS_data = assign_area_and_fishery(extract_CAS_data(CAS, quantity))
   
  CAS_strata_and_catches = CAS_strata_and_catches[, .(EST_NO = sum(EST_NO, na.rm = TRUE), EST_MT = sum(EST_MT, na.rm = TRUE)), keyby = .(FISHERY, AREA, AREA_ORIG, YEAR, QUARTER, FIRST_CLASS_LOW, SIZE_INTERVAL)]
  #CAS_strata = unique(CAS_strata[, .(FISHERY, AREA, AREA_ORIG, YEAR, QUARTER, FIRST_CLASS_LOW, SIZE_INTERVAL)])
  
  colnames(CAS_data)[which(colnames(CAS_data) == quantity)] = "QUANTITY"
  CAS_data = CAS_data[, .(QUANTITY = sum(QUANTITY, na.rm = TRUE)), keyby = .(FISHERY, AREA, AREA_ORIG, YEAR, QUARTER, SIZE_BIN, SIZE_CLASS)]
  colnames(CAS_data)[which(colnames(CAS_data) == "QUANTITY")] = quantity

  CAS_data_pivoted = dcast.data.table(
    CAS_data, 
    FISHERY + AREA + AREA_ORIG + YEAR + QUARTER ~ SIZE_BIN,
    value.var = quantity, 
    fun = sum, 
    fill = 0.0,
    drop = c(TRUE, FALSE)
  )
  
  dbg("prepare_SA_CAS_FIA_Q - gc() - START")
  
  gc()
  
  dbg("prepare_SA_CAS_FIA_Q - gc() - END")
  
  SAMPLES = samples_by_fishery_and_quarter[, .(NUMBER_OF_SAMPLES = sum(NUMBER_OF_SAMPLES, na.rm = TRUE)), keyby = .(FISHERY, AREA, AREA_ORIG, YEAR, QUARTER)]
  
  CAS_strata_catches_samples = 
    merge(CAS_strata_and_catches, SAMPLES,
          all.x = TRUE)[is.na(NUMBER_OF_SAMPLES), NUMBER_OF_SAMPLES := 0]
  
  CAS_OUT = merge(CAS_strata_catches_samples, CAS_data_pivoted,
                  all.x = TRUE)
  
  #by = c("YEAR", "QUARTER", "MONTH", 
  #       "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", 
  #       "FISHING_GROUND_CODE"),
  
  setcolorder(CAS_OUT, c("FISHERY", "AREA", "AREA_ORIG", 
                         "YEAR", "QUARTER", 
                         "FIRST_CLASS_LOW", "SIZE_INTERVAL",
                         "NUMBER_OF_SAMPLES", "EST_NO", "EST_MT"))

  dbg("Finished preparing CAS FIA Q!")
  
  return(CAS_OUT)
}