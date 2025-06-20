#################################################################################################################
#### Prepares 'Standard' CAA in numbers by Year, Quarter, Month, Fleet, Gear, School type and fishing ground ####
#################################################################################################################

prepare_SA_CAA_NO_YQMFG = function(merged_catches_and_quarterly_CAS, age_length_keys, quantity = "FISH_COUNT") {
  CAS = merged_catches_and_quarterly_CAS

  CAS_strata_and_catches = extract_CAS_strata_and_catches(CAS)
  CAS_strata             = extract_CAS_strata(CAS_strata_and_catches)
  CAS_data               = extract_CAS_data(CAS, quantity)
  
  CAS_data$SIZE_CLASS_ALT = CAS_data$SIZE_CLASS
  
  CAA_data = 
    foverlaps(CAS_data, age_length_keys,
              by.x = c("SIZE_CLASS", "SIZE_CLASS_ALT"),
              by.y = c("LengthFrom", "LengthTo"),
              type = "within")
  
  NA_AGE = CAA_data[is.na(Age)]
  
  if(nrow(NA_AGE) > 0) {
    l_warn(paste0(nrow(NA_AGE), " records have been assigned a 'NA' age, for a total of ", round(sum(NA_AGE$FISH_COUNT)), " individuals: removing these unwanted records..."))
    
    CAA_data = CAA_data[!is.na(Age)]
  }
  
  colnames(CAA_data)[which(colnames(CAA_data) == quantity)] = "QUANTITY"
  
  CAA_data = CAA_data[, .(YEAR, QUARTER, MONTH, 
                          FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, 
                          QUANTITY,
                          METHOD = Method, AGE = Age, PROPORTION = Proportion)]
  CAA_data[, QUANTITY := QUANTITY * PROPORTION]
  
  colnames(CAA_data)[which(colnames(CAA_data) == "QUANTITY")] = quantity
  
  CAA_data[, AGE := paste0("A", str_sub(paste0("00", AGE), start = -2))]
  
  CAA_data_pivoted = dcast.data.table(
    CAA_data, 
    YEAR + QUARTER + MONTH + FLEET + GEAR_CODE + SCHOOL_TYPE_CODE + FISHING_GROUND_CODE + METHOD ~ AGE,
    value.var = quantity, 
    fun = sum, 
    fill = 0.0,
    drop = c(TRUE, FALSE)
  )
  
  CAA_OUT = merge(CAS_strata_and_catches, 
                  CAA_data_pivoted,
                  by = c("YEAR", "QUARTER", "MONTH", 
                         "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", 
                         "FISHING_GROUND_CODE"),
                  all.x = TRUE)
  
  delete_column(CAA_OUT, c("FIRST_CLASS_LOW",
                           "SIZE_INTERVAL"))

  setcolorder(CAA_OUT, c("YEAR", "QUARTER", "MONTH",
                         "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", 
                         "FISHING_GROUND_CODE", 
                         "EST_NO", "EST_MT", "METHOD"))

  return(CAA_OUT)
}

prepare_SA_CAA_NO_FIA_Q = function(merged_catches_and_quarterly_CAS, age_length_keys, quantity = "FISH_COUNT") {
  l_info("prepare_SA_CAA_NO_FIA_Q - BEGIN")
  
  CAS = merged_catches_and_quarterly_CAS
  
  CAS_strata_and_catches = assign_area_and_fishery(extract_CAS_strata_and_catches(CAS))
  CAS_data               = assign_area_and_fishery(extract_CAS_data(CAS, quantity))
  CAS_strata_and_catches = CAS_strata_and_catches[, .(EST_NO = sum(EST_NO, na.rm = TRUE), EST_MT = sum(EST_MT, na.rm = TRUE)), keyby = .(FISHERY, AREA, AREA_ORIG, YEAR, QUARTER, FIRST_CLASS_LOW, SIZE_INTERVAL)]
  
  
  colnames(CAS_data)[which(colnames(CAS_data) == quantity)] = "QUANTITY"
  CAS_data               = CAS_data[, .(QUANTITY = sum(QUANTITY, na.rm = TRUE)), keyby = .(FISHERY, AREA, AREA_ORIG, YEAR, QUARTER, SIZE_BIN, SIZE_CLASS)]
  colnames(CAS_data)[which(colnames(CAS_data) == "QUANTITY")] = quantity
  
  CAS_data$SIZE_CLASS_ALT = CAS_data$SIZE_CLASS

  runGC()

  CAA_data = 
    foverlaps(CAS_data, age_length_keys,
              by.x = c("SIZE_CLASS", "SIZE_CLASS_ALT"),
              by.y = c("LengthFrom", "LengthTo"),
              type = "within")
  
  NA_AGE = CAA_data[is.na(Age)]
  
  if(nrow(NA_AGE) > 0) {
    l_warn(paste0(nrow(NA_AGE), " records have been assigned a 'NA' age, for a total of ", round(sum(NA_AGE$FISH_COUNT)), " individuals: removing these unwanted records..."))
    
    CAA_data = CAA_data[!is.na(Age)]
  }
  
  colnames(CAA_data)[which(colnames(CAA_data) == quantity)] = "QUANTITY"
  
  CAA_data = CAA_data[, .(FISHERY, AREA, AREA_ORIG, 
                          YEAR, QUARTER, 
                          QUANTITY,
                          METHOD = Method, AGE = Age, PROPORTION = Proportion)]
  CAA_data[, QUANTITY := QUANTITY * PROPORTION]
  #CAA_data[, FISH_COUNT := FISH_COUNT * PROPORTION]
  
  colnames(CAA_data)[which(colnames(CAA_data) == "QUANTITY")] = quantity
  
  CAA_data[, AGE := paste0("A", str_sub(paste0("00", AGE), start = -2))]

  l_info("Pivoting CAA data - START")
  
  CAA_data_pivoted = dcast.data.table(
    CAA_data, 
    FISHERY + AREA + AREA_ORIG + YEAR + QUARTER + METHOD ~ AGE,
    value.var = quantity, 
    fun = sum, 
    fill = 0.0,
    drop = c(TRUE, FALSE)
  )

  l_info("Pivoting CAA data - END")

  l_info("Merging CAS strata & CAA data - START")
  
  CAA_OUT = merge(CAS_strata_and_catches, 
                  CAA_data_pivoted,
                  by = c("FISHERY", "AREA", "AREA_ORIG",
                         "YEAR", "QUARTER"),
                  all.x = TRUE)

  l_info("Merging CAS strata & CAA data - END")
  
  delete_column(CAA_OUT, c("FIRST_CLASS_LOW",
                           "SIZE_INTERVAL"))
  
  setcolorder(CAA_OUT, c("FISHERY", "AREA", "AREA_ORIG", 
                         "YEAR", "QUARTER", 
                         "EST_NO", "EST_MT", "METHOD"))
  
  l_info("prepare_SA_CAA_NO_FIA_Q - END")
  
  return(CAA_OUT)
}
