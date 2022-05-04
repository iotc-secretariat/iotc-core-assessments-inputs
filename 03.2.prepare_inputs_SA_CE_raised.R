### Processing of CE_raised

load(input_folder(SPECIES, LOCAL_FOLDER, "CAS/grids_5_mappings.RData"))
load(input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_raised.RData"))

prepare_CE_raised = function(raw_data) {
  prepared_CE_raised = copy(raw_data)
  
  # Adds QUARTER info
  prepared_CE_raised[, QUARTER := ifelse(Month <= 3, 1, ifelse(Month <= 6, 2, ifelse(Month <=9, 3, 4)))]
  
  # Sanitizes, renames and reorders columns
  prepared_CE_raised = prepared_CE_raised[, .(YEAR = Year, QUARTER, MONTH = Month, 
                                              FLEET = str_trim(Fleet), GEAR_CODE = str_trim(Gear), SCHOOL_TYPE_CODE = str_trim(SchoolType),
                                              FISHING_GROUND_CODE = as.character(Grid),
                                              EST_NO = NCnoFish, EST_MT = NCmtFish)]

  return(prepared_CE_raised)
}

add_SF_area_to_CE_raised = function(prepared_data, sf_area_mappings) {
  extended_data = assign_SF_area(prepared_data, sf_area_mappings)
  
  # TEMPORARILY REMOVED
  # raw_data[, `:=`(EST_MT = EST_MT * PROPORTION, EST_NO = EST_NO * PROPORTION)]

  extended_data$PROPORTION = NULL
  
  extended_data = extended_data[, .(YEAR, QUARTER, MONTH, 
                                    FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, 
                                    SF_AREA, FISHING_GROUND_CODE, 
                                    EST_NO, EST_MT)]
  
  return(extended_data)    
}

sanitize_duplicated_SF_areas = function(prepared_CE_raised) {
  prepared_CE_raised = copy(prepared_CE_raised)
  prepared_CE_raised[, DUPLICATED_SF_AREAS := .N, by = .(YEAR, QUARTER, MONTH, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)]
  
  prepared_CE_raised[DUPLICATED_SF_AREAS > 1, `:=`(EST_MT = EST_MT / DUPLICATED_SF_AREAS, EST_NO = EST_NO / DUPLICATED_SF_AREAS)]
  delete_column(prepared_CE_raised, "DUPLICATED_SF_AREAS")
  
  return(prepared_CE_raised)
}

prepare_alternative_data = function(CE_raised_by_YQMFG) {
  CE_raised_by_YQMFG = sanitize_duplicated_SF_areas(CE_raised_by_YQMFG)
  
  CE_TWN_JPN_RAW = 
    query(
      DB_IOTDB(), paste0("
      SELECT 
      	YEAR, 
      	MONTH_START AS MONTH,
      	LTRIM(RTRIM(FLEET_CODE)) AS FLEET,
      	LTRIM(RTRIM(FISHERY_CODE)) AS GEAR_CODE,
      	LTRIM(RTRIM(FISHING_GROUND_CODE)) AS FISHING_GROUND_CODE,
      	SUM(CASE WHEN CATCH_UNIT_CODE = 'KG' THEN 0.001 
      	ELSE 1 END * CATCH) AS CATCH,
      	CASE WHEN CATCH_UNIT_CODE = 'KG' THEN 'MT' 
      	ELSE CATCH_UNIT_CODE END AS CATCH_UNIT_CODE,
      	SCHOOL_TYPE_CODE
      FROM 
      	V_LEGACY_CA
      WHERE 
      	SPECIES_CODE = '", SPECIES, "' AND
      	FLEET_CODE IN ('JPN', 'TWN')
      GROUP BY
      	YEAR, MONTH_START, FLEET_CODE, FISHERY_CODE, FISHING_GROUND_CODE,
      	CASE WHEN CATCH_UNIT_CODE = 'KG' THEN 'MT' ELSE CATCH_UNIT_CODE END,
      	SCHOOL_TYPE_CODE
      ORDER BY 
      	1, 2, 3, 4")
      )
  
  CE_TWN_JPN = assign_area_and_fishery(CE_TWN_JPN_RAW)
  CE_TWN_JPN[, QUARTER := ifelse(MONTH <= 3, 1, ifelse(MONTH <= 6, 2, ifelse(MONTH <= 9, 3, 4)))]
  
  CE_TWN_JPN = CE_TWN_JPN[CATCH_UNIT_CODE == "NO", CE_NO := CATCH]
  CE_TWN_JPN = CE_TWN_JPN[CATCH_UNIT_CODE == "MT", CE_MT := CATCH]
  
  CE_TWN_JPN = 
    CE_TWN_JPN[, .(CE_NO = sum(CE_NO, na.rm = TRUE), CE_MT = sum(CE_MT, na.rm = TRUE)),
               keyby = .(YEAR, QUARTER, FISHERY, AREA, AREA_ORIG, FLEET)]
  
  CE_TWN_JPN_MT_Y = CE_TWN_JPN[, .(CE_MT = sum(CE_MT, na.rm = TRUE)), keyby = .(FLEET, YEAR)][CE_MT > 0]
  NC_TWN_JPN_MT_Y = CE_raised_by_YQMFG[GEAR_CODE == "LL" & FLEET %in% c("TWN", "JPN"), .(NC_MT = sum(EST_MT)), keyby = .(YEAR, FLEET)]
  
  CE_TWN_JPN_MT_Y = merge(CE_TWN_JPN_MT_Y, NC_TWN_JPN_MT_Y,
                          by = c("FLEET", "YEAR"),
                          all.x = TRUE)[!is.na(NC_MT)][, MT_F := NC_MT / CE_MT]
  
  CE_TWN_JPN_U = merge(CE_TWN_JPN, CE_TWN_JPN_MT_Y[, .(YEAR, FLEET, NC_MT, MT_F)],
                       ny = c("YEAR", "FLEET"),
                       all.x = TRUE)[!is.na(MT_F), CE_NO_C := CE_NO * MT_F]
  
  CE_R_FIA_Q_ALT = assign_area_and_fishery(CE_raised_by_YQMFG)
  CE_R_FIA_Q_ALT = CE_R_FIA_Q_ALT[, .(CAS_NO = sum(EST_NO, na.rm = TRUE), 
                                      CAS_MT = sum(EST_MT, na.rm = TRUE)),
                                  keyby = .(FLEET, FISHERY, AREA, AREA_ORIG, YEAR, QUARTER)]
  
  CE_R_FIA_Q_ALT = merge(CE_R_FIA_Q_ALT, CE_TWN_JPN_U,
                         by = c("YEAR", "FLEET", "QUARTER", "FISHERY", "AREA", "AREA_ORIG"),
                         all.x = TRUE)
  
  CE_R_FIA_Q_ALT[!is.na(CE_NO_C), EST_NO := CE_NO_C]
  CE_R_FIA_Q_ALT[is.na(CE_NO_C) & !is.na(CE_NO), EST_NO := CE_NO]
  CE_R_FIA_Q_ALT[is.na(EST_NO), EST_NO := CAS_NO]
  CE_R_FIA_Q_ALT[!FLEET %in% c("JPN", "TWN") | !FISHERY %in% c("LL1", "LL2", "LL3", "LL4"), EST_NO := CAS_NO]
  
  CE_R_FIA_Q_ALT = CE_R_FIA_Q_ALT[, .(EST_NO = sum(EST_NO, na.rm = TRUE), EST_MT = sum(CAS_MT, na.rm = TRUE)),
                                  keyby = .(FISHERY, AREA, AREA_ORIG, YEAR, QUARTER)]
  
  ### MIGHT BE NECESSARY TO SCALE UP / DOWN THE SF FOR ALL TWN LL FISHERIES BY THE COEFFICIENT IDENTIFIED FOR THEIR AREA
  
  setcolorder(CE_R_FIA_Q_ALT, c("FISHERY", "AREA", "AREA_ORIG", "YEAR", "QUARTER", "EST_MT", "EST_NO"))
  
  return(CE_R_FIA_Q_ALT)
}

# Raised CE by year, quarter, month, fleet, gear and fishing ground, with SF area attached 
# (might introduce duplicate records due to 5x5 grids belonging to multiple SF areas at a time
CE_R_YQMFG = 
  add_SF_area_to_CE_raised(
    prepare_CE_raised(
      CE_raised
    ), 
    GRIDS_5_PS_LL
  )

# Raised CE by fishery, area, quarter, without SF area attached (not necessary)
CE_R_FIA_Q = 
  assign_area_and_fishery(
    prepare_CE_raised(
      CE_raised
    )
  )[, .(EST_MT = sum(EST_MT, na.rm = TRUE), EST_NO = sum(EST_NO, na.rm = TRUE)),
        keyby = .(FISHERY, AREA, AREA_ORIG, YEAR , QUARTER)]

CE_R_FIA_Q_FL = 
  assign_area_and_fishery(
    prepare_CE_raised(
      CE_raised
    )
  )[, .(EST_MT = sum(EST_MT, na.rm = TRUE), EST_NO = sum(EST_NO, na.rm = TRUE)),
    keyby = .(FISHERY, AREA, AREA_ORIG, YEAR , QUARTER, FLEET)]

CE_R_FIA_Q_ALT = prepare_alternative_data(CE_R_YQMFG)