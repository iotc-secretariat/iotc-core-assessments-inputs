### Processing of CE_all

load(input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_all.RData"))

prepare_CE_all = function(raw_data) {
  raw_data = copy(raw_data)
  
  # Adds QUARTER info
  raw_data[, QUARTER := ifelse(MonthStart <= 3, 1, ifelse(MonthStart <= 6, 2, ifelse(MonthStart <=9, 3, 4)))]
  
  # Sanitizes, renames and reorders columns
  raw_data = raw_data[, .(YEAR = Year, QUARTER, MONTH = MonthStart, 
                          FLEET = str_trim(Fleet), GEAR_CODE = str_trim(Gear), SCHOOL_TYPE_CODE = str_trim(SchoolType),
                          SF_AREA = as.character(SFArea), FISHING_GROUND_CODE = as.character(Grid),
                          NC_NO = NCnoFish, NC_MT = NCmtFish,
                          SF_NO = SFnoFish, SF_MT = SFmtFish)]

  return(raw_data)    
}