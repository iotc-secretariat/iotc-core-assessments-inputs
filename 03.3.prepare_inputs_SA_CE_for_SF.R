### Processing of CE_for_SF

load(input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_for_SF.RData"))

prepare_CE_for_SF = function(raw_data) {
  raw_data = copy(raw_data)
    delete_column(raw_data, c("TI", "Latitude", "Longitude",
                              "NCid", "Species", "IOTC_Area", 
                              "Substitution", "Step", 
                              "FleetA", "GearA", "UseStrata", 
                              "SFnoFish", "SFmtFish",
                              "CEnoFish", "CEmtFish",
                              "SUBSnoFish", "SUBSmtFish", "SizeIntAgg"))
  
  # Sanitizes columns
  raw_data[, `:=`(Fleet = str_trim(Fleet), 
                  Gear = str_trim(Gear), 
                  SchoolType = str_trim(SchoolType), 
                  Grid = as.character(Grid))]
  
  raw_data = change_column_name(raw_data, "Year", "YEAR")
  raw_data = change_column_name(raw_data, "Quarter", "QUARTER")
  raw_data = change_column_name(raw_data, "Fleet", "FLEET")
  raw_data = change_column_name(raw_data, "Gear", "GEAR_CODE")
  raw_data = change_column_name(raw_data, "SchoolType", "SCHOOL_TYPE_CODE")
  raw_data = change_column_name(raw_data, "Grid", "SF_AREA")
  raw_data = change_column_name(raw_data, "FirstClassLow", "FIRST_CLASS_LOW")
  raw_data = change_column_name(raw_data, "SizeInterval", "SIZE_INTERVAL")
  #raw_data = change_column_name(raw_data, "CEnoFish", "EST_NO")
  #raw_data = change_column_name(raw_data, "CEmtFish", "EST_MT")
  
  setcolorder(raw_data, c("YEAR", "QUARTER", "FLEET", 
                          "GEAR_CODE", "SCHOOL_TYPE_CODE", "SF_AREA",
                          #"EST_NO", "EST_MT",
                          "FIRST_CLASS_LOW", "SIZE_INTERVAL"))
  
  return(raw_data)
}

unpivot_CE_for_SF = function(pivoted) {
  unpivoted = 
    melt.data.table(
      data = pivoted, 
      value.name = "FISH_COUNT", 
      variable.name = "SIZE_BIN",
      id.vars = c("YEAR", "QUARTER", 
                  "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", 
                  "SF_AREA", 
                  #"EST_NO", "EST_MT",
                  "FIRST_CLASS_LOW", "SIZE_INTERVAL")
    )
  
  size_bins = sort(unique(unpivoted$SIZE_BIN))
  
  unpivoted$SIZE_BIN = factor(
    unpivoted$SIZE_BIN,
    levels = size_bins,
    labels = size_bins,
    ordered = TRUE
  )
  
  return(
    # Not strictly needed: size class is required only when in need of converting initial class or size interval, and therefore
    # can be calculated on the fly...
    #size_class_from_size_bin( 
      unpivoted[FISH_COUNT > 0]
    #)
  )
}

pivot_CE_for_SF = function(unpivoted) {
  pivoted = 
    dcast.data.table(
      unpivoted[FISH_COUNT > 0], YEAR + QUARTER + FLEET + GEAR_CODE + SCHOOL_TYPE_CODE + SF_AREA + FIRST_CLASS_LOW + SIZE_INTERVAL ~ SIZE_BIN, 
      value.var = "FISH_COUNT", 
      fun = sum, 
      fill = NA,
      drop = c(TRUE, FALSE) 
    )
  
  return(pivoted)
}

change_bins = function(unpivoted, first_class_low, size_interval, max_bins) {
  return(
    size_class_from_size_bin(
      size_bin_from_size_class(
        size_class_from_size_bin(
          unpivoted
        ),
        first_class_low,
        size_interval,
        max_bins
      )
    )
  )
}

normalize_CE_for_SF = function(unpivoted) {
  unpivoted = copy(unpivoted)

  # NECESSARY, OTHERWISE THERE MIGHT BE RESULTS SUCH AS:
  
  # (...)
  # 52: 1955       4   TWN        LL             UNCL 2210080              30             1     T104        133                 0.015
  # 53: 1955       4   TWN        LL             UNCL 2210080              30             1     T110        139                 0.010
  # 54: 1955       4   TWN        LL             UNCL 2210080              30             1     T110        139                 0.010
  
  # Where the same size class / size bin is counted twice
  
  unpivoted = unpivoted[, .(SAMPLES_COUNT = sum(FISH_COUNT, na.rm = TRUE)),
                            keyby = .(YEAR, QUARTER, 
                                      FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, 
                                      SF_AREA, 
                                      FIRST_CLASS_LOW, SIZE_INTERVAL, 
                                      SIZE_BIN, SIZE_CLASS)]
  
  # Keeping the total samples count by stratum (Y / Q / F / G / S / SF_A) instead of
  # normalizing it to 1, otherwise there might be problems when putting together the
  # CAS for those 5x5 PS grids that might be under multiple SF areas...
  
  # Previously:
  #
  #unpivoted[, TOTAL_FISH_COUNT := sum(FISH_COUNT, na.rm = TRUE), by = .(YEAR, QUARTER,
  #                                                                      FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, 
  #                                                                      SF_AREA, 
  #                                                                      FIRST_CLASS_LOW, SIZE_INTERVAL)]
  #unpivoted[TOTAL_FISH_COUNT > 0, FISH_COUNT_PROPORTION := FISH_COUNT / TOTAL_FISH_COUNT]
  #
  #delete_column(unpivoted, c("FISH_COUNT", "TOTAL_FISH_COUNT"))
  
  return(unpivoted)
}

# This is only available with a temporal resolution of a quarter
CE_SF_YQFG = 
  normalize_CE_for_SF(
    change_bins(
      unpivot_CE_for_SF(
        prepare_CE_for_SF(
          CE_for_SF
        )
      ),
      DEFAULT_FIRST_CLASS_LOW, # This constant is species specific, and included with the species configuration
      DEFAULT_SIZE_INTERVAL,   # This constant is species specific, and included with the species configuration
      DEFAULT_NUM_SIZE_BINS    # This constant is species specific, and included with the species configuration
    )
  )