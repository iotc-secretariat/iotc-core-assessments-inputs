### Processing of VW_SF_<species code>

load(input_folder(SPECIES, LOCAL_FOLDER, "IOTDB/SF_FL_WITH_SAMPLES.RData"))
load(input_folder(SPECIES, LOCAL_FOLDER, "CAS/SF_strata_to_delete.RData"))

### Helper functions 

sanitize_SF = function(raw_data, scale_by_samples = FALSE, exclude_larger_grids = TRUE) {
  raw_data = copy(raw_data)
  
  if(scale_by_samples) { 
    raw_data = raw_data[SAMPLE_SIZE > 0]
    #raw_data[SAMPLE_SIZE > 0 & SAMPLE_SIZE > TnoFish, SAMPLE_SIZE := TnoFish]
  }
  
  # Sanitizing the Fleet / Gear / SchoolType and Grid columns
  raw_data[, `:=`(Fleet = str_trim(Fleet), 
                  Gear = str_trim(Gear),
                  SchoolType = str_trim(SchoolType),
                  Grid = as.character(Grid))]

  # Adding QUARTER information (based on month start) and reorders columns
  raw_data[, QUARTER := ifelse(MonthStart <= 3, 1, ifelse(MonthStart <= 6, 2, ifelse(MonthStart <= 9, 3, 4)))]
  
  # The original process excluded all S-F that came as 10x20 or larger grids
  raw_data = raw_data[!str_sub(Grid, 1, 1) %in% c("1", "9")]
  
  # In reality, we shall keep the 10x20 grids and assign "correct" (irregular) grid codes to the IO areas
  # for a later disaggregation into smaller (5x5) areas
  if(!exclude_larger_grids) {
    raw_data[Grid == "9000000", Grid := "IRALLIO"]
    raw_data[Grid == "9000020", Grid := "IRWESIO"]
    raw_data[Grid == "9000080", Grid := "IREASIO"]
    raw_data[Grid == "9100020", Grid := "IRNWEIO"]
    raw_data[Grid == "9100080", Grid := "IRNEAIO"]
    raw_data[Grid == "9200020", Grid := "IRSWEIO"]
    raw_data[Grid == "9200080", Grid := "IRSEAIO"]
  }
  
  raw_data = change_column_name(raw_data, "Year",          "YEAR")
  raw_data = change_column_name(raw_data, "MonthStart",    "MONTH_START")
  raw_data = change_column_name(raw_data, "MonthEnd",      "MONTH_END")
  raw_data = change_column_name(raw_data, "Fleet",         "FLEET")
  raw_data = change_column_name(raw_data, "Gear",          "GEAR_CODE")
  raw_data = change_column_name(raw_data, "SchoolType",    "SCHOOL_TYPE_CODE")
  raw_data = change_column_name(raw_data, "Grid",          "FISHING_GROUND_CODE")
  raw_data = change_column_name(raw_data, "FirstClassLow", "FIRST_CLASS_LOW")
  raw_data = change_column_name(raw_data, "SizeInterval",  "SIZE_INTERVAL")
  raw_data = change_column_name(raw_data, "SAMPLE_SIZE",   "NUMBER_OF_SAMPLES")
  raw_data = change_column_name(raw_data, "TnoFish",       "TOT_FISH_COUNT")
  
  raw_data = delete_column(raw_data, "Species")
  raw_data = delete_column(raw_data, "MeasureType")
  raw_data = delete_column(raw_data, "TkgFish")

  setcolorder(raw_data, c("YEAR", "QUARTER", "MONTH_START", "MONTH_END",
                          "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE",
                          "FISHING_GROUND_CODE",
                          "FIRST_CLASS_LOW", "SIZE_INTERVAL",
                          "NUMBER_OF_SAMPLES", "TOT_FISH_COUNT"))
  
  #raw_data[, GEAR_TYPE := ifelse(Gear == "PS", "PS", "LL")] 
  
  return(raw_data)
}

remove_SF_strata = function(raw_data, strata_to_delete) {
  NUMBER_OF_SAMPLES_orig = sum(raw_data$NUMBER_OF_SAMPLES)
  
  l_info("List of S-F strata to be deleted:")
  
  if(SPECIES == "SKJ") { # Temporarily only applies to SKJ
    print(strata_to_delete[order(+FLEET, +GEAR_CODE, +SCHOOL_TYPE_CODE, -YEAR)])
  } else {
    print(strata_to_delete[order(+FLEET, +GEAR_CODE, -YEAR)])
  }
  
  # Diagnostics after samples deletion...
  
  if(SPECIES == "SKJ") { # Temporarily only applies to SKJ
    raw_data = merge(copy(raw_data), strata_to_delete, by = c("YEAR", "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE"), all.x = TRUE)
  }  else {
    raw_data = merge(copy(raw_data), strata_to_delete, by = c("YEAR", "FLEET", "GEAR_CODE"), all.x = TRUE)
  }
  
  raw_data = raw_data[is.na(DELETE) | DELETE == FALSE]
  
  # Removing leftover columns from previous join
  
  delete_column(raw_data, "DELETE")
  
  NUMBER_OF_SAMPLES = sum(raw_data$NUMBER_OF_SAMPLES)
  
  if(NUMBER_OF_SAMPLES_orig != NUMBER_OF_SAMPLES) {
    l_warn(paste0("Number of samples *before* deletion of unwanted strata (", NUMBER_OF_SAMPLES_orig, ") ", 
                  "differs from number of samples *after* deletion (", NUMBER_OF_SAMPLES, ") by ", (NUMBER_OF_SAMPLES_orig - NUMBER_OF_SAMPLES), " individuals"))
  }
  
  raw_data[!GEAR_CODE %in% c("PS", "PSOB", "PSFS", "PSLS"), SCHOOL_TYPE_CODE := "UNCL"]
  
  return(raw_data)
}

unpivot_SF = function(pivoted, scale_by_samples = FALSE) {
  total_no_fish = sum(pivoted$TOT_FISH_COUNT)
  
  pivoted = copy(pivoted)
  
  # Melting the table to convert "Txyz" columns into proper size classes
  dataset_unpivoted = 
    melt.data.table(data = pivoted, 
                    value.name = "FISH_COUNT", 
                    variable.name = "SIZE_BIN",
                    id.vars = c("YEAR", "QUARTER", "MONTH_START", "MONTH_END", 
                                "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE", 
                                "FISHING_GROUND_CODE", 
                                "FIRST_CLASS_LOW", "SIZE_INTERVAL",
                                "NUMBER_OF_SAMPLES", "TOT_FISH_COUNT")
    )
  
  if(scale_by_samples) {
    # Performs the scaling by using the ratio between the 'actual'number of samples and the original fish count
    dataset_unpivoted[TOT_FISH_COUNT > 0, FISH_COUNT := FISH_COUNT * NUMBER_OF_SAMPLES / TOT_FISH_COUNT]
  }
  
  MAX_BIN = as.integer(str_sub(max(as.character(dataset_unpivoted$SIZE_BIN)), start = -3))
  
  dataset_unpivoted = dataset_unpivoted[!is.na(FISH_COUNT) & FISH_COUNT > 0]# Excludes records with 0 fish
  
  # Building the SIZE_CLASS field
  dataset_unpivoted = size_class_from_size_bin(dataset_unpivoted)
  
  dataset_unpivoted[, SIZE_CLASS := FIRST_CLASS_LOW + ( (as.integer(substr(SIZE_BIN, 2, 4)) - 1) * SIZE_INTERVAL )]
  
  # Renaming / reordering the columns
  dataset_unpivoted = dataset_unpivoted[, .(YEAR, QUARTER, MONTH = MONTH_START,
                                            FLEET, GEAR_CODE, SCHOOL_TYPE_CODE,
                                            FISHING_GROUND_CODE, 
                                            FIRST_CLASS_LOW, SIZE_INTERVAL,
                                            NUMBER_OF_SAMPLES,
                                            SIZE_BIN, SIZE_CLASS, 
                                            FISH_COUNT)]
  
  dataset_unpivoted = size_bin_from_size_class(dataset_unpivoted, max_bins = MAX_BIN)
  
  return(dataset_unpivoted)
}

standardize_grids = function(unpivoted, target_grid_type = grid_5x5) {
  total_no_fish = sum(unpivoted$FISH_COUNT, na.rm = TRUE)
  
  grid_mappings = grid_intersections_by_target_grid_type(unique(unpivoted$FISHING_GROUND_CODE), target_grid_type)
  
  unpivoted = merge(unpivoted, grid_mappings, 
                    by.x = "FISHING_GROUND_CODE",
                    by.y = "SOURCE_FISHING_GROUND_CODE",
                    all.x = TRUE, allow.cartesian = TRUE)
  
  unmapped_grids = unique(unpivoted[is.na(TARGET_FISHING_GROUND_CODE)]$FISHING_GROUND_CODE)
  
  if(length(unmapped_grids) > 0) {
    lost_fish = sum(unpivoted[is.na(TARGET_FISHING_GROUND_CODE)]$FISH_COUNT, na.rm = TRUE)
    
    l_info(paste0(length(unmapped_grids), " unique grids cannot be mapped on regular 5x5 grids, for a total of ", lost_fish, " individuals lost"))
    
    l_warn("Unmapped grids:")
    l_warn(unmapped_grids)
  }
  
  unpivoted = unpivoted[, `:=`(FISH_COUNT = FISH_COUNT * PROPORTION,
                               NUMBER_OF_SAMPLES = NUMBER_OF_SAMPLES * PROPORTION)]
  unpivoted = unpivoted[, .(FISH_COUNT  = sum(FISH_COUNT, na.rm = TRUE)),
                            keyby = .(YEAR, QUARTER, MONTH, 
                                      FLEET, GEAR_CODE, SCHOOL_TYPE_CODE,
                                      FISHING_GROUND_CODE = TARGET_FISHING_GROUND_CODE,
                                      FIRST_CLASS_LOW, SIZE_INTERVAL,
                                      NUMBER_OF_SAMPLES,
                                      SIZE_BIN, SIZE_CLASS)]
  
  total_no_fish_after = sum(unpivoted$FISH_COUNT, na.rm = TRUE)
  
  l_info(paste0("Number of fish before processing: ", total_no_fish, " - ", 
                "After processing: ", total_no_fish_after, " - ", 
                "Difference: ", ( total_no_fish_after - total_no_fish )))
  
  return(unpivoted)
}

repivot_SF_fishery_area = function(unpivoted) {
  unpivoted = assign_area_and_fishery(unpivoted)
  unpivoted = unpivoted[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE)),
                            keyby = .(FISHERY, AREA, AREA_ORIG,
                                      YEAR, QUARTER,
                                      FIRST_CLASS_LOW, SIZE_INTERVAL,
                                      #NUMBER_OF_SAMPLES,
                                      SIZE_BIN, SIZE_CLASS)]
  
  unpivoted[, NUMBER_OF_SAMPLES := round(sum(FISH_COUNT, na.rm = TRUE), 2), 
              by = .(FISHERY, AREA, AREA_ORIG,
                     YEAR, QUARTER,
                     FIRST_CLASS_LOW, SIZE_INTERVAL)]
                     #NUMBER_OF_SAMPLES)]
  
  setcolorder(unpivoted, c("FISHERY", "AREA", "AREA_ORIG",
                           "YEAR", "QUARTER", 
                           "FIRST_CLASS_LOW", "SIZE_INTERVAL", 
                           "NUMBER_OF_SAMPLES",
                           "SIZE_BIN", "SIZE_CLASS",
                           "FISH_COUNT"))
  
  pivoted = 
    dcast.data.table(
      unpivoted[FISH_COUNT > 0], 
      FISHERY + AREA + AREA_ORIG + YEAR + QUARTER + FIRST_CLASS_LOW + SIZE_INTERVAL + NUMBER_OF_SAMPLES ~ SIZE_BIN,
      value.var = "FISH_COUNT", 
      fun = sum, 
      fill = NA,
      drop = c(TRUE, FALSE)
    )

  return(pivoted)
}

repivot_SF = function(unpivoted) {
  unpivoted = assign_area_and_fishery(unpivoted)
  unpivoted = unpivoted[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE)),
                            keyby = .(YEAR, QUARTER, MONTH,
                                      FLEET, GEAR_CODE, SCHOOL_TYPE_CODE,
                                      FISHING_GROUND_CODE,
                                      FIRST_CLASS_LOW, SIZE_INTERVAL,
                                      SIZE_BIN, SIZE_CLASS)]
  
  unpivoted[, NUMBER_OF_SAMPLES := round(sum(FISH_COUNT, na.rm = TRUE), 2), 
              by = .(YEAR, QUARTER, MONTH,
                     FLEET, GEAR_CODE, SCHOOL_TYPE_CODE,
                     FISHING_GROUND_CODE,
                     FIRST_CLASS_LOW, SIZE_INTERVAL)]
  
  setcolorder(unpivoted, c("YEAR", "QUARTER", "MONTH",
                           "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE",
                           "FISHING_GROUND_CODE",
                           "FIRST_CLASS_LOW", "SIZE_INTERVAL", 
                           "NUMBER_OF_SAMPLES",
                           "SIZE_BIN", "SIZE_CLASS",
                           "FISH_COUNT"))
  
  pivoted = 
    dcast.data.table(
      unpivoted[FISH_COUNT > 0], 
      YEAR + QUARTER + MONTH + FLEET + GEAR_CODE + SCHOOL_TYPE_CODE + FISHING_GROUND_CODE + FIRST_CLASS_LOW + SIZE_INTERVAL + NUMBER_OF_SAMPLES ~ SIZE_BIN,
      value.var = "FISH_COUNT", 
      fun = sum, 
      fill = NA,
      drop = c(TRUE, FALSE)
    )
  
  return(pivoted)
}

### Actual data preparation

## Pivoted SF data by fishery, area, year and quarter

SF_FIA_Q   = sanitize_SF(SF_FL_WITH_SAMPLES, scale_by_samples = TRUE)

SF_FIA_Q_UNPIVOTED = 
  size_bin_from_size_class(
    standardize_grids(
      unpivot_SF(
        remove_SF_strata(
          SF_FIA_Q,
          SF_strata_DEL
        ), 
        scale_by_samples = TRUE
      )
    ),
    first_class_low = DEFAULT_FIRST_CLASS_LOW,
    size_interval   = DEFAULT_SIZE_INTERVAL,
    max_bins        = DEFAULT_NUM_SIZE_BINS
  )

SF_FIA_Q = repivot_SF_fishery_area(SF_FIA_Q_UNPIVOTED)

# The following is produced but not really used 

SF_YQMFG = sanitize_SF(SF_FL_WITH_SAMPLES, scale_by_samples = TRUE)

SF_YQMFG_UNPIVOTED = 
  size_bin_from_size_class(
    standardize_grids(
      unpivot_SF(
        remove_SF_strata(
          SF_YQMFG,
          SF_strata_DEL
        ), 
        scale_by_samples = TRUE
      )
    ),
    first_class_low = DEFAULT_FIRST_CLASS_LOW, # This constant is species specific, and included with the species configuration
    size_interval   = DEFAULT_SIZE_INTERVAL,   # This constant is species specific, and included with the species configuration
    max_bins        = DEFAULT_NUM_SIZE_BINS    # This constant is species specific, and included with the species configuration
  )

SF_YQMFG = repivot_SF(SF_YQMFG_UNPIVOTED)
