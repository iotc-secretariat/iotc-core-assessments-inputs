### Processing of SF_all

load(input_folder(SPECIES, LOCAL_FOLDER, "CAS/SF_all.RData"))

SF_all_PIVOTED = copy(SF_all)

# Adds QUARTER info
# SF_all_PIVOTED[, QUARTER := ifelse(MonthStart <= 3, 1, ifelse(MonthStart <= 6, 2, ifelse(MonthStart <=9, 3, 4)))]
SF_all_PIVOTED[, QUARTER := MonthStart]

# Deletes unused columns
SF_all_PIVOTED$id = NULL
SF_all_PIVOTED$TI = NULL
SF_all_PIVOTED$FleetA = NULL
SF_all_PIVOTED$GearA = NULL
SF_all_PIVOTED$Latitude = NULL
SF_all_PIVOTED$Longitude = NULL
SF_all_PIVOTED$Species = NULL
SF_all_PIVOTED$SUBSnoFish = NULL
SF_all_PIVOTED$SUBSmtFish = NULL

# Sanitizes columns
SF_all_PIVOTED[, `:=`(Fleet = str_trim(Fleet), Gear = str_trim(Gear), Schooltype = str_trim(Schooltype),
                      Grid = as.character(Grid))]

# Extracts core data fields, renames and reorders columns
SF_all_PIVOTED_core = copy(SF_all_PIVOTED)[, .(YEAR = Year, QUARTER, MONTH_START = MonthStart,
                                               PERIOD_AGG = PeriodAgg,
                                               FLEET = Fleet, GEAR_CODE = Gear, SCHOOL_TYPE_CODE = Schooltype,
                                               SF_FISHING_GROUND_CODE = Grid,
                                               AREA_AGG = AreaAgg,
                                               SF_NO = SFnoFish, SF_MT = SFmtFish,
                                               SIZE_INT_AGG = SizeIntAgg,
                                               FIRST_CLASS_LOW = FirstClassLow, SIZE_INTERVAL = SizeInterval)]

# Unnecessary
SF_all_PIVOTED_core = unique(SF_all_PIVOTED_core)

# Builds strata from the core data fields
SF_all_PIVOTED_strata = copy(SF_all_PIVOTED_core)[, .(YEAR, QUARTER, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, SF_FISHING_GROUND_CODE)]

# Builds samples data (excluding non-core data fields)
SF_all_PIVOTED_distribution = copy(SF_all_PIVOTED)

# Deletes unnecessary columns
SF_all_PIVOTED_distribution$PeriodAgg = NULL
SF_all_PIVOTED_distribution$AreaAgg = NULL
SF_all_PIVOTED_distribution$SFnoFish = NULL
SF_all_PIVOTED_distribution$SFmtFish = NULL
SF_all_PIVOTED_distribution$SizeIntAgg = NULL

# Melts the table to convert "Txyz" columns into proper size classes
SF_all_UNPIVOTED_distribution = 
  melt.data.table(data = SF_all_PIVOTED_distribution, 
                  value.name = "FISH_COUNT", 
                  variable.name = "SIZE_BIN",
                  id.vars = c("Year", "QUARTER", "MonthStart", "Fleet", "Gear", "Schooltype", "Grid", "FirstClassLow", "SizeInterval")
  )

# Removes all rows without samples 
SF_all_UNPIVOTED_distribution = SF_all_UNPIVOTED_distribution[!is.na(FISH_COUNT) & FISH_COUNT > 0]

# Builds the SIZE_CLASS
SF_all_UNPIVOTED_distribution[, SIZE_CLASS := FirstClassLow + ( (as.integer(substr(SIZE_BIN, 2, 4)) - 1) * SizeInterval )]

# Extracts core data fields, renames and reorders columns
SF_all_UNPIVOTED_distribution = SF_all_UNPIVOTED_distribution[, .(YEAR = Year, QUARTER, MONTH_START = MonthStart, FLEET = Fleet, GEAR_CODE = Gear, SCHOOL_TYPE_CODE = Schooltype, 
                                                                  SF_AREA = Grid,
                                                                  SIZE_BIN, SIZE_CLASS, FISH_COUNT)]

SF_all_UNPIVOTED_distribution_Q = SF_all_UNPIVOTED_distribution[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE)),
                                                                    keyby = .(YEAR, QUARTER, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, SF_AREA, SIZE_BIN, SIZE_CLASS)]