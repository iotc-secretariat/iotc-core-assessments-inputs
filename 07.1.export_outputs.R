# Configure output names
SA_DATA_GENERIC_NAME = paste0("IOTC-", substring(WP_CURRENT, 1, 4), "-", LOCAL_FOLDER, "-", SPECIES)

# 01- Summary of SA Data Inputs and Outputs ####
SA_DATA_SUMMARY_PATH = output_folder(SPECIES, LOCAL_FOLDER, paste0(SA_DATA_GENERIC_NAME, "_01_summary.xlsx"))

# Initiate workbook with metadata notes
SA_DATA = loadWorkbook(paste0("species/", SPECIES, "/WP/", LOCAL_FOLDER, "/input/01-NotesForSummary.xlsx"))

## Add Fishery and Area Mapping ####
addWorksheet(SA_DATA, "Fishery and area mappings")

writeDataTable(SA_DATA, sheet = "Fishery and area mappings", x = SPECIES_SPECIFIC_FISHERY_MAPPINGS, rowNames = FALSE, tableStyle = "TableStyleMedium8")

setColWidths(SA_DATA, sheet = "Fishery and area mappings", cols = 1:ncol(SPECIES_SPECIFIC_FISHERY_MAPPINGS), widths = 22)

## Add Size-Frequency Strata to Exclude ####
addWorksheet(SA_DATA, "SF strata to delete")

writeDataTable(SA_DATA, sheet = "SF strata to delete", x = SF_strata_DEL, rowNames = FALSE, tableStyle = "TableStyleMedium8")

setColWidths(SA_DATA, sheet = "SF strata to delete", cols = 1:ncol(SF_strata_DEL), widths = 14) #widths = "auto")

## Add Catches by Area ####
addWorksheet(SA_DATA, "Catches by area")

### Original Areas ####
writeData(SA_DATA, "Catches by area", "Estimated annual catches by original area (in weight, t)", startCol = 1, startRow = 1)

writeDataTable(SA_DATA, sheet = "Catches by area", x = CATCHES_BY_YEAR_AREA_ORIG_TABLE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = 2)

### Final Assessment Areas ####
writeData(SA_DATA, "Catches by area", "Estimated annual catches by assessment area (in weight, t)", startCol = 1, startRow = nrow(CATCHES_BY_YEAR_AREA_ORIG_TABLE) + 4)

writeDataTable(SA_DATA, sheet = "Catches by area", x = CATCHES_BY_YEAR_AREA_TABLE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = nrow(CATCHES_BY_YEAR_AREA_ORIG_TABLE) + 5)

setColWidths(SA_DATA, sheet = "Catches by area", cols = 2:ncol(CATCHES_BY_YEAR_AREA_TABLE), widths = "auto")

addStyle(SA_DATA, sheet = "Catches by area", style = createStyle(numFmt = "#,##0"), rows = 1:50, cols = 1:ncol(CATCHES_BY_YEAR_AREA_ORIG_TABLE) + 1, gridExpand = TRUE)

## Add Catches by Fishery ####

### Fishery Group ####
addWorksheet(SA_DATA, "Catches by fishery")

writeData(SA_DATA, "Catches by fishery", "Estimated annual catches by fishery group (in weight, t)", startCol = 1, startRow = 1)

writeDataTable(SA_DATA, sheet = "Catches by fishery", x = CATCHES_BY_YEAR_FISHERY_GROUP_TABLE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = 2)

### Fishery ####
writeData(SA_DATA, "Catches by fishery", "Estimated annual catches by fishery (in weight, t)", startCol = 1, startRow = nrow(CATCHES_BY_YEAR_FISHERY_GROUP_TABLE) + 4)

writeDataTable(SA_DATA, sheet = "Catches by fishery", x = CATCHES_BY_YEAR_FISHERY_TABLE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = nrow(CATCHES_BY_YEAR_FISHERY_GROUP_TABLE) + 5)

setColWidths(SA_DATA, sheet = "Catches by fishery", cols = 2:ncol(CATCHES_BY_YEAR_FISHERY_TABLE), widths = "auto")

addStyle(SA_DATA, sheet = "Catches by fishery", style = createStyle(numFmt = "#,##0"), rows = 1:50, cols = 1:ncol(CATCHES_BY_YEAR_FISHERY_TABLE) + 1, gridExpand = TRUE)

## Add Samples and Sampling Coverage
addWorksheet(SA_DATA, "Samples and coverage")

DataRowPosition = 1

### Numbers of Fish Sampled By Year #####
writeData(SA_DATA, "Samples and coverage", paste0("Annual number of ", SPECIES, " specimens sampled for size over estimates of total numbers caught (all fisheries combined)"), startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + 1

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = SAMPLES_VS_ESTIMATED_PIVOT, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + nrow(SAMPLES_VS_ESTIMATED_PIVOT) + 2

### Numbers of Fish Sampled by Year and Fishery
writeData(SA_DATA, "Samples and coverage", paste0("Number of fish ", SPECIES, " specimens sampled for size by year, fishery, and assessment area"), startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + 1

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = SAMPLES_AVAILABLE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + nrow(SAMPLES_AVAILABLE) + 2

addStyle(SA_DATA, sheet = "Samples and coverage", style = createStyle(numFmt = "#,##0"), rows = 1:(nrow(SAMPLES_VS_ESTIMATED_PIVOT) + nrow(SAMPLES_AVAILABLE) + 6), cols = 1:ncol(SAMPLES_VS_ESTIMATED_PIVOT) + 1, gridExpand = TRUE)

writeData(SA_DATA, "Samples and coverage", paste0("Fraction of ", SPECIES, " specimens sampled for size vs. estimated number of fish caught by year, fishery and assessment area"), startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + 1

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = ORIGINAL_VS_ESTIMATED_COVERAGE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataRowPosition)

addStyle(SA_DATA, sheet = "Samples and coverage", style = createStyle(numFmt = "PERCENTAGE"), rows = DataRowPosition:(DataRowPosition + nrow(ORIGINAL_VS_ESTIMATED_COVERAGE)), cols = 1:ncol(SAMPLES_VS_ESTIMATED_PIVOT) + 1, gridExpand = TRUE)

DataRowPosition = DataRowPosition + nrow(ORIGINAL_VS_ESTIMATED_COVERAGE) + 2

### Sampling Target Reached ####
writeData(SA_DATA, "Samples and coverage", paste0("Strata with at least one ", SPECIES, " sampled per metric tonne of estimated catch"), startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + 1

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = MINIMUM_COVERAGE_REACHED, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataRowPosition)

setColWidths(SA_DATA, sheet = "Samples and coverage", cols = 2:ncol(SAMPLES_VS_ESTIMATED_PIVOT) + 1, widths = "auto")

DataRowPosition = DataRowPosition + nrow(MINIMUM_COVERAGE_REACHED)

## Add Catch-at-Size and Numbers of Samples ####
addWorksheet(SA_DATA, "Size distribution")

writeData(SA_DATA, "Size distribution", paste0("Estimated relative distributions of ", SPECIES, " by length class (number of individuals, fork length) for all fisheries and time periods combined"), startCol = 1, startRow = 1)

writeDataTable(SA_DATA, sheet = "Size distribution", x = SAMPLES_BY_SIZE_PIVOT, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = 2)

setColWidths(SA_DATA, sheet = "Size distribution", cols = 2:(ncol(SAMPLES_BY_SIZE_PIVOT) + 1), widths = "auto")

addStyle(SA_DATA, sheet = "Size distribution", style = createStyle(numFmt = "PERCENTAGE"), rows = 3:(nrow(SAMPLES_BY_SIZE_PIVOT) + 2), cols = 2:(ncol(SAMPLES_BY_SIZE_PIVOT) + 1), gridExpand = TRUE)

## Add Average Weights ####
addWorksheet(SA_DATA, "Average weights")

### By Year and Fishery #####
writeData(SA_DATA, "Average weights", paste0("Estimated average weights of ", SPECIES, " by year and fishery"), startCol = 1, startRow = 1)

writeDataTable(SA_DATA, "Average weights", SA_AVG_WEIGHT_FISHERY_PIVOT_TABLE, startCol = 1, startRow = 2, rowNames = FALSE, tableStyle = "TableStyleMedium8")

setColWidths(SA_DATA, sheet = "Average weights", cols = 2:(ncol(SA_AVG_WEIGHT_FISHERY_PIVOT_TABLE) + 1), widths = "auto")

### By year for all Fisheries ####
writeData(SA_DATA, "Average weights", paste0("Estimated average weights of ", SPECIES , " by year (all fisheries combined)"), startCol = 1, startRow = 4 + nrow(SA_AVG_WEIGHT_FISHERY_PIVOT_TABLE))

writeDataTable(SA_DATA, "Average weights", SA_AVG_WEIGHT_HISTORICAL_PIVOT_TABLE[order(WP)], startCol = 1, startRow = 5 + nrow(SA_AVG_WEIGHT_FISHERY_PIVOT_TABLE), rowNames = FALSE, tableStyle = "TableStyleMedium8")

addStyle(SA_DATA, sheet = "Average weights", style = createStyle(numFmt = "#0.0"), cols = 2:(nrow(SA_AVG_WEIGHT_HISTORICAL_PIVOT_TABLE) + 1), rows = 1:50, gridExpand = TRUE)

# Close and save the workbook
saveWorkbook(wb = SA_DATA, file = SA_DATA_SUMMARY_PATH, overwrite = TRUE)

# 02- SS3 Data Inputs ####
SA_DATA_SS3_PATH = output_folder(SPECIES, LOCAL_FOLDER, paste0(SA_DATA_GENERIC_NAME, "_02_SS3.xlsx"))

# Initiate workbook with metadata notes
SA_DATA_SS3 = loadWorkbook(paste0("species/", SPECIES, "/WP/", LOCAL_FOLDER, "/input/02-NotesForSS3.xlsx"))

addWorksheet(SA_DATA_SS3, "SA_SAMPLES")

writeDataTable(SA_DATA_SS3, "SA_SAMPLES", SA_SAMPLES_FIA_Q, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium8")

# Close and save the workbook
saveWorkbook(wb = SA_DATA_SS3, file = SA_DATA_SS3_PATH, overwrite = TRUE)

# 03- CAS Data Inputs ####
SA_DATA_CAS_PATH = output_folder(SPECIES, LOCAL_FOLDER, paste0(SA_DATA_GENERIC_NAME, "_03_CAS.xlsx"))

# Initiate workbook with metadata notes
SA_DATA_CAS = loadWorkbook(paste0("species/", SPECIES, "/WP/", LOCAL_FOLDER, "/input/03-NotesForCAS.xlsx"))

addWorksheet(SA_DATA_CAS, "SA_CAS")

writeDataTable(SA_DATA_CAS, "SA_CAS", CAS_FIA_Q, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium8")

addWorksheet(SA_DATA_CAS, "SA_CAS_W")

writeDataTable(SA_DATA_CAS, "SA_CAS_W", CAS_FIA_Q_W, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium8")

# Close and save the workbook
saveWorkbook(wb = SA_DATA_CAS, file = SA_DATA_CAS_PATH, overwrite = TRUE)

# 04- CAA Data Inputs ####
SA_DATA_CAA_PATH = output_folder(SPECIES, LOCAL_FOLDER, paste0(SA_DATA_GENERIC_NAME, "_04_CAA.xlsx"))

# Initiate workbook with metadata notes
SA_DATA_CAA = loadWorkbook(paste0("species/", SPECIES, "/WP/", LOCAL_FOLDER, "/input/04-NotesForCAA.xlsx"))

addWorksheet(SA_DATA_CAA, "SA_CAA_DLWE1")

writeDataTable(SA_DATA_CAA, "SA_CAA_DLWE1", CAA_FIA_Q, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium8")

addWorksheet(SA_DATA_CAA, "SA_CAA_DLWE1_W")

writeDataTable(SA_DATA_CAA, "SA_CAA_DLWE1_W", CAA_FIA_Q_W, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium8")

# Close and save the workbook
saveWorkbook(wb = SA_DATA_CAA, file = SA_DATA_CAA_PATH, overwrite = TRUE)

# Raised Catch Data ##### 
SA_CATCH_DATA_PATH = output_folder(SPECIES, LOCAL_FOLDER, paste0(SA_DATA_GENERIC_NAME, "-Raised Catch Data.xlsx"))

# Create workbook
SA_CATCH_DATA = createWorkbook()

## Monthly raised catches by 5x5 grid
CA_RAISED_GRIDS = prepare_CE_raised(CE_raised)[order(YEAR, QUARTER, MONTH, FLEET, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)]
setnames(CA_RAISED_GRIDS, old = "FLEET", new = "FLEET_CODE")

addWorksheet(SA_CATCH_DATA, "Monthly raised catch by grid")

writeDataTable(SA_CATCH_DATA, "Monthly raised catch by grid", CA_RAISED_GRIDS, tableStyle = "TableStyleMedium8")

## Quarterly raised catches by SA area
CA_RAISED_AREAS = CE_R_FIA_Q[, .(YEAR, QUARTER, FISHERY, AREA_ORIG, AREA, EST_NO, EST_MT)][order(YEAR, QUARTER, FISHERY, AREA)]

addWorksheet(SA_CATCH_DATA, "Quarterly raised catch by area")

writeDataTable(SA_CATCH_DATA, "Quarterly raised catch by area", CA_RAISED_AREAS, tableStyle = "TableStyleMedium8")

# Close and save the workbook
saveWorkbook(wb = SA_CATCH_DATA, file = SA_CATCH_DATA_PATH, overwrite = TRUE)


