# Stock Assessment Data Inputs and Outputs ####
SA_DATA_SUMMARY_NAME = paste0("IOTC-", substring(WP_CURRENT, 1, 4), "-", LOCAL_FOLDER, "-", SPECIES, "-SA Data Summary.xlsx")
SA_DATA_SUMMARY_PATH = output_folder(SPECIES, LOCAL_FOLDER, paste0(SA_DATA_SUMMARY_NAME))

# XLSX spreadsheet ####

# Create empty workbook
SA_DATA = createWorkbook("SA_OUTPUTS")

## Add Fishery and Area Mapping ####
addWorksheet(SA_DATA, "Fishery and area mappings")

writeDataTable(SA_DATA, sheet = "Fishery and area mappings", x = SPECIES_SPECIFIC_FISHERY_MAPPINGS, rowNames = FALSE, tableStyle = "TableStyleLight11")
setColWidths(SA_DATA, sheet = "Fishery and area mappings", cols = 1:ncol(SPECIES_SPECIFIC_FISHERY_MAPPINGS), widths = "auto")

## Add Size-Frequency Strata to Exclude ####
addWorksheet(SA_DATA, "SF strata to delete")

writeDataTable(SA_DATA, sheet = "SF strata to delete", x = SF_strata_DEL, rowNames = FALSE, tableStyle = "TableStyleLight11")
setColWidths(SA_DATA, sheet = "SF strata to delete", cols = 1:ncol(SF_strata_DEL), widths = "auto")

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

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = SAMPLES_VS_ESTIMATED_PIVOT, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataPosition)

DataRowPosition = DataRowPosition + nrow(SAMPLES_VS_ESTIMATED_PIVOT) + 2

### Numbers of Fish Sampled by Year and Fishery
writeData(SA_DATA, "Samples and coverage", paste0("Number of fish ", SPECIES, " specimens sampled for size by year, fishery, and assessment area"), startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + 1

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = SAMPLES_AVAILABLE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + nrow(SAMPLES_AVAILABLE) + 2

addStyle(SA_DATA, sheet = "Samples and coverage", style = createStyle(numFmt = "#,##0"), rows = 1:(nrow(SAMPLES_VS_ESTIMATED_PIVOT) + nrow(SAMPLES_AVAILABLE) + 6), cols = 1:ncol(SAMPLES_VS_ESTIMATED_PIVOT) + 1, gridExpand = TRUE)

### Percentage of Fish Sampled in the Catch ####
writeData(SA_DATA, "Samples and coverage", paste0("Fraction of ", SPECIES, " specimens sampled for size vs. estimated number of fish caught by year, fishery and assessment area"), startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + 1

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = ORIGINAL_VS_ESTIMATED_COVERAGE, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataRowPosition)

addStyle(SA_DATA, sheet = "Samples and coverage", style = createStyle(numFmt="0.0%"), rows = DataRowPosition:DataRowPosition + nrow(ORIGINAL_VS_ESTIMATED_COVERAGE), cols = 1:ncol(SAMPLES_VS_ESTIMATED_PIVOT) + 1, gridExpand = TRUE)

DataRowPosition = DataRowPosition + nrow(ORIGINAL_VS_ESTIMATED_COVERAGE) + 2

### Sampling Target Reached ####
writeData(SA_DATA, "Samples and coverage", paste0("Strata with at least one ", SPECIES, " sampled per metric tonne of estimated catch"), startCol = 1, startRow = DataRowPosition)

DataRowPosition = DataRowPosition + 1

writeDataTable(SA_DATA, sheet = "Samples and coverage", x = MINIMUM_COVERAGE_REACHED, rowNames = FALSE, tableStyle = "TableStyleMedium8", startCol = 1, startRow = DataRowPosition)

setColWidths(SA_DATA, sheet = "Samples and coverage", cols = 2:ncol(SAMPLES_VS_ESTIMATED_PIVOT) + 1, widths = "auto")

# Close and save the workbook
saveWorkbook(wb = SA_DATA, file = SA_DATA_SUMMARY_PATH, overwrite = TRUE)


# Zip the file
#zip::zip(zipfile = paste0(), ".zip"), files = paste0(output_folder(SPECIES, LOCAL_FOLDER, paste0("catches/", CA_RAISED_OUTPUT_NAME)), ".xlsx"), mode = "cherry-pick", recurse = FALSE))


# CATCH AT SIZE ####






# IOTC-2023-WPTT25(AS)-DATA14-SA_SKJ_01_Rev3-summary.xlsx
# IOTC-2023-WPTT25(AS)-DATA14-SA_SKJ_02_Rev3-SS3.xlsx
# IOTC-2023-WPTT25(AS)-DATA14-SA_SKJ_03_Rev3-CAS.xlsx

## CSV files ####
write.csv(SPECIES_SPECIFIC_FISHERY_MAPPINGS, paste0(CA_RAISED_OUTPUT_PATH, "FISHERY_MAPPING.csv"), row.names = FALSE)

zip::zip(zipfile = paste0(CA_RAISED_OUTPUT_PATH, "FISHERY_MAPPING.zip"), files = paste0(CA_RAISED_OUTPUT_PATH, "FISHERY_MAPPING.csv"), mode = "cherry-pick", recurse = FALSE)

write.csv(CA_RAISED, paste0(CA_RAISED_OUTPUT_PATH, "CATCHES_RAISED.csv"), row.names = FALSE)

zip::zip(zipfile = paste0(CA_RAISED_OUTPUT_PATH, "CATCHES_RAISED.zip"), files = paste0(CA_RAISED_OUTPUT_PATH, "CATCHES_RAISED.csv"), mode = "cherry-pick", recurse = FALSE)

