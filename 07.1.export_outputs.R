# RAISED CATCHES ####
CA_RAISED_OUTPUT_NAME = paste0("IOTC-", WP_CURRENT, "-", SPECIES, "-")
CA_RAISED_OUTPUT_PATH = output_folder(SPECIES, LOCAL_FOLDER, paste0("modellers/", CA_RAISED_OUTPUT_NAME))

## CSV files ####
write.csv(SPECIES_SPECIFIC_FISHERY_MAPPINGS, paste0(CA_RAISED_OUTPUT_PATH, "FISHERY_MAPPING.csv"), row.names = FALSE)

zip::zip(zipfile = paste0(CA_RAISED_OUTPUT_PATH, "FISHERY_MAPPING.zip"), files = paste0(CA_RAISED_OUTPUT_PATH, "FISHERY_MAPPING.csv"), mode = "cherry-pick", recurse = FALSE)

write.csv(CA_RAISED, paste0(CA_RAISED_OUTPUT_PATH, "CATCHES_RAISED.csv"), row.names = FALSE)

zip::zip(zipfile = paste0(CA_RAISED_OUTPUT_PATH, "CATCHES_RAISED.zip"), files = paste0(CA_RAISED_OUTPUT_PATH, "CATCHES_RAISED.csv"), mode = "cherry-pick", recurse = FALSE)

## XLSX spreadsheet ####

# Create empty workbook
CA_RAISED_OUTPUT = createWorkbook("CA_RAISED_OUTPUT")

# Add fishery mapping to workbook
addWorksheet(CA_RAISED_OUTPUT, "FISHERY MAPPING")
writeDataTable(CA_RAISED_OUTPUT, sheet = "FISHERY MAPPING", x = SPECIES_SPECIFIC_FISHERY_MAPPINGS, rowNames = FALSE, tableStyle = "TableStyleLight11")
setColWidths(CA_RAISED_OUTPUT, sheet = "FISHERY MAPPING", cols = 1:ncol(SPECIES_SPECIFIC_FISHERY_MAPPINGS), widths = "auto")

# Add raised catch to workbook
addWorksheet(CA_RAISED_OUTPUT, "CATCHES")
writeDataTable(CA_RAISED_OUTPUT, sheet = "CATCHES", x = CA_RAISED, rowNames = FALSE, tableStyle = "TableStyleMedium2")
setColWidths(CA_RAISED_OUTPUT, sheet = "CATCHES", cols = 1:ncol(CA_RAISED), widths = "auto")

# Save the workbook
saveWorkbook(CA_RAISED_OUTPUT, file = paste0(output_folder(SPECIES, LOCAL_FOLDER, paste0("catches/", CA_RAISED_OUTPUT_NAME)), ".xlsx"), overwrite = TRUE)

# Zip the file
zip::zip(zipfile = paste0(), ".zip"), files = paste0(output_folder(SPECIES, LOCAL_FOLDER, paste0("catches/", CA_RAISED_OUTPUT_NAME)), ".xlsx"), mode = "cherry-pick", recurse = FALSE)

# CATCH AT SIZE ####






# IOTC-2023-WPTT25(AS)-DATA14-SA_SKJ_01_Rev3-summary.xlsx
# IOTC-2023-WPTT25(AS)-DATA14-SA_SKJ_02_Rev3-SS3.xlsx
# IOTC-2023-WPTT25(AS)-DATA14-SA_SKJ_03_Rev3-CAS.xlsx
