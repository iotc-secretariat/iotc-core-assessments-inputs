
# Map IOTC fisheries to SA fisheries

fishery_mapping = fread("./species/SWO/FISHERY_AREA_MAPPINGS.csv")

CE_raised[, Fleet := trimws(Fleet)]
CE_raised[, Fleet := trimws(Fleet)]
CE_raised[, Fleet := trimws(Fleet)]

CE = merge(CE_raised, fishery_mapping, by.x = c("Fleet", "Gear", "SchoolType"), by.y = c("FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE"), all.x = TRUE)
