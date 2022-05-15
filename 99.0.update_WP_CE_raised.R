print("#### Updating WP_CE_raised database...")

print(paste0("!!! Preparing data for ", SPECIES))

CE_R_YQMFG = sanitize_duplicated_SF_areas(CE_R_YQMFG)

CA_RAISED = CE_R_YQMFG[, .(CATCH = sum(EST_MT, na.rm = TRUE), CATCH_IN_NUMBERS = sum(EST_NO, na.rm = TRUE)),
                           keyby = .(YEAR, MONTH_START = MONTH, MONTH_END = MONTH, 
                                     FLEET_CODE = FLEET, GEAR_CODE, FISHING_GROUND_CODE,
                                     CATCH_SCHOOL_TYPE_CODE = SCHOOL_TYPE_CODE)]

CA_RAISED[, `:=`(SPECIES_CODE = SPECIES, CATCH_UNIT_CODE = "MT")]

CA_RAISED_SPECIES = unique(CA_RAISED$SPECIES_CODE)

if(!SPECIES %in% CA_RAISED_SPECIES) {
  stop(paste0("Current species (", SPECIES, ") cannot be found among the species set for the available records from CE_raised (", CA_RAISED_SPECIES, ")"))
}

GEARS = query(DB_IOTDB(), "SELECT ACode AS GEAR_CODE, LAggCESF AS FISHERY_GROUP FROM cdeGears")
FISHERY_TYPES = query(DB_IOTDB(), "SELECT DISTINCT LTRIM(RTRIM(DSFleetCode)) AS FLEET_CODE, LTRIM(RTRIM(Gear)) AS GEAR_CODE, LTRIM(RTRIM(TypeOperation)) AS FISHERY_TYPE_CODE FROM CountryStratvsFleet")

CA_RAISED = merge(CA_RAISED, GEARS,
                  by = "GEAR_CODE",
                  all.x = TRUE)

CA_RAISED = merge(CA_RAISED, FISHERY_TYPES,
                  by = c("FLEET_CODE", "GEAR_CODE"),
                  all.x = TRUE)

CA_RAISED[, FISHERY_GROUP := tolower(FISHERY_GROUP)]

CA_RAISED[,                               FISHERY_GROUP_CODE := "OT"]
CA_RAISED[FISHERY_GROUP == "baitboat",    FISHERY_GROUP_CODE := "BB"]
CA_RAISED[FISHERY_GROUP == "purse seine", FISHERY_GROUP_CODE := "PS"]
CA_RAISED[FISHERY_GROUP == "longline",    FISHERY_GROUP_CODE := "LL"]
CA_RAISED[FISHERY_GROUP == "gillnet",     FISHERY_GROUP_CODE := "GN"]
CA_RAISED[FISHERY_GROUP == "handline",    FISHERY_GROUP_CODE := "HL"]
CA_RAISED[FISHERY_GROUP == "trolling",    FISHERY_GROUP_CODE := "TL"]

CA_RAISED = decorate(CA_RAISED)[, .(YEAR, MONTH_START, MONTH_END, FLEET_CODE, 
                                    GEAR_CODE, GEAR, FISHERY_GROUP_CODE, FISHERY_TYPE_CODE, 
                                    FISHING_GROUND_CODE, CATCH_SCHOOL_TYPE_CODE,
                                    SPECIES_CODE, SPECIES_CATEGORY_CODE, SPECIES_GROUP_CODE, SPECIES_WP_CODE,
                                    IS_IOTC_SPECIES, IS_SPECIES_AGGREGATE, IS_SSI, 
                                    CATCH, CATCH_UNIT_CODE, CATCH_IN_NUMBERS)]

CE_RAISED_DB = DB_WP_CE_RAISED()

### Updating CA_RAISED table 

CURRENT = query(CE_RAISED_DB, paste0("SELECT COUNT(*) AS NUM_RECORDS FROM CA_RAISED WHERE SPECIES_CODE = '", SPECIES, "'"))
  
print(paste0("!!! Deleting existing ", CURRENT$NUM_RECORDS, " records from CA_RAISED for ", SPECIES))

query(CE_RAISED_DB, paste0("DELETE FROM CA_RAISED WHERE SPECIES_CODE = '", SPECIES, "'"))

print(paste0("!!! Storing current data in CA_RAISED for ", SPECIES, ": ", nrow(CA_RAISED), " total records"))

dbWriteTable(CE_RAISED_DB, name = "CA_RAISED", CA_RAISED, overwrite = FALSE, append = TRUE)

### Updating CEForSF table 

CURRENT = query(CE_RAISED_DB, paste0("SELECT COUNT(*) AS NUM_RECORDS FROM CEForSF WHERE Species = '", SPECIES, "'"))

print(paste0("!!! Deleting existing ", CURRENT$NUM_RECORDS, " records from CEForSF for ", SPECIES))

query(CE_RAISED_DB, paste0("DELETE FROM CEForSF WHERE Species = '", SPECIES, "'"))

print(paste0("!!! Storing current data in CEForSF for ", SPECIES, ": ", nrow(CE_for_SF), " total records"))

dbWriteTable(CE_RAISED_DB, name = "CEForSF", CE_for_SF, overwrite = FALSE, append = TRUE)

print("#### Finished updating WP_CE_raised database!")


