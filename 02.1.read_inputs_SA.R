# The list of SF strata (YEAR + FLEET + GEAR) to be deleted
SF_strata_DEL = unique(get_table(IN, "0StrataSF")[Table == SPECIES])

if(SPECIES == "COM") { # This should only apply to SKJ for the time being   # changed SKJ to COM as school type absent from table 0StrataSF so this is not working
  SF_strata_DEL = SF_strata_DEL[, .(YEAR = Year,
                                    FLEET = str_trim(Fleet),
                                    GEAR_CODE = str_trim(Gear),
                                    SCHOOL_TYPE_CODE = str_trim(SchoolType),
                                    DELETE = Delete)]
  
  SF_strata_DEL = SF_strata_DEL[, DELETE := ifelse(DELETE == 1, TRUE, FALSE)][order(-DELETE, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE, YEAR)]
  SF_strata_DEL[, NUM_RECORDS := .N, by = .(YEAR, FLEET, GEAR_CODE, SCHOOL_TYPE_CODE)]
  SF_strata_DEL = SF_strata_DEL[NUM_RECORDS == 1 | DELETE]
  SF_strata_DEL$NUM_RECORDS = NULL
} else {
  SF_strata_DEL = SF_strata_DEL[, .(YEAR = Year,
                                    FLEET = str_trim(Fleet),
                                    GEAR_CODE = str_trim(Gear),
                                    DELETE = Delete)]
  
  SF_strata_DEL = SF_strata_DEL[, DELETE := ifelse(DELETE == 1, TRUE, FALSE)][order(-DELETE, FLEET, GEAR_CODE, YEAR)]
  SF_strata_DEL[, NUM_RECORDS := .N, by = .(YEAR, FLEET, GEAR_CODE)]
  SF_strata_DEL = SF_strata_DEL[NUM_RECORDS == 1 | DELETE]
  SF_strata_DEL$NUM_RECORDS = NULL
}

write.csv(SF_strata_DEL, file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/SF_strata_to_delete.csv"), row.names = FALSE)

SF_strata_DEL = SF_strata_DEL[DELETE == TRUE]

save(list = "SF_strata_DEL", file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/SF_strata_to_delete.RData"))

# Reads all the byproducts of the CE raising process

CE_all    = get_table(OUT, "CEall")   [Species == SPECIES] # To ensure we don't get data for other species (unlikely, yet...)
CE_raised = get_table(OUT, "CEraised")[Species == SPECIES] # To ensure we don't get data for other species (unlikely, yet...)
CE_for_SF = get_table(OUT, "CEforSF") [Species == SPECIES] # To ensure we don't get data for other species (unlikely, yet...)
SF_all    = get_table(OUT, "SFall")   [Species == SPECIES] # To ensure we don't get data for other species (unlikely, yet...)

# Trim data
CE_all[, Fleet := trimws(Fleet)]
CE_all[, SchoolType := trimws(SchoolType)]
CE_all[, Gear := trimws(Gear)]

CE_raised[, Fleet := trimws(Fleet)]
CE_raised[, SchoolType := trimws(SchoolType)]
CE_raised[, Gear := trimws(Gear)]

save(list = "CE_all",    file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_all.RData"))
save(list = "CE_raised", file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_raised.RData"))
save(list = "CE_for_SF", file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_for_SF.RData"))
save(list = "SF_all",    file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/SF_all.RData"))

write.csv(CE_all,    file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_all.csv"),    row.names = FALSE)
write.csv(CE_raised, file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_raised.csv"), row.names = FALSE)
write.csv(CE_for_SF, file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/CE_for_SF.csv"), row.names = FALSE)
write.csv(SF_all,    file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/SF_all.csv"),    row.names = FALSE)

# Reads the age-length keys for the species

AL_KEYS = get_table(AL, "LAKey")[Species == SPECIES]
AL_KEYS[, LengthTo := LengthTo - .01]
setkey(AL_KEYS, LengthFrom, LengthTo) # Necessary to be able to use 'foverlaps'

save(list = "AL_KEYS", file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/AGE_LENGTH_KEYS.RData"))
write.csv(AL_KEYS, file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/AGE_LENGTH_KEYS.csv"), row.names = FALSE)

