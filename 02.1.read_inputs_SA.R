# The mapping between 1x1 grids, 5x5 grids and PS areas
if(FALSE) { # DEPRECATED
  GRIDS_15_MAPPINGS = get_table(IN, "0Grids1Fishing")
  GRIDS_15_MAPPINGS = GRIDS_15_MAPPINGS[, .(GRID_1 = as.character(Grid1), 
                                            GRID_5 = as.character(Grid6), 
                                            PS_AREA_NUMBER = as.character(PSnoArea),
                                            PS_AREA = as.character(PSArea))]
  
  save(list = "GRIDS_15_MAPPINGS", file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/grids_1_5_mappings.RData"))
  write.csv(GRIDS_15_MAPPINGS, file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/grids_1_5_mappings.csv"), row.names = FALSE)
  
  # The mapping between 5x5 grids and PS / LL areas
  GRIDS_5_MAPPINGS = get_table(IN, "0Grids6Fishing")
  GRIDS_5_MAPPINGS = GRIDS_5_MAPPINGS[, .(GRID_5 = as.character(Grid6), 
                                          PS_AREA_NUMBER = as.character(PSnoArea),
                                          PS_AREA = as.character(PSArea),
                                          LL_AREA = as.character(LLArea))]
  
  save(list = "GRIDS_5_MAPPINGS", file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/grids_5_mappings.RData"))
  write.csv(GRIDS_5_MAPPINGS, file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/grids_5_mappings.csv"), row.names = FALSE)
  
  GRIDS_5_LL = GRIDS_5_MAPPINGS[, .(GEAR_TYPE = "LL", GRID_5, SF_AREA = LL_AREA, PROPORTION = 1)]
  GRIDS_5_PS = unique(GRIDS_15_MAPPINGS[, .(GEAR_TYPE = "PS", GRID_5, SF_AREA = PS_AREA, PROPORTION = 1)])
  
  GRIDS_5_PS[, PROPORTION := 1 / .N, by = .(GRID_5)]
  
  GRIDS_5_PS_LL = rbind(GRIDS_5_LL, GRIDS_5_PS)
  
  save(list = "GRIDS_5_PS_LL", file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/grids_5_PS_LL.RData"))
  write.csv(GRIDS_5_MAPPINGS, file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/grids_5_PS_LL.csv"), row.names = FALSE)
}

# The list of SF strata (YEAR + FLEET + GEAR) to be deleted
SF_strata_DEL = unique(get_table(IN, "0StrataSF")[Table == SPECIES])

if(SPECIES == "SKJ") { # This should only apply to SKJ for the time being
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
write.csv(AL_KEYS,     file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/AGE_LENGTH_KEYS.csv"), row.names = FALSE)
