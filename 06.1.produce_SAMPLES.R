### SA_SAMPLES table

l_info("###### SA_SAMPLES ######")

# CE_R_xyz contains the standardized raised catches in numbers and weight by year, quarter and criteria 
# (either fishery + area, or month + fleet + gear + school type + fishing ground)

# CE_R_FIA_Q is created in 03.2.prepare_inputs_SA_CE_raised.R
# CE_R_YQMFG is created in 03.2.prepare_inputs_SA_CE_raised.R

# SF_xyz contains the standardized original samples in numbers by size class, year, quarter and criteria 
# (either fishery + area, or month + fleet + gear + school type + fishing ground)

# SF_FIA_Q is created in 03.5.prepare_inputs_IOTDB_SF.R
# SF_YQMFG is created in 03.5.prepare_inputs_IOTDB_SF.R

############################################################################
#### Produces 'Standard' SA samples by Fishery, Area, Year, and Quarter ####
############################################################################

SF_FIA_Q = SF_FIA_Q[YEAR <= max(CE_R_FIA_Q$YEAR)]
SF_YQMFG = SF_YQMFG[YEAR <= max(CE_R_FIA_Q$YEAR)]

l_info("###### Preparing SA raised catches and raw samples by FI / A / Y / Q...")

SA_SAMPLES_FIA_Q = prepare_SA_samples_FIA_Q(CE_R_FIA_Q, SF_FIA_Q)

SF_SAMPLES_NO_FIA_Q = sum(SF_FIA_Q$NUMBER_OF_SAMPLES)  
SF_SAMPLES_NO_YQMFG = sum(SF_YQMFG$NUMBER_OF_SAMPLES)  

SF_SAMPLES_NO = SF_SAMPLES_NO_FIA_Q

# A first diagnostic

if(SF_SAMPLES_NO_FIA_Q != SF_SAMPLES_NO_YQMFG) {
  l_warn(paste0("The number of samples aggregated by Fishery, Area and Quarter (", SF_SAMPLES_NO_FIA_Q, ") differs from ",
                "the number of samples aggregated by Year, Quarter, Month, Fleet, Gear and fishing ground (", SF_SAMPLES_NO_YQMFG, ") - ",
                "difference = ", ( SF_SAMPLES_NO_FIA_Q - SF_SAMPLES_NO_YQMFG )))
}

# If everything went well, outputs the data as a .csv file that corresponds to the SA_Samples worksheet of the assessment input file
write.csv(SA_SAMPLES_FIA_Q, file = output_folder(SPECIES, LOCAL_FOLDER, "raised_with_samples/SA_SAMPLES.csv"), row.names = FALSE, na = "")

l_info("###### Preparing ALTERNATIVE SA raised catches and raw samples by FI / A / Y / Q...")

SA_SAMPLES_FIA_Q_ALT = prepare_SA_samples_FIA_Q(CE_R_FIA_Q_ALT, SF_FIA_Q)

SF_SAMPLES_NO_FIA_Q_ALT = sum(SF_FIA_Q$NUMBER_OF_SAMPLES)  
SF_SAMPLES_NO_YQMFG = sum(SF_YQMFG$NUMBER_OF_SAMPLES)  

SF_SAMPLES_NO = SF_SAMPLES_NO_FIA_Q

# A first diagnostic

if(SF_SAMPLES_NO_FIA_Q != SF_SAMPLES_NO_YQMFG) {
  l_warn(paste0("The number of samples aggregated by Fishery, Area and Quarter (", SF_SAMPLES_NO_FIA_Q, ") differs from ",
                "the number of samples aggregated by Year, Quarter, Month, Fleet, Gear and fishing ground (", SF_SAMPLES_NO_YQMFG, ") - ",
                "difference = ", ( SF_SAMPLES_NO_FIA_Q - SF_SAMPLES_NO_YQMFG )))
}

# If everything went well, outputs the data as a .csv file that corresponds to the SA_Samples worksheet of the assessment input file
write.csv(SA_SAMPLES_FIA_Q_ALT, file = output_folder(SPECIES, LOCAL_FOLDER, "raised_with_samples/SA_SAMPLES_alt.csv"), row.names = FALSE, na = "")

##################################################################################
#### Produces 'RAW' SA samples by Year, Quarter, Month, Fleet, Gear, and grid ####
##################################################################################

### This only partially works as expected, as by merging CE (raised) with SF (5x5 grid) by year, quarter, month, fleet, 
### gear, school type and fishing ground, several SF data are dropped, therefore yielding a final result that includes
### far less samples than it should

### THE FOLLOWING IS LEFT HERE ONLY AS A REFERENCE ###

l_info("###### Preparing SA raised catches and raw samples by Y / Q / M / F / G / S / FG...")

# Merges the CE_raised and SF_<species code>

SA_SAMPLES_YQMFG = merge(sanitize_duplicated_SF_areas(CE_R_YQMFG), 
                         SF_YQMFG, 
                         by = c("YEAR", "QUARTER", "MONTH",
                                "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE",
                                "FISHING_GROUND_CODE"),
                         all.x = TRUE, 
                         allow.cartesian = TRUE)

setcolorder(
  SA_SAMPLES_YQMFG, 
  c("YEAR", "QUARTER", "MONTH",
    "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE",
    "FISHING_GROUND_CODE", "SF_AREA",
    "FIRST_CLASS_LOW", "SIZE_INTERVAL", "NUMBER_OF_SAMPLES", 
    "EST_NO", "EST_MT")
)

SA_SAMPLES_YQMFG[is.na(NUMBER_OF_SAMPLES), `:=`(NUMBER_OF_SAMPLES = 0,
                                                FIRST_CLASS_LOW = DEFAULT_FIRST_CLASS_LOW, # This constant is species specific, and included with the species configuration
                                                SIZE_INTERVAL   = DEFAULT_SIZE_INTERVAL    # This constant is species specific, and included with the species configuration
                                           )]

### Not saving this file, as it is basically useless considering the constraints it is built upon

# write.csv(SA_SAMPLES_YQMFG, file = output_folder(SPECIES, LOCAL_FOLDER, "raised_with_samples/SA_SAMPLES_raw.csv"), row.names = FALSE, na = "")
