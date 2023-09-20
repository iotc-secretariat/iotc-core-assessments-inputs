### SA_CAS table (in weight)

l_info("###### SA_CAS (t) ######")

# CE_R_xyz contains the standardized raised catches in numbers and weight by year, quarter and criteria 
# (either fishery + area, or month + fleet + gear + school type + fishing ground)

# CE_R_FIA_Q is created in 03.2.prepare_inputs_SA_CE_raised.R
# CE_R_YQMFG is created in 03.2.prepare_inputs_SA_CE_raised.R

# CE_SF_YQFG contains the estimated size distribution in numbers by size class, year, quarter, fleet, gear, school type and SF area

# CE_SF_YQFG is created in 03.3.prepare_inputs_SA_CE_for_SF.R

l_info("###### Converting fish lengths to weights...")

# LW_A and LW_B are configured at species' level and by type of gear (most of the times)

LW_EQ_PS_PL_GI = LW_EQ[FISHERY_TYPE == "PSPLGI"]
LW_EQ_LL_OT    = LW_EQ[FISHERY_TYPE == "LLOT"]

merged_CAS[GEAR_CODE %in% GEAR_PS_PL_GI$CODE, 
           FISH_WEIGHT := FISH_COUNT * LW_EQ_PS_PL_GI$M * LW_EQ_PS_PL_GI$A * ( SIZE_CLASS + SIZE_INTERVAL / 2 ) ^ LW_EQ_PS_PL_GI$B / 1000]
  
merged_CAS[!GEAR_CODE %in% GEAR_PS_PL_GI$CODE, 
           FISH_WEIGHT := FISH_COUNT * LW_EQ_LL_OT$M    * LW_EQ_LL_OT$A    * ( SIZE_CLASS + SIZE_INTERVAL / 2 ) ^ LW_EQ_LL_OT$B    / 1000]

runGC()

if(FALSE) { # Disabled - too much data to process, regular PCs won't handle it
  l_info("###### Preparing CAS (in weight) by Y / Q / M / F / G / S / FG...")
  
  CAS_YQMFG_W = prepare_SA_CAS_YQMFG(merged_CAS_W, "FISH_WEIGHT")

  write.csv(CAS_YQMFG_W, file = output_folder(SPECIES, LOCAL_FOLDER, "CAS/weight/SA_CAS_W_raw.csv"), na = "", row.names = FALSE)
}

l_info("###### Preparing CAS (in weight) by FI / A / Y / Q...")

# SF_FIA_Q is created in 03.5.prepare_inputs_IOTDB_SF.R
CAS_FIA_Q_W = prepare_SA_CAS_FIA_Q(merged_CAS, SF_FIA_Q, "FISH_WEIGHT")

write.csv(CAS_FIA_Q_W, file = output_folder(SPECIES, LOCAL_FOLDER, "CAS/weight/SA_CAS_W.csv"), na = "", row.names = FALSE)