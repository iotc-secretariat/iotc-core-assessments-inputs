### SA_CAS table (in numbers)

print("###### SA_CAS (no) ######")

# CE_R_xyz contains the standardized raised catches in numbers and weight by year, quarter and criteria 
# (either fishery + area, or month + fleet + gear + school type + fishing ground)

# CE_R_FIA_Q is created in 03.2.prepare_inputs_SA_CE_raised.R
# CE_R_YQMFG is created in 03.2.prepare_inputs_SA_CE_raised.R

# CE_SF_YQFG contains the estimated size distribution in numbers by size class, year, quarter, fleet, gear, school type and SF area

# CE_SF_YQFG is created in 03.3.prepare_inputs_SA_CE_for_SF.R

print("###### Merging raised catches and estimated samples by size bins by Y / Q / M / F / G / S / FG...")

merged_CAS   = merge_catches_and_quarterly_CAS(CE_R_YQMFG, CE_SF_YQFG)

print("###### Preparing CAS (in numbers) by Y / Q / M / F / G / S / FG...")

CAS_YQMFG   = prepare_SA_CAS_YQMFG(merged_CAS)

write.csv(CAS_YQMFG,   file = output_folder(SPECIES, LOCAL_FOLDER, "CAS/number/SA_CAS_raw.csv"), na = "", row.names = FALSE)

print("###### Preparing CAA (in numbers) by FI / A / Y / Q...")

# SF_FIA_Q is created in 03.5.prepare_inputs_IOTDB_SF.R
CAS_FIA_Q   = prepare_SA_CAS_FIA_Q(merged_CAS,   SF_FIA_Q)

write.csv(CAS_FIA_Q,   file = output_folder(SPECIES, LOCAL_FOLDER, "CAS/number/SA_CAS.csv"), na = "", row.names = FALSE)