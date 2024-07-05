IOTDB = DB_IOTDB()
IOTC  = DB_IOTCSTATISTICS()

# loads the shapefiles of the IOTC competence area
IO_AREAS = iotc.core.gis.wkt::fishing_grounds_data(c("IRFAO51", "IRFAO57"), connection = IOTC)

# loads all standard IOTC libs fishery colors
ALL_FI_COLORS = iotc.core.utils.aes::all_fishery_colors()

REMOTE_FOLDER_BASE_PREFIX = "Z:/processes/SA"

CAS_FILES_PREFIX = paste0(REMOTE_FOLDER_BASE_PREFIX, "/CAS/")
SA_FILES_PREFIX  = paste0(REMOTE_FOLDER_BASE_PREFIX, "/Assessment/")

CAS_IN_FILE    = "CAS_ProcessINPUTTAB.mdb"
CAS_PROC_FILE  = "CAS_Process.mdb"
CAS_OUT_FILE   = "CAS_ProcessOUTPUTTAB.mdb"

SA_MAIN_FILE   = "WPTT26_DP" # TO BE RE-DEFINED BY EACH SPECIES (currently) - For ALB = "WPTmT_ALB_SA(SS3).accdb"
SA_AGELEN_FILE = "AGELENGTH.mdb"