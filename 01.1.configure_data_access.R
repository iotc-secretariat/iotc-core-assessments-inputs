IN   = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", REMOTE_FOLDER, "/", CAS_IN_FILE))
PROC = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", REMOTE_FOLDER, "/", CAS_PROC_FILE))
OUT  = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", REMOTE_FOLDER, "/", CAS_OUT_FILE))

#SA   = connect_to(paste0(SA_FILES_PREFIX, SPECIES, "/", REMOTE_FOLDER, "/", SA_MAIN_FILE))  # Deprecated
AL   = connect_to(paste0(SA_FILES_PREFIX, SPECIES, "/", REMOTE_FOLDER, "/", SA_AGELEN_FILE))
