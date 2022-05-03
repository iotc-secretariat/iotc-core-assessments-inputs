WP_CURRENT    = "2022-dp"
LOCAL_FOLDER  = "WPTT24-DP"
REMOTE_FOLDER = "WPTT24_DP" # TWN LL and Logbook SF data (2003+) removed, LLOB data all kept

SA_MAIN_FILE  = "WPTT_BET_SA(SS3).mdb"

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_A = 0.00002217
LW_B = 3.01211

# Age-Length slicing method
AL_METHOD = "DMPAB"

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   2
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2016", "2019")

WPS_RECENT_FACTORS = c("2016", "2019")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c("OT1", "OT2", "OT3", "OT4")