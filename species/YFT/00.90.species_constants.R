WP_CURRENT    = "2022-dp"
LOCAL_FOLDER  = "WPTT24-DP"
REMOTE_FOLDER = "WPTT24_DP" 

SA_MAIN_FILE  = "WPTT_YFT_SA(MFCL).mdb"

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_A = 0.00002459
LW_B = 2.96670

# Age-Length slicing method
AL_METHOD = "SLAF2"

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   2
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2018", "2019", "2022-dp")

WPS_RECENT_FACTORS = c("2016", "2018", "2019", "2022-dp")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c("HD", "TR", "OT") 