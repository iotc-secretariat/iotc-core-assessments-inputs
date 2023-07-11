WP_CURRENT    = "2023-dp"
LOCAL_FOLDER  = "WPTT25-DP"
REMOTE_FOLDER = "WPTT25_DP" 

# WAS:

#WP_CURRENT    = "2023-tcac"
#LOCAL_FOLDER  = "TCAC11"
#REMOTE_FOLDER = "TCAC11" 

SA_MAIN_FILE  = "WPTT_YFT_SA(MFCL).mdb"

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Different equations For PS / PL / GI and LL / OT
                   A = c(0.00002459, 0.0000094007), 
                   B = c(2.96670000, 3.1268439870),
                   M = c(1.00000000, 1.1300000000))

# Age-Length slicing method
AL_METHOD = "SLAF2"

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   2
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2010", 
                "2011", 
                "2012",
                "2013",
                "2014",
                "2015",
                "2016",
                "2018",
                "2019",
                "2022-dp",
                "2022-as",
                "2023-tcac",
                "2023-dp")

WPS_RECENT_FACTORS = c("2016",
                       "2018",
                       "2019", 
                       "2022-dp",
                       "2022-as",
                       "2023-tcac",
                       "2023-dp")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c("HD", "TR", "OT") 