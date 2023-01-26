#WP_CURRENT    = "2022-pre"
#LOCAL_FOLDER  = "WPB19-PRE"
#REMOTE_FOLDER = "WPB19 - PRE" # TWN LL and Logbook SF data (2003+) removed, LLOB data all kept

WP_CURRENT    = "2023-tcac"
LOCAL_FOLDER  = "TCAC11"
REMOTE_FOLDER = "TCAC11"

SA_MAIN_FILE  = "WPB_SWO_SA(SS3).accdb"

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Different equations For PS / PL / GI and LL / OT
                   A = c(0.000004203, 3.2134000000), 
                   B = c(0.000004203, 3.2134000000),
                   M = c(1.000000000, 1.0000000000))

# Age-Length slicing method
AL_METHOD = "DMSP2"

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   2
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2011", 
                "2012", 
                "2014", 
                "2017",
                "2018",
                "2023-tcac")

WPS_RECENT_FACTORS = c("2011", "2012", "2014", "2017", "2018", "2023-tcac")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c() 