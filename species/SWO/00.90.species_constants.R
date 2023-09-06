#WP_CURRENT    = "2022-pre"
#LOCAL_FOLDER  = "WPB19-PRE"
#REMOTE_FOLDER = "WPB19 - PRE" # TWN LL and Logbook SF data (2003+) removed, LLOB data all kept

#WP_CURRENT    = "2023-tcac"
#LOCAL_FOLDER  = "TCAC11"
#REMOTE_FOLDER = "TCAC11"

#WP_CURRENT    = "2023-05"
#LOCAL_FOLDER  = "2023-05"
#REMOTE_FOLDER = "2023_05_25" 

WP_CURRENT    = "WPB21"
LOCAL_FOLDER  = "WPB21"
REMOTE_FOLDER = "2023_05_25" 

SA_MAIN_FILE  = "WPB_SWO_SA(SS3).accdb"

# L-W conversion : Length-weight relationships for swordfish from "Data from the Atlantic Ocean, Spanish longline fishery (Mejuto et al., 1988, ICCAT)"
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Same equations For PS / PL / GI and LL / OT
                   A = c(0.000004203, 0.000004203), 
                   B = c(3.213400000, 3.213400000),
                   M = c(1.000000000, 1.000000000))

# Age-Length slicing method
AL_METHOD = "DMSP2"

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 
DEFAULT_SIZE_INTERVAL   =   3
DEFAULT_FIRST_CLASS_LOW =  15
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2011", 
                "2012", 
                "2014", 
                "2017",
                #"2018",
                "2023-tcac",
                "WPB21")

WPS_RECENT_FACTORS = c("2011", "2012", "2014", "2017", 
                       #"2018", 
                       "2023-tcac", "WPB21")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c() 