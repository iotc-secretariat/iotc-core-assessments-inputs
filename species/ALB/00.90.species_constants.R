#WP_CURRENT    = "2022-as"
#LOCAL_FOLDER  = "WPTmT08-AS"
#REMOTE_FOLDER = "WPTmT08_Assessment - TBD" # TWN LL and Logbook SF data (2003+) removed, LLOB data all kept

# WP_CURRENT    = "2023-tcac12"
# LOCAL_FOLDER  = "TCAC12"
# REMOTE_FOLDER = "TCAC12v2" 

WP_CURRENT      = "2025-as"
LOCAL_FOLDER    = "WPTmT09-AS"  # name of the folder in the local project repository
REMOTE_FOLDER   = "WPTmT08_DP"  # name of the folder on the IOTC server IOTCS08 (z:)

# SA_MAIN_FILE  = "WPTmT_ALB_SA(SS3).accdb"   # commented by manu on 2025-04-10 to check if deprecated

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Different equations For PS / PL / GI and LL / OT
                   #A = c(0.0000137180, 0.0000137180), 
                   #B = c(3.0973000000, 3.0973000000),
                   #M = c(1.0000000000, 1.0000000000)) # No diffs between gears in ALB L-W equations 
                   A = c(0.0000569070, 0.0000569070), 
                   B = c(2.7514000000, 2.7514000000),
                   M = c(1.0000000000, 1.0000000000)) # No diffs between gears in ALB L-W equations 
                   

# Age-Length slicing method
AL_METHOD = "SLWE1"

# Output production
DEFAULT_NUM_SIZE_BINS   = 110 

DEFAULT_SIZE_INTERVAL   =   1
DEFAULT_FIRST_CLASS_LOW =  30
DEFAULT_LAST_CLASS_LOW  =  DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2022-as", 
                "2022-dp", 
                "2019-a3", "2019-a2", "2019-a1", "2019-p", 
                "2016", 
                "2014", 
                "2013", 
                "2012", 
                "2011", 
                "2010", 
                "2008")
                

WPS_RECENT_FACTORS = c("2022-as", 
                       "2022-dp", 
                       "2019-a3", "2019-a2", "2019-a1")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c("OT1", "OT2", "OT3", "OT4", "PS2", "PS3", "PS4")
