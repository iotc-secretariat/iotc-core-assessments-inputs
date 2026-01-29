WP_CURRENT    = "2026-tcac"
LOCAL_FOLDER  = "TCAC16"
REMOTE_FOLDER = "TCAC16"

# SA_MAIN_FILE  = "WPB_SWO_SA(SS3).accdb"

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
                "WPB21",
#                "2023-tcac2", 
                "2026-tcac")

WPS_RECENT_FACTORS = c("2011", "2012", "2014", "2017", 
                       #"2018", 
                       "2023-tcac", "WPB21", "2026-tcac")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c()