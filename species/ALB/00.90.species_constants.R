WP_CURRENT    = "2022-as"
LOCAL_FOLDER  = "WPTmT08-AS"
REMOTE_FOLDER = "WPTmT08_Assessment - TBD" # TWN LL and Logbook SF data (2003+) removed, LLOB data all kept

SA_MAIN_FILE  = "WPTmT_ALB_SA(SS3).accdb"

# L-W conversion
LW_A = 0.0000137180
LW_B = 3.09730

# Age-Length slicing method
AL_METHOD = "SLWE1"

# Output production
DEFAULT_NUM_SIZE_BINS   = 110 

DEFAULT_SIZE_INTERVAL   =   1
DEFAULT_FIRST_CLASS_LOW =  30
DEFAULT_LAST_CLASS_LOW  =  DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2008", "2010", "2011", "2012", "2013", "2014", 
                "2016", 
                "2019-p", 
                "2019-a1", "2019-a2", "2019-a3",
                "2022-dp", "2022-as")

WPS_RECENT_FACTORS = c("2016", "2019-a1", "2019-a2", "2019-a3", "2022-dp", "2022-as")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c("OT1", "OT2", "OT3", "OT4")

