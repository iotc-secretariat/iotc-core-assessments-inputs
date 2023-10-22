WP_CURRENT    = "2023-as"
LOCAL_FOLDER  = "WPTT25-AS"
REMOTE_FOLDER = "WPTT25_AS"

# WAS:
#WP_CURRENT    = "2023-TCAC"
#LOCAL_FOLDER  = "TCAC11"
#REMOTE_FOLDER = "TCAC11" # TWN LL and Logbook SF data (2003+) removed, LLOB data all kept

SA_MAIN_FILE  = "WPTT_BET_SA(SS3).mdb"

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Different equations For PS / PL / GI and LL / OT
                   A = c(0.0000221700, 0.000015920700), 
                   B = c(3.0121100000, 3.041541402313),
                   M = c(1.0000000000, 1.130000000000))

# Age-Length slicing method
AL_METHOD = "DMPAB"

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   2
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c(#"2008", 
                "2009", 
                "2010", 
                "2011",
                "2012",
                "2013",
                "2014",
                "2016",
                "2019",
                "2022-dp",
                "2022-as",
                "2023-tcac",
                "2023-dp",
                "2023-as")

WPS_RECENT_FACTORS = c("2014", "2016", "2019", "2022-dp", "2022-as", "2023-tcac", "2023-dp", "2023-as")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c() 