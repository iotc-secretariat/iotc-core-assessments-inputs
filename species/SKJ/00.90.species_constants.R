WP_CURRENT    = "2023-dp"
LOCAL_FOLDER  = "WPTT25-DP"
REMOTE_FOLDER = "WPTT25_DP"

SA_MAIN_FILE  = "WPTT_SKJ_SA(SS3).mdb"

########################################
# TO BE UPDATED WITH SKJ SPECIFIC DATA #
########################################

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Different equations For PS / PL / GI and LL / OT - BUT NOT IN THE CASE OF SKJ, HENCE THE VALUES ARE THE SAME
                   A = c(0.0000049700, 0.0000049700), 
                   B = c(3.3929200000, 3.3929200000),
                   M = c(1.0000000000, 1.0000000000))

# Age-Length slicing method
AL_METHOD = NA # NO CAA is produced for SKJ...

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   1
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c(#"2010", 
                "2011", 
                "2012", 
                "2013", 
                "2014", 
                "2016", 
                "2017-old", 
                "2017", 
                "2022-dp",
                "2022-as",
                "2023-tcac",
                "2023-dp")

WPS_RECENT_FACTORS = c("2014", 
                       "2016", 
                       "2017-old", 
                       "2017", 
                       "2022-dp", 
                       "2022-as",
                       "2023-tcac",
                       "2023-dp")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c() 