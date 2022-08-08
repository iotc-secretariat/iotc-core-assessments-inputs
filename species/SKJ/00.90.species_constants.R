WP_CURRENT    = "2022-dp"
LOCAL_FOLDER  = "WPTT24-DP"
REMOTE_FOLDER = "WPTT24_DP" # TWN LL and Logbook SF data (2003+) removed, LLOB data all kept

SA_MAIN_FILE  = "WPTT_SKJ_SA(SS3).mdb"

########################################
# TO BE UPDATED WITH SKJ SPECIFIC DATA #
########################################

print("!!! SKJ configuration still needs to be updated !!!")

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Different equations For PS / PL / GI and LL / OT - BUT NOT IN THE CASE OF SKJ, HENCE THE VALUES ARE THE SAME
                   A = c(0.0000049700, 0.0000049700), 
                   B = c(3.3929200000, 3.3929200000),
                   M = c(1.0000000000, 1.0000000000))

# Age-Length slicing method
AL_METHOD = "DMPAB"

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   1
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

WPS_FACTORS = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2016", "2019", "2022-dp")

WPS_RECENT_FACTORS = c("2014", "2016", "2019", "2022-dp")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c() 