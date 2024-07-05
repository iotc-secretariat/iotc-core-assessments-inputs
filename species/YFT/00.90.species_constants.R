WP_CURRENT    = "2024-WPTT26-DP"
LOCAL_FOLDER  = "WPTT26-DP"
REMOTE_FOLDER = "WPTT26_DP" 

#SA_MAIN_FILE  = "WPTT_YFT_SA(MFCL).mdb"   # Deprecated

# L-W conversion : Length-weight relationships for tropical tunas caught with purse seine in the Indian Ocean: Update and lessons learned (Chassot, E. et al in IOTC-2016-WPDSC12-INF05)
LW_EQ = data.table(FISHERY_TYPE = c("PSPLGI", "LLOT"), # Different equations For PS / PL / GI and LL / OT
                   A = c(0.00002459, 0.0000094007), 
                   B = c(2.96670000, 3.1268439870),
                   M = c(1.00000000, 1.1300000000))

# Age-Length slicing method
AL_METHOD = "SLAF2"    # To check with DAN. The AL key should be updated according to the new curve

# Output production
DEFAULT_NUM_SIZE_BINS   = 150 

DEFAULT_SIZE_INTERVAL   =   2
DEFAULT_FIRST_CLASS_LOW =  10
DEFAULT_LAST_CLASS_LOW  = DEFAULT_FIRST_CLASS_LOW + ( DEFAULT_NUM_SIZE_BINS - 1 ) * DEFAULT_SIZE_INTERVAL

# WPS_FACTORS = c("2010", 
#                 "2011", 
#                 "2012",
#                 "2013",
#                 "2014",
#                 "2015",
#                 "2016",
#                 "2018",
#                 "2019",
#                 "2022-dp",
#                 "2022-as",
#                 "2023-tcac",
#                 "2023-dp",
#                 "2023-as")

WPS_FACTORS = c("2008-WPTT10", 
                "2009-WPTT11", 
                "2010-WPTT12", 
                "2011-WPTT13", 
                "2012-WPTT14", 
                "2013-WPTT15", 
                "2013-WPTT15-ALT", 
                "2018-WPTT20", 
                "2019-WPTT21", 
                "2021-WPTT23-AS", 
                "2024-WPTT26-DP")

# WPS_RECENT_FACTORS = c("2016",
#                        "2018",
#                        "2019", 
#                        "2022-dp",
#                        "2022-as",
#                        "2023-tcac",
#                        "2023-dp",
#                        "2023-as")

WPS_RECENT_FACTORS = c("2018-WPTT20", 
                "2019-WPTT21", 
                "2021-WPTT23-AS", 
                "2024-WPTT26-DP")

AVG_WEIGHT_FISHERIES_TO_EXCLUDE = c("HD", "TR", "OT") 
