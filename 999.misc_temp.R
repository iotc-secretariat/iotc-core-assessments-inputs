options(scipen = 9999)

load_0StrataSF = function(WP, species) {
  return(
    get_table(
      connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", WP,  "/", CAS_IN_FILE)), 
      "0StrataSF"
    )[Table == species][, .(Species = Table, Fleet, Gear, Year, MonthStart = MIS, MonthEnd = MIE, Delete)]
  )
}

load_SpeciesGearA = function(WP, species) {
  return(
    get_table(
      connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", WP,  "/", CAS_PROC_FILE)), 
      "0cdeSpeciesGearA"
    )[Species == species]
  )
}

load_SF = function(WP, species) {
  return(
    get_table(
      connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", WP,  "/", CAS_IN_FILE)), 
      paste0("SF", species)
    )
  )
}

load_CEForSF = function(WP, species) {
  return(
    get_table(
      connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", WP,  "/", CAS_OUT_FILE)), 
      "CEForSF"
    )[Species == species]
  )
}

load_CERaised = function(WP, species) {
  return(
    get_table(
      connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", WP,  "/", CAS_OUT_FILE)), 
      "CERaised"
    )[, `:=`(Fleet = str_trim(Fleet), Gear = str_trim(Gear))][Species == species]
  )
}

melt_SF = function(SF) {
  mSF = melt.data.table(SF, id.vars = 1:13, variable.name = "BIN", value.name = "NUM_FISH")[NUM_FISH > 0]
  mSF[, ClassLow := FirstClassLow + (((as.integer(str_sub(BIN, 2))) - 1) * SizeInterval)]
  mSF$BIN = NULL
  
  return(mSF[NUM_FISH > 0][, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType, ClassLow)])
}

melt_CEForSF = function(CEForSF) {
  mCEForSF = melt.data.table(CEForSF, id.vars = c("Fleet", "Year", "Gear", "Species", "SchoolType", "FirstClassLow", "SizeInterval"), 
                             variable.name = "BIN", value.name = "NUM_FISH", measure.vars = 27:176)
  mCEForSF = mCEForSF[, ClassLow := FirstClassLow + (((as.integer(str_sub(BIN, 2))) - 1) * SizeInterval)][, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, Species, SchoolType, ClassLow)]
  
  return(mCEForSF[NUM_FISH > 0, .(Fleet, Year, Gear, Species, SchoolType, ClassLow, NUM_FISH)])
}

normalize_SF_FYG = function(SF) {
  SF[, TOT := sum(NUM_FISH), by = .(Fleet, Year, Gear, SchoolType, MeasureType)]
  SF[, FISH_PERC := NUM_FISH / TOT]
  
  return(SF)
}

avg_weight_fleet = function(CERaised) {
  return(
    CERaised[, .(AVG_WEIGHT = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Fleet)]
  )
}

avg_weight_gear = function(CERaised) {
  return(
    CERaised[, .(AVG_WEIGHT = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Gear)]
  )
}

avg_weight = function(CERaised) {
  return(
    CERaised[, .(AVG_WEIGHT = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year)]
  )
}

faceted_SF_plot_year = function(SF) {
  return(
    ggplot(SF, mapping = aes(x = ClassLow, y = FISH_PERC)) + 
      geom_point(mapping = aes(group = Year)) + 
      geom_line() +
      facet_wrap(vars(Year), ncol = 4)
  )
}

faceted_SF_plot_fleet = function(SF) {
  return(
    ggplot(SF, mapping = aes(x = ClassLow, y = FISH_PERC, color = as.factor(Year))) + 
      geom_point(mapping = aes(group = Year)) + 
      geom_line() +
      facet_wrap(vars(Fleet), ncol = 4)
  )
}

faceted_SF_plot_gear = function(SF) {
  return(
    ggplot(SF, mapping = aes(x = ClassLow, y = FISH_PERC, color = as.factor(Year))) + 
      geom_point(mapping = aes(group = Year)) + 
      geom_line() +
      facet_wrap(vars(Gear), ncol = 4)
  )
}

faceted_AW_plot_fleet = function(CERaised) {
  return(
    ggplot(CERaised, mapping = aes(x = Year, y = AVG_WEIGHT)) + 
      geom_point() + 
      geom_line() +
      facet_wrap(vars(Fleet), ncol = 4)
  )
}

faceted_AW_plot_gear = function(CERaised) {
  return(
    ggplot(CERaised, mapping = aes(x = Year, y = AVG_WEIGHT)) + 
      geom_point() + 
      geom_line() +
      facet_wrap(vars(Gear), ncol = 4)
  )
}

AW_plot = function(CERaised) {
  return(
    ggplot(CERaised, mapping = aes(x = Year, y = AVG_WEIGHT)) + 
      geom_point() + 
      geom_line()
  )
}

SFALB2 = 
  normalize_SF_FYG(
    melt_SF(
      load_SF("TCAC12v2", "ALB")
    )
  )

CERaisedALB2 = load_CERaised("TCAC12v2 - Copy", "ALB")
CERaisedALB1 = load_CERaised("2023_05_25", "ALB")

faceted_AW_plot_gear(avg_weight_gear(CERaisedALB2[Fleet == "TWN"]))

AW_plot(avg_weight(CERaisedALB2[Gear == "LLCO"]))

AVG1 = avg_weight_gear(CERaisedALB1) #[, .(AVG_W = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Gear)]
AVG2 = avg_weight_gear(CERaisedALB2) #[, .(AVG_W = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Gear)]

AVG = merge(AVG2, AVG1, by = c("Year", "Gear"), all.x = TRUE, all.y = TRUE)[, .(YEAR = Year, GEAR = Gear, AW_TCAC12 = `AVG_WEIGHT.x`, AW_2023 = `AVG_WEIGHT.y`)]

AVG_d = melt(AVG, id.vars = 1:2, measure.vars = 3:4, variable.name = "WP", value.name = "AVG_WEIGHT")
ggplot(AVG_d[GEAR != "RR"], mapping = aes(x = YEAR, y = AVG_WEIGHT, color = WP)) + geom_point() + geom_line() + facet_wrap(vars(GEAR))


AW_plot(avg_weight(CERaisedALB2[Gear == "FLL"]))
AW_plot(avg_weight(CERaisedALB1[Gear == "FLL"]))

FOO = load_0StrataSF("TCAC12v2", "ALB")

faceted_SF_plot_gear(normalize_SF_FYG(SFALB2[Year >= 2015]))
faceted_SF_plot_fleet(normalize_SF_FYG(SFALB2[Year >= 2015 & Gear == "LLCO"]))

faceted_plot_fleet(SFALB2[Gear %in% c("PS")])

ggplot(mSFALB_2[Gear %in% c("TROL", "TROLM", "TROLN")], 
       mapping = aes(x = ClassLow, y = FISH_PERC, color = as.factor(Year))) + 
  geom_point(mapping = aes(group = Year)) + 
  geom_line() + 
  facet_wrap(vars(Fleet), ncol = 4)


mSFALB_1y = mSFALB_1[, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType)]
#mSFALB_1  = mSFALB_1[, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType, ClassLow)]


PR_1  = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", "2023_05_25",  "/", CAS_PROC_FILE))
PR_2  = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", "TCAC12v2", "/", CAS_PROC_FILE))

IN_1  = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", "2023_05_25",  "/", CAS_IN_FILE))
IN_2  = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", "TCAC12v2", "/", CAS_IN_FILE))

OUT_1 = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", "2023_05_25",  "/", CAS_OUT_FILE))
OUT_2 = connect_to(paste0(CAS_FILES_PREFIX, SPECIES, "/", "TCAC12v2", "/", CAS_OUT_FILE))

SPG_1 = get_table(PR_1, "0cdeSpeciesGearA")
SPG_2 = get_table(PR_2, "0cdeSpeciesGearA")

#sort(unique(SPG_1$Fleet)) # Contains records with leading / trailing spaces in Fleet,
#sort(unique(SPG_2$Fleet)) # Contains records with leading / trailing spaces in Fleet,

SPG_1[, Fleet := str_trim(Fleet)]
SPG_2[, Fleet := str_trim(Fleet)]

#sort(unique(SPG_1$Gear))
#sort(unique(SPG_2$Gear))

SFALB_1 = get_table(IN_1, "SFALB")
SFALB_2 = get_table(IN_2, "SFALB")

#sort(unique(SFALB_1$Fleet))
#sort(unique(SFALB_2$Fleet))

#sort(unique(SFALB_1$Gear))
#sort(unique(SFALB_2$Gear))

CEForSF1 = get_table(OUT_1, "CEForSF")
CEForSF2 = get_table(OUT_2, "CEForSF")

#sort(unique(CEForSF1$Fleet)) # Contains records with leading / trailing spaces in Fleet,
#sort(unique(CEForSF2$Fleet)) # Contains records with leading / trailing spaces in Fleet,

CEForSF1[, Fleet := str_trim(Fleet)]
CEForSF2[, Fleet := str_trim(Fleet)]

#sort(unique(CEForSF1$Gear))
#sort(unique(CEForSF2$Gear))

StrataSF1 = get_table(IN_1, "0StrataSF")
StrataSF2 = get_table(IN_2, "0StrataSF")

#sort(unique(StrataSF1$Fleet))
#sort(unique(StrataSF2$Fleet))

#sort(unique(StrataSF1$Gear))
#sort(unique(StrataSF2$Gear))

###

melt_SF = function(SF) {
  mSF = melt.data.table(SF, id.vars = 1:13, variable.name = "BIN", value.name = "NUM_FISH")[NUM_FISH > 0]
  mSF[, ClassLow := FirstClassLow + (((as.integer(str_sub(BIN, 2))) - 1) * SizeInterval)]
  mSF$BIN = NULL
  
  return(mSF[NUM_FISH > 0][, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType, ClassLow)])
}

mSFALB_1 = melt_SF(SFALB_1)
mSFALB_1y = mSFALB_1[, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType)]
#mSFALB_1  = mSFALB_1[, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType, ClassLow)]
mSFALB_1[, TOT := sum(NUM_FISH), by = .(Fleet, Year, Gear, SchoolType, MeasureType)]
mSFALB_1[, FISH_PERC := NUM_FISH / TOT]

mSFALB_2 = melt_SF(SFALB_2)
mSFALB_2y = mSFALB_2[, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType)]
#mSFALB_2  = mSFALB_2[, .(NUM_FISH = sum(NUM_FISH)), keyby = .(Fleet, Year, Gear, SchoolType, MeasureType, ClassLow)]
mSFALB_2[, TOT := sum(NUM_FISH), by = .(Fleet, Year, Gear, SchoolType, MeasureType)]
mSFALB_2[, FISH_PERC := NUM_FISH / TOT]

mSFALB_all = merge(mSFALB_2y, mSFALB_1y, by = c("Fleet", "Year", "Gear", "SchoolType", "MeasureType"), all.x = TRUE)
mSFALB_all[Year >= 2018 & is.na(NUM_FISH.y)][order(-Year, Fleet, Gear)]

ggplot(mSFALB_2[Gear == "FLL"], mapping = aes(x = ClassLow, y = FISH_PERC, color = Year)) +  geom_point(mapping = aes(group = Year)) + facet_wrap(vars(Fleet), ncol = 4)
ggplot(mSFALB_2[Gear == "LL" ], mapping = aes(x = ClassLow, y = FISH_PERC, color = Year)) +  geom_point(mapping = aes(group = Year)) + facet_wrap(vars(Fleet), ncol = 4)

faceted_plot_year = function(data) {
  ggplot(data, 
         mapping = aes(x = ClassLow, y = FISH_PERC)) + 
    geom_point(mapping = aes(group = Year)) + 
    geom_line() +
    facet_wrap(vars(Year), ncol = 4)
  
}


mCEForSF2 = melt_CEForSF(CEForSF2)
mCEForSF2 = mCEForSF2[, TOT := sum(NUM_FISH), by = .(Fleet, Year, Gear, Species, SchoolType)]
mCEForSF2 = mCEForSF2[, FISH_PERC := NUM_FISH / TOT]

mCEForSF1 = melt_CEForSF(CEForSF1)
mCEForSF1 = mCEForSF1[, TOT := sum(NUM_FISH), by = .(Fleet, Year, Gear, Species, SchoolType)]
mCEForSF1 = mCEForSF1[, FISH_PERC := NUM_FISH / TOT]

CERaised1 = get_table(OUT_1, "CERaised")
CERaised1[, `:=`(Fleet = str_trim(Fleet), Gear = str_trim(Gear))]

CERaised2 = get_table(OUT_2, "CERaised")
CERaised2[, `:=`(Fleet = str_trim(Fleet), Gear = str_trim(Gear))]

AVG1 = CERaised1[Species == "ALB", .(AVG_W = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Gear)]
AVG2 = CERaised2[Species == "ALB", .(AVG_W = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Gear)]

AVG = merge(AVG2, AVG1, by = c("Year", "Gear"), all.x = TRUE, all.y = TRUE)[, .(YEAR = Year, GEAR = Gear, AW_TCAC12 = `AVG_W.x`, AW_2023 = `AVG_W.y`)]

AVG_d = melt(AVG, id.vars = 1:2, measure.vars = 3:4, variable.name = "WP", value.name = "AVG_W")
ggplot(AVG_d[GEAR != "RR"], mapping = aes(x = YEAR, y = AVG_W, color = WP)) + geom_point() + geom_line() + facet_wrap(vars(GEAR))

AVG1F = CERaised1[Species == "ALB" & Gear == "PSS", .(AVG_W = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Fleet)]
AVG2F = CERaised2[Species == "ALB" & Gear == "PSS", .(AVG_W = round(sum(NCmtFish) * 1000 / sum(NCnoFish), 2)), keyby = .(Year, Fleet)]

AVGF = merge(AVG2F, AVG1F, by = c("Year", "Fleet"), all.x = TRUE, all.y = TRUE)[, .(YEAR = Year, FLEET = Fleet, AW_TCAC12 = `AVG_W.x`, AW_2023 = `AVG_W.y`)]

AVGF_d = melt(AVGF, id.vars = 1:2, measure.vars = 3:4, variable.name = "WP", value.name = "AVG_W")
ggplot(AVGF_d, mapping = aes(x = YEAR, y = AVG_W, color = WP)) + geom_point() + geom_line() + facet_wrap(vars(FLEET))

ALB_TROL_2 = ggplot(mSFALB_2[Gear %in% c("TROL", "TROLM", "TROLN")], 
                    mapping = aes(x = ClassLow, y = FISH_PERC, color = as.factor(Year))) + 
             geom_point(mapping = aes(group = Year)) + 
             geom_line() + 
             facet_wrap(vars(Fleet), ncol = 4)

ALB_PSSS_2 = ggplot(mSFALB_2[Gear %in% c("PSS")], 
                    mapping = aes(x = ClassLow, y = FISH_PERC, color = as.factor(Year))) + 
             geom_point(mapping = aes(group = Year)) + 
             geom_line() + 
             facet_wrap(vars(Fleet), ncol = 4)

