# Color constants for current assessment areas

AR_COLOR_0 = ALL_FI_COLORS[FISHERY_CODE == "BB"]   # Area 0
AR_COLOR_1 = ALL_FI_COLORS[FISHERY_CODE == "GN"]   # Area 1
AR_COLOR_2 = ALL_FI_COLORS[FISHERY_CODE == "LLF"]  # Area 2
AR_COLOR_3 = ALL_FI_COLORS[FISHERY_CODE == "PSLS"] # Area 3

AR_COLORS = AR_COLOR_0
AR_COLORS = rbind(AR_COLORS, AR_COLOR_1)
AR_COLORS = rbind(AR_COLORS, AR_COLOR_2)
AR_COLORS = rbind(AR_COLORS, AR_COLOR_3)

# Color constants for current fishery groups

FG_COLORS = ALL_FI_COLORS[FISHERY_CODE == "PSFS"]                     # PS fishery group
FG_COLORS = rbind(FG_COLORS, ALL_FI_COLORS[FISHERY_CODE == "LLF"])    # FL
FG_COLORS = rbind(FG_COLORS, ALL_FI_COLORS[FISHERY_CODE == "LLD"])    # LL
FG_COLORS = rbind(FG_COLORS, ALL_FI_COLORS[FISHERY_CODE == "BB"])     # BB
FG_COLORS = rbind(FG_COLORS, ALL_FI_COLORS[FISHERY_CODE == "LIT"])    # LI
FG_COLORS = rbind(FG_COLORS, ALL_FI_COLORS[FISHERY_CODE == "OT"])    # LI

# Color constants for current fisheries
ALL_FI_COLORS = all_fishery_colors()

FI_COLORS_TEMP = data.table(FISHERY_CODE = character(), FILL = character(), OUTLINE = character())

for(fishery in c("PSFS", "PSLS", "LLD", "LLF", "BB", "LI", "OT")) {
  for(area in c(0, 1, 2, 3)) {
    FI_COLORS_TEMP = rbind(FI_COLORS_TEMP, copy(ALL_FI_COLORS[FISHERY_CODE == fishery])[, FISHERY_CODE := paste0(fishery, area)])
  }
}

FI_COLORS = data.table(FISHERY_CODE = character(), FILL = character(), OUTLINE = character())

for(fishery in c("PSFS", "PSLS", "LLD", "LLF", "BB", "LI", "OT")) {
  for(area in c(0, 1, 2, 3)) {
    key = paste0(fishery, area)
    entry = FI_COLORS_TEMP[FISHERY_CODE == key]
    
    entry$FILL = darken(entry$FILL, amount = 0.2 * area)
    entry$OUTLINE = darken(entry$OUTLINE, amount = 0.2 * area)
    
    FI_COLORS = rbind(FI_COLORS, entry)
  }
}