AVG_WEIGHT_CHART_WIDTH  = 10
AVG_WEIGHT_CHART_HEIGHT = 5.5

# Average annual weight (all fisheries combined)
SA_AVG_WEIGHT = sanitize_duplicated_SF_areas(CE_R_YQMFG)[, .(AVG_WEIGHT = sum(EST_MT, na.rm = TRUE) * 1000 / sum(EST_NO, na.rm = TRUE)), keyby = .(YEAR)]

# Assigns the label for the WP and reorders columns
SA_AVG_WEIGHT$WP = WP_CURRENT

setcolorder(SA_AVG_WEIGHT, c("WP", "YEAR", "AVG_WEIGHT"))

# Writes data as a CSV file
write.csv(CE_R_YQMFG[, .(AVG_WEIGHT = sum(EST_MT, na.rm = TRUE) * 1000 / sum(EST_NO, na.rm = TRUE)), keyby = .(YEAR)], 
          file = output_folder(SPECIES, LOCAL_FOLDER, "avg_weight/SA_AVG_WEIGHT.csv"), row.names = FALSE, na = "")

# Reads the historical average annual weights (all fisheries combined) from previous WP
SA_AVG_WEIGHT_HISTORICAL = fread(references_folder(SPECIES, LOCAL_FOLDER, "SA_AVG_WEIGHT_HISTORY.csv"), header = TRUE)

# Melts the data by WP and year
SA_AVG_WEIGHT_HISTORICAL = 
  melt.data.table(data = SA_AVG_WEIGHT_HISTORICAL, 
                  value.name = "AVG_WEIGHT", 
                  variable.name = "YEAR",
                  id.vars = c("WP"), 
                  na.rm = TRUE)

SA_AVG_WEIGHT_HISTORICAL[, WP := str_trim(as.character(WP))]

# Adds in latest data
SA_AVG_WEIGHT_HISTORICAL = rbind(SA_AVG_WEIGHT_HISTORICAL, SA_AVG_WEIGHT)

SA_AVG_WEIGHT_HISTORICAL$WP = factor(
  SA_AVG_WEIGHT_HISTORICAL$WP,
  levels = WPS_FACTORS,
  labels = WPS_FACTORS,
  ordered = TRUE
)

# Plots the average annual weight (all fisheries combined) for each of the most recent WPs
FILTERED_AVG_WEIGHTS = 
  SA_AVG_WEIGHT_HISTORICAL[WP %in% WPS_RECENT_FACTORS]

W_F = lighten(ALL_FI_COLORS[FISHERY_CODE == "LLD"]$FILL, amount = 0.8)
W_L =  darken(ALL_FI_COLORS[FISHERY_CODE == "LLD"]$FILL, amount = 0.2)

W_COLORS = data.table(FILL = colorRampPalette(c(W_F, W_L))(length(unique(FILTERED_AVG_WEIGHTS$WP))))
W_COLORS[, OUTLINE := darken(FILL, amount = 0.4)]

FILTERED_AVG_WEIGHTS$WP = factor(
  FILTERED_AVG_WEIGHTS$WP,
  levels = sort(unique(FILTERED_AVG_WEIGHTS$WP)),
  labels = sort(unique(FILTERED_AVG_WEIGHTS$WP)),
  ordered = TRUE
)

FILTERED_AVG_WEIGHTS$YEAR = as.integer(as.character(FILTERED_AVG_WEIGHTS$YEAR))

SA_AVG_WEIGHT_HISTORICAL_PLOT = 
  line.value(
    FILTERED_AVG_WEIGHTS,
    value = "AVG_WEIGHT",
    time = "YEAR",
    color_by = "WP",
    colors = W_COLORS,
    plot_points = TRUE,
    num_legend_rows = 1,
    x_axis_label = "Year",
    y_axis_label = "Average fish weight (kg)"
  )

ggsave(SA_AVG_WEIGHT_HISTORICAL_PLOT, filename = output_folder(SPECIES, LOCAL_FOLDER, "avg_weight/AVG_WEIGHT_HISTORICAL.png"), width = AVG_WEIGHT_CHART_WIDTH, height = AVG_WEIGHT_CHART_HEIGHT)
  
SA_AVG_WEIGHT_HISTORICAL_PIVOT_TABLE =
  dcast.data.table(
    SA_AVG_WEIGHT_HISTORICAL,
    WP ~ YEAR,
    value.var = "AVG_WEIGHT",
    fun.aggregate = max,
    fill = NA,
    drop = c(FALSE, FALSE)
  )[order(-WP)]

write.csv(SA_AVG_WEIGHT_HISTORICAL_PIVOT_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "avg_weight/AVG_WEIGHT_HISTORICAL.csv"), na = "", row.names = FALSE)

# Computes the average annual weight by fishery
SA_AVG_WEIGHT_FISHERY = CE_R_FIA_Q[, .(AVG_WEIGHT = sum(EST_MT, na.rm = TRUE) * 1000 / sum(EST_NO, na.rm = TRUE)), keyby = .(YEAR, FISHERY)]

SA_AVG_WEIGHT_FISHERY_PIVOT_TABLE =
  dcast.data.table(
    SA_AVG_WEIGHT_FISHERY,
    FISHERY ~ YEAR,
    value.var = "AVG_WEIGHT",
    fun.aggregate = max,
    fill = NA,
    drop = c(FALSE, FALSE)
  )[order(+FISHERY)]

write.csv(SA_AVG_WEIGHT_FISHERY_PIVOT_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "avg_weight/AVG_WEIGHT_BY_FISHERY.csv"), na = "", row.names = FALSE)

# Plots the average annual weight by fishery for the current WP
SA_AVG_WEIGHT_FISHERY_PLOT = 
  line.value(
    SA_AVG_WEIGHT_FISHERY[!FISHERY %in% AVG_WEIGHT_FISHERIES_TO_EXCLUDE],
    value = "AVG_WEIGHT",
    time = "YEAR",
    num_legend_rows = 1,
    color_by = "FISHERY",
    colors = FI_COLORS[!FISHERY_CODE %in% AVG_WEIGHT_FISHERIES_TO_EXCLUDE],
    plot_points = TRUE,
    x_axis_label = "Year",
    y_axis_label = "Average fish weight (kg)"
  )

ggsave(SA_AVG_WEIGHT_FISHERY_PLOT, filename = output_folder(SPECIES, LOCAL_FOLDER, "avg_weight/AVG_WEIGHT_BY_FISHERY.png"), width = AVG_WEIGHT_CHART_WIDTH, height = AVG_WEIGHT_CHART_HEIGHT)
