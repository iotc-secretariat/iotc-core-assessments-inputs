### Production of samples' summary tables and charts

print("###### Producing samples' summaries ######")

# Requires the CAS in numbers
SA_CAS_NO = merged_CAS[, .(FISH_COUNT  = sum(FISH_COUNT,  na.rm = TRUE)), keyby = .(SIZE_CLASS)]
TOT_NO = sum(SA_CAS_NO$FISH_COUNT)
SA_CAS_NO[, `:=`(FRACTION = FISH_COUNT / TOT_NO, Source = "Catch-at-size (numbers)")]

# Requires the CAS in weight
SA_CAS_W  = merged_CAS[, .(FISH_WEIGHT = sum(FISH_WEIGHT, na.rm = TRUE)), keyby = .(SIZE_CLASS)]
TOT_W  = sum(SA_CAS_W$FISH_WEIGHT)
SA_CAS_W[, `:=`(FRACTION = FISH_WEIGHT / TOT_W, Source = "Catch-at-size (weight)")]

# Calculates original samples distribution (% fish by size class) from the original samples 
SA_SF = 
  size_class_from_size_bin(
    melt.data.table(
      SF_YQMFG,
      value.name = "FISH_COUNT",
      variable.name = "SIZE_BIN",
      id.vars = c("YEAR", "QUARTER", "MONTH", "FLEET", "GEAR_CODE", "SCHOOL_TYPE_CODE",
                  "FISHING_GROUND_CODE", "FIRST_CLASS_LOW", "SIZE_INTERVAL", "NUMBER_OF_SAMPLES")
    )[!is.na(FISH_COUNT)]
  )[, .(FISH_COUNT = sum(FISH_COUNT, na.rm = TRUE)), keyby = .(SIZE_CLASS)] 
  
TOT_SF = sum(SA_SF$FISH_COUNT)
SA_SF[, `:=`(FRACTION = FISH_COUNT / TOT_SF, Source = "Samples (numbers)")]

SAMPLES_BY_SIZE = SA_SF[, .(SIZE_CLASS, FRACTION, Source)]
SAMPLES_BY_SIZE = rbind(SAMPLES_BY_SIZE, SA_CAS_NO[, .(SIZE_CLASS, FRACTION, Source)])
SAMPLES_BY_SIZE = rbind(SAMPLES_BY_SIZE, SA_CAS_W[, .(SIZE_CLASS, FRACTION, Source)])

SAMPLES_BY_SIZE_PIVOT = 
  dcast.data.table(
    SAMPLES_BY_SIZE,
    value.var = "FRACTION",
    formula = Source ~ SIZE_CLASS,
    fun.aggregate = sum,
    drop = c(FALSE, FALSE)
  )

write.csv(SAMPLES_BY_SIZE_PIVOT, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_DISTRIBUTION.csv"), na = "", row.names = FALSE)

SAMPLES_BY_SIZE[, FRACTION := 100 * FRACTION]

COLOR_SAMPLES_NO = colors_for_fishery("GN")
COLOR_RAISED_NO  = colors_for_fishery("LLF")
COLOR_RAISED_W   = colors_for_fishery("LLF")

COLOR_SAMPLES_NO$FILL     = darken(COLOR_SAMPLES_NO$FILL, amount = 0.2)
COLOR_SAMPLES_NO$OUTLINE  = darken(COLOR_SAMPLES_NO$OUTLINE, amount = 0.2)

COLOR_RAISED_W$FILL    = lighten(COLOR_RAISED_W$FILL, amount = 0.6)
COLOR_RAISED_W$OUTLINE = lighten(COLOR_RAISED_W$OUTLINE, amount = 0.6)

SAMPLES_SOURCE_COLORS = COLOR_RAISED_NO
SAMPLES_SOURCE_COLORS = rbind(SAMPLES_SOURCE_COLORS, COLOR_RAISED_W)
SAMPLES_SOURCE_COLORS = rbind(SAMPLES_SOURCE_COLORS, COLOR_SAMPLES_NO)

SAMPLES_BY_SIZE_PLOT = 
  line.value(
    SAMPLES_BY_SIZE,
    time = "SIZE_CLASS",
    value = "FRACTION",
    color_by = "Source",
    x_axis_label = "Fork length (cm)",
    y_axis_label = "% of individuals",
    plot_points = TRUE,
    colors = SAMPLES_SOURCE_COLORS #color_table(unique_colors(3))
  )

ggsave(SAMPLES_BY_SIZE_PLOT, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_DISTRIBUTION.png"), width = 12, height = 6.75) 

SAMPLES_VS_ESTIMATED = CAS_FIA_Q_W[, .(Sampled = sum(NUMBER_OF_SAMPLES, na.rm = TRUE), 
                                       NotSampled = sum(EST_NO, na.rm = TRUE) - sum(NUMBER_OF_SAMPLES, na.rm = TRUE)), keyby = .(Year = YEAR)]
            
SAMPLES_VS_ESTIMATED_MELT =
  melt.data.table(
    SAMPLES_VS_ESTIMATED,
    value.name = "FISH_COUNT",
    variable.name = "TYPE",
    id.vars = "Year")[TYPE == "NotSampled", TYPE := "Not sampled"]

SAMPLES_VS_ESTIMATED_MELT$TYPE = factor(
  SAMPLES_VS_ESTIMATED_MELT$TYPE,
  labels = c("Not sampled", "Sampled"),
  levels = c("Not sampled", "Sampled"),
  ordered = TRUE
)

SAMPLES_VS_ESTIMATED_PIVOT =
  dcast.data.table(
    SAMPLES_VS_ESTIMATED_MELT[, .(Year, Type = TYPE, FISH_COUNT)],
    value.var = "FISH_COUNT",
    formula = Type ~ Year,
    fun.aggregate = sum,
    drop = c(FALSE, FALSE)
  )
                         
write.csv(SAMPLES_VS_ESTIMATED_PIVOT, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_BY_TYPE.csv"), na = "", row.names = FALSE)
                         
SAMPLES_TYPE_COLORS = colors_for_fishery_group("PS")
SAMPLES_TYPE_COLORS = rbind(SAMPLES_TYPE_COLORS, colors_for_fishery_group("LL"))

SAMPLES_BY_TYPE_PLOT = bar.value(
  SAMPLES_VS_ESTIMATED_MELT,
  time = "Year",
  value = "FISH_COUNT",
  scale = 1000000,
  fill_by = "TYPE",
  colors = SAMPLES_TYPE_COLORS,
  x_axis_label = "Year",
  y_axis_label = "Number of individuals (x 1,000,000)"
)

ggsave(SAMPLES_BY_TYPE_PLOT, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_BY_TYPE.png"), width = 12, height = 6.75) 

SAMPLES_BY_TYPE_PLOT_REL = bar.value.rel(
  SAMPLES_VS_ESTIMATED_MELT,
  time = "Year",
  value = "FISH_COUNT",
  fill_by = "TYPE",
  colors = SAMPLES_TYPE_COLORS,
  x_axis_label = "Year",
  y_axis_label = "% of individuals"
)

ggsave(SAMPLES_BY_TYPE_PLOT_REL, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_BY_TYPE_REL.png"), width = 12, height = 6.75) 

SAMPLES_EST_Y = SA_SAMPLES_FIA_Q    [, .(EST_NO = sum(EST_NO, na.rm = TRUE)), keyby = .(Year = YEAR)]
SAMPLES_ALT_Y = SA_SAMPLES_FIA_Q_ALT[, .(EST_NO = sum(EST_NO, na.rm = TRUE)), keyby = .(Year = YEAR)]

SAMPLES_EST_Y$Source = "Estimated"
SAMPLES_ALT_Y$Source = "Adjusted"

SAMPLES_Y = rbind(SAMPLES_EST_Y, SAMPLES_ALT_Y)

SAMPLES_Y_PIVOT = 
  dcast.data.table(
    SAMPLES_Y,
    Source ~ Year,
    fun.aggregate = sum,
    value.var = "EST_NO",
    drop = c(FALSE, FALSE)
  )

write.csv(SAMPLES_Y_PIVOT, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_BY_SOURCE.csv"), na = "", row.names = FALSE)

SAMPLES_Y_PLOT = 
  line.value(
    SAMPLES_Y,
    time = "Year",
    value = "EST_NO",
    scale = 1000000,
    color_by = "Source",
    plot_points = TRUE,
    colors = color_table(unique_colors(2)),
    x_axis_label = "Year",
    y_axis_label = "Number of fish (x 1,000,000)"
  )

ggsave(SAMPLES_Y_PLOT, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_BY_SOURCE.png"), width = 12, height = 6.75) 

YEARS = min(CAS_FIA_Q$YEAR):max(CAS_FIA_Q$YEAR)

ORIGINAL_SAMPLES_BY_FIAY =
  SF_FIA_Q[, .(NUMBER_OF_SAMPLES = sum(NUMBER_OF_SAMPLES, na.rm = TRUE)),
               keyby = .(FISHERY, AREA, YEAR)]

YFA = expand.grid(YEAR = YEARS, 
                  FISHERY = sort(unique(ORIGINAL_SAMPLES_BY_FIAY$FISHERY)))

ORIGINAL_SAMPLES_BY_FIAY = merge(YFA, ORIGINAL_SAMPLES_BY_FIAY,
                                 all.x = TRUE)

SAMPLES_AVAILABLE =
  dcast.data.table(
    as.data.table(ORIGINAL_SAMPLES_BY_FIAY),
    formula = FISHERY + AREA ~ YEAR,
    fun.aggregate = sum,
    value.var = "NUMBER_OF_SAMPLES",
    drop = c(TRUE, FALSE),
    fill = NA
  )[!is.na(AREA)]

write.csv(SAMPLES_AVAILABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_AVAILABLE.csv"), na = "", row.names = FALSE)

ESTIMATED_FISH_BY_FIAY =
  CAS_FIA_Q[, .(EST_NO = sum(EST_NO, na.rm = TRUE), EST_MT = sum(EST_MT, na.rm = TRUE)),
                keyby = .(FISHERY, AREA, YEAR)]

SAMPLES_ESTIMATED = 
  dcast.data.table(
    ESTIMATED_FISH_BY_FIAY,
    formula = FISHERY + AREA ~ YEAR,
    fun.aggregate = sum,
    value.var = "EST_NO",
    drop = c(TRUE, FALSE),
    fill = NA
  )

write.csv(SAMPLES_ESTIMATED, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_ESTIMATED.csv"), na = "", row.names = FALSE)

ORIGINAL_VS_ESTIMATED_FISH_BY_FIAY = 
  merge(ESTIMATED_FISH_BY_FIAY, ORIGINAL_SAMPLES_BY_FIAY,
        all.x = TRUE)

ORIGINAL_VS_ESTIMATED_FISH_BY_FIAY[!is.na(EST_NO), `:=`(COVERAGE   = ifelse(is.na(NUMBER_OF_SAMPLES), 0,     NUMBER_OF_SAMPLES / EST_NO),
                                                        COVERAGE_W = ifelse(is.na(NUMBER_OF_SAMPLES), FALSE, NUMBER_OF_SAMPLES >= EST_MT))][COVERAGE > 1, COVERAGE := 1]

ORIGINAL_VS_ESTIMATED_COVERAGE =
  dcast.data.table(
    ORIGINAL_VS_ESTIMATED_FISH_BY_FIAY,
    formula = FISHERY + AREA ~ YEAR,
    fun.aggregate = max,
    value.var = "COVERAGE",
    drop = c(TRUE, FALSE),
    fill = NA
  )[!is.na(AREA)]

write.csv(ORIGINAL_VS_ESTIMATED_COVERAGE, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_COVERAGE.csv"), na = "", row.names = FALSE)

MINIMUM_COVERAGE_REACHED =
  dcast.data.table(
    ORIGINAL_VS_ESTIMATED_FISH_BY_FIAY,
    formula = FISHERY + AREA ~ YEAR,
    fun.aggregate = max,
    value.var = "COVERAGE_W",
    drop = c(TRUE, FALSE),
    fill = NA
  )

write.csv(MINIMUM_COVERAGE_REACHED, file = output_folder(SPECIES, LOCAL_FOLDER, "samples/SAMPLES_COVERAGE_REACHED.csv"), na = "", row.names = FALSE)