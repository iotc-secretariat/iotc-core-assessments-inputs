### Production of catch summary tables and charts

# SA_AREAS are defined in 00.92.species_area_fishery.R (species-specific)

# Puts data together
CATCHES = assign_area_and_fishery(CE_R_YQMFG)
CATCHES$AREA    = factor(CATCHES$AREA)
CATCHES$FISHERY = factor(CATCHES$FISHERY)

levels(CATCHES$AREA) = AREA_NAMES

# This is implemented in 00.92.species_area_fishery.R (species-specific)
CATCHES = update_fishery_groups(CATCHES)

CATCHES[, Decade := as.integer(floor(YEAR / 10) * 10)]

CATCHES = CATCHES[, .(Decade, 
                      Year = YEAR, 
                      FisheryGroup = FISHERY_GROUP, 
                      Fishery = FISHERY, 
                      Area = AREA, 
                      Catches = EST_MT)]

### SUMMARIES OF CATCHES BY FISHERY GROUP (species specific)

#### ANNUAL

CATCHES_BY_YEAR_FISHERY_GROUP_TABLE = 
  dcast.data.table(
    CATCHES,
    FisheryGroup ~ Year,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

write.csv(CATCHES_BY_YEAR_FISHERY_GROUP_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery_group/CA_FG_Y.csv"), na = "", row.names = FALSE)

colnames(CATCHES_BY_YEAR_FISHERY_GROUP_TABLE)[1] = "Fishery group"

CATCHES_BY_YEAR_FISHERY_GROUP_PLOT = 
  bar.value(CATCHES, 
            value = "Catches", time = "Year", 
            fill_by = "FisheryGroup", 
            colors = FG_COLORS,
            scale = 1000,
            x_axis_label = "Year",
            y_axis_label = "Annual catches (x 1,000 t)")

ggsave(CATCHES_BY_YEAR_FISHERY_GROUP_PLOT, filename = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery_group/CA_FG_Y.png"), width = 12, height = 6.75)

CATCHES_BY_YEAR_FISHERY_GROUP_PLOT_REL = 
  bar.value.rel(CATCHES, 
                value = "Catches", time = "Year", 
                fill_by = "FisheryGroup", 
                colors = FG_COLORS,
                x_axis_label = "Year",
                y_axis_label = "Annual catches (%)")

ggsave(CATCHES_BY_YEAR_FISHERY_GROUP_PLOT_REL, filename = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery_group/CA_FG_Y_REL.png"), width = 12, height = 6.75)

#### DECADAL

CATCHES_BY_DECADE_FISHERY_GROUP_TABLE = 
  dcast.data.table(
    CATCHES,
    FisheryGroup ~ Decade,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

colnames(CATCHES_BY_DECADE_FISHERY_GROUP_TABLE)[1] = "Fishery group"

write.csv(CATCHES_BY_DECADE_FISHERY_GROUP_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery_group/CA_FG_D.csv"), na = "", row.names = FALSE)

### SUMMARIES OF CATCHES BY FISHERY AND AREA

#### ANNUAL

CATCHES_BY_YEAR_FISHERY_AND_AREA_TABLE = 
  dcast.data.table(
    CATCHES,
    Fishery + Area ~ Year,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

write.csv(CATCHES_BY_YEAR_FISHERY_AND_AREA_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery/CA_FI_A_Y.csv"), na = "", row.names = FALSE)

#### DECADAL

CATCHES_BY_DECADE_FISHERY_AND_AREA_TABLE = 
  dcast.data.table(
    CATCHES,
    Fishery + Area ~ Decade,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

write.csv(CATCHES_BY_DECADE_FISHERY_AND_AREA_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery/CA_FI_A_D.csv"), na = "", row.names = FALSE)

### SUMMARIES OF CATCHES BY FISHERY

#### ANNUAL

CATCHES_BY_YEAR_FISHERY_TABLE = 
  dcast.data.table(
    CATCHES,
    Fishery ~ Year,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

write.csv(CATCHES_BY_YEAR_FISHERY_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery/CA_FI_Y.csv"), na = "", row.names = FALSE)

CATCHES_BY_YEAR_FISHERY_PLOT = 
  bar.value(CATCHES, 
            value = "Catches", time = "Year", 
            fill_by = "Fishery", 
            colors = FI_COLORS,
            scale = 1000,
            num_legend_rows = 1,
            x_axis_label = "Year",
            y_axis_label = "Annual catches (x 1,000 t)")

ggsave(CATCHES_BY_YEAR_FISHERY_PLOT, filename = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery/CA_FI_Y.png"), width = 12, height = 6.75)

CATCHES_BY_YEAR_FISHERY_PLOT_REL = 
  bar.value.rel(CATCHES, 
                value = "Catches", time = "Year", 
                fill_by = "Fishery", 
                colors = FI_COLORS,
                num_legend_rows = 1,
                x_axis_label = "Year",
                y_axis_label = "Annual catches (%)")

ggsave(CATCHES_BY_YEAR_FISHERY_PLOT_REL, filename = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery/CA_FI_Y_REL.png"), width = 12, height = 6.75)

#### DECADAL

CATCHES_BY_DECADE_FISHERY_TABLE = 
  dcast.data.table(
    CATCHES,
    Fishery ~ Decade,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

write.csv(CATCHES_BY_DECADE_FISHERY_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_fishery/CA_FI_D.csv"), na = "", row.names = FALSE)

### SUMMARIES OF CATCHES BY AREA 

SA_SFs = WKT_to_simple_feature(SA_AREAS)
SA_SFs$NAME_SHORT = SA_AREAS$NAME_SHORT

SA_MAP = iotc.core.gis.maps::IO_map()
SA_MAP = 
  SA_MAP + 
    geom_sf(SA_SFs, mapping = aes(geometry = WKT, fill = NAME_SHORT, color = NAME_SHORT, alpha = 0.3)) +
    scale_fill_discrete(type = AR_COLORS$FILL) +
    scale_color_discrete(type = AR_COLORS$OUTLINE) +
    theme(legend.position = "top") + 
    labs(fill = "Stock assessment area") + 
    guides(alpha = "none", color = "none")

ggsave(SA_MAP, file = output_folder(SPECIES, LOCAL_FOLDER, "SA_area.png"), width = 9, height = 7.5)

#### ANNUAL

CATCHES_BY_YEAR_AREA_TABLE = 
  dcast.data.table(
    CATCHES,
    Area ~ Year,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

write.csv(CATCHES_BY_YEAR_AREA_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_area/CA_A_Y.csv"), na = "", row.names = FALSE)

CATCHES_BY_YEAR_AREA_TABLE_PLOT = 
  bar.value(CATCHES, 
            value = "Catches", time = "Year", 
            fill_by = "Area", 
            colors = AR_COLORS,
            scale = 1000,
            num_legend_rows = 1,
            x_axis_label = "Year",
            y_axis_label = "Annual catches (x 1,000 t)")

ggsave(CATCHES_BY_YEAR_AREA_TABLE_PLOT, filename = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_area/CA_A_Y.png"), width = 12, height = 6.75)

CATCHES_BY_YEAR_AREA_TABLE_PLOT_REL = 
  bar.value.rel(CATCHES, 
                value = "Catches", time = "Year", 
                fill_by = "Area", 
                colors = AR_COLORS,
                num_legend_rows = 1,
                x_axis_label = "Year",
                y_axis_label = "Annual catches (%)")

ggsave(CATCHES_BY_YEAR_AREA_TABLE_PLOT_REL, filename = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_area/CA_A_Y_REL.png"), width = 12, height = 6.75)

#### DECADAL

CATCHES_BY_DECADE_AREA_TABLE = 
  dcast.data.table(
    CATCHES,
    Area ~ Decade,
    value.var = "Catches",
    fill = NA,
    drop = c(FALSE, FALSE),
    fun.aggregate = sum
  )

write.csv(CATCHES_BY_DECADE_AREA_TABLE, file = output_folder(SPECIES, LOCAL_FOLDER, "catches/by_area/CA_A_D.csv"), na = "", row.names = FALSE)