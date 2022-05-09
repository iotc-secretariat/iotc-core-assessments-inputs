### SA_CAA table (no)

print("###### SA_CAA ######")

load(file = input_folder(SPECIES, LOCAL_FOLDER, "CAS/AGE_LENGTH_KEYS.RData"))

# CE_R_xyz contains the standardized raised catches in numbers and weight by year, quarter and criteria 
# (either fishery + area, or month + fleet + gear + school type + fishing ground)

# CE_R_FIA_Q is created in 03.2.prepare_inputs_SA_CE_raised.R
# CE_R_YQMFG is created in 03.2.prepare_inputs_SA_CE_raised.R

# CE_SF_YQFG contains the estimated size distribution in numbers by size class, year, quarter, fleet, gear, school type and SF area

# CE_SF_YQFG is created in 03.3.prepare_inputs_SA_CE_for_SF.R

# At this stage, merged_CAS should be already available from previous calculations

# merged_CAS = merge_catches_and_quarterly_CAS(CE_R_YQMFG, CE_SF_YQFG)

if(FALSE) {
  print("###### Preparing CAA (in weight) by Y / Q / M / F / G / S / FG...")
  
  CAA_YQMFG_W = prepare_SA_CAA_NO_YQMFG(merged_CAS, AL_KEYS_METHOD, "FISH_WEIGHT")
  
  write.csv(CAA_YQMFG_W, file = output_folder(SPECIES, LOCAL_FOLDER, paste0("CAA/weight/SA_CAA_", AL_METHOD, "_W_raw.csv")), na = "", row.names = FALSE)
}

print("###### Preparing CAA (in weight) by FI / A / Y / Q...")

CAA_FIA_Q_W = prepare_SA_CAA_NO_FIA_Q(merged_CAS, AL_KEYS_METHOD, "FISH_WEIGHT")

write.csv(CAA_FIA_Q_W, file = output_folder(SPECIES, LOCAL_FOLDER, paste0("CAA/weight/SA_CAA_", AL_METHOD, "_W.csv")), na = "", row.names = FALSE)
              
CAA_FIA_Q_W_U = copy(CAA_FIA_Q_W)
delete_column(CAA_FIA_Q_W_U, c("EST_MT", "EST_NO", "METHOD"))

CAA_FIA_Q_UNPIVOTED_W = 
  melt.data.table(
    CAA_FIA_Q_W_U,
    id.vars = c("FISHERY", "AREA", "AREA_ORIG", "YEAR", "QUARTER"),
    value.name = "FISH_WEIGHT", 
    variable.name = "AGE"
  )

AGES = as.character(sort(unique(CAA_FIA_Q_UNPIVOTED_W$AGE)))

N_AGE_CLASSES = length(AGES)

CAA_FIA_Q_UNPIVOTED_W$AGE = factor(
  CAA_FIA_Q_UNPIVOTED_W$AGE,
  levels = AGES,
  labels = AGES,
  ordered = TRUE
)

AGE_COLORS = color_table(unique_colors(length(N_AGE_CLASSES)))

A00 = darken(ALL_FI_COLORS[FISHERY_CODE == "PSLS"]$FILL, amount = 0.2)
A99 = darken(ALL_FI_COLORS[FISHERY_CODE ==  "LLD"]$FILL, amount = 0.2)

RAMP_PALETTE = colorRampPalette(c(A00, A99))

AGE_COLORS = data.table(FILL = RAMP_PALETTE(N_AGE_CLASSES))
AGE_COLORS[, OUTLINE := darken(FILL, amount = 0.4)]

CAA_FIA_Q_MT_PLOT = 
  bar.value(CAA_FIA_Q_UNPIVOTED_W,
            value = "FISH_WEIGHT",
            time = "YEAR",
            fill_by = "AGE",
            scale = 1000,
            num_legend_rows = 1,
            max_categories = N_AGE_CLASSES,
            colors = AGE_COLORS,
            x_axis_label = "Year",
            y_axis_label = "Fish weight (x 1,000 t)")

ggsave(CAA_FIA_Q_MT_PLOT, file = output_folder(SPECIES, LOCAL_FOLDER, paste0("CAA/weight/SA_CAA_", AL_METHOD, "_W.png")), width = 12, height = 6.75)

for(fishery in unique(CAA_FIA_Q_UNPIVOTED_W$FISHERY)) {
  CAA_FIA_Q_MT_PLOT = 
    bar.value(CAA_FIA_Q_UNPIVOTED_W[FISHERY == fishery],
              value = "FISH_WEIGHT",
              time = "YEAR",
              fill_by = "AGE",
              scale = 1000,
              num_legend_rows = 1,
              max_categories = N_AGE_CLASSES,
              colors = AGE_COLORS,
              x_axis_label = "Year",
              y_axis_label = "Fish weight (x 1,000 t)")
  
  ggsave(CAA_FIA_Q_MT_PLOT, file = output_folder(SPECIES, LOCAL_FOLDER, paste0("CAA/weight/by_fishery/SA_CAA_", AL_METHOD, "_W_", fishery, ".png")), width = 12, height = 6.75)
}

CAA_FIA_Q_W_PLOT_REL = 
  bar.value.rel(CAA_FIA_Q_UNPIVOTED_W,
                value = "FISH_WEIGHT",
                time = "YEAR",
                fill_by = "AGE",
                num_legend_rows = 1,
                max_categories = N_AGE_CLASSES,
                colors = AGE_COLORS,
                x_axis_label = "Year",
                y_axis_label = "Fish weight (%)")

ggsave(CAA_FIA_Q_W_PLOT_REL, file = output_folder(SPECIES, LOCAL_FOLDER, paste0("CAA/weight/SA_CAA_", AL_METHOD, "_W_REL.png")), width = 12, height = 6.75)

for(fishery in unique(CAA_FIA_Q_UNPIVOTED_W$FISHERY)) {
  CAA_FIA_Q_W_PLOT_REL = 
    bar.value.rel(CAA_FIA_Q_UNPIVOTED_W[FISHERY == fishery],
                  value = "FISH_WEIGHT",
                  time = "YEAR",
                  fill_by = "AGE",
                  num_legend_rows = 1,
                  max_categories = N_AGE_CLASSES,
                  colors = AGE_COLORS,
                  x_axis_label = "Year",
                  y_axis_label = "Fish weight (%)")
  
  ggsave(CAA_FIA_Q_W_PLOT_REL, file = output_folder(SPECIES, LOCAL_FOLDER, paste0("CAA/weight/by_fishery/SA_CAA_", AL_METHOD, "_W_REL_", fishery, ".png")), width = 12, height = 6.75)
}