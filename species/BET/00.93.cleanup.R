# Produces the map of all original areas
SA_SF_ORIGs = as.data.table(WKT_to_simple_feature(SA_AREAS_ORIG))
SA_SF_ORIGs$NAME_SHORT = SA_AREAS_ORIG$NAME_SHORT

SORTED_NAMES = SA_SF_ORIGs[order(CODE)]$NAME_SHORT

SA_SF_ORIGs$NAME_SHORT = factor(
  SA_SF_ORIGs$NAME_SHORT,
  levels = SORTED_NAMES,
  labels = SORTED_NAMES,
  ordered = TRUE
)

SA_ORIG_MAP = iotc.core.gis.maps::IO_map(show_EEZs = TRUE, show_high_seas = FALSE, show_IO_areas = FALSE) 
SA_ORIG_MAP =
  SA_ORIG_MAP +
    geom_sf(data = SA_SF_ORIGs, mapping = aes(geometry = WKT, color = NAME_SHORT, fill = NAME_SHORT, alpha = NAME_SHORT)) +
    scale_fill_manual(values = AR_ORIG_COLORS$FILL) +
    scale_color_manual(values = AR_ORIG_COLORS$OUTLINE) +
    scale_alpha_manual(values = rep(0.6, nrow(SA_SF_ORIGs))) + 
    theme(legend.position = "right") + 
    labs(fill = "Original stock assessment area") + 
    guides(alpha = "none", color = "none")

ggsave(SA_ORIG_MAP, file = output_folder(SPECIES, LOCAL_FOLDER, "SA_AREAS_ORIG.png"), width = 12, height = 7.5)