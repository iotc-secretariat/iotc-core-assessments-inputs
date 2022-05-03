size_class_from_size_bin = function(dataset) {
  # Requires 'dataset' to include the FIRST_CLASS_LOW and SIZE_INTERVAL column for each row
  dataset = dataset[, SIZE_CLASS := FIRST_CLASS_LOW + ( as.integer(str_sub(SIZE_BIN, start = -3)) - 1 ) * SIZE_INTERVAL ]
  
  return(dataset)
}

size_bin_from_size_class = function(dataset, first_class_low = NA, size_interval = NA, max_bins = 110) {
  if(!is.na(first_class_low)) dataset[, FIRST_CLASS_LOW := first_class_low]
  if(!is.na(size_interval))   dataset[, SIZE_INTERVAL   := size_interval]
  
  dataset[, SIZE_BIN := NA ]
  dataset[SIZE_CLASS <= FIRST_CLASS_LOW, SIZE_BIN := "T001"]
  dataset[SIZE_CLASS >= FIRST_CLASS_LOW + ( max_bins - 1 ) * SIZE_INTERVAL, SIZE_BIN := paste0("T", str_sub(paste0("000", max_bins), start = -3))]
  dataset[is.na(SIZE_BIN), SIZE_BIN := paste0("T", str_sub(paste0("000", 1 + as.integer(( SIZE_CLASS - FIRST_CLASS_LOW ) / SIZE_INTERVAL)), start = -3))]

  all_size_bins = seq(1:max_bins)
  all_size_bins = paste0("T", str_sub(paste0("000", all_size_bins), start = -3))
  
  dataset$SIZE_BIN = factor(
    dataset$SIZE_BIN,
    labels = all_size_bins,
    levels = all_size_bins,
    ordered = TRUE
  )
  
  return(dataset)
}