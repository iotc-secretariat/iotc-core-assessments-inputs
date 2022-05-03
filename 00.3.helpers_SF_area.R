### SF AREA FUNCTIONS

#load(file = paste0("./versions/", LOCAL_FOLDER, "/input/CAS/grids_1_5_mappings.RData"))
#load(file = paste0("./versions/", LOCAL_FOLDER, "/input/CAS/grids_5_mappings.RData"))
#load(file = paste0("./versions/", LOCAL_FOLDER, "/input/CAS/grids_5_PS_LL.RData"))

assign_SF_area = function(dataset, sf_area_mappings) {
  # Computes the gear type 
  dataset[, GEAR_TYPE := ifelse(GEAR_CODE == "PS", "PS", "LL")]
  
  # Applies the 5x5 grid to PS / LL SF area mappings 
  dataset = merge(dataset, sf_area_mappings,
                  by.x = c("GEAR_TYPE", "FISHING_GROUND_CODE"),
                  by.y = c("GEAR_TYPE", "GRID_5"),
                  all.x = TRUE,
                  allow.cartesian = TRUE)
  
  # FOLLOWING COMMENTED LINES ARE NOT NEEDED ANYMORE!
  # NOW THE EXPANSION BY THE GRID_5 TO SF_AREA MAPPING IS DONE
  # THROUGH A MORE ACCURATE PROCESS THAT CONSIDERS (FOR PS)
  # HOW A SINGLE 5x5 GRID COULD INDEED BELONG TO MULTIPLE
  # SF AREAS... THIS MEANS THAT AFTER ASSIGNING THE SF_AREA
  # THERE'S THE NEED TO APPLY A PROPORTION TO THE ACTUAL 
  # VALUES HELD BY THE ORIGINAL DATASET (e.g., EST_MT AND SIMILAR)
  
  # Creates the SF_AREA column
  #dataset[, SF_AREA := ifelse(GEAR_TYPE == "PS", PS_AREA, LL_AREA)]
  
  # Removes stale columns
  #dataset$GEAR_TYPE      = NULL
  #dataset$PS_AREA_NUMBER = NULL
  #dataset$PS_AREA        = NULL
  #dataset$LL_AREA        = NULL
  
  return(dataset)
}