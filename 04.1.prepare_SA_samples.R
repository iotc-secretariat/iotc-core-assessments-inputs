############################################################################
#### Prepares 'Standard' SA samples by Fishery, Area, Year, and Quarter ####
############################################################################

prepare_SA_samples_FIA_Q = function(raised_catches, size_frequency_samples) {
  SF_SAMPLES_NO = sum(size_frequency_samples$NUMBER_OF_SAMPLES)
  
  CE_RAISED_MT = sum(raised_catches$EST_MT)
  CE_RAISED_NO = sum(raised_catches$EST_NO)
  
  # Merges the CE_raised and SF
  SA_samples_FIA_Q = merge(raised_catches, 
                           size_frequency_samples, 
                           all.x = TRUE)
  
  setcolorder(
    SA_samples_FIA_Q, 
    c("FISHERY", "AREA", "AREA_ORIG",
      "YEAR", "QUARTER", 
      "FIRST_CLASS_LOW", "SIZE_INTERVAL", "NUMBER_OF_SAMPLES", 
      "EST_NO", "EST_MT")
  )
  
  SA_samples_FIA_Q[is.na(NUMBER_OF_SAMPLES), `:=`(NUMBER_OF_SAMPLES = 0,
                                                  FIRST_CLASS_LOW = DEFAULT_FIRST_CLASS_LOW, # Species-specific constant imported through species' configuration
                                                  SIZE_INTERVAL = DEFAULT_SIZE_INTERVAL      # Species-specific constant imported through species' configuration
                                              )]
  
  # Orders the result and assigns 0 as number of samples, and the default first class low and 
  # size-interval values to those records for which there was no SF_<species code> data
  
  SA_samples_FIA_Q = SA_samples_FIA_Q[order(-YEAR, +FISHERY, +AREA, +AREA_ORIG, +QUARTER)]
  SA_samples_FIA_Q[is.na(NUMBER_OF_SAMPLES), `:=`(NUMBER_OF_SAMPLES = 0,
                                                  FIRST_CLASS_LOW   = DEFAULT_FIRST_CLASS_LOW, # Species-specific constant imported through species' configuration
                                                  SIZE_INTERVAL     = DEFAULT_SIZE_INTERVAL    # Species-specific constant imported through species' configuration
                                              )]
  
  
  ### Performs some sanity checks
  SA_samples_FIA_Q_SA = sum(SA_samples_FIA_Q$NUMBER_OF_SAMPLES)
  SA_samples_FIA_Q_MT = sum(SA_samples_FIA_Q$EST_MT)
  SA_samples_FIA_Q_NO = sum(SA_samples_FIA_Q$EST_NO)
  
  l_info("")
  l_info("### Samples FI_YQ ###")
  l_info("")
  l_info(paste0("Original no. samples: ", SF_SAMPLES_NO))
  l_info(paste0("No. samples         : ", SA_samples_FIA_Q_SA))
  l_info(paste0("Difference          : ", SF_SAMPLES_NO - SA_samples_FIA_Q_SA))
  
  if(( abs(SF_SAMPLES_NO - SA_samples_FIA_Q_SA) / SF_SAMPLES_NO ) > 0.1) 
    stop("Total samples number after pivoting by fishery / area / year / quarter differs by over 1% compared to original number of samples")
  
  l_info("")
  l_info("### Est. weight FI_YQ ###")
  l_info("")
  l_info(paste0("Original est. weight: ", CE_RAISED_MT))
  l_info(paste0("Est. weight         : ", SA_samples_FIA_Q_MT))
  l_info(paste0("Difference          : ", CE_RAISED_MT - SA_samples_FIA_Q_MT))
  
  if(( abs(CE_RAISED_MT - SA_samples_FIA_Q_MT) / CE_RAISED_MT ) > 0.1) 
    stop("Total estimated weight after pivoting by fishery / area / year / quarter differs by over 1% compared to original estimated weight")
  
  l_info("")
  l_info("### Est. number FI_YQ ###")
  l_info("")
  l_info(paste0("Original est. number: ", CE_RAISED_NO))
  l_info(paste0("Est. number         : ", SA_samples_FIA_Q_NO))
  l_info(paste0("Difference          : ", CE_RAISED_NO - SA_samples_FIA_Q_NO))
  
  if(( abs(CE_RAISED_NO - SA_samples_FIA_Q_NO) / CE_RAISED_NO ) > 0.1) 
    stop("Total estimated number after pivoting by fishery / area / year / quarter differs by over 1% compared to original estimated number")
  
  return(SA_samples_FIA_Q)
}