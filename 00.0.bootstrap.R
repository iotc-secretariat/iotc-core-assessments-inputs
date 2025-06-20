# Clears the environment
rm(list = ls())

library(iotc.base.common.plots)
library(RODBC)

# Change this to run the process for a different species (that will self-configure)
SPECIES = "ALB" 

source("./00.1.common_constants.R")
source("./00.2.helpers_misc.R")

source(species_folder(SPECIES, "00.90.species_constants.R"))
source(species_folder(SPECIES, "00.91.species_aes.R"))
source(species_folder(SPECIES, "00.92.species_area_fishery.R"))

source("./00.3.helpers_SF_area.R")
source("./00.4.helpers_fishery_area.R")
source("./00.5.helpers_size_class_and_bin.R")

source("./01.1.configure_data_access.R")

# Scripts below read all relevant datasets from the Access DB files (for the SA) and from the IOTDB
if(TRUE) {   
  source("./02.1.read_inputs_SA.R")
  source("./02.2.read_inputs_IOTDB.R")
}

#source("./03.1.prepare_inputs_SA_CE_all.R")
source("./03.2.prepare_inputs_SA_CE_raised.R")
source("./03.3.prepare_inputs_SA_CE_for_SF.R")
#source("./03.4.prepare_inputs_SA_SF_all.R")

source("./03.5.prepare_inputs_IOTDB_SF.R")
source("./04.1.prepare_SA_samples.R")
source("./04.2.prepare_SA_CAS.R")
source("./04.3.prepare_SA_CAA.R")
source("./05.1.produce_catch_summaries.R")
source("./05.2.produce_avg_weight.R")
source("./06.1.produce_SAMPLES.R")
source("./06.2.produce_CAS_NO.R")
source("./06.3.produce_CAS_W.R")

if(!is.na(AL_METHOD)) { 
  source("./06.4.produce_CAA_NO.R")
  source("./06.5.produce_CAA_W.R")
}

source("./06.6.produce_samples_summaries.R")

# source(species_folder(SPECIES, "./00.93.cleanup.R"))    # Not yet developed

source("./99.0.update_WP_CE_raised.R")
