### DEBUG

runGC = function() {
  if(FALSE) l_info("GC() - START")
  gc()
  if(FALSE) l_info("GC() - END")
}

### FILE ACCESS FUNCTIONS

species_folder = function(species_code, file_name = NA) {
  return(paste0("./species/", species_code, "/", ifelse(is.na(file_name), "" , file_name)))
}

base_folder = function(species_code, wp_version, file_name = NA) {
  return(paste0("./species/", species_code, "/WP/", wp_version, "/", ifelse(is.na(file_name), "" , file_name)))
}

input_folder = function(species_code, wp_version, file_name) {
  return(paste0("./species/", species_code, "/WP/", wp_version, "/input/", ifelse(is.na(file_name), "", file_name)))
}

output_folder = function(species_code, wp_version, file_name) {
  return(paste0("./species/", species_code, "/WP/", wp_version, "/output/", ifelse(is.na(file_name), "", file_name)))
}

references_folder = function(species_code, wp_version, file_name) {
  return(paste0("./species/", species_code, "/WP/", wp_version, "/references/", ifelse(is.na(file_name), "", file_name)))
}

### DATA TABLE FUNCTIONS

change_column_name = function(data_table, old_name, new_name) {
  colnames(data_table)[which(colnames(data_table) == old_name)] = new_name
  
  return(data_table)
}

delete_column = function(data_table, column_name) {
  data_table[, (column_name) := NULL]
  
  return(data_table)
}

### ACCESS DB FUNCTIONS

connect_to = function(access_file) {
  l_info(paste("Attempting to connect to", access_file))
  return (
    odbcDriverConnect(
      paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", access_file)
    )
  )
}

get_table = function(connection, table_name) {
  return (
    as.data.table(
      sqlFetch(connection, table_name)
    )
  )
}

### Gear codes fro PS / PL / GI
GEAR_PS_PL_GI = query(IOTDB, "SELECT ACode AS CODE FROM cdeGears WHERE LAggCode IN ('Baitboat', 'Gillnet', 'Purse Seine') OR ( LAggCESF = 'Other' )")