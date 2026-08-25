IOTDB_FLEETS = query(
  IOTDB,
  "
  	SELECT DISTINCT 		
  		CASE 
  			WHEN C.Country = C.ReportingCo THEN C.Country 
  			ELSE CONCAT(C.Country, '-', C.ReportingCo) 
  		END                       AS FLEET_CODE,
  		LTRIM(RTRIM(GEAR))        AS FISHERY_CODE,
  		LTRIM(RTRIM(DSFleetCode)) AS FLEET,
  		TypeOperation             AS FISHERY_TYPE
  	FROM 
  		[IOTDB].[dbo].CountryStratvsFleet C
  "
)

IOTDB_IRREGULAR_GRID_MAPPINGS = query(
  IOTDB,
  "
    SELECT 
      LTRIM(RTRIM(ACode))      AS FISHING_GROUND_CODE, 
      CAST(RegArea AS CHAR(7)) AS REGULAR_FISHING_GROUND_CODE
    FROM 
      [cdeGeoFeatures]
  "
)

### Standardized SF data (from vwSF<species code>)

# SF_FL_WITH_SAMPLES = query(
#   IOTDB,
#   paste0("
#   WITH CSVF AS (
# 	  SELECT DISTINCT
# 		  C.Gear AS FISHERY_CODE,
# 		  CASE 
# 			  WHEN Country = ReportingCo THEN Country
# 			  ELSE CONCAT(Country, '-', ReportingCo) 
# 		  END AS FLEET_CODE,
# 		  DSFleetCode AS FLEET
# 	  FROM CountryStratvsFleet C
#   ), SA AS (
# 	  SELECT
#   		A.YEAR,
#   		A.MONTH_START,
#   		A.MONTH_END,
#   		C.FLEET,
#   		A.FISHERY_CODE,
#   		A.SCHOOL_TYPE_CODE,
#   		A.FISHING_GROUND_CODE, 
#   		F.RegArea AS FISHING_GROUND_CODE,
#   		A.SAMPLE_SIZE --SUM(A.SAMPLE_SIZE) AS SAMPLE_SIZE
#   	FROM
#   		V_LEGACY_SA A
#   	INNER JOIN	
#   		CSVF C
#   	ON
#   		A.FISHERY_CODE = C.FISHERY_CODE AND
#   		A.FLEET_CODE = C.FLEET_CODE
#   	INNER JOIN
#   		cdeGeoFeatures F
#   	ON
#   		A.FISHING_GROUND_CODE = F.ACode
#   	WHERE
#   		A.SPECIES_CODE = '", SPECIES, "'
#   )
#   SELECT 
#   	SA.SAMPLE_SIZE,
#   	F.*
#   FROM 
#     vwSF", SPECIES, " F
#   LEFT JOIN 
#     SA
#   ON 
#   	F.Year = SA.YEAR AND
#   	F.MonthStart = SA.MONTH_START AND
#   	F.MonthEnd = SA.MONTH_END AND
#   	F.Fleet = SA.FLEET AND
#   	F.Gear = SA.FISHERY_CODE AND
#   	F.SchoolType = SA.SCHOOL_TYPE_CODE AND
#   	F.Grid = SA.FISHING_GROUND_CODE
#   ")
# )

# Query to extract the numbers of samples by stratum
# The join with cdeGeoFeatures enables to assign regular grids to irregular ones
sql = paste0("
  WITH CSVF AS (
	  SELECT DISTINCT
		  FISHERY_CODE, FLEET_CODE, FLEET
	  FROM meta.temp_csvf C
  )
  SELECT
  		A.YEAR, 
  		A.MONTH_START, 
  		A.MONTH_END, 
  		C.FLEET AS FLEET_CODE, 
  		A.FISHERY_CODE AS GEAR_CODE, 
  		A.SCHOOL_TYPE_CODE, 
  		A.FISHING_GROUND_CODE AS ORIGINAL_FISHING_GROUND_CODE, 
  		F.RegArea AS LEGACY_REGULAR_FISHING_GROUND_CODE, 
  		A.SAMPLE_SIZE --, 
  		--A.RAISE_CODE, 
  		--A.QUALITY_CODE
  	FROM
  		V_LEGACY_SA A
  	INNER JOIN CSVF C ON (A.FISHERY_CODE = C.FISHERY_CODE AND A.FLEET_CODE = C.FLEET_CODE)
  	INNER JOIN cdeGeoFeatures F ON (A.FISHING_GROUND_CODE = F.ACode) 
  	WHERE
  		A.SPECIES_CODE = \'", SPECIES, "\'
  		")

SF_SAMPLE_NUMBERS_RAW = data.table(dbGetQuery(DB_IOTDB(), sql))
SF_SAMPLE_NUMBERS_RAW[, LEGACY_REGULAR_FISHING_GROUND_CODE := trimws(LEGACY_REGULAR_FISHING_GROUND_CODE)]

# Update areas from IOTDB to IOTC_ReferenceData codelist
sf_area_mapping = fread("./references/MAPPING_SF_IRREGULAR_AREAS_IOTDB_MASTER.csv", colClasses = c("character", "character"))

# Irregular areas using "regular" format (e.g., 2100040)
SF_SAMPLE_NUMBERS_RAW = merge(SF_SAMPLE_NUMBERS_RAW, sf_area_mapping, by.x = "LEGACY_REGULAR_FISHING_GROUND_CODE", by.y = "LEGACY_FISHING_GROUND_CODE", all.x = TRUE)

# Keep proper regular areas (1x1 and 5x5)
SF_SAMPLE_NUMBERS_RAW[is.na(FISHING_GROUND_CODE) & substr(LEGACY_REGULAR_FISHING_GROUND_CODE, 1, 1) %in% c("5", "6"), FISHING_GROUND_CODE := LEGACY_REGULAR_FISHING_GROUND_CODE]

## Aggregate to account for multiple records across fishing grounds 
SF_SAMPLE_NUMBERS = SF_SAMPLE_NUMBERS_RAW[, .(SAMPLE_SIZE = sum(SAMPLE_SIZE)), keyby = .(YEAR, MONTH_START, MONTH_END, FLEET_CODE, GEAR_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)]  #, RAISE_CODE, QUALITY_CODE

# Add sample size to size frequency data
SF_DATA = query(DB_IOTDB(), paste0("SELECT * FROM vwSF", SPECIES))

setnames(SF_DATA, old = "Grid", new = "Legacy_Grid")

# Irregular areas using "regular" format (e.g., 2100040)
SF_DATA <- merge(SF_DATA, sf_area_mapping[, .(LEGACY_FISHING_GROUND_CODE, Grid = FISHING_GROUND_CODE)], by.x = "Legacy_Grid", by.y = "LEGACY_FISHING_GROUND_CODE", all.x = TRUE)

SF_DATA[is.na(Grid), Grid := Legacy_Grid][, Legacy_Grid := NULL]

# Combine size-frequency datasets with sample numbers
SF_FL_WITH_SAMPLES = merge(SF_DATA, SF_SAMPLE_NUMBERS, by.x = c("Year", "MonthStart", "MonthEnd", "Fleet", "Gear", "SchoolType", "Grid"), by.y = c("YEAR", "MONTH_START", "MONTH_END", "FLEET_CODE", "GEAR_CODE", "SCHOOL_TYPE_CODE", "FISHING_GROUND_CODE"))

save(list = "SF_FL_WITH_SAMPLES", file = input_folder(SPECIES, LOCAL_FOLDER, "IOTDB/SF_FL_WITH_SAMPLES.RData"))

write.csv(SF_FL_WITH_SAMPLES, file = input_folder(SPECIES, LOCAL_FOLDER, "IOTDB/SF_FL_WITH_SAMPLES.csv"), row.names = FALSE)

SF_SAMPLES_NO_ORIG = sum(SF_FL_WITH_SAMPLES$TnoFish)
