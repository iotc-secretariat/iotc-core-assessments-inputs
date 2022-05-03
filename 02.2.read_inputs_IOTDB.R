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

SF_FL_WITH_SAMPLES = query(
  IOTDB,
  paste0("
  WITH CSVF AS (
	  SELECT DISTINCT
		  C.Gear AS FISHERY_CODE,
		  CASE 
			  WHEN Country = ReportingCo THEN Country
			  ELSE CONCAT(Country, '-', ReportingCo) 
		  END AS FLEET_CODE,
		  DSFleetCode AS FLEET
	  FROM CountryStratvsFleet C
  ), SA AS (
	  SELECT
  		A.YEAR,
  		A.MONTH_START,
  		A.MONTH_END,
  		C.FLEET,
  		A.FISHERY_CODE,
  		A.SCHOOL_TYPE_CODE,
  		F.RegArea AS FISHING_GROUND_CODE,
  		A.SAMPLE_SIZE --SUM(A.SAMPLE_SIZE) AS SAMPLE_SIZE
  	FROM
  		V_LEGACY_SA A
  	INNER JOIN	
  		CSVF C
  	ON
  		A.FISHERY_CODE = C.FISHERY_CODE AND
  		A.FLEET_CODE = C.FLEET_CODE
  	INNER JOIN
  		cdeGeoFeatures F
  	ON
  		A.FISHING_GROUND_CODE = F.ACode
  	WHERE
  		A.SPECIES_CODE = '", SPECIES, "'
  )
  SELECT 
  	SA.SAMPLE_SIZE,
  	F.*
  FROM 
    vwSF", SPECIES, " F
  LEFT JOIN 
    SA
  ON 
  	F.Year = SA.YEAR AND
  	F.MonthStart = SA.MONTH_START AND
  	F.MonthEnd = SA.MONTH_END AND
  	F.Fleet = SA.FLEET AND
  	F.Gear = SA.FISHERY_CODE AND
  	F.SchoolType = SA.SCHOOL_TYPE_CODE AND
  	F.Grid = SA.FISHING_GROUND_CODE
  ")
)

save(list = "SF_FL_WITH_SAMPLES", file = input_folder(SPECIES, LOCAL_FOLDER, "IOTDB/SF_FL_WITH_SAMPLES.RData"))
write.csv(SF_FL_WITH_SAMPLES,     file = input_folder(SPECIES, LOCAL_FOLDER, "IOTDB/SF_FL_WITH_SAMPLES.csv"), row.names = FALSE)

SF_SAMPLES_NO_ORIG = sum(SF_FL_WITH_SAMPLES$TnoFish)