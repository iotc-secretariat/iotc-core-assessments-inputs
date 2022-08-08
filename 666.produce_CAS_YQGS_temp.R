merged_CAS_YQG = merged_CAS[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = .(YEAR, QUARTER, GEAR_CODE, 
                                                                         SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, 
                                                                         FIRST_CLASS_LOW, SIZE_INTERVAL, 
                                                                         SIZE_BIN, SIZE_CLASS)]

CAS_NUM = 
  dcast.data.table(
    merged_CAS_YQG,
    YEAR + QUARTER + 
    GEAR_CODE + SCHOOL_TYPE_CODE +
    FISHING_GROUND_CODE +
    FIRST_CLASS_LOW + SIZE_INTERVAL ~ SIZE_BIN,
    value.var = FISH_COUNT, 
    fun = sum, 
    fill = 0.0,
    drop = c(TRUE, FALSE)
  )

write.table(CAS_NUM, file="CAS_SKJ_NUM.csv", sep = ",", dec = ".", row.names = FALSE)

merged_CAS_YQG = merged_CAS[, .(FISH_WEIGHT = sum(FISH_WEIGHT)), keyby = .(YEAR, QUARTER, GEAR_CODE, 
                                                                           SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, 
                                                                           FIRST_CLASS_LOW, SIZE_INTERVAL, 
                                                                           SIZE_BIN, SIZE_CLASS)]

CAS_WEIGHT = 
  dcast.data.table(
    merged_CAS_YQG,
    YEAR + QUARTER + 
    GEAR_CODE + SCHOOL_TYPE_CODE +
    FISHING_GROUND_CODE +
    FIRST_CLASS_LOW + SIZE_INTERVAL ~ SIZE_BIN,
    value.var = "FISH_WEIGHT", 
    fun = sum, 
    fill = 0.0,
    drop = c(TRUE, FALSE)
  )

write.table(CAS_WEIGHT, file="CAS_SKJ_WEIGHT.csv", sep = ",", dec = ".", row.names = FALSE)