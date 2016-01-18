# Remove metrics with high correlation before building model
removeCorrelatedVars <- FALSE
R_VALUE <- 0.92
PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid31/sampling2/model_coefficients/"
VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid31/sampling2/output_files_validation/joined/" 
cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23")
filter <- c("cpu_system","mem_buffers","mem_cached" ,"cpu_idle", "cpu_nice","reads","writes","read_time" ,"write_time","mem_shared","interrupts","cpu_system","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
#filter <- c("cpu.migrations","mem_shared","interrupts","cpu_intr", "cpu_sintr","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
fff <- c("cpu.migrations","L2_RQSTS_ALL_CODE_RD","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS","MEMORY_REQUESTS","mem_cached")
disk.metrics <- c("bytes_read","bytes_written")
network.metrics <- c("bytes_in","bytes_out", "pkts_in", "pkts_out")
filter2 <- c("powerWatts",fff,filter,network.metrics,"RETIRED_MMX_FP_INSTRUCTIONS_MMX","contexts", "cpu_user",disk.metrics,"L1.icache.load.misses", "instructions")
keepme <- c()
idle.file <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid31/sampling2/idleWithVM_bscgrid31.csv"

# removeCorrelatedVars <- FALSE
# R_VALUE <- 0.92
# PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid31/sampling2/model_coefficients/"
# VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid31/sampling2/output_files_validation/joined/" 
# cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23")
# filter <- c("cpu_system", "cpu_idle", "cpu_nice","mem_cached","reads","writes","read_time" ,"write_time","mem_shared","interrupts","cpu_system","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
# #filter <- c("cpu.migrations","mem_shared","interrupts","cpu_intr", "cpu_sintr","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
# fff <- c("cpu.migrations","L2_RQSTS_ALL_CODE_RD","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS","mem_cached")
# disk.metrics <- c("bytes_read","bytes_written")
# network.metrics <- c("bytes_in","bytes_out", "pkts_in", "pkts_out")
# filter2 <- c("powerWatts","mem_buffers",fff,filter,network.metrics,"RETIRED_MMX_FP_INSTRUCTIONS_MMX","contexts", "cpu_user",disk.metrics, "L1.icache.load.misses", "instructions")
# keepme <- c()
# idle.file <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid31/sampling2/idleWithVM_bscgrid31.csv"