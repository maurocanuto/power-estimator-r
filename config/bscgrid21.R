# # HOST MODEL
# # Remove metrics with high correlation before building model
# removeCorrelatedVars <- FALSE
# R_VALUE <- 0.95
# PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid21/sampling2/model_coefficients/"
# VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid21/sampling2/output_files_validation/joined/" 
# cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15")
# filter <- c("cpu.migrations","contexts","mem_buffers","mem_cached","cpu_idle","cpu_user","cpu_nice","reads","writes","read_time" ,"write_time","mem_shared","interrupts","cpu_system","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
# disk.metrics <- c("bytes_read","bytes_written")
# network.metrics <- c("bytes_in","bytes_out")
# filter2 <- c("powerWatts",filter)
# keepme <- c()

#VM
# Remove metrics with high correlation before building model
removeCorrelatedVars <- FALSE
R_VALUE <- 0.95
PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid21/sampling2/model_coefficients/"
VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid21/sampling2/output_files_validation/joined/" 
cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15")
#filter <- c("contexts","mem_buffers","mem_cached","cpu_idle", "cpu_nice","reads","writes","read_time" ,"write_time","mem_shared","interrupts","cpu_system","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
#filter <- c("cpu.migrations","mem_shared","interrupts","cpu_intr", "cpu_sintr","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
filter <- c("cpu.migrations","contexts","mem_buffers","mem_cached","cpu_idle","cpu_user","cpu_nice","reads","writes","read_time" ,"write_time","mem_shared","interrupts","cpu_system","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
disk.metrics <- c("bytes_read","bytes_written")
network.metrics <- c("bytes_in","bytes_out")
filter2 <- c("powerWatts",filter, network.metrics,disk.metrics, "cpu.migrations","RETIRED_MMX_FP_INSTRUCTIONS_MMX","cpu.cycles","RETIRED_SSE_OPS", "cpu_user")
keepme <- c()
idle.file <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid21/sampling2/idleWithVM_bscgrid21.csv"

