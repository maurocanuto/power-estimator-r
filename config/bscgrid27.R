# Remove metrics with high correlation before building model
removeCorrelatedVars <- FALSE
R_VALUE <- 0.95
PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid27/sampling2/model_coefficients/"
VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid27/sampling2/output_files_validation/joined/" 
cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23")
filter_base <- c("cpu_idle", "mem_cached","mem_buffers","cpu_nice","reads","writes","read_time" ,"write_time","mem_shared","interrupts","cpu_system","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","page_in","page_out","swap_in","swap_out","swap_free","numSockets")
filter_extra <- c("branches","cpu.migrations","FP_COMP_OPS_EXE_SSE2_INTEGER","FP_COMP_OPS_EXE_SSE_SINGLE_PRECISION","SSEX_UOPS_RETIRED_VECTOR_INTEGER","SSEX_UOPS_RETIRED_PACKED_DOUBLE"   )
disk.metrics <- c("bytes_read","bytes_written")
network.metrics <- c("bytes_in","bytes_out", "pkts_in", "pkts_out")
filter2 <- c("powerWatts", filter_base,disk.metrics,"cpu_user" ,"branches","branch.misses" ,"L1.icache.loads", "SSEX_UOPS_RETIRED_VECTOR_INTEGER",network.metrics,"instructions","L1.dcache.stores","L1.dcache.store.misses","FP_COMP_OPS_EXE_SSE_FP","FP_COMP_OPS_EXE_SSE_FP_SCALAR","LLC.loads")
keepme <- c()
idle.file <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid27/sampling2/idleWithVM_bscgrid27.csv"
# 
# if (server == "bscgrid27"){
# # Remove metrics with high correlation before building model
# removeCorrelatedVars <- FALSE
# 
# R_VALUE <- 0.95
# PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid27/sampling2/model_coefficients/"
# VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid27/sampling2/output_files_validation/joined/" 
# cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23")
# filter_base <- c("cpu_idle","cpu_user" ,"cpu_nice","reads","writes","read_time" ,"write_time","mem_shared","interrupts","cpu_system","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","page_in","page_out","swap_in","swap_out","swap_free","numSockets")
# filter_extra <- c("branches","cpu.migrations","FP_COMP_OPS_EXE_SSE2_INTEGER","FP_COMP_OPS_EXE_SSE_SINGLE_PRECISION","SSEX_UOPS_RETIRED_VECTOR_INTEGER","SSEX_UOPS_RETIRED_PACKED_DOUBLE"   )
# disk.metrics <- c("bytes_read","bytes_written")
# network.metrics <- c("bytes_in","bytes_out", "pkts_in", "pkts_out")
# #filter2 <- c(filter, disk.metrics, network.metrics, cores)
# #keepme <- c("powerWatts", disk.metrics, network.metrics, cores)
# filter2 <- c("powerWatts", filter_base,filter_extra, disk.metrics,network.metrics,"mem_buffers","contexts", "cpu.migrations","mem_cached","Core_1CPU","Core_2CPU","branches","branch.misses" ,"L1.icache.loads", "SSEX_UOPS_RETIRED_VECTOR_INTEGER",network.metrics,"instructions","L1.dcache.stores","L1.dcache.store.misses","FP_COMP_OPS_EXE_SSE_FP","FP_COMP_OPS_EXE_SSE_FP_SCALAR","LLC.loads")
# keepme <- c()
# idle.file <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid27/sampling2/idleWithVM_bscgrid27.csv"
# }
