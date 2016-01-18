# 
# #if (server == "renewit02"){
#   
#   # Remove metrics with high correlation before building model
#   removeCorrelatedVars <- FALSE
#   
#   R_VALUE <- 0.94
#   PATH <- "/home/mcanuto/BSC/bscgrid/experiments/RenewIT/renewit02/model_coefficients/"
#   VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/dataR/renewIT/validation/" 
#   cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23","Core_24","Core_25","Core_26","Core_27","Core_28","Core_29","Core_30","Core_31")
#   filter <- c("cpu_system","cpu_nice","write_time","read_time","cpu_wio","cpu.migrations","mem_shared","interrupts","cpu_intr", "cpu_sintr","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets")
#   #"load","FP_COMP_OPS_EXE_X87",
#   disk.metrics <- c("bytes_read","bytes_written")
#   network.metrics <- c("bytes_in","bytes_out")
#   filter2 <- c("powerWatts", filter,"load","contexts", "branches" , "branch.misses", "instructions","L1.dcache.load.misses", "LLC.load.misses", "cpu.cycles","SIMD_FP_256_PACKED_DOUBLE","SIMD_FP_256_PACKED_SINGLE","LLC.load.misses","LLC.store.misses","FP_COMP_OPS_EXE_SSE_FP_SCALAR_SINGLE")
#   
#   keepme <- c()
#   
# #}



# if (server == "renewit02"){
#   
#   # Remove metrics with high correlation before building model
#   removeCorrelatedVars <- FALSE
#   
#   R_VALUE <- 0.9
#   PATH <- "/home/mcanuto/BSC/bscgrid/experiments/RenewIT/renewit02/model_coefficients/"
#   VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/dataR/renewIT/validation/" 
#   cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23","Core_24","Core_25","Core_26","Core_27","Core_28","Core_29","Core_30","Core_31")
#   filter <- c("cpu_nice","cpu_user","cpu_system","cpu_idle","L2_RQSTS_CODE_RD_HIT","write_time","read_time","cpu_wio","cpu.migrations","mem_shared","interrupts","cpu_intr", "cpu_sintr","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS","FP_COMP_OPS_EXE_SSE_PACKED_SINGLE")
#   #"load","FP_COMP_OPS_EXE_X87",
#   disk.metrics <- c("bytes_read","bytes_written")
#   network.metrics <- c("bytes_in","bytes_out", "pkts_in", "pkts_out")
#   filter2 <- c("powerWatts", filter, network.metrics)
#   keepme <- c(disk.metrics, network.metrics, "cpu_user")
#   
# }


# if (server == "renewit02"){

# Remove metrics with high correlation before building model
removeCorrelatedVars <- FALSE

R_VALUE <- 0.94
#PATH <- "/home/mcanuto/BSC/bscgrid/experiments/RenewIT/renewit02/model_coefficients/"
PATH <- "/home/mcanuto/BSC/bscgrid/experiments/RenewIT/joined/"
VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/dataR/renewIT/validation/" 
cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23","Core_24","Core_25","Core_26","Core_27","Core_28","Core_29","Core_30","Core_31")
filter <- c("reads", "writes","cpu_nice","cpu_system","cpu_idle","L2_RQSTS_CODE_RD_HIT","write_time","read_time","cpu_wio","cpu.migrations","mem_shared","interrupts","cpu_intr", "cpu_sintr","page_in","page_out","swap_in","swap_out","swap_free","Core_1CPU","Core_2CPU","numSockets","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS","L2_RQSTS_ALL_CODE_RD","FP_COMP_OPS_EXE_SSE_PACKED_SINGLE")
#"load","FP_COMP_OPS_EXE_X87",
disk.metrics <- c("bytes_read","bytes_written")
network.metrics <- c("bytes_in","bytes_out", "pkts_in", "pkts_out")
ftest <- c("load","contexts","branch.misses","cpu_user","SIMD_FP_256_PACKED_SINGLE", "SIMD_FP_256_PACKED_DOUBLE", "instructions", "cpu.cycles")
filter2 <- c("powerWatts", ftest, filter, disk.metrics, network.metrics,"mem_cached", "branches", "mem_buffers")
keepme <- c()
idle.file <- "/home/mcanuto/BSC/bscgrid/experiments/RenewIT/renewit04/idleWithVM.csv"
# }

