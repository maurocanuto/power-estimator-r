# if (server == "bscgrid29"){
#   # Remove metrics with high correlation before building model
#   removeCorrelatedVars <- FALSE
#   
#   R_VALUE <- 0.95
#   PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid29/sampling2/model_coefficients/"
#   VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid29/sampling2/output_files_validation/joined/"
#   cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23","Core_24","Core_25","Core_26","Core_27","Core_28","Core_29","Core_30","Core_31")
#   filter <- c("page_in","page_out","swap_in","swap_out","interrupts","read_time","write_time","reads","writes","cpu_system","Core_1CPU","Core_2CPU","cpu_idle","cpu_nice","mem_shared","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","swap_free","numSockets","L2_RQSTS_ALL_CODE_RD","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS")
#   disk.metrics <- c("bytes_read","bytes_written")
#   network.metrics <- c("bytes_in","bytes_out")
#   filter2 <- c("powerWatts",filter,"L1.icache.load.misses","LLC.loads","L1.dcache.stores","load")
#   keepme <- c()
# }


# Remove metrics with high correlation before building model
  removeCorrelatedVars <- FALSE
  
  R_VALUE <- 0.95
  PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid29/sampling2/model_coefficients/"
  VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid29/sampling2/output_files_validation/joined/"
  cores <- c("Core_0","Core_1","Core_2","Core_3","Core_4","Core_5","Core_6","Core_7","Core_8","Core_9","Core_10","Core_11","Core_12","Core_13","Core_14","Core_15","Core_16","Core_17","Core_18","Core_19","Core_20","Core_21","Core_22","Core_23","Core_24","Core_25","Core_26","Core_27","Core_28","Core_29","Core_30","Core_31")
  filter <- c("page_in","page_out","swap_in","swap_out","interrupts","read_time","write_time","reads","writes","cpu_system","Core_1CPU","Core_2CPU","cpu_idle","cpu_nice","mem_shared","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","swap_free","numSockets","L2_RQSTS_ALL_CODE_RD","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS")
  disk.metrics <- c("bytes_read","bytes_written")
  network.metrics <- c("bytes_in","bytes_out", "pkts_in","pkts_out")
  filter2 <- c("powerWatts","cpu_user",filter,disk.metrics,network.metrics, "mem_buffers", "mem_cached")
  keepme <- c()
  idle.file <- "/home/mcanuto/BSC/bscgrid/experiments/bscgrid29/sampling2/idleVM_bscgrid30.csv"