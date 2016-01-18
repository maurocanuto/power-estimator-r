
#if (server == "arm"){
  coresSum <- TRUE
  # Remove metrics with high correlation before building model
  removeCorrelatedVars <- FALSE
  
  R_VALUE <- 0.93
  PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Arndale/Data/model_training/"
  VALIDATION_PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Arndale/Data/model_validation/"
  cores <- c("Core_0","Core_1")
  filter <- c("mem_cached","mem_buffers","page_in","page_out","swap_in","swap_out","interrupts","read_time","write_time","reads","writes","cpu_user","cpu_system","Core_1CPU","Core_2CPU","cpu_idle","cpu_nice","mem_shared","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","swap_free","numSockets","L2_RQSTS_ALL_CODE_RD","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS", "load_one", "load_five", "load_fifteen","proc_run","proc_total")
  #filter <- c("Timestamp", "interrupts", "branch.instructions", "cache.references", "L1.icache.loads", "branch.loads", "cpu_user")
  disk.metrics <- c("bytes_read","bytes_written")
  network.metrics <- c("bytes_in","bytes_out")
  filter_extra <- c("instructions","LLC.store.misses","INST_SPEC_EXEC_LOAD","INST_SPEC_EXEC_STORE","INST_SPEC_EXEC_LOAD_STORE","INST_SPEC_EXEC_INTEGER_INST","L1.dcache.loads","L1.icache.loads","L1.icache.load.misses","iTLB.load.misses", "branch.loads", "branch.load.misses", "INST_SPEC_EXEC_LDREX", "INST_SPEC_EXEC_STREX_PASS","INST_SPEC_EXEC_STREX_FAIL", "branches","branch.misses","cache.references", "INST_SPEC_EXEC_SOFT_PC", "dTLB.load.misses", "dTLB.store.misses")
  filter_extra2 <- c()
  filter2 <- c("powerWatts", filter, filter_extra, filter_extra2, "LLC.stores","L1.dcache.load.misses","contexts")
  
  # LLC.loads
  keepme <- c()
#}
#OOOOOOK
# if (server == "arm"){
#   coresSum <- TRUE
#   # Remove metrics with high correlation before building model
#   removeCorrelatedVars <- FALSE
#   
#   R_VALUE <- 0.9
#   PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Arndale/Data/model_training/"
#   VALIDATION_PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Arndale/Data/model_validation/"
#   cores <- c("Core_0","Core_1")
#   filter <- c("page_in","page_out","swap_in","swap_out","interrupts","read_time","write_time","reads","writes","cpu_user","cpu_system","Core_1CPU","Core_2CPU","cpu_idle","cpu_nice","mem_shared","cpu_wio","cpu_intr", "cpu_sintr","pkts_in","pkts_out","swap_free","numSockets","L2_RQSTS_ALL_CODE_RD","L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS", "load_one", "load_five", "load_fifteen","proc_run","proc_total")
#   #filter <- c("Timestamp", "interrupts", "branch.instructions", "cache.references", "L1.icache.loads", "branch.loads", "cpu_user")
#   disk.metrics <- c("bytes_read","bytes_written")
#   network.metrics <- c("bytes_in","bytes_out")
#   filter_extra <- c("instructions","INST_SPEC_EXEC_LOAD","INST_SPEC_EXEC_STORE","INST_SPEC_EXEC_LOAD_STORE","INST_SPEC_EXEC_INTEGER_INST","L1.dcache.loads","L1.icache.loads","L1.icache.load.misses","iTLB.load.misses", "branch.loads", "branch.load.misses", "INST_SPEC_EXEC_LDREX", "INST_SPEC_EXEC_STREX_PASS","INST_SPEC_EXEC_STREX_FAIL", "branches","branch.misses","cache.references", "INST_SPEC_EXEC_SOFT_PC", "dTLB.load.misses", "dTLB.store.misses")
#   filter_extra2 <- c("LLC.store.misses")
#   filter2 <- c("powerWatts", filter, filter_extra, filter_extra2, disk.metrics)
#   keepme <- c()
# }
