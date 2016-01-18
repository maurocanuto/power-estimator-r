
#if (server == "vm_renewit"){
  
  # # Remove metrics with high correlation before building model
  removeCorrelatedVars <- FALSE
  
  coresSum <- FALSE
  R_VALUE <- 0.95
  PATH <- "/home/mcanuto/BSC/bscgrid/experiments/VM_experiments/model_training/"
  VALIDATION_PATH <- "/home/mcanuto/BSC/bscgrid/experiments/VM_experiments/model_validation/" 
  cores <- c()
  
  filter <- c("L1.icache.load.misses","L2_RQSTS_ALL_CODE_RD","branches","branch.misses","cpu.cycles","cpu.migrations", "vdisk_errs", "vdisk_free", "vdisk_total", "vmem_util", "vdrops_in", "vdrops_out", "verrs_in", "verrs_out", "L2_RQSTS_CODE_RD_HIT","L2_RQSTS_CODE_RD_MISS","FP_COMP_OPS_EXE_SSE_PACKED_SINGLE")
  #   disk.metrics <- c("bytes_read","bytes_written")
  #   network.metrics <- c("bytes_in","bytes_out", "pkts_in", "pkts_out")
  #   filter2 <- c("powerWatts", filter)
  disk.metrics <- c("vbytes_read","vbytes_written")
  network.metrics <- c("vbytes_in","vbytes_out")
  filter2 <- c("powerWatts", filter)
  keepme <- c(network.metrics, disk.metrics)
  
  
  
#}
