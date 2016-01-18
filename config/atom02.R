# if (server == "atom02"){
  coresSum <- TRUE
  # Remove metrics with high correlation before building model
  removeCorrelatedVars <- FALSE
  
  R_VALUE <- 0.9
  PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Atom/Data/model_training/"
  VALIDATION_PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Atom/Data/model_validation/"
  cores <- c("Core_0","Core_1","Core_2","Core_3")
  filter <- c("swap_in","swap_out","mem_cached","interrupts","read_time","write_time","reads","writes","Core_1CPU","Core_2CPU","mem_shared","cpu_intr", "cpu_sintr","swap_free","numSockets","load_one", "load_five", "load_fifteen","proc_run","proc_total")
  #filter <- c("Timestamp", "interrupts", "branch.instructions", "cache.references", "L1.icache.loads", "branch.loads", "cpu_user")
  disk.metrics <- c("bytes_read","bytes_written")
  network.metrics <- c("bytes_in","bytes_out","pkts_in","pkts_out")
  filter_extra <- c("ref.cycles","cache.references","branches","dTLB.loads","dTLB.load.misses","dTLB.stores","dTLB.store.misses","iTLB.loads","iTLB.load.misses","branch.loads","contexts","cpu_idle","cpu_nice","cpu_wio","mem_buffers")
  filter_extra2 <- c("BUS_TRANS_MEM" ,        
                     "L2_REQUESTS" ,                         
                     "L2_MISSES"  ,                         
                     "L2_STORES"   ,
                     "L1.icache.loads"   ,                 
                     "L1.icache.load.misses",
                     "cpu_system" ,                         
                     "branch.load.misses" ,
                     "branch.misses"  ,
                     "SIMD_COMP_INST_RETIRED_PACKED_DOUBLE" ,
                     "SIMD_COMP_INST_RETIRED_SCALAR_DOUBLE"   ,
                     "SIMD_INST_RETIRED_PACKED_SINGLE"  ,
                     "SIMD_INST_RETIRED_SCALAR_SINGLE"    ,
                     "SIMD_INST_RETIRED_PACKED_DOUBLE"  ,
                     "SIMD_INST_RETIRED_VECTOR"   ,
                     "X87_COMP_OPS_EXE_EXECUTED"   ,
                     "L1.dcache.stores",
                     "LLC.loads"  ,
                     "LLC.stores",
                     "bus.cycles"
  )
  filter2 <- c("powerWatts", filter, filter_extra,filter_extra2, network.metrics, "load")
  #filter2 <- c("powerWatts", filter)
  
  # LLC.loads
  keepme <- c()
# }

# if (server == "atom02"){
#   coresSum <- TRUE
#   # Remove metrics with high correlation before building model
#   removeCorrelatedVars <- FALSE
#   
#   R_VALUE <- 0.9
#   PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Atom/Data/model_training/"
#   VALIDATION_PATH <- "/home/mcanuto/BSC/JosepSubirats/PowerModel/Atom/Data/model_validation/"
#   cores <- c("Core_0","Core_1","Core_2","Core_3")
#   filter <- c("swap_in","swap_out","mem_cached","interrupts","read_time","write_time","reads","writes","Core_1CPU","Core_2CPU","mem_shared","cpu_intr", "cpu_sintr","swap_free","numSockets","load_one", "load_five", "load_fifteen","proc_run","proc_total")
#   #filter <- c("Timestamp", "interrupts", "branch.instructions", "cache.references", "L1.icache.loads", "branch.loads", "cpu_user")
#   disk.metrics <- c("bytes_read","bytes_written")
#   network.metrics <- c("bytes_in","bytes_out","pkts_in","pkts_out")
#   filter_extra <- c("ref.cycles","cache.references","branches","dTLB.loads","dTLB.load.misses","dTLB.stores","dTLB.store.misses","iTLB.loads","iTLB.load.misses","branch.loads","contexts","cpu_idle","cpu_nice","cpu_wio","mem_buffers")
#   filter_extra2 <- c("BUS_TRANS_MEM" ,        
#                      "L2_REQUESTS" ,                         
#                      "L2_MISSES"  ,                         
#                      "L2_STORES"   ,
#                      "L1.icache.loads"   ,                 
#                      "L1.icache.load.misses",
#                      "cpu_system" ,                         
#                      "branch.load.misses" ,
#                      "branch.misses"  ,
#                      "SIMD_COMP_INST_RETIRED_PACKED_DOUBLE" ,
#                      "SIMD_COMP_INST_RETIRED_SCALAR_DOUBLE"   ,
#                      "SIMD_INST_RETIRED_PACKED_SINGLE"  ,
#                      "SIMD_INST_RETIRED_SCALAR_SINGLE"    ,
#                      "SIMD_INST_RETIRED_PACKED_DOUBLE"  ,
#                      "SIMD_INST_RETIRED_VECTOR"   ,
#                      "X87_COMP_OPS_EXE_EXECUTED"   ,
#                      "L1.dcache.stores",
#                      "LLC.loads"  ,
#                      "LLC.stores",
#                      "bus.cycles"
#   )
#   filter2 <- c("powerWatts", filter, filter_extra,filter_extra2,network.metrics, "load")
#   #filter2 <- c("powerWatts", filter)
#   
#   # LLC.loads
#   keepme <- c()
# }
# 
