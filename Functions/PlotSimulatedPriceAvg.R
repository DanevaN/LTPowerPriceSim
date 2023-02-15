PlotSimulatedPriceAvg <- function(v.index,
                               outputPathSim
                               ){
  # This function creates multiple line plots with simulated paths, using each file in v.files for one plot and
  # melting the columns
  # Input
  #       index: which simulated price index to plot
  #       outputPathSim: path and filename where the RDA for simulated prices is (simPathLength x chunkSize x d)
  
  # Output: list of ggplots l.g with length equal to length of v.files

  # initialize list for output
  
  load(outputPathSim)
  
  dt.tmp <- data.table(apply(ar.simPrices[, , (v.namesPrices %in% v.index)],
        MARGIN = c(1,3), mean))
    
  simPathLength <- dim(dt.tmp)[1]
  dt.tmp[, Steps := seq(1:simPathLength)]
  a <- data.table::melt.data.table(dt.tmp, id = "Steps")
  g <- ggplot(a,aes(x = Steps, y=value, color = variable))+
      geom_line(show.legend = TRUE) +
      ggtitle("Durchschnittliche Preissimulation") +
      xlab("Time steps") +
      ylab("Simulated price EUR/MWh")
    
 
  return(g)
}