PlotSimulatedPaths <- function(v.files,
                               v.titles,
                               outputPathCRISAM,
                               maxPathNumber = NULL){
  # This function creates multiple line plots with simulated paths, using each file in v.files for one plot and
  # melting the columns
  # Input
  #       v.files: vector of strings, containing csv file names (with extension) placed under path outputPathCRISAM
  #               ( # simulated time steps) x (# simulated paths)
  #       v.titles: vector of strings with the titles to be used in the plots
  #       outputPathCRISAM: path only where the data is saved
  #       maxPathNumber: integer, maximal number of paths to be plotted (to avoid messy plots)
  #                       if not provided all paths are plotted

  # Output: list of ggplots l.g with length equal to length of v.files

  # initialize list for output
  l.g <- list()
  
  for(i in 1:length(v.files)){
    m.tmp <- read.csv2(file = paste0(outputPathCRISAM,  v.files[i]))[, -1]
    if (!is.null(maxPathNumber)){
      minCols <- min(maxPathNumber, dim(m.tmp)[2])
      m.tmp <- m.tmp[, 1:minCols]
    }
    
    simPathLength <- dim(m.tmp)[1]
    dt.tmp <- data.table(m.tmp)[, Steps := seq(1:simPathLength)]
    a <- data.table::melt.data.table(dt.tmp, id = "Steps")
    g <- ggplot(a,aes(x = Steps, y=value, color = variable))+
      geom_line(show.legend = FALSE) +
      ggtitle(v.titles[i]) +
      xlab("Time steps") +
      ylab("Simulated price EUR/MWh")
    
    l.g <- c(l.g, list(g))
  }
  return(l.g)
}