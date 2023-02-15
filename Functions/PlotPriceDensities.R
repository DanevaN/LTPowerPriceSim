PlotPriceDensities <- function(v.files,
                               v.titles,
                               outputPathCRISAM){
  # This function creates multiple density plots, using each file in v.files for one plot and
  # melting the columns
  # Input
  #       v.files: vector of strings, containing csv file names (with extension) placed under path outputPathCRISAM
  #       v.titles: vector of strings with the titles to be used in the plots
  #       outputPathCRISAM: path only where the data is saved
  # Output: list of ggplots l.g with length equal to length of v.files
  v.dataStructures <- paste0("m.",gsub(".csv", "", v.files))
  
  # initialize list for output
  l.g <- list()

  for(i in 1:length(v.files)){
    m.tmp <- read.csv2(file = paste0(outputPathCRISAM,  v.files[i]))[, -1]
    simYears <- dim(m.tmp)[2]
    colnames(m.tmp) <- paste0("a+", c(1:simYears) - 1, sep = "")
    
    a <- data.frame(data.table::melt(as.data.table(m.tmp)))
    g <- ggplot(a, aes(x = value, fill = variable)) + 
      geom_density(alpha=0.2, position="identity") +
      ggtitle(v.titles[i]) +
      xlab("Simulated price EUR/MWh")
    print(i)
    g
    l.g <- c(l.g, list(g))
  }
  return(l.g)
  }