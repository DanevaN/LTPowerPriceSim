PlotPriceDistribution <- function(stepPlot, dt.priceData, ar.sim){
  # Plots simulated rice densities vs. historical price densities for each
  # index in the vector v.names containing Y and Spot (which are power calendar 
  # products and spot products)
  # Input:
  #       stepPlot  integer containing the time step at which the simulated prices are taken
  #       dt.priceData data table containing historical prices in columns
  #       ar.sim  3d array, chunk of presimulated prices (simPathLength x chunkSize x d)
  # Returns  l.g list of ggplots with historical vs. simulated price density for each priceindex
  # get dimensions aand names of simulated indices
  simPathLength <- dim(ar.sim)[1]
  simIter <- dim(ar.sim)[2]
  sampleSize <- dt.priceData[, .N]
  v.names <- dimnames(ar.sim)[[3]]
  v.cols <- c(v.names[grepl("Y", v.names)], v.names[grepl("Spot", v.names)])
  m.prices <- as.matrix(dt.priceData[, ..v.cols])
  l.g <- list()
  for (i in v.cols){
    idx <- which(v.names == i)
    m.sim <- ar.sim[, , idx]
    df.hist <- data.frame(sample =  c(m.prices[, i], m.sim[stepPlot, ]), 
                          label = c(rep(v.names[idx], sampleSize), 
                                    rep(paste("Sim", v.names[idx]), simIter))
    )
    g <- ggplot(df.hist , aes(x = sample, fill = label)) + 
      geom_density(alpha=0.2, position="identity") 
    l.g <- c(l.g, list(g))
  }
  return(l.g)
}