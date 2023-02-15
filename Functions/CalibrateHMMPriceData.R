CalibrateHMMPriceData <- function(priceDataPath,   
                            outputPathHMM, nStates =2
                            ) {
  # ATTENTION: function is DIRTY (writes data to disk)
  # This function perfoms part one of price simulation model: the HMM calimbration
  # The model consists of two parts: HMM to learn the hidden states of the time series
  # and Ornstein-Uhlenbeck process to simulate prices within each state
  # In this function the first step is accomplished: HMM models are calibrated for each provided
  # price tie series in the data set
  # Dependancies: libraries depmixS4, MultiRNG, stats
  # Inputs:
  #       priceDataPath path to where files with prices are
  #       nStates: number of hidden states to be learnt in HMM
  #       outputPathHMM: string, path and filename for the saving of outputs

  #      priceSaving: if to save prepared price data structures
  #       
  # Outputs:  none
  #   Following lists are saved under outputPathHMM. 
  #     Each element of the list i  the i-th product in v.names:
  #     l.dataStats: list of d   df.dataStats: absolute and differenced mean and stDev of data 
  #     l.stateStats: list of d   df.stateStats: learnt state stats  absolute and differenced mean and stDev
  #                      in each state
  #     l.viterbi: list of d    v.viterbi: inferenced Viterbi sequence 
  #     l.pi: list of d    v.pi: stationary distribution of the Markov Chain 
  #     l.transMat: list of d   m.transMat: transition matrix learnt of the Markov chain 
  #     l.params: list of d   m.params: states x (mean, stdev) of the emmission distribitions for each state 
  #     l.HMMParams: list of input paramneters used for starting the calibration
  #     v.names: character vector with the names of the products corresponding to the provided 
  #              list of calibrated parameters
  #     v.startPrices: numeric vector with the last row of the price data provided used for starting
  #             the simulation
  #
 
  # get data
  base::load(paste0(priceDataPath, "priceData.rda"))
  
  # store names of time series
  v.names <- colnames(dt.priceData)[-1]
  #store last prices for starting simulation
  v.startPrices <-as.matrix( dt.priceData[.N, -1, with = FALSE])[1, ]
  # get number of products to simulate
  d <- length(v.names)
  
  # initialize a matrix to put the inferred Viterbi state sequences for 
  # correlation calculation afterwards
  nObs <- dt.priceData[,.N] 
  
  m.allViterbi <- matrix(numeric(nObs * d), nrow = nObs, ncol = d)
  
  # we create a list of the data structures where element i contains the
  # information for the ith product
  l.HMMOutput <- vector(mode = "list", length = d)
  l.dataStats <- vector(mode = "list", length = d)
  l.stateStats <- vector(mode = "list", length = d)
  l.viterbi <- vector(mode = "list", length = d)
  l.pi <- vector(mode = "list", length = d)
  l.transMat <- vector(mode = "list", length = d)
  
  # first calibrate HMM separately for each price product in a loop
  for (i in 1:d){
    #debug 
    print(paste("Calibrate HMM for",v.names[i]))
    # get time series for the product as vector
    v.data <- as.matrix(dt.priceData[, -1])[, i]
    # get result in a temp list
    l.results <-  CalibrateHMM (v.data = v.data, nStates = nStates, stand = TRUE)
    # start assigning to enumerated data structure. One number for each time series
    #l.HMMOutput[[i]] <- l.results
    l.dataStats[[i]] <- l.results[[1]]
    l.stateStats[[i]] <- l.results[[2]]
    l.viterbi[[i]] <- l.results[[3]]
    l.pi[[i]] <- l.results[[4]]
    l.transMat[[i]] <- l.results[[5]]
    
    # prepare a matrix of the inferred state sequences to estimate correlations from
    m.allViterbi[, i] <- l.viterbi[[i]]
  }
  names(l.dataStats) <- v.names
  names(l.stateStats) <- v.names
  names(l.viterbi) <- v.names
  names(l.pi) <- v.names
  names(l.transMat) <- v.names
  
  ############################# Prepare correlatation matrix for states and prices #####################################
  m.corrViterbi <- cor(m.allViterbi) #diag(d) 
  m.corrPrices <- cor(dt.priceData[,-1]) #first column is date
  m.covViterbi <- cov(m.allViterbi) #diag(d) 
  m.covPrices <- cov(dt.priceData[,-1]) #first column is date
  ##saving ########################
  # Document all parameters of the simulation
  l.HMMParams <- list(priceDataPath = priceDataPath,  
                      outputPathHMM = outputPathHMM,
                      nStates = nStates
                      )
  
  # output
  v.output <- vector(mode = "character", length = 10)
  v.output[1] <- "l.HMMParams"
  v.output[2] <- "l.dataStats"
  v.output[3] <- "l.stateStats"
  v.output[4] <- "l.viterbi"
  v.output[5] <- "l.pi"
  v.output[6] <- "l.transMat"
  v.output[7] <- "v.names"
  v.output[8] <- 'v.startPrices'
  v.output[9] <- "m.corrViterbi"
  v.output[10] <- "m.corrPrices"
  v.output[11] <- "m.covViterbi"
  v.output[12] <- "m.covPrices"
  save(list = v.output, file = outputPathHMM)
  
}
