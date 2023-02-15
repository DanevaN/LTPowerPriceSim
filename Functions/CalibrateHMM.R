CalibrateHMM <- function(v.data, nStates, stand = TRUE){
  # First differences the data. Then standardizes it, then:
  # calibrates the parameters of HMM with Gaussian emissions to a one dimensional data
  # calculates the stationary distribution of the transition matrix
  # calculates the most probable state sequence behind the observed data
  # using Viterbi
  # depends on depmixS4 and HiddenMarkov libraries
  # inputs:
  #       v.data: numeric vector containing the data 
  #       nStates: number of states
  # outputs: list of
  #       df.dataStats: absolute and differenced mean and stDev of data 
  #       df.stateStats: learnt state stats  absolute and differenced mean and stDev
  #                      in each state
  #       v.viterbi: inferenced Viterbi sequence 
  #       v.pi: stationary distribution of the Markov Chain 
  #       m.transMat: transition matrix learnt of the Markov chain 
  #       m.params: states x (mean, stdev) of the emmission distribitions for each state 
  
  # 1. Prepare data: if stand is TRUE standardize standardize via(x-mu)/sigma
  # save scaling parameters to rescale and/or return
  absMean <- mean(v.data)
  absStDev <- sd(v.data)
  # save stats to return them
  df.dataStats <- data.frame(absMean = absMean, 
                             absStDev = absStDev) 
  if (stand == TRUE){
  v.standData <- scale(v.data)
    data <- data.frame(returns = v.standData )
  } else {
    data <- data.frame(returns = v.data)
  }
  
  hmm <- depmixS4::depmix(returns ~ 1, family = gaussian(), nstates = nStates, 
                          data = data)
  hmmfit <- depmixS4::fit(object = hmm, verbose = FALSE)
  v.viterbi <- viterbi(hmmfit)$state
  
  #get parameters
  v.hmmPars <- getpars(hmmfit)
  m.transMat = matrix(v.hmmPars[(nStates + 1) : ((nStates + 1) * nStates)], ncol = nStates, byrow = T)    
  m.params = matrix(v.hmmPars[((nStates + 1) * nStates + 1) : length(v.hmmPars)], nrow = nStates, byrow = T) # (# states x (mean, stDev))
  
  # find stationary distribution
  v.pi <- CalculatePi(m.transMat, useEigenDecomposition = FALSE)
  
  #find rescaled parameters learnt for each state, these will be the learnt mean and stdev for each state 
  # of the ABSOLUTE DIFFERENCES of prices, as this was the time series on which the hmm ran
  if (stand == TRUE){
   v.absMeans <- m.params[, 1] * absStDev + absMean
   v.absStDevs <- m.params[, 2] * absStDev
  } else {
   v.absMeans <- m.params[, 1] 
   v.absStDevs <- m.params[, 2] 
  }
  #take statistics of learnt sequence in difference
  v.diffMeans <- v.diffStDevs <- vector(mode = "numeric", length = nStates)
  for (i in 1:nStates){
    v.diffMeans[i] <- mean(diff(v.data)[v.viterbi[2:length(v.viterbi)] == i])
    v.diffStDevs[i] <- sd(diff(v.data)[v.viterbi[2:length(v.viterbi)] == i])
  }
  
  #save learnt state stats to return them
  df.stateStats <- data.frame(absMeans = v.absMeans,
                              absStDevs = v.absStDevs,
                              diffMeans = v.diffMeans, 
                              diffStDevs = v.diffStDevs)
  
  return(list(df.dataStats = df.dataStats, 
              df.stateStats = df.stateStats, 
              v.viterbi = v.viterbi, 
              v.pi = v.pi, 
              m.transMat = m.transMat, 
              m.params = m.params,
              logLik = as.numeric(logLik(hmmfit))))
}

