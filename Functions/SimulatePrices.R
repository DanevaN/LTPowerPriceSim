SimulatePrices <- function(outputPathHMM, outputPathSim, priceMappingPath,
                           simPathLength, simIter, 
                           deltat, m.theta = NULL,  v.startPrices = NULL,
                           v.simNames = NULL,
                           rseed = 15, 
                           v.volInc = c(0,0),
                           excludeQuarter = 1
                           ){
  # ATTENTION: function is DIRTY (writes data to disk)
  # This function perfoms part two of price simulation model: the simulation
  # The model consists of two parts: HMM to learn the hidden states of the time series
  # and Ornstein-Uhlenbeck process to simulate prices within each state
  # During simulation first a state sequence is sampled then conditionally on the state
  # a mean reverting price movement.
  # the correlations are preserved via correlation in the state sequences and correlations
  # of the white noise fed into the Ornstein Uhlenbeck equations.
  # Finally simulation results and calibrated parameters are saved in a data structure
  # Dependancies: libraries depmixS4, MultiRNG, stats
  # Inputs:
  #       outputPathHMM: string, path and filename where HMM calibrated output is saved
  #       outputPathSim: string, path and filename for the saving simulated prices
  #       priceMappingPath: string, path where price mapping csv files are stored
  #                       1. arSimPricesMapping.csv: mapping for calculating quarter prices out of cal and spread
  #                       2. arSimPricesOffPeakMapping.csv: for calculating off peak prices out of base and peak
  #       simPathLength: length of simulated path 
  #                     (measured in the time units in which deltat is defined. E.g.:
  #                     if we daily workdays timeseries in the data and we have delta t = 1
  #                     simPathLength = 250 then we will simulate 250 working days)
  #       simIter: number of simulations
  #       deltat: decimal, time step for Simulation
  #       m.theta: (nStates x d) mean reversion Parameter for every state and every time series:  1/20 means 
  #              that we expect to return to the mean on average in 20 time steps (as defined by delta t) 
  #       v.simNames: character vector, if a subset of the columns of dt.priceData which needs to be simulated
  #                 then the names of the products need to be provided. used in Trichter
  #       v.startPrices: vector of numbers -> start prices; dimension has to correspond to 
  #                      the number of price time series in priceData. if omitted last row of 
  #                      priceData is taken
  #                  e.g. columns: Pricedate	Base Y1	Base Y2	Base Y3	Peak Y1	Peak Y2
  #       outputPathSim: string, path and filename for the saving of outputs
  #       rseed: seed for the random numbers
  #       v.volInc: deterministic increment in volatility POWER and GAS cal 2 member vector (EUR/MWh per year)
  #       excludeQuarter: integer from 1 to 4. If we calculate all quarters quarter prices will not be 
  #                       artbitrage free as spreads add up to 0
  #                       0: calculate all quarters
  #                       1-4: exclude this quarter
  #                       5: no quarter calculation
  #       
  # Outputs:  none
  #   Following list is saved under outputPathSim. List of 
  #       1. ar.sim : multidimensional array (simPathLength x simIter x d)
  #         each ar.im[, , i] is a matrix m.sim (simPathLength x simIter) with simulated paths in columns.
  #         Paths do not correspond to prices, spreads are simulated as spreads.
  #         d corresponds to number of time series to simulate. E.g. we exclude a quarter, then we apply
  #         quarter spreads on each front year etc.It is the number of columns in dt.priceData - 1
  #       2. v.names: (1 x d) character vector of names of products corresponding to dim 3 of ar.sim 
  #       3. ar.simPrices: (simPathLength x simIter x d')
  #         each ar.im[, , i] is a matrix m.sim (simPathLength x simIter) with simulated paths in columns.
  #         d' corresponds to the number of products to return simulations for. It should be the sum of the provided price indices 
  #         (except AT spot - their history is not enough, they are modeled via regression on DE spot elsewhere) 
  #       4. v.namesPrices: (1 x d) character vector of names of products corresponding to dim 3 of ar.simPrices 
  #       5.l.simParams: list with all input parameters used for the simulation
  #
  # set seed for the random numbers
  set.seed(rseed)
  # copy of provided prices to be checked an taken later on. Start prices are also saved under outputPathHMM
  v.startPricesProvided <- v.startPrices
  
  # get data from calibrated HMMs (function CalibrateHMMPriceData)
  base::load(outputPathHMM)
  
  
  # if subset of the originally calibrated products needs to be simulated take subsets of all inputs
  # adjust all inputs so that only these products are there
  if (!is.null(v.simNames)){
    # create an index match
    v.idxMatch <- v.names %in% v.simNames
    # subset all inputs
    l.dataStats <- l.dataStats[v.idxMatch]
    l.stateStats <- l.stateStats[v.idxMatch]
    l.viterbi <- l.viterbi[v.idxMatch]
    l.pi <- l.pi[v.idxMatch]
    l.transMat <- l.transMat[v.idxMatch]
    v.names <- v.names[v.idxMatch]
    v.startPrices <- v.startPrices[v.idxMatch]
    m.corrViterbi <- m.corrViterbi[v.idxMatch, v.idxMatch]
    m.corrPrices <- m.corrPrices[v.idxMatch, v.idxMatch]
 
    
  }
  
  # dimensionality
  d <- length(v.names)
  ###################################### load price mappings from priceData #####################################
  ############################### to ar.simPrices and from ar.simPrices to ar.simPricesOffPeak ##################
  #load price mappings for getting type of product
  dt.priceDataMapping <- data.table::data.table(read.csv2(paste0(priceMappingPath,"priceDataMapping.csv"),
                                                                   colClasses = rep("character",3))
  )[priceData %in% v.names, ]
  # populate a vector with length v.names with the volatility increments
  v.volIncNames <- v.names %in% dt.priceDataMapping[Type %in% c("cal", "spot") & Product == "POWER", priceData] * v.volInc[1] + 
    v.names %in% dt.priceDataMapping[Type == "cal" & Product == "GAS", priceData] * v.volInc[2]
  #load price mappings for calculating quarter prices out of cal and spread 
  dt.arSimPricesMapping <- data.table::data.table(read.csv2(paste0(priceMappingPath,"arSimPricesMapping.csv"),
                                                colClasses = rep("character",3))
  )
  
  #load price mappings for calculating off peak prices out of base and peak
  dt.arSimPricesOffPeakMapping <- data.table::data.table(read.csv2(paste0(priceMappingPath,"arSimPricesOffPeakMapping.csv"),
                                                                   colClasses = rep("character",3))
  )
  # convert column of strings to column of character vectors
  dt.arSimPricesMapping[, priceData := sapply(priceData,
                                              function(x) strsplit(x, split ="\\,"))]
  dt.arSimPricesOffPeakMapping[, arSimPrices := sapply(arSimPrices,
                                              function(x) strsplit(x, split ="\\,"))]
 # take only those prices which can be calculated out of the provided names in v.simNames
  if (!is.null(v.simNames)){
    dt.arSimPricesMapping <- dt.arSimPricesMapping[sapply(priceData, 
                                                          function(x) all(x %in% v.simNames)), ]
      
    # take only those off peak products which correspond to the given simnames
    dt.arSimPricesOffPeakMapping <- dt.arSimPricesOffPeakMapping[sapply(arSimPrices, 
                                                                        function(x) all(x %in% dt.arSimPricesMapping[, arSimPrices])), ]
      
  }
  dt.arSimPricesMapping <- dt.arSimPricesMapping[IndexType == "quarter",]
  # convert to list 
  l.arSimPricesMapping <- as.list(dt.arSimPricesMapping[, priceData])
                                 
  l.arSimPricesOffPeakMapping <- as.list(dt.arSimPricesOffPeakMapping[, arSimPrices])
    
  # adjust the excludeQuarter control variable for the case that all quarter products were excluded
  excludeQuarter <- ifelse(dt.arSimPricesMapping[, .N] == 0, 0, excludeQuarter)
  
  ###########################################################################################################
  ############################# Prepare correlated uniform and correlated normal ###########################################
  ###########################################################################################################
  # we use correlations instead of covariances as we want to assume that the random variables are standardized
  # This is allowed because cov(standardized random variable) = cor(non standardized random variable)
  # Correlations have been precalulated and saved under outputPathHMM:
  # 1.m.corrViterbi : correlations of state sequences
  # 2.m.corrPrices : correlations of prices
  # build up a 3d array, to store simIter matrices each containing
  # uniform correlated sample where random variables are in columns
  # same for a correlated normal sample
  
  ar.uniform <- array(0, c(simPathLength, simIter, d))
  ar.normal <- array(0, c(simPathLength, simIter, d))  
  
  # fill arrays in loop over number of simulations
  # we need the loop as the random generator does not generate 3 dimensional arrays
  
  for (i in 1: simIter){
    if (d > 1){
      ar.uniform[, i, ] <- MultiRNG::draw.d.variate.uniform(simPathLength, d, m.corrViterbi)
      ar.normal[, i, ]  <- MultiRNG::draw.d.variate.normal(simPathLength, d, rep(0, d), m.corrPrices)
    } else {
      ar.uniform[, i, 1] <- stats::runif(simPathLength)
      ar.normal[, i, 1] <- stats::rnorm(simPathLength)
    }
    
    
  }
  
  ###########################################################################################################
  ############################## Perform simulations ##########################################################################################
  ###########################################################################################################
  # note mu has the meaning of a level to which the absolute price will converge with rate theta
  # if we consider the time step deltat to be 1 day, theta = 1/20 means that we assume that without
  # stochastic shocks the price will converge to this mu within 20 days
  
  # create a 3 d array the simulations for each product
  # here we swap two dimensions, for each product we have
  # a simulated matrix of size (simPathLength x simIter) and we have d such matrices
  ar.sim <- array(0, c(simPathLength, simIter, d + ifelse(excludeQuarter > 0, 2, 0)))
  if (excludeQuarter > 0 & excludeQuarter < 5){
    v.names <- c(v.names, "NA", "NA")
  }
  
  #check if start prices are provided, if yes take them otherwise take the loaded start prices
  # which correspond to  last row of dt.priceData
  if (!is.null(v.startPricesProvided)){
    v.startPrices <-v.startPricesProvided
  }
  # we simulate each product in a loop over the number of products
  if (is.null(m.theta)){ 
    calcTheta <- TRUE
    m.theta <- matrix(numeric(nStates * d), nrow = nStates, ncol = d)
  } else {calcTheta <- FALSE}
  
  for (i in 1:d){
    #print(paste("Simulate Prices for time series",v.names[i]))
    
    #calculate posterior of state distribution given that we start at a given price
    # use this for kicking off the simulation
    v.likelihood <- dnorm(x = v.startPrices[i], 
                          mean = l.stateStats[[i]]$absMeans,
                          sd = l.stateStats[[i]]$absStDevs)
    v.startState <- l.pi[[i]] *  v.likelihood / as.numeric(t(l.pi[[i]]) %*% v.likelihood)
    
    # if no long term theta is provided calculate it
    if (calcTheta){
      m.theta <- matrix(0, nrow = nStates, ncol = d) 
      theta <- t(v.startState) %*% l.stateStats[[i]]$absMeans / l.dataStats[[i]]$absMean #long term stationary mean of the modulated Ornstein Uhlenbeck
      #print(paste("time series", i, "theta estimated at",theta))
      m.theta[, i] <- theta 
    } 
    
    # build matrix of parameters for i-th time series only 
    m.params <- matrix(numeric(nStates * 3), nrow = nStates, ncol = 3)
    # make different logic for Spot: we use volatiit of absolute price differences for 
    # forwards as sigma of Ornstein Uhlenbeck, but modified absolute volatility for Spot
    m.params <- cbind(m.theta[, i], 
                      l.stateStats[[i]]$absMeans, 
                      sqrt(l.stateStats[[i]]$absStDevs^2 * 2 * m.theta[, i]) )
    # get the calibrated parameters for product i
    ar.sim[, , i] <- MMOrnsteinUhlenbeck(m.params = m.params,  
                                         deltat = deltat, 
                                         m.transMat = l.transMat[[i]], 
                                         v.startState = l.pi[[i]], 
                                         m.uniform = ar.uniform[, , i],
                                         startX = v.startPrices[i], 
                                         m.normal = ar.normal[, , i],
                                         simPathLength = simPathLength, 
                                         simIter = simIter,
                                         volInc =v.volIncNames[i])
  }
  ###########################################################################################################
  ################################### convert spreads to prices #############################################
  ###########################################################################################################
  ## create array to store prices simulation derived from spreads
  # get dimensions
  peakForwardCal <- dt.priceDataMapping[Type == "cal" & Product == "POWER" & Block == "BASE",.N] #sum(grepl("PY", v.names))
  baseForwardCal <- dt.priceDataMapping[Type == "cal" & Product == "POWER" & Block == "PEAK",.N] #sum(grepl("BY", v.names))
  gasForwardCal <- dt.priceDataMapping[Type=="cal"&Product=="GAS",.N] #sum(grepl("GY", v.names))
  
  calProducts <- peakForwardCal + baseForwardCal + gasForwardCal
  
  # quarters are recognized if there is the string Spread in cols of priceData
  peakForwardQ <- peakForwardCal * (sum(grepl("SpreadP", v.names)) + 
                                      ifelse(sum(grepl("SpreadP", v.names)) == 3, 1, 0))
  baseForwardQ <- baseForwardCal * (sum(grepl("SpreadB", v.names)) + 
                                      ifelse(sum(grepl("SpreadP", v.names)) == 3, 1, 0))
  # add quarters of Y0 which is current year, those are always 3  
  peakForward0Q <- sum(grepl("Spread0P", v.names))
  baseForward0Q <- sum(grepl("Spread0B", v.names))
    
  quarterProducts <- peakForwardQ + baseForwardQ + peakForward0Q + baseForward0Q
  
  # spots are recognized if there is the string Spot in cols of priceData
  spot <- sum(grepl("Spot", v.names)) 

  # total
  m <- calProducts + quarterProducts + spot 
  
  # initialize
  ar.simPrices <- array(0, c(simPathLength, simIter, m))
  v.namesPrices <- vector(mode = "character", length = m)
  # save calendar simulations
  ar.simPrices[, , 1:calProducts] <- ar.sim[, , which(grepl("Y", v.names) == TRUE)]
  v.namesPrices[1:calProducts] <- v.names[grepl("Y", v.names)]
  
  if (spot > 0) {
    #save spot
    ar.simPrices[, , (calProducts + 1): (calProducts + spot)] <- ar.sim[, , grepl("Spot", v.names)]
    v.namesPrices[(calProducts + 1): (calProducts + spot)] <- v.names[grepl("Spot", v.names)]
  }
  
  # only if quarter calculation necessary
  if (quarterProducts > 0) {
    # calculate missing quarter spread
    if (excludeQuarter > 0){
      missingQ <- paste0("Q", switch(excludeQuarter, "I", "II", "III", "IV"))
      ar.sim[, , d + 1] <- - apply(ar.sim[, , grepl("SpreadB", v.names)], MARGIN = c(1,2), sum)
      v.names[d + 1] <- paste0("SpreadB", missingQ)
      ar.sim[, , d + 2] <- - apply(ar.sim[, , grepl("SpreadP", v.names)], MARGIN = c(1,2), sum)
      v.names[d + 2] <- paste0("SpreadP", missingQ)
    }
    
    
    # calculate quarters
    ar.simPricesQ <- abind::abind(
      lapply(l.arSimPricesMapping,
             function(x) round(ar.sim[, , (v.names %in% x[1])] + ar.sim[, , (v.names %in% x[2])], 2)
               ),
             along = 3
      )
    
    v.namesPricesQ <- dt.arSimPricesMapping[IndexType == "quarter", arSimPrices]
    dimnames(ar.simPricesQ)[[3]] <- v.namesPricesQ
    #add them to common data structure
    v.namesPrices[(calProducts + spot + 1): m] <- v.namesPricesQ
    ar.simPrices[, , (calProducts + spot + 1): m] <- ar.simPricesQ
  }
  dimnames(ar.simPrices)[[3]] <- v.namesPrices
  dimnames(ar.sim)[[3]] <- v.names
  
  #########################################################################################
  # Calulate off peak products
  # by off peak = 2 * base - peak (approx)
  #########################################################################################
  # build data table with cartesian product of all simulated price names
  # create array (simPathLength x simIter x # off peak products) for off peak prices
  ar.simPricesOffPeak <- abind::abind(
    lapply(l.arSimPricesOffPeakMapping, 
           function(x) round(2 * ar.simPrices[, , (v.namesPrices %in% x[1])] - ar.simPrices[, , (v.namesPrices %in% x[2])], 2)), 
    along = 3) 
  v.namesPricesOffPeak <- sub('B', 'offP', dt.arSimPricesOffPeakMapping[, arSimPricesOffPeak])
  # take only names which were calculabel, i.e. both indices in the mapping were simulated
  v.idx <- abind::abind(
    lapply(l.arSimPricesOffPeakMapping, 
           function(x) any(v.namesPrices %in% x[1])& any(v.namesPrices %in% x[2]))
  )
  
  dimnames(ar.simPricesOffPeak)[[3]] <- v.namesPricesOffPeak[v.idx]
  
  ##saving ########################
  # Document all parameters of the simulation
  l.simParams <- list(
                      simPathLength = simPathLength, 
                      simIter = simIter,
                      deltat = deltat,
                      m.theta = m.theta,
                      v.startPrices = v.startPrices,
                      excludeQuarter = excludeQuarter)
  
  # output
  v.output <- vector(mode = "character", length = 6)
  v.output[1] <- "ar.sim"
  v.output[2] <- "v.names"
  v.output[3] <- "ar.simPrices"
  v.output[4] <- "v.namesPrices"
  v.output[5] <- "l.simParams"
  v.output[6] <- "ar.simPricesOffPeak"
  v.output[7] <- "v.namesPricesOffPeak"
  save(list = v.output, file = outputPathSim)
  
}
