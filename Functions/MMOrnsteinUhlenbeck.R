MMOrnsteinUhlenbeck <- function(m.params, deltat, 
                                      m.transMat, v.startState, m.uniform = NULL, 
                                      startX, m.normal = NULL,
                                      simPathLength, simIter, volInc = 0){
  #function returns simulation of a single path of a mixture of two Brownians
  #Specifically: we simulate X as the result of:
  # 1. take a markov chain S with 2 states and simulate it for simPathLength steps starting from state = v.startState
  # 2. take two set of parameters for Ornstein-Uhlenebeck process of the mean reverting form
  # dXt = theta * (mu - Xt) * deltat + sigma * dWt
  # Discretization is with delta t = deltat 
  # (depending on the units the output will come in these time steps. E.g. deltat = 1/365 the steps are daily but time is years)
  # 3. Start simulation from X0 = startX via
  # - observe state St and choose the correct set of paramters
  # then draw random normal number multiply by sigma then add theta * (mu - previous simulated value)
  # 
  # input:
  #         m.params: matrix (nStates x 3) every row represents (theta, mu, sigma) for each state
  #                   where theta is mean reversion speed
  #         m.transMat: 2 x 2 transition matrix for the Markov chain
  #         v.startState:  vector (1 x # states) of probabilities to sample the start state
  #                     e.g. if we provide c(1,0) start state will be 1
  #         m.uniform: optional matrix (simPathLength x simIter) uniform(0,1) 
  #                   to generate the state sequence of the Markov chain from
  #         startX: start value for our process
  #         m.normal: optional matrix (simPathLength x simIter) normal(0,1) 
  #                   to generate the Wt term of the Ornstein Uhlenbeck process from
  #         simPathLength: how many time steps are simulated for X
  #         simIter: how many simulation paths are simulated
  #         volInc: deterministic increment in volatility EUR/MWh
  # output:
  #  matrix(simPathLength x simIter) with the simulated values of the compount process X
  
  #initialize S and X (simPathLength x simIter)
  X <- S <- matrix(numeric(simPathLength * simIter), nrow = simPathLength, ncol = simIter)#vector(mode = "numeric", length = simPathLength)
  X[1, ] <- round(startX, 2)
  
  # group parameters in (nStates x 1) vectors by type: 
  v.theta <- m.params[, 1]
  v.mu <- m.params[, 2]
  #print(m.params)
  
  # long term Ornstein Uhenbeck is normal (mu, sigma^2 / (2*theta))
  # this means we have to retrieve the volatility parameter sigma from variance
  v.sigma <- m.params[, 3] 
  
  # loop over the time steps, process is recursive and we cannot vectorize
  # 1. sample initial state from v.startState
  # repeat cumulative probabilities rows into matrix (simIter x nStates)
  s <- Rfast::rep_row(cumsum(v.startState), simIter)
  if (is.null(m.uniform)){
    # use random uniform for sample
    S[1, ] <- Rfast::rowMaxs((runif(simIter) < s) * 1, value = FALSE)
  } else {
    # use provided uniform for sample
    S[1, ] <- Rfast::rowMaxs((m.uniform[1, ] < s) * 1, value = FALSE)
  }
  
  # 2. go for all other time steps
  for (i in 2:simPathLength){
    # for each row of transMat corresponding to the previuos states S[i-1] take cumsum per row
    s <- t(apply(m.transMat[S[i - 1, ], ], 1, cumsum))
    if (is.null(m.uniform)){
      # if now uniform sample provided take the index of the first hit of a uniform sample
      # vector in each row of s (draw from multinomial distribution) for the next index
      S[i,] <- Rfast::rowMaxs((runif(simIter) < s) * 1, value = FALSE)
    } else {
      # take provided unifrom sample and drow from multinomial with it
      # always the first hit of the uniform random variable in the
      # cumulative sum vector of the states
      S[i,] <- Rfast::rowMaxs((m.uniform[i, ] < s) * 1, value = FALSE)
    }
    
    # conditioned on the state in the second dimension simIter get the correct parameters for the
    # Ornstein Uhlenbek process
    v.condeltatheta <- v.theta[S[i,]] #(1 x simIter)
    v.condMu <- v.mu[S[i,]]  #(1 x simIter)
    # attention: added determiistic increments in volatility of 15 EUR/MWh per year
    v.condSigma <- v.sigma[S[i,]] + volInc/365*sqrt(i) #(1 x simIter)
    
    if (is.null(m.normal)){
      # simulate standard normal
      v.epsilon <- rnorm(simIter) #(1 x simIter)
    } else {
      v.epsilon <- m.normal[i, ]
    }
    
    # calculate the next step for the Ornstein Ulenbeck process
    X[i,] <- round(
      X[i-1, ] + v.condeltatheta * (v.condMu -  X[i-1, ]) * deltat + v.condSigma * deltat^0.5 * v.epsilon ,
      2)
  }
  return(X)
}


