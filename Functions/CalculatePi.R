# get stationary distribution for matrix P

CalculatePi <- function(P, useEigenDecomposition = TRUE){
  # from a given transition matrix P of a Markov chain calculate the stationary distribution
  # if useEigenDecomposition is TRUE calculation via eigenvectors, 
  # if not -> numerically
  
  ind <- NULL
  if (useEigenDecomposition){
    # estimate left eigenvectors, we need ginv to convert from standard right vectors to left eigenvectors
    ev <- MASS::ginv(eigen(P)$vectors)
    # estimate which index has the 1 eigenvalue
    ind <- which(round(eigen(P)$values, 10) == 1)
    
    # if there is no eigenvalue 1
    if (length(ind)==0){
      print("Error, matrix is has no eigenvalue = 1! Trying numerically..")
    } else {
      pi <- ev[ind,] / sum(ev[ind,])
    }
   
  } 
  if (length(ind) == 0 | !useEigenDecomposition) {
  # numerical  
    Pstat <- P
    for (i in 1:100000){
      Pprev <- Pstat
      Pstat <- Pstat %*% P
      i <- i + 1
      err <- sum(colSums((Pstat - Pprev)^2))
      if (is.na(err)){
        print(paste("Error, matrix is not irreducible or recurrent! Iteration when stopped:",i))
        break
      }
      else if (err < 0.00000001){
        print(paste("Convergence at iteration:",i))
        break
      }
      
    }
    pi <- Pstat[1,]
  }
  
  return(pi)
}
