#####################################################################################
################# Batch preparing simulated price data #################################
############################## for CRISAM ###########################################
#####################################################################################

# Author: Nadia Daneva
# Date first version: Dec 2020
# Date current version: Dec 2020
#####################################################################################
# Calibrate and simulate a number of correlated power prices.
# Documentation about the model
# https://confluence.kelag.at/display/COR/Roadmap+Implementierung?preview=%2F102989929%2F109937019%2FPreissimulation_DE.html

#####################################################################################
## Set up working directory to be the one from which the current script is started
#####################################################################################

# renv will take care to install all libraries in their correct versions
##### ATTENTION!!! When you first clone run this commande ONCE, choose 2. option
# renv::init()
renv::restore()

# taken over from stackoverflow for getting the current directory
stub <- function() {}
thisPath <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    # RStudio
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
  } else {
    stop("Cannot find file path")
  }
}

script.dir <- thisPath()
setwd(script.dir)
print(script.dir)


#source all functions in folder functions incl used libraries
v.functions <- list.files("./Functions/",pattern = "(.*)\\.R$" )
lapply(v.functions,function(x){source(paste0("./Functions/",x))})

#####################################################################################
# 2. Price simulation
# source("./MMOrnsteinUhlenbeck.R") #Markov Modulated Ornstein Uhlenbeck
# source("./CalculatePi.R")
# source("./CalibrateHMMPriceData.R")
# source("./CalibrateHMM.R")
# source("./SimulatePrices.R")
# source("./SimulateATSpot.R")
# source("./LoadDataPrices.R")
# source("./LoadAvgIKOrdersPrice.R")
# source("./ConnectToMontel.R")
# source("./LoadMontelData.R")


#####################################################################################
################# Destination for saving for CRISAM #################################
valDate <- 20201101
# if we want to copy existing content of the folder "./Output/"
# into a subfolder YYYYMMDD
copyOldOutput <- FALSE
valDatePrev <- 20200601
simYears <- 1 # number of full years to simulate after end of current year

# create different date formats needed
valDateDt <- as.Date(as.character(paste0(substr(valDate, 7, 8), ".",
                                         substr(valDate, 5, 6), ".", 
                                         substr(valDate, 1, 4))),
                     "%d.%m.%Y")
valDateStr <- as.character(valDate)
valDatePrevStr <- as.character(valDatePrev)

#for quarter product simulation, one quarter is always the result of presimulated
# calender year and 3 other quarters in order to have arbitrage free set of prices 
# for each future step
excludeQuarter <- 1 

################################### input paths ###################################
priceDataPath <- paste0(script.dir, "/PriceData", 
                        valDateStr,
                        "/")
priceMappingPath <- "./PriceMapping/"
################################### output paths ##################################
outputPath <- paste0(script.dir,"/Output/")
  
# calibrated HMM
outputPathHMM <- paste0(outputPath, 
                        "HMM_", 
                        valDateStr,
                        ".RDA")
# prices
# here no extension as chunk number needs to be added as well, so extension is added at the call
outputPathSim <- paste0(outputPath,
                        "PriceSim_", 
                        valDateStr,
                        "_")
################# Copy current csv files to new folder ##############################
currentFolder <- outputPath
newFolder  <- ""
if (copyOldOutput){
  newFolder <- paste0(outputPath, valDatePrev)
  dir.create(newFolder)
  l.files <- list.files(currentFolder, ".csv") 
  file.copy(file.path(currentFolder,l.files), newFolder)
}

################# Params #####################################################
#random seed
rseed <- 150
set.seed(rseed)

# set number of states
nStates <-2
#simPathLength from above
#simIter from above
deltat <-1
#valDateDt <- from above
v.indicesQ <- c("DEBQ1", "DEBQ2", "DEBQ3", "DEBQ4", "DEBQ5", "DEBQ6", "DEBQ7",
                "DEPQ1", "DEPQ2", "DEPQ3", "DEPQ4", "DEPQ5", "DEPQ6", "DEPQ7")
v.indicesCal <- c("DEBY1", "DEBY2", "DEBY3", "DEPY1", "DEPY2", "DEPY3") 

v.indicesSpot <- c( "DEBSpot", "DEPSpot")


m.theta <-  matrix(1/40, nrow = nStates, 
                   ncol = length(v.indicesCal) + length(v.indicesQ) + length(v.indicesSpot)) 
################################################################################
###### Params for chunkung as arrays can become too big to handle in environment
################################################################################

### common params ###
simIter <- 1000
# chunks
chunkSize <- 1000
# create vector with simIter for every chunk
v.simIter <- rep(chunkSize, simIter %/% chunkSize)
# count the number of chunks
chunkCount <- length(v.simIter)
if (simIter %% chunkSize != 0){
  v.simIter <- c(v.simIter, simIter %% chunkSize)
}
# put parameter at which iteration of the chunk loop to start
startChunkIter <- 1
#calculate how many days to simulate
simPathLength <- as.numeric(as.Date(paste0("31.12.", as.numeric(substr(valDateDt, 1, 4)) + simYears), 
                         "%d.%m.%Y") - valDateDt)
### save all parameters into a log file as list ###
l.params <- list(currentFolder 	=	 currentFolder,
                 deltat 	=	 deltat, 
                 excludeQuarter	=	excludeQuarter,
                 m.theta 	=	 m.theta, 
                 newFolder 	=	 newFolder,
                 nStates 	=	 nStates, 
                 outputPath	=	outputPath,
                 outputPathHMM 	=	 outputPathHMM,
                 outputPathSim 	=	 outputPathSim,
                 priceDataPath 	=	 priceDataPath, 
                 priceMappingPath	=	priceMappingPath,
                 rseed 	=	 rseed,
                 simPathLength 	=	 simPathLength,
                 simYears 	=	 simYears,
                 v.indicesCal 	=	 v.indicesCal, 
                 v.indicesQ 	=	v.indicesQ, 
                 v.indicesSpot 	=	 v.indicesSpot, 
                 v.simIter 	=	 v.simIter, 
                 v.volInc 	=	 v.volInc,
                 valDateDt 	=	 valDateDt,
                 valDatePrevStr 	=	valDatePrevStr
                 
)
save(l.params,
     file = paste0(outputPathSim, "params.RDA"))
########################################################################################
##################  EXECUTION #####################################################
########################################################################################


################## Load Price Data from CSV to dt.priceData ##############################
# This is a dirty function which loads csv price data, and takesn care to align the 
# relative tenors to absolute future periods and calculate quarter spreads accordingly
# the result is a single data.table which is saved under priceDataPath if requested
LoadDataPrices(priceDataPath, 
               v.indicesCal = v.indicesCal, 
               v.indicesQ = v.indicesQ, 
               v.indicesSpot = v.indicesSpot,
               excludeQuarter = excludeQuarter,
               saving = TRUE)

################# Calibrate HMM Parameters ############################################################
# also dirty function which reads the precalculated and prepared price data data.table and calibrates 
# an HMM with transition matrix, Viterbi sequence of states
# and a set of mean and standard deviation for each state
# HMMs for all price time series
CalibrateHMMPriceData (priceDataPath = priceDataPath,
                       outputPathHMM = outputPathHMM, 
                       nStates = nStates
)
################# Simulate price Chunks ############################################################
# simulation of all price time series
parallel::mcmapply(function(x)
  SimulatePrices(outputPathHMM = outputPathHMM,
                 outputPathSim = paste0(outputPathSim, x, ".RDA"),
                 priceMappingPath = priceMappingPath,
                 simPathLength = simPathLength, 
                 simIter = v.simIter[x], 
                 deltat = deltat, 
                 m.theta = m.theta,
                 rseed = rseed,
                 excludeQuarter = excludeQuarter
  ),
  startChunkIter:chunkCount
  )


