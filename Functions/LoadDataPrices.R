
LoadDataPrices<-function(priceDataPath, 
                    v.indicesCal, v.indicesQ = NULL, v.indicesSpot = NULL,
                    excludeQuarter = 1,
                    saving = TRUE)
  # loads price data index per index from the priceDataPath given. If v.indicesQ is provided
  # loads quarter data and calculates quarter spreads over calender data
  # inputs: 
  #     priceDataPath string the location where the csv files are
  #     v.indicesCal character vector of calender (annual) indices to load. Must match the names of 
  #           the csv files in location, cannot be NULL
  #     v.indicesQ character vector of calender (annual) indices to load. Must match the names of 
  #           the csv files in location. If NULL is provided no quarter data loaded
  #     v.indicesSpot character vector of calender (annual) indices to load. Must match the names of 
  #           the csv files in location. If NULL is provided no quarter data loaded
  #     excludeQuarter: integer from 0 to 4. 0 means no quarter excluded.
  #           If we calculate all quarters quarter prices will not be 
  #           artbitrage free as spreads add up to 0. 
  #     saving: wheather tosave prepared data structures under priceDataPath
  # returns:
  #     dt.pticeData
  #         datatable with columns Date, calendar price indices, spot price indices and base/peak spreads
  #         for each  quarter of the year. Spot only if v.indicesSpot provided. Spreads only if 
  #         v.indicesQ provided.
  
{
  # check if quarterly indices are provided and set a flag
  if (is.null(v.indicesQ)) {quarterCalc <- FALSE} else {quarterCalc <- TRUE}
  
  # initialize data structures. If no quarter calculation NULL will be return for it
  dt.spreadDataQ <- NULL
  dt.priceData <- NULL
  dt.priceDataQ <- NULL
  l.priceDataQAll <- NULL
  ########### calculate spreads of each quarter of particular deivery year ###################################
  ########################## to calendar product for that year ###############################################
  if (quarterCalc){
    # 1. load quarter data and group each front quarter into quarter of the year
    # we use Q1, Q2 to mean front quarters and QI to QIV to mean first to fourth quarter of a year
    
    v.fileNames <-c(paste0(priceDataPath, v.indicesQ, ".csv"))
    # create a list of all csv files and convert them to data.table
    l.tmp <- lapply(v.fileNames , read.csv2, stringsAsFactors=FALSE)
    names(l.tmp) <- v.indicesQ
    l.tmp <- lapply(l.tmp, data.table)
    # manipulate some of the columns to make them useful and set key to all list members
    l.tmp <- lapply( l.tmp, 
                     function(dt.x) dt.x[, ':=' ( Date= as.Date(as.character(Date), format="%d.%m.%Y"),
                                                  ContractYear = as.integer(substr(ContractName, stringr::str_count(ContractName)- 3, stringr::str_count(ContractName)))
                     )])
    # group data for each quarter into separate data.table
    l.priceDataQ <- list(
    dt.priceDataBQI = do.call("rbind",
                              lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "B")], 
                                     function(dt.x) dt.x[substr(ContractName,1,2)== "Q1", ])
    ),
    dt.priceDataBQII = do.call("rbind",
                                lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "B")], 
                                       function(dt.x) dt.x[substr(ContractName,1,2)== "Q2", ])
    ),
    dt.priceDataBQIII = do.call("rbind",
                                 lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "B")], 
                                        function(dt.x) dt.x[substr(ContractName,1,2)== "Q3", ])
    ),
    dt.priceDataBQIV = do.call("rbind",
                                lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "B")], 
                                       function(dt.x) dt.x[substr(ContractName,1,2)== "Q4", ])
    ),
    dt.priceDataPQI = do.call("rbind",
                               lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "P")], 
                                      function(dt.x) dt.x[substr(ContractName,1,2)== "Q1", ])
    ),
    dt.priceDataPQII = do.call("rbind",
                                lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "P")], 
                                       function(dt.x) dt.x[substr(ContractName,1,2)== "Q2", ])
    ),
    dt.priceDataPQIII = do.call("rbind",
                                 lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "P")], 
                                        function(dt.x) dt.x[substr(ContractName,1,2)== "Q3", ])
    ),
    dt.priceDataPQIV = do.call("rbind",
                                lapply(l.tmp[which(substring(names(l.tmp), 3, 3) == "P")], 
                                       function(dt.x) dt.x[substr(ContractName,1,2)== "Q4", ])
    )
    )
     
     #drop contractName
    l.priceDataQ <- lapply(l.priceDataQ, function(dt.x) dt.x[, ContractName := NULL])
    
    # copy list to keep all records, even 2 or 3 per observastion date
    # as we need those to determine the spreads. The _Copy version is used later
    l.priceDataQAll <- copy(l.priceDataQ)
    # eliminate double dates to join together and output row prices as well
    # (if same quarter e.g. Q1 is delivered for two delivery years e.g. 2011 and 2012 we keep the latest)
    l.priceDataQ <- lapply(l.priceDataQ, 
                          function(dt.x){unique(copy(dt.x))[, valRank:= frank(ContractYear),
                                                                       by = c("Date")][valRank == 1]}
    )
    l.priceDataQ <- lapply(l.priceDataQ, 
                          function(dt.x) dt.x[, valRank:= NULL]
                          )
    # define column names list to assign to each member of priceData list
    l.colnames <- mapply(c,gsub( "dt.priceData", "", names(l.priceDataQ)), 
                         "Date",
                         paste0(gsub( "dt.priceData", "", names(l.priceDataQ)), "Year"),
                         SIMPLIFY=FALSE
    )
    # set key
    l.priceDataQ <- lapply( l.priceDataQ, 
                                function(dt.x) setkeyv(dt.x, c("Date", "ContractYear")))
    
    for (i in 1:length(l.priceDataQ)){
      colnames(l.priceDataQ[[i]])<-l.colnames[[i]]
    }
    
    # join to return joined data.table for reference
    dt.priceDataQ <- Reduce(function(dt.x, dt.y){merge(dt.x,dt.y, on = c("Date"), nomatch = FALSE)},
                            l.priceDataQ)
    

    # 2. load calendar indices to calculate spreads
    # here we create a data structure which is only meant for calculating spreads
    # Dte and ContratName (delivery year) are the keys and columns P for peak and B for base
    v.fileNames <-c(paste0(priceDataPath, v.indicesCal, ".csv"))
    # create a list of all csv files and convert them to data.table
    l.tmp <- lapply(v.fileNames , read.csv2, stringsAsFactors=FALSE)
    names(l.tmp) <- v.indicesCal
    l.tmp <- lapply(l.tmp, data.table)
    # manipulate some of the columns to make them useful and set key to all list members
    l.tmp <- lapply( l.tmp, 
                     function(dt.x) dt.x[, ':=' ( Date= as.Date(as.character(Date), format="%d.%m.%Y"),
                                                  ContractYear = as.integer(substr(ContractName, stringr::str_count(ContractName)- 3, stringr::str_count(ContractName)))
                     )])
    
    # group data for each quarter into separate data.table
    l.priceDataCal <- list(
      dt.priceDataB = do.call("rbind",
                                l.tmp[which(substring(names(l.tmp), 3, 3) == "B")]),
      dt.priceDataP = do.call("rbind",
                                 l.tmp[which(substring(names(l.tmp), 3, 3) == "P")])
    )
    #drop contractName
    l.priceDataCal <- lapply(l.priceDataCal, function(dt.x) dt.x[, ContractName := NULL])
    
    # define column names to assign
    l.colnames <- mapply(c,gsub( "dt.priceData", "", names(l.priceDataCal)), 
                         "Date", "ContractYear",
                         SIMPLIFY=FALSE
    )
   
    #assign column names
    for (i in 1:length(l.priceDataCal)){
      colnames(l.priceDataCal[[i]])<-l.colnames[[i]]
    }
    # set key
    l.priceDataCal <- lapply( l.priceDataCal, 
                     function(dt.x) setkeyv(dt.x, c("Date", "ContractYear")))
    # join to return joined data.table for reference
    dt.calData <- Reduce(function(dt.x, dt.y){merge(dt.x,dt.y, nomatch = NULL)}, 
                         l.priceDataCal)
    setkeyv(dt.calData, c("Date", "ContractYear"))
    test <- copy(dt.calData)
    testQ <- copy(l.priceDataQ)
    dt.calData <- copy(test)
    l.priceDataQ <- copy(testQ)
    # finally put column date in front
    #setcolorder(dt.calData, c(2, 3, 1))
    
    # 3. Calculate spreads matching each price date and delivery year for each member of list
    # BQI
    l.spreadData <- copy(lapply(l.priceDataQAll,
      function(dt.x){merge(dt.calData,dt.x, nomatch = NULL)}
      ))
    # replace names of members of the list to have spread instead of priceData
    names(l.spreadData) <- gsub( "dt.priceData", "dt.spread", names(l.spreadData))
    # calculate spread
    l.spreadData <- c(
      lapply(l.spreadData[which(substring(names(l.spreadData), 10, 10) == "B")], 
                           function(dt.x) dt.x[, Spread := Settlement - B]),
      lapply(l.spreadData[which(substring(names(l.spreadData), 10, 10) == "P")], 
                           function(dt.x) dt.x[, Spread := Settlement - P])
    )
    
    l.spreadData <- lapply(l.spreadData, function(dt.x) unique(dt.x))
    #set key
    l.spreadData <- lapply( l.spreadData, 
                             function(dt.x) setkeyv(dt.x, c("Date", "ContractYear")))
    # eliminate double dates for different delivery years
    l.spreadData <- lapply(l.spreadData, 
                           function(dt.x) dt.x[, valRank:= frank(ContractYear), by = Date][valRank == 1])
    # drop columns. Settlement used to be the quarter product price. 
    # after we have the spreads we don't need it anymore
    l.spreadData <- lapply(l.spreadData, 
                           function(dt.x) dt.x[, ':=' (valRank= NULL, 
                                                       Settlement = NULL,
                                                       ContractYear = NULL,
                                                       P = NULL,
                                                       B = NULL
                                                       )])
    #reset key to date after eliminating double entries
    l.spreadData <- lapply( l.spreadData, 
                            function(dt.x) setkey(dt.x, "Date"))
    # assign column names revealing quaerter in order to not mess the final join
    # define column names to assign
    l.colnames <- mapply(c,"Date",
                         gsub( "dt.spread", "Spread", names(l.spreadData)), 
                         SIMPLIFY=FALSE
    )
    #assign column names
    for (i in 1:length(l.spreadData)){
      colnames(l.spreadData[[i]])<-l.colnames[[i]]
    }
    #set key to be able to join
    
    # in order to be able to simulate quarters in the current year where no calendar products exist, we 
    # model the spread of QIII year 0 to calendar year 1. This spread we will then apply to 
    # front calendar products to extract current quarter. Only needed for QII, QIII and QIV
    # B0QIII
    l.spreadData0 <- copy(l.priceDataQAll[which(gsub("dt.priceData", "", names(l.priceDataQAll)) == "BQII" | 
                                          gsub("dt.priceData", "", names(l.priceDataQAll)) == "BQIII" | 
                                          gsub("dt.priceData", "", names(l.priceDataQAll)) == "BQIV" |
                                            gsub("dt.priceData", "", names(l.priceDataQAll)) == "PQII" |
                                          gsub("dt.priceData", "", names(l.priceDataQAll)) == "PQIII" |
                                          gsub("dt.priceData", "", names(l.priceDataQAll)) == "PQIV")])
    
    # increment contractYear to be able to join to front year cal price
    l.spreadData0 <-lapply(l.spreadData0, 
                           function(dt.x) dt.x[, ContractYear := ContractYear + 1])
                           
    #set key
    l.spreadData0 <- lapply( l.spreadData0, 
                              function(dt.x) setkeyv(dt.x, c("Date", "ContractYear")))
    l.spreadData0<-lapply(l.spreadData0,                        
                            function(dt.x){merge(dt.calData,dt.x, nomatch = NULL)}
    )
    # replace names of members of the list to have spread instead of priceData
    names(l.spreadData0) <- gsub( "dt.priceData", "dt.spread", names(l.spreadData0))
    # calculate spread
    l.spreadData0 <- c(
      lapply(l.spreadData0[which(substring(names(l.spreadData0), 10, 10) == "B")], 
             function(dt.x) dt.x[, Spread := Settlement - B]),
      lapply(l.spreadData0[which(substring(names(l.spreadData0), 10, 10) == "P")], 
             function(dt.x) dt.x[, Spread := Settlement - P])
    )
    
    l.spreadData0 <- lapply(l.spreadData0, function(dt.x) unique(dt.x))
    
    # eliminate double dates for different delivery years
    l.spreadData0 <- lapply(l.spreadData0, 
                           function(dt.x) dt.x[, valRank:= frank(ContractYear), by = Date][valRank == 1])
    # drop valRank and Settlement columns. Settlement used to be the quarter product price. 
    # after we have the spreads we don't need it anymore
    l.spreadData0 <- lapply(l.spreadData0, 
                           function(dt.x) dt.x[, ':=' (valRank= NULL, 
                                                       Settlement = NULL,
                                                       ContractYear = NULL,
                                                       P = NULL,
                                                       B = NULL
                           )])
    #reset key after eliminating double entries
    l.spreadData0 <- lapply( l.spreadData0, 
                             function(dt.x) setkey(dt.x, "Date"))
    # assign column names revealing quaerter in order to not mess the final join
    # define column names to assign
    l.colnames <- mapply(c,"Date",
                         gsub( "dt.spread", "Spread0", names(l.spreadData0)), 
                         SIMPLIFY=FALSE
    )
    #assign column names
    for (i in 1:length(l.spreadData0)){
      colnames(l.spreadData0[[i]])<-l.colnames[[i]]
    }
    #join together, exlude QI -> it is most volatile and we have one reduntant time series
    # as the spreads sum to zero for same delivery year
    # join to return joined data.table for reference
    dt.spreadDataQ <- Reduce(function(dt.x, dt.y){merge(dt.x,dt.y, nomatch = NULL)}, 
                         c(l.spreadData, l.spreadData0))
    
                      
   if (excludeQuarter == 1){
     dt.spreadDataQ[, ':='(SpreadBQI = NULL, SpreadPQI = NULL)]
   } else if (excludeQuarter == 2){
     dt.spreadDataQ[, ':='(SpreadBQII = NULL, SpreadPQII = NULL)]
   } else if (excludeQuarter == 3){
     dt.spreadDataQ[, ':='(SpreadBQIII = NULL, SpreadPQIII = NULL)]
   } else if (excludeQuarter == 4){
     dt.spreadDataQ[, ':='(SpreadBQIV = NULL, SpreadPQIV = NULL)] 
   }
  }
  
  ################ Load standard calendar data #################################################
  # spot and calendar products follow the same load logic, no spreads
  # load them together
  v.indices <- c(v.indicesCal, v.indicesSpot)
  
  #get number of indices, aka products
  d <- length(v.indices)
  
  # start by creating data table from the first index
  dt.priceData <- data.table(read.csv2(file = paste0(priceDataPath, v.indices[1], ".csv"), stringsAsFactors=FALSE))
  
  # put sensible name of the column where prices are
  colnames(dt.priceData)[1] <- v.indices[1]
  
  #make sure dates are dates
  if ("ContractName" %in% colnames(dt.priceData)){
    dt.priceData[, ':=' (Date = as.Date(as.character(Date), format="%d.%m.%Y"),
                   ContractName = NULL)]
  } else {
    dt.priceData[, ':=' (Date = as.Date(as.character(Date), format="%d.%m.%Y"))]
  }
  
  #set key to join the other indices
  setkey(dt.priceData, Date)
  
  if (d > 1){
    for (i in 2:d){
      #load next indexinto temporary data.table and set key
      dt.tmp <- data.table(read.csv2(file = paste0(priceDataPath, v.indices[i], ".csv"),stringsAsFactors=FALSE))
      
      if ("ContractName" %in% colnames(dt.tmp)){
        dt.tmp[, ':=' (Date = as.Date(as.character(Date), format="%d.%m.%Y"),
                       ContractName = NULL)]
      } else {
        dt.tmp[, ':=' (Date = as.Date(as.character(Date), format="%d.%m.%Y"))]
      }
      
      colnames(dt.tmp)[1] <- v.indices[i]
      
      setkey(dt.tmp, Date)
      
      dt.priceData <- dt.priceData[dt.tmp, on = "Date", nomatch = NULL]
    }
  }
  
  #############################################
  # spot and calendar products follow the same load logic, no spreads
  # load them together
  v.indices <- c(v.indicesCal, v.indicesSpot)
  
  v.fileNames <-c(paste0(priceDataPath, v.indices, ".csv"))
  # create a list of all csv files and convert them to data.table
  l.tmp <- lapply(v.fileNames , read.csv2, stringsAsFactors=FALSE)
  names(l.tmp) <- v.indices
  l.tmp <- lapply(l.tmp, data.table)
  # manipulate some of the columns to make them useful and set key to all list members
  l.tmp <- lapply( l.tmp, 
                   function(dt.x) {
                     if ("ContractName" %in% colnames(dt.x)){
                       dt.x[, ':=' ( Date= as.Date(as.character(Date), format="%d.%m.%Y"),
                                     ContractName = NULL
                                   )]
                     } else {
                       dt.x[, ':=' (Date = as.Date(as.character(Date), format="%d.%m.%Y"))]   
                     }
                   })
  #reset key after eliminating double entries
  l.tmp <- lapply( l.tmp, 
                           function(dt.x) setkey(dt.x, "Date"))
  # assign column names revealing quaerter in order to not mess the final join
  # define column names to assign
  l.colnames <- mapply(c, names(l.tmp), "Date", SIMPLIFY=FALSE)
  #assign column names
  for (i in 1:length(l.tmp)){
    colnames(l.tmp[[i]])<-l.colnames[[i]]
  }
  #join together, exlude QI -> it is most volatile and we have one reduntant time series
  # as the spreads sum to zero for same delivery year
  # join to return joined data.table for reference
  dt.priceData <- Reduce(function(dt.x, dt.y){merge(dt.x,dt.y, nomatch = NULL)}, 
                           l.tmp)
 
  
  ################################################
  
  if (quarterCalc){
    dt.priceData <- dt.priceData[dt.spreadDataQ, on = "Date", nomatch = NULL]
  } else {
    dt.priceDataQ <- NULL
    l.priceDataQ <- NULL
  }
  
  dt.priceData <- dt.priceData[order(Date),]
  if (saving){
    v.output <- vector(mode = "character", length = 3)
    v.output[1] <- "dt.priceData"
    v.output[2] <- "dt.priceDataQ"
    v.output[3] <- "l.priceDataQAll"
    
    save(list = v.output, file = paste0(priceDataPath, "priceData.rda"))
    
  } else{
    return(list(dt.priceData, dt.priceDataQ, l.priceDataQAll))
  }
}
