require(lubridate)

occuDataPrep <- function(data, 
                         dataActive, 
                         species, 
                         activeDeploy, 
                         activeEnd,
                         occasionLength){
  
  #  Create DFs for each species and store in list
  speciesList <- vector("list", length = length(species))
  speciesYs <- vector("list", length = length(speciesList))
  obsCovs <- vector("list", length = length(speciesList))

  # Get max deploy date
  maxDeploy <-
    max(round(yday(activeEnd)/occasionLength) - 
                     round(yday(activeDeploy)/occasionLength))
  
  for (i in 1:length(species)) {
    speciesList[[i]] <-
      droplevels(data[data$Species == species[i],])
    speciesList[[i]]$samplingOccasion <- round(yday(speciesList[[i]]$DTcorrected) / occasionLength)
  }

  ##### Start loop ####
  speciesYs <- vector("list", length = length(speciesList))
  obsCovs <- vector("list", length = length(speciesList))
  
  for (i in 1:length(speciesList)) {
    
    # Create empty presence.absence for all activty dates, for each station
    stationList <-
      vector("list", length = length(unique(dataActive$camStation)))
    
    for (j in 1:length(unique(dataActive$camStation))) {
      stationList[[j]] <-
        data.frame(
          camStation = dataActive$camStation[j],
          date = seq.Date(
            from = activeDeploy[j],
            to = activeEnd[j],
            by = 1
          ),
          Active = "yes"
        )
      stationList[[j]]$year <- year(stationList[[j]]$date)
      stationList[[j]]$samplingOccasion <- round(yday(stationList[[j]]$date) / occasionLength)
    }
    
    activeDF <- do.call("rbind", stationList)
    
    # Create unique station and date vars for merging pres/abs of given sp
    activeDF$stationDate <-
      paste(activeDF$camStation,
            activeDF$year,
            activeDF$samplingOccasion,
            sep = "_")
    
    speciesList[[i]]$stationDate <-
      paste(speciesList[[i]]$camStation, 
            year(speciesList[[i]]$DTcorrected), 
            speciesList[[i]]$samplingOccasion,
            sep = "_")
    
    # Merge species pres/abs
    speciesList[[i]]$y <- 1 # presence of species on given date
    
    speciesList[[i]] <-
      unique(speciesList[[i]]) # dateTime format is down to minute resolution, so some duplicates taken seconds apart
    activeDF <- unique(activeDF)
    
    speciesdat <-
      merge(activeDF,
            speciesList[[i]][, c("stationDate", "y")],
            by = "stationDate",
            all.x = T)
    
    # Change y to be 0 if NA
    speciesdat$y <-
      ifelse(is.na(speciesdat$y),
             0,
             speciesdat$y)
    
    speciesdat <- speciesdat[, c("camStation", "year", "samplingOccasion", "y")]
    
    speciesdat <-
      speciesdat[order(speciesdat$camStation, speciesdat$year, speciesdat$samplingOccasion), ]
    
    speciesdat <- unique(speciesdat)
    
    # Get the Ys and the obs
    speciesYs[[i]] <- matrix(nrow = length(unique(speciesdat$camStation)),
                             ncol = maxDeploy)
    
    obsCovs[[i]] <- matrix(nrow = length(unique(speciesdat$camStation)),
                           ncol = maxDeploy)
    
    # Store detect histories and obs covs (sampling occasion block in year)
    for (k in 1:length(unique(speciesdat$camStation))) {
      
      newStat <- unique(speciesdat$camStation)[k]
      
      # Store the ys
      newY <-
        speciesdat[speciesdat$camStation == newStat, "y"]
      
      # Make the length same as maximum deployment (fill end with NAs)
      length(newY) <- maxDeploy
      speciesYs[[i]][k,] <- t(newY)
      names(speciesYs) <- species
      
      # Repeat above for dates instead of ys
      obsSubs <- speciesdat[speciesdat$camStation == newStat,]
      newObs <-
        (as.numeric(as.Date(obsSubs$samplingOccasion * occasionLength, origin = paste0(obsSubs$year, "-01", "-01"))) - mean(as.numeric(as.Date(speciesdat$samplingOccasion * occasionLength, origin = paste0(speciesdat$year, "-01", "-01"))))) / sd(as.numeric(as.Date(speciesdat$samplingOccasion * occasionLength, origin = paste0(speciesdat$year, "-01", "-01"))))
      
      length(newObs) <- maxDeploy
      
      obsCovs[[i]][k,] <- t(newObs)
    }
    
  }
  
  return(list(speciesYs = speciesYs, 
              obsCovs = obsCovs,
              meanDate = mean(as.numeric(as.Date(speciesdat$samplingOccasion * occasionLength, 
                                                 origin = paste0(speciesdat$year, "-01", "-01")))),
              sdDate = sd(as.numeric(as.Date(speciesdat$samplingOccasion * occasionLength, 
                                             origin = paste0(speciesdat$year, "-01", "-01"))))))
}
