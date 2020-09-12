library(unmarked)
library(lubridate)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)

# Read in data
allDat <- mergeLabels_subset

head(allDat)
summary(allDat)

# Get this right!
allDat$DTcorrected <-
  as.Date(ymd_hms(allDat$DateTimeOriginal))

summary(allDat$DTcorrected) # 16 Jan 2018 to 10 May 2019

# Get just species of interest
unique(allDat$Species_Appsilon)
table(allDat$Species_Appsilon)

# Store all species to be used in Unmarked frame (can be multiple species if doing Rota or multi species occu)
species <-
  c("Cat_Golden",
    "Elephant_African",
    "Leopard_African",
    "Chimpanzee")

# Load the activity .csv
LopeIvindo <- read.csv("../data/camStations_LopeIvindo_2020-05-06.csv")
LopeIvindo$study <- "LopeIvindo"

MoukalabaMayumba <- read.csv("../data/camStations_MoukalabaMayumba_2020-06-24.csv")
MoukalabaMayumba$study <- "MoukalabaMayumba"

LoangoMoukalaba <- read.csv("../data/camStations_LoangoMoukal_2020-06-24.csv")
LoangoMoukalaba$study <- "LoangoMoukalaba"

allDatActive <- do.call("rbind", list(LopeIvindo, MoukalabaMayumba, LoangoMoukalaba))
 
allDatActive$camStation <- paste0(allDatActive$camStation, "_", allDatActive$study)

head(allDatActive)
summary(allDatActive)
length(unique(allDatActive$camStation)) # 232 stations in total

allDatActive$deployDate <-
  as.Date(allDatActive$deployDate, format = "%d/%Om/%Y")
allDatActive$endDateCorrected <-
  as.Date(allDatActive$endDateCorrected, format = "%d/%Om/%Y")

# Store variables to setup data
data = allDat
dataActive = allDatActive
species = species
activeDeploy = allDatActive$deployDate
activeEnd = allDatActive$endDateCorrected
occasionLength = 5

# data needs column called 'Species' for labels and camStation for camera stations
allDat$Species <- allDat$Species_Appsilon
allDat$camStation<- allDat$Station

# Load the data prep function
source("functions/UnmarkedPrep.R")

results <- occuDataPrep(
  data = allDat,
  dataActive = allDatActive,
  species = species,
  activeDeploy = allDatActive$deployDate,
  activeEnd = allDatActive$endDateCorrected,
  occasionLength = occasionLength
)

results

# Check it worked
results$speciesYs # good
length(results$speciesYs) # good
names(results$speciesYs) # good

results$obsCovs # good

# Store site covs for unmarked
siteCovs <- allDatActive[, c("elev",
                             "meanVillageDist",
                             "minDistanceRoad",
                             "minDistanceRiver")]

pairs(siteCovs) # nothing to worry about in terms of multicollinearity

# Scale continuous data
siteCovs$elevv <- scale(siteCovs$elev)
siteCovs$meanVillageDist <- scale(siteCovs$meanVillageDist)
siteCovs$minDistanceRoad <- scale(siteCovs$minDistanceRoad)
siteCovs$minDistanceRiver <- scale(siteCovs$minDistanceRiver)

#### Models ####

# Run single season single species
occuFrameList <- vector("list", length = length(species))

for (i in 1:length(occuFrameList)) {
  occuFrameList[[i]] <- unmarkedFrameOccu(
    y = results$speciesYs[[i]],
    siteCovs = siteCovs,
    obsCovs = list(date = results$obsCovs[[i]])
  )
  
}
species
summary(occuFrameList[[1]])
summary(occuFrameList[[2]])
summary(occuFrameList[[3]])
summary(occuFrameList[[4]])

# Run models
occuModListReference <-
  vector("list", length = length(occuFrameList))

for (i in 1:length(occuModListReference)) {
  occuModListReference[[i]] <- occu(
    ~ elev + date + I(date ^ 2) ~
      +elev +  minDistanceRiver + minDistanceRoad + meanVillageDist,
    data = occuFrameList[[i]]
  )
  
}

# Reference predictions
occuModPredsReference <-
  vector("list", length = length(occuFrameList))

for (i in 1:length(occuModListReference)) {
  occuModPredsReference[[i]] <-
    predict(occuModListReference[[i]], type = "state")
  
  
}

# Repeat for no threshold and thresholds 0.5 to 0.9
thresholds <- seq(0,0.9,0.1)

# Plots
pdf("../figures/occuPlotsEvalModels_90.pdf",
    width = 10,
    height = 3)
par(
  mfrow = c(1, 4),
  mar = c(2, 2, 2, 2),
  oma = c(4, 4, 1, 1)
)
blue = rgb(
  red = 0,
  green = 153,
  blue = 153,
  alpha = 125,
  maxColorValue = 255
)

for (t in 1:length(thresholds)) {
  # Now re do again for topLabel, no threshold
  allDat_topLab <- allDat[allDat$topLabelScore >= thresholds[t], ]
  
  # data needs column called 'Species' for labels
  allDat_topLab$Species <- allDat_topLab$topLabel
  
  results_topLab <- occuDataPrep(
    data = allDat_topLab,
    dataActive = allDatActive,
    species = species,
    activeDeploy = allDatActive$deployDate,
    activeEnd = allDatActive$endDateCorrected,
    occasionLength = occasionLength
  )
  
  occuFrameList_topLab <- vector("list", length = length(species))
  
  for (i in 1:length(occuFrameList_topLab)) {
    occuFrameList_topLab[[i]] <-
      unmarkedFrameOccu(
        y = results_topLab$speciesYs[[i]],
        siteCovs = siteCovs,
        obsCovs = list(date = results_topLab$obsCovs[[i]])
      )
    
  }
  
  summary(occuFrameList_topLab[[1]]) # all good
  
  # Run models
  occuModList_topLab <-
    vector("list", length = length(occuFrameList_topLab))
  
  for (i in 1:length(occuModList_topLab)) {
    occuModList_topLab[[i]] <- occu(
      ~ elev + date + I(date ^ 2) ~
        +elev +  minDistanceRiver + minDistanceRoad + meanVillageDist,
      data = occuFrameList_topLab[[i]]
    )
    
  }
  
  # Reference predictions
  occuModPreds_topLab <-
    vector("list", length = length(occuFrameList_topLab))
  
  for (i in 1:length(occuModList_topLab)) {
    occuModPreds_topLab[[i]] <-
      predict(occuModList_topLab[[i]], type = "state")
    
    
  }
  
  
  for (i in 1:length(occuModPreds_topLab)) {
    plot(
     occuModPreds_topLab[[i]]$Predicted ~  occuModPredsReference[[i]]$Predicted ,
      main = if (t == 1) {
        species[i]
      } else {
        ""
      },
      xlab = "",
      ylab = "",
      xlim = c(0, 1),
      ylim = c(0, 1),
      col = blue,
      pch = 16,
      cex = 1,
      cex.axis = 0.8,
      cex.main = 0.9
    )
    
    mod <-
      lm(occuModPreds_topLab[[i]]$Predicted ~ occuModPredsReference[[i]]$Predicted)
    pred <- predict(mod)
    lines(pred ~  occuModPredsReference[[i]]$Predicted)
    
    text(
      x = 0,
      y = 0.9,
      paste0("Slope: ", signif(mod$coefficients[2], 2)),
      adj = -0.01
    )
    text(
      x = 0,
      y = 0.7,
      paste0("R sq: ", signif(summary(mod)$r.squared, 2)),
      adj = -0.01
    )
    
    x <- seq(0, 1, by = 0.1)
    y <- seq(0, 1, by = 0.1)
    abline(lm(x ~ y), col = "darkblue", lty = 3)
  }
}
mtext("Occupancy ML labels", 2, line = 2, outer = TRUE)
mtext("Occupancy expert labels",
      1,
      line = 2,
      outer = TRUE)

dev.off()

# Notes - we want precision to be high as possible to minimise false positives, but also the F1 score probably also useful