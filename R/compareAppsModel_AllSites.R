library(caret)
library(forcats)
library(yardstick)
library(ggplot2)
library(magick)
library(pheatmap)
library(viridis)
library(lubridate)
library(activity)

#### Load in cleaned data for analysis ####
mergeLabels_subset_LopeIvindo <- read.csv("../data/publicData/LopeIvindo_corrected_mergeLabels_subset_noCorrupted_public.csv")
mergeLabels_subset_MoukalabaMayumba <- read.csv("../data/publicData/MoukalabaMayumba_corrected_mergelabels_subset_noCorrupted_public.csv")
mergeLabels_subset_LoangoMoukalaba <- read.csv("../data/publicData/LoangoMoukalaba_corrected_mergelabels_subset_noCorrupted_public.csv")

head(mergeLabels_subset_LopeIvindo)
head(mergeLabels_subset_MoukalabaMayumba)
head(mergeLabels_subset_LoangoMoukalaba)

mergeLabels_subset_LopeIvindo <- mergeLabels_subset_LopeIvindo[,-1]
mergeLabels_subset_MoukalabaMayumba <- mergeLabels_subset_MoukalabaMayumba[,-1]
mergeLabels_subset_LoangoMoukalaba <- mergeLabels_subset_LoangoMoukalaba[,-1]

names(mergeLabels_subset_LopeIvindo)
extractCols <- names(mergeLabels_subset_LopeIvindo)[c(1,3:36,38:51)]
mergeLabels_subset_LopeIvindo <- mergeLabels_subset_LopeIvindo[,which(names(mergeLabels_subset_LopeIvindo) %in% extractCols)]
mergeLabels_subset_LopeIvindo$Station <- paste0(mergeLabels_subset_LopeIvindo$Station, "_LopeIvindo")

mergeLabels_subset_LoangoMoukalaba <- mergeLabels_subset_LoangoMoukalaba[,which(names(mergeLabels_subset_LoangoMoukalaba) %in% extractCols)]
mergeLabels_subset_LoangoMoukalaba$corrupted <- FALSE
mergeLabels_subset_LoangoMoukalaba$Station <- paste0(mergeLabels_subset_LoangoMoukalaba$Station, "_LoangoMoukalaba")

mergeLabels_subset_MoukalabaMayumba <- mergeLabels_subset_MoukalabaMayumba[,which(names(mergeLabels_subset_MoukalabaMayumba) %in% extractCols)]
mergeLabels_subset_MoukalabaMayumba$corrupted <- FALSE
mergeLabels_subset_MoukalabaMayumba$Station <- paste0(mergeLabels_subset_MoukalabaMayumba$Station, "_MoukalabaMayumba")

# Remove "newBlanks" from LopeIvindo
mergeLabels_subset_LopeIvindo <- mergeLabels_subset_LopeIvindo[,-48]

mergeLabels_subset <- do.call("rbind", list(mergeLabels_subset_LoangoMoukalaba,
                      mergeLabels_subset_LopeIvindo,
                      mergeLabels_subset_MoukalabaMayumba))

#  Store missing labels
# missingTopLabel <- c(
#   "Civet_African_Palm",
#   "Guineafowl_Black",
#   "Mongoose",
#   "Rail_Nkulengu",
#   "Rat_Giant")

# Remove corrupted images
mergeLabels_subset <- subset(mergeLabels_subset, corrupted == FALSE)

# Noticed that levels STILL has species not used in training data, fix
levels(mergeLabels_subset$topLabel)
levels(mergeLabels_subset$Species_Appsilon)

mergeLabels_subset <- droplevels(mergeLabels_subset)
levels(mergeLabels_subset$Species_Appsilon) <- c(levels(mergeLabels_subset$Species_Appsilon), missingTopLabel) # ignore this

mergeLabels_subset$topLabel <- fct_relevel(mergeLabels_subset$topLabel, sort)
mergeLabels_subset$Species_Appsilon <- fct_relevel(mergeLabels_subset$Species_Appsilon, sort)

levels(mergeLabels_subset$topLabel)
levels(mergeLabels_subset$Species_Appsilon)

# Re calc top five
mergeLabels_subset$topFive <- FALSE

for(i in 1:nrow(mergeLabels_subset)){
  
  newDat <- data.frame(probs = t(mergeLabels_subset[i,c(2:29)])[,1],
                       species = row.names(t(mergeLabels_subset[i,c(2:29)])))
  
  newDat <- newDat[order(newDat$probs, decreasing = T),]
  mergeLabels_subset$topFive[i] <- mergeLabels_subset$Species_Appsilon[i] %in% paste(newDat[1:5,"species"])
  
}

summary(mergeLabels_subset$topFive)
100 - length(which(mergeLabels_subset$topOne == FALSE)) / nrow(mergeLabels_subset) * 100 # 77.63% Top one
100 - length(which(mergeLabels_subset$topFive == FALSE)) / nrow(mergeLabels_subset) * 100 # 94.24% Top five

# Re-run the confusion matrix
confusionMatrix(mergeLabels_subset$topLabel, mergeLabels_subset$Species_Appsilon)

cm3 <- conf_mat(data = mergeLabels_subset, estimate = topLabel, truth = Species_Appsilon)

pdf("../figures/allData_topLabel_confMat.pdf", width = 10, height = 8)
autoplot(cm3, type = "heatmap") +
  scale_fill_gradient(low= "#D6EAF8", high = "#2E86C1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()

# Save results in raw csv
write.csv(mergeLabels_subset[,c("Species_Appsilon", "topLabel", "topLabelScore")], "../results/confMat_raw.csv", row.names = F)

# Use thresholds from 0 to 1 and calculate top one and top five
thresholds <- seq(0,0.9,0.1)

# Create DF for values
evalResults <- data.frame(topOne = rep(NA, length = length(thresholds)),
                          topFive = NA,
                          percentDiscarded = NA)

confMatrixList <- vector("list", length = length(thresholds))
discardedLabelsTopOne <- vector("list", length = length(thresholds))
discardedLabelsTopFive <- vector("list", length = length(thresholds))

# Store rows in full data
dataN <- nrow(mergeLabels_subset)

for(i in 1:length(thresholds)){
  
  newDat <- mergeLabels_subset[mergeLabels_subset$topLabelScore >= thresholds[i],]
  
  # Store % discarded
  evalResults$percentDiscarded[i] <- 100-(100/dataN * nrow(newDat))
  evalResults$topOne[i] <- 100 - length(which(newDat$topOne == FALSE)) / nrow(newDat) * 100
  
  # Re calc top five
  newDat$topFive <- FALSE
  
  for(k in 1:nrow(newDat)){
    
    topFiveDat <- data.frame(probs = t(newDat[k,c(2:29)])[,1],
                         species = row.names(t(newDat[k,c(2:29)])))
    
    topFiveDat <- topFiveDat[order(topFiveDat$probs, decreasing = T),]
    newDat$topFive[k] <- newDat$Species_Appsilon[k] %in% paste(topFiveDat[1:5,"species"])
  }

  evalResults$topFive[i] <- 100 - length(which(newDat$topFive == FALSE)) / nrow(newDat) * 100
  
  # Store confusion matrix
  confMatrixList[[i]] <- confusionMatrix(newDat$topLabel, newDat$Species_Appsilon)
  
  # Store frequencies of species after discard
  discardedLabelsTopOne[[i]] <- newDat[which(! newDat$topLabel == newDat$Species_Appsilon),"Species_Appsilon"]
  discardedLabelsTopFive[[i]] <- newDat[which(! newDat$Species_Appsilon %in% newDat$topFive),"Species_Appsilon"]
  
}

evalResults$threshold <- thresholds

# Get balanced accuracy better with error
evalResults$balancedAccuracy <- NA
evalResults$balancedAccuracyUpper <- NA
evalResults$balancedAccuracyLower <- NA

for(i in 1:nrow(evalResults)){
  
  evalResults$balancedAccuracy[i] <- confMatrixList[[i]]$overall[1]
  evalResults$balancedAccuracyLower[i] <- confMatrixList[[i]]$overall[3]
  evalResults$balancedAccuracyUpper[i] <- confMatrixList[[i]]$overall[4]
  
}

#### Plots ####
pdf("../figures/threshold_sampleSize.pdf", width = 5, height = 5)
par(mfrow = c(1,1), mar = c(4,4,4,1))

# Top one
plot(evalResults$percentDiscarded,
     evalResults$balancedAccuracy*100,
     xlim = c(0, 40),
     ylim = c(70, 100),
     ylab = "Prediction accuracy (%)",
     xlab = "Data removed (%)",
)

arrows(
  x0 = evalResults$percentDiscarded,
  y0 = evalResults$balancedAccuracyLower*100,
  x1 = evalResults$percentDiscarded,
  y1 = evalResults$balancedAccuracyUpper*100,
  length = 0.05,
  angle = 90,
  code = 3
)

axis(3, at = c( evalResults$percentDiscarded), labels = evalResults$threshold*100)
lines(( evalResults$percentDiscarded),
       evalResults$balancedAccuracy*100)
points((evalResults$percentDiscarded), evalResults$topFive)
lines((evalResults$percentDiscarded), evalResults$topFive, lty = 2)
mtext("Minimum threshold to accept top label (%)",side=3,line=2.5)
legend(14,75, bty = "n", legend = c("Top five accuracy", "Top one balanced accuracy"),
       pch = 21, lty = c(2,1))

abline(h = 90, lty = 2, col = "red")

dev.off()

# Write the confusion matrices
for(i in 1:length(confMatrixList)){
  
  write.csv(confMatrixList[[i]]$byClass, paste0("../results/","confMatrix_threshold_", evalResults$threshold[i],".csv"))
  
}

#### Species richness ####
# Now compare real and predicted species richness at each camera station for each threshold 
stations <- unique(mergeLabels_subset$Station)
speciesRichness <- vector("list", length = length(length(stations)))


for (i in 1:length(stations)) {
  newDat <-
    mergeLabels_subset[mergeLabels_subset$Station == stations[i],]
  
  outFrame <- data.frame(
    threshold = thresholds,
    station = stations[i],
    predictedRichness = NA,
    observedRichness = NA
  )
  
  for (k in 1:length(thresholds)) {
    outFrame$predictedRichness[k] <-
      length(unique(newDat[which(newDat$topLabelScore >= thresholds[k]), "topLabel"]))
    outFrame$observedRichness[k] <-
      length(unique(mergeLabels_subset[mergeLabels_subset$Station == stations[i], "Species_Appsilon"]))
    
  }
  
  speciesRichness[[i]] <- outFrame
  
}
  
speciesRichness <- do.call("rbind", speciesRichness)

# Plot the results
# Make a ncie color

blue = rgb(red = 0, green = 153, blue = 153, alpha = 125, maxColorValue = 255)

pdf("../figures/speciesRichness_thresholds.pdf", width = 11, height = 8.5)
par(mfrow = c(3,4), mar = c(2,2,2,2), oma = c(4,4,1,1))



evalSpeciesRichnessCorr <- data.frame(thresholds = thresholds,
                                      coef = NA,
                                      R2 = NA)

for(i in 1:length(thresholds)){
  
  
  newDat <- speciesRichness[speciesRichness$threshold == thresholds[i],]
  plot(
    newDat$predictedRichness ~ newDat$observedRichness,
    ylim = c(0, 20),
    xlim = c(0, 20),
    xlab = "",
    ylab = "",
    col = blue,
    pch = 16,
    cex = 1.2
  )
  
  
  evalSpeciesRichnessCorr$coef[i] <- coef(lm(predictedRichness ~ observedRichness, data = newDat))[[2]]
  mod <- lm(predictedRichness ~ observedRichness, data = newDat)
  evalSpeciesRichnessCorr$R2[i] <- summary(mod)$r.squared
  
  predDat <- data.frame(observedRichness = seq(min(newDat$observedRichness), 
                                                max(newDat$observedRichness),
                                                length.out = nrow(newDat)))
  pred <- predict(mod, newdata = predDat)
  lines(pred ~ predDat$observedRichness)
  text(x = 19, paste0("Threshold: ", thresholds[i]), adj = -0.01)
  text(x = 17, paste0("Slope coefficient: ", signif(evalSpeciesRichnessCorr$coef[i],2)), adj = -0.01)
  text(x = 15, paste0("R sq: ", signif(evalSpeciesRichnessCorr$R2[i],2)), adj = -0.01)
  
  x <- 1:20
  y <- 1:20
  abline(lm(x~y), col = "darkblue", lty = 3)
  }

mtext("Species richness (ML labels)", 2, line = 2, outer = TRUE)
mtext("Species richness (expert labels)", 1, line = 2, outer = TRUE)

dev.off()

#### Capture rates #### NOT CURRENLY IN MS, NO THREHOLDING APPLIED YET
stationCaptures <- data.frame(table(mergeLabels_subset$Station))
stationCaptures$Freq <- stationCaptures$Freq/100
captureList <- vector("list", length = length(stations))
uniqueSpecies <- unique(mergeLabels_subset$topLabel)

for(i in 1:length(stations)) {
  res <- data.frame(
    uniqueSpecies = uniqueSpecies,
    Station = stations[i],
    captureRateExpert = NA,
    captureRateML = NA
  )
  
  for (j in 1:length(uniqueSpecies)) {
    res$captureRateExpert[j] <- nrow(mergeLabels_subset[mergeLabels_subset$Station == stations[i] &
                                              mergeLabels_subset$Species_Appsilon == uniqueSpecies[j],]) / stationCaptures[which(stationCaptures$Var1 == stations[i]), "Freq"]
    
    
    
    res$captureRateML[j] <- nrow(mergeLabels_subset[mergeLabels_subset$Station == stations[i] &
                                              mergeLabels_subset$topLabel == uniqueSpecies[j],]) / stationCaptures[which(stationCaptures$Var1 == stations[i]), "Freq"]
    
    
    
    
  }
  
  captureList[[i]] <- res
  
  
}

captureRates <- do.call("rbind", captureList)

# Plots
par(mfrow = c(5,6))

for(i in 1:length(uniqueSpecies)){
  
  plot(captureRateExpert ~ captureRateML, subset(captureRates, uniqueSpecies == uniqueSpecies[i]), main = uniqueSpecies[i])
  
  
}

#### Activity patterns ####
focalSpecies <-
  c("Elephant_African",
    "Leopard_African",
    "Cat_Golden",
    "Chimpanzee")

# Calculate baseline activity model for each focal species using full dataset and manual labels
manualActivityModels <- vector("list", length = length(focalSpecies))

for(i in 1:length(focalSpecies)){
  
  TimesManual <- as.POSIXct(ymd_hms(
    mergeLabels_subset[mergeLabels_subset$Species_Appsilon == focalSpecies[i],"date_time"]))
  TimesManual <- gettime(TimesManual)
  modManual <- fitact(TimesManual, sample = "model", reps = 200)
  manualActivityModels[[i]] <- modManual
  
  
}

#Calculate activity patterns for focal species
MLActivityModels <- vector("list", length = length(thresholds)*length(focalSpecies))

index <- 0
for(i in 1:length(thresholds)) {
  
  if(index == 0){index <- index +1}
  
  newDat <-
    mergeLabels_subset[mergeLabels_subset$topLabelScore >= thresholds[i], ]
  
  for (j in 1:length(focalSpecies)) {
    newDatSpecies <- newDat[newDat$topLabel == focalSpecies[j], ]
    
    if (nrow(newDatSpecies) > 10) {
      TimesML <- as.POSIXct(ymd_hms(newDatSpecies[newDatSpecies$topLabel == focalSpecies[j], "date_time"]))
      
      TimesML <- gettime(TimesML)
      
      modML <- fitact(TimesML, sample = "model", reps = 200)
      
    } else {
      
      modML <- NA
    }

    MLActivityModels[[index]] <- modML
    index <- index +1
    
  }
  
  
}


# Compare models
compareResDF <- data.frame(Difference = NA, 
                           SE = NA, 
                           W = NA, 
                           p = NA, 
                           Species = NA,
                           threshold = NA)
index <- 0
for(i in 1:length(thresholds)) {
  if(index == 0){index <- index + 1}
  
  for (j in 1:length(focalSpecies)) {
    if (!is.na(MLActivityModels[[index]])) {
     
      mod1 <- MLActivityModels[[index]]
      mod2 <- manualActivityModels[[j]]
      
      res <-
        data.frame(compareAct(list(
          mod1, mod2
        )))
      res$Species <- focalSpecies[j]
      res$threshold <- thresholds[i]
      compareResDF <- rbind(compareResDF, res)
      index <- index + 1
    } else {
      res <- data.frame(Difference = NA, 
                                        SE = NA, 
                                        W = NA, 
                                        p = NA, 
                                        Species = focalSpecies[j],
                        threshold = thresholds[i])
      compareResDF <- rbind(compareResDF, res)
      index <- index + 1
    }
  }
  
}

compareResDF <- compareResDF[-1,] # removoe the extra line from beginning
compareResDF # no difference at any threshold

par(mfrow = c(2,2))
for(i in 1:length(focalSpecies)){
  
  newDat <- subset(compareResDF, Species == focalSpecies[i])
  plot(Difference ~ threshold, newDat, main = focalSpecies[i], ylim = c(-0.3,0.3))
  

  }

write.csv(compareResDF, "../results/compareActivity.csv", row.names = F)

#### Activity plots ####
# Create nice colours
lightOrange <- rgb(1, 0.4, 0, alpha = 0.3)
darkOrange <- rgb(1, 0.4, 0, alpha = 1)

lightBlue <- rgb(0, 0.4, 0.8, alpha = 0.3)
darkBlue <- rgb(0, 0.4, 0.8, alpha = 1)

# Differences generally become non significant after 0.7
pdf("../figures/activityPattern_point7.pdf", width = 6, height = 6)
  
par(mfrow = c(2,2), mar = c(2,2,2,2), oma = c(4,4,1,1))

index <- 29:32
  for(i in 1:length(focalSpecies)){
    
      plot(MLActivityModels[[index[i]]], 
         dline = list(col = lightOrange), 
         tline = list(col = darkOrange, lwd = 2),
         cline = list(col = NULL),
         yunit = "density",
         main = paste0(focalSpecies[[i]]),
         bty = "o",
         ylim = c(0,0.2))
    
    plot(manualActivityModels[[i]], 
         add = TRUE,
         dline = list(col = lightBlue), 
         tline = list(col = darkBlue, lwd =2),
         cline = list(col = NULL),
         yunit = "density",
         ylim = c(0,0.25))
    }
  
  mtext("Time (24 h)", 1, line = 2, outer = TRUE)
  mtext("Density", 2, line = 2, outer = TRUE)
dev.off()

# Plot histograms for topLabelScore for each focal species
pdf("../figures/confidenceHists.pdf", width = 12, height = 8)
par(mfrow = c(5,6), mar = c(2,2,2,2), oma = c(4,4,1,1))

breaksDF <- data.frame(Var1 = seq(0,1, by = 0.1))

for(i in 1:length(unique(mergeLabels_subset$topLabel))){

  
  # One barplot command to get histogram of x
  x <- data.frame(prop.table(table(round(mergeLabels_subset[mergeLabels_subset$topLabel == uniqueSpecies[i], "topLabelScore"], digits = 1))))
  xMerge <- merge(breaksDF, x, by = "Var1", all.x = TRUE)
  xMerge$Freq <- ifelse(is.na(xMerge$Freq), 0, xMerge$Freq)
  
  xPos <- barplot(height = xMerge$Freq,
          ylab = "proportion",
          xlab = "values",
          ylim = c(0,1),
          main = uniqueSpecies[i],
          cex.main = 1,
          names.arg = xMerge$Var1)
  axis(1, at = xPos,labels = FALSE)
  
}

mtext("Label confidence (probability)", 1, line = 2, outer = TRUE)
mtext("Density", 2, line = 2, outer = TRUE)

dev.off()

# Look at overall capture frequency for top labels and manual labels
ML_freq <- as.data.frame(table(mergeLabels_subset$topLabel))
ML_freq <- ML_freq[order(ML_freq$Freq),]

Manual_freq <- as.data.frame(table(mergeLabels_subset$Species_Appsilon))
Manual_freq <- Manual_freq[order(Manual_freq$Freq),]

pdf("../figures/captureFrequencies.pdf", width = 8, height = 6)
par(mfrow = c(1,2), mar = c(4,12,1,1))
barplot(ML_freq$Freq, names.arg = ML_freq$Var1, las = 2, horiz = T, main = "ML labels (top one)")
barplot(Manual_freq$Freq, names.arg = Manual_freq$Var1, las = 2, horiz = T, main = "Reference")
dev.off()

# Table showing capture frequencies per station
hist(table(mergeLabels_subset$Station))
# Now move to occupancy models
file.edit('unmarked_evalModels_AllSites.R')
