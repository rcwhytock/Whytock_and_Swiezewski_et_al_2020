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
mergeLabels_subset

# Re-leabel manual classifications with reduced number of labels
dput(levels(mergeLabels_subset$Species_Appsilon))
levels(mergeLabels_subset$Species_Appsilon) <- c("Bird", "Blank", "Ungulate", "Cat", "Chevrotain_Water", 
                                                 "Primate", "Carnivore_Small", "Ungulate", "Ungulate", 
                                                 "Ungulate", "Elephant_African", "Carnivore_Small", "Primate", 
                                                 "Bird", "Bird", "Ungulate", "Human", "Cat", 
                                                 "Primate", "Carnivore_Small", "Carnivore_Small", "Primate", 
                                                 "Pangolin", "Rodent", "Bird", "Rodent", 
                                                 "Rodent")

# Repeat for topLabel
dput(levels(mergeLabels_subset$topLabel))
levels(mergeLabels_subset$topLabel) <- c("Bird", "Blank", "Ungulate", "Cat", "Chevrotain_Water", 
                                         "Primate", "Carnivore_Small", "Ungulate", "Ungulate", 
                                         "Ungulate", "Elephant_African", "Carnivore_Small", "Primate", 
                                         "Bird", "Bird", "Ungulate", "Human", "Cat", 
                                         "Primate", "Carnivore_Small", "Carnivore_Small", "Primate", 
                                         "Pangolin", "Rodent", "Bird", "Rodent", 
                                         "Rodent")

mergeLabels_subset$topOne <- ifelse(mergeLabels_subset$topLabel == mergeLabels_subset$Species_Appsilon, TRUE, FALSE)

# Re calc top five
dput(names(mergeLabels_subset)[2:29])

newNames<- c("Bird", "Blank", "Ungulate", "Cat", "Chevrotain_Water", 
  "Primate", "Carnivore_Small", "Ungulate", "Ungulate", 
  "Ungulate", "Elephant_African", "Carnivore_Small", "Primate", 
  "Bird", "Bird", "Ungulate", "Human", 
  "Cat", "Primate", "Carnivore_Small", "Carnivore_Small", 
  "Primate", "Pangolin", "Rodent", "Bird", 
  "Rodent", "Rodent", "Rodent")



mergeLabels_subset$topFive <- FALSE

for(i in 1:nrow(mergeLabels_subset)){
  
  newDat <- data.frame(probs = t(mergeLabels_subset[i,c(2:29)])[,1],
                       species = newNames)
  
  newDat <- newDat[order(newDat$probs, decreasing = T),]
  mergeLabels_subset$topFive[i] <- mergeLabels_subset$Species_Appsilon[i] %in% paste(newDat[1:5,"species"])
  
}

100 - length(which(mergeLabels_subset$topOne == FALSE)) / nrow(mergeLabels_subset) * 100 # 83.98% Top one
100 - length(which(mergeLabels_subset$topFive == FALSE)) / nrow(mergeLabels_subset) * 100 # 96.12% Top five


# Re-run the confusion matrix
confusionMatrix(mergeLabels_subset$topLabel, mergeLabels_subset$Species_Appsilon)

cm3 <- conf_mat(data = mergeLabels_subset, estimate = topLabel, truth = Species_Appsilon)

pdf("../figures/allData_topLabel_confMat_reducedLabels.pdf", width = 10, height = 8)
ggplot2::autoplot(cm3, type = "heatmap") +
  scale_fill_gradient(low= "#D6EAF8", high = "#2E86C1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()

# Save results in raw csv
write.csv(mergeLabels_subset[,c("Species_Appsilon", "topLabel", "topLabelScore")], "../results/reducedLabels_confMat_raw.csv", row.names = F)

# Use thresholds from 0.5 to 1 and calculate top one and top five
thresholds <- c(0.50,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9)

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
    
    topFiveDat <- data.frame(probs = t(newDat[k,c(3:30)])[,1],
                         species = row.names(t(newDat[k,c(3:30)])))
    
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
pdf("../figures/threshold_sampleSize_reducedLabels.pdf", width = 5, height = 5)
par(mfrow = c(1,1), mar = c(4,4,4,1))

# Top one
plot(evalResults$percentDiscarded,
     evalResults$balancedAccuracy*100,
     xlim = c(10, 50),
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

axis(3,
     at = c(evalResults$percentDiscarded),
     labels = evalResults$threshold)
lines((evalResults$percentDiscarded),
      evalResults$balancedAccuracy * 100)
points((evalResults$percentDiscarded), evalResults$topFive)
lines((evalResults$percentDiscarded), evalResults$topFive, lty = 2)
mtext("Minimum threshold to accept top label (%)",
      side = 3,
      line = 2.5)

abline(h = 90, lty = 2, col = "red")

dev.off()

# This result means to get the balanced prediction accuracy up to 90% we would need to discard (or manually classify, e.g. via citizen science) around 25% of the data
# This means that effort has been reduce by ~75% (c.f. Marco Willi 43%) but check what threhold they wanted for prediction accuracy (we use 90%, red line in plot)
# Repeat for individual species
for(i in 1:length(confMatrixList)){
  
  write.csv(confMatrixList[[i]]$byClass, paste0("../results/","confMatrix_threshold_reducedLabels", evalResults$threshold[i],".csv"))
  
}

### NOT RUN BELOW HERE FOR REDUCED LABELS ####
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

pdf("../figures/speciesRuchness_thresholds.pdf", width = 8, height = 8)
par(mfrow = c(3,3), mar = c(2,2,2,2), oma = c(4,4,1,1))



evalSpeciesRichnessCorr <- data.frame(thresholds = thresholds,
                                      coef = NA,
                                      R2 = NA)

for(i in 1:length(thresholds)){
  
  
  newDat <- speciesRichness[speciesRichness$threshold == thresholds[i],]
  plot(
    newDat$observedRichness ~ newDat$predictedRichness,
    ylim = c(0, 20),
    xlim = c(0, 20),
    xlab = "",
    ylab = "",
    col = blue,
    pch = 16,
    cex = 1.2
  )
  
  
  evalSpeciesRichnessCorr$coef[i] <- coef(lm(observedRichness ~ predictedRichness, data = newDat))[[2]]
  mod <- lm(observedRichness ~ predictedRichness, data = newDat)
  evalSpeciesRichnessCorr$R2[i] <- summary(mod)$r.squared
  
  predDat <- data.frame(predictedRichness = seq(min(newDat$predictedRichness), 
                                                max(newDat$predictedRichness),
                                                length.out = nrow(newDat)))
  pred <- predict(mod, newdata = predDat)
  lines(pred ~ predDat$predictedRichness)
  text(x = 19, paste0("Threshold: ", thresholds[i]), adj = -0.01)
  text(x = 17, paste0("Slope coefficient: ", signif(evalSpeciesRichnessCorr$coef[i],2)), adj = -0.01)
  text(x = 15, paste0("R sq: ", signif(evalSpeciesRichnessCorr$R2[i],2)), adj = -0.01)
  
  x <- 1:20
  y <- 1:20
  abline(lm(x~y), col = "darkblue", lty = 3)
  }

mtext("Predicted species richness", 1, line = 2, outer = TRUE)
mtext("Observed species richness", 2, line = 2, outer = TRUE)

dev.off()

# Look at histograms of species richness
pdf("../figures/speciesRichness_hists_thresholds_reducedLabels.pdf", width = 8, height = 8)

par(mfrow = c(3,3), mar = c(2,2,2,2), oma = c(4,4,1,1))

for(i in 1:length(thresholds)){
  
  
  newDat <- speciesRichness[speciesRichness$threshold == thresholds[i],]
  allMean <- unique(speciesRichness[,c(2:4)])
  allMean <- mean(allMean$observedRichness)
  
  hist(
    newDat$observedRichness,
    ylim = c(0, 25),
    xlim = c(0, 20),
    xlab = "",
    ylab = "",
    col = blue,
    pch = 16,
    cex = 1.2,
    main = ""
  )
   abline(v = allMean, col = "red", lty = 3, lwd = 2)
   abline(v = mean(newDat$predictedRichness), col = "gray10", lty = 3, lwd = 2)
   
}

mtext("Predicted species richness", 1, line = 2, outer = TRUE)
mtext("Frequency", 2, line = 2, outer = TRUE)

dev.off()

par(mfrow = c(1,1))
plot(
  coef ~ thresholds,
  ylim = c(0.7, 1),
  xlim = c(0.4, 1),
  evalSpeciesRichnessCorr,
  xlab = "Minimum threshold to accept top label (%)",
  ylab = "Slope coefficient"
)


# These results suggest that to get a slope of > 0.9 (almost 1:1) for the relationship between observed and predicted species richness, use a trheshold of ~0.75 for topOne predicted accuracy (i.e. discard everything below that threshold). Alongside the results earlier, this would be getting rid of c. 35% of the data.

#### Activity patterns ####

focalSpecies <-
  c("Elephant_African",
    "Leopard_African",
    "Cat_Golden",
    "Gorilla",
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
MLActivityModels <- vector("list", length = length(thresholds))

for(i in 1:length(thresholds)) {
  MLfocalSpeciesList <- vector("list", length = length(focalSpecies))
  newDat <-
    mergeLabels_subset[mergeLabels_subset$topLabelScore >= thresholds[i], ]
  
  for (j in 1:length(focalSpecies)) {
    newDatSpecies <- newDat[newDat$topLabel == focalSpecies[j], ]
    
    if (nrow(newDatSpecies) > 10) {# Don't calculate if n < 10
      TimesML <- as.POSIXct(ymd_hms(newDatSpecies[newDatSpecies$topLabel == focalSpecies[j], "date_time"]))
      
      TimesML <- gettime(TimesML)
      
      modML <- fitact(TimesML, sample = "model", reps = 200)
      
    } else {
      
      modML <- NA
    }
    
    MLActivityModels[[i]] <- append(MLActivityModels[[i]], list(modML))

  }
  
  
}


#### Activity plots ####
# Create nice colours
lightOrange <- rgb(1, 0.4, 0, alpha = 0.3)
darkOrange <- rgb(1, 0.4, 0, alpha = 1)

lightBlue <- rgb(0, 0.4, 0.8, alpha = 0.3)
darkBlue <- rgb(0, 0.4, 0.8, alpha = 1)

for(i in 1:length(focalSpecies)){
  
  pdf(paste0("../figures/activityPattern_", focalSpecies[i], ".pdf", width = 6, height = 6))
  
  par(mfrow = c(3,3), mar = c(2,2,2,2), oma = c(4,4,1,1))
  
  for(j in 1:length(thresholds)){
    
    if(! is.na(MLActivityModels[[j]][[i]])){
    plot(MLActivityModels[[j]][[i]], 
         dline = list(col = lightOrange), 
         tline = list(col = darkOrange, lwd = 2),
         cline = list(col = NULL),
         yunit = "density",
         main = paste0("Threshold: ", thresholds[j]),
         bty = "o")
    
    plot(manualActivityModels[[i]], 
         add = TRUE,
         dline = list(col = lightBlue), 
         tline = list(col = darkBlue, lwd =2),
         cline = list(col = NULL),
         yunit = "density")
    } else { plot(1,1, col = "white", axes = F, main = paste0("Threshold: ", thresholds[j]))
      box()}
  }
  
  mtext("Time (24 h)", 1, line = 2, outer = TRUE)
  mtext("Density", 2, line = 2, outer = TRUE)
  dev.off()
  
}


# Plot histograms for topLabelScore for each focal species
pdf("../figures/confidenceHists_reducedLabels.pdf", width = 10, height = 8)
par(mfrow = c(5,6), mar = c(2,2,2,2), oma = c(4,4,1,1))

for(i in 1:length(unique(mergeLabels_subset$topLabel))){
  uniqueSpecies <- unique(mergeLabels_subset$topLabel)

  hist(mergeLabels_subset[mergeLabels_subset$topLabel == uniqueSpecies[i], "topLabelScore"], main = uniqueSpecies[i], breaks = 15, cex.main = 1)
}

mtext("Label confidence (probability)", 1, line = 2, outer = TRUE)
mtext("Frequency", 2, line = 2, outer = TRUE)

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

# Now move to occupancy models
file.edit('unmarked_evalModels_LopeIvindo.R')
