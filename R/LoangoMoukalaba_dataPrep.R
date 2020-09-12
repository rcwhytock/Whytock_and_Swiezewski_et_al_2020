library(caret)
library(forcats)
library(yardstick)
library(ggplot2)
library(magick)
library(pheatmap)
library(viridis)
library(lubridate)
library(activity)

#### Prepare data ####
RobbieLabels <- read.csv("../data/S43_20190331_20190912_2020.06.16_10.46_All_records_export.csv")
AppsilonLabels <- read.csv("../data/stage4a-intermediate_bestmodel_preds_2020-06-15_Loango_Moukalaba.csv")

RobbieLabels$Species_Appsilon <- RobbieLabels$Species

dput(levels(RobbieLabels$Species_Appsilon))

levels(RobbieLabels$Species_Appsilon) <- c(
  "Badger_Honey",
  "Bat",
  "Bird",
  "Blank",
  "Buffalo_African",
  "Bushbaby_Greater",
  "Cat_Golden", # Was Cat_African_Golden
  "Chevrotain_Water",
  "Chimpanzee",
  "Civet_African",
  "Duiker_Blue",
  "Duiker_Red",
  "Duiker_Yellow_Backed",
  "Elephant_African",
  "Genet", # Was Genet servaline
  "Gorilla", # Was Gorilla_Western
  "Monkey", # Was "Guenon_Moustached"
  "Hippopotamus",
  "Hog_Red_River",
  "Human",
  "Insect",
  "Leopard_African",
  "Monkey", # "Mangabey_Collared"
  "Monkey",
  "Mongoose_Black_Footed",
  "Mongoose", # Was  "Mongoose_Water"
  "Monkey",
  "Pangolin",
  "Pangolin",
  "Porcupine_Brush_Tailed", # Was "Porcupine_African_Brush_Tailed
  "Rat_Giant",
  "Sitatunga",
  "Squirrel", # Was "Squirrel_African_Giant"
  "Unclassified",
  "Vehicle"
)

# Birds in reference need to be separated out into Guineafowl_Black, Guineafowl_Crested, Rail_Nkulengu and Bird

birds <- which(RobbieLabels$Species_Appsilon == "Bird")
RobbieLabels$Species_Appsilon <- as.character(RobbieLabels$Species_Appsilon)
for(i in 1:length(birds)){

    print(image_read(paste0("/home/robbie/Dropbox/Gabon_Loango_Moukalaba_Corridor/images_Gabon_Loango_moukalaba_Corridor/",
                            RobbieLabels[birds[i], "FileName_New"]))%>%
          image_scale(geometry = geometry_size_percent(width = 25)))
  RobbieLabels$Species_Appsilon[birds[i]] <- readline("Bird species: ")

}

# Save indices for later so don't have to repeat
dput(which(RobbieLabels$Species_Appsilon %in% c("Guineafowl_Black", "Bird", "Rail_Nkulengu", "Guineafowl_Crested")))

c(1L, 218L, 519L, 520L, 521L, 1001L, 1002L, 1003L, 1060L, 1061L, 
  1300L, 1431L, 1432L, 1433L, 1434L, 1435L, 1436L, 1437L, 1438L, 
  1439L, 1771L, 1772L, 1845L, 1846L, 1847L, 1848L, 1849L, 1850L, 
  1851L, 1852L, 1853L, 2190L, 2191L, 2364L, 2461L, 2661L, 2662L, 
  2663L, 2664L, 2665L, 2666L, 3114L, 3115L, 3116L, 3117L, 3118L, 
  3119L, 3120L, 3121L, 3493L, 3806L, 4634L, 4806L, 4807L, 5240L, 
  5241L, 5242L, 5243L, 5244L, 5245L, 5246L, 5771L, 5772L, 5773L, 
  5774L, 5775L, 5776L, 6369L, 6675L, 6832L, 7211L, 7391L, 7392L, 
  7529L, 7530L, 8323L, 8324L, 8325L, 8326L, 8327L, 8328L, 8329L, 
  8330L, 8331L, 8332L, 8333L, 8334L, 8335L, 8336L, 8337L, 8338L, 
  8339L, 8665L, 8666L, 8667L, 8668L, 8669L, 8670L, 8671L, 8672L, 
  8673L, 8674L, 8675L, 8676L, 9235L, 9678L, 9746L, 9747L, 9845L, 
  10286L, 10552L, 11013L, 11014L, 11481L, 11526L, 11732L, 12491L, 
  12659L, 12878L)


# Convert Species_Appsilon back to factor
RobbieLabels$Species_Appsilon <-as.factor(RobbieLabels$Species_Appsilon)

#### Choose top one accuracy for Appsilon ####
AppsilonLabels$topLabel <- NA
AppsilonLabels$topLabelScore <- 0

for(i in 1:nrow(AppsilonLabels)){
  
  AppsilonLabels$topLabel[i] <- names(which.max(AppsilonLabels[i,-c(1,30,31)]))
  AppsilonLabels$topLabelScore[i] <- max(AppsilonLabels[i,-c(1,30,31)])
  
}

# Merge AppsilonLabels with RobbieLabels
AppsilonLabels$FileName_New <- gsub(x = AppsilonLabels$X, pattern = ".*Corridor/", replacement = "")

# Merge the two DFs
mergeLabels <- merge(AppsilonLabels, RobbieLabels, by = "FileName_New")

mergeLabels$topOne <- ifelse(mergeLabels$Species_Appsilon == mergeLabels$topLabel, TRUE, FALSE)
100 - length(which(mergeLabels$topOne == FALSE)) / nrow(mergeLabels) * 100 # 78.201% Top one

##### Top five accuracy #####
mergeLabels$topFive <- FALSE

for(i in 1:nrow(mergeLabels)){
  
  newDat <- data.frame(probs = t(mergeLabels[i,c(3:30)])[,1],
                       species = row.names(t(mergeLabels[i,c(3:30)])))
  
  newDat <- newDat[order(newDat$probs, decreasing = T),]
  mergeLabels$topFive[i] <- mergeLabels$Species_Appsilon[i] %in% paste(newDat[1:5,"species"])
  
}

summary(mergeLabels$topFive)
100 - length(which(mergeLabels$topFive == FALSE)) / nrow(mergeLabels) * 100 # 90.40 Top five

#### Confusion Matrix ####
# Order factor levels
mergeLabels$topLabel <- fct_relevel(mergeLabels$topLabel, sort)
mergeLabels$Species_Appsilon <- fct_relevel(mergeLabels$Species_Appsilon)

# Make sure both DFs have same factor levels
which(!levels(mergeLabels$topLabel) %in% levels(mergeLabels$Species_Appsilon))
levels(mergeLabels$topLabel) 

missingTopLabel <- c(
  "Civet_African_Palm",
  "Mandrillus")

which(!levels(mergeLabels$Species_Appsilon) %in% levels(mergeLabels$topLabel))
levels(mergeLabels$Species_Appsilon)
missingSpeciesAppsilon <- c(
  "Badger_Honey",
  "Bat",
  "Bushbaby_Greater",
  "Civet_African",
  "Hippopotamus",
  "Insect",
  "Sitatunga",
  "Unclassified",
  "Vehicle")

# Add the missing labels from TopLabel into Species_Appsilon
levels(mergeLabels$Species_Appsilon) <- c(levels(mergeLabels$Species_Appsilon), missingTopLabel)
levels(mergeLabels$topLabel) <- c(levels(mergeLabels$topLabel), missingSpeciesAppsilon)

levels(mergeLabels$Species_Appsilon) %in% levels(mergeLabels$topLabel)
levels(mergeLabels$topLabel) %in% levels(mergeLabels$Species_Appsilon)

mergeLabels$topLabel <- fct_relevel(mergeLabels$topLabel, sort)
mergeLabels$Species_Appsilon <- fct_relevel(mergeLabels$Species_Appsilon, sort)
levels(mergeLabels$Species_Appsilon)
levels(mergeLabels$topLabel)

#### Create conf mat.
confusionMatrix(mergeLabels$topLabel, mergeLabels$Species_Appsilon)

cm <- conf_mat(data = mergeLabels, estimate = topLabel, truth = Species_Appsilon)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low= "#D6EAF8", high = "#2E86C1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Repeat but only with species used in training
mergeLabels_subset <- mergeLabels[-which(mergeLabels$Species_Appsilon %in% missingSpeciesAppsilon),]
#levels(mergeLabels_subset$Species_Appsilon) <- levels(mergeLabels_subset$topFive)
#mergeLabels_subset <- droplevels(mergeLabels_subset)

100 - length(which(mergeLabels_subset$topOne == FALSE)) / nrow(mergeLabels_subset) * 100 # 82.21% Top five
100 - length(which(mergeLabels_subset$topFive == FALSE)) / nrow(mergeLabels_subset) * 100 # 95.04% Top five

confusionMatrix(mergeLabels_subset$topLabel, mergeLabels_subset$Species_Appsilon)

cm2 <- conf_mat(data = mergeLabels_subset, estimate = topLabel, truth = Species_Appsilon)
autoplot(cm2, type = "heatmap") +
  scale_fill_gradient(low= "#D6EAF8", high = "#2E86C1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

write.csv(mergeLabels_subset, "../data/LoangoMoukalaba_corrected_mergelabels_subset_noCorrupted.csv", row.names = FALSE)
