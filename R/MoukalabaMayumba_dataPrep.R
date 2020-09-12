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
RobbieLabels <- read.csv("../data/S44_20190614_20191004_2020.06.16_10.47_All_records_export.csv")
AppsilonLabels <- read.csv("../data/stage4a-intermediate_bestmodel_preds_2020-06-15_Moukalaba_Mayumba.csv")

RobbieLabels$Species_Appsilon <- RobbieLabels$Species

dput(levels(RobbieLabels$Species_Appsilon))

levels(RobbieLabels$Species_Appsilon) <- c(
  "Bat",
  "Bird",
  "Blank",
  "Buffalo_African",
  "Bushbuck",
  "Cat_Golden", # Was Cat_African_Golden
  "Chevrotain_Water",
  "Chimpanzee",
  "Civet_African",
  "Civet_African_Palm",
  "Duiker_Blue",
  "Duiker_Red",
  "Duiker_Yellow_Backed",
  "Elephant_African",
  "Genet", # Was Genet servaline
  "Genet",
  "Gorilla",
  "Gorilla", # Was Gorilla_Western
  "Guineafowl_Black",
  "Hippopotamus",
  "Hog_Red_River",
  "Human",
  "Insect",
  "Leopard_African",
  "Mandrillus",
  "Mandrillus",
  "Monkey", # "Mangabey_Collared"
  "Mongoose",
  "Mongoose_Black_Footed",
  "Mongoose", # Was  "Mongoose_Water"
  "Monkey",
  "Pangolin",
  "Pangolin",
  "Porcupine_Brush_Tailed", # Was "Porcupine_African_Brush_Tailed
  "Porcupine_Brush_Tailed", # Was "Porcupine_African_Brush_Tailed
  "Rodent", # was Rat_Cane
  "Rat_Giant",
  "Serval", 
  "Squirrel",
  "Squirrel",  # Was "Squirrel_African_Giant"
  "Unclassified",
  "Waterbuck"
)

# Birds in reference need to be separated out into Guineafowl_Black, Guineafowl_Crested, Rail_Nkulengu and Bird

birds <- which(RobbieLabels$Species_Appsilon == "Bird")
RobbieLabels$Species_Appsilon <- as.character(RobbieLabels$Species_Appsilon)
for(i in 1:length(birds)){

    print(image_read(paste0("/home/robbie/Dropbox/Gabon_Moukalaba_Mayumba_Corridor/Images_Gabon_Moukalaba_Mayumba_Corridor/",
                            RobbieLabels[birds[i], "FileName_New"]))%>%
          image_scale(geometry = geometry_size_percent(width = 25)))
  RobbieLabels$Species_Appsilon[birds[i]] <- readline("Bird species: ")

}

# Save indices for later so don't have to repeat
dput(which(RobbieLabels$Species_Appsilon %in% c("Guineafowl_Black", "Bird", "Rail_Nkulengu", "Guineafowl_Crested")))

c(1144L, 1145L, 1323L, 1420L, 1544L, 2212L, 2213L, 2214L, 2216L, 
  2344L, 3550L, 3551L, 3552L, 3553L, 3603L, 3604L, 3605L, 3830L, 
  3965L, 4100L, 4306L, 4630L, 4649L, 4650L, 4651L, 4652L, 4653L, 
  4654L, 4655L)


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
100 - length(which(mergeLabels$topOne == FALSE)) / nrow(mergeLabels) * 100 # 71.701% Top one

##### Top five accuracy #####
mergeLabels$topFive <- FALSE

for(i in 1:nrow(mergeLabels)){
  
  newDat <- data.frame(probs = t(mergeLabels[i,c(3:30)])[,1],
                       species = row.names(t(mergeLabels[i,c(3:30)])))
  
  newDat <- newDat[order(newDat$probs, decreasing = T),]
  mergeLabels$topFive[i] <- mergeLabels$Species_Appsilon[i] %in% paste(newDat[1:5,"species"])
  
}

summary(mergeLabels$topFive)
100 - length(which(mergeLabels$topFive == FALSE)) / nrow(mergeLabels) * 100 # 87.47 Top five

#### Confusion Matrix ####
# Order factor levels
mergeLabels$topLabel <- fct_relevel(mergeLabels$topLabel, sort)
mergeLabels$Species_Appsilon <- fct_relevel(mergeLabels$Species_Appsilon)

# Make sure both DFs have same factor levels
which(!levels(mergeLabels$topLabel) %in% levels(mergeLabels$Species_Appsilon))
levels(mergeLabels$topLabel) 

missingTopLabel <- c(
  "Rail_Nkulengu")

which(!levels(mergeLabels$Species_Appsilon) %in% levels(mergeLabels$topLabel))
levels(mergeLabels$Species_Appsilon)
missingSpeciesAppsilon <- c(
  "Bat",
  "Bushbuck",
  "Civet_African",
  "Guineafowl_Crested",
  "Hippopotamus",
  "Insect",
  "Rodent",
  "Serval",
  "Unclassified",
  "Waterbuck")

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

100 - length(which(mergeLabels_subset$topOne == FALSE)) / nrow(mergeLabels_subset) * 100 # 76.55% Top five
100 - length(which(mergeLabels_subset$topFive == FALSE)) / nrow(mergeLabels_subset) * 100 # 93.29% Top five

confusionMatrix(mergeLabels_subset$topLabel, mergeLabels_subset$Species_Appsilon)

cm2 <- conf_mat(data = mergeLabels_subset, estimate = topLabel, truth = Species_Appsilon)
autoplot(cm2, type = "heatmap") +
  scale_fill_gradient(low= "#D6EAF8", high = "#2E86C1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

write.csv(mergeLabels_subset, "../data/MoukalabaMayumba_corrected_mergelabels_subset_noCorrupted.csv", row.names = FALSE)
