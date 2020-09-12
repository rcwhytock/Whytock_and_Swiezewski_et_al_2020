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
RobbieLabels <- read.csv("../data/CORRECT_S40_20180116_20180510_2020.04.28_13.18_All_records_export.csv")
AppsilonLabels <- read.csv("../data/stage4a-intermediate_bestmodel_preds_2020-04-28_Lope_Ivindo.csv")

RobbieLabels$Species_Appsilon <- RobbieLabels$Species

dput(levels(RobbieLabels$Species_Appsilon))

levels(RobbieLabels$Species_Appsilon) <- c(
  "Aardvark", 
  "Badger_Honey",
  "Bat",
  "Bird",
  "Blank",
  "Buffalo_African",
  "Cat_Golden", # Was Cat_African_Golden
  "Chevrotain_Water",
  "Chimpanzee",
  "Civet_African",
  "Duiker_Blue",
  "Duiker_Red",
  "Duiker_Yellow_Backed",
  "Elephant_African",
  "Genet", # Was Genet_Servaline
  "Gorilla", # Was Gorilla_Western
  "Monkey", # Was Guenon_Greater_Spot_Nosed
  "Monkey", # Was Guenon_Moustached
  "Hog_Red_River",
  "Human",
  "Insect",
  "Leopard_African",
  "Mandrillus", # Was Mandrill,
  "Mongoose_Black_Footed",
  "Monkey", # Mongoose_Water
  "Monkey", # Was Monkey_Sun_Tailed
  "Pangolin", # Pangolin_Giant
  "Porcupine_Brush_Tailed", # Was Porcupine_African_Brush_Tailed
  "Squirrel", # Was Squirrel_African_Giant
  "Unclassified",
  "Human" # Was vehicle
)

# Birds in reference need to be separated out into Guineafowl_Black, Guineafowl_Crested, Rail_Nkulengu and Bird

# birds <- which(RobbieLabels$Species_Appsilon == "Bird")
# RobbieLabels$Species_Appsilon <- as.character(RobbieLabels$Species_Appsilon)
# for(i in 1:length(birds)){
#   
#     print(image_read(paste0("/home/robbie/Dropbox/Lope_Ivindo CT pictures/S40_20180116_20180510/images/", 
#                             RobbieLabels[birds[i], "FileName_New"]))%>% 
#           image_scale(geometry = geometry_size_percent(width = 25)))
#   RobbieLabels$Species_Appsilon[birds[i]] <- readline("Bird species: ")
#   
# }

# Save indices for later so don't have to repeat
# dput(which(RobbieLabels$Species_Appsilon == c("Guineafowl_Black")))
levels(RobbieLabels$Species_Appsilon) <- c(levels(RobbieLabels$Species_Appsilon), "Guineafowl_Black")
RobbieLabels[c(4L, 5L, 3883L, 4673L, 4674L, 6353L, 6354L),"Species_Appsilon"] <- "Guineafowl_Black"

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
AppsilonLabels$FileName_New <- gsub(x = AppsilonLabels$X, pattern = ".*images/", replacement = "")

# Merge the two DFs
mergeLabels <- merge(AppsilonLabels, RobbieLabels, by = "FileName_New")

mergeLabels$topOne <- ifelse(mergeLabels$Species_Appsilon == mergeLabels$topLabel, TRUE, FALSE)
100 - length(which(mergeLabels$topOne == FALSE)) / nrow(mergeLabels) * 100 # 68.70% Top one

##### Top five accuracy #####
mergeLabels$topFive <- FALSE

for(i in 1:nrow(mergeLabels)){
  
  newDat <- data.frame(probs = t(mergeLabels[i,c(3:30)])[,1],
                       species = row.names(t(mergeLabels[i,c(3:30)])))
  
  newDat <- newDat[order(newDat$probs, decreasing = T),]
  mergeLabels$topFive[i] <- mergeLabels$Species_Appsilon[i] %in% paste(newDat[1:5,"species"])
  
}

summary(mergeLabels$topFive)
100 - length(which(mergeLabels$topFive == FALSE)) / nrow(mergeLabels) * 100 # 92.05 Top five

#### Confusion Matrix ####
# Order factor levels
mergeLabels$topLabel <- fct_relevel(mergeLabels$topLabel, sort)
mergeLabels$Species_Appsilon <- fct_relevel(mergeLabels$Species_Appsilon)

# Make sure both DFs have same factor levels
which(!levels(mergeLabels$topLabel) %in% levels(mergeLabels$Species_Appsilon))
levels(mergeLabels$topLabel) 

missingTopLabel <- c(
  "Civet_African_Palm",
  "Guineafowl_Black",
  "Mongoose",
  "Rail_Nkulengu",
  "Rat_Giant")

which(!levels(mergeLabels$Species_Appsilon) %in% levels(mergeLabels$topLabel))
levels(mergeLabels$Species_Appsilon)
missingSpeciesAppsilon <- c(
  "Aardvark",
  "Badger_Honey",
  "Bat",
  "Civet_African",
  "Insect",
  "Unclassified")

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

100 - length(which(mergeLabels_subset$topOne == FALSE)) / nrow(mergeLabels_subset) * 100 # 69.14% Top five
100 - length(which(mergeLabels_subset$topFive == FALSE)) / nrow(mergeLabels_subset) * 100 # 92.64% Top five

confusionMatrix(mergeLabels_subset$topLabel, mergeLabels_subset$Species_Appsilon)

cm2 <- conf_mat(data = mergeLabels_subset, estimate = topLabel, truth = Species_Appsilon)
autoplot(cm2, type = "heatmap") +
  scale_fill_gradient(low= "#D6EAF8", high = "#2E86C1") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# blanks seem to be an issue, are the blanks classified as animals really blank?
# birds in reference need to be separated out into guineafowl_black, guineafowl_crested, rail_nkulengu and bird

 blanks <- which(mergelabels_subset$species_appsilon == "blank" & mergelabels_subset$toplabel != "blank")
 mergelabels_subset$newblanks <- ""

 for(i in 1054:length(blanks)){

     print(
       image_read(
         paste0(
           "/home/robbie/dropbox/lope_ivindo ct pictures/s40_20180116_20180510/images/",
           mergelabels_subset[blanks[i], "filename_new"]
         )
       ) %>%
         image_scale(geometry = geometry_size_percent(width = 35)) %>%
         image_annotate(
           paste0(
             mergelabels_subset[blanks[i], "toplabel"],
             " | ",
             round(mergelabels_subset[blanks[i], "toplabelscore"], digits = 3)
           ),
           boxcolor = "white",
           size = 20
         )
     )
     mergelabels_subset$newblanks[blanks[i]] <-
       readline("blank new : ")

  }

 dput(mergelabels_subset$newblanks)

 mergelabels_subset$newblanks <- ifelse(mergelabels_subset$newblanks == "c", as.character(mergelabels_subset$toplabel), mergelabels_subset$newblnks)
 unique(mergelabels_subset$newblanks)

 # fix typos
 mergelabels_subset$newblanks[which(mergelabels_subset$newblanks %in% c("``", "]"))] <- ""

 mergelabels_subset$species_appsilon[which(! mergelabels_subset$newblanks == "")] <- mergelabels_subset$newblanks[which(! mergelabels_subset$newlanks == "")]

write.csv(mergelabels_subset, "../data/corrected_mergelabels_subset.csv", row.names = f)

### load in cleaned data for analysis ####
mergelabels_subset <- read.csv("../data/corrected_mergelabels_subset.csv")

#some images seemed to be corrupt, lets check
 mergelabels_subset$corrupted <- true

 for (i in 1:nrow(mergelabels_subset)) {

   skip_to_next <- false

   # note that print(b) fails since b doesn't exist

   trycatch(

     image_read(paste0(
       "/home/robbie/dropbox/lope_ivindo ct pictures/s40_20180116_20180510/images/",
       mergelabels_subset[i, "filename_new"]
     )
   ),
   error = function(e) {
     skip_to_next <<- true
   })

   if(skip_to_next) {

     mergelabels_subset$corrupted[i] <- false

     next }
 }
 table(mergelabels_subset$corrupted)

write.csv(mergelabels_subset, "../data/corrected_mergelabels_subset_nocorrupted.csv", row.names = f)