LoMay <- read.csv("../data/Camera_Active_Loango_Moukalaba2.csv")
MayMou <- read.csv("../data/Camera_Active_Mayumba_Moukalaba.csv")

# LoMay first
head(LoMay)
dates <- names(LoMay[1,c(6:171)])
dates <- sub(pattern = "X", replacement = "", x = dates)
dates <- sub(pattern = "\\.", replacement = "/", x = dates)
dates <- sub(pattern = "\\.", replacement = "/", x = dates)

LoMay$LoMay$deployDate <- NA
LoMay$endDateCorrected <- NA

for(i in 1:nrow(LoMay)){
  
  LoMay$deployDate[i] <- dates[min(which(LoMay[i,c(6:171)] == 1))]
  LoMay$endDateCorrected[i] <- dates[max(which(LoMay[i,c(6:171)] == 1))]
  
}

head(LoMay)

LoMay <- LoMay[,c(1,5,3,2,173,174)]
names(LoMay)[2] <- "elev"

write.csv(LoMay, "../data/camStations_LoangoMoukalaba_2020-06-24.csv", row.names = F)

# MayMou first
head(MayMou)
dates <- names(MayMou[1,c(9:121)])
dates <- sub(pattern = "X", replacement = "", x = dates)
dates <- sub(pattern = "\\.", replacement = "/", x = dates)
dates <- sub(pattern = "\\.", replacement = "/", x = dates)

MayMou$deployDate <- NA
MayMou$endDateCorrected <- NA

for(i in 1:nrow(MayMou)){
  
  MayMou$deployDate[i] <- dates[min(which(MayMou[i,c(9:121)] == 1))]
  MayMou$endDateCorrected[i] <- dates[max(which(MayMou[i,c(9:121)] == 1))]
  
}

head(MayMou)

MayMou <- MayMou[,c(1,6,3,2,7,8)]
names(MayMou)[2] <- "elev"

write.csv(MayMou, "../data/camStations_MayumbaMoukalaba_2020-06-24.csv", row.names = F)

