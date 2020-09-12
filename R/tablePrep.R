tabs <- list.files("../results/", full = T)
tabs <- tabs[2:11] # get the confMatrix results

tabsList <- vector("list", length = length(tabs))
thresholds <- c(seq(0.1,0.9, 0.1), 0)

for(i in 1:length(tabs)){
  
  tabsList[[i]] <- read.csv(tabs[i])
  tabsList[[i]]$threshold <- thresholds[i]*100
  
}

tabsList

tabsRes <- do.call("rbind", tabsList)

head(tabsRes)

names(tabsRes)[1] <- "species"

# Only keep 4 mearues of interest and prevalance
tabsRes <- tabsRes[,c(1,6,7,8,9,12,13)]

write.csv(tabsRes, "../results/combinedConfMats.csv")
