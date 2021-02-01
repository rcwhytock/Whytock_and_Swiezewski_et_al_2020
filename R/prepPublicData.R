mergeLabels_subset_LopeIvindo <- read.csv("../data/LopeIvindo_corrected_mergeLabels_subset_noCorrupted.csv")
mergeLabels_subset_MoukalabaMayumba <- read.csv("../data/MoukalabaMayumba_corrected_mergelabels_subset_noCorrupted.csv")
mergeLabels_subset_LoangoMoukalaba <- read.csv("../data/LoangoMoukalaba_corrected_mergelabels_subset_noCorrupted.csv")

head(mergeLabels_subset_LopeIvindo)
head(mergeLabels_subset_MoukalabaMayumba)
head(mergeLabels_subset_LoangoMoukalaba)

mergeLabels_subset_LopeIvindo$Y <- NA
mergeLabels_subset_LopeIvindo$X.y <- NA

mergeLabels_subset_MoukalabaMayumba$Y <- NA
mergeLabels_subset_MoukalabaMayumba$X.y <- NA

mergeLabels_subset_LoangoMoukalaba$Y <- NA
mergeLabels_subset_LoangoMoukalaba$X.y <- NA

head(mergeLabels_subset_LopeIvindo)
head(mergeLabels_subset_MoukalabaMayumba)
head(mergeLabels_subset_LoangoMoukalaba)

write.csv(mergeLabels_subset_LopeIvindo, "../data/publicData/LopeIvindo_corrected_mergeLabels_subset_noCorrupted_public.csv")
write.csv(mergeLabels_subset_MoukalabaMayumba, "../data/publicData/MoukalabaMayumba_corrected_mergelabels_subset_noCorrupted_public.csv")
write.csv(mergeLabels_subset_LoangoMoukalaba, "../data/publicData/LoangoMoukalaba_corrected_mergelabels_subset_noCorrupted_public.csv")
