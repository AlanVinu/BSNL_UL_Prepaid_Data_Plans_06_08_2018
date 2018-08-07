#Initial Ranking Formula
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018[15:20] <- 0
colnames(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018)[15:20] <- c("Rank(Days)","Rank(Price per day)","Rank(3G Data per day)","Rank(Data per month)","Score","Final Rank")
for (i in 1:nrow(BSNL_Prepaid_Data_Plans_as_per_16_08_2018)) {
  BSNL_Prepaid_Data_Plans_as_per_16_08_2018$Score[i] =
    (
      max(BSNL_Prepaid_Data_Plans_as_per_16_08_2018$Days) - BSNL_Prepaid_Data_Plans_as_per_16_08_2018$Days[i]
    ) + (abs(
      min(BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Price per day`) - BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Price per day`[i]
    )) + (
      max(BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`3G Data per day`) - BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`3G Data per day`[i]
    ) + (
      max(BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Data per month`) - BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Data per month`[i]
    )
}


#Latest Ranking Formula
for (i in 1:nrow(BSNL_Prepaid_Data_Plans_as_per_16_08_2018)) {
  BSNL_Prepaid_Data_Plans_as_per_16_08_2018$Score[i] = BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Rank(3G Data per day)`[i] + BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Rank(Data per month)`[i] + BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Rank(Days)`[i] + BSNL_Prepaid_Data_Plans_as_per_16_08_2018$`Rank(Price per day)`[i]
}