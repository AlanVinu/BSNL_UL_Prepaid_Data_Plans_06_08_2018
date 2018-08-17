


BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018[15:20] <- 0
colnames(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018)[15:20] <-
  c(
    "Rank(Days)",
    "Rank(Price per day)",
    "Rank(3G Data per day)",
    "Rank(Data per month)",
    "Score",
    "Final Rank"
  )

#Initial Ranking Formula
for (i in 1:nrow(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018)) {
  BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Score[i] =
    (
      max(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Days) - BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Days[i]
    ) + (abs(
      min(
        BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Price per day`
      ) - BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Price per day`[i]
    )) + (
      max(
        BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`3G Data per day`
      ) - BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`3G Data per day`[i]
    ) + (
      max(
        BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Data per month`
      ) - BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Data per month`[i]
    )
}


#Latest Ranking Formula
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Days)` <-
  ave(
    BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Days,
    FUN = function(x)
      rank(-x)
  )
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Price per day)` <-
  ave(
    BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Price per day`,
    FUN = function(x)
      rank(x)
  )
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(3G Data per day)` <-
  ave(
    BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`3G Data per day`,
    FUN = function(x)
      rank(-x)
  )
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Data per month)` <-
  ave(
    BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Data per month`,
    FUN = function(x)
      rank(-x)
  )
#Weighted Ranks
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Days)`[1:15] <-
  (BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Days)`[1:15]) * (1 /
                                                                         (
                                                                           BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Days[1:15] / mean(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Days)
                                                                         ))
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Price per day)`[1:15] <-
  (BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Price per day)`[1:15]) * (
    BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Price per day`[1:15] / mean(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Price per day`)
  )
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(3G Data per day)`[1:15] <-
  (BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(3G Data per day)`[1:15]) * (1 /
                                                                                    (
                                                                                      BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`3G Data per day`[1:15] / mean(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`3G Data per day`)
                                                                                    ))
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Data per month)`[1:15] <-
  (BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Data per month)`[1:15]) * (1 /
                                                                                    (
                                                                                      BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Data per month`[1:15] / mean(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Data per month`)
                                                                                    ))

for (i in 1:nrow(BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018)) {
  BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Score[i] = BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(3G Data per day)`[i] + BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Data per month)`[i] + BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Days)`[i] + BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Rank(Price per day)`[i]
}
BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$`Final Rank` <-
  ave(
    BSNL_UL_Prepaid_Data_Plans_as_per_06_08_2018$Score,
    FUN = function(x)
      rank(x)
  )