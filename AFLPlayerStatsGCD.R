#################
##
##  Section 1
##
##  Load packages and set up environment.
##
#################

setwd("~/RFolder/AFLPlayerStatsGCD")

library(XML)
library(plyr)
library(dplyr)
library(tidyr)

#################
##
##  Section 2
##
##  Get Game by Game Stats
##
#################

## Get game by game stats. So far this is just for Adelaide.

gbg_url <- "http://afltables.com/afl/stats/teams/adelaide/2015_gbg.html"

if(!(file.exists("./files/adelgbg2015.html"))){
        download.file(gbg_url, destfile="./files/adelgbg2015.html")
}

adel_gbg_2015 <- readHTMLTable("./files/adelgbg2015.html", stringsAsFactors = FALSE)

adel_DI <- adel_gbg_2015[[1]] %>% select(-Tot) %>% gather(Round, Disposals, -Player)
adel_KI <- adel_gbg_2015[[2]] %>% select(-Tot) %>% gather(Round, Kicks, -Player)
adel_2015 <- full_join(adel_DI, adel_KI)
adel_MK <- adel_gbg_2015[[3]] %>% select(-Tot) %>% gather(Round, Marks, -Player)
adel_2015 <- full_join(adel_2015, adel_MK)
adel_HB <- adel_gbg_2015[[4]] %>% select(-Tot) %>% gather(Round, Handballs, -Player)
adel_2015 <- full_join(adel_2015, adel_HB)
adel_GL <- adel_gbg_2015[[5]] %>% select(-Tot) %>% gather(Round, Goals, -Player)
adel_2015 <- full_join(adel_2015, adel_GL)
adel_BH <- adel_gbg_2015[[6]] %>% select(-Tot) %>% gather(Round, Behinds, -Player)
adel_2015 <- full_join(adel_2015, adel_BH)
adel_HO <- adel_gbg_2015[[7]] %>% select(-Tot) %>% gather(Round, Hitouts, -Player)
adel_2015 <- full_join(adel_2015, adel_HO)
adel_TK <- adel_gbg_2015[[8]] %>% select(-Tot) %>% gather(Round, Tackles, -Player)
adel_2015 <- full_join(adel_2015, adel_TK)
adel_RB <- adel_gbg_2015[[9]] %>% select(-Tot) %>% gather(Round, Rebound50s, -Player)
adel_2015 <- full_join(adel_2015, adel_RB)
adel_I5 <- adel_gbg_2015[[10]] %>% select(-Tot) %>% gather(Round, Inside50s, -Player)
adel_2015 <- full_join(adel_2015, adel_I5)
adel_CL <- adel_gbg_2015[[11]] %>% select(-Tot) %>% gather(Round, Clearances, -Player)
adel_2015 <- full_join(adel_2015, adel_CL)
adel_CG <- adel_gbg_2015[[12]] %>% select(-Tot) %>% gather(Round, Clangers, -Player)
adel_2015 <- full_join(adel_2015, adel_CG)
adel_FF <- adel_gbg_2015[[13]] %>% select(-Tot) %>% gather(Round, FreesFor, -Player)
adel_2015 <- full_join(adel_2015, adel_FF)
adel_FA <- adel_gbg_2015[[14]] %>% select(-Tot) %>% gather(Round, FreesAgainst, -Player)
adel_2015 <- full_join(adel_2015, adel_FA)
adel_CP <- adel_gbg_2015[[16]] %>% select(-Tot) %>% gather(Round, ContestedPossessions, -Player)
adel_2015 <- full_join(adel_2015, adel_CP)
adel_UP <- adel_gbg_2015[[17]] %>% select(-Tot) %>% gather(Round, UncontestedPossessions, -Player)
adel_2015 <- full_join(adel_2015, adel_UP)
adel_CM <- adel_gbg_2015[[18]] %>% select(-Tot) %>% gather(Round, ContestedMarks, -Player)
adel_2015 <- full_join(adel_2015, adel_CM)
adel_MI <- adel_gbg_2015[[19]] %>% select(-Tot) %>% gather(Round, MarksInside50, -Player)
adel_2015 <- full_join(adel_2015, adel_MI)
adel_OP <- adel_gbg_2015[[20]] %>% select(-Tot) %>% gather(Round, OnePercenters, -Player)
adel_2015 <- full_join(adel_2015, adel_OP)
adel_GA <- adel_gbg_2015[[22]] %>% select(-Tot) %>% gather(Round, GoalAssists, -Player)
adel_2015 <- full_join(adel_2015, adel_GA)

for (i in 3:ncol(adel_2015)) {
        adel_2015[, i] <- as.numeric(adel_2015[, i])
}
adel_2015[is.na(adel_2015)] <- 0

adel_2015 <- adel_2015[rowSums(adel_2015[, 3:22]) > 0, ]

## add mean disposals, kicks, handballs, Inside50s and other key stats

adel_ave_disp_2015 <- adel_2015 %>% group_by(Player) %>% summarise(Avg.Disposals = mean(Disposals))
adel_ave_ins50_2015 <- adel_2015 %>% group_by(Player) %>% summarise(Avg.Inside50s = mean(Inside50s))

adel_2015 <- left_join(adel_2015, adel_ave_disp_2015, by = "Player")
adel_2015 <- left_join(adel_2015, adel_ave_ins50_2015, by = "Player")

#################
##
##  Section 3
##
##  Get Round by Round Score, Win/Loss, Opposition, Margin, Venue for every team.
##
#################

## So far this is just for 2015

rbr_url <- "http://afltables.com/afl/seas/2015.html"

if(!(file.exists("./files/rbr2015.html"))){
        download.file(rbr_url, destfile="./files/rbr2015.html")
}

rbr_2015 <- readHTMLTable("./files/rbr2015.html", stringsAsFactors = FALSE)

## clean rbr_2015 to make it a list of dataframes. Each dataframe is a Round. 
## Remove all other entries in the list from rbr_2015.
## At this point it omits Finals - to be added later.

mylist <- lapply(rbr_2015, function(df) {
        return (dim(df)[1] > 18)
}
)

mylist <- as.vector(mylist, mode = "logical")
mylist[is.na(mylist)] <- FALSE
rbr_2015 <- rbr_2015[mylist]
names(rbr_2015) <- paste("R", 1:23, sep = "")

round_details <- function(df) {
        WinLoss <- rep(NA, nrow(df))
        Margin <- rep(NA, nrow(df))
        Opposition <- rep(NA, nrow(df))
        Date <- rep(as.Date(NA), nrow(df))
        Venue <- rep(NA, nrow(df))
        for (i in 1:as.integer(nrow(df) / 2)) {
                Date[2 * i - 1] <- as.Date(df$Remarks[2 * i - 1], format = "%a %d-%b-%Y")
                Date[2 * i] <- as.Date(df$Remarks[2 * i - 1], format = "%a %d-%b-%Y")
                Margin[2 * i - 1] <- df$Score[2 * i - 1] - df$Score[2 * i]
                Margin[2 * i] <- df$Score[2 * i] - df$Score[2 * i - 1]
                Opposition[2 * i - 1] <- df$Team[2 * i]
                Opposition[2 * i] <- df$Team[2 * i - 1]
                Venue[2 * i - 1] <- strsplit(df$Remarks[2 * i - 1], ".*Venue: ")[[1]][2]
                Venue[2 * i] <- strsplit(df$Remarks[2 * i - 1], ".*Venue: ")[[1]][2]
                if (df$Score[2 * i - 1] > df$Score[2 * i]) {
                        WinLoss[2 * i - 1] <- "W"
                        WinLoss[2 * i] <- "L"
                }
                else if (df$Score[2 * i - 1] < df$Score[2 * i]) {
                        WinLoss[2 * i - 1] <- "L"
                        WinLoss[2 * i] <- "W"
                }
                else {
                        WinLoss[2 * i - 1] <- "D"
                        WinLoss[2 * i] <- "D"
                }
        }
        data.frame(Date, WinLoss, Margin, Opposition, Venue)
}

## Now create a list of 23 dataframes, each dataframe is a Round, in the desired format.
rbr_2015_list <- lapply(rbr_2015, function(df) {
        r_res <- df %>% select(V1, V3, V4) %>% slice(1:18) %>% filter(!is.na(V4))
        names(r_res) <- c("Team", "Score", "Remarks")
        r_res$Score <- as.integer(r_res$Score)
        r_res <- bind_cols(r_res, round_details(r_res))
        r_res
})

## Now combine the list of 23 dataframes into 1 larger one.
rbr_2015 <- ldply(rbr_2015_list)
colnames(rbr_2015)[1] <- "Round"


#################
##
##  Section 4
##
##  Combine Game by Game with Round by Round
##
#################

## So far this is just for Adelaide.

rbr_adel_2015 <- rbr_2015 %>% select (-Remarks) %>% filter(Team == "Adelaide")

all_adel_2015 <- left_join(adel_2015, rbr_adel_2015, by = "Round")

