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

clubs <- as.list(c("Adelaide", "Brisbane Lions", "Carlton", "Collingwood", "Essendon",
           "Fremantle", "Geelong", "Gold Coast", "Greater Western Sydney",
           "Hawthorn", "Melbourne", "North Melbourne", "Port Adelaide",
           "Richmond", "St Kilda", "Sydney", "West Coast", "Western Bulldogs"))

#################
##
##  Section 2
##
##  Get Game by Game Stats
##
#################

load_gbg_stats <- function(club) {
        ## special handling for 6 teams
        if (club == "Brisbane Lions") {
                gbg_url <- "http://afltables.com/afl/stats/teams/brisbanel/2015_gbg.html"
        }
        else if (club == "Greater Western Sydney") {
                gbg_url <- "http://afltables.com/afl/stats/teams/gws/2015_gbg.html"
        }
        else if (club == "North Melbourne") {
                gbg_url <- "http://afltables.com/afl/stats/teams/kangaroos/2015_gbg.html"
        }
        else if (club == "Port Adelaide") {
                gbg_url <- "http://afltables.com/afl/stats/teams/padelaide/2015_gbg.html"
        }
        else if (club == "Sydney") {
                gbg_url <- "http://afltables.com/afl/stats/teams/swans/2015_gbg.html"
        }
        else if (club == "Western Bulldogs") {
                gbg_url <- "http://afltables.com/afl/stats/teams/bullldogs/2015_gbg.html"
        }
        else {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/", 
                        tolower(gsub(" ", "", club)), "/2015_gbg.html", sep = "")
        }
        
        gbg_filename <- paste("./files/", gsub(" ", "", club), "_GBG_2015.html", sep = "")
        
        if (!file.exists(gbg_filename)) {
                download.file(gbg_url, destfile = gbg_filename)
        }
        return(readHTMLTable(gbg_filename, stringsAsFactors = FALSE))
}

list_raw_gbg_2015 <- lapply(clubs, load_gbg_stats)
names(list_raw_gbg_2015) <- clubs

## remove unwanted stats (eg % game played, sub on/off etc) from each data fram within the lists:

list_raw_gbg_2015 <- lapply(list_raw_gbg_2015, function(x) {
        x <- x[-c(15, 21, 23, 24)]
})


clean_gbg_stats <- function(list_club_stats) {
        ## a function that gathers eack key stat and combines all into one dataframe
        ## per club. dplyr::full_join is used to join one by one. This is 'safer'
        ## than just using cbind to bind all at once.
        
        disp <- list_club_stats[[1]] %>% select(-Tot) %>% gather(Round, Disposals, -Player)
        kicks <- list_club_stats[[2]] %>% select(-Tot) %>% gather(Round, Kicks, -Player)
        club_stats <- full_join(disp, kicks)
        marks <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Marks, -Player)
        club_stats <- full_join(club_stats, marks)
        hballs <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Handballs, -Player)
        club_stats <- full_join(club_stats, hballs)
        goals <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Goals, -Player)
        club_stats <- full_join(club_stats, goals)
        behinds <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Behinds, -Player)
        club_stats <- full_join(club_stats, behinds)
        hitouts <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Hitouts, -Player)
        club_stats <- full_join(club_stats, hitouts)
        tackles <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Tackles, -Player)
        club_stats <- full_join(club_stats, tackles)
        r50s <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Rebound50s, -Player)
        club_stats <- full_join(club_stats, r50s)
        i50s <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Inside50s, -Player)
        club_stats <- full_join(club_stats, i50s)
        clears <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Clearances, -Player)
        club_stats <- full_join(club_stats, clears)
        clangs <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, Clangers, -Player)
        club_stats <- full_join(club_stats, clangs)
        frees_for <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, FreesFor, -Player)
        club_stats <- full_join(club_stats, frees_for)
        frees_ag <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, FreesAgainst, -Player)
        club_stats <- full_join(club_stats, frees_ag)
        con_pos <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, ContendedPossessions, -Player)
        club_stats <- full_join(club_stats, con_pos)
        uncon_pos <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, UncontendedPossessions, -Player)
        club_stats <- full_join(club_stats, uncon_pos)
        con_marks <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, ContestedMarks, -Player)
        club_stats <- full_join(club_stats, con_marks)
        marks_i50 <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, MarksInside50, -Player)
        club_stats <- full_join(club_stats, marks_i50)
        one_pers <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, OnePercenters, -Player)
        club_stats <- full_join(club_stats, one_pers)
        goal_ass <- list_club_stats[[3]] %>% select(-Tot) %>% gather(Round, GoalAssists, -Player)
        club_stats <- full_join(club_stats, goal_ass)
        
        for (i in 3:ncol(club_stats)) {
                club_stats[, i] <- as.numeric(club_stats[, i])
        }
        club_stats[is.na(club_stats)] <- 0
        
        club_stats <- club_stats[rowSums(club_stats[, 3:ncol(club_stats)]) > 0, ]
}

list_gbg_2015 <- lapply(list_raw_gbg_2015, clean_gbg_stats)

gbg_stats_2015 <- ldply(list_gbg_2015)
colnames(gbg_stats_2015)[1] <- "Team"


## add mean disposals, kicks, handballs, Inside50s and other key stats

avg_disp <- gbg_stats_2015 %>% group_by(Team, Player) %>% summarise(Avg.Disposals = mean(Disposals))
avg_ins50 <- gbg_stats_2015 %>% group_by(Team, Player) %>% summarise(Avg.Inside50s = mean(Inside50s))

gbg_stats_2015 <- left_join(gbg_stats_2015, avg_disp, by = "Player")
gbg_stats_2015 <- left_join(gbg_stats_2015, avg_ins50, by = "Player")

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

