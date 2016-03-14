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

load_gbg_stats <- function(club, year = 2015) {
        year <- as.character(year)
        ## special handling for 6 teams
        if (club == "Brisbane Lions") {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/brisbanel/", year, "_gbg.html", sep = "")
        }
        else if (club == "Greater Western Sydney") {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/gws/", year, "_gbg.html", sep = "")
        }
        else if (club == "North Melbourne") {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/kangaroos/", year, "_gbg.html", sep = "")
        }
        else if (club == "Port Adelaide") {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/padelaide/", year, "_gbg.html", sep = "")
        }
        else if (club == "Sydney") {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/swans/", year, "_gbg.html", sep = "")
        }
        else if (club == "Western Bulldogs") {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/bullldogs/", year, "_gbg.html", sep = "")
        }
        else {
                gbg_url <- paste("http://afltables.com/afl/stats/teams/", 
                        tolower(gsub(" ", "", club)), "/", year, "_gbg.html", sep = "")
        }
        
        gbg_filename <- paste("./files/", gsub(" ", "", club), "_GBG_", year, ".html", sep = "")
        
        if (!file.exists(gbg_filename)) {
                download.file(gbg_url, destfile = gbg_filename)
        }
        return(readHTMLTable(gbg_filename, stringsAsFactors = FALSE))
}

list_raw_gbg_2015 <- lapply(clubs, load_gbg_stats)
names(list_raw_gbg_2015) <- clubs

list_raw_gbg_2014 <- lapply(clubs, load_gbg_stats, year = 2014)
names(list_raw_gbg_2014) <- clubs

list_raw_gbg_2013 <- lapply(clubs, load_gbg_stats, year = 2013)
names(list_raw_gbg_2013) <- clubs

## remove unwanted stats (eg % game played, sub on/off etc) from each data fram within the lists:

list_raw_gbg_2015 <- lapply(list_raw_gbg_2015, function(x) {
        x <- x[-c(15, 21, 23, 24)]
})

list_raw_gbg_2014 <- lapply(list_raw_gbg_2014, function(x) {
        x <- x[-c(15, 21, 23, 24)]
})

list_raw_gbg_2013 <- lapply(list_raw_gbg_2013, function(x) {
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
        hballs <- list_club_stats[[4]] %>% select(-Tot) %>% gather(Round, Handballs, -Player)
        club_stats <- full_join(club_stats, hballs)
        goals <- list_club_stats[[5]] %>% select(-Tot) %>% gather(Round, Goals, -Player)
        club_stats <- full_join(club_stats, goals)
        behinds <- list_club_stats[[6]] %>% select(-Tot) %>% gather(Round, Behinds, -Player)
        club_stats <- full_join(club_stats, behinds)
        hitouts <- list_club_stats[[7]] %>% select(-Tot) %>% gather(Round, Hitouts, -Player)
        club_stats <- full_join(club_stats, hitouts)
        tackles <- list_club_stats[[8]] %>% select(-Tot) %>% gather(Round, Tackles, -Player)
        club_stats <- full_join(club_stats, tackles)
        r50s <- list_club_stats[[9]] %>% select(-Tot) %>% gather(Round, Rebound50s, -Player)
        club_stats <- full_join(club_stats, r50s)
        i50s <- list_club_stats[[10]] %>% select(-Tot) %>% gather(Round, Inside50s, -Player)
        club_stats <- full_join(club_stats, i50s)
        clears <- list_club_stats[[11]] %>% select(-Tot) %>% gather(Round, Clearances, -Player)
        club_stats <- full_join(club_stats, clears)
        clangs <- list_club_stats[[12]] %>% select(-Tot) %>% gather(Round, Clangers, -Player)
        club_stats <- full_join(club_stats, clangs)
        frees_for <- list_club_stats[[13]] %>% select(-Tot) %>% gather(Round, FreesFor, -Player)
        club_stats <- full_join(club_stats, frees_for)
        frees_ag <- list_club_stats[[14]] %>% select(-Tot) %>% gather(Round, FreesAgainst, -Player)
        club_stats <- full_join(club_stats, frees_ag)
        con_pos <- list_club_stats[[15]] %>% select(-Tot) %>% gather(Round, ContendedPossessions, -Player)
        club_stats <- full_join(club_stats, con_pos)
        uncon_pos <- list_club_stats[[16]] %>% select(-Tot) %>% gather(Round, UncontendedPossessions, -Player)
        club_stats <- full_join(club_stats, uncon_pos)
        con_marks <- list_club_stats[[17]] %>% select(-Tot) %>% gather(Round, ContestedMarks, -Player)
        club_stats <- full_join(club_stats, con_marks)
        marks_i50 <- list_club_stats[[18]] %>% select(-Tot) %>% gather(Round, MarksInside50, -Player)
        club_stats <- full_join(club_stats, marks_i50)
        one_pers <- list_club_stats[[19]] %>% select(-Tot) %>% gather(Round, OnePercenters, -Player)
        club_stats <- full_join(club_stats, one_pers)
        goal_ass <- list_club_stats[[20]] %>% select(-Tot) %>% gather(Round, GoalAssists, -Player)
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

list_gbg_2014 <- lapply(list_raw_gbg_2014, clean_gbg_stats)
gbg_stats_2014 <- ldply(list_gbg_2014)
colnames(gbg_stats_2014)[1] <- "Team"

list_gbg_2013 <- lapply(list_raw_gbg_2013, clean_gbg_stats)
gbg_stats_2013 <- ldply(list_gbg_2013)
colnames(gbg_stats_2013)[1] <- "Team"


#################
##
##  Section 3
##
##  Get Round by Round Score, Win/Loss, Opposition, Margin, Venue for every team.
##
#################


rbr_url <- "http://afltables.com/afl/seas/2015.html"

if(!(file.exists("./files/rbr2015.html"))){
        download.file(rbr_url, destfile="./files/rbr2015.html")
}

list_raw_rbr_2015 <- readHTMLTable("./files/rbr2015.html", stringsAsFactors = FALSE)

rbr_url <- "http://afltables.com/afl/seas/2014.html"

if(!(file.exists("./files/rbr2014.html"))){
        download.file(rbr_url, destfile="./files/rbr2014.html")
}

list_raw_rbr_2014 <- readHTMLTable("./files/rbr2014.html", stringsAsFactors = FALSE)

rbr_url <- "http://afltables.com/afl/seas/2013.html"

if(!(file.exists("./files/rbr2013.html"))){
        download.file(rbr_url, destfile="./files/rbr2013.html")
}

list_raw_rbr_2013 <- readHTMLTable("./files/rbr2013.html", stringsAsFactors = FALSE)


## clean list_raw_rbr_YYYY to make each a list of dataframes. Each dataframe is a Round. 
## Remove all other entries in the list from list_raw_rbr_YYYY.

## First, extract just the home and away rounds, removing everything else.

mylist <- lapply(list_raw_rbr_2015, function(df) {
        return (dim(df)[1] > 18)
})

mylist2014 <- lapply(list_raw_rbr_2014, function(df) {
        return (dim(df)[1] > 18)
})

mylist2013 <- lapply(list_raw_rbr_2013, function(df) {
        return (dim(df)[1] > 18)
})

mylist <- as.vector(mylist, mode = "logical")
mylist[is.na(mylist)] <- FALSE
list_raw_home_away_2015 <- list_raw_rbr_2015[mylist]
names(list_raw_home_away_2015) <- paste("R", 1:23, sep = "")

mylist2014 <- as.vector(mylist2014, mode = "logical")
mylist2014[is.na(mylist2014)] <- FALSE
list_raw_home_away_2014 <- list_raw_rbr_2014[mylist2014]
names(list_raw_home_away_2014) <- paste("R", 1:23, sep = "")

mylist2013 <- as.vector(mylist2013, mode = "logical")
mylist2013[is.na(mylist2013)] <- FALSE
list_raw_home_away_2013 <- list_raw_rbr_2013[mylist2013]
names(list_raw_home_away_2013) <- paste("R", 1:23, sep = "")

## Next, extract the finals series and work on them separately.

## list_raw_finals_2015 <- tail(list_raw_rbr_2015, n = 18)
list_raw_finals_2015 <- readHTMLTable("./files/rbr2015.html", stringsAsFactors = FALSE,
        which = (length(list_raw_rbr_2015) - 17):length(list_raw_rbr_2015), header = FALSE)
mylist <- sapply(list_raw_finals_2015, function(df){
        return (dim(df)[2] > 2)
})

list_raw_finals_2014 <- readHTMLTable("./files/rbr2014.html", stringsAsFactors = FALSE,
        which = (length(list_raw_rbr_2014) - 17):length(list_raw_rbr_2014), header = FALSE)
mylist2014 <- sapply(list_raw_finals_2014, function(df){
        return (dim(df)[2] > 2)
})

list_raw_finals_2013 <- readHTMLTable("./files/rbr2013.html", stringsAsFactors = FALSE,
        which = (length(list_raw_rbr_2013) - 17):length(list_raw_rbr_2013), header = FALSE)
mylist2013 <- sapply(list_raw_finals_2013, function(df){
        return (dim(df)[2] > 2)
})

list_raw_finals_2015 <- list_raw_finals_2015[mylist]
names(list_raw_finals_2015) <- c("QF", "QF", "EF", "EF", "SF", "SF", "PF", "PF", "GF")

list_raw_finals_2014 <- list_raw_finals_2014[mylist2014]
names(list_raw_finals_2015) <- c("QF", "QF", "EF", "EF", "SF", "SF", "PF", "PF", "GF")

list_raw_finals_2013 <- list_raw_finals_2013[mylist2014]
names(list_raw_finals_2015) <- c("QF", "QF", "EF", "EF", "SF", "SF", "PF", "PF", "GF")

## Now, join the home and away list to the finals list

list_raw_rbr_2015 <- list_raw_home_away_2015
list_raw_rbr_2015[(length(list_raw_rbr_2015) + 1):(length(list_raw_rbr_2015) + 
                length(list_raw_finals_2015))] <- list_raw_finals_2015
names(list_raw_rbr_2015) <- c(paste("R", 1:23, sep = ""), "QF", "QF", "EF", "EF", "SF", "SF", "PF", "PF", "GF")

list_raw_rbr_2014 <- list_raw_home_away_2014
list_raw_rbr_2014[(length(list_raw_rbr_2014) + 1):(length(list_raw_rbr_2014) + 
                length(list_raw_finals_2014))] <- list_raw_finals_2014
names(list_raw_rbr_2014) <- c(paste("R", 1:23, sep = ""), "QF", "QF", "EF", "EF", "SF", "SF", "PF", "PF", "GF")

list_raw_rbr_2013 <- list_raw_home_away_2013
list_raw_rbr_2013[(length(list_raw_rbr_2013) + 1):(length(list_raw_rbr_2013) + 
                length(list_raw_finals_2013))] <- list_raw_finals_2013
names(list_raw_rbr_2013) <- c(paste("R", 1:23, sep = ""), "QF", "QF", "EF", "EF", "SF", "SF", "PF", "PF", "GF")

round_details <- function(df) {
        WinLoss <- rep(NA, nrow(df))
        Margin <- rep(NA, nrow(df))
        Opposition <- rep(NA, nrow(df))
        Season <- rep(NA, nrow(df))
        Date <- rep(as.Date(NA), nrow(df))
        Venue <- rep(NA, nrow(df))
        for (i in 1:as.integer(nrow(df) / 2)) {
                Date[2 * i - 1] <- as.Date(df$Remarks[2 * i - 1], format = "%a %d-%b-%Y")
                Date[2 * i] <- as.Date(df$Remarks[2 * i - 1], format = "%a %d-%b-%Y")
                Season[2 * i - 1] <- format(Date[2 * i - 1], "%Y")
                Season[2 * i] <- format(Date[2 * i], "%Y")
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
        data.frame(Season, Date, WinLoss, Margin, Opposition, Venue)
}

## Now create a list of dataframes, each dataframe is a Round or Final, in the desired format.
rbr_2015_list <- lapply(list_raw_rbr_2015, function(df) {
        r_res <- df %>% select(V1, V3, V4) %>% slice(1:18) %>% filter(!is.na(V4))
        names(r_res) <- c("Team", "Score", "Remarks")
        r_res$Score <- as.integer(r_res$Score)
        r_res <- bind_cols(r_res, round_details(r_res))
        r_res
})

rbr_2014_list <- lapply(list_raw_rbr_2014, function(df) {
        r_res <- df %>% select(V1, V3, V4) %>% slice(1:18) %>% filter(!is.na(V4))
        names(r_res) <- c("Team", "Score", "Remarks")
        r_res$Score <- as.integer(r_res$Score)
        r_res <- bind_cols(r_res, round_details(r_res))
        r_res
})

rbr_2013_list <- lapply(list_raw_rbr_2013, function(df) {
        r_res <- df %>% select(V1, V3, V4) %>% slice(1:18) %>% filter(!is.na(V4))
        names(r_res) <- c("Team", "Score", "Remarks")
        r_res$Score <- as.integer(r_res$Score)
        r_res <- bind_cols(r_res, round_details(r_res))
        r_res
})

## Now combine the list of dataframes into 1 larger one.
rbr_2015 <- ldply(rbr_2015_list)
colnames(rbr_2015)[1] <- "Round"

rbr_2014 <- ldply(rbr_2014_list)
colnames(rbr_2014)[1] <- "Round"

rbr_2013 <- ldply(rbr_2013_list)
colnames(rbr_2013)[1] <- "Round"

#################
##
##  Section 4
##
##  Combine Game by Game with Round by Round & Combine all 3 years & Write CSV file.
##
#################


all_stats_2015 <- left_join(gbg_stats_2015, rbr_2015, by = c("Team", "Round"))
all_stats_2014 <- left_join(gbg_stats_2014, rbr_2014, by = c("Team", "Round"))
all_stats_2013 <- left_join(gbg_stats_2013, rbr_2013, by = c("Team", "Round"))

all_stats_2013_2015 <- all_stats_2015 %>% bind_rows(all_stats_2014) %>% bind_rows(all_stats_2013)

# Mar 2016, adjusted for players sharing names: Thompson, Kennedy, Lynch

all_stats_2013_2015$Player[all_stats_2013_2015$Player == "Thompson, Scott" & all_stats_2013_2015$Team == "Adelaide"] <- "Thompson, Scott"
all_stats_2013_2015$Player[all_stats_2013_2015$Player == "Thompson, Scott" & all_stats_2013_2015$Team == "North Melbourne"] <- "Thompson, Scott D"
all_stats_2013_2015$Player[all_stats_2013_2015$Player == "Lynch, Tom" & all_stats_2013_2015$Team == "Adelaide"] <- "Lynch, Tom T"
all_stats_2013_2015$Player[all_stats_2013_2015$Player == "Lynch, Tom" & all_stats_2013_2015$Team == "Gold Coast"] <- "Lynch, Tom J"
all_stats_2013_2015$Player[all_stats_2013_2015$Player == "Kennedy, Josh" & all_stats_2013_2015$Team == "West Coast"] <- "Kennedy, Josh J"
all_stats_2013_2015$Player[all_stats_2013_2015$Player == "Kennedy, Josh" & all_stats_2013_2015$Team == "Sydney"] <- "Kennedy, Josh P"

write.csv(x = all_stats_2013_2015, file = "./all_stats_2013_2015.csv", sep = "|")