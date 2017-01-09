#################
##
##  Section 1
##
##  Load packages and set up environment.
##
#################

setwd("~/RFolder/AFLPlayerStatsGCD")

library(XML)
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

load_gbg_stats <- function(club = "Adelaide", year = 2016) {
        ## loads game by game stats for a given club and season from
        ## http://afltables.com
        
        year <- as.character(year)
        print(club)
        print(year)
        
        ## special handling for 6 teams
        if (club == "Brisbane Lions")
                gbg_url <- paste("http://afltables.com/afl/stats/teams/brisbanel/", 
                                 year, "_gbg.html", sep = "")
        else if (club == "Greater Western Sydney")
                gbg_url <- paste("http://afltables.com/afl/stats/teams/gws/", 
                                 year, "_gbg.html", sep = "")
        else if (club == "North Melbourne")
                gbg_url <- paste("http://afltables.com/afl/stats/teams/kangaroos/", 
                                 year, "_gbg.html", sep = "")
        else if (club == "Port Adelaide")
                gbg_url <- paste("http://afltables.com/afl/stats/teams/padelaide/", 
                                 year, "_gbg.html", sep = "")
        else if (club == "Sydney") 
                gbg_url <- paste("http://afltables.com/afl/stats/teams/swans/", 
                                 year, "_gbg.html", sep = "")
        else if (club == "Western Bulldogs") 
                gbg_url <- paste("http://afltables.com/afl/stats/teams/bullldogs/", 
                                 year, "_gbg.html", sep = "")
        else gbg_url <- paste("http://afltables.com/afl/stats/teams/", 
                tolower(gsub(" ", "", club)), "/", year, "_gbg.html", sep = "")
        
        gbg_filename <- paste("./files/", gsub(" ", "", club), "_GBG_", year, ".html", sep = "")
        
        if (!dir.exists("./files")) dir.create("./files/")
        if (!file.exists(gbg_filename)) download.file(gbg_url, destfile = gbg_filename)
        
        list_raw_gbg <- readHTMLTable(gbg_filename, stringsAsFactors = FALSE)
        
        ## remove unwanted stats (ie Brownlow votes, sub on/off) 
        if (year == "2016") list_raw_gbg <- list_raw_gbg[-c(15)]
        else list_raw_gbg <- list_raw_gbg[-c(15, 24)]
        
        ## Gather each key stat and combine all into one dataframe per club
        disp <- list_raw_gbg[[1]] %>% select(-Tot) %>% gather(Round, Disposals, -Player)
        kicks <- list_raw_gbg[[2]] %>% select(-Tot) %>% gather(Round, Kicks, -Player)
        club_stats <- full_join(disp, kicks)
        marks <- list_raw_gbg[[3]] %>% select(-Tot) %>% gather(Round, Marks, -Player)
        club_stats <- full_join(club_stats, marks)
        hballs <- list_raw_gbg[[4]] %>% select(-Tot) %>% gather(Round, Handballs, -Player)
        club_stats <- full_join(club_stats, hballs)
        goals <- list_raw_gbg[[5]] %>% select(-Tot) %>% gather(Round, Goals, -Player)
        club_stats <- full_join(club_stats, goals)
        behinds <- list_raw_gbg[[6]] %>% select(-Tot) %>% gather(Round, Behinds, -Player)
        club_stats <- full_join(club_stats, behinds)
        hitouts <- list_raw_gbg[[7]] %>% select(-Tot) %>% gather(Round, Hitouts, -Player)
        club_stats <- full_join(club_stats, hitouts)
        tackles <- list_raw_gbg[[8]] %>% select(-Tot) %>% gather(Round, Tackles, -Player)
        club_stats <- full_join(club_stats, tackles)
        r50s <- list_raw_gbg[[9]] %>% select(-Tot) %>% gather(Round, Rebound50s, -Player)
        club_stats <- full_join(club_stats, r50s)
        i50s <- list_raw_gbg[[10]] %>% select(-Tot) %>% gather(Round, Inside50s, -Player)
        club_stats <- full_join(club_stats, i50s)
        clears <- list_raw_gbg[[11]] %>% select(-Tot) %>% gather(Round, Clearances, -Player)
        club_stats <- full_join(club_stats, clears)
        clangs <- list_raw_gbg[[12]] %>% select(-Tot) %>% gather(Round, Clangers, -Player)
        club_stats <- full_join(club_stats, clangs)
        frees_for <- list_raw_gbg[[13]] %>% select(-Tot) %>% gather(Round, FreesFor, -Player)
        club_stats <- full_join(club_stats, frees_for)
        frees_ag <- list_raw_gbg[[14]] %>% select(-Tot) %>% gather(Round, FreesAgainst, -Player)
        club_stats <- full_join(club_stats, frees_ag)
        con_pos <- list_raw_gbg[[15]] %>% select(-Tot) %>% gather(Round, ContendedPossessions, -Player)
        club_stats <- full_join(club_stats, con_pos)
        uncon_pos <- list_raw_gbg[[16]] %>% select(-Tot) %>% gather(Round, UncontendedPossessions, -Player)
        club_stats <- full_join(club_stats, uncon_pos)
        con_marks <- list_raw_gbg[[17]] %>% select(-Tot) %>% gather(Round, ContestedMarks, -Player)
        club_stats <- full_join(club_stats, con_marks)
        marks_i50 <- list_raw_gbg[[18]] %>% select(-Tot) %>% gather(Round, MarksInside50, -Player)
        club_stats <- full_join(club_stats, marks_i50)
        one_pers <- list_raw_gbg[[19]] %>% select(-Tot) %>% gather(Round, OnePercenters, -Player)
        club_stats <- full_join(club_stats, one_pers)
        bounces <- list_raw_gbg[[20]] %>% select(-Tot) %>% gather(Round, Bounces, -Player)
        club_stats <- full_join(club_stats, bounces)
        goal_ass <- list_raw_gbg[[21]] %>% select(-Tot) %>% gather(Round, GoalAssists, -Player)
        club_stats <- full_join(club_stats, goal_ass)
        per_plyd <- list_raw_gbg[[22]] %>% select(-Tot) %>% gather(Round, PercentPlayed, -Player)
        club_stats <- full_join(club_stats, per_plyd)
        
        for (i in 3:ncol(club_stats)) {
                club_stats[, i] <- as.numeric(club_stats[, i])
        }
        club_stats[is.na(club_stats)] <- 0
        
        club_stats <- club_stats[rowSums(club_stats[, 3:ncol(club_stats)]) > 0, ]
        club_stats$Season <- as.integer(year)
        club_stats$Team <- club
        
        return(club_stats)

}

#adel2016 <- load_gbg_stats("Adelaide")

gbg2016.df <- do.call(bind_rows, lapply(clubs, load_gbg_stats))
gbg2015.df <- do.call(bind_rows, lapply(clubs, load_gbg_stats, year = 2015))
gbg2014.df <- do.call(bind_rows, lapply(clubs, load_gbg_stats, year = 2014))
gbg2013.df <- do.call(bind_rows, lapply(clubs, load_gbg_stats, year = 2013))

#################
##
##  Section 3
##
##  Get Round by Round Score, Win/Loss, Opposition, Margin, Venue for every team.
##
#################

round_details <- function(df) {
        # A function to glean the round by round details from the rbr dataframe.
        WinLoss <- rep(NA, nrow(df))
        Margin <- rep(NA, nrow(df))
        Opposition <- rep(NA, nrow(df))
        Score <- df$Score
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
                #WinLoss <- as.character(WinLoss)
                #Opposition <- as.character(Opposition)
                #Venue <- as.character(Venue)
        }
        return(data.frame(Date, Score, Margin, WinLoss, Opposition, Venue,
                          stringsAsFactors = FALSE))
}

load_rbr_results <- function(year = 2016) {
        ## loads round by round results for a given year from
        ## http://afltables.com
        
        year <- as.character(year)
        print(year)
        
        rbr.url <- paste("http://afltables.com/afl/seas/", year, ".html", sep = "")
        rbr.filename <- paste("./files/rbr", year, ".html", sep = "")
        
        if (!file.exists(rbr.filename)) download.file(rbr.url, destfile = rbr.filename)
        
        rbr.ls <- readHTMLTable(rbr.filename, stringsAsFactors = FALSE)
        
        ## Clean rbr.ls to make it a list of dataframes. Each dataframe is a Round. 
        ## Remove all other entries in the list from rbr.ls
        
        ## First, extract just the home and away rounds, removing everything else,
        ## and convert to a single dataframe. "ha" = home & away rounds
        rounds <- lapply(rbr.ls, function(df) (dim(df)[1] > 18))
        rounds <- as.vector(rounds, mode = "logical")
        rounds[is.na(rounds)] <- FALSE
        rbrha.ls <- rbr.ls[rounds]
        names(rbrha.ls) <- paste("R", 1:length(rbrha.ls), sep = "")
        #rbrha.df <- do.call(bind_rows, lapply(rbrha.ls, data.frame, stringsAsFactors = FALSE))
        rbrha.df <- bind_rows(rbrha.ls, .id = "Round")
        
        ## Next, do the same for finals rounds.
        rbrfinals.ls <- readHTMLTable(rbr.filename, stringsAsFactors = FALSE,
                                      which = (length(rbr.ls) - 17):length(rbr.ls), header = FALSE)
        # rbrfinals.ls <- tail(rbr.ls, n = 18)
        finals <- sapply(rbrfinals.ls, function(df) (dim(df)[2] > 2))
        finals <- as.vector(finals, mode = "logical")
        finals[is.na(finals)] <- FALSE
        rbrfinals.ls <- rbrfinals.ls[finals]
        ## Finals were played in different order in different seasons, so names 
        ## is dependent on the year
        if(year != 2016) 
                names(rbrfinals.ls) <- c("QF", "QF", "EF", "EF", "SF", "SF", "PF", "PF", "GF")
        else 
                names(rbrfinals.ls) <- c("EF", "QF", "QF", "EF", "SF", "SF", "PF", "PF", "GF")
        #rbrfinals.df <- do.call(bind_rows, lapply(rbrfinals.ls, data.frame, stringsAsFactors = FALSE))
        rbrfinals.df <- bind_rows(rbrfinals.ls, .id = "Round")
        
        ## Next, bind ha and finals dataframes and extract relevant data.
        rbr.df <- bind_rows(rbrha.df, rbrfinals.df)
        
        rbr.df$Season <- year
        rbr.df <- select(rbr.df, -V2)
        names(rbr.df) <- c("Round", "Team", "Score", "Remarks", "Season")
        rbr.df$Score <- as.integer(rbr.df$Score)
        rbr.df$Season <- as.integer(rbr.df$Season)
        rbrladder.df <- filter(rbr.df, !Team %in% clubs)
        names(rbrladder.df) <- c("Round", "Team", "Points", "Percentage", "Season")
        rbr.df <- filter(rbr.df, Team %in% clubs)
        rbr.df <- filter(rbr.df, !is.na(Score))
        df1 <- round_details(rbr.df)

        rbr.df <- rbr.df %>% select(Season, Round, Team) %>% bind_cols(df1)
        
        return(rbr.df)
}

rbr2016.df <- load_rbr_results()
rbr2015.df <- load_rbr_results(2015)
rbr2014.df <- load_rbr_results(2014)
rbr2013.df <- load_rbr_results(2013)

#################
##
##  Section 4
##
##  Combine Game by Game with Round by Round & Combine all 4 years & Write CSV file.
##
#################

stats2016.df <- left_join(gbg2016.df, rbr2016.df, by = c("Team", "Round", "Season"))
stats2016.df <- select(stats2016.df, Team, Player, Season, Round, Date:Venue, 
                       Disposals:PercentPlayed)
stats2015.df <- left_join(gbg2015.df, rbr2015.df, by = c("Team", "Round", "Season"))
stats2015.df <- select(stats2015.df, Team, Player, Season, Round, Date:Venue, 
                       Disposals:PercentPlayed)
stats2014.df <- left_join(gbg2014.df, rbr2014.df, by = c("Team", "Round", "Season"))
stats2014.df <- select(stats2014.df, Team, Player, Season, Round, Date:Venue, 
                       Disposals:PercentPlayed)
stats2013.df <- left_join(gbg2013.df, rbr2013.df, by = c("Team", "Round", "Season"))
stats2013.df <- select(stats2013.df, Team, Player, Season, Round, Date:Venue, 
                       Disposals:PercentPlayed)

stats.df <- stats2016.df %>% bind_rows(stats2015.df) %>% 
        bind_rows(stats2014.df) %>% bind_rows(stats2013.df)

## Some players share the same name, so find those and distinguish them with middle initial.
find_duplicate_names <- function(year) {
        # a function for finding players sharing the same name, eg Tom Lynch etc
        names.df <- stats.df %>% filter(Season == as.integer(year)) %>% select(Player, Team) %>% distinct()
        return(names.df[duplicated(names.df$Player), 1])
}
sapply(c("2016", "2015", "2014", "2013"), find_duplicate_names)

## This returns 5 names. Further investigations reveal there are only 2 sharing each name, ie 10 people.

## Tom Lynch (Adelaide) has only played for Adelaide in 2013 - 2016.
stats.df$Player[stats.df$Player == "Lynch, Tom" & stats.df$Team == "Adelaide"] <- "Lynch, Tom T"

## Tom Lynch (Gold Coast) has only played for Gold Coast in 2013 - 2016.
stats.df$Player[stats.df$Player == "Lynch, Tom" & stats.df$Team == "Gold Coast"] <- "Lynch, Tom J"

## Scott Thompson (Adelaide) has only played for Adelaide in 2013 - 2016.
stats.df$Player[stats.df$Player == "Thompson, Scott" & stats.df$Team == "Adelaide"] <- "Thompson, Scott"

## Scott Thompson (North Melbourne) has only played for North Melbourne in 2013 - 2016.
stats.df$Player[stats.df$Player == "Thompson, Scott" & stats.df$Team == "North Melbourne"] <- "Thompson, Scott D"

## Josh Kennedy (West Coast) has only played for West Coast in 2013 - 2016.
stats.df$Player[stats.df$Player == "Kennedy, Josh" & stats.df$Team == "West Coast"] <- "Kennedy, Josh J"

## Josh Kennedy (Sydney) has only played for Sydney in 2013 - 2016.
stats.df$Player[stats.df$Player == "Kennedy, Josh" & stats.df$Team == "Sydney"] <- "Kennedy, Josh P"

## Mitch Brown (West Coast) has only played for West Coast in 2013 - 2016.
stats.df$Player[stats.df$Player == "Brown, Mitch" & stats.df$Team == "West Coast"] <- "Brown, Mitch WC"

## Mitch Brown (Essendon, 2016) also played for Geelong in 2013 and 2014 and didnt play in 2015.


write.csv(stats.df, file = "./stats.csv", row.names = FALSE)
