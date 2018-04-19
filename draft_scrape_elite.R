#install.packages(c("dplyr", "tidyr", "readr", "data.table", "stringr", "rvest", "stringi", "XML", "plyr", "selectr"))
options(scipen=999)
theme_set(theme_bw())
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)
library(rvest)
library(stringi)
library(XML)
library(plyr)
library(selectr)
library(ggplot2)
library(plotly)
library(ggpmisc)


draftAnalysis <-data.table()
draftDOB <- data.table()
draftYearStats <- data.table()
dobtable <- data.table()

for(season in c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)){
  url = paste(c("http://www.eliteprospects.com/draft.php?year=", season, "&sort=nation&nation=&dteam=&Leaguename=NHL"), collapse = "")
  draft_year <- read_html(url)
  draft_table <- html_nodes(draft_year, '.tableborder')
  player_id <- html_attr(html_nodes(draft_table, "a"), "href")
  
  
  #td <- xmlRoot(xmlParse(draft_table))
  
  
  draft <- html_table(draft_table[[1]])
  draft <- draft[-(1:4), ]
  names(draft) <- c("pick", "Team", "Player", "Pro_Seasons", "GP", "G", "A", "PTS", "PIM")
  draft$Player <- stri_replace_all_fixed(draft$Player, "-", "_")
  draft$Player <- stri_replace_all_fixed(draft$Player, " (", "-")
  draft$Player <- stri_replace_all_fixed(draft$Player, ")", "")
  draft <- separate(draft, Player, c("Player", "POS"), sep="-", remove=TRUE)
  draft$Player <- stri_replace_all_fixed(draft$Player, "_", "-")
  draft$pick <- stri_replace_all_fixed(draft$pick, "#", "")
  draft <- filter(draft, pick >= 1)
  draft <- filter(draft, Player != "No selection was made")
  draft$year <- season
  draft$Player_ID <- player_id
  draft$Player_ID <- stri_replace_all_fixed(draft$Player_ID, "player.php?player=", "")
  draft$Player_DOB <- ""
  draftAnalysis <- rbind(draftAnalysis, draft)
  print(season)
} 
#Remove Goaltenders from the Analysis
draftAnalysis <- filter(draftAnalysis, draftAnalysis$POS != "G")

for (i in 1:nrow(draftAnalysis)){
  playerID <- draftAnalysis[i, "Player_ID"]
  playerPick <- draftAnalysis[i, "pick"]
  playerTeam <- draftAnalysis[i, "Team"]
  playerPlayer <- draftAnalysis[i, "Player"]
  playerPOS <- draftAnalysis[i, "POS"]
  playerProSeasons <- draftAnalysis[i, "Pro_Seasons"]
  playerGP <- draftAnalysis[i, "GP"]
  playerG <- draftAnalysis[i, "G"]
  playerA <- draftAnalysis[i, "A"]
  playerPTS <- draftAnalysis[i, "PTS"]
  playerPIM <- draftAnalysis[i, "PIM"]
  playerYear <- draftAnalysis[i, "year"]
  birthUrl = paste(c("http://www.eliteprospects.com/player.php?player=", playerID), collapse = "")
  player_dob <- read_html(birthUrl)
  dob <- html_nodes(player_dob, "td:nth-child(1) tr:nth-child(1) td:nth-child(2) a")
  dob <- stri_replace_all_fixed(dob[2], "<a href=\"birthdate.php?Birthdate=", "")
  dob <- sub("(.*?)&amp.*", "\\1", dob)
  dob <- sub("(.*?)-.*", "\\1", dob)
  height.d <- html_nodes(player_dob, "tr:nth-child(4) td:nth-child(2)")
  height.d <- stri_replace_all_fixed(height.d[4], "<td>", "")
  height.d <- stri_replace_all_fixed(height.d, "</td>", "")
  height.d <- gsub('\"', "", height.d, fixed = TRUE)
  height.cm <- substr(height.d, 1,6)
  height.in <- str_sub(height.d, -4)
  height.in <- trimws(height.in)
  
  #draft year stats
  draftSeasonM1 <- playerYear - 1
  toYear <- stri_sub(playerYear, -2, -1)
  dyFilter <- paste(c(draftSeasonM1, "-", toYear), collapse = "")
  htmlTB <- html_nodes(player_dob, ".tableborder")
  stats <- html_table(htmlTB[1])
  statsTB <- as.data.table(stats)
  statsTB <- select(statsTB, num_range("X", 1:9))
  names(statsTB) <- c("Season", "Team", "League", "GP", "G", "A", "TP", "PIM", "PlusMinus")
  statsTB <- statsTB[-(1:1), ]
  
  #Clean statsTB 
  for (x in 1:nrow(statsTB)) {
    rSeason <- statsTB[x, "Season"]
    Slength <- nchar(rSeason)
    if (Slength <1) {
      statsTB[x, "Season"] <- lastValidSeason
    } else {
      lastValidSeason <- rSeason
    }
  }
  
  #Delete Non Draft Year Stats
  
  statsTB <- filter(statsTB, statsTB$Season == dyFilter)
  
  if (nrow(statsTB) > 0) {
    statsTB <- mutate(statsTB, PlayerID = playerID)
    statsTB <- data.table(statsTB)
    
    #Append statsTB (This players Draft Year Stats) to the draftYearStats table
    draftYearStats <- rbind(draftYearStats, statsTB)
  }
  
  #city of birth
  cob <- html_nodes(player_dob, "td:nth-child(1) tr:nth-child(1) td~ td+ td a")
  cob <- stri_replace_all_fixed(cob, "<a href=\"birthplace.php?birthplace=", "")
  cob <- stri_replace_all_fixed(cob, "%20", " ")
  cob <- sub('.*\\\">', '', cob)
  cob <- sub("(.*?),.*", "\\1", cob)
  if(length(cob)==0){
    cob = "blank"
  }
  cobtable <- data.table(cob)
  dobTB <- data.table(dob)
  
  dobtable <- cbind(cobtable, dobTB)
  
  dobtable$playerid <- playerID
  dobtable$Pick <- playerPick
  dobtable$Team <- playerTeam
  dobtable$Player <- playerPlayer
  dobtable$POS <- playerPOS
  dobtable$Pro.Seasons <- playerProSeasons
  dobtable$GP <- playerGP
  dobtable$G <- playerG
  dobtable$A <- playerA
  dobtable$PTS <- playerPTS
  dobtable$PIM <- playerPIM
  dobtable$Year <- playerYear
  dobtable$height.in <- height.in
  dobtable$height.cm <- height.cm
  draftDOB <- rbind(draftDOB, dobtable)
  print(paste(c("Record ", i, " of ", nrow(draftAnalysis)), collapse = ""))
}

#Combine Draft Order to Draft Year Stats
draftDOB$height.in <- sapply(strsplit(as.character(draftDOB$height.in),"'|\""),
                                 function(x){ifelse(nchar(x[2])>1, paste0(as.numeric(x[1]), ".", as.numeric(x[2])), paste0(as.numeric(x[1]), ".", 0,as.numeric(x[2])))})
draftAnalysis <- draftDOB
names(draftAnalysis) <- c("cob","dob","PlayerID","Pick","NHL.Team","Player","POS","Pro.Seasons","Pro.Games","Pro.G","Pro.A","Pro.Pts","Pro.PIMS","Draft.Year", "Height.IN", "Height.CM")
draft.combined <- merge(x=draftYearStats, y=draftAnalysis, by="PlayerID", all.x = TRUE)
draft.combined$pos.clean <- ifelse(substr(draft.combined$POS, 1, 1)=="D", "D", "F")
draft.combined$jr.ptspg <- as.numeric(draft.combined$TP)/as.numeric(draft.combined$GP)
draft.combined$pro.ptspg <- as.numeric(draft.combined$Pro.Pts)/as.numeric(draft.combined$Pro.Games)


#Clean Draft Data Table
draft.combined[is.na(draft.combined)] <- 0
draft.combined$Pro.Games <- sub("^$", 0, draft.combined$Pro.Games)
draft.combined$Pro.Seasons <- sub("^$", 0, draft.combined$Pro.Seasons)
draft.combined$Pro.G <- sub("^$", 0, draft.combined$Pro.G)
draft.combined$Pro.A <- sub("^$", 0, draft.combined$Pro.A)
draft.combined$Pro.Pts <- sub("^$", 0, draft.combined$Pro.Pts)
draft.combined$Pro.PIMS <- sub("^$", 0, draft.combined$Pro.PIMS)
draft.combined$Pro.Games <- as.numeric(draft.combined$Pro.Games)
draft.combined$Pro.Seasons <- as.numeric(draft.combined$Pro.Seasons)
draft.combined$Pro.G <- as.numeric(draft.combined$Pro.G)
draft.combined$Pro.A <- as.numeric(draft.combined$Pro.A)
draft.combined$Pro.Pts <- as.numeric(draft.combined$Pro.Pts)
draft.combined$Pro.PIMS <- as.numeric(draft.combined$Pro.PIMS)
draft.combined$GP <- as.numeric(draft.combined$GP)
draft.combined$G <- as.numeric(draft.combined$G)
draft.combined$A <- as.numeric(draft.combined$A)
draft.combined$TP <- as.numeric(draft.combined$TP)
draft.combined$PIM <- as.numeric(draft.combined$PIM)

fwrite(draft.combined, "data/draftCombined.csv")
fwrite(draftDOB, "data/draft_data.csv")
fwrite(draftYearStats, "data/draftStats.csv")
