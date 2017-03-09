library(rvest)
library(stringr)

football <- lapply(paste0('http://futmobile.net/q?page=', 1:228),
                   function(url){
                     url %>% read_html() %>% 
                       html_nodes("table") %>% 
                       html_table()
                   })

new <- NULL

for(i in 1:length(football)){
  
  new2 <- (data.frame(football[i]))
  new <- rbind(new, new2)
  
}

write.csv(new, "futmobile.csv")

new <- read.csv("futmobile.csv")

new$Player...Card <- gsub("[[:space:]]", "", new$Player...Card)
new$Rating <- gsub("[^[:digit:]]","",new$Player...Card)
new$Position <- gsub("[^[:alpha:]]","",new$Player...Card)


############ Player Details #############

player <- data.frame(new$Player...Card.3)
player$new.Player...Card.3 <- as.character(player$new.Player...Card.3)


player$name <- gsub("\r?\n|\r", ",", player$new.Player...Card.3)

loc <- str_locate(player$name, ",")
player$fullname <- str_sub(player$name, start = 1 , end = loc[,"end"]-1)
player$fullname <- trimws(player$fullname)
player$name <- str_sub(player$name, start = loc[,"start"]+1 , end = length(player$name))


loc <- str_locate(player$name, ",")
player$Category <- str_sub(player$name, start = 1 , end = loc[,"end"]-1)
player$name <- str_sub(player$name, start = loc[,"start"]+3 , end = length(player$name))

loc <- str_locate(player$name, ",")
player$name <- str_sub(player$name, start = loc[,"start"]+1 , end = length(player$name))

loc <- str_locate(player$name, ",")
player$Country <- str_sub(player$name, start = 1 , end = loc[,"end"]-1)
player$name <- str_sub(player$name, start = loc[,"start"]+1 , end = length(player$name))

loc <- str_locate(player$name, ",")
player$name <- str_sub(player$name, start = loc[,"start"]+1 , end = length(player$name))

loc <- str_locate(player$name, ",")
player$League <- str_sub(player$name, start = 1 , end = loc[,"end"]-1)
player$name <- str_sub(player$name, start = loc[,"start"]+1 , end = length(player$name))

loc <- str_locate(player$name, ",")
player$name <- str_sub(player$name, start = loc[,"start"]+1 , end = length(player$name))
names(player)[2] <- "Club"


player$Position <- new$Position
player$Rating <- new$Rating
player$Stats <- new$Stats

player$Category <- trimws(player$Category)
player$Club <- trimws(player$Club)
player$Country <- trimws(player$Country)
player$League <- trimws(player$League)

player$Category[player$Category == ""] <- "NA"

player$Country[player$Category == "NA"] <- "NA"
player$League[player$Category == "NA"] <- "NA"
player$Club[player$Category == "NA"] <- "NA"


####### Base players treatment ####################################################

baseplayers <- subset(player, player$Category == "NA")
specialplayers <- subset(player, player$Category != "NA")
baseplayers$name <- gsub("\r?\n|\r", ",", baseplayers$new.Player...Card.3)

loc <- str_locate(baseplayers$name, ",")
baseplayers$name <- str_sub(baseplayers$name, start = loc[,"start"]+1 , end = length(baseplayers$name))

loc <- str_locate(baseplayers$name, ",")
baseplayers$name <- str_sub(baseplayers$name, start = loc[,"start"]+1 , end = length(baseplayers$name))
baseplayers$name <- trimws(baseplayers$name)
baseplayers$Country <- str_sub(baseplayers$name, start = 1 , end = loc[,"end"])
baseplayers$Country <- trimws(baseplayers$Country)
baseplayers$Country <- substr(baseplayers$Country, 1, nchar(baseplayers$Country)-1)

loc <- str_locate(baseplayers$name, ",")
baseplayers$name <- str_sub(baseplayers$name, start = loc[,"start"]+1 , end = length(baseplayers$name))
loc <- str_locate(baseplayers$name, ",")
baseplayers$name <- str_sub(baseplayers$name, start = loc[,"start"]+1 , end = length(baseplayers$name))
baseplayers$name <- trimws(baseplayers$name)
baseplayers$League <- str_sub(baseplayers$name, start = 1 , end = loc[,"end"]-1)
baseplayers$League <- trimws(baseplayers$League)
baseplayers$League <- substr(baseplayers$League, 1, nchar(baseplayers$League)-1)

loc <- str_locate(baseplayers$name, ",")
baseplayers$name <- str_sub(baseplayers$name, start = loc[,"start"]+1 , end = length(baseplayers$name))
loc <- str_locate(baseplayers$name, ",")
baseplayers$name <- str_sub(baseplayers$name, start = loc[,"start"]+1 , end = length(baseplayers$name))
baseplayers$name <- trimws(baseplayers$name)

baseplayers$Club <- NULL
names(baseplayers)[9] <- "Club"

baseplayers$Category <- "Basic"

player <- rbind(baseplayers, specialplayers)
player$new.Player...Card.3 <- NULL

write.csv(player, "curated_futmobile.csv")

################## Stats Extraction ############

player <- read.csv("curated_futmobile.csv")

gk <- subset(player, player$Position == "GK")
outfield <- subset(player, player$Position != "GK")

stats_O <- data.frame(outfield$Stats)
stats_O$outfield.Stats <- as.character(stats_O$outfield.Stats)
stats_O$stats <- gsub("[[:space:]]", "", stats_O$outfield.Stats)
stats_O$outfield.Stats <- NULL


stats_O$SPD <- substr(stats_O$stats, 4, 6)
stats_O$SPD <- gsub("[^[:digit:]]","",stats_O$SPD)

loc <- str_locate(stats_O$stats, "ACC")
stats_O$ACC <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$ACC <- gsub("[^[:digit:]]","",stats_O$ACC)

loc <- str_locate(stats_O$stats, "STR")
stats_O$STR <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$STR <- gsub("[^[:digit:]]","",stats_O$STR)

loc <- str_locate(stats_O$stats, "FIN")
stats_O$FIN <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$FIN <- gsub("[^[:digit:]]","",stats_O$FIN)

loc <- str_locate(stats_O$stats, "CRO")
stats_O$CRO <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$CRO <- gsub("[^[:digit:]]","",stats_O$CRO)

loc <- str_locate(stats_O$stats, "MRK")
stats_O$MRK <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$MRK <- gsub("[^[:digit:]]","",stats_O$MRK)

loc <- str_locate(stats_O$stats, "TAC")
stats_O$TAC <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$TAC <- gsub("[^[:digit:]]","",stats_O$TAC)

loc <- str_locate(stats_O$stats, "GKP")
stats_O$GKP <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$GKP <- gsub("[^[:digit:]]","",stats_O$GKP)

loc <- str_locate(stats_O$stats, "SHO")
stats_O$SHO <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$SHO <- gsub("[^[:digit:]]","",stats_O$SHO)

loc <- str_locate(stats_O$stats, "HEA")
stats_O$HEA <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$HEA <- gsub("[^[:digit:]]","",stats_O$HEA)

loc <- str_locate(stats_O$stats, "BAC")
stats_O$BAC <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$BAC <- gsub("[^[:digit:]]","",stats_O$BAC)

loc <- str_locate(stats_O$stats, "DRI")
stats_O$DRI <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$DRI <- gsub("[^[:digit:]]","",stats_O$DRI)

loc <- str_locate(stats_O$stats, "COM")
stats_O$COM <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$COM <- gsub("[^[:digit:]]","",stats_O$COM)

loc <- str_locate(stats_O$stats, "SPA")
stats_O$SPA <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$SPA <- gsub("[^[:digit:]]","",stats_O$SPA)

loc <- str_locate(stats_O$stats, "LPA")
stats_O$LPA <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$LPA <- gsub("[^[:digit:]]","",stats_O$LPA)

loc <- str_locate(stats_O$stats, "LSA")
stats_O$LSA <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$LSA <- gsub("[^[:digit:]]","",stats_O$LSA)

loc <- str_locate(stats_O$stats, "FRK")
stats_O$FRK <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$FRK <- gsub("[^[:digit:]]","",stats_O$FRK)

loc <- str_locate(stats_O$stats, "POS")
stats_O$POS <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$POS <- gsub("[^[:digit:]]","",stats_O$POS)

loc <- str_locate(stats_O$stats, "AGG")
stats_O$AGG <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$AGG <- gsub("[^[:digit:]]","",stats_O$AGG)

loc <- str_locate(stats_O$stats, "AWR")
stats_O$AWR <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$AWR <- gsub("[^[:digit:]]","",stats_O$AWR)

loc <- str_locate(stats_O$stats, "REA")
stats_O$REA <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$REA <- gsub("[^[:digit:]]","",stats_O$REA)

loc <- str_locate(stats_O$stats, "STA")
stats_O$STA <- str_sub(stats_O$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_O$STA <- gsub("[^[:digit:]]","",stats_O$STA)
stats_O$stats <- NULL


stats_G <- data.frame(gk$Stats)
stats_G$gk.Stats <- as.character(stats_G$gk.Stats)
stats_G$stats <- gsub("[[:space:]]", "", stats_G$gk.Stats)
stats_G$gk.Stats <- NULL


loc <- str_locate(stats_G$stats, "DIV")
stats_G$DIV <- str_sub(stats_G$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_G$DIV <- gsub("[^[:digit:]]","",stats_G$DIV)

loc <- str_locate(stats_G$stats, "REF")
stats_G$REF <- str_sub(stats_G$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_G$REF <- gsub("[^[:digit:]]","",stats_G$REF)

loc <- str_locate(stats_G$stats, "GKP")
stats_G$GKP <- str_sub(stats_G$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_G$GKP <- gsub("[^[:digit:]]","",stats_G$GKP)

loc <- str_locate(stats_G$stats, "HAN")
stats_G$HAN <- str_sub(stats_G$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_G$HAN <- gsub("[^[:digit:]]","",stats_G$HAN)

loc <- str_locate(stats_G$stats, "POS")
stats_G$POS <- str_sub(stats_G$stats, start = loc[,"end"] + 1, end = loc[,"end"]+3)
stats_G$POS <- gsub("[^[:digit:]]","",stats_G$POS)
stats_G$stats <- NULL

#### Add all data and create outfield and GK data ####

outfield <- cbind(outfield, stats_O)
gk <- cbind(gk, stats_G)

outfield$Stats <- NULL
outfield$stats <- NULL
gk$Stats <- NULL
gk$stats <- NULL

write_csv( gk, "goalkeepers.csv")
write.csv(outfield, "outfielders.csv")
