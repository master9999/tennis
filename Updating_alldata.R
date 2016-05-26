rm(list=ls())

require(RODBC)


olddata <- read.csv("ATP\\dataset_20160512.csv",header=TRUE, sep = ",")


olddata <- olddata[with(olddata,order(-as.numeric(olddata$DATE_G))),]

last_date <- as.Date(olddata$DATE_G[1],"%Y-%m-%d")

#last_date <- as.Date('2016-04-01',"%Y-%m-%d")

mdbConnect<-odbcConnectAccess("OnCourt")


############ Getting ID's of TOP 100 players ###########


top50 <- sqlQuery(mdbConnect,"SELECT TOP 50 ID_P_R
FROM ratings_atp ra 

ORDER BY ra.DATE_R DESC, ra.POS_R ", as.is=TRUE)

total_data <- as.data.frame(matrix(ncol=65, nrow=0))




####### Function for updating dataset #######



for (i in 1:50) { 

####### Function for creating dataset #######

#loading data for player 1092

query_text_win <- sprintf("SELECT TOP 50 g.*,o.*,c.ID_C
FROM games_atp g, odds_atp o, tours_atp t, courts c
where g.ID_T_G = o.ID_T_O
AND g.ID_R_G = o.ID_R_o
AND o.ID_B_O = 1
AND g.ID1_G = %s
AND o.ID1_O = %s
AND g.ID_T_G = t.ID_T
AND t.ID_C_T = c.ID_C
ORDER BY DATE_G DESC ",top50[i,],top50[i,])


combined_data_win <- sqlQuery(mdbConnect, query_text_win, as.is=TRUE)



query_text_loss <- sprintf("SELECT TOP 50 g.*, o.*, c.ID_C
FROM games_atp g, odds_atp o, tours_atp t, courts c
where g.ID_T_G = o.ID_T_O
AND g.ID_R_G = o.ID_R_o
AND o.ID_B_O = 1
AND g.ID2_G = %s
AND o.ID2_O = %s
AND g.ID_T_G = t.ID_T
AND t.ID_C_T = c.ID_C
ORDER BY DATE_G DESC",top50[i,],top50[i,])


combined_data_loss <- sqlQuery(mdbConnect, query_text_loss, as.is=TRUE)

alldata <- rbind(combined_data_win,combined_data_loss)

alldata <- subset(alldata,alldata$DATE_G > last_date)


continue <- FALSE

if (nrow(alldata)>0) {continue <- TRUE} #only continue with loop if there has been retrieved any data


if (continue) {
alldata$win <- ifelse(alldata$ID1_G == top50[i,],1,0)
alldata$opponent <- ifelse(alldata$ID1_G == top50[i,],alldata$ID2_G,alldata$ID1_G) #creando columna con id del oponente
alldata$DATE_G <- as.Date(alldata$DATE_G, "%Y-%m-%d")
alldata$simpleodds <- ifelse(alldata$win == 1,alldata$K1/alldata$K2,alldata$K2/alldata$K1) 
alldata$player <- ifelse(alldata$ID1_G == alldata$opponent,alldata$ID2_G,alldata$ID1_G)
#boxplot(alldata$simpleodds ~ alldata$win, horizontal=TRUE, col = c("lightgreen","lightblue"), names = c("loss","win"), xlab="Odds", main="Odds of winning")	




#### Getting data for opponents ####

data_rows <- nrow(alldata)


###wins & losses & matches of opponents:

#last 30 days
alldata$matches_opponents_30_days <- rep(0,nrow(alldata))
alldata$percentage_won_30 <- rep(0,nrow(alldata))


for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
opponent_id <- alldata$opponent[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <30
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <30
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)


wins_30 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_30 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_30 <- as.numeric(wins_30) + as.numeric(losses_30)
percentage_won_30 <- as.numeric(wins_30)/as.numeric(total_matches_30)

alldata$matches_opponents_30_days[i] <- total_matches_30
alldata$percentage_won_30[i] <- percentage_won_30

}



##last 60 days
alldata$matches_opponents_60_days <- rep(0,nrow(alldata))
alldata$percentage_won_60 <- rep(0,nrow(alldata))


for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
opponent_id <- alldata$opponent[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <60
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <60
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)


wins_60 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_60 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_60 <- as.numeric(wins_60) + as.numeric(losses_60)
percentage_won_60 <- as.numeric(wins_60)/as.numeric(total_matches_60)

alldata$matches_opponents_60_days[i] <- total_matches_60
alldata$percentage_won_60[i] <- percentage_won_60

}






##last 90 days
alldata$matches_opponents_90_days <- rep(0,nrow(alldata))
alldata$percentage_won_90 <- rep(0,nrow(alldata))


for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
opponent_id <- alldata$opponent[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <90
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <90
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)


wins_90 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_90 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_90 <- as.numeric(wins_90) + as.numeric(losses_90)
percentage_won_90 <- as.numeric(wins_90)/as.numeric(total_matches_90)

alldata$matches_opponents_90_days[i] <- total_matches_90
alldata$percentage_won_90[i] <- percentage_won_90

}


##last 120 days
alldata$matches_opponents_120_days <- rep(0,nrow(alldata))
alldata$percentage_won_120 <- rep(0,nrow(alldata))


for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
opponent_id <- alldata$opponent[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <120
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <120
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)


wins_120 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_120 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_120 <- as.numeric(wins_120) + as.numeric(losses_120)
percentage_won_120 <- as.numeric(wins_120)/as.numeric(total_matches_120)

alldata$matches_opponents_120_days[i] <- total_matches_120
alldata$percentage_won_120[i] <- percentage_won_120

}



##last 180 days
alldata$matches_opponents_180_days <- rep(0,nrow(alldata))
alldata$percentage_won_180 <- rep(0,nrow(alldata))


for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
opponent_id <- alldata$opponent[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <180
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <180
and  CDATE('%s') - Date_G > 0",opponent_id,compare_date,compare_date)


wins_180 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_180 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_180 <- as.numeric(wins_180) + as.numeric(losses_180)
percentage_won_180 <- as.numeric(wins_180)/as.numeric(total_matches_180)

alldata$matches_opponents_180_days[i] <- total_matches_180
alldata$percentage_won_180[i] <- percentage_won_180

}






#### Creating variables for player ####


#matches last 30 days:

alldata$matches_player_30_days <- rep(0,nrow(alldata))
alldata$percentage_won_player_30 <- rep(0,nrow(alldata))

for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
player_id <- alldata$player[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <30
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <30
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)


wins_30 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_30 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_30 <- as.numeric(wins_30) + as.numeric(losses_30)
percentage_won_30 <- as.numeric(wins_30)/as.numeric(total_matches_30)


alldata$matches_player_30_days[i] <- total_matches_30
alldata$percentage_won_player_30[i] <- percentage_won_30
}




#matches last 60 days:
alldata$matches_player_60_days <- rep(0,nrow(alldata))
alldata$percentage_won_player_60 <- rep(0,nrow(alldata))

for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
player_id <- alldata$player[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <60
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <60
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)


wins_60 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_60 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_60 <- as.numeric(wins_60) + as.numeric(losses_60)
percentage_won_60 <- as.numeric(wins_60)/as.numeric(total_matches_60)


alldata$matches_player_60_days[i] <- total_matches_60
alldata$percentage_won_player_60[i] <- percentage_won_60
}




#matches last 90 days:
alldata$matches_player_90_days <- rep(0,nrow(alldata))
alldata$percentage_won_player_90 <- rep(0,nrow(alldata))

for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
player_id <- alldata$player[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <90
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <90
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)


wins_90 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_90 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_90 <- as.numeric(wins_90) + as.numeric(losses_90)
percentage_won_90 <- as.numeric(wins_90)/as.numeric(total_matches_90)


alldata$matches_player_90_days[i] <- total_matches_90
alldata$percentage_won_player_90[i] <- percentage_won_90
}




#matches last 120 days:
alldata$matches_player_120_days <- rep(0,nrow(alldata))
alldata$percentage_won_player_120 <- rep(0,nrow(alldata))

for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
player_id <- alldata$player[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <120
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <120
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)


wins_120 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_120 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_120 <- as.numeric(wins_120) + as.numeric(losses_120)
percentage_won_120 <- as.numeric(wins_120)/as.numeric(total_matches_120)


alldata$matches_player_120_days[i] <- total_matches_120
alldata$percentage_won_player_120[i] <- percentage_won_120
}




#matches last 180 days:
alldata$matches_player_180_days <- rep(0,nrow(alldata))
alldata$percentage_won_player_180 <- rep(0,nrow(alldata))

for (i in 1:data_rows) {

compare_date <- alldata$DATE_G[i]
player_id <- alldata$player[i]

myquery_wins <- sprintf("SELECT count(*)
FROM games_atp
where ID1_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <180
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)

myquery_losses <- sprintf("SELECT count(*)
FROM games_atp
where ID2_G=%s
and DATE_G is not null
and CDATE('%s') - Date_G <180
and  CDATE('%s') - Date_G > 0",player_id,compare_date,compare_date)


wins_180 <- sqlQuery(mdbConnect, myquery_wins, as.is=TRUE)
losses_180 <- sqlQuery(mdbConnect, myquery_losses, as.is=TRUE)
total_matches_180 <- as.numeric(wins_180) + as.numeric(losses_180)
percentage_won_180 <- as.numeric(wins_180)/as.numeric(total_matches_180)


alldata$matches_player_180_days[i] <- total_matches_180
alldata$percentage_won_player_180[i] <- percentage_won_180
}





#### Evolution of player matches won in last 30, 60, 90 days #######
alldata$percentage_player_won_30_vs_60 <- alldata$percentage_won_player_30/alldata$percentage_won_player_60
alldata$percentage_player_won_30_vs_90 <- alldata$percentage_won_player_30/alldata$percentage_won_player_90
alldata$percentage_player_won_60_vs_90 <- alldata$percentage_won_player_60/alldata$percentage_won_player_90



#### Evolution of opponent matches won in last 30, 60, 90 days #######
alldata$percentage_opponent_won_30_vs_60 <- alldata$percentage_won_30/alldata$percentage_won_60
alldata$percentage_opponent_won_30_vs_90 <- alldata$percentage_won_30/alldata$percentage_won_90
alldata$percentage_opponent_won_60_vs_90 <- alldata$percentage_won_60/alldata$percentage_won_90


#### Evolution of matches won in last 30, 60, 90 days player vs. opponent #######
alldata$evolution_won_30_vs_60_player_vs_opponent <- alldata$percentage_player_won_30_vs_60/alldata$percentage_opponent_won_30_vs_60
alldata$evolution_won_30_vs_90_player_vs_opponent <- alldata$percentage_player_won_30_vs_90/alldata$percentage_opponent_won_30_vs_90
alldata$evolution_won_60_vs_90_player_vs_opponent <- alldata$percentage_player_won_60_vs_90/alldata$percentage_opponent_won_60_vs_90





#### Getting number of sets from previous match #######
alldata <- alldata[order(-as.numeric(alldata$DATE_G)),]


alldata$sets_previous_match <- rep(0,nrow(alldata))


for (i in 1:data_rows) {
	
	alldata$sets_previous_match[i] <- nchar(alldata$RESULT_G[i+1])

}


#### Days since previous match #######

alldata$days_since_previous_match <- rep(0,nrow(alldata))


for (i in 1:data_rows) {
	
	alldata$days_since_previous_match[i] <- as.numeric(alldata$DATE_G[i]) - as.numeric(alldata$DATE_G[i+1])

}

total_data <- rbind(total_data,alldata)

}
}


colnames(total_data) <- colnames(olddata[,-1])

olddata <- rbind(total_data,olddata[,-1])


write.csv(olddata,'ATP\\dataset_20160520.csv')


