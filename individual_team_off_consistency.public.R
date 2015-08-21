################################
# Individual and Team Offensive Consistency 
# Produced for Saberseminar 2015
# Bill Petti
# August 2015
###############################

# load required packages
# dplyr for data manipulation
# XML for web scraping 
# RMySQL for connecting to and querying databases
# reldist for calculating Gini coefficients
# relaimpo for calculating the relative importance of variables
# ggplot2 for graphs and plotting

require(dplyr)
require(XML)
require(RMySQL)
require(reldist)
require(relaimpo) # note that calling relaimpo will mask dplyr::select
require(reshape2)
require(ggplot2)

# We are going to be calculating daily weighted runs created for individual hitters and then caalculating gini coefficients based on the daily distribution to classify a player's degree of offensive consistency. The formula for wRC is (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA. So, we need to create lookup tables for Leaguw wOBA, wOBA scale, and League R/PA for each year back to 1974. We can scrape this from FanGraphs

# Scrape FanGraphs Guts! table

fg_guts <- readHTMLTable("http://www.fangraphs.com/guts.aspx?type=cn")
guts_table <- as.data.frame(fg_guts[16])
guts_table <- guts_table[,c(1:12)]
names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa")

# the guts table will show all variables as factors, so we need to convert them to numeric

asNumeric <- function(x) as.numeric(as.character(x))
guts_table <- as.data.frame(lapply(guts_table, asNumeric))

# Now we need to pull individual player logs going back to 1974. Here I am pulling from the FanGraphs' writers database. 
# connect to database

con<-dbConnect(RMySQL::MySQL(), dbname="", username = "", password = "", host = "", port = xx)

# query for daily records, excluding players that were primarily pitchers

daily <- dbGetQuery(con, "select w.*, s.ShortName as 'Team', s.PF, p.FirstName, p.LastName from wpa_daily_batting w left join season_team s on w.TeamId = s.TeamId left join player_info p on w.PlayerId = p.PlayerId where substr(w.GameDate, 1,4) = s.Season")

dbDisconnect(con)

# We have a lot of data manipulation to do, so lets grab a sample of the data first to make sure everything will work like hope since the actual data frame has over XX million records

Sample <- daily %>% sample_n(1000)

# Let's clean up the variables, as we don't need them all

Sample <- Sample[,-c(1, 44:52)]

# Create a column that indicates what season each record is from

Sample$season <- as.numeric(substr(Sample$GameDate, 1, 4))

# Create a column for the number of unintentional walks

Sample$uBB <- Sample$BB - Sample$IBB

# We need to match in the guts data so that we can accurate calculate daily wOBA and then wRC for each player

Sample_join <- left_join(Sample, guts_table, by = "season")

# Calculate daily wOBA for each record

Sample_join$daily_woba <- round((((Sample_join$wBB * Sample_join$uBB) + (Sample_join$wHBP * Sample_join$HBP) + (Sample_join$w1B * Sample_join$`1B`) + (Sample_join$w2B * Sample_join$`2B`) + (Sample_join$w3B * Sample_join$`3B`) + (Sample_join$wHR * Sample_join$HR))/Sample_join$PA),3)

# now we have to calculate wRC for each daily record. Again, here is the formala for wRC: (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA

Sample_join$wRC <- round(((((Sample_join$daily_woba - Sample_join$lg_woba) / Sample_join$woba_scale) + (Sample_join$lg_r_pa)) * Sample_join$PA),3)

# generate total wRC per player, per year on the sample data

wrc_player_yr <- Sample_join %>% group_by(PlayerId, season) %>% summarise(count = n(), wRC = sum(wRC))

## if everything checks out, apply the prior code to the daily data frame

# Let's clean up the variables, as we don't need them all

daily2 <- daily[,-c(1, 44:52)]

# Create a column that indicates what season each record is from

daily2$season <- as.numeric(substr(daily2$GameDate, 1, 4))

# Create a column for the number of unintentional walks

daily2$uBB <- daily2$BB - daily2$IBB

# We need to match in the guts data so that we can accurate calculate daily wOBA and then wRC for each player

daily2_join <- left_join(daily2, guts_table, by = "season")

# Calculate daily wOBA for each record

daily2_join$daily_woba <- round((((daily2_join$wBB * daily2_join$uBB) + (daily2_join$wHBP * daily2_join$HBP) + (daily2_join$w1B * daily2_join$`1B`) + (daily2_join$w2B * daily2_join$`2B`) + (daily2_join$w3B * daily2_join$`3B`) + (daily2_join$wHR * daily2_join$HR))/daily2_join$PA),3)

# now we have to calculate wRC for each daily record. Again, here is the formala for wRC: (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA

daily2_join$wRC <- round(((((daily2_join$daily_woba - daily2_join$lg_woba) / daily2_join$woba_scale) + (daily2_join$lg_r_pa)) * daily2_join$PA),3)

# Concatenate the player name, and move it to the first column in the data set using dplyr::select

daily2_join$Name <- paste(daily2_join$FirstName, daily2_join$LastName, sep = " ")
daily2_join <- daily2_join %>% select(Name, everything())

# generate total wRC per player, per year on the sample data

wrc_player_yr <- daily2_join %>% select(PlayerId, Name, season, G, PA, wRC) %>% na.omit() %>% group_by(PlayerId, Name, season) %>% summarise(Games = sum(G), PA = sum(PA), wRC = sum(wRC))

# generate total wRC per team, per year

wrc_team_yr <- daily2_join %>% select(TeamId, Team, season, wRC) %>% na.omit() %>% group_by(TeamId, Team, season) %>% summarise(count = n(), wRC = sum(wRC))

### Calculate Gini coefficients by player, by year to serve as consistency scores (wRC_CON), where lower scores indicate more even distribution of wRC on a game by game basis. Merge player names into consistency scores from our daily2_join data set

# Gini coefficients do not work well with negative values, so we need to correct the wRC values so they are not negative. To do this, we can simply add the difference between the lowest value and 0 to each individual wRC.

constant <- 0 - min(daily2_join$wRC, na.rm = TRUE)
daily2_join$wRC_adj <- daily2_join$wRC + constant

wrc_player_yr <- daily2_join %>% select(PlayerId, season, wRC_adj) %>% na.omit() %>% aggregate(wRC_adj ~ PlayerId + season, data = ., FUN = "gini") %>% left_join(wrc_player_yr, ., by = c("PlayerId" = "PlayerId", "season" = "season"))

names(wrc_player_yr)[7] <- "wRC_CON"

wrc_player_yr$wRC_CON <- round(wrc_player_yr$wRC_CON, 3)

# group each player season by PAs per game to compare how consistency varies by playing time

wrc_player_yr$PA_G <- round((wrc_player_yr$PA / wrc_player_yr$G),1) 
wrc_player_yr$PA_grp <- ifelse(wrc_player_yr$PA_G <=1,1,ifelse(wrc_player_yr$PA_G >1 & wrc_player_yr$PA_G <= 1.5, 1.5, ifelse(wrc_player_yr$PA_G >1.5 & wrc_player_yr$PA_G <=2,2, ifelse(wrc_player_yr$PA_G > 2 & wrc_player_yr$PA_G <= 2.5, 2.5, ifelse(wrc_player_yr$PA_G > 2.5 & wrc_player_yr$PA_G <= 3, 3, ifelse(wrc_player_yr$PA_G > 3 & wrc_player_yr$PA_G <= 3.5, 3.5, ifelse(wrc_player_yr$PA_G > 3.5 & wrc_player_yr$PA_G <= 4, 4, ifelse(wrc_player_yr$PA_G > 4 & wrc_player_yr$PA_G <= 4.5, 4.5, 5))))))))

# now group by PA_grp and run correlations between PA_G and wRC_CON

cor_PA_G <- wrc_player_yr %>% filter(PA_grp > 1) %>% group_by(PA_grp) %>% summarise(Correlation = cor(PA_G, wRC_CON))

cor_PA_G <- wrc_player_yr %>% filter(PA_grp > 1) %>% group_by(PA_grp) %>% summarise(Count = n()) %>% left_join(cor_PA_G, by = "PA_grp")

# correlate for players with a min number of 100 games played

cor_PA_G_m100 <- wrc_player_yr %>% filter(PA_grp > 1, Games > 99) %>% group_by(PA_grp) %>% summarise(Correlation = cor(PA_G, wRC_CON))

cor_PA_G_m100 <- wrc_player_yr %>% filter(PA_grp > 1, Games > 99) %>% group_by(PA_grp) %>% summarise(Count = n()) %>% left_join(cor_PA_G_m100, by = "PA_grp")

# correlate w/ wRC

cor_wRC_m100 <- wrc_player_yr %>% filter(PA_grp > 2, Games > 99) %>% group_by(PA_grp) %>% summarise(Correlation = cor(wRC, wRC_CON))

cor_wRC_m100 <- wrc_player_yr %>% filter(PA_grp > 2, Games > 99) %>% group_by(PA_grp) %>% summarise(Count = n()) %>% left_join(cor_wRC_m100, by = "PA_grp")

# create a data set with just players with >= 100 games and >=3 PA per Game

wrc_player_yr_m100 <- filter(wrc_player_yr, PA_grp > 2, Games > 99)

# since the average consistency of a hitter shifts a little each year, we should create a normalized metric to make it easier to compare hitters in different run environments over time. For this, we will use z-scores. Z-scores are calculated by taking (score-pop mean) / pop std

z_scores_year <- wrc_player_yr %>% group_by(season) %>% summarise(dat_sd = sd(wRC_CON), dat_mean = mean(wRC_CON)) %>% left_join(wrc_player_yr, ., by = "season")

z_scores_year$zscore <- (z_scores_year$wRC_CON - z_scores_year$dat_mean) / z_scores_year$dat_sd

# we can also restrict to everyday players, using 100 games played as a cut off

z_scores_year_everyday <- wrc_player_yr_m100 %>% group_by(season) %>% summarise(dat_sd = sd(wRC_CON), dat_mean = mean(wRC_CON)) %>% left_join(wrc_player_yr_m100, ., by = "season")

z_scores_year_everyday$zscore_everyday <- (z_scores_year_everyday$wRC_CON - z_scores_year_everyday$dat_mean) / z_scores_year_everyday$dat_sd

# join the two separate z_scores

z_scores_year_all <- left_join(z_scores_year, z_scores_year_everyday[,c(1,3,10:12)], by = c("PlayerId" = "PlayerId", "season" = "season"))


# plot average consistency by year 

plot_CON_yr <- ggplot(z_scores_year, aes(x=season,y=dat_mean)) + geom_point(size = 6, colour="grey50") + geom_point(size = 4, colour="firebrick") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Year") + ylab("More Consistency                                                                                                                                                   Less Consistent") + ggtitle ("Average League-wide Individual Consistency") + stat_smooth() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

plot_CON_yr

ggsave("lg_con_year.png", plot_CON_yr, width = 8.77, height = 5.05, units = "in")

# plot average consistency by year 

plot_CON_yr_everyday <- ggplot(z_scores_year_everyday, aes(x=season,y=dat_mean)) + geom_point(size = 6, colour="grey50") + geom_point(size = 4, colour="firebrick") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Year") + ylab("More Consistency                                                                                                                                                   Less Consistent") + ggtitle ("Average League-wide Individual Consistency: Min 100 Games") + stat_smooth() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

plot_CON_yr_everyday

ggsave("lg_con_year_min100.png", plot_CON_yr_everyday, width = 8.77, height = 5.05, units = "in")

# join and co-plot the two different sets of players

z_join <- left_join(z_scores_year[,c(1,3)], z_scores_year_everyday[,c(1,3)], by = "season")
names(z_join) <- c("season", "All", "Min 100 G")
z_join_melt <- melt(z_join , id.vars="season")

plot_CON_yr_compare <- ggplot(z_join_melt, aes(x=season, y=value, colour=factor(variable))) + geom_point(size=6) + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Year") + ylab("More Consistency                                                                                                                                                   Less Consistent") + ggtitle ("League-wide Individual Consistency Over Time") + stat_smooth() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + labs(colour="# Games Played")

plot_CON_yr_compare

ggsave("lg_con_year_comp.png", plot_CON_yr_compare, width = 10, height = 5.76, units = "in")

# plot individual data points with longitudinal smoothing

plot_CON_yr_ind <- ggplot(wrc_player_yr_m100, aes(x=season,y=wRC_CON)) + geom_point(size = 6, alpha = 0.5, colour="grey50") + geom_point(size = 4, alpha=0.25, colour="firebrick") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Year") + ylab("More Consistency                                                                                                                                          Less Consistent") + ggtitle ("Average League-wide Individual Consistency: Min 100 Games Played and 3 PAs per Game") + stat_smooth() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + ylim(.15, .35)

plot_CON_yr_ind

ggsave("lg_con_year_ind_m100.png", plot_CON_yr_ind, width = 10, height = 5.76, units = "in")

### Now we need to pull in seasonal data for each player year so we can explore relationships between consistency and hitter attributes. Start by restablishing a connection with your database

con<-dbConnect(RMySQL::MySQL(), dbname="", username = "", password = "", host = "", port = xx)

season_batting <- dbGetQuery(con, "select * from stats_batting_master_pfx where Season > 1973 and type = 0")

dbDisconnect(con)

# Let's trim down the data set

# it's easier to create a numbered variable list as a dataframe since the dataset has more than 100 variables. For this I've written a little function that gives you the values for the first observation for each variable and also numbers each variable

variable_list <- function(x) {
  v <- x[1,]
  n <- c(1:(length(v)))
  v <- t(v)
  v <- as.data.frame(cbind(v, n))
  names(v) <- c("first_obs", "variable_number")
  return(v)
}

vars <- variable_list(season_batting)

season_batting_red <- season_batting[,c(1:7, 35:51, 54:56, 63, 66:70, 125, 130:143, 209:215, 227:232)]

# join annual wRC_CON data with seasonal data

season_con_join <- left_join(z_scores_year_all, season_batting_red, by = c("PlayerId" = "playerid", "season" = "Season"))
season_con_join <- season_con_join[,-c(5,6)]
names(season_con_join)[c(18,38)] <- c("PA", "wRC")

# now, pull in all data for each player season after the current observation

# first, let's pull a sample of data to make sure it pulls correctly

Sample <- ungroup(season_con_join) %>% filter(PlayerId == "1" | PlayerId == "2" |  PlayerId == "97")

Sample$YR2 <- Sample$season + 1

Sample_join <- left_join(Sample[,c(1,3,73)], Sample[,-c(2,73)], by = c("PlayerId" = "PlayerId", "YR2" = "season"))

# now, renname the new variables

colnames(Sample_join)[-c(1:3)] <- paste("yr2", colnames(Sample_join)[-c(1:3)], sep = "_")

# join the two sets of data so that a players t and t+1 seasons are in the same case

Sample_yoy <- left_join(Sample[,-73], Sample_join, by = c("PlayerId" = "PlayerId", "season" = "season"))

# calculate the year over year change for each relevant variable

change <- Sample_yoy[,c(74:83, 86:142)] - Sample_yoy[,c(4:13, 16:72)]
colnames(change) <- gsub("yr2_", "", colnames(change), fixed=TRUE)

# clear up the names of the columns and then merge back in to the main data set

colnames(change) <- paste("change", colnames(change), sep = "_")

Sample_yoy <- cbind(Sample_yoy, change)

# once the sample code works out, apply to the larger data sets

season_con_join$YR2 <- season_con_join$season + 1

season_join <- left_join(season_con_join[,c(1,3,73)], season_con_join[,-c(2,73)], by = c("PlayerId" = "PlayerId", "YR2" = "season"))

# now, renname the new variables

colnames(season_join)[-c(1:3)] <- paste("yr2", colnames(season_join)[-c(1:3)], sep = "_")

# join the two sets of data so that a players t and t+1 seasons are in the same case

season_yoy <- left_join(season_con_join[,-73], season_join, by = c("PlayerId" = "PlayerId", "season" = "season"))

# calculate the year over year change for each relevant variable

season_change <- season_yoy[,c(74:83, 86:142)] - season_yoy[,c(4:13, 16:72)]
colnames(season_change) <- gsub("yr2_", "change_", colnames(season_change), fixed=TRUE)

# clear up the names of the columns and then merge back in to the main data set

season_yoy <- cbind(season_yoy, season_change)

### now that we have our data set up properly, we can start the analysis
### we want to do two things: 1) calculate aging curves for consistency to see how much consistency and changes in consistency is partly a function of player age, and  2) understand what other player characteristics relate to consistency
### first, let's look at aging curves

# we are going to use the delta method of the harmonic mean to calculate aging curves

# first, restrict the data to players with at least 100 games played and more than 2 plate appearances per game

aging <- filter(season_yoy, Games > 99, PA_G > 2)

# test calculating the harmonic mean with only three sets of age groups

aging_test <- filter(aging, Age > 22, Age < 26)
aging_test <- aging_test[,c(5,18,41,88,75,111,144,149,152)]
aging_test <- na.omit(aging_test)
aging_test$PA_ave <- (aging_test$PA + aging_test$yr2_PA)/2
aging_test$age_cohort <- paste(aging_test$Age, aging_test$yr2_Age, sep = "-")

# in R, the formula for calculating the harmonic mean is 1 / (mean(1/x)), where x is a vector of data. If you want a weighted harmonic mean, you just need to substitute the weighted mean function, i.e. 1 / (weighted.mean(1/x, w)) where w = the variable you are weighting by

x <- aging_test %>% group_by(age_cohort) %>% summarise(N = n(), YR1_PA = sum(PA), YR2_PA = sum(yr2_PA), YR1_wRC_CON = sum(wRC_CON), YR2_wRC_CON = sum(yr2_wRC_CON))

x$YR1w <- x$YR1_wRC_CON * (x$YR1_PA / (x$N/(1/x$YR1_PA + 1/x$YR2_PA)))
x$YR2w <- x$YR2_wRC_CON * (x$YR2_PA / (x$N/(1/x$YR1_PA + 1/x$YR2_PA)))
x$w_wRC_CON <- round((x$YR2w - x$YR1w),4)
x$cum_change <- cumsum(x$w_wRC_CON)

# now apply to the entire aging data set

aging <- aging[,c(5,18,41,88,75,111,144,149,152)]
aging <- na.omit(aging) %>% filter(yr2_Age - Age == 1)
aging$PA_ave <- (aging$PA + aging$yr2_PA)/2
aging$age_cohort <- paste(aging$Age, aging$yr2_Age, sep = "-")

aging_group <- aging %>% group_by(Age, age_cohort) %>% summarise(N = n(), YR1_PA = sum(PA), YR2_PA = sum(yr2_PA), YR1_wRC_CON = sum(wRC_CON), YR2_wRC_CON = sum(yr2_wRC_CON))

aging_group$YR1w <- aging_group$YR1_wRC_CON * (aging_group$YR1_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$YR2w <- aging_group$YR2_wRC_CON * (aging_group$YR2_PA / (aging_group$N/(1/aging_group$YR1_PA + 1/aging_group$YR2_PA)))
aging_group$w_wRC_CON <- round((aging_group$YR2w - aging_group$YR1w),4)
aging_group$cum_change <- cumsum(aging_group$w_wRC_CON)

# remove smaller sample sized cohorts and re-run cummulative change

aging_group_red <- aging_group[c(5:20),]
aging_group_red$cum_change <- cumsum(aging_group_red$w_wRC_CON)

aging_plot <- ggplot(aging_group_red, aes(x=age_cohort,y=cum_change, group=1)) + geom_point(size = 10, colour="grey50") + geom_point(size = 8, colour="firebrick") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + stat_smooth()

aging_plot

# rescale so that the max cummulative score is set to 0
aging_group_red$rescale <- aging_group_red$cum_change - max(aging_group_red$cum_change)

aging_plot_zeroscale <- ggplot(aging_group_red, aes(x=age_cohort,y=rescale, group=1)) + geom_point(size = 10, colour = "firebrick1") + theme(plot.title = element_text(size=20, face="bold", vjust=2)) + theme(axis.text = element_text(size = rel(1.05),angle = 0)) + theme(axis.title = element_text(size = rel(1.2))) + theme(axis.title.x = element_text(face="bold", vjust = -.25)) + theme(axis.title.y = element_text(face="bold", vjust = 1.75)) + theme(axis.text.y = element_text(face="bold",colour="black")) + theme(axis.text.x = element_text(face="bold",colour="black")) + xlab("Age") + ylab("Cummulative Change in Consistency") + ggtitle ("Aging Curve for Offensive Consistency") + theme(panel.grid.major = element_line(colour = "grey50", linetype = "dotted"), panel.grid.minor = element_blank()) + stat_smooth(colour = "firebrick1")

aging_plot_zeroscale

ggsave("aging_m100.png", aging_plot_zeroscale, width = 10, height = 5.76, units = "in")

### second, let's look at co-variates of consistency. We will once again restrict to our Games and PA_G minimums

correlates <- filter(season_yoy, Games > 99, PA_G > 2)
correlates <- correlates[,-c(2,15,85)]
# creat a correlation matrix and select for wRC_CON, yr2_wRC_CON, and change_wRC_CON

cor.matrix <- as.data.frame(round(cor(correlates, use = "pairwise.complete.obs", method= "spearman"), 2))
cor.matrix.vars <- as.data.frame(t(cor.matrix[1,]))
cor.matrix.vars$var_number <- c(1:206)
cor.matrix.vars$var_name <- row.names(cor.matrix.vars)
cor.matrix.reduce <- cor.matrix[,c(4,73,141)]

# top correlations for wRC_CON: GB/FB, pfxContact%, BB/K, wRC+, K%, wOBA, FB%, OPS, SLG, ISO
# others of interest BABIP, OBP, BB%

correlates <- correlates[,c(5:6, 10, 13, 17:72)]
cor.matrix.reduce <- round(cor(correlates[,c(1,3,4,7,8,10:60)], use = "pairwise.complete.obs", method= "spearman"), 2)
cor.matrix.reduce <- cor.matrix.reduce[c(1:3),]

# generate a linear model for wRC_CON

RC_lm_reduced <- lm(wRC_CON ~ wRC + G + PA_G + `GB/FB` + ISO + OBP, data = correlates)
summary(RC_lm_reduced)
relaimpo.RC <- calc.relimp(RC_lm_reduced, type = "lmg", rela = TRUE)
relaimpo.RC
relaimpo.RC_lmg <- as.data.frame(relaimpo.RC$lmg)
relaimpo.RC_lmg$vars <- row.names(relaimpo.RC_lmg)
names(relaimpo.RC_lmg) <- c("Relative Importance", "Metric")
relaimpo.RC_lmg <- arrange(relaimpo.RC_lmg, desc(`Relative Importance`))
relaimpo.RC_lmg

# plot the relative importance of each variabe

RC_lm_reduced_plot <- ggplot(relaimpo.RC_lmg, aes(x=factor(Metric), y=`Relative Importance`)) + geom_bar(stat="identity")
RC_lm_reduced_plot

### generate some leaderboards 
current_m100 <- season_yoy[,c(2:6,10,13,15:42)] %>% filter(season == 2015, Games > 80, PA_G > 2) %>% write.csv("current_leaders.csv")

past3years_m100 <- season_yoy[,c(2:6,10,13,15:42)] %>% filter(season > 2011, season < 2015, Games > 100, PA_G > 2) %>% write.csv("2012_2014_leaders.csv")







