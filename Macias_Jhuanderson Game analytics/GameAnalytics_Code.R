library(googleVis)

library(RCurl)
library(RJSONIO)

# webpage <- paste0("http://powerful-meadow-8588.herokuapp.com/",
#          "data/12months_departures_joiners.json", sep="")
# 
# data <- fromJSON(getURL(webpage)) 

##
# save(data, file="glitch1.RData")
load(file="glitch1.RData")
##



nodes.info <- 
  do.call("rbind", 
          lapply(data$nodes, data.frame))

head(nodes.info)



months <- unique(nodes.info$month)
months

table(nodes.info$month)

# Using the aggregate function,
# compute the data frame for the total players joining each month. Name the columns as Month and Joining.
df <- aggregate(nodes.info$joining, 
                by=list(nodes.info$month), 
                FUN = sum)

colnames(df)[colnames(df)=="Group.1"] <- "Month"
colnames(df)[colnames(df)=="x"] <- "Join"

# Using the aggregate function, compute the data frame for the total players departing each month.
# Names the columns as Month and Departing.
df2 <- aggregate(nodes.info$departing, 
                by=list(nodes.info$month), 
                FUN = sum)

colnames(df2)[colnames(df2)=="Group.1"] <- "Month"
colnames(df2)[colnames(df2)=="x"] <- "depart"


#Merge the two data frames by Month column with sort option as FALSE.
total <- merge(df,df2,by=c("Month"), sort=FALSE)


#Show month-by-month comparison of the above numbers using the Google Line chart and Google Column chart. 
#Merge the two into a single chart.
Line <- gvisLineChart(total)
plot(Line)

Bar <- gvisBarChart(total)
plot(Bar)


bar.stack <- 
  gvisColumnChart(
    total,
    options=list(
      height=750, width=850,
      isStacked = TRUE,
      colors="['#2defe9','#109692']"))
plot(bar.line)

cc<- gvisComboChart(total, xvar='Month',
                    yvar=c('Join','depart'),
                    options=list(seriesType='bars',
                                 with=350, height=300, title='Join/Depart', series='{1: {type:"line"}}'))

plot(cc)
#Show the Google Gauge chart with default options for 
#the monthly departing data. Use the range from 0 to 4030.
gaugedata <- c('Month','depart')
gaugedata <- total[gaugedata]
Gauge <-  gvisGauge(gaugedata, 
                    options=list(min=0, max=4030, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)

#Show the Google Gauge chart for the monthly departing data with the green
#range 0 – 1000, yellow range 1000 – 2000, and the red range 2000 – 4030.
Gauge <-  gvisGauge(gaugedata, 
                    options=list(min=0, max=4030, greenFrom=1,
                                 greenTo=1000, yellowFrom=1000, yellowTo=2000,
                                 redFrom=2000, redTo=4030, width=400, height=300))
plot(Gauge)


#Retrieve the NBA data for the 13-14 season.

library(SportsAnalytics)
nba1314 <- fetch_NBAPlayerStatistics("13-14")

#Which player has the best field point percentage?
nba1314$percentage <- nba1314$FieldGoalsMade /nba1314$FieldGoalsAttempted
nba1314 <- nba1314[order(-nba1314$percentage),] 
nba1314$Name[1]
nba1314$Name[2]

#"Andris Biedrins"  "Deandre Liggins" were the two highest



#  Which player has the best free throw percentage?
nba1314$percentageFT <- nba1314$FreeThrowsMade /nba1314$FreeThrowsAttempted
nba1314 <- nba1314[order(-nba1314$percentageFT),] 
FT <- subset(nba1314,percentageFT==1)
FT$Name

#  Which player has the best three point percentage?

nba1314$percentagethree <- nba1314$ThreesMade /nba1314$ThreesAttempted
nba1314 <- nba1314[order(-nba1314$percentagethree),]
three <- subset(nba1314,percentagethree==1)
three$Name


#  Do you suspect any error in the TotalPoints column in the dataset?
table(nba1314$TotalPoints)
hist(nba1314$TotalPoints)
which(nba1314$TotalPoints < 0) # No negative values.
which(is.na(nba1314$TotalPoints)) # No null values.

#  Show the top 10 players in terms of TotalPoints, arranged from the highest to lowest.
#Use at least 5 Google charts (your choice) to show relevant data from this dataset.
library(dplyr)
nba1314 <- nba1314[order(-nba1314$TotalPoints),] 
Top<-  slice(nba1314,c(1:10))


#three pointers
nba1314Three<- Top[c(2,21)]

Line <- gvisLineChart(nba1314Three)
plot(Line)

# total numbers played
nba1314Three<- Top[c(2,6)]
Bar <- gvisBarChart(nba1314Three)
plot(Bar)

# total percentage
Combo <- gvisComboChart(Top, xvar="Name",
                        yvar=c("FieldGoalsMade", "FieldGoalsMade"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}'))
plot(Combo)


# Blocks/Steals/TotalRebounds
Dashed <-  gvisLineChart(Top, xvar="Name", yvar=c("Steals","TotalRebounds"),
                         options=list(
                           series="[{color:'green', targetAxisIndex: 0, 
                           lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                           {color: 'blue',targetAxisIndex: 1, 
                           lineWidth: 2, lineDashStyle: [4, 1]}]",
                           vAxes="[{title:'val1'}, {title:'val2'}]"
                         ))
plot(Dashed)



# point made by taken
Scatter <- gvisScatterChart(Top[c(7,8)], 
                            options=list(
                              legend="none",
                              lineWidth=2, pointSize=0,
                              title="%", vAxis="{title:'Made points'}",
                              hAxis="{title:'Attempts taken'}", 
                              width=300, height=300))
plot(Scatter)



library(googleVis); library(XML); library(stringr)
library(httr)

webpage <- paste0("http://www.landofbasketball.com/","championships/year_by_year.htm")

data<- readHTMLTable(rawToChar(GET(webpage)$content), header = T, stringsAsFactors = F)
data <- as.character(unlist(data))
save(list = ("data"), file = "data.RData")

data.split <- strsplit(data, split='(\n|\t)')

Year <- sapply(data.split, FUN = function (x) substr(x[[1]][1], 3, 7))
Winner <- sapply(data.split,FUN = function (x) str_trim(x[[7]]))
Series <- sapply(data.split,FUN = function (x) str_trim(x[[11]]))
Opponent <- sapply(data.split,FUN = function (x) str_trim(x[[13]]))


data <- data.frame(Year=Year, Winner= Winner, Series=Series, Opponent=Opponent)





#How many times was the series swept, i.e., decided by the series score 4-0?

length(which(data$Series == '4-0'))



#  How many times was the series decided by game 7?  (Series score 4-3)

length(which(data$Series == '4-3'))
#Show 5 teams that have the most wins in descending order.

top.team <- as.data.frame(table(data$Winner))
topTeams <- top.team$Var1[order(top.team$Freq, decreasing = TRUE)]
topTeams[c(1:5)]
  