batting = read.csv('Batting.csv')

head(batting)
length(batting)
str(batting)
summary(batting)

any(is.na(batting))

# On Base Percentage
batting$OBP = (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

sal = read.csv('Salaries.csv')

subset(batting,yearID >= 1985)
#or
batting[batting$yearID >= 1985,]
#or
library(dplyr)
filter(batting, yearID >= 1985)

#batting & sal data into one by playerID & yearID
combo = merge(batting, sal, by = c('playerID', 'yearID'))

summary(combo)

lost_players = subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
head(lost_players, 3)
#or
head(combo[combo$playerID == 'giambja01' | combo$playerID == 'damonjo01' | combo$playerID == 'saenzol01', ], 3)
#or using filter()....

#------------------------------------------------------------------

#Saving money
players = filter(combo, yearID == 2001)
#ggplot(players,aes(x=OBP,y=salary)) + geom_point()
plot(players$OBP, players$salary)

players = filter(players,salary < 8000000, OBP > 0)
players = filter(players,AB >= 500)
plot(players$OBP, players$salary)

possible = head(arrange(players,desc(OBP)),10)
possible = possible[,c('playerID','OBP','AB','salary')]
possible


