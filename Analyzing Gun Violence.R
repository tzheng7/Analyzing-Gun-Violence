##Tony Zheng
#Final Project Gun Violence Analysis
#rm(list=ls())

GV <- read.csv("D:/Gun Violence Analysis Final Project/gun-violence-data_01-2013_03-2018.csv")

#=============================================================================================
#------------------------------Exploratory Analysis-------------------------------------------
summary(GV)
#install.packages("lubridate")
library(lubridate)
#I need the library lubridate to use the function
#ymd to parse data with year, month, day
#to be able to do analysis on the date easier

GV$date<-ymd(GV$date)
str(GV$date)
summary(GV$date)
#this shows where the first date recorded
#which is 1/1/2013 and last date 3/31/2018

#I want to analyze only the years
#Need to extract from date using lubridate's year fcn

GV$year<-year(GV$date)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)
##also allows the usage of pipes %>%


#ggplot(GV,aes(x=year)) + geom_bar(stat='count', fill='blue')
#Not a very detailed graph. Need to label bars for better visual.

ggplot(GV,aes(x=as.factor(year))) + geom_bar(stat="count", fill="light blue")+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#The Guns Violence Archive website does not have many records for 2013
#Also the data for 2018 is incomplete only has data for the first quarter
#This graph does accurately show this.


GV$quarter<-quarter(GV$date)
#uses quarter fcn to split the date into q1,q2,q3,q4

qAnalysis <- GV %>% select(year, quarter) %>% count(quarter) %>%
  ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat="identity", fill="red")+
  geom_text(aes(label=n), vjust=-0.25)
 
qAnalysis
## according to this it seems q1 and q3 of the year has higher incidents
# q1 is highest due to 2018 only having q1 data.

q2018 <- GV %>% filter(year==2018) %>% select(year, quarter) %>% group_by(year) %>% count(quarter)%>% 
  ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat="identity", fill="grey")+
  geom_text(aes(label=n), vjust=-0.25)
q2018
#only q1 data available making it the highest

q2017 <- GV %>% filter(year==2017) %>% select(year, quarter) %>% group_by(year) %>% count(quarter)%>% 
  ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat="identity", fill="grey")+
  geom_text(aes(label=n), vjust=-0.25)
q2017
# q2 and q3 highest

q2016 <- GV %>% filter(year==2016) %>% select(year, quarter) %>% group_by(year) %>% count(quarter)%>% 
  ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat="identity", fill="orange")+
  geom_text(aes(label=n), vjust=-0.25)
q2016
# q3 and q4 highest

q2015 <- GV %>% filter(year==2015) %>% select(year, quarter) %>% group_by(year) %>% count(quarter)%>% 
ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat="identity", fill="green")+
  geom_text(aes(label=n), vjust=-0.25)
q2015
# q3 and q2 highest

q2014 <- GV %>% filter(year==2014) %>% select(year, quarter) %>% group_by(year) %>% count(quarter)%>% 
  ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat="identity", fill="purple")+
  geom_text(aes(label=n), vjust=-0.25)
q2014
# q3 and q4 highest

q2013 <- GV %>% filter(year==2013) %>% select(year, quarter) %>% group_by(year) %>% count(quarter)%>% 
  ggplot(aes(x=as.factor(quarter), y=n)) + geom_bar(stat="identity", fill="pink")+
  geom_text(aes(label=n), vjust=-0.25)
q2013
# q3 and q2 highest
## The above is just going into exploratory of each individual year and the incidents


qNo2018 <- GV %>% filter(year!=2018) %>% select(year, quarter) %>% group_by(year) %>% count(quarter)%>% 
  ggplot(aes(x=as.factor(quarter), y=n)) +geom_bar(stat="identity", fill="red")
qNo2018
##Without 2018 the most incidents occur in Q3 followed by Q2

#---------------------------- End of Exploratory Analysis------------------------------------
#=============================================================================================

#=============================================================================================
#-------------------------- Incidents by Month Bar Graph--------------------------------------

GV$month<-month(GV$date, label=TRUE)
#GV$month<-month(GV$date)
##the label = TRUE part lets you display the month as words instead of 1 it'll be JANUARY

#install.packages("plotly")
library("plotly")

plotly::ggplotly(GV %>% filter(year!= 2018) %>% count(month) %>%
                   ggplot(aes(x=month, y=n)) + geom_bar(stat='identity', fill='orange') +
                   labs(x='Month', y='Number of incidents', title='Incidents by Month'))
## Removed 2018 from analysis since that would skew data in favor of q1
## Observations: it seems like generally warmer months (summer/vacation time) has more incidents
## When comparing to colder months, but January has high incident count, might be due to New Years

#-------------------------- End of Incidents by Month Bar Graph------------------------------
#=============================================================================================

#=============================================================================================
#-------------------------- Most Incident-prone Dates Analysis------------------------------

GV$day<-day(GV$date)

GV %>% filter(year!= 2018) %>% count(day) %>% top_n(10) %>% arrange(desc(n))
#Not much insight aside from later in the month and the first few days being more incident prone

GV<-GV%>%mutate(MonthDay=paste(month,day))
GV %>% filter(year!=2018) %>% count(MonthDay) %>% top_n(10) %>% arrange(desc(n))
## This proves the previous reasoning of why January has many incidents, it seems its because of New Years
## This also applies for July 4th being a holiday and 5th being the day afterwards.
## the other dates seem to all be in the warmer months where individuals are vacationing

plotly::ggplotly(GV %>% count(MonthDay) %>% top_n(10) %>%
                   ggplot(aes(x=MonthDay, y=n)) + geom_bar(stat='identity', fill='purple') +
                   labs(x='Date', y='Number of incidents', title='Most Dangerous Dates'))

GV[,'state']<-lapply(GV[,'state'], as.factor)
## Need to convert the state into a categorical variable
plotly::ggplotly(GV %>% count(state) %>%
                   ggplot(aes(x=reorder(state, n), y=n, fill=n, text=state)) +
                   geom_bar(stat='identity', fill='purple') + coord_flip() +
                   labs(x='', y='Number of incidents'), tooltip=c("text", "y"))
## Observation: It seems that Illinois has the most incidents in this dataset and Hawaii has the least
## For further insights would need to adjust based on population


#-----------------------End of Most Incident-prone Dates Analysis-----------------------------
#=============================================================================================

#=============================================================================================
#--------------------- Utilizing Secondary Dataset of Census----------------------------------
#=============================================================================================

Census <- read.csv("~/Data Mining/acs2015_county_data.csv")
#View(Census)

#=============================================================================================
#---------------------------------------- Frequency Analysis----------------------------------

statePopulationZ<-aggregate(Census$TotalPop,by=list(Census$State), FUN=sum)
colnames(statePopulationZ)<-c('State','Population')
statePopulationZ<-statePopulationZ%>%rename(state=State)
#Renaming column names to do joins
statePopulationZ$state <- as.factor(statePopulationZ$state)
#Converting the states into categorical variable
#View(statePopulationZ)

incidentsByStateZ <- GV%>%group_by(state) %>% summarize(stateIncidents=n())
incidentsByStateZ <-left_join(incidentsByStateZ, statePopulationZ, by="state")
incidentsByStateZ$frequencyIn100000 <- round((incidentsByStateZ$stateIncidents/incidentsByStateZ$Population)*100000)
#Joining the Gun Violence table with the Census data by State
#To enable further analysis based on population
head(incidentsByStateZ)
#View(incidentsByStateZ)

dfIncidents <- incidentsByStateZ%>%
  arrange(desc(frequencyIn100000)) %>%
  select(state,frequencyIn100000, Population)
View(dfIncidents)

#top_n(dfIncidents, 10, frequencyIn100000)

dfIncidents %>%
  arrange(desc(frequencyIn100000)) %>%
  slice(2:11) 
#Sliced 2->11 to show top 10, since District of Columbia was #1 and it is not a State
#Top 3 most incidents (both death and injury) per 100,000 is Alaska, Delaware and Louisianna
#Alaska having so much gun violence for such a small population is a bit alarming
#Upon further research: According to Kaiser's analysis, Alaska has the highest rate of gun-related deaths of any state.
# Alaska has 19.8 deaths per 100,000 residents, while the national average is 10.6 gun deaths 
##Source: https://www.businessinsider.com/the-state-where-youre-most-likely-to-be-killed-by-a-gun-is-one-of-the-most-beautiful-places-on-earth-2015-6

dfIncidents %>%
  arrange(desc(frequencyIn100000)) %>%
  slice(51:41) 
#Safest States being Hawaii, Arizona, and Utah, New York is actually #8 on safest State and NJ being #11

plotly::ggplotly(incidentsByStateZ%>% filter(state!="District of Columbia") %>%
                   ggplot(aes(x=reorder(state, frequencyIn100000), y=frequencyIn100000, fill=frequencyIn100000, text=state)) +
                   geom_bar(stat='identity') + coord_flip() +
                   labs(x='', y='Incidents per 100,000 inhabitants') + scale_fill_gradient(low="pink", high="purple") +
                   theme(legend.position="none"),
                 tooltip=c("text", "y"))
## Note: District of Columbia was removed
## Here's a graph giving a better visual of the incidents by State. Purple being the most incident per 100,000
## and the pink being least incidents per 100,000.


#---------------------------------End of Frequency Analysis----------------------------------
#=============================================================================================


#=============================================================================================
#----------------------------------------- Poverty Analysis----------------------------------

statePoverty<-aggregate(Census$Poverty,by=list(Census$State), FUN=sum)
colnames(statePoverty)<-c('State','Poverty')
statePoverty<-statePoverty%>%rename(state=State)
#Renaming column names to do joins
statePoverty$state <- as.factor(statePoverty$state)
#Converting the states into categorical variable
#View(statePoverty)
#Need to remove District of Columbia from dataframe
statePoverty <- statePoverty[c(statePoverty$state!="District of Columbia"), ]

PovertyByState <- incidentsByStateZ%>%group_by(state)
PovertyByState <-left_join(PovertyByState, statePoverty, by="state")

plotly::ggplotly(PovertyByState %>% filter(state!="District of Columbia")  %>%
                   ggplot(aes(x=reorder(state, Poverty), y=Poverty, fill=Poverty, text=state)) +
                   geom_bar(stat='identity') + coord_flip() +
                   labs(x='', y='Poverty Level') + scale_fill_gradient(low="yellow", high="red") +
                   theme(legend.position="none"),
                 tooltip=c("text", "y"))
#This graph can be improved if adjusted by population
## Top 5 are Texas, Georgia, Kentucky, Mississippi, and Missouri

Census$povertyPopulation<-(Census$TotalPop*Census$Poverty)*1/100
stateByPoverty<-aggregate(Census$povertyPopulation,by=list(Census$State),FUN=sum, na.rm=TRUE)
colnames(stateByPoverty)<-c('state', 'PovertyByPop')
stateByPoverty <- stateByPoverty[c(stateByPoverty$state!="District of Columbia"), ]
stateByPoverty <- stateByPoverty[c(stateByPoverty$state!="Puerto Rico"), ]

#View(stateByPoverty) 
byState<-merge(statePopulationZ,stateByPoverty,by='state')
byState$povertyRate<-(byState$PovertyByPop/byState$Population)*100
byState$noPoverty<-byState$Population-byState$PovertyByPop
#View(byState)

NewInc <- incidentsByStateZ[c(incidentsByStateZ$state!="District of Columbia"), ]
#View(NewInc)
incidentBasedonProverty<-left_join(NewInc, byState, by="state")
# Joining incident and poverty dataframe
#View(incidentBasedonProverty)

plotly::ggplotly(incidentBasedonProverty %>%
                   ggplot(aes(x=reorder(state, povertyRate), y=povertyRate, fill=povertyRate, text=state)) +
                   geom_bar(stat='identity') + coord_flip() +
                   labs(x='', y='Poverty Rate adjusted Population') + scale_fill_gradient(low="yellow", high="red") +
                   theme(legend.position="none"),
                 tooltip=c("text", "y"))
## This shows the poverty rate adjusted by population size
# On the previous graph the top 5 were : Texas, Georgia, Kentucky, Mississippi, and Missouri
## Now using population as a factor, our new top 5 is: Mississippi, New Mexico, Lousiana, Arkansas and Kentucky
## Note: Mississippi and Kentucky are still in the top 5 in both graphs
# NJ is ranked 45th and NY 20th. 
#install.packages("car")
library("car") 

#scatterplot(stateIncidents~povertyRate, data=incidentBasedonProverty, 
           # ylab="Incident Rate", xlab="Poverty Rate")

plot(PovertyByPop~stateIncidents, data=incidentBasedonProverty, 
     pch = 16, cex = 1.3, col = "blue",
     ylab="Population in Poverty", xlab="Incidents")
lm(formula = incidentBasedonProverty$PovertyByPop ~ incidentBasedonProverty$stateIncidents)
abline(-115566.9, 231.4, col= "black")
#To plot the regression line
#This is the plot of total poverty popluation vs total incidents

plot(stateIncidents~povertyRate, data=incidentBasedonProverty, 
            pch = 16, cex = 1.3, col = "red",
            ylab="Incident Rate", xlab="Poverty Rate")
lm(formula = incidentBasedonProverty$stateIncidents ~ incidentBasedonProverty$povertyRate)
abline(-1027.3, 388.8, col= "blue")
##This comparison is between total number of incidents and poverty rate which is population adjusted
## Observation: The slope isn't as big as the previous regression line, but there is still a clear increase

plot(frequencyIn100000~povertyRate, data=incidentBasedonProverty, 
     pch = 16, cex = 1.3, col = "purple",
     ylab="Incident Rate", xlab="Poverty Rate")
lm(formula = incidentBasedonProverty$frequencyIn100000 ~ incidentBasedonProverty$povertyRate)
abline(58.467, 1.685, col= "red")
# This is a comparison of frequency of incidents in 100,000 and poverty rate
# This should be the most accurate of the relationship between poverty and incidents of gun violence.
cor(incidentBasedonProverty$PovertyByPop,incidentBasedonProverty$stateIncidents)
cor(incidentBasedonProverty$stateIncidents,incidentBasedonProverty$povertyRate)
cor(incidentBasedonProverty$frequencyIn100000,incidentBasedonProverty$povertyRate)
# The trend for correlation seems to follow the higher the number the bigger the slope. 

cor(incidentBasedonProverty$noPoverty,incidentBasedonProverty$frequencyIn100000)

plot(noPoverty~frequencyIn100000, data=incidentBasedonProverty, 
     pch = 16, cex = 1.3, col = "blue",
     ylab="Population NOT in Poverty", xlab="Incidents")
lm(formula = incidentBasedonProverty$noPoverty ~ incidentBasedonProverty$frequencyIn100000)
abline(8598721, -39082, col= "black")
# Doing the opposite analysis of frequency of incidents and population not in poverty
# Helps support the previous findings, the more individuals not in poverty the less likelihood for gun violence.


#logistic_reg <- incidentBasedonProverty[,c("povertyRate","stateIncidents","frequencyIn100000")]
#test_data = glm(formula = stateIncidents~povertyRate+frequencyIn100000, data = incidentBasedonProverty)
#print(summary(test_data))
#summary(logistic_reg)

#----------------------------------End of Poverty Analysis------------------------------------
#=============================================================================================


#=============================================================================================
#------------------------------------Type of Incident Analysis--------------------------------

#install.packages("splitstackshape")
library("splitstackshape")
GV$incident_characteristics <- gsub("\\|\\|", "|", GV$incident_characteristics)
## Cleaning up the data inside incident characteristics

incidentType <- splitstackshape::cSplit(GV %>% select(incident_id, state, city_or_county, 
                                      incident_characteristics), 'incident_characteristics', 
                                     sep =  '|', direction="long")

incidentType %>% count(incident_characteristics) %>% top_n(15, wt=n) %>%
  ggplot(aes(x=reorder(incident_characteristics, n), y=n)) +
  coord_flip() + labs(x='Incident Type', y='Number of Incidents')+
  geom_bar(stat='identity', fill='orange') +
  geom_text(aes(label=n), hjust=1)

# This shows the top 15 type of incidents in the US.
# Being shot and getting wounded and injured is #1

incidentType %>% count(incident_characteristics) %>% top_n(-15, wt=n) %>%
  ggplot(aes(x=reorder(incident_characteristics, n), y=n)) +
  coord_flip() + labs(x='Incident Type', y='Number of Incidents')+
  geom_bar(stat='identity', fill='yellow') +
  geom_text(aes(label=n), hjust=1)
# This shows the least likely incident types. 



#-----------------------------End of Type of Incident Analysis--------------------------------
#=============================================================================================