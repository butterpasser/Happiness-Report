library(tidyverse)
library(rvest)
library(XML)
library(httr)
literacy_rate <- read.csv("literacyData.csv")
literacyRate <- subset(literacy_rate,select=c(Indicator,Country,Time,Value))
#ggplot(subset(literacyRate,Country=="Developing countries"),aes(Time,Value))+geom_line(aes(color=Indicator))

hiv <- read.csv('HIV_0000000001.csv')
names(hiv)<-c('Country','Number_of_Victims')
#geom_maps()

lifegdp <- read.csv("gdp-vs-happiness.csv")
names(lifegdp)<-c("Entity","Code","Year","GDP_per_Capita","Life_Satisfaction","Population")
lifegdp <-subset(lifegdp,select=-Code)
lifegdp <- filter(lifegdp,Year >2000)
#ggplot(lifegdp,aes(Year,Life_Satisfaction))+geom_line(aes(color=Entity,size=GDP_per_Capita))+guides(color='none',size='none')+theme_classic()
lifevsexpect <- read.csv('life-satisfaction-vs-life-expectancy.csv')
names(lifevsexpect) <- c('Entity','Code','Year','Life_Satisfaction','Life_expectancy','Population')
lifevsexpect <-subset(lifevsexpect,select=-Code)

broadband <- read.csv('broadband-penetration-by-country.csv')
names(broadband) <- c('Country','Code','Year','Percentage_of_population')
broadband <- subset(broadband,select=-Code)

mobileSubscribers <- read.csv('mobile-cellular-subscriptions-per-100-people.csv')
names(mobileSubscribers) <- c('Country','Code','Year','Mobile_Subscribers_per_100_people')

internet <- read.csv('share-of-individuals-using-the-internet.csv')
names(internet) <- c('Country','Code','Year','PopulationwithInternet')

birthvsdeath <- read.csv('birth-rate-vs-death-rate.csv')
names(birthvsdeath) <- c('Country','Code','Year','Deaths_per_1000_people','Births_per_1000_people','Population')

povertyPop<-read.csv('share-of-the-population-living-in-extreme-poverty.csv')
names(povertyPop) <- c('Country','Code','Year','Poverty_headCount')
worldpovertyPop<-filter(povertyPop, Country == 'World')
worldpovertyPop<-subset(worldpovertyPop,select=c(Year,Poverty_headCount))
x <- data.frame(0)
names(x) <- "Poverty_headCount"
modelpop <- lm(Year ~ Poverty_headCount,worldpovertyPop)
x$Year<-round(predict(modelpop,x))
worldpovertyPop <- rbind(worldpovertyPop,x)
#ggplot(worldpovertyPop,aes(Year,Poverty_headCount))+geom_line()+theme_classic()+labs(title='Absolute poverty over time',x='Years',y='Percent of population living in absolute poverty')
# World poverty ends at 2026

url <-"https://en.wikipedia.org/wiki/World_Happiness_Report"
url <- GET(url)
tableList <- readHTMLTable(doc=content(url,"text"))
worldHappinessReport2018 <- tableList[[5]]