#clear working space
rm(list = ls())

# Load packages
library(tidyverse)
library(doBy)
library(foreign)
library(gdata)
library(readstata13)
library(sandwich)
library(stargazer)
library(ISwR)
library(psych)
library(car)
library(rgl)
library(gridExtra)
library(maps)
library(rgeos)
library(maptools)
library(rgdal)
library(mapproj)
library(ggmap)

# turn off scientific notation except for big numbers. 
options(scipen = 9)

#download  sat11 to sat18 data
#variable definitions (https://www.cde.ca.gov/ds/sp/ai/glossarysat2018.asp)
sat18 <- read_excel("sat18.xls", col_names = TRUE)
sat17 <- read_excel("sat17.xls", col_names = TRUE)
sat16 <- read_excel("sat16.xls", col_names = TRUE)
sat15 <- read_excel("sat15.xls", col_names = TRUE)
sat14 <- read_excel("sat14.xls", col_names = TRUE)

# corresponding population data
PopulationData2013 <- read.csv("PEP_2013_PEPCUMCHG.ST05_with_ann.csv")
PopulationData2014 <- read.csv("PEP_2014_PEPCUMCHG.ST05_with_ann.csv")
PopulationData2015 <- read.csv("PEP_2015_PEPCUMCHG.ST05_with_ann.csv")
PopulationData2016 <- read.csv("PEP_2016_PEPCUMCHG.ST05_with_ann.csv")
PopulationData2017 <- read.csv("PEP_2017_PEPCUMCHG.ST05_with_ann.csv")

#2018 SAT data
sat18$enroll12 <- as.integer(sat18$enroll12)
sat18$NumTstTakr <- as.integer(sat18$NumTstTakr)
sat18$NumCurrElaBenchmark <- as.integer(sat18$NumCurrElaBenchmark)
sat18$NumPreElaBenchmark <- as.integer(sat18$NumPreElaBenchmark)
sat18$TotNumElaBenchmark <- as.integer(sat18$TotNumElaBenchmark)
sat18$PctElaBenchmark <- as.integer(sat18$PctElaBenchmark)
sat18$NumCurrMathBenchmark <- as.integer(sat18$NumCurrMathBenchmark)
sat18$NumPreMathBenchmark <- as.integer(sat18$NumPreMathBenchmark)
sat18$TotNumMathBenchmark <- as.integer(sat18$TotNumMathBenchmark)
sat18$PctMathBenchmark <- as.integer(sat18$PctMathBenchmark)
sat18$TotNumBothBenchmark <- as.integer(sat18$TotNumBothBenchmark)
sat18$PctBothBenchmark <- as.integer(sat18$PctBothBenchmark)
sat18[is.na(sat18)] <- 0 

#2018 County data
sat18_county <- subset(sat18, rtype=="C")  
sat18_county <- sat18_county[-c(2),]

#2018 district data
sat18_district <- subset(sat18, rtype=="D")

#2017 SAT data
sat17[is.na(sat17)] <- 0 
sat17_county <- subset(sat17, rtype=="C")  
sat17_district <- subset(sat17, rtype=="D") 

#2016 SAT data
sat16[is.na(sat16)] <- 0 
sat16_county <- subset(sat16, rtype=="C")  
sat16_district <- subset(sat16, rtype=="D") 

#2015 SAT data
sat15[is.na(sat15)] <- 0 
sat15_county <- subset(sat15, rtype=="C")  
sat15_district <- subset(sat15, rtype=="D") 

#2014 SAT data
sat14[is.na(sat14)] <- 0 
sat14_county <- subset(sat14, rtype=="C")
sat14_district <- subset(sat14, rtype=="D") 

# merging county data
sat14to15countydata <- merge(sat14_county,sat15_county, by = c("rtype", "sname", "dname","cname"))
sat14to16countydata <- merge(sat14to15countydata, sat16_county, by = c("rtype", "sname", "dname","cname"))
sat14to17countydata <- merge(sat14to16countydata, sat17_county, by = c("rtype", "sname", "dname","cname"))
sat14to18countydata <- merge(sat14to17countydata, sat18_county, by = c("rtype", "sname", "dname","cname"))
sat14to18countydata  <- sat14to18countydata [order(sat14to18countydata $cname),]
sat14to18countydata  <- sat14to18countydata[-c(1, 2, 3)]

# district data
sat14to15district <- merge(sat14_district,sat15_district, by = c("rtype", "sname", "dname","cname"))
sat14to16district <- merge(sat14to15district,sat16_district, by = c("rtype", "sname", "dname","cname"))
sat14to17district <- merge(sat14to16district,sat17_district, by = c("rtype", "sname", "dname","cname"))
sat14to18district <- merge(sat14to17district,sat18_district, by = c("rtype", "sname", "dname","cname"))
sat14to18district <-  sat14to18district[-c(1,2)]
sat14to18district <- sat14to18district[order(sat14to18district$cname),]
sat14to18district <- sat14to18district[, c(2, 1, 3:52)]

# county data
sat18_county <- subset(sat18, rtype=="C")  

#district data
sat18_district <- subset(sat18, rtype=="D")
sat18_districtSantaClara <- subset(sat18_district, cname == "Santa Clara")
sat18_districtSantaClara <-  sat18_districtSantaClara [-c(8),]

#2018 ELA data
#Old testing
ggplot(sat18_districtSantaClara, aes(x=NumPreElaBenchmark, y=dname)) + geom_point() + labs(title = "Number Old ELA Benchmark in Santa Clara", subtitle = "ELA = English Language Acquisition", x="Number Previous ELA Benchmark", y="District Name")
#New testing
ggplot(sat18_districtSantaClara, aes(x=NumCurrElaBenchmark, y=dname)) + geom_point() + labs(title = "Number New Ela Benchmark in Santa Clara", subtitle = "ELA = English Language Acquisition", x="Number Current ELA Benchmark", y="District Name")

#County Level
#Old Test
ggplot(sat18_county, aes(x=NumPreElaBenchmark, y=cname)) + geom_point() + labs(title = "Number Old Ela Benchmark in California", subtitle = "ELA = English Language Acquisition", x="Number Previous ELA Benchmark", y="County Name")
#New test
ggplot(sat18_county, aes(x=NumCurrElaBenchmark, y=cname)) + geom_point() + labs(title = "Number New Ela Benchmark in California", subtitle = "ELA = English Language Acquisition", x="Number Current ELA Benchmark", y="County Name")

#2018 data: Visualize ELA and Math success rates at district level and county level
#Pct Ela Benchmark county
sat18_county_restricted <-  sat18_county[-c(2,46),]
ggplot(sat18_county_restricted, aes(x=PctElaBenchmark , y=cname)) + geom_point() + labs(title = "2018: Percent ELA Benchmark in California Counties", subtitle = "ELA = English Language Acquisition",   x="Percent ELA Benchmark", y="County Name")

#Pct Ela Benchmark district-level
sat18_district_ela_restrict <-  subset(sat18_district, sat18_district$PctElaBenchmark > 0)
sat18_district_ela_restrict <-  sat18_district_ela_restrict[-c(2,3)]

ggplot(sat18_district_ela_restrict, aes(x=PctElaBenchmark , y=cname)) + geom_point() + labs(title = "2018: Percent ELA Benchmark in California Districts", subtitle = "ELA = English Language Acquisition",   x="Percent ELA Benchmark", y="County Name")

#number of districts per county
barplot(table(sat18_district$cname))

#pct math benchmark county
ggplot(sat18_county_restricted, aes(x=PctMathBenchmark , y=cname)) + geom_point() + labs(title = "2018: Percent Math Benchmark in California Counties",   x="Percent Math Benchmark", y="County Name")

#pct math benchmark district
sat18_district_math_restrict <- subset(sat18_district, sat18_district$PctMathBenchmark> 0)
ggplot(sat18_district_math_restrict, aes(x=PctMathBenchmark , y=dname)) + geom_point() + labs(title = "2018: Percent ELA Benchmark in California Districts",   x="Percent Math Benchmark", y="District Name")

# Restrict data to Santa Clara, Los Angeles, San Diego, San Francisico

# Santa Clara
sat18_districtSantaClara <- subset(sat18_district, cname == "Santa Clara")
sat18_districtSantaClara <-  sat18_districtSantaClara [-c(8),]
ggplot(sat18_districtSantaClara, aes(x=PctMathBenchmark , y=dname)) + geom_point() + labs(title = "2018: Percent Math Benchmark in Santa Clara Districts",   x="Percent Math Benchmark", y="Santa Clara District Names")
# Los Angeles
sat18_districtLosAngeles <- subset(sat18_district, cname == "Los Angeles")
sat18_districtLosAngeles <-  sat18_districtLosAngeles [-c(8),]
sat18_districtLosAngelesrestrict <-  subset(sat18_districtLosAngeles, sat18_districtLosAngeles$PctMathBenchmark> 0)
ggplot(sat18_districtLosAngelesrestrict, aes(x=PctMathBenchmark , y=dname)) + geom_point() + labs(title = "2018: Percent Math Benchmark in Los Angeles Districts",   x="Percent Math Benchmark", y="Los Angeles District Names")
# San Diego
sat18_districtSanDiego <- subset(sat18_district, cname == "San Diego")
sat18_districtSanDiegorestrict <-  subset(sat18_districtSanDiego, sat18_districtSanDiego$PctMathBenchmark> 0)
ggplot(sat18_districtSanDiegorestrict, aes(x=PctMathBenchmark , y=dname)) + geom_point() + labs(title = "2018: Percent Math Benchmark in California Districts",   x="Percent Math Benchmark", y="San Diego District Names")
# San Francisco
sat18_districtSanFrancisco<- subset(sat18_district, cname == "San Francisco")
sat18_districtSanFranciscorestrict <-  subset(sat18_districtSanFrancisco, sat18_districtSanFrancisco$PctMathBenchmark> 0)
ggplot(sat18_districtSanFranciscorestrict, aes(x=PctMathBenchmark , y=dname)) + geom_point() + labs(title = "2018: Percent Math Benchmark in San Francisco District",   x="Percent Math Benchmark", y="San Francisco District Name")
#number of districts in each county
table(sat18_district$cname)
barplot(table(sat18_district$cname), ylab = "Number of Districts", main = "Number of Districts in Each County", las =3)
#Benchmark at County level
ggplot(sat18_county_restricted, aes(x=PctBothBenchmark , y=cname)) + geom_point() + labs(title = "Percent Both Benchmark in Counties",   x="Percent Both Benchmark", y="County Names")
# Benchmark at District level
sat18_district_overall_restricted <- subset(sat18_district, sat18_district$PctBothBenchmark> 0)
ggplot(sat18_district_overall_restricted, aes(x=PctBothBenchmark , y=cname)) + geom_point() + labs(title = "Percent Both Benchmark in Counties (Including Corresponding Districts)", x="Percent Both Benchmark", y="County Names")

# Population Analysis
#2013 County Population Data
PopulationData2013 <- PopulationData2013[-c(1:6)]
PopulationData2013 <- PopulationData2013[-c(1,2),]
names(PopulationData2013) <- c("Geography", "Population Estimates - April 1, 2010 Estimates Base", "Population Estimates - July 1, 2013", "Change, 2010 to 2013 - Number", "Change, 2010 to 2013 - Percent", "Rankings - Population Estimates - April 1, 2010 Estimates Base", "Rankings - Population Estimates - July 1, 2013", "Rankings - Change, 2010 to 2013 - Number", "Rankings - Change, 2010 to 2013 - Percent")
PopulationData2013 <- PopulationData2013[-c(59:137),]
PopulationData2013$County_Names <- gsub("County", " ", PopulationData2013$Geography)
PopulationData2013 <- PopulationData2013[,c(10,2,3,4,5,6,7,8,9,1)]
PopulationData2013$Geography <- NULL
PopulationData2013$County_Names <- tolower(PopulationData2013$County_Names)
PopulationData2013$Yr2013<-NA
PopulationData2013 <- PopulationData2013[,c(10,1,2,3,4,5,6,7,8,9)]
#2014 County Population Data
PopulationData2014 <- PopulationData2014[-c(1:6)]
PopulationData2014 <- PopulationData2014[-c(1,2),]
names(PopulationData2014) <- c("Geography", "Population Estimates - April 1, 2010 Estimates Base", "Population Estimates - July 1, 2014", "Change, 2010 to 2014 - Number", "Change, 2010 to 2014 - Percent", "Rankings - Population Estimates - April 1, 2010 Estimates Base", "Rankings - Population Estimates - July 1, 2014", "	Rankings - Change, 2010 to 2014 - Number", "Rankings - Change, 2010 to 2014 - Percent")
PopulationData2014 <- PopulationData2014[-c(59:137),]
PopulationData2014$County_Names <- gsub("County", " ", PopulationData2014$Geography)
PopulationData2014 <- PopulationData2014[,c(10,2,3,4,5,6,7,8,9,1)]
PopulationData2014$Geography <- NULL
PopulationData2014$County_Names <- tolower(PopulationData2014$County_Names)
PopulationData2014$Yr2014<-NA
PopulationData2014 <- PopulationData2014[,c(10,1,2,3,4,5,6,7,8,9)]
#2015 County Population Data
PopulationData2015 <- PopulationData2015[-c(1:6)]
PopulationData2015 <- PopulationData2015[-c(1,2),]
names(PopulationData2015) <- c("Geography", "Population Estimates - April 1, 2010 Estimates Base", "Population Estimates - July 1, 2015", "Change, 2010 to 2015 - Number", "Change, 2010 to 2015 - Percent", "Rankings - Population Estimates - April 1, 2010 Estimates Base", "Rankings - Population Estimates - July 1, 2015", "	Rankings - Change, 2010 to 2015 - Number", "Rankings - Change, 2010 to 2015 - Percent")
PopulationData2015 <- PopulationData2015[-c(59:137),]
PopulationData2015$County_Names <- gsub("County", " ", PopulationData2015$Geography)
PopulationData2015 <- PopulationData2015[,c(10,2,3,4,5,6,7,8,9,1)]
PopulationData2015$Geography <- NULL
PopulationData2015$County_Names <- tolower(PopulationData2015$County_Names)
PopulationData2015$Yr2015<-NA
PopulationData2015 <- PopulationData2015[,c(10,1,2,3,4,5,6,7,8,9)]

#2016 County Population Data
PopulationData2016 <- PopulationData2016[-c(1:6)]
PopulationData2016 <- PopulationData2016[-c(1,2),]
names(PopulationData2016) <- c("Geography", "Population Estimates - April 1, 2010 Estimates Base", "Population Estimates - July 1, 2016", "Change, 2010 to 2016 - Number", "Change, 2010 to 2016 - Percent", "Rankings - Population Estimates - April 1, 2010 Estimates Base", "Rankings - Population Estimates - July 1, 2016", "	Rankings - Change, 2010 to 2016 - Number", "Rankings - Change, 2010 to 2016 - Percent")
PopulationData2016 <- PopulationData2016[-c(59:137),]
PopulationData2016$County_Names <- gsub("County", " ", PopulationData2016$Geography)
PopulationData2016 <- PopulationData2016[,c(10,2,3,4,5,6,7,8,9,1)]
PopulationData2016$Geography <- NULL
PopulationData2016$County_Names <- tolower(PopulationData2016$County_Names)
PopulationData2016$Yr2016<-NA
PopulationData2016 <- PopulationData2016[,c(10,1,2,3,4,5,6,7,8,9)]

#2017 County Population Data
PopulationData2017 <- PopulationData2017[-c(1:6)]
PopulationData2017 <- PopulationData2017[-c(1,2),]
names(PopulationData2017) <- c("Geography", "Population Estimates - April 1, 2010 Estimates Base", "Population Estimates - July 1, 2017", "Change, 2010 to 2017 - Number", "Change, 2010 to 2017 - Percent", "Rankings - Population Estimates - April 1, 2010 Estimates Base", "Rankings - Population Estimates - July 1, 2017", "	Rankings - Change, 2010 to 2017 - Number", "Rankings - Change, 2010 to 2017 - Percent")
PopulationData2017 <- PopulationData2017[-c(59:137),]
PopulationData2017$County_Names <- gsub("County", " ", PopulationData2017$Geography)
PopulationData2017 <- PopulationData2017[,c(10,2,3,4,5,6,7,8,9,1)]
PopulationData2017$Geography <- NULL
PopulationData2017$County_Names <- tolower(PopulationData2017$County_Names)
PopulationData2017$Yr2017<-NA
PopulationData2017 <- PopulationData2017[,c(10,1,2,3,4,5,6,7,8,9)]

#merging all county population
county_population2013_14 <- merge(PopulationData2013, PopulationData2014, by=c("County_Names"))
county_population2013_15 <- merge(county_population2013_14, PopulationData2015, by=c("County_Names"))
county_population2013_16 <- merge(county_population2013_15, PopulationData2016, by=c("County_Names"))
county_population2013_2017<- merge(county_population2013_16, PopulationData2017, by=c("County_Names"))
sat14to18countydata[[1]] <- tolower(sat14to18countydata[[1]])
sat14to16OldScoreData <- sat14to18countydata[c(1:25)]
sat17to18NewScoreData <- sat14to18countydata[c(1,26:50)]
Only_County_Populations <- sat17to18NewScoreData[c(7:10,16:19,25:28,34:37,43:46)]
Only_County_Populations2 <- sat14to16OldScoreData[-c(5:7,13:15,21:23)]

# needed for the first comparison
County_Population13to15 <- Only_County_Populations[c(1:16)]
#needed for the second comparison
County_Population16to17 <- Only_County_Populations2[c(1:16)]

#cleaning data
sat14to16OldScoreData <- sat14to16OldScoreData[-c(5:7,13:15,21:23)]
sat17to18NewScoreData <- sat17to18NewScoreData[-c(45),]
County_Population13to15[is.na(County_Population13to15)] <- 0
County_Population13to15 <- County_Population13to15[-c(2),]
County_Population13to15[is.na(County_Population13to15)] <- 0
sat17to18NewScoreData[is.na(sat17to18NewScoreData)] <- 0

#organizing row names
rownames(County_Population13to15) <- seq(length=nrow(County_Population13to15)) 
rownames(sat14to16OldScoreData) <- seq(length=nrow(sat14to16OldScoreData)) 
names(County_Population13to15) <- c("cname", "Year_2013", "2010_Population_Estimates", "2013_Population_Estimates_Number", "Change_In_2013_Population_Estimates_Number", "Change_In_2013_Population_Estimates_Percent", "Year_2014", "2010_Population_Estimates", "2014_Population_Estimates_Number", "Change_In_2014_Population_Estimates_Number", "Change_In_2014_Population_Estimates_Percent","Year_2015", "2010_Population_Estimates", "2015_Population_Estimates_Number", "Change_In_2015_Population_Estimates_Number", "Change_In_2015_Population_Estimates_Percent")

#set to same variable type
sat14to16OldScoreData$`2013-2014` <- as.factor(sat14to16OldScoreData$`2013-2014`)
sat14to16OldScoreData$`2014-2015` <- as.factor(sat14to16OldScoreData$`2014-2015`)
sat14to16OldScoreData$`2015-2016` <- as.factor(sat14to16OldScoreData$`2015-2016`)
sat14to16OldScoreData$enroll12.x <- as.factor(sat14to16OldScoreData$enroll12.x)
sat14to16OldScoreData$enroll12.y <- as.factor(sat14to16OldScoreData$enroll12.y)
sat14to16OldScoreData$enroll12.x.1 <- as.factor(sat14to16OldScoreData$enroll12.x.1)
sat14to16OldScoreData$NumTstTakr.x <- as.factor(sat14to16OldScoreData$NumTstTakr.x)
sat14to16OldScoreData$NumTstTakr.y <- as.factor(sat14to16OldScoreData$NumTstTakr.y)
sat14to16OldScoreData$NumTstTakr.x.1 <- as.factor(sat14to16OldScoreData$NumTstTakr.x.1)
sat14to16OldScoreData$NumGE1500.x <- as.factor(sat14to16OldScoreData$NumGE1500.x)
sat14to16OldScoreData$NumGE1500.y <- as.factor(sat14to16OldScoreData$NumGE1500.y)
sat14to16OldScoreData$NumGE1500 <- as.factor(sat14to16OldScoreData$NumGE1500)
sat14to16OldScoreData$PctGE1500.x <- as.factor(sat14to16OldScoreData$PctGE1500.x)
sat14to16OldScoreData$PctGE1500.y <- as.factor(sat14to16OldScoreData$PctGE1500.y)
sat14to16OldScoreData$PctGE1500 <- as.factor(sat14to16OldScoreData$PctGE1500)

#Merging Data Sets
MergedOlderData <- merge(sat14to16OldScoreData, County_Population13to15, by="cname")
MergedOlderData <- MergedOlderData[-c(5:7, 13:15, 21:23)]
colnames(county_population2013_2017)
names(county_population2013_2017)[names(county_population2013_2017) == "County_Names"] <- "cname"
colnames(sat17to18NewScoreData)
county_population2013_2017 <-county_population2013_2017[-c(2),]

#Merging Newer Data
levels(as.factor(sat17to18NewScoreData$cname))
levels(as.factor(county_population2013_2017$cname))
county_population2013_2017$cname <- trimws(county_population2013_2017$cname)
Only_County_Populations2$ScoreDifferences <- (Only_County_Populations2$PctGE1500 - Only_County_Populations2$PctGE1500.x)
MergedNewestData <- merge(sat17to18NewScoreData, county_population2013_2017, by="cname",all=TRUE) 
MergedNewestData <- MergedNewestData[-c(45),]

# plots of change in respective years of population
ggplot(MergedNewestData, aes(x=`Change, 2010 to 2015 - Percent` , y=cname)) + geom_point() + labs(title = "County Percent Total Change in Population",  subtitle = "From 2010 - 2011 to 2015 - 2016", x="Percent Change", y="County Name")
ggplot(Only_County_Populations2, aes(x=`Change, 2010 to 2015 - Percent` , y=cname)) + geom_point() + labs(title = "County Percent Total Change in Population",  subtitle = "From 2010 - 2011 to 2015 - 2016", x="Percent Change", y="County Name")
ggplot(FinalSat14to16OldScoreData, aes(x=TotalPercentChange , y=cname)) + geom_point() + labs(title = "Total Percent Change in County SAT Scores",  subtitle = "From 2013 - 2014 to 2015 - 2016", x="Percent Change", y="County Name")
