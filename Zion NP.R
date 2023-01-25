
# Class Participation Time Series 1

# NPS Stats https://irma.nps.gov/Stats/




# DATA:

# Annual Visitors (since 1919)
# data downloaded Wed 8 Jan 2020

#Title,Bookmark
#Zion NP,Bookmark this report: https://irma.nps.gov/Stats/SSRSReports/Park%20Specific%20Reports/Annual%20Park%20Recreation%20Visitation%20(1904%20-%20Last%20Calendar%20Year)

zion<-read.csv(header=TRUE,stringsAsFactors=FALSE,text='
Year,RecreationVisitors,TotalRecreationVisitors
1919,"1,814","120,135,923"
1920,"3,692","120,135,923"
1921,"2,937","120,135,923"
1922,"4,109","120,135,923"
1923,"6,408","120,135,923"
1924,"8,400","120,135,923"
1925,"16,817","120,135,923"
1926,"21,964","120,135,923"
1927,"24,303","120,135,923"
1928,"30,016","120,135,923"
1929,"33,383","120,135,923"
1930,"55,297","120,135,923"
1931,"59,186","120,135,923"
1932,"51,650","120,135,923"
1933,"48,763","120,135,923"
1934,"68,801","120,135,923"
1935,"97,280","120,135,923"
1936,"124,393","120,135,923"
1937,"137,404","120,135,923"
1938,"149,075","120,135,923"
1939,"158,063","120,135,923"
1940,"165,029","120,135,923"
1941,"192,805","120,135,923"
1942,"68,797","120,135,923"
1943,"44,089","120,135,923"
1944,"42,243","120,135,923"
1945,"78,280","120,135,923"
1946,"212,280","120,135,923"
1947,"273,953","120,135,923"
1948,"297,571","120,135,923"
1949,"307,881","120,135,923"
1950,"323,402","120,135,923"
1951,"331,079","120,135,923"
1952,"352,921","120,135,923"
1953,"389,445","120,135,923"
1954,"416,800","120,135,923"
1955,"406,800","120,135,923"
1956,"421,200","120,135,923"
1957,"525,100","120,135,923"
1958,"590,700","120,135,923"
1959,"585,000","120,135,923"
1960,"575,800","120,135,923"
1961,"604,700","120,135,923"
1962,"622,100","120,135,923"
1963,"681,100","120,135,923"
1964,"705,200","120,135,923"
1965,"763,600","120,135,923"
1966,"815,200","120,135,923"
1967,"788,400","120,135,923"
1968,"877,100","120,135,923"
1969,"904,300","120,135,923"
1970,"903,600","120,135,923"
1971,"897,000","120,135,923"
1972,"889,417","120,135,923"
1973,"993,800","120,135,923"
1974,"859,300","120,135,923"
1975,"1,055,200","120,135,923"
1976,"1,090,000","120,135,923"
1977,"1,105,900","120,135,923"
1978,"1,193,212","120,135,923"
1979,"1,040,528","120,135,923"
1980,"1,123,846","120,135,923"
1981,"1,288,808","120,135,923"
1982,"1,246,290","120,135,923"
1983,"1,273,030","120,135,923"
1984,"1,377,254","120,135,923"
1985,"1,503,272","120,135,923"
1986,"1,670,503","120,135,923"
1987,"1,777,619","120,135,923"
1988,"1,948,332","120,135,923"
1989,"1,998,856","120,135,923"
1990,"2,102,400","120,135,923"
1991,"2,236,997","120,135,923"
1992,"2,390,626","120,135,923"
1993,"2,392,580","120,135,923"
1994,"2,270,871","120,135,923"
1995,"2,430,162","120,135,923"
1996,"2,498,001","120,135,923"
1997,"2,445,534","120,135,923"
1998,"2,370,048","120,135,923"
1999,"2,449,664","120,135,923"
2000,"2,432,348","120,135,923"
2001,"2,217,779","120,135,923"
2002,"2,592,545","120,135,923"
2003,"2,458,792","120,135,923"
2004,"2,677,342","120,135,923"
2005,"2,586,665","120,135,923"
2006,"2,567,350","120,135,923"
2007,"2,657,281","120,135,923"
2008,"2,690,154","120,135,923"
2009,"2,735,402","120,135,923"
2010,"2,665,972","120,135,923"
2011,"2,825,505","120,135,923"
2012,"2,973,607","120,135,923"
2013,"2,807,387","120,135,923"
2014,"3,189,696","120,135,923"
2015,"3,648,846","120,135,923"
2016,"4,295,127","120,135,923"
2017,"4,504,812","120,135,923"
2018,"4,320,033","120,135,923"
')


# remove commas from within values
zion$RecreationVisitors<-as.numeric(gsub(',','',zion$RecreationVisitors))

# change to "Visitors (in millions)"
zion$RecreationVisitors<-zion$RecreationVisitors / 10^6


# EDA

plot(zion$Year,zion$RecreationVisitors,type="b",
     ylab="Zion NP Annual Visitors (millions)",xlab="Year")

# Analysis

# ARIMA has two requirements: additive and constant mean

# clear that visitors is not additive ... appears multiplicative
# consider transformation
zion$lnVisitors<-log(zion$RecreationVisitors)
# confirm
plot(zion$Year,zion$lnVisitors,type="b",
     ylab="log Zion NP Annual Visitors (millions)",xlab="Year")

# clear that the mean isn't constant over the full series (concerned about early days and WW2)
# consider filtering
zion1950<-subset(zion,Year>1950)
# confirm
plot(zion1950$Year,zion1950$lnVisitors,type="b",
     ylab="log Zion NP Annual Visitors (millions)",xlab="Year")

# Model: ARIMA(1,1,1)
# features: mean change year-to-year, long memory, short memory

# fit model, report parameter etimates & std errors, predictions & prediction intervals

# add library astsa
install.packages("astsa")
library(astsa)

zion.out<-sarima(zion1950$lnVisitors, 1,1,1)

zion.out$ttable

# predict next 5 years
zion.future<-sarima.for(zion1950$lnVisitors, 1,1,1, n.ahead=5)

# prediction interval for transformed response variable
zion.future.L<-zion.future$pred - 2 * zion.future$se
zion.future.U<-zion.future$pred + 2 * zion.future$se

# table of predictions and 95% prediction intervals
cbind( exp(zion.future$pred), exp(zion.future.L), exp(zion.future.U) )

# create a publication quality graphic

plot(zion$Year,zion$RecreationVisitors,type="b",
     ylab="Zion NP Annual Visitors (millions)",xlab="Year",
     xlim=c(1990,2025),ylim=c(1,7))
lines(2019:2023,exp(zion.future$pred), type="b", pch=19, col="darkorange2")
lines(2019:2023,exp(zion.future.L), lty=2, col="darkorange2")
lines(2019:2023,exp(zion.future.U), lty=2, col="darkorange2")



# Analysis Review

# Research Task: Predict Future Values

# Data Features:
# Time-Series
# Trend
# Memory

# Strengths:
# predictions looked reasonable
# uncertainty bands

# Weaknesses:
# No explanation for "why"



# Challenge:
# I would forecast the number of home runs that will be hit in the MLB every year for the next 3 years
# Past data can be found here: https://www.baseball-almanac.com/hitting/hihr6.shtml




