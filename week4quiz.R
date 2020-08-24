library(dplyr)
library(quantmod)
library(lubridate)

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv', 'housing2.csv')
housing2 <- read.csv('housing2.csv')
answer1 <- strsplit(names(housing2), 'wgtp')[123]

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv', 'gross2.csv')
gross2 <- read.csv('gross2.csv')
mean(as.numeric(gsub(',', '', gross2$X.3[1:196])), na.rm = TRUE)

grep('^United', gross2$X.2)
#3 countries

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv', 'fedstats.csv')
fedstats <- read.csv('fedstats.csv')

result <- inner_join(fedstats, gross2, by = c('CountryCode' = 'X'))
junes <- length(grep('Fiscal year end: June 30', result$Special.Notes))

amzn <- getSymbols("AMZN",auto.assign=FALSE)
sampleTimes <- index(amzn)
amznyears <- year(ymd(sampleTimes))
amznweekdays <- weekdays(ymd(sampleTimes))
amznfull <- table(amznyears, amznweekdays)