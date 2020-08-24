library(dplyr)
library(tidyr)

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv', 'FGDP.csv')
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv', 'FEDSTATS.csv')

FGDP <- read.csv('FGDP.csv')
FEDSTATS <- read.csv('FEDSTATS.csv')

FGDP_cleaned <- select(FGDP, X:Gross.domestic.product.2012) %>%
mutate(Gross.domestic.product.2012 = as.numeric(Gross.domestic.product.2012)) %>%
filter(!is.na(Gross.domestic.product.2012))

result <- inner_join(FEDSTATS, FGDP_cleaned, by = c('CountryCode' = 'X'))
result_ordered <- arrange(result, desc(Gross.domestic.product.2012))

#rename(Gross.domestic.product.2012 = GDP)
#This coderaises an error:
#Can't rename columns that don't exist.
#Column `GDP` doesn't exist.
  
names(result_ordered)[names(result_ordered) == 'Gross.domestic.product.2012'] <- 'GDP'

print(result_ordered[13,])
oecd <- mean(result_ordered$GDP[result_ordered$Income.Group 
                                                        == 'High income: OECD'])
noOecd <- mean(result_ordered$GDP[result_ordered$Income.Group 
                                                          == 'High income: nonOECD'])

print(oecd)
print(noOecd)

result_quantiled <- mutate(result_ordered, quantileGroup = as.integer(cut(GDP, 
                                                       quantile(GDP, probs = seq(0, 1, 0.2)), 
                                                       include.lowest=TRUE)))

result_grouped <- arrange(result_quantiled, Income.Group, quantileGroup) %>%
  select(Long.Name, Income.Group, quantileGroup) %>%
  filter(Income.Group == 'Lower middle income' & quantileGroup == '1')