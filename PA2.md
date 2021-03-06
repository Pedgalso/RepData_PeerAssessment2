---
title: "Floods, huricanes, typhoons and tornados the greatest enviromental threats"
author: "Pedro M. Gallardo"
date: "21/2/2016"
output: html_document
---

##Synopsis 
This study analyses what are the meteorological events more harmful to properties, crops and human health. The data has been taken from National Oceanic and Atmospheric Administration (NOAA  <http://www.noaa.gov/>) which has been gathering information about enviromental events in EEUU since 1950. The study disregards records before 1996 as data before that year is mainly focus on tornados. According to the data it might be concluded that, in general, most harmful enviromental events for properties, crops and human health are floods, hurricanes, typhoons and tornados.

## Data Processing

The Storm Event Database from NOAA covers enviromental events from January 1950 to September 2015. Hovewer, most of the events recorded before 1996 were only tornados. From 1996 onwards, all type of events have been taken into account. <http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype>. So, in order to deal with a database more representative of every enviromental event the information before 1996 has been discarded.


```r
"download.file(\"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2\",\"StormData.csv\")"
## [1] "download.file(\"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2\",\"StormData.csv\")"
data <- read.csv("./StormData.csv")

library(lubridate)
"transform BGN_DATE column into a convenient date format to remove those events before 1996"
## [1] "transform BGN_DATE column into a convenient date format to remove those events before 1996"
data$BGN_DATE <- as.Date(as.character(data$BGN_DATE), "%m/%d/%Y %H:%M:%S")
dataf <- data[year(data$BGN_DATE) >= 1996, ]
```

Now it is required to calculate the economic cost of each event. The database presents a column for the cost of damages on properties and another for crops. However, these columns are not complete, they miss the multiplier (whether the values reported are hundreds, thousands, millions...) so another 2 columns present these multipliers. In order to transform these multipliers into a numeric values, the following process has been performed:


```r
"Data from 1996 onwards, does not use the numerical multipliers (0,1,2..) from PROPDMGEXP and PROPDMGEXP as it is shown:"
## [1] "Data from 1996 onwards, does not use the numerical multipliers (0,1,2..) from PROPDMGEXP and PROPDMGEXP as it is shown:"
unique(dataf$PROPDMGEXP)
## [1] K   M B 0
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
unique(dataf$CROPDMGEXP)
## [1] K   M B
## Levels:  ? 0 2 B k K m M
"Therefore, in the following function, numerical multipliers have been disregarded."
## [1] "Therefore, in the following function, numerical multipliers have been disregarded."

f <- function(x) {
    "toupper function converts whatever letter to capital letter"
    if (toupper(x[2]) == "H") {
        y <- as.numeric(x[1]) * 100
    } else if (toupper(x[2]) == "K") {
        y <- as.numeric(x[1]) * 1000
    } else if (toupper(x[2]) == "M") {
        y <- as.numeric(x[1]) * 1e+06
    } else if (toupper(x[2]) == "B") {
        y <- as.numeric(x[1]) * 1e+09
    } else {
        y <- as.numeric(x[1])
    }
    y
}

"now, Columns PROPDMG and CROPDMG are changed to accound for the information of the columns PROPDMGEXP and CROPDMGEXP"
## [1] "now, Columns PROPDMG and CROPDMG are changed to accound for the information of the columns PROPDMGEXP and CROPDMGEXP"

dataf$PROPDMG <- apply(data.frame(dataf$PROPDMG, dataf$PROPDMGEXP), 1, f)
dataf$CROPDMG <- apply(data.frame(dataf$CROPDMG, dataf$CROPDMGEXP), 1, f)

"A column with the total economic damage it is worked out."
## [1] "A column with the total economic damage it is worked out."
EconomicDMG <- dataf$PROPDMG + dataf$CROPDMG
```
Once it has been calculated the total economic cost for each reported event since 1996 the information is summarized by events, so a comparation may be done.


```r
finalSet <- data.frame(EVTYPE = dataf$EVTYPE, FATALITIES = dataf$FATALITIES, 
    INJURIES = dataf$INJURIES, PROPDMG = dataf$PROPDMG, CROPDMG = dataf$CROPDMG, 
    EconomicDMG)
finalSet$EVTYPE <- factor(finalSet$EVTYPE, levels = finalSet$EVTYPE[order(finalSet$EconomicDMG, 
    decreasing = TRUE)])

a <- aggregate(EconomicDMG ~ EVTYPE, finalSet, mean)
b <- aggregate(cbind(FATALITIES, INJURIES, EconomicDMG) ~ EVTYPE, finalSet, 
    sum)
b$EVTYPE <- factor(b$EVTYPE, levels = b$EVTYPE[order(b$EconomicDMG, decreasing = TRUE)])
cc <- data.frame(b, AvgDMGperEvent = a$EconomicDMG)
```

## Results
The damages of an enviromental event are assessed according to two different criteria:

1. Human health damages which are assessed by quantifying the number of injures and fatalities.
2. Economic damages which are assessed by calculating the cost of the damages to properties and crops.

### 1 Human health damage:
In order to assess the damage impact to human health the driver parameter for the comparison is injuries even though fatalities is more severe. As this study assumes the hypothesis that injured people have higher impact for government in terms of cost, time and means than fatalities.
The table below is arranged according to the events that gather more records of injured people. It is depicted that the top 3 most hazardous enviromental events for human health are tornados, floods and excessive heat. 

```r
PeopleDMG <- b[order(b$INJURIES, decreasing = TRUE), ]
head(PeopleDMG)
##            EVTYPE FATALITIES INJURIES  EconomicDMG
## 7         TORNADO       1511    20667  24900370720
## 1           FLOOD        414     6758 148919611950
## 18 EXCESSIVE HEAT       1797     6391    500125700
## 45      LIGHTNING        651     4141    749975520
## 22      TSTM WIND        241     3629   5031941790
## 12    FLASH FLOOD        887     1674  16557105610
```

### 2 Economic damage

In the left graph below, The most hazardous meteorological events are ordered according to the total economic damage. 
For instance, the case of the flood. Even though its averaged cost per event is low, it has been reported more often than others, thus being the most hazardous event for properties and crops. It is followed by storm surge and hurricane/typhoon.

It is worth noting that next to the graph of the cost of damages is the total number of injures and fatalities reported for each of those enviromental events.


```r

library(ggplot2)
"Most harmful enviromental events to properties and crops"
## [1] "Most harmful enviromental events to properties and crops"
p1 <- ggplot(data = cc[1:10, ], aes(x = EVTYPE, col = "Type of cost")) + ggtitle("Effects on properties") + 
    geom_point(aes(y = EconomicDMG, col = "Total")) + geom_point(aes(y = AvgDMGperEvent, 
    col = "Averaged")) + theme(axis.title.y = element_blank(), plot.title = element_text(lineheight = 0.8, 
    face = "bold")) + scale_y_log10() + ylab("Cost of damage in $ (Log10)") + 
    coord_flip()

"Effect on public health"
## [1] "Effect on public health"
library(reshape2)
mostDMG <- cc$EVTYPE[1:10]
PublicHealth <- melt(b, id = c("EVTYPE", "EconomicDMG"))
PublicHealth <- PublicHealth[PublicHealth$EVTYPE %in% mostDMG, ]
PublicHealth$EVTYPE <- factor(PublicHealth$EVTYPE)

p2 <- ggplot(PublicHealth, aes(x = EVTYPE, y = value, fill = variable)) + ggtitle("Effect on public health") + 
    geom_bar(stat = "identity", position = "dodge") + scale_y_log10() + ylab("People (Log10)") + 
    theme(axis.title.y = element_blank(), plot.title = element_text(lineheight = 0.8, 
        face = "bold")) + scale_fill_discrete("") + theme(axis.text.y = element_blank(), 
    axis.title.y = element_blank()) + coord_flip()

library(gridExtra)
grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
```

<img src="figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />

### Conclusion

Each one of the previous categories should have different policy actions, in the case of an event more hazardous for human health policies of risk awarness among population should be followed.
In summary, it can be concluded tha the more dangerous events for people are floods, huricanes, typhoons and tornados.


