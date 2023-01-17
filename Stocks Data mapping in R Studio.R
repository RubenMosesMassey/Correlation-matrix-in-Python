
library(quantmod)
library(tseries)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpmisc)

# Renaming the column
colnames(Tesla)<-c('Date', 'Close')

# Downloading the stock data
Tesla <- as.data.frame(get.hist.quote( "tsla",start="2021-1-1", end="2022-1-1", quote=c("Cl")))
JPM <- as.data.frame(get.hist.quote( "jpm",start="2021-1-1", end="2022-1-1", quote=c("Cl")))
Agi<- as.data.frame(get.hist.quote( "A",start="2021-1-1", end="2022-1-1", quote=c("Cl")))

# changing date
Tesla <- transform(Tesla, date=ymd(row.names(Tesla))) 
JPM <- transform(JPM, date=ymd(row.names(JPM))) 
Agi <- transform(Agi, date=ymd(row.names(Agi))) 

# Adding company name 
Tesla<- transform(Tesla, company="Tesla")
JPM<- transform(JPM, company="JPM")
Agi<- transform(Agi, company="Agi")

combined<- rbind(Tesla, JPM, Agi)

# Creating a data frame for points of min and max values
ann_text1<-data.frame(Tesla[Tesla$Close== max(Tesla$Close) & Tesla$company =='Tesla',])
an_text1<-data.frame(Tesla[Tesla$Close== min(Tesla$Close) & Tesla$company =='Tesla',])

ann_text2<-data.frame(JPM[JPM$Close== max(JPM$Close) & JPM$company =='JPM',])
an_text2<-data.frame(JPM[JPM$Close== min(JPM$Close) & JPM$company =='JPM',])

ann_text3<-data.frame(Agi[Agi$Close== max(Agi$Close) & Agi$company =='Agi',])
an_text3<-data.frame(Agi[Agi$Close== min(Agi$Close) & Agi$company =='Agi',])

#Max
ann_text<- rbind(ann_text1,ann_text2, ann_text3)
#Min
an_text<- rbind(an_text1,an_text2, an_text3)

# Creating a plot
  ggplot(data=combined)+ geom_line(aes(x=date,y=Close,colour=company))+ 
    geom_text(data = ann_text, mapping = aes(x= date , y=Close , label= paste0(("stock Value $"), round(Close,digits = 2))),hjust=0, vjust=0) + 
    geom_text(data = an_text, mapping = aes(x= date , y=Close , label= paste0(("stock Value $"), round(Close,digits = 2))),hjust=0,vjust=1) + 
    geom_point(data = ann_text, mapping = aes(x= date , y=Close),colour="red", alpha=0.7) + 
    geom_point(data = an_text, mapping = aes(x= date , y=Close),colour="red", alpha=0.7)
