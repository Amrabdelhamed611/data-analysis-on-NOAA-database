library(ggplot2)
library(dplyr)
## Loading and preprocessing the data-------------------
#download the data from the link
if(!file.exists("NOAADataset.csv.bz2")){
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(url, destfile= "NOAADataset.csv.bz2") 
}
#unzip the data and load the data read csv will handel the unzip of the file 
NOAADataset <-read.csv("NOAADataset.csv.bz2")
head(NOAADataset)
#subset the columns needed for the analysis as we knowfrom names
names(NOAASub)
colsName <-  c("EVTYPE","FATALITIES","INJURIES","PROPDMG",
               "PROPDMGEXP","CROPDMG","CROPDMGEXP")
NOAASub <- subset(NOAADataset,select =colsName)
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
#PROPDMGEXP and CROPDMGEXP decode
sum(is.na(NOAASub$PROPDMGEXP))
sum(is.nan(NOAASub$PROPDMGEXP))
sum(is.nan(NOAASub$CROPDMGEXP))
sum(is.na(NOAASub$CROPDMGEXP))
levels(NOAASub$CROPDMGEXP)
levels(NOAASub$PROPDMGEXP)
union(levels(NOAASub$PROPDMGEXP),levels(NOAASub$CROPDMGEXP))

NOAASub$PROPDMGEXP <- as.factor(toupper(NOAASub$PROPDMGEXP))
NOAASub$CROPDMGEXP <- as.factor(toupper(NOAASub$CROPDMGEXP))
NOAASub$EVTYPE <- as.factor(toupper(NOAASub$EVTYPE))

levels(NOAASub$CROPDMGEXP)
levels(NOAASub$PROPDMGEXP)
union(levels(NOAASub$PROPDMGEXP),levels(NOAASub$CROPDMGEXP))
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
#will assign values accourding to union output as will assign it to names
#"" ,"-" ,"?" ,"+" = 0 ,and the numeric's = 1
#"K" for thousands,"M" for millions,"B" for billions, and "H" for hundreds

decode <- c(rep(0,4),rep(1,9),10**12,100,10**3,10**6)
names(decode) <- as.factor(union(levels(NOAASub$PROPDMGEXP),levels(NOAASub$CROPDMGEXP)))
names(decode)

NOAASub <- aggregate(.~EVTYPE + CROPDMGEXP +PROPDMGEXP, data = NOAASub, sum)

xpdecoder <- function(decodedvar){
    if(as.character(decodedvar)==""){
        var0 <- 0
    } else {
        var0 <-decode[as.character(decodedvar)]
        }
    return(var0)
}
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------

NOAASub$CROPDMGEXP<- sapply(NOAASub$CROPDMGEXP, xpdecoder)
NOAASub$PROPDMGEXP<- sapply(NOAASub$PROPDMGEXP, xpdecoder)
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
NOAASub$CROPDMG <-as.numeric( NOAASub$CROPDMGEXP * NOAASub$CROPDMG)
NOAASub$PROPDMG <-as.numeric( NOAASub$PROPDMGEXP * NOAASub$PROPDMG)
NOAASub <- subset(NOAASub,select = -c(CROPDMGEXP,PROPDMGEXP) )
NOAASub <- aggregate(.~EVTYPE, data =NOAASub, sum)
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
NOAASub$HarmfulHealth <-rowSums(NOAASub[,c("FATALITIES","INJURIES")])
NOAASub$HarmfulEconomic <-rowSums(NOAASub[,c("CROPDMG","PROPDMG")])
#NOAASub <- subset(NOAASub,select = c(EVTYPE,HarmfulHealth,HarmfulEconomic))
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
TopHealthHarm <- arrange(NOAASub,HarmfulHealth) %>%
                 subset(.,select = c(EVTYPE,HarmfulHealth))%>% `rownames<-`(.,NULL) %>%
                 top_n(.,10) 
#by thousands
g<-ggplot(TopHealthHarm , aes(x=EVTYPE, y= HarmfulHealth/10**3)) 
(g + geom_bar(stat="identity",width=0.5, color="black", fill="red4")
   + coord_flip()
   + labs(title = "Events most harmful with respect to population health") 
   + labs(x = "Event Type", y = "FATALITIES and INJURIES (by thousands)")
   + theme_minimal()  )

toph <- TopHealthHarm[which.max(TopHealthHarm$HarmfulHealth),]
paste(toph[[1]],round(toph[2]/10**3,4),"By thousands")
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
TopEconHarm <- arrange(NOAASub,HarmfulEconomic) %>%
               subset(.,select = c(EVTYPE,HarmfulEconomic)) %>%
               top_n(.,10)

p<-ggplot(TopEconHarm , aes(x=as.factor(EVTYPE), y= (HarmfulEconomic/10**12))) 
(p + geom_bar(stat="identity",width=0.5,color="black", fill="blue3")
   + coord_flip()
   + labs(title = "Events have the greatest economic consequences ") 
   + labs(x = "Event Type", y = "economic cost by billions US dollar")
   + theme_minimal()  )
toph <- TopEconHarm[which.max(TopEconHarm$HarmfulEconomic),]
paste(toph[[1]],round(toph[2]/10**12,4),"By billions")
##-----------------------------------------------------------------------
