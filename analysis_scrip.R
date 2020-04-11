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
#"" ,"-" ,"?" ,"+" = 0 ,and the numeric's valus is power
#"K" for thousands,"M" for millions,"B" for billions, and "H" for hundreds

decode <- c(rep(0,4),1,10,10**2,10**3,10**4,10**5,10**6,10**7,10**8,10**12,100,10**3,10**6)
names(decode) <- as.factor(union(levels(NOAASub$PROPDMGEXP),levels(NOAASub$CROPDMGEXP)))
decode

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
NOAASub$EVTYPE <- as.factor(tolower(NOAASub$EVTYPE))
NOAASub <- aggregate(.~EVTYPE, data =NOAASub, sum)
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
officialnames <-tolower(
                c( "Astronomical Low Tide"   ,"Avalanche"         ,
                   "Blizzard"                ,"Coastal Flood"     ,
                   "Cold/Wind Chill"         ,"Debris Flow"       ,
                   "Dense Fog"               ,"Dense Smoke"       ,
                   "Drought"                 ,"Dust Devil"        ,
                   "Dust Storm"              ,"Excessive Heat"    ,
                   "Extreme Cold/Wind Chill" ,"Flash Flood"       ,
                   "Flood"                   ,"Frost/Freeze"      , 
                   "Funnel Cloud"            ,"Freezing Fog"      ,
                   "Hail"                    ,"Heat"              , 
                   "Heavy Rain"              ,"Heavy Snow"        ,
                   "High Surf"               ,"High Wind"         ,
                   "Hurricane (Typhoon)"     ,"Ice Storm"         ,
                   "Lake-Effect Snow"        ,"Lakeshore Flood"   ,
                   "Lightning"               ,"Marine Hail"       ,
                   "Marine High Wind"        ,"Marine Strong Wind",
                   "Marine Thunderstorm Wind","Rip Current"       ,
                   "Seiche"                  ,"Sleet"             ,
                   "Storm Surge/Tide"        ,"Strong Wind"       ,
                   "Thunderstorm Wind"       ,"Tornado"           ,
                   "Tropical Depression"     ,"Tropical Storm"    , 
                   "Tsunami"                 ,"Volcanic Ash"      ,
                   "Waterspout"              ,"Wildfire"          ,
                   "Winter Storm"            ,"Winter Weather"
    ))
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
library(stringdist)
amatch(x = weather.data$EVTYPE, table = official.events, maxDist = 5)
for(i in c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10)){
    matchedlist <- amatch(x = NOAASub$EVTYPE,useBytes = T,
                          table = officialnames, maxDist = i)
    print(paste(i ,"   ",length(unique(matchedlist)),"   ",sum(is.na(matchedlist))))
    
}

amatch(c("Winter Weather","Winter Storm","Waterspout","Frost Freeze" ),
       table =("Weather") ,
       maxDist = 7)



##-----------------------------------------------------------------------
##-----------------------------------------------------------------------



##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
NOAASub$HarmfulEconomic <-rowSums(NOAASub[,c("CROPDMG","PROPDMG")])

Fatalities <-sum(NOAASub$FATALITIES)
paste("Total Fatalities near",round(Fatalities/10**3,2)," thousand citizen.")
Injuries <- sum(NOAASub$INJURIES)
paste("Total Injuries near",round(Injuries/10**3,2)," thousand citizen.")
PropDmg <- sum(NOAASub$PROPDMG)
paste("Total properties damage cost around",round(PropDmg/10**12,2)," billion US dollar.")
CropDmg <- sum(NOAASub$CROPDMG)
paste("Total crops damage cost around",round(CropDmg/10**12,2)," billion US dollar.")
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
TopFATALITIES <- subset(NOAASub,select = c(EVTYPE,FATALITIES))%>% 
                 top_n(.,10) 
#by thousands
g<-ggplot(TopFATALITIES, aes(x=reorder(EVTYPE,FATALITIES), y= FATALITIES/10**3)) 
(g + geom_bar(stat="identity",width=0.5, color="black", fill="red4")
   + coord_flip()
   + labs(title = "Harmful Events with respect to Fatalities") 
   + labs(x = "Event", y = "Fatalities (by thousands)")
   + theme_minimal()  )

toph <- TopFATALITIES[which.max(TopFATALITIES$FATALITIES),]
paste("The Most FATALITIES caused by",toph[[1]],"near",round(toph[2]/10**3,4),"thousand citizen")

##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
TopINJURIES <- subset(NOAASub,select = c(EVTYPE,INJURIES))  %>% 
               top_n(.,10) 
#by thousands
g<-ggplot(TopINJURIES , aes(x=reorder(EVTYPE,INJURIES), y= INJURIES/10**3)) 
(g + geom_bar(stat="identity",width=0.5, color="black", fill="firebrick3")
    + coord_flip()
    + labs(title = "Harmful Events with respect to Injuries") 
    + labs(x = "Event", y = " Injuries (by thousands)")
    + theme_minimal()  )

toph <- TopINJURIES[which.max(TopINJURIES$INJURIES),]
paste("The Most INJURIES caused by",toph[[1]],"near",round(toph[2]/10**3,4),"thousand citizen")
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
TopEconHarm <- subset(NOAASub,select = c(EVTYPE,HarmfulEconomic))%>% 
               top_n(.,10)

p<-ggplot(TopEconHarm , aes(x=reorder(EVTYPE,HarmfulEconomic), y= (HarmfulEconomic/10**12))) 
(p + geom_bar(stat="identity",width=0.5,color="black", fill="blue3")
   + coord_flip()
   + labs(title = "Events have the greatest economic Cost ") 
   + labs(x = "Event", y = "Cost by billions US dollar")
   + theme_minimal()  )
toph <- TopEconHarm[which.max(TopEconHarm$HarmfulEconomic),]
paste("The Most economic damage caused by",toph[[1]],"around",round(toph[2]/10**12,4),"billion US Dollar")
##-----------------------------------------------------------------------
