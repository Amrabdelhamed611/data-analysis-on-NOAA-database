---
title: "Title"
output: 
  html_document:
    keep_md: true
---
## synopsis that describes and summarizes the data analysis in less than 10 sentences



This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

##Loading the required packages for the analysis

## Data Processing 
download the data and loading it to R

```r
#download the data from the link
if(!file.exists("NOAADataset.csv.bz2")){
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(url, destfile= "NOAADataset.csv.bz2") 
}
#unzip the data and load the data read csv will handel the unzip of the file 
#bz2 is slow due to high comperssion so cacheing her is usefull
NOAADataset <-read.csv("NOAADataset.csv.bz2")
```
#subset the columns needed for the analysis as we knowfrom names 

```r
colsName <-  c("EVTYPE","FATALITIES","INJURIES","PROPDMG",
               "PROPDMGEXP","CROPDMG","CROPDMGEXP")
NOAASub <- subset(NOAADataset,select =colsName)
```


