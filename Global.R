#global


#load packages

library(WDI)
library(shiny)
library(rsconnect)



#gather and clean data
thedata <- WDI(country="all", indicator=c("SP.POP.TOTL","NY.GDP.PCAP.CD" ,"SH.DYN.MORT", "SP.DYN.TFRT.IN", "SP.DYN.LE00.IN"), extra=TRUE, start=1900, end=2016)
continent <- as.data.frame(read.csv("Data/Countries-Continents-csv.csv"))
thedata1 <- thedata[!(thedata$region=="Aggregates" | is.na(thedata$region)),]
thedata2 <- merge(thedata1,continent, by.x="country", by.y="Country", all.x=TRUE)
thedata2$lngdp <- log(thedata2$NY.GDP.PCAP.CD)
thedata2$lnmort <- log(thedata2$SH.DYN.MORT)
thedata2$lnfert <- log(thedata2$SP.DYN.TFRT.IN)
thedata2$lnlexp <- log(thedata2$SP.DYN.LE00.IN)
colnames(thedata2) <-c("", "iso2c", "year", "population", "GDP", "mortality", "fertility", "lifeexp" ,"iso3c", "region", "capital", "longitude","latitude", "income", "lending", "continent", "continent1", "lngdp", "lnmort","lnfert", "lnlexp" )
thedata2$GDP <- as.numeric(thedata2$GDP)
thedata2$mortality <- as.numeric(thedata2$mortality)
thedata2$population <- as.numeric(thedata2$population)
thedata2$fertility <- as.numeric(thedata2$fertility)
thedata2$lifeexp <- as.numeric(thedata2$lifeexp)

thedata2$GDP <- round(thedata2$GDP,0)
thedata2$lifeexp <- round(thedata2$lifeexp,1)



# a couple of functions that I need to make the graph
sizefx <- function(...) ifelse(...>=400000000, 10*(.../1000000000), 
                               (
                                 ifelse(...>=100000000, (.../10000000)^(9/20), 
                                        (
                                          ifelse(...>10000000, (.../10000000)^(1/3), (.../100000)^(1/13))))))

# bubble color formula 


colorf <-  function(...) ifelse(...=="East Asia & Pacific","#ff000075",
                                ifelse(...=="Sub-Saharan Africa","#164B8E85",
                                       ifelse(...=="Middle East & North Africa","#68CF0775",
                                              ifelse(...=="America","#FFFF0075",
                                                     ifelse(...=="Europe & Central Asia","#EC9B4A85","#029bc975")))))




# Assignment 10 shiny Application



#greate population legend graph and export as image to put in the legend.   
x <- c(1,1.5,2,2.5)
y <- c(1,1,1,1)
size <- c(100000, 10000000,100000000,1000000000)
plot(x,y, xlim= c(.7,3), ylim=c(.95,1.005), cex=sizefx(size), axes=FALSE, ylab="", xlab="", pch=16, col="gray", main="Population Size", cex.main=2)
text(1,.994,"100,000")
text(1.5,.994,"10 million")
text(2,.994,"100 million")
text(2.5,.994,"1 billion")

#the world map was an image I created using powerpoint.  I had a template that could fill in differnet regions much easier than using r.  

summary(thedata2)

#GDP min 35.37 max 193648.13
#mortality min 1.9   max 443.50
# fertility min .827  max 8.873
# lifeexp min 19.27 max 83.98


