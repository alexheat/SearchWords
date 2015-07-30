
library("lubridate")
library("dplyr")
library(doParallel)
library("ggplot2")
library("reshape2")
library("TTR")
library("forecast")

registerDoParallel(cores=2)

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Read all 178 Files of keywords and merge into one file
file_list <- list.files("./csv")
for (file in file_list){
  # if the merged dataset doesn't exist, create it
  path = paste(getwd(),"/csv/",file,sep = "")
  searchDate <- ymd(substr(file, 15,22))
  if (!exists("dataset")){
    dataset <- read.csv(path, header=TRUE)
    dataset$searchDate <- searchDate
  } else {
    temp_dataset <-read.csv(path, header=FALSE, skip=1)
    temp_dataset$searchDate <- searchDate
    names(temp_dataset) <- names(dataset)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

#Remove the collums we don't need and do other cleanup on the data
dataset$Global.Monthly.Searches <- NULL
dataset$Estimated.Ad.Position <-  NULL
names(dataset) <- c("Keyword", "CPC", "Clicks", "Revenue", "MonthlySearches", "SearchDate")
dataset <-dataset[as.character(dataset$MonthlySearches) != as.character("-"),] 
dataset$MonthlySearches <- as.numeric(dataset$MonthlySearches)
dataset$Revenue <- as.numeric(gsub(",", "", dataset$Revenue, fixed = TRUE)) 
dataset <- dataset[dataset$SearchDate >= "2012-02-01 UTC",] #Start from February because Jan data is not reliable
dataset <- dataset[dataset$Clicks > 0,] #Remove rows with no clicks
dataset <-dataset[complete.cases(dataset),] #Remove rows with NAs

#save this clean csv for later
write.csv(dataset,"keywords.csv")
#dataset <- read.csv("keywords.csv", stringsAsFactors=FALSE); 
#dataset$SearchDate <- as.Date(dataset$SearchDate)

#Analysis 1 summary by data
by_date <- group_by(dataset, SearchDate)
date_summary <- summarize(by_date, CPC=mean(CPC), Revenue=sum(Revenue), Clicks=sum(Clicks))

Dates <- date_summary$SearchDate
Revenue <- date_summary$Revenue/1000
Clicks <- date_summary$Clicks
CPC <- date_summary$CPC

p1 <- qplot(x=Dates, y=Revenue, main="Revenue per Day", xlab=NULL, ylab=NULL)
p2 <- qplot(x=Dates, y=Clicks, main="Clicks per Day", xlab=NULL, ylab=NULL)
p3 <- qplot(x=Dates, y=CPC, main="Average CPC per Day", xlab=NULL, ylab=NULL)
multiplot(p1, p2, p3, cols=3)

#Analys 2, indentify the top increasing and decreasing keywords
#First group by 



#Analysis 3 netflix only
dataset_netflix <- dataset[dataset$Keyword=="netflix",]
by_date_netflix <- group_by(dataset_netflix, SearchDate)
date_summary_netflix <- summarize(by_date_netflix, CPC=mean(CPC), Revenue=sum(Revenue), Clicks=sum(Clicks))

Dates <- date_summary_netflix$SearchDate
Revenue <- date_summary_netflix$Revenue/1000
Clicks <- date_summary_netflix$Clicks
CPC <- date_summary_netflix$CPC

p1 <- qplot(x=Dates, y=Revenue, main="Revenue per Day", xlab=NULL, ylab=NULL)
p2 <- qplot(x=Dates, y=Clicks, main="Clicks per Day", xlab=NULL, ylab=NULL)
p3 <- qplot(x=Dates, y=CPC, main="Average CPC per Day", xlab=NULL, ylab=NULL)
multiplot(p1, p2, p3, cols=3)


#4 month charts without Netflix
by_date <- group_by(dataset, SearchDate)
date_summary <- summarize(by_date, CPC=mean(CPC), Revenue=sum(Revenue), Clicks=sum(Clicks))
date_summary <- arrange(date_summary, SearchDate)

Revenue <- round(date_summary$Revenue)
Start <- as.Date(date_summary[1,]$SearchDate)

RevTS <- ts(Revenue)
#Fill in some of the days that had very low sales
Revenue[18:32] <- round(seq(from=211060, to=179251, length.out =15))
Revenue[85:87] <- round(seq(from=124112, to=123375, length.out =3))

Revenue
RevTS <- ts(Revenue)
plot(RevTS)

RevTS.seriesarima <- arima(RevTS, order=c(1,0,0))
RevTS.seriesarima2 <- forecast.Arima(RevTS.seriesarima, h=30)
plot.forecast(RevTS.seriesarima2, main = "Revenue Time Series with June Forcast", xaxt="n") 
axis(side=1, at=c(0,30,60,90,120), labels = c("February", "March", "April", "May", "June")) +
  abline(h=c(0,30,60,90,120), v=c(0,30,60,90,120), col="gray", lty=3)

