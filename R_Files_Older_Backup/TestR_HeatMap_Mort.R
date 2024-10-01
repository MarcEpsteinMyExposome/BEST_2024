# https://r-graph-gallery.com/79-levelplot-with-ggplot2.html
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization


# load the package
library(tidyverse)
library("ggplot2")
library(plotly)
library(forcats)

#numRows<-1000
#numDays<-400

numRows<-200   # How many rows in heatmap (i.e. how many jobs?)
numDays<-200   # How long a time period

stages <- c(  "2-started" , "3-on truck", "4-dispatched", "5-at site", "6-returned","9-hold")

# NOTE:  Need to hard-code colors to have the correct # to match "numStages" as alphbetized
myColors<-c( "darkolivegreen4" , "chartreuse" , "chartreuse3" , "darkseagreen3", "darkolivegreen2","red")

generateHeatMapData <- function(rowCount) {
  returnTable<-NULL
  dayCount<-1

  # declaring an empty data frame
  returnTable <- data.frame(row = integer(),
                            day = integer(),
                            stage = integer(),
                            stringsAsFactors = FALSE)

  # Loop through all Y values (dispatcches)
  for (row in 1:numRows) {
      day<-sample(1:numDays, 1) # Start this dispatch at a random day in our timeframe
      # Decide how long to be in stage "started"
      stage <- stages[1]
      howLongInStage<-sample(1:7,1) # between 1 and 7 days in stage
      for (i in 1:howLongInStage ) {
        if( day < numDays){
          returnTable[nrow(returnTable) + 1, ] <- c(row,day,stage)
          day <- day + 1
        }
      }
      if (day>numDays) {break}

      stage <- stages[2]
      howLongInStage<-sample(1:7,1) # between 1 and 7 days in stage
      for (i in 1:howLongInStage ) {
        if( day < numDays){
          returnTable[nrow(returnTable) + 1, ] <- c(row,day,stage)
          day <- day + 1
        }
      }

      if(sample(1:10,1)<4) { # DECIDE IF I WANT TO INSERT A HOLD
        #insertHold(sample(1:7,1))
        stage <- stages[6]
        howLongInStage<-sample(1:7,1) # between 1 and 7 days in stage
        for (i in 1:howLongInStage ) {
          if( day < numDays){
            returnTable[nrow(returnTable) + 1, ] <- c(row,day,stage)
            day <- day + 1
          }
        }
      }
      if (day>numDays) {break}

      stage <- stages[3]
      howLongInStage<-sample(1:7,1) # between 1 and 7 days in stage
      for (i in 1:howLongInStage ) {
        if( day < numDays){
          returnTable[nrow(returnTable) + 1, ] <- c(row,day,stage)
          day <- day + 1
        }
      }
      if (day>numDays) {break}


      if(sample(1:10,1)<4) { # DECIDE IF I WANT TO INSERT A HOLD
        #insertHold(sample(1:7,1))
        stage <- stages[6]
        howLongInStage<-sample(3:20,1) # between 1 and 7 days in stage
        for (i in 1:howLongInStage ) {
          if( day < numDays){
            returnTable[nrow(returnTable) + 1, ] <- c(row,day,stage)
            day <- day + 1
          }
        }
      }
      if (day>numDays) {break}

      stage <- stages[4]
      howLongInStage<-sample(1:7,1) # between 1 and 7 days in stage
      for (i in 1:howLongInStage ) {
        if( day < numDays){
          returnTable[nrow(returnTable) + 1, ] <- c(row,day,stage)
          day <- day + 1
        }
      }
      if (day>numDays) {break}

      stage <- stages[5]
      howLongInStage<-sample(1:7,1) # between 1 and 7 days in stage
      for (i in 1:howLongInStage ) {
        if( day < numDays){
          returnTable[nrow(returnTable) + 1, ] <- c(row,day,stage)
          day <- day + 1
        }
      }
      if (day>numDays) {break}
  }

  returnTable$row<-as.factor(returnTable$row)
  returnTable$day<-as.integer(returnTable$day)
  returnTable
}

data<-generateHeatMapData(10)
data <- data %>%  mutate(text = paste0("x: ", day, "\n", "y: ", row, "\n", "Value: ",stage))

str(data)

head(data)
#data

### THIS WORKS   ORDERED BY DISPATCH NUMBER
p<-ggplot(data, aes(x=day, y=fct_inseq(row), fill= factor(stage))) +
  geom_tile(color = "black") +         # Just set the color of the grid
  scale_fill_manual(values=myColors,
                    name = "Stage",)+
  labs(x="Time:  Days from January 1 2021",
       y="Dispatched Order",
       title="Heatmap of Dispatch Progress")
p




# TRYING TO REODER THE Y AXIS  ORDERED BY LONGEST PROJECT
ggplot(data, aes(x=day, y=fct_infreq(row), fill= factor(stage))) +
  geom_tile(color = "black") +         # Just set the color of the grid
  scale_fill_manual(values=myColors,
                    name = "Stage",)+
  labs(x="Time:  Days from January 1 2021",
       y="Dispatched Order",
       title="Heatmap of Dispatch Progress")


