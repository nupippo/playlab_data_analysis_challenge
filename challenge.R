####playlab

#### Import data to dataset
dataset <- read.csv("D:/playlab/cleaned_data.csv")

#### convert millisecond timestamp to datetime format
dataset$date <- as.POSIXlt(as.POSIXct(dataset$Timestamp/1000, origin="1970-01-01", tz = "GMT"))
summary(dataset$date)
#### We found min/max date is 2014-08-17 00:00:13 and 2014-08-18 01:26:09 (GMT. time)
library("ggplot2")
library("reshape2")

#### overall DAU (on 2014-08-17)
day17 <- dataset[(dataset$date$mday == "17") == TRUE,]
dau.day <- unique(day17$Session)
length(dau.day)

#### overall DAU per version
version.list <- sort(as.character(unique(day17$Version)))
dau.version = array(length(version.list))

#hist(dau.version)
# qplot(dau.version,xlab = "Version",binwidth = 15)
# ggplot(data = as.numeric(dau.version))


#### find won player score
player.won <- dataset[grep("Success", dataset$Result)+1,]

#### find won or fail player
player.success <- dataset[grep("Success", dataset$Result),]
player.failure <- dataset[grep("Failure", dataset$Result),]

level.list <- sort(unique(dataset$LevelNumber))
range = length(version.list) * length(level.list)
won.version.summary <- data.frame(version= character(range),level = numeric(range),max= numeric(range), mean= numeric(range), min = numeric(range),median = numeric(range),stringsAsFactors=FALSE)
online.time <- data.frame(max= numeric(length(version.list)),mean= numeric(length(version.list)),min= numeric(length(version.list)),sum = numeric(length(version.list)))

count <- length(version.list) * length(level.list)
win.rate <- data.frame(Version= character(count),Level= integer(count), Success= numeric(count),Failure = numeric(count))


k <- 1
  for(i in 1:length(version.list)){
    ####dau 
    dau.version[i] <- length(unique(day17$Session[day17$Version==version.list[i]]))
     remove(time.diff)
     time.diff <- array()
    ####How long do players remain in the game per game version ?
     session <- unique(dataset$Session[dataset$Version==version.list[i]])
       for(x in 1:length(session)){
         time.max <- max(dataset$Timestamp[dataset$Session==session[x]])
         time.min <- min(dataset$Timestamp[dataset$Session==session[x]])
         time.diff[x] <- time.max-time.min
      
       }
    
     time in game
     online.time$max[i] <- max(time.diff)
     online.time$mean[i] <- mean(time.diff)
     online.time$min[i] <- min(time.diff)
     online.time$sum[i] <- sum(time.diff)
     
     won.version <- player.won[player.won$Version == version.list[i],]
  
    for(j in 1:length(level.list)){
      ####find won in version and level
      won.level <- won.version[won.version$LevelNumber==level.list[j],]
      won.version.summary$version[k] <- version.list[i]
      won.version.summary$level[k] <- level.list[j]
      
      if(nrow(won.level) != 0){
        player.won.level <- head(won.level$Score,ceiling(nrow(won.version)/10))
        won.version.summary$max[k] <- max(player.won.level)
        won.version.summary$mean[k] <- mean(player.won.level)
        won.version.summary$min[k] <- min(player.won.level)
        won.version.summary$median[k] <- median(won.level$Score)
    
      }

    #### find Success or Failure  
    win.rate$Version[k] <- version.list[i]
    win.rate$Level[k] <- level.list[j]
        
    win.rate$Success[k] <- nrow(player.success[player.success$Version == version.list[i] & player.success$LevelNumber == level.list[j],])
    win.rate$Failure[k] <- nrow(player.failure[player.failure$Version == version.list[i] & player.failure$LevelNumber == level.list[j],])  
    
    k <- k+1
  } 
  }

won.version.summary.m <- melt(won.version.summary, id.var="version")




