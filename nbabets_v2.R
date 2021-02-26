{
  # install.packages('GGally')
# install.packages("ellipse")
# set the working directory
setwd('C:/Users/seanh/Desktop/Basketball Scores')
# load the R Data environment
load("C:/Users/seanh/Desktop/Basketball Scores/myData.RData")
# library('caret')
# library('GGally')
# library('dplyr')
}


# Change daily date numbers for file path and daily variable
{
  # Save daily league PPG csv from basketball ref
  # Update daily league PPG csv
  df <- read.csv('./TeamAverages/league_02_18_2021.csv')
  # Update daily lines csv  here
  feb18 <- read.csv('./Lines/02-18-2021_Lines.csv')
  # Convert the date from the lines csv file to Date datatype
  feb18$Date <- as.Date(feb18$Date,'%m/%d/%Y')
  d <- as.Date('2/18/2021','%m/%d/%Y')
  d
}

# Record number of rows for the day and merge
{
  # # Record original number of rows + 1
  sb <- sb[!sb$Date == d,]
  origRows <- nrow(sb) + 1
  sb <- merge(sb, feb18, all=TRUE)
  # # Record new number of rows
  newRows <- nrow(sb)
}


#Declare new columns for sportsbook
{
  sb[origRows:newRows,]$AwayPPG <- 0
  sb[origRows:newRows,]$HomePPG <- 0
  {
    # Record average points for Away team
    for(i in origRows:newRows){
      for(y in 1:nrow(df))
        if(grepl(sb[i,]$Away, df[y,]$Team)){
          sb[i,]$AwayPPG <- df[y,]$PTS
        }
    }
    # Record average points for Home team
    for(i in origRows:newRows){
      for(y in 1:nrow(df))
        if(grepl(sb[i,]$Home, df[y,]$Team)){
          sb[i,]$HomePPG <- df[y,]$PTS
        }
    }
  } 
  sb[origRows:newRows,]$TotalPPG <- sb[origRows:newRows,]$AwayPPG + sb[origRows:newRows,]$HomePPG
  sb[origRows:newRows,]$LineStatDiff <- sb[origRows:newRows,]$Over-sb[origRows:newRows,]$TotalPPG
  # EOD: Update daily final scores with vector of final scores
  sb[origRows:newRows,]$FinalScore <- 0
  # sb[sb$Date == d,]$FinalScore <- c(122+114,112+120,114+96,89+107,128+130,134+128,102+105,113+118,113+122,126+124)
  sb$LineFinalScoreDiff <-  sb$Over - sb$FinalScore
  sb[origRows:newRows,]$AvgPPGFinalScoreDiff <- sb[origRows:newRows,]$TotalPPG - sb[origRows:newRows,]$FinalScore
# Check Vegas confidence - run this every time
  {
    sb$Confidence <- ''
    confidence <- c('Pessimistic++', 'Pessimistic+', 'Pessimistic', 'Optimistic', 'Optimistic+','Optimistic++')
    confidence.factors <- factor(confidence, levels = confidence, order = TRUE)
    
    # Create quantiles vector for
    qPes <- quantile(sb[sb$LineStatDiff < 0,]$LineStatDiff/sb[sb$LineStatDiff < 0,]$TotalPPG, c(0,1/3,2/3,1))
    qOpt <- quantile(sb[sb$LineStatDiff > 0,]$LineStatDiff/sb[sb$LineStatDiff > 0,]$TotalPPG, c(0,1/3,2/3,1))
    qPes
    qOpt
    for(i in 1:3){
      sb[sb$LineStatDiff/sb$TotalPPG >= qPes[i] & sb$LineStatDiff/sb$TotalPPG <= qPes[i+1],]$Confidence <- levels(confidence.factors)[i]
      sb[sb$LineStatDiff/sb$TotalPPG >= qOpt[i] & sb$LineStatDiff/sb$TotalPPG <= qOpt[i+1],]$Confidence <- levels(confidence.factors)[i+3]
    }

  }
}

# Check hit or miss
{
  sb$Hit_Over <- sb$FinalScore > sb$Over
  sb$Hit_Under <- sb$FinalScore < sb$Under
  sb$Hit_Push <- sb$FinalScore == sb$Over
  
  # Track hit or miss rate
  sb$HitRate_Over <- round(nrow(sb[sb$Hit_Over == TRUE,])/nrow(sb),2)
  sb$HitRate_Under <- round(nrow(sb[sb$Hit_Over == FALSE,])/nrow(sb),2)
}




# Charts
# {
#   x1 <- sb[!sb$Date == d,4:17]
#   x1<- x1[,1:9]
#   y1 <- as.factor(sb[!sb$Date == d,2])
#   y2 <- as.factor(sb[!sb$Date == d,3])
# 
#   ggpairs(x1, title='The Matrix')
# 
#   
#   
# }

displayIt <- function(){
  # Teams
  xdf <- sb[sb$Home == 'Kings' | sb$Away == 'Kings' | sb$Home == 'Heat' | sb$Away == 'Heat' | sb$Home == 'Nets' | sb$Away == 'Nets' | sb$Home == 'Lakers' | sb$Away == 'Lakers'| sb$Home == 'Raptors' | sb$Away == 'Raptors' | sb$Home == 'Bucks' | sb$Away == 'Bucks',c('Date','Home','Away','Over','Under','LineStatDiff','LineFinalScoreDiff','Confidence')]
  
  xdf <- xdf[order(xdf$Home),]
  print(xdf)
}

displayIt()


# sb <- sb_backup


# sb_backup <- sb


# Confidence parameter is purely based on the line vs the both teams PPG averages, does not account for other variables in play
# Save a backup of sportsbook

save.image("C:/Users/seanh/Desktop/Basketball Scores/myData.RData")

# write.csv(sb,"C:/Users/seanh/Desktop/Basketball Scores/Exports/Feb_15_2021_sb.csv")
