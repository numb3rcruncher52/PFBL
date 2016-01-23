## Functions for analysis

calcWOBA <- function(season,AB,SNG,DBL,TRI,HR,UBB,HBP,SF)
{
  coef <- coef_wOBA[coef_wOBA$Season==season,]
  
  wOBA <- (coef[,'wBB']*UBB + coef[,'wHBP']*HBP + coef[,'w1B']*SNG + coef[,'w2B']*DBL + coef[,'w3B']*TRI + coef[,'wHR']*HR)/(AB + UBB + HBP + SF)
  return(round(wOBA,3))
}

calcWRAA <- function(wOBA, lg.wOBA, scale, PA)
{
  return((wOBA - lg.wOBA)/scale*PA)
}


splitErrorRating <- function(fielders, colName)
{
  range <- as.character(lapply(fielders[,colName],FUN=function(x) ifelse(is.na(x), NA,strsplit(x,split="/")[[1]][1])))
  error <- as.numeric(lapply(fielders[,colName],FUN=function(x) ifelse(is.na(x),NA,strsplit(x,split="/")[[1]][2])))
  return(list(range=range,error=error))
}


optimalUsage <- function(team)
{
  ## For each position, add that positions portion to the objective
  team2 <- subset(team,select=c('Name','wRAA.RHP.C','wRAA.RHP.1B','wRAA.RHP.2B','wRAA.RHP.3B','wRAA.RHP.SS','wRAA.RHP.LF','wRAA.RHP.CF','wRAA.RHP.RF','wRAA.RHP.DH','minPA','maxPAvsR'))
  team3 <- melt(team2,id.vars=c('Name','minPA','maxPAvsR'),na.rm=T)
  test <- ddply(team3, .(variable), summarize, numb=length(minPA), maxPAtot=sum(maxPAvsR))
  
  a <- team3$value
  b1 <- team$maxPAvsR
  b2 <- team$minPA*.773
  b3 <- rep(544,9)
  
  A1 <- as.numeric(team3$Name==team$Name[1])
  for (i in 2:nrow(team))
  {
    A1 <- c(A1,as.numeric(team3$Name==team$Name[i]))
  }
  A1 <- matrix(A1,nrow=nrow(team),byrow=T)
  
  pos <- as.numeric(factor(team3$variable))
  fp <- rep(c("C","1B","2B",'3B','SS','LF','CF','RF','DH'),times=test$numb)
  pl.names <- paste(team3$Name,fp,sep="-")
  
  A3 <- as.numeric(pos==1)
  for (i in 2:9)
  {
    A3 <- c(A3,as.numeric(pos==i))
  }
  A3 <- matrix(A3,nrow=9,byrow=T)
  
  s <- simplex(a,A1,b1,A1,b2,A3,b3,maxi=T)
  names(s$soln) <- pl.names
  
  pl.pos.val.dad <- s$soln*s$obj
  names(pl.pos.val.dad) <- names(s$soln)
  
  return(s)
}