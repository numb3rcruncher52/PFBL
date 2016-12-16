## Functions for analysis

# add_wOBA <- function(stats, seasonal_constants) {
#   ## Take a list of stats and calculate the wOBA for every player split
#   
#   stats <- stats %>%
#     left_join(seasonal_constants, by = c('Season' = 'Season')) %>%
#     mutate(wOBA = calcWOBA(PA, SNG, DBL, TRI, HR, UBB, HBP, wBB, wHBP, w1B,
#                            w2B, w3B, wHR),
#            wRAA = calcWRAA(wOBA, lg_wOBA, wOBAScale, 1)) %>%
#     select(Season, split, ID:OPS, PA, wOBA, wRAA)
#   
#   stats
# }

calcPA <- function(AB, UBB, HBP, SF) {
  ## Calculates PA based on individual components
  AB + UBB + HBP + SF
}

calcWOBA <- function(PA,SNG,DBL,TRI,HR,UBB,HBP,wBB,wHBP,w1B,w2B,w3B,wHR) {
  ## Takes counting statistics and league run values and returns a wOBA value
  
  wOBA <- ifelse(PA == 0, 
                 0, 
                 (wBB*UBB + wHBP*HBP + w1B*SNG + w2B*DBL + w3B*TRI + wHR*HR)/PA)
  round(wOBA,3)
}

calcWRAA <- function(role, wOBA, lg_wOBA, scale, PA) {
  wRAA = (wOBA - lg_wOBA)/scale*PA
  
  ifelse(role == "batter", wRAA, -wRAA)
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