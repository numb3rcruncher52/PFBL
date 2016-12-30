## Optimal usage test

## Percentage of PA that are RH
RH_PERCENT <- RH_PA_FULL / (LH_PA_FULL + RH_PA_FULL)

## Combine batter information with fielder information
bat_field <- batters %>%
  select(ID, Name, min_PA, maxPAvsL, maxPAvsR, LHP_wRAA, RHP_wRAA) %>%
  right_join(fielding_join) %>%
  mutate(minPAvsR = min_PA * RH_PERCENT,
         minPAvsL = min_PA * (1 - RH_PERCENT)) %>%
  select(-ID, -min_PA)

bat_field_rh <- bat_field %>%
  select(-minPAvsL, -maxPAvsL, -LHP_wRAA)

optimalUsage <- function(player_position)
{
  ## For each position, add that positions portion to the objective
  team2 <- subset(team,select=c('Name','wRAA.RHP.C','wRAA.RHP.1B','wRAA.RHP.2B','wRAA.RHP.3B','wRAA.RHP.SS','wRAA.RHP.LF','wRAA.RHP.CF','wRAA.RHP.RF','wRAA.RHP.DH','minPA','maxPAvsR'))
  team3 <- melt(team2,id.vars=c('Name','minPA','maxPAvsR'),na.rm=T)
  
  # a = coefficient vector of obj function, run values/PA at each position 
  #     for every player, length n
  a <- player_position$
  # A1 = 
  # b1 = less-than-or-equal constraint; maximum PA for each player/position
  # A2 = 
  # b2 = greater-than-or-equal constraint; minimum PA for each player/position
  # A3 = m3 x n matrix of positions & 
  # b3 = equality constraint; total PA at each overall position
  
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