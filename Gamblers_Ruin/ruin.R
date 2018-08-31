##################################################
#  R code written by:                            #
#                                                #
#  Peter B. Chi (pchi@calpoly.edu)               #
#  Department of Statistics                      #
#  Cal Poly State Univ, San Luis Obispo          #
#  Web: www.calpoly.edu/pchi                     #
#                                                #
#  ............................................  #
#                                                #
#  Shiny app site:                               #
#        calpolystat.shinyapps.io/GamblersRuin   #
#  ............................................  #
#                                                #
#                     Code updated on: 26SEP2014 #
##################################################


run.ruin<-function(fortuneA,fortuneB,probA){
  path<-fortuneA    # track with respect to player A
  while(path[length(path)]!=0 & path[length(path)]!=(fortuneA+fortuneB)){    
    path<-c(path,ifelse(rbinom(1,1,probA),path[length(path)]+1,path[length(path)]-1))
  } 
  return(path)
}



last.summ<-function(path){
  winner<-ifelse(path[length(path)]==0,"B","A")
  turns<-length(path)-1
  return(list(winner=winner,turns=turns))
}


run.lots<-function(fortuneA,fortuneB,probA,num){
  wins<-rep(NA,num)
  game.length<-rep(NA,num)
  for(i in 1:num){
    one.run<-run.ruin(fortuneA,fortuneB,probA) 
    result<-last.summ(one.run)
    wins[i]<-result$winner
    game.length[i]<-result$turns
  }
  wins.A<-sum(wins=="A")
  avg.length<-mean(game.length)
  return(list(last.run=one.run,playerA.wins=wins.A,average=avg.length))
}


plot.ruin<-function(path,fortuneA,fortuneB){
   summary<-last.summ(path)
   par(mar=c(5.1,5,4.1,2.1))
   plot(path~seq(0,(length(path)-1)),type='l',main="Graphical Representation of last game",
        xlab="Turn Number",ylab="",yaxt="n",ylim=c(0,fortuneA+fortuneB),col="skyblue3",lwd=2.5)
   axis(2,at=c(0,fortuneA+fortuneB),labels=c("Player B","Player A"),las=2)
   mtext(paste("The winner is Player",summary$winner,"after",summary$turns,"turns"),
         3,line=0.25,cex=1.2)
   mtext("Fortune",2,line=1)
}

