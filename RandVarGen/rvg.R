#############################################
# Probability Integral Transform
#

# function to initialize dataframe, for either example
init.all<-function(){
  return(data.frame(NULL))
}

# Exponential
do.one.exp<-function(lambda,all.out){
  n.samples<-dim(all.out)[1]
  U<-runif(1,0,1)
  X<-(-log(1-U)/lambda)
  if(n.samples>0){
    height.X<-sum(round(all.out$X[1:n.samples],digits=2)==round(X,digits=2))
    height.U<-sum(round(all.out$U[1:n.samples],digits=2)==round(U,digits=2))
  } else {
    height.X<-0
    height.U<-0
  }
  return(data.frame(U=U,X=X,height.U=height.U,height.X=height.X))
}


plot.exp<-function(lambda,history){
  pdf.max<-lambda
  x.max<-max(c(1,qexp(0.95,lambda)))
  x<-seq(0,x.max,by=0.01)
  plot(dexp(x,lambda)~x,type='l',xlab="x",ylab="density",ylim=c(0,1.2*pdf.max))
  lines(c(0,0),c(0,pdf.max))

  n.points<-length(history$U)
  if(n.points>0){
#    for(i in 1:length(history$X)){
#       height[i]<-sum(round(history$X[1:i],digits=2)==round(history$X[i],digits=2))-1  # can i do this without a loop?
#    }
       color<-rep("black",n.points)
       color[n.points]<-"green"      # make the last one red
       cex<-rep(1.5,n.points) 
       cex[n.points]<-3
       hght.X<-history$height.X/(ifelse(max(history$height.X)<(10*pdf.max),10,(max(history$height.X)/pdf.max)))
       
       points(history$X,hght.X,pch="*",cex=cex,col=color)
   }
}


add.exp<-function(exp.all.out,lambda,num){
  for(i in 1:num){
    exp.all.out<-rbind(exp.all.out,do.one.exp(lambda,exp.all.out))    # need to loop this so that heights get added sequentially
  }
  return(exp.all.out)
#  return(rbind(exp.all.out,rdply(num,do.one.exp(lambda,exp.all.out))[,-1]))  # [,-1] is to get rid of the first column (which is indices)
}


# Linear Example
do.one.linear<-function(all.out){
  n.samples<-dim(all.out)[1]
  U<-runif(1,0,1)
  X<-(4*sqrt(U))
  if(n.samples>0){
    height.X<-sum(round(all.out$X[1:n.samples],digits=2)==round(X,digits=2))
    height.U<-sum(round(all.out$U[1:n.samples],digits=2)==round(U,digits=2))
  } else {
    height.X<-0
    height.U<-0
  }
  return(data.frame(U=U,X=X,height.U=height.U,height.X=height.X))
}



plot.linear<-function(history){
  pdf.max<-(1/2)
  x<-seq(0,4,by=0.01)
  plot((x/8)~x,type='l',xlab="x",ylab="density",ylim=c(0,1.2*pdf.max))
  lines(c(4,4),c(0,pdf.max))
  
  n.points<-length(history$U)
  if(n.points>0){
    #    for(i in 1:length(history$X)){
    #       height[i]<-sum(round(history$X[1:i],digits=2)==round(history$X[i],digits=2))-1  # can i do this without a loop?
    #    }
    color<-rep("black",n.points)
    color[n.points]<-"green"      # make the last one red
    cex<-rep(1.5,n.points) 
    cex[n.points]<-3
    hght.X<-history$height.X/(ifelse(max(history$height.X)<(10*pdf.max),10,(max(history$height.X)/pdf.max)))
    
    points(history$X,hght.X,pch="*",cex=cex,col=color)
  }
}


add.linear<-function(linear.all.out,num){
  for(i in 1:num){
    linear.all.out<-rbind(linear.all.out,do.one.linear(linear.all.out))    # need to loop this so that heights get added sequentially
  }
  return(linear.all.out)
  #  return(rbind(exp.all.out,rdply(num,do.one.exp(lambda,exp.all.out))[,-1]))  # [,-1] is to get rid of the first column (which is indices)
}

# most recent point details?

#################################################
# accept-reject

# function to run Accept-reject once
ar.runone<-function(fx,gx,Y,M,history){        
   U<-runif(1,0,1)
   fy<-fx(Y)
   gy<-gx(Y)
   status<-ifelse(U<=fy/(M*gy),"accept","reject")
   n.samples<-length(history$Y)
   if(n.samples>0){
     height.U<-sum(round(history$U[1:n.samples],digits=2)==round(U,digits=2))
     if(status=="accept"){
       height.Y<-sum(round(history$Y[1:n.samples],digits=2)==round(Y,digits=2) & history$status[1:n.samples]=="accept")  # can i do this without a loop?
     } else {
       height.Y<-(-sum(round(history$Y[1:n.samples],digits=2)==round(Y,digits=2) & history$status[1:n.samples]=="reject"))
     } 
   } else {
     height.U<-0
     height.Y<-0
   }
   return(data.frame(U=U,Y=Y,height.U,height.Y,status=status,fy=fy,gy=gy,M=M))
}

# Beta example
do.one.beta<-function(alpha,beta,all.out){
	gx<-function(x){return(dunif(x,0,1))}
	fx<-function(x){return(dbeta(x,shape1=alpha,shape2=beta))}
  y<-runif(1,0,1)    # use g(x)=Unif[0,1] for Beta example
	M<-optimize(dbeta,shape1=alpha,shape2=beta,interval=c(0,1),maximum=T)$objective
  # M is the maximum of the beta density function
	return(ar.runone(fx,gx,y,M,all.out))
}

plot.unif<-function(all.out){
  plot(NA,xlim=c(0,1),ylim=c(0,1),yaxt="n",xlab="u",ylab="")
  n.points<-length(all.out$U)
  if(n.points>0){
    cex<-rep(1.5,n.points)
    cex[n.points]<-3
    points(all.out$U,all.out$height.U/(ifelse(max(all.out$height.U)<10,10,max(all.out$height.U))),pch="*",cex=cex,col=c(rep("black",n.points-1),"green"))
  }
}

plot.beta<-function(alpha,beta,history){
  pdf.max<-optimize(dbeta,shape1=alpha,shape2=beta,interval=c(0,1),maximum=T)$objective
  x<-seq(0,1,by=0.01)
  plot(dbeta(x,alpha,beta)~x,type='l',xlab="y",ylab="density",ylim=c(0,pdf.max*1.5))
  if(alpha==1){
    lines(c(0,0),c(0,dbeta(0,alpha,beta)))
  }
  if(beta==1){
    lines(c(1,1),c(0,dbeta(1,alpha,beta)))
  }
  n.points<-dim(history)[1]
  if(n.points>0){
    hght.Y<-ifelse(history$status=="accept",history$height.Y/20,1.5*pdf.max+history$height.Y/20)
    if((max(history$height.Y)/20)>pdf.max){
     hght.Y<-ifelse(history$status=="accept",((pdf.max*history$height.Y)/(max(history$height.Y))),1.5*pdf.max+(1.5*pdf.max)*(history$height.Y/max(history$height.Y)))
    }
#    hght.Y<-history$height.Y/(ifelse(max(history$height.Y)<(10*pdf.max),10,(max(history$height.Y)/pdf.max)))
    color<-ifelse(history$status=="accept","black","gray80")
    color[n.points]<-ifelse(history$status[n.points]=="accept","green","red")      # make the last one red
    cex<-rep(1.5,n.points)
    cex[n.points]<-3
    points(history$Y,hght.Y,pch="*",cex=cex,col=color)
  }
}

temp.start<-function(alpha,beta){
  return(data.frame(U=NULL,V=NULL))
}

temp.start2<-function(beta.all.out,alpha,beta,num){
  for(i in 1:num){
    beta.all.out<-rbind(beta.all.out,do.one.beta(alpha,beta,beta.all.out))    # need to loop this so that heights get added sequentially
  }
  return(beta.all.out)
}


# Truncated Normal
do.one.tnorm<-function(all.out){
  gx<-function(x){return(exp(2-x))}   # might want to double-check this
  fx<-function(x){return(dnorm(x)/(1-pnorm(2)))}  # and this
  y<-2-log(1-runif(1,0,1))    # use PIT for f(y)=e^2e^-y1_{y \geq 2}
  M<-dnorm(2)/(1-pnorm(2))    
  return(ar.runone(fx,gx,y,M,all.out))
}

plot.tnorm<-function(history){
  pdf.max<-dnorm(2)/(1-pnorm(2))
  x<-seq(2,5,by=0.01)
  plot(dnorm(x)/(1-pnorm(2))~x,type='l',xlab="y",ylab="density",ylim=c(0,pdf.max*1.1))
  lines(pdf.max*exp(2-x)~x)

  n.points<-dim(history)[1]
  if(n.points>0){
    hght.Y<-ifelse(history$status=="accept",history$height.Y/20,pdf.max*exp(2-history$Y)+history$height.Y/20)
    if((max(history$height.Y)/20)>pdf.max){
      hght.Y<-ifelse(history$status=="accept",((pdf.max*history$height.Y)/(max(history$height.Y))),exp(2-history$Y)*pdf.max+(pdf.max)*(history$height.Y/max(history$height.Y)))
    }
    #    hght.Y<-history$height.Y/(ifelse(max(history$height.Y)<(10*pdf.max),10,(max(history$height.Y)/pdf.max)))
    color<-ifelse(history$status=="accept","black","gray80")
    color[n.points]<-ifelse(history$status[n.points]=="accept","green","red")      # make the last one red
    cex<-rep(1.5,n.points)
    cex[n.points]<-3
    points(history$Y,hght.Y,pch="*",cex=cex,col=color)
  }
}

start.tnorm<-function(){
  return(data.frame(NULL))
}

add.tnorm<-function(tnorm.all.out,num){
  for(i in 1:num){
    tnorm.all.out<-rbind(tnorm.all.out,do.one.tnorm(tnorm.all.out))    # need to loop this so that heights get added sequentially
  }
  return(tnorm.all.out)
}


#temp.start2<-function(linear.all.out,num){
#  for(i in 1:num){
##    linear.all.out<-rbind(linear.all.out,do.one.linear(linear.all.out))    # need to loop this so that heights get added sequentially
#  }
#  return(linear.all.out)
  #  return(rbind(exp.all.out,rdply(num,do.one.exp(lambda,exp.all.out))[,-1]))  # [,-1] is to get rid of the first column (which is indices)
#}


#add.one.beta<-function(alpha,beta){
#  
#}



