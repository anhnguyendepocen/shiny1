# -----------------------------------------------------------------
# App Title: Heaped Distribution Estimation
#    Author: Jimmy Wong
# -----------------------------------------------------------------

##############################################
##############################################
## Libraries
##############################################
##############################################

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("CompGLM")) install.packages("CompGLM")
if (!require("VGAM")) install.packages("VGAM")
if (!require("actuar")) install.packages("actuar")
if (!require("compoisson")) install.packages("compoisson")
if (!require("Delaporte")) install.packages("Delaporte")

library(ggplot2)
library(gridExtra)
library(CompGLM)
library(VGAM)
library(actuar)
library(compoisson)
library(Delaporte)

##############################################
##############################################
## Expit function
##############################################
##############################################

expit = function(x)
{
  exp(x)/(1+exp(x))
}

##############################################
##############################################
## Logit function
##############################################
##############################################

logit = function(p)
{
  log(p/(1-p))
}

##############################################
##############################################
## Loglikelihood functions
##############################################
##############################################

loglik.nb = function(params, degree)
{
  alpha1 = expit(params[1])
  alpha2 = expit(params[2])
  r = exp(params[3])
  mu = exp(params[4])
  
  prob.vec = rep(NA, length(degree))
  
  ind1 = which(degree==0)
  ind2 = which(degree==1 | degree==2 | degree==3 | degree==4)
  ind3 = which(degree==5)
  ind4 = which(degree>5 & degree%%5!=0)
  ind5 = which(degree>5 & degree%%5==0)
  
  prob.vec[ind1] = dnbinom(0, size=r, mu=mu)
  prob.vec[ind2] = (1-alpha1)*(dnbinom(degree[ind2], size=r, mu=mu))
  prob.vec[ind3] = dnbinom(5, size=r, mu=mu) + alpha1*sum(dnbinom(1:4, size=r, mu=mu)) + alpha2*sum(dnbinom(6:7, size=r, mu=mu))
  prob.vec[ind4] = (1-alpha2)*dnbinom(degree[ind4], size=r, mu=mu)
  prob.vec[ind5] = alpha2*(dnbinom(degree[ind5]-2, size=r, mu=mu) + dnbinom(degree[ind5]-1, size=r, mu=mu) +
                           dnbinom(degree[ind5]+1, size=r, mu=mu) + dnbinom(degree[ind5]+2, size=r, mu=mu)) + 
                   dnbinom(degree[ind5], size=r, mu=mu)
  
  loglik.sum = sum(log(prob.vec)[which(log(prob.vec)!=-Inf & !is.na(prob.vec))])
  return(loglik.sum)
}

loglik.bb = function(params, degree)
{
  alpha1 = expit(params[1])
  alpha2 = expit(params[2])
  r = round(exp(params[3]))
  p = expit(params[4])
  cor = expit(params[5])
  
  prob.vec = rep(NA, length(degree))
  
  ind1 = which(degree==0)
  ind2 = which(degree==1 | degree==2 | degree==3 | degree==4)
  ind3 = which(degree==5)
  ind4 = which(degree>5 & degree%%5!=0)
  ind5 = which(degree>5 & degree%%5==0)
    
  prob.vec[ind1] = dbetabinom(0, size=r, prob=p, rho=cor)
  prob.vec[ind2] = (1-alpha1)*(dbetabinom(degree[ind2], size=r, prob=p, rho=cor))
  prob.vec[ind3] = dbetabinom(5, size=r, prob=p, rho=cor) + alpha1*sum(dbetabinom(1:4, size=r, prob=p, rho=cor)) + 
                   alpha2*sum(dbetabinom(6:7, size=r, prob=p, rho=cor))
  prob.vec[ind4] = (1-alpha2)*dbetabinom(degree[ind4], size=r, prob=p, rho=cor)
  prob.vec[ind5] = alpha2*(dbetabinom(degree[ind5]-2, size=r, prob=p, rho=cor) + dbetabinom(degree[ind5]-1, size=r, prob=p, rho=cor) + 
                           dbetabinom(degree[ind5]+1, size=r, prob=p, rho=cor) + dbetabinom(degree[ind5]+2, size=r, prob=p, rho=cor)) + 
                   dbetabinom(degree[ind5], size=r, prob=p, rho=cor)
  
  loglik.sum = sum(log(prob.vec)[which(log(prob.vec)!=-Inf | !is.na(prob.vec))])
  return(loglik.sum)
}

loglik.cmp = function(params, degree)
{
  alpha1 = expit(params[1])
  alpha2 = expit(params[2])
  lambda = exp(params[3])
  nu = exp(params[4])
  
  prob.vec = rep(NA, length(degree))
  
  ind1 = which(degree==0)
  ind2 = which(degree==1 | degree==2 | degree==3 | degree==4)
  ind3 = which(degree==5)
  ind4 = which(degree>5 & degree%%5!=0)
  ind5 = which(degree>5 & degree%%5==0)
  
  prob.vec[ind1] = dcom(0, lambda=lambda, nu=nu)
  prob.vec[ind2] = (1-alpha1)*(dcom(degree[ind2], lambda=lambda, nu=nu))
  prob.vec[ind3] = dcom(5, lambda=lambda, nu=nu) + alpha1*sum(dcom(1:4, lambda=lambda, nu=nu)) + 
                   alpha2*sum(dcom(6:7, lambda=lambda, nu=nu))
  prob.vec[ind4] = (1-alpha2)*dcom(degree[ind4], lambda=lambda, nu=nu)
  prob.vec[ind5] = alpha2*(dcom(degree[ind5]-2, lambda=lambda, nu=nu) + dcom(degree[ind5]-1, lambda=lambda, nu=nu) + 
                           dcom(degree[ind5]+1, lambda=lambda, nu=nu) + dcom(degree[ind5]+2, lambda=lambda, nu=nu)) + 
                   dcom(degree[ind5], lambda=lambda, nu=nu)
  
  loglik.sum = sum(log(prob.vec)[which(log(prob.vec)!=-Inf & !is.na(prob.vec))])
  return(loglik.sum)
}

loglik.del = function(params, degree)
{
  alpha1 = expit(params[1])
  alpha2 = expit(params[2])
  a.d = exp(params[3])
  b.d = exp(params[4])
  c.d = exp(params[5])
  
  prob.vec = rep(NA, length(degree))
  
  ind1 = which(degree==0)
  ind2 = which(degree==1 | degree==2 | degree==3 | degree==4)
  ind3 = which(degree==5)
  ind4 = which(degree>5 & degree%%5!=0)
  ind5 = which(degree>5 & degree%%5==0)
  
  prob.vec[ind1] = ddelap(0,alpha=a.d,beta=b.d,lambda=c.d)
  prob.vec[ind2] = (1-alpha1)*(ddelap(degree[ind2], alpha=a.d, beta=b.d, lambda=c.d))
  prob.vec[ind3] = ddelap(5, alpha=a.d, beta=b.d, lambda=c.d) + alpha1*sum(ddelap(1:4, alpha=a.d, beta=b.d, lambda=c.d)) + 
                   alpha2*sum(ddelap(6:7, alpha=a.d, beta=b.d, lambda=c.d))
  prob.vec[ind4] = (1-alpha2)*ddelap(degree[ind4], alpha=a.d, beta=b.d, lambda=c.d)
  prob.vec[ind5] = alpha2*(ddelap(degree[ind5]-2, alpha=a.d, beta=b.d, lambda=c.d) + ddelap(degree[ind5]-1, alpha=a.d, beta=b.d, lambda=c.d) + 
                           ddelap(degree[ind5]+1, alpha=a.d, beta=b.d, lambda=c.d) + ddelap(degree[ind5]+2, alpha=a.d, beta=b.d, lambda=c.d)) + 
                   ddelap(degree[ind5], alpha=a.d, beta=b.d, lambda=c.d)
  
  loglik.sum = sum(log(prob.vec)[which(log(prob.vec)!=-Inf & !is.na(prob.vec))])
  return(loglik.sum)
}

loglik.ys= function(params, degree)
{
  alpha1 = expit(params[1])
  alpha2 = expit(params[2])
  r.ys = exp(params[3])
  
  prob.vec = rep(NA, length(degree))
  
  ind1 = which(degree==0)
  ind2 = which(degree==1 | degree==2 | degree==3 | degree==4)
  ind3 = which(degree==5)
  ind4 = which(degree>5 & degree%%5!=0)
  ind5 = which(degree>5 & degree%%5==0)
  
  prob.vec[ind1] = dyules(0, rho=r.ys)
  prob.vec[ind2] = (1-alpha1)*(dyules(degree[ind2], rho=r.ys))
  prob.vec[ind3] = dyules(5, rho=r.ys) + alpha1*sum(dyules(1:4, rho=r.ys)) + 
                   alpha2*sum(dyules(6:7, rho=r.ys))
  prob.vec[ind4] = (1-alpha2)*dyules(degree[ind4], rho=r.ys)
  prob.vec[ind5] = alpha2*(dyules(degree[ind5]-2, rho=r.ys) + dyules(degree[ind5]-1, rho=r.ys) + 
                           dyules(degree[ind5]+1, rho=r.ys) + dyules(degree[ind5]+2, rho=r.ys)) + 
                   dyules(degree[ind5], rho=r.ys)
  
  loglik.sum = sum(log(prob.vec)[which(log(prob.vec)!=-Inf & !is.na(prob.vec))])
  return(loglik.sum)
}

##############################################
##############################################
## Maximizing loglik functions
##############################################
##############################################

max.loglik.nb = function(a,b,c,d,e,f,g,h,degree)
{
  i=a
  m=1
  mle.nb=NULL
  conv.nb=NULL
  pars.nb=list()
  while(i<b)
  {
    i=i+.1
    j=c
    while(j<d)
    {
      j=j+.1
      k=e
      while(k<f)
      {
        k=k+1
        l=g
        while(l<h)
        {
          l=l+1
          mle.res = optim(par=c(logit(i),logit(j),log(k),log(l)), 
                          loglik.nb, degree=degree, control=list(fnscale=-1))
          mle.nb[m] = mle.res$value
          conv.nb[m] = mle.res$convergence
          pars.nb[[m]] = c(expit(mle.res$par[1:2]), exp(mle.res$par[3:4]))
          m=m+1
        }
      }
    }
  }  
  return(list(mle.nb, conv.nb, pars.nb))
}

max.loglik.bb = function(a,b,c,d,e,f,g,h,aa,bb,degree)
{
  i=a
  x=1
  mle.bb=NULL
  conv.bb=NULL
  pars.bb=list()
  while(i<b)
  {
    i=i+.2
    j=c
    while(j<d)
    {
      j=j+.2
      k=e
      while(k<f)
      {
        k=k+2
        l=g
        while(l<h)
        {
          l=l+.2
          m=aa
          while(m<bb)
          {
            m=m+.2
            mle.res = try(optim(par=c(logit(i),logit(j),log(k),logit(l),logit(m)), 
                                loglik.bb, degree=degree, control=list(fnscale=-1, maxit=10000)), 
                          silent=TRUE)
            if(class(mle.res)!="try-error")
            {
              mle.bb[x] = mle.res$value
              conv.bb[x] = mle.res$convergence
              pars.bb[[x]] = c(expit(mle.res$par[1:2]), exp(mle.res$par[3]), expit(mle.res$par[4:5]))
              x=x+1
            }
          }
        }
      }
    }
  }
  return(list(mle.bb, conv.bb, pars.bb))
}

max.loglik.cmp = function(a,b,c,d,e,f,g,h,degree)
{
  i=a
  x=1
  mle.cmp=NULL
  conv.cmp=NULL
  pars.cmp=list()
  while(i<b)
  {
    i=i+.2
    j=c
    while(j<d)
    {
      j=j+.2
      k=e
      while(k<f)
      {
        k=k+1
        l=g
        while(l<h)
        {
          l=l+1
          mle.res = try(optim(par=c(logit(i),logit(j),log(k),log(l)), 
                              loglik.cmp, degree=degree, control=list(fnscale=-1,maxit=10000)),
                        silent=TRUE)
          if(class(mle.res)!="try-error")
          {
            mle.cmp[x] = mle.res$value
            conv.cmp[x] = mle.res$convergence
            pars.cmp[[x]] = c(expit(mle.res$par[1:2]), exp(mle.res$par[3:4]))
            x=x+1
          }
        }
      }
    }
  } 
  return(list(mle.cmp, conv.cmp, pars.cmp))
}

max.loglik.del = function(a,b,c,d,e,f,g,h,aa,bb,degree)
{
  i=a
  x=1
  mle.d=NULL
  conv.d=NULL
  pars.d=list()
  while(i<b)
  {
    i=i+.2
    j=c
    while(j<d)
    {
      j=j+.2
      k=e
      while(k<f)
      {
        k=k+2
        l=g
        while(l<h)
        {
          l=l+2
          m=aa
          while(m<bb)
          {
            m=m+2
            mle.res = try(optim(par=c(logit(i),logit(j),log(k),log(l),log(m)),
                                loglik.del, degree=degree, control=list(fnscale=-1,maxit=10000)),
                          silent=TRUE)
            if(class(mle.res)!="try-error")
            {
              mle.d[x] = mle.res$value
              conv.d[x] = mle.res$convergence
              pars.d[[x]] = c(expit(mle.res$par[1:2]), exp(mle.res$par[3:5]))
              x=x+1
            }
          }
        }
      }
    }
  }
  return(list(mle.d, conv.d, pars.d))
}

max.loglik.gp = function(a,b,c,d,e,f,g,h,aa,bb,cc,dd)
{
  i=a
  x=1
  mle.gp.trunc=NULL
  conv.gp.trunc=NULL
  pars.gp.trunc=list()
  while(i<b)
  {
    i=i+.1
    j=c
    while(j<d)
    {
      j=j+.1
      k=e
      while(k<f)
      {
        k=k+1
        l=g
        while(l<h)
        {
          l=l+1
          m=aa
          while(m<bb)
          {
            m=m+1
            n=cc
            while(n<dd)
            {
              n=n+.1
              mle.res = try(optim(par=c(logit(i),logit(j),log(k),log(l),log(m),logit(n)), 
                                  loglik.gp.trunc, degree=yes.am, control=list(fnscale=-1)),
                            silent=TRUE)
              if(class(mle.res)!="try-error")
              {
                mle.gp.trunc[x] = mle.res$value
                conv.gp.trunc[x] = mle.res$convergence
                pars.gp.trunc[[x]] = c(expit(mle.res$par[1:2]), exp(mle.res$par[3:5]), expit(mle.res$par[6]))
                x=x+1
              }
            }
          }
        }
      }
    }
  }  
}

max.loglik.ys = function(a,b,c,d,e,f,degree)
{
  i=a
  x=1
  mle.ys=NULL
  conv.ys=NULL
  pars.ys=list()
  while(i<b)
  {
    i=i+.2
    j=c
    while(j<d)
    {
      j=j+.2
      k=e
      while(k<f)
      {
        k=k+2
        mle.res = try(optim(par=c(logit(i),logit(j),log(k)), 
                            loglik.ys, degree=degree, control=list(fnscale=-1,maxit=10000)),
                      silent=TRUE)
        if(class(mle.res)!="try-error")
        {
          mle.ys[x] = mle.res$value
          conv.ys[x] = mle.res$convergence
          pars.ys[[x]] = c(expit(mle.res$par[1:2]), exp(mle.res$par[3]))
          x=x+1
        }
      }
    }
  }
  return(list(mle.ys, conv.ys, pars.ys))
}

##############################################
##############################################
## Simulation functions
##############################################
##############################################

nb.sim = function(n,a1,a2,size,mu,sample.size)
{
  mle.sim=matrix(nrow=n, ncol=4)
  i=0
  while(i<n)
  {
    i=i+1
    sim.deg = rnbinom(sample.size, size=size, mu=mu)
    
    ind1 = which(sim.deg==1 | sim.deg==2 | sim.deg==3 | sim.deg==4)
    ind2 = which(sim.deg>5 & sim.deg%%5!=0)
    
    bin1 = rbinom(n=length(ind1), size=1, p=a1)
    bin2 = rbinom(n=length(ind2), size=1, p=a2)
    
    sim.deg[ind1][which(bin1==1)] = 5
    sim.deg[ind2][which(bin2==1)] = (sim.deg[ind2][which(bin2==1)] - sim.deg[ind2][which(bin2==1)]%%5)*(sim.deg[ind2][which(bin2==1)]%%5<=2) +
                                    (sim.deg[ind2][which(bin2==1)] + (5-sim.deg[ind2][which(bin2==1)]%%5))*(sim.deg[ind2][which(bin2==1)]%%5>2)
    
    sim.res = optim(par=c(logit(a1),logit(a2),log(size),log(mu)), loglik.nb, degree=sim.deg, 
                    control=list(fnscale=-1,maxit=10000))
    
    mle.sim[i,1:2] = expit(sim.res$par[1:2])
    mle.sim[i,3:4] = exp(sim.res$par[3:4])
  }
  return(mle.sim)
}

bb.sim = function(n,a1,a2,size,prob,rho,sample.size)
{
  mle.sim=matrix(nrow=n, ncol=5)
  i=0
  while(i<n)
  {
    i=i+1
    sim.deg = rbetabinom(sample.size,size=size,prob=prob,rho=rho)
    
    ind1 = which(sim.deg==1 | sim.deg==2 | sim.deg==3 | sim.deg==4)
    ind2 = which(sim.deg>5 & sim.deg%%5!=0)
    
    bin1 = rbinom(n=length(ind1), size=1, p=a1)
    bin2 = rbinom(n=length(ind2), size=1, p=a2)
    
    sim.deg[ind1][which(bin1==1)] = 5
    sim.deg[ind2][which(bin2==1)] = (sim.deg[ind2][which(bin2==1)] - sim.deg[ind2][which(bin2==1)]%%5)*(sim.deg[ind2][which(bin2==1)]%%5<=2) +
                                    (sim.deg[ind2][which(bin2==1)] + (5-sim.deg[ind2][which(bin2==1)]%%5))*(sim.deg[ind2][which(bin2==1)]%%5>2)
    
    sim.res = optim(par=c(logit(a1),logit(a2),log(size),logit(prob),logit(rho)), 
                    loglik.bb, degree=sim.deg, control=list(fnscale=-1,maxit=10000))
    
    mle.sim[i,1:2] = expit(sim.res$par[1:2])
    mle.sim[i,3] = exp(sim.res$par[3])
    mle.sim[i,4:5] = expit(sim.res$par[4:5])
  }
  return(mle.sim)
}

cmp.sim = function(n,a1,a2,lam,nu,sample.size)
{
  mle.sim=matrix(nrow=n, ncol=4)
  i=0
  x=0
  while(i<n)
  {
    i=i+1
    sim.deg = rcom(sample.size,lambda=lam,nu=nu)
    
    ind1 = which(sim.deg==1 | sim.deg==2 | sim.deg==3 | sim.deg==4)
    ind2 = which(sim.deg>5 & sim.deg%%5!=0)
    
    bin1 = rbinom(n=length(ind1), size=1, p=a1)
    bin2 = rbinom(n=length(ind2), size=1, p=a2)
    
    sim.deg[ind1][which(bin1==1)] = 5
    sim.deg[ind2][which(bin2==1)] = (sim.deg[ind2][which(bin2==1)] - sim.deg[ind2][which(bin2==1)]%%5)*(sim.deg[ind2][which(bin2==1)]%%5<=2) +
                                    (sim.deg[ind2][which(bin2==1)] + (5-sim.deg[ind2][which(bin2==1)]%%5))*(sim.deg[ind2][which(bin2==1)]%%5>2)
    
    sim.res = try(optim(par=c(logit(a1),logit(a2),log(lam),log(nu)),loglik.cmp, degree=sim.deg, 
                        control=list(fnscale=-1,maxit=10000)),silent=TRUE)
    
    if(class(sim.res)!="try-error")
    {
      x=x+1
      mle.sim[x,1:2] = expit(sim.res$par[1:2])
      mle.sim[x,3:4] = exp(sim.res$par[3:4])
    }
  }
  return(mle.sim)
}

del.sim = function(n,a1,a2,alpha,beta,lambda,sample.size)
{
  mle.sim=matrix(nrow=n, ncol=5)
  i=0
  x=0
  while(i<n)
  {
    i=i+1
    sim.deg = rdelap(sample.size,alpha=alpha,beta=beta,lambda=lambda)
    
    ind1 = which(sim.deg==1 | sim.deg==2 | sim.deg==3 | sim.deg==4)
    ind2 = which(sim.deg>5 & sim.deg%%5!=0)
    
    bin1 = rbinom(n=length(ind1), size=1, p=a1)
    bin2 = rbinom(n=length(ind2), size=1, p=a2)
    
    sim.deg[ind1][which(bin1==1)] = 5
    sim.deg[ind2][which(bin2==1)] = (sim.deg[ind2][which(bin2==1)] - sim.deg[ind2][which(bin2==1)]%%5)*(sim.deg[ind2][which(bin2==1)]%%5<=2) +
                                    (sim.deg[ind2][which(bin2==1)] + (5-sim.deg[ind2][which(bin2==1)]%%5))*(sim.deg[ind2][which(bin2==1)]%%5>2)
    
    sim.res = try(optim(par=c(logit(a1),logit(a2),log(alpha),log(beta),log(lambda)), loglik.del, degree=sim.deg, 
                        control=list(fnscale=-1,maxit=10000)),silent=TRUE)
    
    if(class(sim.res)!="try-error")
    {
      x=x+1
      mle.sim[x,1:2] = expit(sim.res$par[1:2])
      mle.sim[x,3:5] = exp(sim.res$par[3:5])
    }
  }
  return(mle.sim)
}

ys.sim = function(n,a1,a2,rho,sample.size)
{
  mle.sim=matrix(nrow=n, ncol=3)
  i=0
  x=0
  while(i<n)
  {
    i=i+1
    sim.deg = ryules(sample.size,rho=rho)
    
    ind1 = which(sim.deg==1 | sim.deg==2 | sim.deg==3 | sim.deg==4)
    ind2 = which(sim.deg>5 & sim.deg%%5!=0)
    
    bin1 = rbinom(n=length(ind1), size=1, p=a1)
    bin2 = rbinom(n=length(ind2), size=1, p=a2)
    
    sim.deg[ind1][which(bin1==1)] = 5
    sim.deg[ind2][which(bin2==1)] = (sim.deg[ind2][which(bin2==1)] - sim.deg[ind2][which(bin2==1)]%%5)*(sim.deg[ind2][which(bin2==1)]%%5<=2) +
                                    (sim.deg[ind2][which(bin2==1)] + (5-sim.deg[ind2][which(bin2==1)]%%5))*(sim.deg[ind2][which(bin2==1)]%%5>2)
    
    sim.res = try(optim(par=c(logit(a1),logit(a2),log(rho)),loglik.ys, degree=sim.deg, 
                        control=list(fnscale=-1,maxit=10000)),silent=TRUE)
    
    if(class(sim.res)!="try-error")
    {
      x=x+1
      mle.sim[x,1:2] = expit(sim.res$par[1:2])
      mle.sim[x,3] = exp(sim.res$par[3])
    }
  }
  return(mle.sim)
}

##############################################################################################################
##############################################################################################################
## Shiny server
##############################################################################################################
##############################################################################################################

shinyServer(function(input, output, session) {
  
  ##############################################
  ##############################################
  ## Simulate Data Panel
  ##############################################
  ##############################################
  
  simdat = reactive({
    if(input$dataupload & !is.null(input$file))
    {
      file = read.csv(input$file$datapath, header=input$header, sep=input$sep, quote=input$quote)
      return(file)
    }else if(!input$dataupload)
    {
      input$beginstudy
      isolate({
        if(input$distdat=="Negative Binomial")
        {
          x = rnbinom(input$nsim.dat,size=input$size.dat,mu=input$mu.dat)
          y = dnbinom(min(x):max(x),size=input$size.dat,mu=input$mu.dat)
        } else if(input$distdat=="Beta Binomial")
        {
          x = rbetabinom(input$nsim.dat,size=input$rbb.dat,prob=input$pbb.dat,rho=input$corbb.dat)
          y = dbetabinom(min(x):max(x),size=input$rbb.dat,prob=input$pbb.dat,rho=input$corbb.dat)
        } else if(input$distdat=="Conway-Maxwell Poisson") 
        {
          x = rcom(input$nsim.dat,lambda=input$lam.dat,nu=input$nu.dat)
          y = dcom(min(x):max(x),lambda=input$lam.dat,nu=input$nu.dat)
        } else if(input$distdat=="Delaporte")
        {
          x = rdelap(input$nsim.dat,alpha=input$adel.dat,beta=input$bdel.dat,lambda=input$cdel.dat)
          y = ddelap(min(x):max(x),alpha=input$adel.dat,beta=input$bdel.dat,lambda=input$cdel.dat)
        } else if(input$distdat=="Yule-Simon")
        {
          x = ryules(input$nsim.dat,rho=input$rho.dat)
          y = dyules(min(x):max(x),rho=input$rho.dat)
        }
        return(list(samp=x,dens=y))
      })
    }
  })
  
  output$simdatplot = renderPlot({
    if(input$dataupload & !is.null(simdat()))
    {
      dat = data.frame(x=simdat()[[1]])
      ggplot() + geom_histogram(data=dat, aes(x=x,y=..density..),color="navy",fill="white",binwidth=1) +
        xlab("Uploaded data") + ylab("Relative frequency") + ggtitle("Histogram of uploaded data") +
        theme_bw() + scale_x_continuous(breaks=seq(min(dat$x),max(dat$x),by=5)) 
    }else if(!input$dataupload)
    {
      input$beginstudy
      isolate({
        if(input$beginstudy>0)
        {
          dat = data.frame(x=simdat()[[1]])
          dat1 = data.frame(x=min(simdat()[[1]]):max(simdat()[[1]]),
                            y=simdat()[[2]])
          ggplot() + geom_histogram(data=dat, aes(x=x,y=..density..),color="navy",fill="white",binwidth=1) +
            xlab("Actual values") + ylab("Relative frequency") + ggtitle("Histogram of actual values C with underlying distribution overlayed") +
            theme_bw() + geom_point(data=dat1, aes(x=x,y=y), size=3, shape=1) +
            scale_x_continuous(breaks=seq(min(dat$x),max(dat$x),by=5)) 
        }
      })
    }
  })
  
  output$distselect = renderUI({
    if(input$beginstudy>=0)
    {
      if(input$distdat=="Negative Binomial")
      {
        return(p("You have selected: C ~", code(paste0("NB(",input$size.dat,",",input$mu.dat,")"),
                                                style = "color:navy")))
      } else if(input$distdat=="Beta Binomial")
      {
        return(p("You have selected: C ~", 
                 code(paste0("BB(",input$rbb.dat,",",input$pbb.dat,",",input$corbb.dat,")"),
                      style = "color:navy")))
      } else if(input$distdat=="Conway-Maxwell Poisson") 
      {
        return(p("You have selected: C ~", 
                 code(paste0("COM-Poisson(",input$lam.dat,",",input$nu.dat,")"),
                      style = "color:navy")))
      } else if(input$distdat=="Delaporte")
      {
        return(p("You have selected: C ~", 
                 code(paste0("DEL(",input$adel.dat,",",input$bdel.dat,",",input$cdel.dat,")"),
                      style = "color:navy")))
      } else if(input$distdat=="Yule-Simon")
      {
        return(p("You have selected: C ~", code(paste0("YS(",input$rho.dat,")"),
                                                style = "color:navy")))
      }
    }
    return(list(samp=x,dens=y))
  })
  
  ##############################################
  ##############################################
  ## Heaping Process Panel
  ##############################################
  ##############################################
  
  output$prob1 = renderUI({
    withMathJax("\\(\\alpha_1 = P(Y=5\\,|\\,C\\in\\{1,2,3,4\\})\\)")
  })
  
  output$prob2 = renderUI({
    withMathJax("\\(\\alpha_2 = P(Y= 5[C/5]\\,|\\,C>5\\,and\\,not\\,a\\,multiple\\,of\\,5)\\)")
  })
  
  heapeddat = reactive({
    if(input$dataupload & !is.null(simdat()))
    {
      return(list(simdat()[[1]]))
    }else if(!input$dataupload)
    {
      input$beginround
      isolate({
        dat = simdat()[[1]]
        ind1 = which(dat==1 | dat==2 | dat==3 | dat==4)
        ind2 = which(dat>5 & dat%%5!=0)
        
        bin1 = rbinom(n=length(ind1), size=1, p=input$alpha1)
        bin2 = rbinom(n=length(ind2), size=1, p=input$alpha2)
        
        dat[ind1][which(bin1==1)] = 5
        dat[ind2][which(bin2==1)] = (dat[ind2][which(bin2==1)] - dat[ind2][which(bin2==1)]%%5)*(dat[ind2][which(bin2==1)]%%5<=2) +
                                    (dat[ind2][which(bin2==1)] + (5-dat[ind2][which(bin2==1)]%%5))*(dat[ind2][which(bin2==1)]%%5>2)
        
        if(input$distdat=="Negative Binomial")
        {
          dens = dnbinom(min(dat):max(dat),size=input$size.dat,mu=input$mu.dat)
        } else if(input$distdat=="Beta Binomial")
        {
          dens = dbetabinom(min(dat):max(dat),size=input$rbb.dat,prob=input$pbb.dat,rho=input$corbb.dat)
        } else if(input$distdat=="Delaporte")
        {
          dens = ddelap(min(dat):max(dat),alpha=input$adel.dat,beta=input$bdel.dat,lambda=input$cdel.dat)
        } else if(input$distdat=="Conway-Maxwell Poisson") 
        {
          dens = dcom(min(dat):max(dat),lambda=input$lam.dat,nu=input$nu.dat)
        } else if(input$distdat=="Yule-Simon")
        {
          dens = dyules(min(dat):max(dat),rho=input$rho.dat)
        }
        return(list(heaped=dat,dens=dens))
      })
    }
  })
   
  output$heapeddatplot = renderPlot({
    if(input$dataupload & !is.null(simdat()))
    {
      dat3 = data.frame(x=simdat()[[1]])
      
      g1 = ggplot() + geom_histogram(data=dat3, aes(x=x,y=..density..),color="darkred",fill="white",binwidth=1) +
        xlab("Rounded values") + ylab("Relative frequency") + ggtitle("Histogram of rounded values Y") +
        theme_bw() + scale_x_continuous(breaks=seq(min(dat3$x),max(dat3$x),by=5)) 
      
      return(g1)
    }else if(!input$dataupload)
    {
      input$beginround
      isolate({
        dat3 = data.frame(x=simdat()[[1]])
        dat4 = data.frame(x=min(simdat()[[1]]):max(simdat()[[1]]),
                          y=simdat()[[2]])
        
        g1 = ggplot() + geom_histogram(data=dat3, aes(x=x,y=..density..),color="navy",fill="white",binwidth=1) +
          xlab("Actual values") + ylab("Relative frequency") + ggtitle("Histogram of actual values C with\nunderlying distribution overlayed") +
          theme_bw() + geom_point(data=dat4, aes(x=x,y=y), size=3, shape=1) +
          scale_x_continuous(breaks=seq(min(dat3$x),max(dat3$x),by=5)) 
        
        if(input$beginround==0 & input$beginstudy>0)
        {
          graph = arrangeGrob(g1,nrow=1,ncol=2)
          return(graph)
        } else if(input$beginround>0 & input$beginstudy!=0)
        {        
          dat1 = data.frame(x=heapeddat()[[1]])
          dat2 = data.frame(x=min(heapeddat()[[1]]):max(heapeddat()[[1]]),
                            y=heapeddat()[[2]])
          
          g2 = ggplot() + geom_histogram(data=dat1, aes(x=x,y=..density..),fill="white",color="darkred",binwidth=1) +
            xlab("Rounded values") + ylab("Relative frequency") + ggtitle("Histogram of rounded values Y with\nunderlying distribution overlayed") +
            theme_bw() + geom_point(data=dat2, aes(x=x,y=y), size=3, shape=1) + 
            scale_x_continuous(breaks=seq(min(dat1$x),max(dat1$x),by=5)) 
          
          graph = arrangeGrob(g1,g2,nrow=1)
          return(graph)
        }
      })
    }
  })
  
  output$heapedpop = renderUI({
    if(!input$dataupload & input$beginround>0)
      return(bsPopover("heapeddatplot","Compare and contrast","Note the unusual peaks at multiples of 5 in the heaped distribution compared to the underlying distribution.  In addition, when both rounding probabilities are 0, the heaped distribution is the same as the underlying distribution.",
                       trigger="hover",placement="left"))
  })
  
  ##############################################
  ##############################################
  ## Maximum Likelihood Estimation Panel
  ##############################################
  ##############################################
  
  createAlert(session, inputId = "optimtime",
              message = "Choose each range of starting values to be close to the specified parameters; this will reduce the length of the optimization period.",
              type = "warning", dismiss = TRUE, block = FALSE,append = TRUE
  )
  
  mleoptim = reactive({
    input$startoptim
    withProgress(session, {
      setProgress(message = "Optimizing loglikelihood function...",value=1)
      isolate({
        if (input$startoptim>0)
        {
          if ((!input$dataupload & input$distdat=="Negative Binomial") | (input$dataupload & input$distdat1=="Negative Binomial"))
          {
            max.loglik.nb(input$alpha1.mle[1],input$alpha1.mle[2],input$alpha2.mle[1],input$alpha2.mle[2],
                          input$size.mle.nb[1],input$size.mle.nb[2],input$mu.mle.nb[1],input$mu.mle.nb[2],degree=heapeddat()[[1]])
          } else if((!input$dataupload & input$distdat=="Beta Binomial") | (input$dataupload & input$distdat1=="Beta Binomial"))
          {
            max.loglik.bb(input$alpha1.mle[1],input$alpha1.mle[2],input$alpha2.mle[1],input$alpha2.mle[2],
                          input$size.mle.bb[1],input$size.mle.bb[2],input$prob.mle.bb[1],input$prob.mle.bb[2],
                          input$rho.mle.bb[1],input$rho.mle.bb[2],degree=heapeddat()[[1]])
          } else if((!input$dataupload & input$distdat=="Conway-Maxwell Poisson") | (input$dataupload & input$distdat1=="Conway-Maxwell Poisson"))
          {
            max.loglik.cmp(input$alpha1.mle[1],input$alpha1.mle[2],input$alpha2.mle[1],input$alpha2.mle[2],
                           input$lam.mle.cmp[1],input$lam.mle.cmp[2],input$nu.mle.cmp[1],input$nu.mle.cmp[2],
                           degree=heapeddat()[[1]])
          } else if((!input$dataupload & input$distdat=="Delaporte") | (input$dataupload & input$distdat1=="Delaporte"))
          {
            max.loglik.del(input$alpha1.mle[1],input$alpha1.mle[2],input$alpha2.mle[1],input$alpha2.mle[2],
                           input$a.mle.del[1],input$a.mle.del[2],input$b.mle.del[1],input$b.mle.del[2],
                           input$c.mle.del[1],input$c.mle.del[2],degree=heapeddat()[[1]])
          } else if((!input$dataupload & input$distdat=="Yule-Simon") | (input$dataupload & input$distdat1=="Yule-Simon"))
          {
            max.loglik.ys(input$alpha1.mle[1],input$alpha1.mle[2],input$alpha2.mle[1],input$alpha2.mle[2],
                          input$rho.mle.ys[1],input$rho.mle.ys[2],degree=heapeddat()[[1]])
          }
        }
      })
    })
  })
  
  mles = reactive({
    ind = which.max(mleoptim()[[1]])[1]
    mleoptim()[[3]][[ind]]
  })
  
  heapeddatmle = reactive({
    input$startoptim
    isolate({
      dat = heapeddat()[[1]]
      
      if((!input$dataupload & input$distdat=="Negative Binomial") | (input$dataupload & input$distdat1=="Negative Binomial"))
      {
        dens = dnbinom(min(dat):max(dat),size=round(mles()[3]),mu=mles()[4])
      } else if((!input$dataupload & input$distdat=="Beta Binomial") | (input$dataupload & input$distdat1=="Beta Binomial"))
      {
        dens = dbetabinom(min(dat):max(dat),size=round(mles()[3]),prob=mles()[4],rho=mles()[5])
      } else if((!input$dataupload & input$distdat=="Delaporte") | (input$dataupload & input$distdat1=="Delaporte"))
      {
        dens = ddelap(min(dat):max(dat),alpha=mles()[3],beta=mles()[4],lambda=mles()[5])
      } else if((!input$dataupload & input$distdat=="Conway-Maxwell Poisson") | (input$dataupload & input$distdat1=="Conway-Maxwell Poisson")) 
      {
        dens = dcom(min(dat):max(dat),lambda=mles()[3],nu=mles()[4])
      } else if((!input$dataupload & input$distdat=="Yule-Simon") | (input$dataupload & input$distdat1=="Yule-Simon"))
      {
        dens = dyules(min(dat):max(dat),rho=mles()[3])
      }
      
      return(dens)
    })
  })
  
  output$estdistplot = renderPlot({
    input$startoptim
    isolate({
      if(input$startoptim>0)
      { 
        if(input$dataupload)
        {
          dat1 = data.frame(x=heapeddat()[[1]])
          dat2 = data.frame(x=min(heapeddat()[[1]]):max(heapeddat()[[1]]),
                            y=heapeddatmle())
          
          g1 = ggplot() + geom_histogram(data=dat1, aes(x=x,y=..density..),fill="white",color="darkred",binwidth=1) +
            xlab("Rounded values") + ylab("Relative frequency") + ggtitle("Histogram of rounded values with\nestimated distribution overlayed") +
            theme_bw() + geom_point(data=dat2, aes(x=x,y=y), size=3, shape=1, color="darkseagreen4") + 
            scale_x_continuous(breaks=seq(min(dat1$x),max(dat1$x),by=5)) 
          
          return(g1)
        }else if(!input$dataupload)
        {
          dat1 = data.frame(x=heapeddat()[[1]])
          dat2 = data.frame(x=min(heapeddat()[[1]]):max(heapeddat()[[1]]),
                            y=heapeddatmle())
          
          g1 = ggplot() + geom_histogram(data=dat1, aes(x=x,y=..density..),fill="white",color="darkred",binwidth=1) +
            xlab("Rounded values") + ylab("Relative frequency") + ggtitle("Histogram of rounded values with\nestimated distribution overlayed") +
            theme_bw() + geom_point(data=dat2, aes(x=x,y=y), size=3, shape=1, color="darkseagreen4") + 
            scale_x_continuous(breaks=seq(min(dat1$x),max(dat1$x),by=5)) 
          
          dat3 = data.frame(x=heapeddat()[[1]])
          dat4 = data.frame(x=min(heapeddat()[[1]]):max(heapeddat()[[1]]),
                            y=heapeddat()[[2]])
          
          g2 = ggplot() + geom_histogram(data=dat3, aes(x=x,y=..density..),fill="white",color="darkred",binwidth=1) +
            xlab("Rounded values") + ylab("Relative frequency") + ggtitle("Histogram of rounded values with\nunderlying distribution overlayed") +
            theme_bw() + geom_point(data=dat4, aes(x=x,y=y), size=3, shape=1) + 
            scale_x_continuous(breaks=seq(min(dat3$x),max(dat3$x),by=5)) 
          
          graph = arrangeGrob(g1,g2,nrow=1,ncol=2)
          return(graph)
        }
      }
    })
  })
  
  output$loglikvalsplot = renderPlot({
    input$startoptim
    isolate({
      if(input$startoptim>0)
      {
        loglik.dat = data.frame(lk=1:length(mleoptim()[[1]]), ll=mleoptim()[[1]])
        ldat = data.frame(x=which.max(mleoptim()[[1]]), y=max(mleoptim()[[1]]))
        
        g1 = ggplot() + geom_point(data=loglik.dat, aes(x=lk,y=ll), shape=19, size=3) + 
              geom_point(data=ldat, aes(x=x,y=y), color="tomato", size=5) +
              xlab("Iteration number") + ylab("Loglikelihood value") + theme_bw() +
              ggtitle(paste("Scatterplot of loglikelihood value\nversus iteration number ",
                            "(", length(mleoptim()[[1]]), " iterations)", sep=""))
        return(g1)
      }
    })
  })
  
  output$loglikvalspop = renderUI({
    if(input$startoptim>0)
      return(bsPopover("loglikvalsplot","Optimization process","The iteration corresponding to the red point yielded the set of estimates that maximized the loglikelihood function.  Thus, the set of estimates in the above table are called the maxmimum likelihood estimators.",
                       trigger="hover",placement="left"))
  })
  
  output$mlevalstable = renderTable({
    input$startoptim
    isolate({
      if (input$startoptim>0)
      {
        if(input$dataupload)
        {
          if (input$distdat1=="Negative Binomial") 
          {
            tab = matrix(nrow=1, ncol=4)
            rownames(tab) = c("MLE")
            colnames(tab) = c(HTML("&alpha;<sub>1</sub>"), HTML("&alpha;<sub>2</sub>"), "r", "&mu;")
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2))
            tab
          } else if(input$distdat1=="Beta Binomial")
          {
            tab = matrix(nrow=1, ncol=5)
            rownames(tab) = c("MLE")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "Pr", "&rho;")
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2),
                        round(mles()[5], digits=2))
            tab
          } else if(input$distdat1=="Conway-Maxwell Poisson")
          {
            tab = matrix(nrow=1, ncol=4)
            rownames(tab) = c("MLE")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&lambda;", "&nu;")
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2))
            tab
          } else if(input$distdat1=="Delaporte")
          {
            tab = matrix(nrow=1, ncol=5)
            rownames(tab) = c("MLE")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>","&alpha;", "&beta;", "&lambda;")
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2),
                        round(mles()[5], digits=2))
            tab
          } else if(input$distdat1=="Yule-Simon")
          {
            tab = matrix(nrow=1, ncol=3)
            rownames(tab) = c("MLE")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&rho;")
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2))
            tab
          }
        }else if(!input$dataupload)
        {
          if (input$distdat=="Negative Binomial") 
          {
            tab = matrix(nrow=2, ncol=4)
            rownames(tab) = c("MLE","Underlying")
            colnames(tab) = c(HTML("&alpha;<sub>1</sub>"), HTML("&alpha;<sub>2</sub>"), "r", "&mu;")
            tab[2,] = c(input$alpha1,input$alpha2,input$size.dat,input$mu.dat)
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2))
            tab
          } else if(input$distdat=="Beta Binomial")
          {
            tab = matrix(nrow=2, ncol=5)
            rownames(tab) = c("MLE","Underlying")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "Pr", "&rho;")
            tab[2,] = c(input$alpha1,input$alpha2,input$rbb.dat,input$pbb.dat,input$corbb.dat)
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2),
                        round(mles()[5], digits=2))
            tab
          } else if(input$distdat=="Conway-Maxwell Poisson")
          {
            tab = matrix(nrow=2, ncol=4)
            rownames(tab) = c("MLE","Underlying")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&lambda;", "&nu;")
            tab[2,] = c(input$alpha1,input$alpha2,input$lam.dat,input$nu.dat)
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2))
            tab
          } else if(input$distdat=="Delaporte")
          {
            tab = matrix(nrow=2, ncol=5)
            rownames(tab) = c("MLE","Underlying")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>","&alpha;", "&beta;", "&lambda;")
            tab[2,] = c(input$alpha1,input$alpha2,input$adel.dat,input$bdel.dat,input$cdel.dat)
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2),
                        round(mles()[4], digits=2),
                        round(mles()[5], digits=2))
            tab
          } else if(input$distdat=="Yule-Simon")
          {
            tab = matrix(nrow=2, ncol=3)
            rownames(tab) = c("MLE","Underlying")
            colnames(tab) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&rho;")
            tab[2,] = c(input$alpha1,input$alpha2,input$rho.dat)
            tab[1,] = c(round(mles()[1], digits=2),
                        round(mles()[2], digits=2),
                        round(mles()[3], digits=2))
            tab
          }
        }
      }
    })
  }, sanitize.text.function = function(x) x)
  
  #################################################################
  #################################################################
  ## Confidence Intervals
  #################################################################
  #################################################################
  
  # g function
  g = function(params,dist)
  {
    if(dist=="Negative Binomial")
      return(c(expit(params[1]),expit(params[2]),exp(params[3]),exp(params[4])))
    else if(dist=="Beta Binomial")
      return(c(expit(params[1]),expit(params[2]),exp(params[3]),expit(params[4]),expit(params[5])))
    else if(dist=="Conway-Maxwell Poisson")
      return(c(expit(params[1]),expit(params[2]),exp(params[3]),exp(params[4])))
    else if(dist=="Delaporte")
      return(c(expit(params[1]),expit(params[2]),exp(params[3]),exp(params[4]),exp(params[5])))
    else if(dist=="Yule-Simon")
      return(c(expit(params[1]),expit(params[2]),exp(params[3])))
  }
  
  # grad.g function
  grad.g = function(params,dist)
  {
    if(dist=="Negative Binomial")
    {
      mat = matrix(0, nrow=4, ncol=4)
      diag(mat) = c((exp(params[1])/(1+exp(params[1]))^2),(exp(params[2])/(1+exp(params[2]))^2),exp(params[3]),exp(params[4]))
      return(mat)
    } else if(dist=="Beta Binomial")
    {
      mat = matrix(0, nrow=5, ncol=5)
      diag(mat) = c((exp(params[1])/(1+exp(params[1]))^2),(exp(params[2])/(1+exp(params[2]))^2),exp(params[3]),
                    (exp(params[4])/(1+exp(params[4]))^2),(exp(params[5])/(1+exp(params[5]))^2))
      return(mat)
    } else if(dist=="Conway-Maxwell Poisson")
    {
      mat = matrix(0, nrow=4, ncol=4)
      diag(mat) = c((exp(params[1])/(1+exp(params[1]))^2),(exp(params[2])/(1+exp(params[2]))^2),exp(params[3]),exp(params[4]))
      return(mat)
    } else if(dist=="Delaporte")
    {
      mat = matrix(0, nrow=5, ncol=5)
      diag(mat) = c((exp(params[1])/(1+exp(params[1]))^2),(exp(params[2])/(1+exp(params[2]))^2),
                    exp(params[3]),exp(params[4]),exp(params[5]))
      return(mat)
    } else if(dist=="Yule-Simon")
    {
      mat = matrix(0, nrow=3, ncol=3)
      diag(mat) = c((exp(params[1])/(1+exp(params[1]))^2),(exp(params[2])/(1+exp(params[2]))^2),exp(params[3]))
      return(mat)
    }
  }
  
  info.mat = reactive({
    input$startci
    isolate({
      if(input$startci>0)
      {
        if(input$dataupload)
        {
          if(input$distdat1=="Negative Binomial")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.nb, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat1=="Beta Binomial")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),logit(mles()[4]),logit(mles()[5])), loglik.bb, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat1=="Conway-Maxwell Poisson")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.cmp, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat1=="Delaporte")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4]),log(mles()[5])), loglik.del, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat1=="Yule-Simon")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3])), loglik.ys, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          }
          
          # obtaining the Fisher info matrix
          info.mat = -mm$hessian 
          
          return(list(info.mat,mm))
        } else if(!input$dataupload)
        {
          if(input$distdat=="Negative Binomial")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.nb, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat=="Beta Binomial")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),logit(mles()[4]),logit(mles()[5])), loglik.bb, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat=="Conway-Maxwell Poisson")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.cmp, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat=="Delaporte")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4]),log(mles()[5])), loglik.del, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          } else if(input$distdat=="Yule-Simon")
          {
            mm = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3])), loglik.ys, 
                       degree=heapeddat()[[1]],control=list(fnscale=-1,maxit=10000), hessian=TRUE)
          }
          # obtaining the Fisher info matrix
          info.mat = -mm$hessian
          
          return(list(info.mat,mm))
        }        
      }
    })
  })
  
  output$invert = renderUI({
    input$startci
    isolate({
      if(input$startci>0)
      {
        if(det(info.mat()[[1]])==0)
        {
          return(code("Fisher information matrix is not invertible.  Need to calculate CI's with bootstrapping."))
        } 
      }
    })
  })
  
  output$ci = renderTable({
    input$startci
    isolate({
      if(input$startci>0)
      {
        if(input$dataupload & det(info.mat()[[1]])!=0)
        {
          cov.mat = solve(info.mat()[[1]])
          se.vec = sqrt(diag(cov.mat))
          
          # transformed estimates
          t.est = g(info.mat()[[2]]$par,dist=input$distdat1)
          
          # matrix
          mat1 = grad.g(info.mat()[[2]]$par,dist=input$distdat1)
          
          # covariance matrix
          cov.mat1 = t(mat1)%*%cov.mat%*%mat1
          
          # obtaining standard errors
          se = sqrt(diag(cov.mat1))
          
          # computing 95% CI's
          ci=matrix(nrow=length(t.est),ncol=3)
          i=0
          while(i<length(t.est))
          {
            i=i+1
            ci[i,] = c(t.est[i], t.est[i]-2*se[i],t.est[i]+2*se[i])
            
            if(ci[i,2]<0) ci[i,2]=0
            if(i==1 | i==2)
            {
              if(ci[i,3]>1) ci[i,3]=1
            }
          }
          
          colnames(ci) = c("midpoint", "95% lower bound", "95% upper bound")
          
          if(input$distdat1=="Negative Binomial")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "&mu;")
          else if(input$distdat1=="Beta Binomial")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "Pr","&rho;")
          else if(input$distdat1=="Conway-Maxwell Poisson")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&lambda;", "&nu;")
          else if(input$distdat1=="Delaporte")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&alpha;", "&beta;", "&lambda;")
          else if(input$distdat1=="Yule-Simon")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&rho;")
          
          return(ci)
          
        }else if(!input$dataupload & det(info.mat()[[1]])!=0)
        {
          cov.mat = solve(info.mat()[[1]])
          se.vec = sqrt(diag(cov.mat))
          
          # transformed estimates
          t.est = g(info.mat()[[2]]$par,dist=input$distdat)
          
          # matrix
          mat1 = grad.g(info.mat()[[2]]$par,dist=input$distdat)
          
          # covariance matrix
          cov.mat1 = t(mat1)%*%cov.mat%*%mat1
          
          # obtaining standard errors
          se = sqrt(diag(cov.mat1))
          
          # computing 95% CI's
          ci=matrix(nrow=length(t.est),ncol=3)
          i=0
          while(i<length(t.est))
          {
            i=i+1
            ci[i,] = c(t.est[i], t.est[i]-2*se[i],t.est[i]+2*se[i])
            
            if(ci[i,2]<0) ci[i,2]=0
            if(i==1 | i==2)
            {
              if(ci[i,3]>1) ci[i,3]=1
            }
          }
          
          colnames(ci) = c("Midpoint", "95% Lower bound", "95% Upper bound")
          
          if(input$distdat=="Negative Binomial")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "&mu;")
          else if(input$distdat=="Beta Binomial")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "Pr","&rho;")
          else if(input$distdat=="Conway-Maxwell Poisson")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&lambda;", "&nu;")
          else if(input$distdat=="Delaporte")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&alpha;", "&beta;", "&lambda;")
          else if(input$distdat=="Yule-Simon")
            rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&rho;")
          
          return(ci)
        }        
      }
    })
  }, sanitize.text.function = function(x) x)
  
  #################################################################
  #################################################################
  ## Bootstrapped Confidence Intervals
  #################################################################
  #################################################################
  
  createAlert(session, inputId = "boottime",
              message = "Please be patient as the bootstrapping process may take a few minutes depending on the specified distribution and number of parameters in the model.",
              type = "warning", dismiss = TRUE, block = FALSE,append = TRUE)
  
  boot.est = reactive({
    input$startciboot
    withProgress(session, {
      setProgress(message = "Bootstrapping...",value=1)
      isolate({
        if(input$startciboot>0)
        {
          if(input$dataupload)
          {
            if(input$distdat1=="Negative Binomial")
            {
              bootlist=matrix(nrow=500,ncol=4)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.nb, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3:4]))
              }
              return(bootlist)
            } else if(input$distdat1=="Beta Binomial")
            {
              bootlist=matrix(nrow=500,ncol=5)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),logit(mles()[4]),logit(mles()[5])), loglik.bb, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3]),expit(res$par[4:5]))
              }
              return(bootlist)
            } else if(input$distdat1=="Conway-Maxwell Poisson")
            {
              bootlist=matrix(nrow=500,ncol=4)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.cmp, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3:4]))
              }
              return(bootlist)
            } else if(input$distdat1=="Delaporte")
            {
              bootlist=matrix(nrow=500,ncol=5)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4]),log(mles()[5])), loglik.del, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3:5]))
              }
              return(bootlist)
            } else if(input$distdat1=="Yule-Simon")
            {
              bootlist=matrix(nrow=500,ncol=3)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3])), loglik.ys, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3]))
              }
              return(bootlist)
            }
          } else if(!input$dataupload)
          {
            if(input$distdat=="Negative Binomial")
            {
              bootlist=matrix(nrow=500,ncol=4)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.nb, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3:4]))
              }
              return(bootlist)
            } else if(input$distdat=="Beta Binomial")
            {
              bootlist=matrix(nrow=500,ncol=5)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),logit(mles()[4]),logit(mles()[5])), loglik.bb, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3]),expit(res$par[4:5]))
              }
              return(bootlist)
            } else if(input$distdat=="Conway-Maxwell Poisson")
            {
              bootlist=matrix(nrow=500,ncol=4)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4])), loglik.cmp, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3:4]))
              }
              return(bootlist)
            } else if(input$distdat=="Delaporte")
            {
              bootlist=matrix(nrow=500,ncol=5)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3]),log(mles()[4]),log(mles()[5])), loglik.del, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3:5]))
              }
              return(bootlist)
            } else if(input$distdat=="Yule-Simon")
            {
              bootlist=matrix(nrow=500,ncol=3)
              i=0
              while(i<500)
              {
                i=i+1
                new.dat = sample(heapeddat()[[1]],size=length(heapeddat()[[1]]),replace=TRUE)
                res = optim(par=c(logit(mles()[1]),logit(mles()[2]),log(mles()[3])), loglik.ys, 
                            degree=new.dat,control=list(fnscale=-1,maxit=10000))
                bootlist[i,] = c(expit(res$par[1:2]),exp(res$par[3]))
              }
              return(bootlist)
            }
          }
        }
      })
    })
  })
  
  output$bootdist = renderPlot({
    input$startciboot
    isolate({
      if(input$startciboot>0)
      {
        cols = c("aquamarine3", "aquamarine3", "darksalmon", "darksalmon","darksalmon")
        
        if((!input$dataupload & input$distdat=="Negative Binomial") | (input$dataupload & input$distdat1=="Negative Binomial"))
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(r)^MLE), bquote(hat(mu)^MLE))
        else if((!input$dataupload & input$distdat=="Beta Binomial") | (input$dataupload & input$distdat1=="Beta Binomial"))
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(r)^MLE), 
                    bquote(hat(Pr)^MLE), bquote(hat(rho)^MLE))
        else if((!input$dataupload & input$distdat=="Conway-Maxwell Poisson") | (input$dataupload & input$distdat1=="Conway-Maxwell Poisson"))
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(lambda)^MLE), bquote(hat(nu)^MLE))
        else if((!input$dataupload & input$distdat=="Delaporte") | (input$dataupload & input$distdat1=="Delaporte"))
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(alpha)^MLE), 
                    bquote(hat(beta)^MLE), bquote(hat(lambda)^MLE))
        else if((!input$dataupload & input$distdat=="Yule-Simon") | (input$dataupload & input$distdat1=="Yule-Simon"))
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(rho)^MLE))
        
        if(!input$dataupload & input$distdat=="Negative Binomial")
          true = c(input$alpha1,input$alpha2,input$size.dat,input$mu.dat)
        else if(!input$dataupload & input$distdat=="Beta Binomial")
          true = c(input$alpha1,input$alpha2,input$rbb.dat,input$pbb.dat,input$corbb.dat)
        else if(!input$dataupload & input$distdat=="Conway-Maxwell Poisson")
          true = c(input$alpha1,input$alpha2,input$lam.dat,input$nu.dat)
        else if(!input$dataupload & input$distdat=="Delaporte")
          true = c(input$alpha1,input$alpha2,input$adel.dat,input$bdel.dat,input$cdel.dat)
        else if(!input$dataupload & input$distdat=="Yule-Simon")
          true = c(input$alpha1,input$alpha2,input$rho.dat)

        plot=list()
        i=0
        while(i<dim(boot.est())[2])
        {
          i=i+1
          boot.dat = data.frame(x = boot.est()[,i])
          plot[[i]] = ggplot(boot.dat) + geom_histogram(aes(x=x), fill=cols[i], alpha=.7) +
                        xlab("") + ylab("") + theme_bw() +
                        geom_vline(aes(xintercept=quantile(x,.025)), color="gray") + 
                        geom_vline(aes(xintercept=quantile(x,.975)), color="gray") +
                        ggtitle(substitute("Bootstrap distribution of" ~ name,list(name=names[[i]])))
          
          if(!input$dataupload)
            plot[[i]] = plot[[i]] + geom_vline(xintercept=true[i],color="navy")
        }
        
        if(dim(boot.est())[2]==3)
          graph = arrangeGrob(plot[[1]], plot[[2]], plot[[3]], ncol=2)
        else if(dim(boot.est())[2]==4)
          graph = arrangeGrob(plot[[1]], plot[[2]], plot[[3]], plot[[4]], ncol=2)
        else if(dim(boot.est())[2]==5)
          graph = arrangeGrob(plot[[1]], plot[[2]], plot[[3]], plot[[4]], plot[[5]], ncol=2)
        
        return(graph)
      }
    })
  })
  
  output$bootdistpop = renderUI({
    if(input$startciboot>0 & !input$dataupload)
      return(bsPopover("bootdist","Information","In each of the bootstrap distributions, the underlying parameter is shown by the navy line while the 2.5 and 97.5 percentiles are shown by the gray lines.",
                       trigger="hover",placement="left"))
    else if(input$startciboot>0 & input$dataupload)
      return(bsPopover("bootdist","Information","In each of the bootstrap distributions, the 2.5 and 97.5 percentiles are shown by the gray lines.",
                       trigger="hover",placement="left"))
  })
    
  output$boottable = renderTable({
    input$startciboot
    isolate({
      if(input$startciboot>0)
      {
        ci=matrix(nrow=dim(boot.est())[2],ncol=3)
        i=0
        while(i<dim(boot.est())[2])
        {
          i=i+1
          ci[i,] = c(mean(mles()[i]), quantile(boot.est()[,i],.025),quantile(boot.est()[,i],.975))
          
          if(ci[i,2]<0) ci[i,2]=0
          if(i==1 | i==2)
          {
            if(ci[i,3]>1) ci[i,3]=1
          }
        }
        
        colnames(ci) = c("Mean", "2.5 Percentile", "97.5 Percentile")
        
        if((!input$dataupload & input$distdat=="Negative Binomial") | (input$dataupload & input$distdat1=="Negative Binomial"))
          rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "&mu;")
        else if((!input$dataupload & input$distdat=="Beta Binomial") | (input$dataupload & input$distdat1=="Beta Binomial"))
          rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "Pr","&rho;")
        else if((!input$dataupload & input$distdat=="Conway-Maxwell Poisson") | (input$dataupload & input$distdat1=="Conway-Maxwell Poisson"))
          rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&lambda;", "&nu;")
        else if((!input$dataupload & input$distdat=="Delaporte") | (input$dataupload & input$distdat1=="Delaporte"))
          rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&alpha;", "&beta;", "&lambda;")
        else if((!input$dataupload & input$distdat=="Yule-Simon") | (input$dataupload & input$distdat1=="Yule-Simon"))
          rownames(ci) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&rho;")
        
        return(ci)
      }
    })
  }, sanitize.text.function = function(x) x)
  
  ##############################################
  ##############################################
  ## Method Validation Panel
  ##############################################
  ##############################################
  
  createAlert(session, inputId = "simtime",
              message = "Please be patient as the simulation process may take a few minutes depending on the specified distribution and number of simulations.",
              type = "warning", dismiss = TRUE, block = FALSE,append = TRUE)
  
  sim.mles = reactive({
    input$startvalid
    withProgress(session, {
      setProgress(message = "Running simulations...", value=1)
      isolate({
        if(input$startvalid>0)
        {
          if(input$dataupload)
          {
            if(input$distdat1=="Negative Binomial")
            {
              nb.sim(n=input$nsim.mv, a1=mles()[1], a2=mles()[2], size=round(mles()[3]), mu=mles()[4], 
                     sample.size=input$nsim.dat)
            } else if(input$distdat1=="Beta Binomial") 
            {
              bb.sim(n=input$nsim.mv, a1=mles()[1], a2=mles()[2], size=round(mles()[3]), prob=mles()[4],
                     rho=mles()[5], sample.size=input$nsim.dat)
            } else if(input$distdat1=="Conway-Maxwell Poisson")
            {
              cmp.sim(n=input$nsim.mv, a1=mles()[1], a2=mles()[2], lam=mles()[3], nu=mles()[4],
                      sample.size=input$nsim.dat)
            } else if(input$distdat1=="Delaporte")
            {
              del.sim(n=input$nsim.mv, a1=mles()[1], a2=mles()[2], alpha=mles()[3], beta=mles()[4],
                      lambda=mles()[5], sample.size=input$nsim.dat)
            } else if(input$distdat1=="Yule-Simon")
            {
              ys.sim(n=input$nsim.mv, a1=mles()[1], a2=mles()[2], rho=mles()[3], 
                     sample.size=input$nsim.dat)
            }
          } else if(!input$dataupload)
          {
            if(input$distdat=="Negative Binomial")
            {
              nb.sim(n=input$nsim.mv, a1=input$alpha1, a2=input$alpha2, size=input$size.dat, mu=input$mu.dat, 
                     sample.size=input$nsim.dat)
            } else if(input$distdat=="Beta Binomial") 
            {
              bb.sim(n=input$nsim.mv, a1=input$alpha1, a2=input$alpha2, size=input$rbb.dat, prob=input$pbb.dat,
                     rho=input$corbb.dat, sample.size=input$nsim.dat)
            } else if(input$distdat=="Conway-Maxwell Poisson")
            {
              cmp.sim(n=input$nsim.mv, a1=input$alpha1, a2=input$alpha2, lam=input$lam.dat, nu=input$nu.dat,
                      sample.size=input$nsim.dat)
            } else if(input$distdat=="Delaporte")
            {
              del.sim(n=input$nsim.mv, a1=input$alpha1, a2=input$alpha2, alpha=input$adel.dat, beta=input$bdel.dat,
                      lambda=input$cdel.dat, sample.size=input$nsim.dat)
            } else if(input$distdat=="Yule-Simon")
            {
              ys.sim(n=input$nsim.mv, a1=input$alpha1, a2=input$alpha2, rho=input$rho.dat, 
                     sample.size=input$nsim.dat)
            }
          }
        }
      })
    })
  })
  
  output$simplots = renderPlot({
    input$startvalid
    isolate({
      if(input$startvalid>0)
      {
        cols = c("aquamarine3", "aquamarine3", "darksalmon", "darksalmon", "darksalmon")
        
        if(input$dataupload)
        {
          true=mles()
        } else if(!input$dataupload)
        {
          if(input$distdat=="Negative Binomial")
          {
            true=c(input$alpha1,input$alpha2,input$size.dat,input$mu.dat)
          } else if(input$distdat=="Beta Binomial") 
          {
            true=c(input$alpha1,input$alpha2,input$rbb.dat,input$pbb.dat,input$corbb.dat)
          } else if(input$distdat=="Conway-Maxwell Poisson")
          {
            true=c(input$alpha1,input$alpha2,input$lam.dat,input$nu.dat)
          } else if(input$distdat=="Delaporte")
          {
            true=c(input$alpha1,input$alpha2,input$adel.dat,input$bdel.dat,input$cdel.dat)
          } else if(input$distdat=="Yule-Simon")
          {
            true=c(input$alpha1,input$alpha2,input$rho.dat)
          }
        }
        
        if((!input$dataupload & input$distdat=="Negative Binomial") | (input$dataupload & input$distdat1=="Negative Binomial"))
        {
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(r)^MLE), bquote(hat(mu)^MLE))
          plot=list()
          i=0
          while(i<4)
          {
            i=i+1
            sim.dat = data.frame(x = sim.mles()[,i])
            plot[[i]] = ggplot(sim.dat) + geom_histogram(aes(x=x), fill=cols[i], alpha=.7) + xlab("") + ylab("") + 
                          ggtitle(substitute("Simulated distribution of"~name,list(name=names[[i]]))) + theme_bw() +
                          geom_vline(xintercept=true[i],color="navy") + geom_vline(aes(xintercept=mean(x)),color="gold2")
          }
          print(arrangeGrob(plot[[1]], plot[[2]], plot[[3]], plot[[4]], ncol=2))
        } else if((!input$dataupload & input$distdat=="Beta Binomial") | (input$dataupload & input$distdat1=="Beta Binomial"))
        {
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(r)^MLE), 
                    bquote(hat(Pr)^MLE), bquote(hat(rho)^MLE))
          plot=list()
          i=0
          while(i<5)
          {
            i=i+1
            sim.dat = data.frame(x = sim.mles()[,i])
            plot[[i]] = ggplot(sim.dat) + geom_histogram(aes(x=x), fill=cols[i], alpha=.7) + xlab("") + ylab("") + 
                          ggtitle(substitute("Simulated distribution of"~name,list(name=names[[i]]))) + theme_bw() +
                          geom_vline(xintercept=true[i],color="navy") + geom_vline(aes(xintercept=mean(x)),color="gold2")
          }
          print(arrangeGrob(plot[[1]], plot[[2]], plot[[3]], plot[[4]], plot[[5]], ncol=2))
        } else if((!input$dataupload & input$distdat=="Conway-Maxwell Poisson") | (input$dataupload & input$distdat1=="Conway-Maxwell Poisson"))
        {
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(lambda)^MLE), bquote(hat(nu)^MLE))
          plot=list()
          i=0
          while(i<4)
          {
            i=i+1
            sim.dat = data.frame(x = sim.mles()[,i])
            plot[[i]] = ggplot(sim.dat) + geom_histogram(aes(x=x), fill=cols[i], alpha=.7) + xlab("") + ylab("") + 
                          ggtitle(substitute("Simulated distribution of"~name,list(name=names[[i]]))) + theme_bw() +
                          geom_vline(xintercept=true[i],color="navy") + geom_vline(aes(xintercept=mean(x)),color="gold2")
          }
          print(arrangeGrob(plot[[1]], plot[[2]], plot[[3]], plot[[4]], ncol=2))
        } else if((!input$dataupload & input$distdat=="Delaporte") | (input$dataupload & input$distdat1=="Delaporte"))
        {
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(alpha)^MLE), 
                    bquote(hat(beta)^MLE), bquote(hat(lambda)^MLE))
          plot=list()
          i=0
          while(i<5)
          {
            i=i+1
            sim.dat = data.frame(x = sim.mles()[,i])
            plot[[i]] = ggplot(sim.dat) + geom_histogram(aes(x=x), fill=cols[i], alpha=.7) + xlab("") + ylab("") + 
                          ggtitle(substitute("Simulated distribution of"~name,list(name=names[[i]]))) + theme_bw() +
                          geom_vline(xintercept=true[i],color="navy") + geom_vline(aes(xintercept=mean(x)),color="gold2")
          }
          print(arrangeGrob(plot[[1]], plot[[2]], plot[[3]], plot[[4]], plot[[5]], ncol=2))
        } else if((!input$dataupload & input$distdat=="Yule-Simon") | (input$dataupload & input$distdat1=="Yule-Simon"))
        {
          names = c(bquote(hat(alpha)[1]^MLE), bquote(hat(alpha)[2]^MLE), bquote(hat(rho)^MLE))
          plot=list()
          i=0
          while(i<3)
          {
            i=i+1
            sim.dat = data.frame(x = sim.mles()[,i])
            plot[[i]] = ggplot(sim.dat) + geom_histogram(aes(x=x), fill=cols[i], alpha=.7) + xlab("") + ylab("") + 
                          ggtitle(substitute("Simulated distribution of"~name,list(name=names[[i]]))) + theme_bw() +
                          geom_vline(xintercept=true[i],color="navy") + geom_vline(aes(xintercept=mean(x)),color="gold2")
          }
          print(arrangeGrob(plot[[1]], plot[[2]], plot[[3]], ncol=2))
        }
      }
    })
  })
  
  output$plots.sim.pop = renderUI({
    if(input$startvalid>0)
      return(bsPopover("simplots","Information","In each of the simulated distributions, the underlying parameter is shown by the navy line while the mean is shown by the yellow line.",
                       trigger="hover",placement="left"))
  })
  
  output$means.sim.tab = renderTable({
    input$startvalid
    isolate({
      if(input$startvalid>0)
      {
        if(input$dataupload)
        {
          if(input$distdat1=="Negative Binomial") 
          {
            sim.mat = matrix(nrow=2, ncol=4)
            sim.mat[2,] = c(mles()[1], mles()[2], mles()[3], mles()[4])
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "&mu;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat1=="Beta Binomial")
          {
            sim.mat = matrix(nrow=2, ncol=5)
            sim.mat[2,] = c(mles()[1], mles()[2], mles()[3], mles()[4], mles()[5])
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]), mean(sim.mles()[,5]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "Pr", "&rho;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat1=="Conway-Maxwell Poisson")
          {
            sim.mat = matrix(nrow=2, ncol=4)
            sim.mat[2,] = c(mles()[1], mles()[2], mles()[3], mles()[4])
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&lambda;", "&nu;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat1=="Delaporte")
          {
            sim.mat = matrix(nrow=2, ncol=5)
            sim.mat[2,] = c(mles()[1], mles()[2], mles()[3], mles()[4], mles()[5])
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]), mean(sim.mles()[,5]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&alpha;", "&beta;", "&lambda;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat1=="Yule-Simon")
          {
            sim.mat = matrix(nrow=2, ncol=3)
            sim.mat[2,] = c(mles()[1], mles()[2], mles()[3])
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&rho;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          }
        }else if(!input$dataupload)
        {
          if(input$distdat=="Negative Binomial") 
          {
            sim.mat = matrix(nrow=2, ncol=4)
            sim.mat[2,] = c(input$alpha1, input$alpha2, input$size.dat, input$mu.dat)
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "&mu;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat=="Beta Binomial")
          {
            sim.mat = matrix(nrow=2, ncol=5)
            sim.mat[2,] = c(input$alpha1, input$alpha2, input$rbb.dat, input$pbb.dat, input$corbb.dat)
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]), mean(sim.mles()[,5]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "r", "Pr", "&rho;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat=="Conway-Maxwell Poisson")
          {
            sim.mat = matrix(nrow=2, ncol=4)
            sim.mat[2,] = c(input$alpha1, input$alpha2, input$lam.dat, input$nu.dat)
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&lambda;", "&nu;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat=="Delaporte")
          {
            sim.mat = matrix(nrow=2, ncol=5)
            sim.mat[2,] = c(input$alpha1, input$alpha2, input$adel.dat, input$bdel.dat, input$cdel.dat)
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]), mean(sim.mles()[,4]), mean(sim.mles()[,5]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&alpha;", "&beta;", "&lambda;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          } else if(input$distdat=="Yule-Simon")
          {
            sim.mat = matrix(nrow=2, ncol=3)
            sim.mat[2,] = c(input$alpha1, input$alpha2, input$rho.dat)
            sim.mat[1,] = c(mean(sim.mles()[,1]), mean(sim.mles()[,2]), mean(sim.mles()[,3]))
            colnames(sim.mat) = c("&alpha;<sub>1</sub>", "&alpha;<sub>2</sub>", "&rho;")
            rownames(sim.mat) = c("Mean","Underlying")
            return(sim.mat)
          }
        }
      }
    })
  }, sanitize.text.function = function(x) x)
  
})