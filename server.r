############################################################################################################
############################################################################################################
## Loading necessary libraries
############################################################################################################
############################################################################################################

library(ggplot2)
library(gridExtra)
library(lme4)
library(lmerTest)
library(robustlmm)
library(MCMCglmm)
library(xtable)
library(shinyBS)
library(nlme)
library(combinat)

############################################################################################################
############################################################################################################
## Global functions
############################################################################################################
############################################################################################################

ggCaterpillar=function(re, QQ=TRUE, likeDotplot=TRUE) 
{
  require(ggplot2)
  f = function(x) {
    pv=attr(x, "postVar")
    cols=1:(dim(pv)[1])
    se=unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord=unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf=data.frame(y=unlist(x)[ord],ci=1.96*se[ord],nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                   ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]), ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  
      p = ggplot(pDf, aes(nQQ, y))
      p = p + facet_wrap(~ ind, scales="free")
      p = p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else { 
      p = ggplot(pDf, aes(ID, y)) + coord_flip() + ggtitle("Caterpillar plot showing the group-level\nerror terms for each random effect") 
      if(likeDotplot) {  
        p = p + facet_wrap(~ ind)
      } else {           
        p = p + facet_grid(ind ~ ., scales="free_y")
      }
      p = p + xlab("Groups") + ylab("Deviations")
    }
    
    p = p + theme_bw() + theme(legend.position="none") + geom_hline(yintercept=0) + 
      geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black") + geom_point(aes(size=2), shape=1) 
    
    return(p)
  }
  lapply(re, f)
}

############################################################################################################
############################################################################################################
## Loading sample data sets
############################################################################################################
############################################################################################################

kentucky=read.csv("data/KentuckyMathScores.csv")
music=read.csv("data/musicdata.csv")

############################################################################################################
############################################################################################################
## Shiny server
############################################################################################################
############################################################################################################

shinyServer(function(input, output, session) {
  
  ############################################################################################################
  ############################################################################################################
  ## Upload Data Panel
  ############################################################################################################
  ############################################################################################################
  
  data = reactive({
    if(is.null(input$file) & !input$usesample) 
    {
      return(NULL)
    } else if(!is.null(input$file) & !input$usesample)
    {
      file = read.csv(input$file$datapath, header=input$header, sep=input$sep, quote=input$quote)
      return(file)
    } else if(input$usesample)
    {
      return(music)
    }
    })
  
  output$datatable = renderDataTable({
    if(!input$usesample)
    {
      return(data())
    }
  })
  
  output$sampledata = renderDataTable({
    data()
  })
  
  output$selectresponse = renderUI({ 
    selectInput("response","Response variable:",choices=names(data()),selectize=TRUE)  
  })
  
  output$selectlevel1fixedpred = renderUI({
    selectInput("level1.fixed.pred","Level 1 fixed predictors:",choices=names(data()),multiple=TRUE,selectize=TRUE)
  })
  
  output$selectlevel1randpred = renderUI({
    selectInput("level1.rand.pred","Level 1 random predictors:",choices=names(data()),multiple=TRUE,selectize=TRUE)
  })
  
  output$selectlevel2id = renderUI({
    selectInput("level2.id","Level 2 observational unit:",choices=names(data()),selectize=TRUE)
  })
  
  output$selectlevel2pred = renderUI({
    selectInput("level2.pred","Level 2 predictors",choices=names(data()),multiple=TRUE,selectize=TRUE)
  })
  
  ############################################################################################################
  ############################################################################################################
  ## HLM Study Panel
  ############################################################################################################
  ############################################################################################################
  
  ############################################################################################################
  ############################################################################################################
  ## Varying-Intercept Only
  ############################################################################################################
  ############################################################################################################
  
  output$selectresponse1 = renderUI({ 
    selectInput("response1","Response variable:",choices=names(data()),selectize=TRUE)  
  })
  
  output$selectlevel2id1 = renderUI({
    selectInput("level2.id1","Level 2 observational unit:",choices=names(data()),selectize=TRUE)
  })
  
  dat.nopred = reactive({
     if(!input$usesample)
     {
       dat = data.frame(response = data()[,paste(input$response1)],
                        level2.id = factor(data()[,paste(input$level2.id1)]))
       return(dat)
     } else if (input$usesample)
    {
      dat = data.frame(response = data()$negative_affect,
                       level2.id = factor(data()$id))
    }
  })
  
  output$datatab1 = renderDataTable({
    if(!input$usesample) return(dat.nopred())
  })
  
  label = reactive({
    if(!input$usesample)
    {
      return(c(paste(input$response1),paste(input$level2.id1)))
    }else if(input$usesample)
    {
      return(c("negative affect","musicians"))
    }
  })
  
  ############################################################################################################
  ## Pooled Regression 1
  ############################################################################################################
    
  output$poolednopredgraph = renderPlot({
    ggplot(data=dat.nopred()) + geom_histogram(aes(x=response),fill="white",color="navy") + 
      xlab(paste(label()[1])) + ylab("Frequency") + ggtitle(paste("Histogram of",label()[1],"with pooled mean imposed")) + 
      theme_bw() + geom_vline(aes(xintercept=mean(response)),linetype="dashed",color="navy")
  })
  
  pooled.mod.nopred = reactive({
    lm(response~1, data=dat.nopred())
  })
  
  output$pooled.nopred.info = renderUI({
    mean = round(mean(dat.nopred()$response,na.rm=TRUE),digits=3)
    p(HTML("<li type=circle> y&#773<sub>all</sub> = pooled mean ="), code(paste0(mean)))
  })
  
  ############################################################################################################
  ## Unpooled Regression 1
  ############################################################################################################
  
  output$unpooled.nopred.graph = renderPlot({
    ggplot(data=dat.nopred(),aes(x=level2.id,y=response)) + geom_hline(aes(yintercept=mean(response)),linetype="dashed",color="navy") + 
      geom_boxplot(fill="white",color="navy") + xlab(paste(label()[2])) + ylab(paste(label()[1])) + 
      ggtitle(paste("Boxplots of",label()[1],"by",label()[2],"with pooled and unpooled means imposed")) +
      theme_bw() + theme(axis.text.x=element_text(angle=90)) +
      stat_summary(fun.y="mean", geom="point", shape=1, size=3, fill="white", color="gold2") 
  })
  
  unpooled.mod.nopred = reactive({
    lm(response~level2.id+0, data=dat.nopred())
  })
  
  output$unpooled.mod.summary = renderTable({
    anova(unpooled.mod.nopred())
  })
  
  output$unpooled.nopred.info = renderDataTable({
    mean.dat = data.frame(tapply(dat.nopred()$response,dat.nopred()$level2.id,mean,na.rm=TRUE))
    mean = round(as.numeric(mean.dat[,1]),digits=3)
    var = round(as.numeric(tapply(dat.nopred()$response,dat.nopred()$level2.id,var,na.rm=TRUE)))
    n = as.numeric(tapply(dat.nopred()$response,dat.nopred()$level2.id, function(x) length(!is.na(x))))
    
    dat = data.frame(ID=rownames(mean.dat),Mean=mean,Variance=var,n=n)
    return(dat) 
  })
  
  ############################################################################################################
  ## Hierarchical Linear Model 1
  ############################################################################################################
  
  hlm.mod.nopred = reactive({
    lmer(response ~ 1 + (1|level2.id), data=dat.nopred(),
         control=lmerControl(optCtrl=list(maxfun=50000)))
  })
  
  output$hlm.nopred1 = renderPrint({
    summary(hlm.mod.nopred())
  })

  coefs = reactive({
    data.frame(coef=coef(hlm.mod.nopred())$level2.id,ran=ranef(hlm.mod.nopred())$level2.id)
  })
  
  output$hlm.nopred2 = renderTable({
    conf = confint(hlm.mod.nopred(),method="boot")
    colnames(conf) = c("95% Lower Bound","95% Upper Bound")
    rownames(conf) = c("Between-group SD","Within-group SD","Intercept")
    return(conf)
  })
  
  output$intraclass1 = renderUI({
    withMathJax("$$\\hat{\\rho} = \\frac{\\hat{\\sigma}_\\alpha^{2}}{\\hat{\\sigma}_\\alpha^{2}+\\hat{\\sigma}_y^{2}}$$")
  })
  
  variances = reactive({
    c(round(as.numeric(attr(VarCorr(hlm.mod.nopred())[[1]],"stddev")^2),digits=3),
      round(as.numeric(attr(VarCorr(hlm.mod.nopred()),"sc")^2),digits=3))
  })
    
  output$calcintraclass1 = renderUI({
    icc = round(variances()[1]/sum(variances()),digits=3)
     
    p("ICC =",code(paste(variances()[1]))," / (",
      code(paste(variances()[1])),"+",
      code(paste(variances()[2])),") = ",
      code(paste(icc)))
  })
  
  output$ratio1 = renderUI({
    withMathJax("$$Variance\\,ratio = \\frac{\\hat{\\sigma}_y^{2}}{\\hat{\\sigma}_\\alpha^{2}}$$")
  })
  
  output$calcratiovariances1 = renderUI({
    ratio = round(variances()[2]/variances()[1],digits=3)
    
    p("Ratio =",code(paste(variances()[2]))," / ",
      code(paste(variances()[1])), " = ", code(paste(ratio)))
  })
  
  output$hlmdist1 = renderPlot({  
    ggplot(data=coefs(),aes(x=X.Intercept.)) + geom_histogram(color="seagreen2",fill="white") +
      xlab("HLM means") + ylab("Frequency") + ggtitle("Histogram of HLM means") + theme_bw()
  })

  output$hlmdisterror1 = renderPlot({
    ggplot(data=coefs(),aes(x=X.Intercept..1)) + geom_histogram(color="salmon",fill="white") +
      xlab("Group-level intercept errors") + ylab("Frequency") + 
      ggtitle("Histogram of group-level intercept errors") + theme_bw()
  })

  output$hlmdistinfo1 = renderUI({
    p(HTML("<ol><li type=circle>"),withMathJax("\\(\\hat{\\mu}_{\\alpha} =\\)"),code(paste(round(as.numeric(fixef(hlm.mod.nopred())),digits=3))),
      HTML("</li><li type=circle>"),withMathJax("\\(\\hat{\\sigma}_\\alpha^{2} =\\)"),code(paste(variances()[1])),HTML("</ol>"))
  })
  
  output$hlmtable = renderDataTable({
    mean.dat = data.frame(tapply(dat.nopred()$response,dat.nopred()$level2.id,mean,na.rm=TRUE))
    n = as.numeric(tapply(dat.nopred()$response,dat.nopred()$level2.id, function(x) length(!is.na(x))))
    rancoefs = round(coefs()$X.Intercept..1,digits=3)
    coefs = round(coefs()$X.Intercept.,digits=3)
    
    dat = matrix(c(rownames(mean.dat),coefs,rancoefs,n),ncol=4)
    colnames(dat) = c("Group","Estimated intercept","Group-level error","Sample size")
    return(dat) 
  })
  
  output$ggcat1 = renderPlot({
    ggCaterpillar(ranef(hlm.mod.nopred(), condVar=TRUE), QQ=FALSE)
  })
  
  ############################################################################################################
  ## Comparison of Methods 1
  ############################################################################################################
  
  output$shrinkplot = renderPlot({
    samplesize = tapply(dat.nopred()$response,dat.nopred()$level2.id, function(x) length(x))
    ind = order(samplesize)
    hlmmean = as.vector(unlist(coef(hlm.mod.nopred())))
    hlmmean = hlmmean[ind]
    hlmse = sqrt(attr(ranef(hlm.mod.nopred(), condVar = TRUE)[[1]], "postVar")[1, , ])/samplesize
    hlmse = hlmse[ind]
    unpooledse = tapply(dat.nopred()$response,dat.nopred()$level2.id, function(x) sd(x,na.rm=TRUE)/sqrt(length(x)))
    unpooledse = unpooledse[ind]
    unpooledmean = tapply(dat.nopred()$response,dat.nopred()$level2.id,mean,na.rm=TRUE)
    unpooledmean = unpooledmean[ind]
    groups = levels(factor(dat.nopred()$level2.id))
    groups = groups[ind]
    samplesize1 = samplesize[ind]
    dat = data.frame(hlmse=hlmse,hlmmean=hlmmean,unpooledse=unpooledse,unpooledmean=unpooledmean,groups=groups,
                     n=samplesize1)
    mean = data.frame(x=mean(dat.nopred()$response),na.rm=TRUE)
    
    g1=ggplot() + theme_bw() + geom_point(data=dat,aes(x=groups,y=unpooledmean,color="Unpooled"),shape=1) +
      geom_point(data=dat,aes(x=groups,y=hlmmean,color="HLM"),shape=16) + 
      geom_hline(data=mean,aes(yintercept=x,color="Pooled"),linetype="dashed") + 
      geom_errorbar(data=dat,aes(x=groups,y=unpooledmean,ymin=unpooledmean-1.96*unpooledse,ymax=unpooledmean+1.96*unpooledse,color="Unpooled")) +
      geom_errorbar(data=dat,aes(x=groups,y=hlmmean,ymin=hlmmean-1.96*hlmse,ymax=hlmmean+1.96*hlmse,color="HLM")) +
      ylab("Mean") + xlab("Group") + ggtitle("Shrinkage plot: 95% CIs using unpooled method and HLM with pooled mean imposed") +
      scale_color_manual(name="Modelling method",values=c("HLM"="tomato","Unpooled"="gold2","Pooled"="navy")) + theme(legend.position="bottom")
    
    g2=ggplot() + theme_bw() + geom_histogram(data=dat,aes(x=unpooledmean,y=..density..,fill="Unpooled"),alpha=.5) +
      geom_histogram(data=dat,aes(x=hlmmean,y=..density..,fill="HLM"),alpha=.5) +
      geom_density(data=dat,aes(x=unpooledmean),color="gold2") +
      geom_density(data=dat,aes(x=hlmmean),color="tomato") +
      geom_vline(data=mean,aes(xintercept=x,fill="Pooled"),color="navy",linetype="dashed") +
      scale_fill_manual(name="Modelling method",values=c("HLM"="tomato","Unpooled"="gold2","Pooled"="navy")) + 
      theme(legend.position="bottom") + ylab("Relative frequency") + xlab("Mean") + ggtitle("Histogram of unpooled and HLM means with pooled mean imposed") 
    
    g3 = arrangeGrob(g1,g2,nrow=1)
    return(g3)
  },height=500)

  ############################################################################################################
  ## Bayesian Hierarchical Linear Model 1
  ############################################################################################################

  hlmbayes1 = reactive({
    MCMCglmm(response ~ 1, random=~level2.id, data=dat.nopred(), verbose=FALSE, pr=TRUE)
  })

  output$hlm.bayes1 = renderPrint({
    summary(hlmbayes1())
  })

  output$postdist = renderPlot({
    group.error = data.frame(x=as.vector(hlmbayes1()$VCV[,1]))
    indiv.error = data.frame(x=as.vector(hlmbayes1()$VCV[,2]))
    int = data.frame(x=as.vector(hlmbayes1()$Sol[,1]))
    
    group.plot = ggplot(data=group.error) + geom_density(aes(x=x)) + theme_bw() + ylab("") + xlab("Between-group var in response") +
                   geom_vline(aes(xintercept=mean(x)),color="seagreen2",size=1)
    group.trace = ggplot(data=group.error) + geom_line(aes(x=1:length(x),y=x)) + theme_bw() + 
                    geom_hline(aes(yintercept=mean(x)),color="seagreen2",size=1) + 
                    xlab("Iteration") + ylab("Between-group\nvar in response")
    indiv.plot = ggplot(data=indiv.error) + geom_density(aes(x=x)) + theme_bw() + ylab("") + xlab("Within-group var in response") +
                   geom_vline(aes(xintercept=mean(x)),color="seagreen2",size=1)
    indiv.trace = ggplot(data=indiv.error) + geom_line(aes(x=1:length(x),y=x)) + theme_bw() + 
                    geom_hline(aes(yintercept=mean(x)),color="seagreen2",size=1) + 
                    xlab("Iteration") + ylab("Within-group\nvar in response")
    int.plot = ggplot(data=int) + geom_density(aes(x=x)) + theme_bw() + ylab("") + xlab("Intercept") +
                 geom_vline(aes(xintercept=mean(x)),color="seagreen2",size=1)
    int.trace = ggplot(data=int) + geom_line(aes(x=1:length(x),y=x)) + theme_bw() + 
                  geom_hline(aes(yintercept=mean(x)),color="seagreen2",size=1) + 
                  xlab("Iteration") + ylab("Intercept")
    graph = arrangeGrob(group.trace,group.plot,indiv.trace,indiv.plot,int.trace,int.plot,nrow=3,ncol=2,
                        main=textGrob("Traceplots and Posterior Distributions",gp=gpar(fontsize=15)))
    graph
  },height=500)

  ############################################################################################################
  ############################################################################################################
  ## Varying-intercept and varying-slope 
  ############################################################################################################
  ############################################################################################################

  output$selectresponse2 = renderUI({ 
    selectInput("response2","Response variable:",choices=names(data()),selectize=TRUE)  
  })
  
  output$selectlevel2id2 = renderUI({
    selectInput("level2.id2","Level 2 observational unit:",choices=names(data()),selectize=TRUE)
  })

  output$selectlevel1pred1 = renderUI({
    selectInput("level1.pred1","Level 1 predictor:",choices=names(data()),selectize=TRUE)
  })

  dat.pred1 = reactive({
    if(!input$usesample)
    {
      dat = data.frame(predictor = data()[,paste(input$level1.pred1)],
                       response = data()[,paste(input$response2)],
                       level2.id = factor(data()[,paste(input$level2.id2)]))
    } else if (input$usesample)
    {
      dat = data.frame(predictor = data()$previous,
                       response = data()$negative_affect,
                       level2.id = factor(data()$id))
      return(dat)
    }
  })

  label1 = reactive({
    if(!input$usesample)
    {
      return(c(paste(input$response2),paste(input$level1.pred1),paste(input$level2.id2)))
    }else if(input$usesample)
    {
      return(c("negative affect","previous","musicians"))
    }
  })
    
  ############################################################################################################
  ## Pooled Regression 2
  ############################################################################################################
  
  output$pooled.graph = renderPlot({
      ggplot(data=dat.pred1(), aes(x=predictor,y=response)) + 
        geom_point(position = "jitter", size=3, shape=1) +
        geom_smooth(method="lm",color="navy",se=FALSE) + xlab(paste(label1()[2])) + ylab(paste(label1()[1])) +
        ggtitle(paste("Scatterplot of",label1()[1],"versus", label1()[2],"with pooled line imposed")) + theme_bw()
  })
  
  pooled.mod = reactive({
      lm(response~predictor, data=dat.pred1())
  })
    
  output$pooled.output = renderTable({
    return(summary(pooled.mod()))
  })
    
  output$pooled.info = renderText({
    paste0("R-squared=",round(summary(pooled.mod())$r.squared,digits=3),"; Residual df=",pooled.mod()$df.residual)
  })
  
  ############################################################################################################
  ## Unpooled Regression 2
  ############################################################################################################
  
  output$unpooled.graph = renderPlot({
      ggplot(data=dat.pred1(), aes(x=predictor,y=response)) + geom_point(position = "jitter", size=3, shape=1) +
        geom_smooth(aes(group=level2.id),method="lm",color="gold2",se=FALSE) + 
        geom_smooth(aes(group=1),method="lm",color="navy",se=FALSE) + xlab(paste(label1()[2])) + ylab(paste(label1()[1])) +
        ggtitle(paste("Scatterplot of",label1()[1],"versus",label1()[2],"with unpooled lines imposed")) + theme_bw()
  })
  
  unpooled.mod = reactive({
    lm(response~0+predictor+factor(level2.id)+factor(level2.id)*predictor, data=dat.pred1())
  })
  
  output$unpooled.output = renderTable({
    return(anova(unpooled.mod()))
  })
  
  output$unpooled.info = renderText({
    paste0("R-squared=",round(sum(anova(unpooled.mod())$Sum[1:3])/sum(anova(unpooled.mod())$Sum),digits=3))
  })
  
  slope.intercept = reactive({
    id = unique(dat.pred1()$level2.id)
    slope=NULL
    intercept=NULL
    n=NULL
    i=0
    while(i<length(id))
    {
      i=i+1
      sub.dat = dat.pred1()[which(dat.pred1()$level2.id==id[i]),]
      n[i] = dim(sub.dat[which(complete.cases(sub.dat)),])[1]
      mod = lm(response~predictor,data=sub.dat)
      intercept[i] = mod$coefficients[1]
      slope[i] = mod$coefficients[2]
    }
    intercept = round(intercept,digits=3)
    slope = round(slope,digits=3)
    
    return(data.frame(intercept=intercept,slope=slope,n=n))
  })
  
  output$unpooled.est = renderDataTable({
    mat = matrix(c(unique(dat.pred1()$level2.id),slope.intercept()$intercept,
                   slope.intercept()$slope,slope.intercept()$n),ncol=4)
    colnames(mat) = c("ID","Intercept","Slope","n")
    return(mat)
  })
  
  ############################################################################################################
  ## Hierarchical Linear Model 2
  ############################################################################################################

  hlm = reactive({
    return(lmer(response ~ predictor + (predictor|level2.id), data=dat.pred1(),
                control=lmerControl(optCtrl=list(maxfun=50000))))
  })
  
  output$hlm.summary = renderPrint({
    summary(hlm())
  })

  output$hlm.confint1 = renderTable({
    conf = confint(hlm(),method="boot")
    colnames(conf) = c("95% Lower Bound","95% Upper Bound")
    rownames(conf) = c("Between-group Intercept SD","Correlation","Between-group Slope SD","Within-group SD",
                       "Intercept","Slope")
    return(conf)
  })
  
  output$hlm.graph = renderPlot({
    sub.dat = dat.pred1()[which(complete.cases(dat.pred1())),]
    sub.dat$predicted = predict(hlm())

    ggplot(data=sub.dat) + geom_point(aes(x=predictor,y=response), position = "jitter", size=3, shape=1) +
      xlab(paste(label1()[2])) + ylab(paste(label1()[1])) + 
      ggtitle(paste("Scatterplot of",label1()[1],"versus\n",label1()[2],"with HLM lines imposed")) +
      geom_smooth(data=sub.dat, aes(x=predictor,y=predicted,group=level2.id),method="lm",color="tomato",se=FALSE) +
      geom_smooth(data=sub.dat, aes(x=predictor,y=response,group=1),method="lm",color="navy",se=FALSE) + theme_bw()
  })
  
  hlm.slope.intercept = reactive({
    coef = coefficients(hlm())
    
    id = unique(dat.pred1()$level2.id)
    n=NULL
    i=0
    while(i<length(id))
    {
      i=i+1
      sub.dat = dat.pred1()[which(dat.pred1()$level2.id==id[i]),]
      n[i] = dim(sub.dat[which(complete.cases(sub.dat)),])[1]
    }
    coef[[1]][,1] = round(coef[[1]][,1],digits=3)
    coef[[1]][,2] = round(coef[[1]][,2],digits=3)
    
    return(data.frame(int.vec=coef[[1]][,1], slope.vec=coef[[1]][,2],n=n))
  })

  output$hlm.est = renderDataTable({
    dat = data.frame(ranef(hlm())$level2.id)
    mat = matrix(c(unique(dat.pred1()$level2.id),hlm.slope.intercept()$int.vec,
                   round(dat$X.Intercept.,digits=3),hlm.slope.intercept()$slope.vec, 
                   round(dat$predictor,digits=3),hlm.slope.intercept()$n),ncol=6)
    colnames(mat) = c("ID","Intercept","Group-level Intercept Error","Slope","Group-level Slope Error","n")
    return(mat)
  })

  output$hlmparamdist1 = renderPlot({
    dat = data.frame(coef(hlm())$level2.id)
    plot1 = ggplot(dat) + geom_histogram(aes(x=X.Intercept.), color="seagreen2",fill="white") +
              xlab("HLM intercepts") + ylab("Frequency") + ggtitle("Histogram of HLM intercepts") + theme_bw()
    plot2 = ggplot(dat) + geom_histogram(aes(x=predictor), color="seagreen2",fill="white") +
              xlab("HLM slopes") + ylab("Frequency") + ggtitle("Histogram of HLM slopes") + theme_bw()
    plot3 = arrangeGrob(plot1,plot2,nrow=1)
    return(plot3)
  })

  output$hlmparamdistinfo1 = renderUI({
    p(HTML("<ol><li type=circle>"),withMathJax("\\(\\hat{\\mu}_{\\alpha} =\\)"),code(paste(round(as.numeric(fixef(hlm.mod.nopred())),digits=3))),
      HTML("</li><li type=circle>"),withMathJax("\\(\\hat{\\sigma}_\\alpha^{2} =\\)"),code(paste(variances()[2])),HTML("</ol>"))
  })

  output$hlmparamdist2 = renderPlot({
    dat = data.frame(ranef(hlm())$level2.id)
    plot1 = ggplot(dat) + geom_histogram(aes(x=X.Intercept.), color="salmon",fill="white") +
              xlab("Group-level intercept errors") + ylab("Frequency") +
              ggtitle("Histogram of group-level\nintercept errors") + theme_bw()
    plot2 = ggplot(dat) + geom_histogram(aes(x=predictor), color="salmon",fill="white") +
              xlab("Group-level slope errors") + ylab("Frequency") +
              ggtitle("Histogram of group-level\nslope errors") + theme_bw()
    plot3 = arrangeGrob(plot1,plot2,nrow=1)
    return(plot3)
  })

  output$ggcat2 = renderPlot({
    ggCaterpillar(ranef(hlm(), condVar=TRUE), QQ=FALSE)
  })
  
  ############################################################################################################
  ## Comparison of Methods 2
  ############################################################################################################
  
  output$slopeint.comp = renderPlot({
    dat = dat.pred1()
    mod = lm(response~predictor,data=dat)
    datline = data.frame(x=as.numeric(mod$coef[1]),y=as.numeric(mod$coef[2]))
    
    intercept = ggplot() + geom_histogram(data=slope.intercept(),aes(x=intercept,y=..density..,fill="Unpooled"),alpha=.5) +
      geom_histogram(data=hlm.slope.intercept(),aes(x=int.vec,y=..density..,fill="HLM"),alpha=.5) + 
      geom_density(data=slope.intercept(),aes(x=intercept),color="gold2") + 
      geom_vline(data=datline,aes(xintercept=x),color="navy",linetype="dashed") +
      geom_density(data=hlm.slope.intercept(),aes(x=int.vec),color="tomato") +
      ggtitle("Histogram of sample intercepts") + xlab("Sample intercepts") + ylab("Relative frequency") +
      scale_fill_manual(name="Model",values=c("HLM"="tomato","Unpooled"="gold2","Pooled"="navy")) + 
      guides(fill=FALSE) + theme_bw()
    
    slope = ggplot() + geom_histogram(data=slope.intercept(),aes(x=slope,y=..density..,fill="Unpooled"),alpha=.5) +
      geom_histogram(data=hlm.slope.intercept(),aes(x=slope.vec,y=..density..,fill="HLM"),alpha=.5) + 
      geom_density(data=slope.intercept(),aes(x=slope),color="gold2") + 
      geom_vline(data=datline,aes(xintercept=y),color="navy",linetype="dashed") +
      geom_density(data=hlm.slope.intercept(),aes(x=slope.vec),color="tomato") + 
      ggtitle("Histogram of sample slopes") + xlab("Sample slopes") + ylab("Relative frequency") +
      scale_fill_manual(name="Model",values=c("HLM"="tomato","Unpooled"="gold2","Pooled"="navy")) + 
      guides(fill=FALSE) + theme_bw()
    
    graph1 = arrangeGrob(intercept,slope,nrow=1)
  
    graph2 = ggplot() + geom_vline(data=datline,aes(xintercept=x),color="navy",linetype="dashed") + 
      geom_hline(data=datline,aes(yintercept=y),color="navy",linetype="dashed") + 
      geom_point(data=hlm.slope.intercept(),aes(x=int.vec,y=slope.vec,color="HLM"),size=3,shape=1) +
      geom_point(data=slope.intercept(),aes(x=intercept,y=slope,color="Unpooled"),size=3,shape=1) +
      geom_smooth(data=hlm.slope.intercept(),aes(x=int.vec,y=slope.vec,color="HLM"),method="lm",se=FALSE) + 
      geom_smooth(data=slope.intercept(),aes(x=intercept,y=slope,color="Unpooled"),method="lm",se=FALSE) + 
      ggtitle("Scatterplot of sample slopes versus intercepts with pooled estimates imposed") +
      xlab("Intercepts") + ylab("Slopes") + scale_color_manual(name="Modelling method",values=c("HLM"="tomato","Unpooled"="gold2")) +
      theme_bw() + theme(legend.position="bottom")
    
    graph3 = arrangeGrob(graph2,graph1,ncol=1)
    
    return(graph3)
  },height=800)

  output$comp.method = renderPlot({
    dat = dat.pred1()
    dat$hlmpred = fitted(hlm())
    
    mod = lm(response~predictor,data=dat)
    dat$x=as.numeric(mod$coef[1])
    dat$y=as.numeric(mod$coef[2])
    
    ggplot(data=dat) + geom_point(aes(x=predictor,y=response),shape=1) + facet_wrap(~level2.id) + theme_bw() +
      geom_smooth(aes(x=predictor,y=response, color="Unpooled"), method="lm", se=FALSE) +
      geom_abline(aes(intercept=x,slope=y, color="Pooled"),size=.5) +
      geom_smooth(aes(x=predictor,y=hlmpred,color="HLM"),method="lm",se=FALSE) +
      scale_color_manual(name="Modelling method", values=c("Pooled"="navy","Unpooled"="gold2","HLM"="tomato")) +
      theme(legend.position="bottom", legend.direction="horizontal") + xlab(paste(label1()[2])) + ylab(paste(label1()[1])) +
      ggtitle("Comparison of the three modelling methods")
  },height=800)

  ############################################################################################################
  ## Bayesian Hierarchical Linear Model 2
  ############################################################################################################

  hlmbayes2 = reactive({
    MCMCglmm(response ~ predictor, random=~level2.id+predictor, data=dat.pred1(), 
             verbose=FALSE, pr=TRUE)
  })
  
  output$hlm.bayes2 = renderPrint({
    summary(hlmbayes2())
  })
  
  output$postdist2 = renderPlot({
    slope.error = data.frame(x=as.vector(hlmbayes2()$VCV[,2]))
    indiv.error = data.frame(x=as.vector(hlmbayes2()$VCV[,3]))
    group.error = data.frame(x=as.vector(hlmbayes2()$VCV[,1]))
    int = data.frame(x=as.vector(hlmbayes2()$Sol[,1]))
    group.slope.plot = ggplot(data=slope.error) + geom_density(aes(x=x)) + theme_bw() + ylab("") + xlab("Between-group var in slopes") +
                         geom_vline(aes(xintercept=mean(x)),color="seagreen2",size=1)
    group.slope.trace = ggplot(data=slope.error) + geom_line(aes(x=1:length(x),y=x)) + theme_bw() + 
                          geom_hline(aes(yintercept=mean(x)),color="seagreen2",size=1) + 
                          xlab("Iteration") + ylab("Between-group\nvar in slopes")
    group.error.plot = ggplot(data=group.error) + geom_density(aes(x=x)) + theme_bw() + ylab("") + xlab("Between-group var in intercepts") +
                         geom_vline(aes(xintercept=mean(x)),color="seagreen2",size=1)
    group.error.trace = ggplot(data=group.error) + geom_line(aes(x=1:length(x),y=x)) + theme_bw() + 
                        geom_hline(aes(yintercept=mean(x)),color="seagreen2",size=1) + 
                        xlab("Iteration") + ylab("Between-group\nvar in intercepts")
    indiv.plot = ggplot(data=indiv.error) + geom_density(aes(x=x)) + theme_bw() + ylab("") + xlab("Within-group var in response") +
                   geom_vline(aes(xintercept=mean(x)),color="seagreen2",size=1)
    indiv.trace = ggplot(data=indiv.error) + geom_line(aes(x=1:length(x),y=x)) + theme_bw() + 
                    geom_hline(aes(yintercept=mean(x)),color="seagreen2",size=1) + 
                    xlab("Iteration") + ylab("Within-group\nvar in response")
    int.plot = ggplot(data=int) + geom_density(aes(x=x)) + theme_bw() + ylab("") + xlab("Intercept") +
                 geom_vline(aes(xintercept=mean(x)),color="seagreen2",size=1)
    int.trace = ggplot(data=int) + geom_line(aes(x=1:length(x),y=x)) + theme_bw() + 
                  geom_hline(aes(yintercept=mean(x)),color="seagreen2",size=1) + 
                  xlab("Iteration") + ylab("Intercept")
    graph = arrangeGrob(group.error.trace,group.error.plot,group.slope.trace,group.slope.plot,indiv.trace,
                        indiv.plot,int.trace,int.plot,nrow=4,ncol=2,main=textGrob("Traceplots and Posterior Distributions",gp=gpar(fontsize=15)))
    return(graph)
  },height=600)

  ############################################################################################################
  ############################################################################################################
  ## Varying-intercept and varying-slope with level 2 predictor
  ############################################################################################################
  ############################################################################################################

  output$selectresponse3 = renderUI({ 
    selectInput("response3","Response variable:",choices=names(data()),selectize=TRUE)  
  })
  
  output$selectlevel2id3 = renderUI({
    selectInput("level2.id3","Level 2 observational unit:",choices=names(data()),selectize=TRUE)
  })
  
  output$selectlevel1pred2 = renderUI({
    selectInput("level1.pred2","Level 1 predictor:",choices=names(data()),selectize=TRUE)
  })

  output$selectlevel2pred1 = renderUI({
    selectInput("level2.pred1","Level 2 predictor:",choices=names(data()),selectize=TRUE)
  })

  dat.pred2 = reactive({
    if(!input$usesample)
    {
      dat = data.frame(predictor = data()[,paste(input$level1.pred2)],
                       response = data()[,paste(input$response3)],
                       level2.id = factor(data()[,paste(input$level2.id3)]),
                       level2.predictor = data()[,paste(input$level2.pred1)])
      return(dat)
    }else if(input$usesample)
    {
      dat = data.frame(predictor = data()$previous,
                       response = data()$negative_affect,
                       level2.id = factor(data()$id),
                       level2.predictor = data()$years_study)
      return(dat)
    }
  })

  label2 = reactive({
    if(!input$usesample)
    {
      return(c(paste(input$response3),paste(input$level1.pred2),paste(input$level2.id3),paste(input$level2.pred1)))
    } else if(input$usesample)
    {
      return(c("negative affect","previous","musicians","years study"))
    }
  })

  ############################################################################################################
  ## Two stage modelling
  ############################################################################################################

  stage2 = reactive({
    dat.pred2()[!duplicated(dat.pred2()$level2.id),]
  })

  output$int.mod = renderTable({ 
    dat = data.frame(intercept=slope.intercept()$intercept,level2.predictor=stage2()$level2.predictor)
    
    lm(intercept ~ level2.predictor, data=dat)
  })

  output$slope.mod = renderTable({    
    dat = data.frame(slope=slope.intercept()$slope,level2.predictor=stage2()$level2.predictor)
    
    lm(slope ~ level2.predictor, data=dat)
  })

  output$stage2graph = renderPlot({
    dat = data.frame(int=slope.intercept()$intercept,slope=slope.intercept()$slope,pred=stage2()$level2.predictor)
    
    g1 = ggplot(data=dat,aes(x=pred,y=int)) + geom_point(shape=1) + geom_smooth(method="lm",se=FALSE,color="gold2") +
      xlab(paste(label2()[4])) + ylab("Unpooled intercepts") + theme_bw() + 
      ggtitle(paste("Scatterplot of unpooled\nintercepts versus",label2()[4]))
    
    g2 = ggplot(data=dat,aes(x=pred,y=slope)) + geom_point(shape=1) + geom_smooth(method="lm",se=FALSE,color="gold2") +
      xlab(paste(label2()[4])) + ylab("Unpooled slopes") + theme_bw() +
      ggtitle(paste("Scatterplot of unpooled\nslopes versus",label2()[4]))
    
    g3 = arrangeGrob(g1,g2,nrow=1)
    return(g3)
  })

  ############################################################################################################
  ## Hierarchical Linear Model 3
  ############################################################################################################

  hlm2 = reactive({
    return(lmer(response ~ predictor + level2.predictor + predictor:level2.predictor + 
                  (1 + predictor|level2.id), data=dat.pred2(),
                control=lmerControl(optCtrl=list(maxfun=50000))))
  })
  
  output$hlm.summary1 = renderPrint({
    summary(hlm2())
  })

  output$hlm.confint2 = renderTable({
    conf = confint(hlm2(),method="boot")
    colnames(conf) = c("95% Lower Bound","95% Upper Bound")
    rownames(conf) = c("Between-group Intercept SD","Correlation","Between-group Slope SD","Within-group SD",
                       "Intercept","Level 1 Predictor","Level 2 Predictor","Cross-level Interaction")
    return(conf)
  })

  output$hlmhyper1 = renderTable({
    vec = as.vector(fixef(hlm2()))
    tab = matrix(vec,nrow=1)
    colnames(tab) = c("Intercept","Level 1 Predictor","Level 2 Predictor","Cross-level Interaction")
    rownames(tab) = "Estimates"
    return(tab)
  })

  output$hlmhyper2 = renderPrint({
    VarCorr(hlm2())
  })

  output$ggcat3 = renderPlot({
    ggCaterpillar(ranef(hlm2(), condVar=TRUE), QQ=FALSE)
  })

  ############################################################################################################
  ## Comparison of Methods 3
  ############################################################################################################

  output$compgraph = renderPlot({
    dat = data.frame(int=slope.intercept()$intercept,slope=slope.intercept()$slope,pred=stage2()$level2.predictor)
    dat1 = data.frame(int1=fixef(hlm2())[1],slope1=fixef(hlm2())[3],int2=fixef(hlm2())[2],slope2=fixef(hlm2())[4])
    
    g1 = ggplot() + geom_point(data=dat,aes(x=pred,y=int,color="Unpooled"),shape=1) + 
      geom_smooth(data=dat,aes(x=pred,y=int,color="Unpooled"),method="lm",se=FALSE) +
      xlab(paste(label2()[4])) + ylab("Intercepts") + theme_bw() +
      geom_abline(data=dat1,aes(intercept=int1,slope=slope1,color="HLM")) +
      scale_color_manual(name="Modelling method", values=c("Unpooled"="gold2","HLM"="tomato")) +
      theme(legend.position="bottom", legend.direction="horizontal") +
      ggtitle(paste("Scatterplot of intercepts versus",label2()[4],"\ncomparing the unpooled method and HLM"))
      
    g2 = ggplot() + geom_point(data=dat,aes(x=pred,y=slope,color="Unpooled"),shape=1) + 
      geom_smooth(data=dat,aes(x=pred,y=slope,color="Unpooled"),method="lm",se=FALSE) +
      xlab(paste(label2()[4])) + ylab("Slopes") + theme_bw() +
      geom_abline(data=dat1,aes(intercept=int2,slope=slope2,color="HLM")) +
      scale_color_manual(name="Modelling method", values=c("Unpooled"="gold2","HLM"="tomato")) +
      theme(legend.position="bottom", legend.direction="horizontal") +
      ggtitle(paste("Scatterplot of slopes versus",label2()[4],"\ncomparing the unpooled method and HLM"))
        
    g3 = arrangeGrob(g1,g2,nrow=1)
    return(g3)
  },height=500)
  

  ############################################################################################################
  ############################################################################################################
  ## Analyze Data Tab
  ############################################################################################################
  ############################################################################################################
  
  output$selectresponse.analyze = renderUI({ 
    selectInput("responseanalyze","Response variable:",choices=names(data()),selectize=TRUE)  
  })
  
  output$selectlevel2id.anaylze = renderUI({
    selectInput("level2.idanalyze","Level 2 observational unit:",choices=names(data()),selectize=TRUE)
  })
  
  output$selectlevel1pred.analyze = renderUI({
    selectizeInput("level1.predanalyze","Level 1 predictor(s):",choices=names(data()),
                multiple=TRUE,options=list(placeholder="level 1 predictor(s)"))
  })

  output$selectlevel1pred.analyze.rand = renderUI({
    selectizeInput("level1.predanalyze.rand","Random predictor(s):",choices=input$level1.predanalyze,
                   multiple=TRUE,options=list(placeholder="random predictor(s)"))
  })
  
  output$selectlevel2pred.analyze = renderUI({
    selectizeInput("level2.predanalyze","Level 2 predictor(s):",choices=names(data()),
                multiple=TRUE,options=list(placeholder="level 2 predictor(s)"))
  })

  output$selectcrosslevint = renderUI({
    selectizeInput("crosslevelint", "Cross-level interaction(s):", 
                   choices=as.vector(outer(input$level1.predanalyze.rand, input$level2.predanalyze, paste, sep=":")),
                   multiple=TRUE,options=list(placeholder="cross-level interaction(s)"))
  })

  dat.analyze = reactive({
    dat.pred1=matrix(nrow=dim(data())[1],ncol=length(input$level1.predanalyze))
    i=0
    formula1 = NULL
    formula1 = input$level1.predanalyze[1]
    while(i<length(input$level1.predanalyze))
    {
      i=i+1
      dat.pred1[,i] = data()[,paste(input$level1.predanalyze[i])]
      if(i>1) formula1 = paste0(formula1,"+",input$level1.predanalyze[i])
    }
    colnames(dat.pred1) = input$level1.predanalyze
    dat.pred1 = data.frame(dat.pred1)
    
    formula2=NULL
    dat.pred2=matrix(nrow=dim(data())[1],ncol=length(input$responseanalyze))
    if(length(input$level2.predanalyze)>0)
    {
      j=0
      formula2 = paste("+",input$level2.predanalyze[1])
      while(j<length(input$level2.predanalyze))
      {
        j=j+1
        dat.pred2[,j] = data()[,paste(input$level2.predanalyze[j])]
        if(j>1) formula2 = paste0(formula2,"+",input$level2.predanalyze[j])
      }
      colnames(dat.pred2) = input$level2.predanalyze
      dat.pred2 = data.frame(dat.pred2)
    }
    
    formula3=NULL
    if(length(input$level1.predanalyze.rand)>0)
    {
      k=1
      formula3 = paste0("+",input$level1.predanalyze.rand[1])
      while(k<length(input$level1.predanalyze.rand))
      {
        k=k+1
        formula3 = paste0(formula3,"+",input$level1.predanalyze.rand[k])
      }
    }
    
    formula4=NULL
    if(length(input$crosslevelint)>0)
    {
      l=1
      formula4 = paste0("+",input$crosslevelint[1])
      while(l<length(input$crosslevelint))
      {
        l=l+1
        formula4 = paste0(formula4,"+",input$crosslevelint[l])
      }
    }
    
    dat = data.frame(response = data()[,paste(input$responseanalyze)],
                     level2.id = factor(data()[,paste(input$level2.idanalyze)]),
                     dat.pred1,dat.pred2)
    names(dat)[1] = input$responseanalyze
    names(dat)[2] = input$level2.idanalyze
    
    return(list(dat=dat,formula1=formula1,formula2=formula2,formula3=formula3,formula4=formula4))
  })

  mod.analyze = reactive({
    do.call(paste("lmer"),list(formula=paste0(input$responseanalyze,"~",dat.analyze()[[2]],dat.analyze()[[3]],
                                              dat.analyze()[[5]],"+ (1",dat.analyze()[[4]],"|",input$level2.idanalyze,")"),
                               data=dat.analyze()[[1]],control=lmerControl(optCtrl=list(maxfun=50000))))
  })

  output$summary.analyze = renderPrint({
    input$runmod
    isolate({
      if(input$runmod>0)
      {
        summary = summary(mod.analyze())
        summary$call=NULL
        return(summary)
      }
    })
  })  

})