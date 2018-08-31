options(rgl.useNULL=TRUE)

############# CODE FOR THE IRIS DATA ################################
data(iris)
dfiris <- iris
dfiris $ Petal.Area = dfiris$Petal.Width * dfiris$Petal.Length

colorsirisCat = array(dim =length(dfiris$Species))
colorsirisCat[which(dfiris$Species == "setosa")] = "red"
colorsirisCat[which(dfiris$Species == "versicolor")] = "blue"
colorsirisCat[which(dfiris$Species == "virginica")] = "darkgreen"

Sepal.Length <- seq(min(dfiris$Sepal.Length),max(dfiris$Sepal.Length),len=30)
Sepal.Width <- seq(min(dfiris$Sepal.Width),max(dfiris$Sepal.Width),len=30)

##### IRIS SIMPLE MULTIPLE REGRESSION #####

irisfit <- lm(Petal.Area~Sepal.Length+Sepal.Width,dfiris)

plot.dfiris <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width)
plot.dfiris$Petal.Area.Pred <- predict(irisfit,newdata=plot.dfiris)
irisheight <- dcast(plot.dfiris,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

############# IRIS INTERACTION DATA ################################

fitIrisInt <- lm(Petal.Area~Sepal.Length*Sepal.Width,dfiris)

plot.dfIrisInt <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width)
plot.dfIrisInt$Petal.Area.Pred <- predict(fitIrisInt,newdata=plot.dfIrisInt)
heightIrisInt <- dcast(plot.dfIrisInt,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]


############# IRIS CATEGORICAL DATA ################################

fitIrisCat <- lm(Petal.Area ~ Sepal.Width + Sepal.Length + Species, data=dfiris)

plot.dfIrisCat1 <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width,Species = "setosa")
plot.dfIrisCat1$Petal.Area.Pred <- predict(fitIrisCat,newdata=plot.dfIrisCat1)
heightIrisCat1 <- dcast(plot.dfIrisCat1,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

plot.dfIrisCat2 <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width,Species = "versicolor")
plot.dfIrisCat2$Petal.Area.Pred <- predict(fitIrisCat,newdata=plot.dfIrisCat2)
heightIrisCat2 <- dcast(plot.dfIrisCat2,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

plot.dfIrisCat3 <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width,Species = "virginica")
plot.dfIrisCat3$Petal.Area.Pred <- predict(fitIrisCat,newdata=plot.dfIrisCat3)
heightIrisCat3 <- dcast(plot.dfIrisCat3,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

############# IRIS CATEGORICAL INTERACTION DATA ################################

fitIrisCatInt <- lm(Petal.Area ~ Sepal.Width * Species + Sepal.Length * Species, data=dfiris)
# fitIrisCatInt <- lm(Petal.Area ~ Sepal.Width * Sepal.Length * Species, data=dfiris)

plot.dfIrisCatInt1 <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width,Species = "setosa")
plot.dfIrisCatInt1$Petal.Area.Pred <- predict(fitIrisCatInt,newdata=plot.dfIrisCatInt1)
heightIrisCatInt1 <- dcast(plot.dfIrisCatInt1,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

plot.dfIrisCatInt2 <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width,Species = "versicolor")
plot.dfIrisCatInt2$Petal.Area.Pred <- predict(fitIrisCatInt,newdata=plot.dfIrisCatInt2)
heightIrisCatInt2 <- dcast(plot.dfIrisCatInt2,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

plot.dfIrisCatInt3 <- expand.grid(Sepal.Length = Sepal.Length,Sepal.Width = Sepal.Width,Species = "virginica")
plot.dfIrisCatInt3$Petal.Area.Pred <- predict(fitIrisCatInt,newdata=plot.dfIrisCatInt3)
heightIrisCatInt3 <- dcast(plot.dfIrisCatInt3,Sepal.Length~Sepal.Width,value.var="Petal.Area.Pred")[-1]

########## CODE FOR CARS DATA ############

data(mtcars)

dfcars<- data.frame(mpg = mtcars$mpg,hp = mtcars$hp, wt = mtcars$wt, am = factor(mtcars$am))
levels(dfcars$am) = c("automatic","manual")

colorcarsCat <- array(dim = length(dfcars$am))
colorcarsCat[which(dfcars$am == "manual")] = "red"
colorcarsCat[which(dfcars$am == "automatic")] = "green"

hp <- seq(min(dfcars$hp),max(dfcars$hp),len=30)
wt <- seq(min(dfcars$wt),max(dfcars$wt),len=30)

### CARS SIMPLE MULTIPLE REGRESSION###

carsfit <- lm(mpg~hp+wt,dfcars)
plot.dfcars <- expand.grid(hp = hp,wt = wt)
plot.dfcars$mpgcars.pred <- predict(carsfit,newdata=plot.dfcars)
carsheight <- dcast(plot.dfcars,hp~wt,value.var="mpgcars.pred")[-1]

#### CARS INTERACTION DATA ####
fitcarsInt <- lm(mpg~hp*wt,dfcars)
plot.dfcarsInt <- expand.grid(hp = hp, wt = wt)
plot.dfcarsInt$mpgcars.pred <- predict(fitcarsInt,newdata=plot.dfcarsInt)
heightcarsInt <- dcast(plot.dfcarsInt,hp~wt,value.var="mpgcars.pred")[-1]

##### CARS CATEGORICAL DATA #####

fitcarsCat <- lm(mpg~hp+wt+am,dfcars)

plot.dfcarsCat1 <- expand.grid(hp = hp, wt = wt, am = "manual")
plot.dfcarsCat1$mpgcars.pred <- predict(fitcarsCat,newdata=plot.dfcarsCat1)
heightcarsCat1 <- dcast(plot.dfcarsCat1,hp~wt,value.var="mpgcars.pred")[-1]

plot.dfcarsCat2 <- expand.grid(hp = hp, wt = wt, am = "automatic")
plot.dfcarsCat2$mpgcars.pred <- predict(fitcarsCat,newdata=plot.dfcarsCat2)
heightcarsCat2 <- dcast(plot.dfcarsCat2,hp~wt,value.var="mpgcars.pred")[-1]

##### CARS CATEGORICAL DATA #####

fitcarsCatInt <- lm(mpg~hp*am + wt*am,dfcars)
# fitcarsCatInt <- lm(mpg~hp*wt*am,dfcars)

plot.dfcarsCatInt1 <- expand.grid(hp = hp, wt = wt, am = "manual")
plot.dfcarsCatInt1$mpgcars.pred <- predict(fitcarsCatInt,newdata=plot.dfcarsCatInt1)
heightcarsCatInt1 <- dcast(plot.dfcarsCatInt1,hp~wt,value.var="mpgcars.pred")[-1]

plot.dfcarsCatInt2 <- expand.grid(hp = hp, wt = wt, am = "automatic")
plot.dfcarsCatInt2$mpgcars.pred <- predict(fitcarsCatInt,newdata=plot.dfcarsCatInt2)
heightcarsCatInt2 <- dcast(plot.dfcarsCatInt2,hp~wt,value.var="mpgcars.pred")[-1]

########## CODE FOR STATE DATA #################

states <- as.data.frame(state.x77)
names(states)[4] = "LifeExp"
names(states)[6] = "HSGrad"
states$Region = c("South","West","West","South","West","West","Northeast",
                  "South","South","South","West","West","Midwest","Midwest",
                  "Midwest","Midwest","South","South","Northeast","South",
                  "Northeast","Midwest","Midwest","South","Midwest","West",
                  "Midwest","West","Northeast","Northeast","West","Northeast",
                  "South","Midwest","Midwest","South","West","Northeast",
                  "Northeast","South","Midwest","South","South","West",
                  "Northeast","South","West","South","Midwest","West")
states$Region = factor(states$Region)
states = states[c(4,5,6,9)]

colorstateCat = array(dim = length(states$Region))
colorstateCat[which(states$Region == "Midwest")] = "blue"
colorstateCat[which(states$Region == "Northeast")] = "red"
colorstateCat[which(states$Region == "South")] = "green"
colorstateCat[which(states$Region == "West")] = "black"

Murder <- seq(min(states$Murder),max(states$Murder),len=30)
HSGrad <- seq(min(states$HSGrad),max(states$HSGrad),len=30)

##### STATES SIMPLE MULTIPLE REGRESSION #######

fitstate <- lm(LifeExp ~ Murder + HSGrad, data = states)
plot.dfstate <- expand.grid(Murder = Murder,HSGrad = HSGrad)
plot.dfstate$LifeExpState.pred <- predict(fitstate,newdata=plot.dfstate)
stateheight <- dcast(plot.dfstate,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

##### STATES INTERACTION DATA #########

fitstateInt <- lm(LifeExp ~ Murder*HSGrad, data = states)
plot.dfstateInt <- expand.grid(Murder = Murder, HSGrad = HSGrad)
plot.dfstateInt$LifeExpState.pred <- predict(fitstateInt,newdata=plot.dfstateInt)
stateheightInt <- dcast(plot.dfstateInt,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

#####  STATES CATEGORICAL DATA #####

fitstateCat <- lm(LifeExp ~ Murder + HSGrad + Region, data = states)

plot.dfstateCat1 <- expand.grid(Murder = Murder, HSGrad = HSGrad, Region = "Northeast")
plot.dfstateCat1$LifeExpState.pred <- predict(fitstateCat,newdata=plot.dfstateCat1)
stateheightCat1 <- dcast(plot.dfstateCat1,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

plot.dfstateCat2 <- expand.grid(Murder = Murder, HSGrad = HSGrad, Region = "South")
plot.dfstateCat2$LifeExpState.pred <- predict(fitstateCat,newdata=plot.dfstateCat2)
stateheightCat2 <- dcast(plot.dfstateCat2,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

plot.dfstateCat3 <- expand.grid(Murder = Murder, HSGrad = HSGrad, Region = "West")
plot.dfstateCat3$LifeExpState.pred <- predict(fitstateCat,newdata=plot.dfstateCat3)
stateheightCat3 <- dcast(plot.dfstateCat3,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

#####  STATES CATEGORICAL DATA #####

# fitstateCatInt <- lm(LifeExp ~ Murder * HSGrad * Region, data = states)
fitstateCatInt <- lm(LifeExp ~ Murder * Region + HSGrad * Region, data = states)

plot.dfstateCatInt1 <- expand.grid(Murder = Murder, HSGrad = HSGrad, Region = "Northeast")
plot.dfstateCatInt1$LifeExpState.pred <- predict(fitstateCatInt,newdata=plot.dfstateCatInt1)
stateheightCatInt1 <- dcast(plot.dfstateCatInt1,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

plot.dfstateCatInt2 <- expand.grid(Murder = Murder, HSGrad = HSGrad, Region = "South")
plot.dfstateCatInt2$LifeExpState.pred <- predict(fitstateCatInt,newdata=plot.dfstateCatInt2)
stateheightCatInt2 <- dcast(plot.dfstateCatInt2,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

plot.dfstateCatInt3 <- expand.grid(Murder = Murder, HSGrad = HSGrad, Region = "West")
plot.dfstateCatInt3$LifeExpState.pred <- predict(fitstateCatInt,newdata=plot.dfstateCatInt3)
stateheightCatInt3 <- dcast(plot.dfstateCatInt3,Murder~HSGrad,value.var="LifeExpState.pred")[-1]

####BEGINNING OF SHINY CODE ###########
shinyServer(function(input, output){

  output$troisPlot <- renderRglwidget({

    if (input$dataset == "iris")
    {
      if (input$expTypes1 == 5){
        par3d(scale=c(1,1,0.2),cex=.6)
        points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area)
        axes3d()
        title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
      }else if (input$expTypes1 == 1){
        par3d(scale=c(1,1,0.2),cex=.6)
        points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(irisheight),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
      }else if (input$expTypes1 == 2){
        par3d(scale=c(1,1,0.2),cex=.6)
        points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(heightIrisInt),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Sepal Area")
      }else if (input$expTypes1 == 4){
        par3d(scale=c(1,1,0.2),cex=.6)
        points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area,col = colorsirisCat)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(heightIrisCat1),col="blue",alpha=.5)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(heightIrisCat2),col="blue",alpha=.5)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(heightIrisCat3),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
      }else if (input$expTypes1 == 6){
        par3d(scale=c(1,1,0.2),cex=.6)
        points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area,col = colorsirisCat)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(heightIrisCatInt1),col="blue",alpha=.5)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(heightIrisCatInt2),col="blue",alpha=.5)
        surface3d(Sepal.Length,Sepal.Width,as.matrix(heightIrisCatInt3),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
      }

    }else if (input$dataset == "mtcars"){
      if (input$expTypes2 == 5){
        par3d(scale=c(0.02,1,0.2),cex=.5)
        points3d(dfcars$hp,dfcars$wt,dfcars$mpg)
        axes3d()
        title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
      }else if (input$expTypes2 == 1){
        par3d(scale=c(0.02,1,0.2),cex=.5)
        points3d(dfcars$hp,dfcars$wt,dfcars$mpg)
        surface3d(hp,wt,as.matrix(carsheight),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
      }else if (input$expTypes2 == 2){
        par3d(scale=c(0.02,1,0.2),cex=.6)
        points3d(dfcars$hp,dfcars$wt,dfcars$mpg)
        surface3d(hp,wt,as.matrix(heightcarsInt),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
      }else if (input$expTypes2 == 4){
        par3d(scale=c(0.02,1,0.2),cex=.6)
        points3d(dfcars$hp,dfcars$wt,dfcars$mpg,col=colorcarsCat)
        surface3d(hp,wt,as.matrix(heightcarsCat1),col="blue",alpha=.5)
        surface3d(hp,wt,as.matrix(heightcarsCat2),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
      }else if (input$expTypes2 == 6){
        par3d(scale=c(0.02,1,0.2),cex=.6)
        points3d(dfcars$hp,dfcars$wt,dfcars$mpg,col=colorcarsCat)
        surface3d(hp,wt,as.matrix(heightcarsCatInt1),col="blue",alpha=.5)
        surface3d(hp,wt,as.matrix(heightcarsCatInt2),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
      }

    } else if (input$dataset == "state.x77"){
      if (input$expTypes3 == 5){
        par3d(scale=c(1,.5,2),cex=.5)
        points3d(states$Murder,states$HSGrad,states$LifeExp)
        axes3d()
        title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
      }else if (input$expTypes3 == 1){
        par3d(scale=c(1,.5,2),cex=.5)
        points3d(states$Murder,states$HSGrad,states$LifeExp)
        surface3d(Murder,HSGrad,as.matrix(stateheight),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
      }else if (input$expTypes3 == 2){
        par3d(scale=c(1,.5,2),cex=.5)
        points3d(states$Murder,states$HSGrad,states$LifeExp)
        surface3d(Murder,HSGrad,as.matrix(stateheightInt),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
      }else if (input$expTypes3 == 4){
        par3d(scale=c(1,.5,2),cex=.5)
        points3d(states$Murder,states$HSGrad,states$LifeExp,col = colorstateCat)
        surface3d(Murder,HSGrad,as.matrix(stateheightCat1),col="blue",alpha=.5)
        surface3d(Murder,HSGrad,as.matrix(stateheightCat2),col="blue",alpha=.5)
        surface3d(Murder,HSGrad,as.matrix(stateheightCat3),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
      }else if (input$expTypes3 == 6){
        par3d(scale=c(1,.5,2),cex=.5)
        points3d(states$Murder,states$HSGrad,states$LifeExp,col = colorstateCat)
        surface3d(Murder,HSGrad,as.matrix(stateheightCatInt1),col="blue",alpha=.5)
        surface3d(Murder,HSGrad,as.matrix(stateheightCatInt2),col="blue",alpha=.5)
        surface3d(Murder,HSGrad,as.matrix(stateheightCatInt3),col="blue",alpha=.5)
        axes3d()
        title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
      }
    }
    rglwidget()
  })

  output$responseVar <- renderPrint({
    if (input$dataset == "iris"){
      paste("Petal Area")
    }else if (input$dataset == "mtcars"){
      paste("Miles Per Gallon")
    }else if (input$dataset == "state.x77"){
      paste("Life Expectancy")
    }
  })

  output$modelEQ <- renderPrint({
    if (input$dataset == "iris"){
      if (input$expTypes1 == 1){
        summary(irisfit)$coefficients
      }else if (input$expTypes1 == 2){
        summary(fitIrisInt)$coefficients
      }else if (input$expTypes1 == 3){
        summary(fitIrisCat)$coefficients
      }else if (input$expTypes1 == 5){
        paste("No Model")
      }else if (input$expTypes1 == 6){
        summary(fitIrisCatInt)$coefficients
      }else{
        summary(fitIrisCat)$coefficients
      }
    }else if (input$dataset == "mtcars"){
      if (input$expTypes2 == 1){
        summary(carsfit)$coefficients
      }else if (input$expTypes2 == 2){
        summary(fitcarsInt)$coefficients
      }else if (input$expTypes2 == 3){
        summary(fitcarsCat)$coefficients
      }else if (input$expTypes2 == 5){
        paste("No Model")
      }else if (input$expTypes2 == 6){
        summary(fitcarsCatInt)$coefficients
      }else{
        summary(fitcarsCat)$coefficients
      }
    }else if (input$dataset == "state.x77"){
      if (input$expTypes3 == 1){
        summary(fitstate)$coefficients
      }else if (input$expTypes3 == 2){
        summary(fitstateInt)$coefficients
      }else if (input$expTypes3 == 3){
        summary(fitstateCat)$coefficients
      }else if (input$expTypes3 == 5){
        paste("No Model")
      }else if (input$expTypes3 == 6){
        summary(fitstateCatInt)$coefficients
      }else{
        summary(fitstateCat)$coefficients
      }
    }
  })


  output$modelRsq <- renderText({
    if (input$dataset == "iris"){
      if (input$expTypes1 == 1){
        summary(irisfit)$adj.r.squared
      }else if (input$expTypes1 == 2){
        summary(fitIrisInt)$adj.r.squared
      }else if (input$expTypes1 == 3){
        summary(fitIrisCat)$adj.r.squared
      }else if (input$expTypes1 == 5){
        paste("No Model")
      }else if (input$expTypes1 == 6){
        summary(fitIrisCatInt)$adj.r.squared
      }else{
        summary(fitIrisCat)$adj.r.squared
      }
    }else if (input$dataset == "mtcars"){
      if (input$expTypes2 == 1){
        summary(carsfit)$adj.r.squared
      }else if (input$expTypes2 == 2){
        summary(fitcarsInt)$adj.r.squared
      }else if (input$expTypes2 == 3){
        summary(fitcarsCat)$adj.r.squared
      }else if (input$expTypes2 == 5){
        paste("No Model")
      }else if (input$expTypes2 == 6){
        summary(fitcarsCatInt)$adj.r.squared
      }else{
        summary(fitcarsCat)$adj.r.squared
      }
    }else if (input$dataset == "state.x77"){
      if (input$expTypes3 == 1){
        summary(fitstate)$adj.r.squared
      }else if (input$expTypes3 == 2){
        summary(fitstateInt)$adj.r.squared
      }else if (input$expTypes3 == 3){
        summary(fitstateCat)$adj.r.squared
      }else if (input$expTypes3 == 5){
        paste("No Model")
      }else if (input$expTypes3 == 6){
        summary(fitstateCatInt)$adj.r.squared
      }else{
        summary(fitstateCat)$adj.r.squared
      }
    }
  })

  #############2D Tab#########
   output$cat2d <- renderPlot({

    if (input$dataset1 == "iris"){
      if (input$interact2D == "yes"){
        pAonsL <- lm(Petal.Area~Sepal.Length * Species,dfiris)
        setosa = coef(pAonsL)[c(1,2)]
        versicolor = c(coef(pAonsL)[1] + coef(pAonsL)[3], coef(pAonsL)[2] + coef(pAonsL)[5])
        virginica = c(coef(pAonsL)[1] + coef(pAonsL)[4], coef(pAonsL)[2] + coef(pAonsL)[6])

        plot(dfiris$Sepal.Length,dfiris$Petal.Area,xlab="Sepal Length",ylab="Petal Area",
             main="Petal Area on Length by Species",col=colorsirisCat,pch = 19)

        abline(setosa,col="red")
        abline(versicolor,col="blue")
        abline(virginica,col="darkgreen")

      }else if (input$interact2D == "no"){
        pAonsL <- lm(Petal.Area~Sepal.Length + Species,dfiris)
        setosa = coef(pAonsL)[c(1,2)]
        versicolor = c(coef(pAonsL)[1] + coef(pAonsL)[3], coef(pAonsL)[2])
        virginica = c(coef(pAonsL)[1] + coef(pAonsL)[4], coef(pAonsL)[2])

        plot(dfiris$Sepal.Length,dfiris$Petal.Area,xlab="Sepal Length",ylab="Petal Area",
             main="Petal Area on Length by Species",col=colorsirisCat,pch = 19)

        abline(setosa,col="red")
        abline(versicolor,col="blue")
        abline(virginica,col="darkgreen")

      }else{
        pAonsL <- lm(Petal.Area~Sepal.Length,dfiris)

        plot(dfiris$Sepal.Length,dfiris$Petal.Area,xlab="Sepal Length",ylab="Petal Area",
             main="Petal Area on Length",col=colorsirisCat,pch = 19)
        abline(pAonsL)
      }

#       plot(dfiris$Sepal.Length,dfiris$Petal.Area,xlab="Sepal Length",ylab="Petal Area",
#            main="Petal Area on Length by Species",col=colorsirisCat,pch = 19)
#
#       abline(setosa,col="red")
#       abline(versicolor,col="blue")
#       abline(virginica,col="darkgreen")
      legend("topleft",legend = levels(dfiris$Species),col=c("red","blue","darkgreen"),pch = 19)
    }else if (input$dataset1 == "mtcars"){
      if (input$interact2D == "yes"){
        mpgonwt <- lm(mpg~wt*am,dfcars)
        automatic = coef(mpgonwt)[c(1,2)]
        manual = c(coef(mpgonwt)[1] + coef(mpgonwt)[3], coef(mpgonwt)[2]+coef(mpgonwt)[4])

        plot(dfcars$wt,dfcars$mpg,xlab="Weight (lb/1000)",ylab="Miles/(US) gallon",
             main="Automobile MPG on Weight by Transmission",col=colorcarsCat,pch = 19)

        abline(automatic,col="green")
        abline(manual,col="red")

      }else if (input$interact2D == "no"){
        mpgonwt <- lm(mpg~wt+am,dfcars)
        automatic = coef(mpgonwt)[c(1,2)]
        manual = c(coef(mpgonwt)[1] + coef(mpgonwt)[3], coef(mpgonwt)[2])

        plot(dfcars$wt,dfcars$mpg,xlab="Weight (lb/1000)",ylab="Miles/(US) gallon",
             main="Automobile MPG on Weight by Transmission",col=colorcarsCat,pch = 19)

        abline(automatic,col="green")
        abline(manual,col="red")

      } else {
        mpgonwt <- lm(mpg~wt,dfcars)

        plot(dfcars$wt,dfcars$mpg,xlab="Weight (lb/1000)",ylab="Miles/(US) gallon",
             main="Automobile MPG on Weight",col=colorcarsCat,pch = 19)

        abline(mpgonwt)
      }

#       plot(dfcars$wt,dfcars$mpg,xlab="Weight (lb/1000)",ylab="Miles/(US) gallon",
#            main="Automobile MPG on Weight by Transmission",col=colorcarsCat,pch = 19)
#
#       abline(automatic,col="green")
#       abline(manual,col="red")
      legend("topright",legend = levels(dfcars$am),col=c("green","red"),pch = 19,cex=.8)
    }else if (input$dataset1 == "state.x77"){
      if (input$interact2D == "yes")
      {
        lEonHg <- lm(LifeExp~HSGrad*Region,states)
        midwest = coef(lEonHg)[c(1,2)]
        northeast = c(coef(lEonHg)[1] + coef(lEonHg)[3],coef(lEonHg)[2] + coef(lEonHg)[6])
        south = c(coef(lEonHg)[1] + coef(lEonHg)[4],coef(lEonHg)[2] + coef(lEonHg)[7])
        west = c(coef(lEonHg)[1] + coef(lEonHg)[5],coef(lEonHg)[2] + coef(lEonHg)[8])

        plot(states$HSGrad,states$LifeExp,xlab="High School Graduation Rate",ylab="Life Expectancy",
             main="High School Graduation Rate on Life Expectancy by Region",col=colorstateCat,pch = 19)

        abline(midwest,col="blue")
        abline(northeast,col="red")
        abline(south,col="green")
        abline(west,col="black")

      }else if (input$interact2D == "no"){
        lEonHg <- lm(LifeExp~HSGrad+Region,states)
        midwest = coef(lEonHg)[c(1,2)]
        northeast = c(coef(lEonHg)[1] + coef(lEonHg)[3],coef(lEonHg)[2])
        south = c(coef(lEonHg)[1] + coef(lEonHg)[4],coef(lEonHg)[2])
        west = c(coef(lEonHg)[1] + coef(lEonHg)[5],coef(lEonHg)[2])

        plot(states$HSGrad,states$LifeExp,xlab="High School Graduation Rate",ylab="Life Expectancy",
             main="High School Graduation Rate on Life Expectancy by Region",col=colorstateCat,pch = 19)

        abline(midwest,col="blue")
        abline(northeast,col="red")
        abline(south,col="green")
        abline(west,col="black")
      }else{
        lEonHg <- lm(LifeExp~HSGrad,states)

        plot(states$HSGrad,states$LifeExp,xlab="High School Graduation Rate",ylab="Life Expectancy",
             main="High School Graduation Rate on Life Expectancy",col=colorstateCat,pch = 19)

        abline(lEonHg)
      }

#       plot(states$HSGrad,states$LifeExp,xlab="High School Graduation Rate",ylab="Life Expectancy",
#            main="High School Graduation Rate on Life Expectancy by Region",col=colorstateCat,pch = 19)
#
#       abline(midwest,col="blue")
#       abline(northeast,col="red")
#       abline(south,col="green")
#       abline(west,col="black")

      legend("topleft",legend = levels(states$Region),col=c("blue","red","green","black"),pch = 19,cex=.8)
    }

  })

  output$catResp <- renderPrint({
    if (input$dataset1 == "iris"){
      paste("Petal Area")
    }else if (input$dataset1 == "mtcars"){
      paste("Miles Per Gallon")
    }else if (input$dataset1 == "state.x77"){
      paste("Life Expectancy")
    }
  })

  output$catModel <- renderPrint({
    if (input$interact2D == "no"){
      if (input$dataset1 == "iris"){
        pAonsL <- lm(Petal.Area~Sepal.Length+Species,dfiris)
        summary(pAonsL)$coefficients
      }else if (input$dataset1 == "mtcars"){
        mpgonwt <- lm(mpg~wt+am,dfcars)
        summary(mpgonwt)$coefficients
      }else if (input$dataset1 == "state.x77"){
        lEonHg <- lm(LifeExp~HSGrad+Region,states)
        summary(lEonHg)$coefficients
      }
    }else if (input$interact2D == "yes"){
      if (input$dataset1 == "iris"){
        pAonsL <- lm(Petal.Area~Sepal.Length*Species,dfiris)
        summary(pAonsL)$coefficients
      }else if (input$dataset1 == "mtcars"){
        mpgonwt <- lm(mpg~wt*am,dfcars)
        summary(mpgonwt)$coefficients
      }else if (input$dataset1 == "state.x77"){
        lEonHg <- lm(LifeExp~HSGrad*Region,states)
        summary(lEonHg)$coefficients
      }
    }else{
      if (input$dataset1 == "iris"){
        pAonsL <- lm(Petal.Area~Sepal.Length,dfiris)
        summary(pAonsL)$coefficients
      }else if (input$dataset1 == "mtcars"){
        mpgonwt <- lm(mpg~wt,dfcars)
        summary(mpgonwt)$coefficients
      }else if (input$dataset1 == "state.x77"){
        lEonHg <- lm(LifeExp~HSGrad,states)
        summary(lEonHg)$coefficients
      }
    }

  })

  output$catRsq <- renderText({
    if (input$interact2D == "no"){
      if (input$dataset1 == "iris"){
        pAonsL <- lm(Petal.Area~Sepal.Length+Species,dfiris)
        summary(pAonsL)$adj.r.sq
      }else if (input$dataset1 == "mtcars"){
        mpgonwt <- lm(mpg~wt+am,dfcars)
        summary(mpgonwt)$adj.r.sq
      }else if (input$dataset1 == "state.x77"){
        lEonHg <- lm(LifeExp~HSGrad+Region,states)
        summary(lEonHg)$adj.r.sq
      }
    }else if (input$interact2D == "yes"){
      if (input$dataset1 == "iris"){
        pAonsL <- lm(Petal.Area~Sepal.Length*Species,dfiris)
        summary(pAonsL)$adj.r.sq
      }else if (input$dataset1 == "mtcars"){
        mpgonwt <- lm(mpg~wt*am,dfcars)
        summary(mpgonwt)$adj.r.sq
      }else if (input$dataset1 == "state.x77"){
        lEonHg <- lm(LifeExp~HSGrad*Region,states)
        summary(lEonHg)$adj.r.sq
      }
    }else{
      if (input$dataset1 == "iris"){
        pAonsL <- lm(Petal.Area~Sepal.Length,dfiris)
        summary(pAonsL)$adj.r.sq
      }else if (input$dataset1 == "mtcars"){
        mpgonwt <- lm(mpg~wt,dfcars)
        summary(mpgonwt)$adj.r.sq
      }else if (input$dataset1 == "state.x77"){
        lEonHg <- lm(LifeExp~HSGrad,states)
        summary(lEonHg)$adj.r.sq
      }
    }

  })
})