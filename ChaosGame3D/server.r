# ----------------------------------------
#  App Title: Chaos Game -- 3 Dimensions
#     Author: Jimmy Doi
# ----------------------------------------

###################################################################
# Tetrahedron
###################################################################

tetra.gen <- function(wt){
    weight <- wt

    len <- 50000

    # loci matrix to contain all endpoints
		loci <- matrix(NA,ncol=4,nrow=4)

		loci[1,] <- c(1,1,0,-1/sqrt(2))
		loci[2,] <- c(2,-1,0,-1/sqrt(2))
		loci[3,] <- c(3,0,1,1/sqrt(2))
  	loci[4,] <- c(4,0,-1,1/sqrt(2))

    # vertices contains all random vertex points
    vertices <- runif(len)
    vertices[which(vertices>3/4)]<- 4
    vertices[which(2/4<vertices & vertices<=3/4)]<- 3
    vertices[which(1/4<vertices & vertices<=2/4)]<- 2
    vertices[which(vertices<=1/4)]<- 1

    coords <- matrix(NA,ncol=4,nrow=(len+1))

    coords[1,] <- c(runif(1),runif(1)*sqrt(3)/2,runif(1)*sqrt(6)/3,0)

		for (i in 1:len){
		  row <- i+1
      spot <- which(loci[,1]==vertices[i])
      x <- loci[spot,2]
      y <- loci[spot,3]
      z <- loci[spot,4]
      x.new <- weight*x + (1-weight)*coords[i,1]
      y.new <- weight*y + (1-weight)*coords[i,2]
      z.new <- weight*z + (1-weight)*coords[i,3]
	    coords[row,]<-c(x.new,y.new,z.new,0)
		}

    for (i in 1:(len+1)){
      d1 <- sqrt((coords[i,1]-loci[1,2])**2 + (coords[i,2]-loci[1,3])**2 + (coords[i,3]-loci[1,4])**2)
      d2 <- sqrt((coords[i,1]-loci[2,2])**2 + (coords[i,2]-loci[2,3])**2 + (coords[i,3]-loci[2,4])**2)
      d3 <- sqrt((coords[i,1]-loci[3,2])**2 + (coords[i,2]-loci[3,3])**2 + (coords[i,3]-loci[3,4])**2)
      d4 <- sqrt((coords[i,1]-loci[4,2])**2 + (coords[i,2]-loci[4,3])**2 + (coords[i,3]-loci[4,4])**2)
      if (min(d1,d2,d3,d4)==d1) coords[i,4]<-1
      if (min(d1,d2,d3,d4)==d2) coords[i,4]<-2
      if (min(d1,d2,d3,d4)==d3) coords[i,4]<-3
      if (min(d1,d2,d3,d4)==d4) coords[i,4]<-4
    }

    return(list(loci,vertices,coords))
}

###################################################################
# Cube
###################################################################

cube.gen <- function(wt){
    weight <- wt

    len <- 50000

    # loci matrix to contain all endpoints
		loci <- matrix(NA,ncol=4,nrow=20)

		loci[1,]  <-  c(1,0,0,0)
		loci[2,]  <-  c(2,.5,0,0)
		loci[3,]  <-  c(3,1,0,0)
  	loci[4,]  <-  c(4,1,.5,0)
    loci[5,]  <-  c(5,1,1,0)
    loci[6,]  <-  c(6,.5,1,0)
    loci[7,]  <-  c(7,0,1,0)
    loci[8,]  <-  c(8,0,.5,0)
    loci[9,]  <-  c(9,1,0,.5)
    loci[10,] <- c(10,0,0,.5)
    loci[11,] <- c(11,0,1,.5)
    loci[12,] <- c(12,1,1,.5)
  	loci[13,] <- c(13,0,0,1)
		loci[14,] <- c(14,.5,0,1)
		loci[15,] <- c(15,1,0,1)
  	loci[16,] <- c(16,1,.5,1)
    loci[17,] <- c(17,1,1,1)
    loci[18,] <- c(18,.5,1,1)
    loci[19,] <- c(19,0,1,1)
    loci[20,] <- c(20,0,.5,1)

    # vertices contains all random vertex points
    vertices <- runif(len)
    vertices[which(vertices>19/20)]<- 20
    vertices[which(18/20<vertices & vertices<=19/20)]<- 19
    vertices[which(17/20<vertices & vertices<=18/20)]<- 18
    vertices[which(16/20<vertices & vertices<=17/20)]<- 17
    vertices[which(15/20<vertices & vertices<=16/20)]<- 16
    vertices[which(14/20<vertices & vertices<=15/20)]<- 15
    vertices[which(13/20<vertices & vertices<=14/20)]<- 14
    vertices[which(12/20<vertices & vertices<=13/20)]<- 13
    vertices[which(11/20<vertices & vertices<=12/20)]<- 12
    vertices[which(10/20<vertices & vertices<=11/20)]<- 11
    vertices[which(9/20<vertices & vertices<= 10/20)]<- 10
    vertices[which(8/20<vertices & vertices<= 9/20)]<- 9
    vertices[which(7/20<vertices & vertices<= 8/20)]<- 8
    vertices[which(6/20<vertices & vertices<= 7/20)]<- 7
    vertices[which(5/20<vertices & vertices<= 6/20)]<- 6
    vertices[which(4/20<vertices & vertices<= 5/20)]<- 5
    vertices[which(3/20<vertices & vertices<= 4/20)]<- 4
    vertices[which(2/20<vertices & vertices<= 3/20)]<- 3
    vertices[which(1/20<vertices & vertices<= 2/20)]<- 2
    vertices[which(vertices<=1/20)]<- 1

    coords <- matrix(NA,ncol=4,nrow=(len+1))

    coords[1,] <- c(runif(1),runif(1),runif(1),0)

		for (i in 1:len){
		  row <- i+1
      spot <- which(loci[,1]==vertices[i])
      x <- loci[spot,2]
      y <- loci[spot,3]
      z <- loci[spot,4]
      x.new <- weight*x + (1-weight)*coords[i,1]
      y.new <- weight*y + (1-weight)*coords[i,2]
      z.new <- weight*z + (1-weight)*coords[i,3]
	    coords[row,]<-c(x.new,y.new,z.new,0)
		}

    for (i in 1:(len+1)){
      d1 <- sqrt((coords[i,1]-loci[1,2])**2 + (coords[i,2]- loci[1,3])**2 + (coords[i,3]- loci[1,4])**2)
      d2 <- sqrt((coords[i,1]-loci[2,2])**2 + (coords[i,2]- loci[2,3])**2 + (coords[i,3]- loci[2,4])**2)
      d3 <- sqrt((coords[i,1]-loci[3,2])**2 + (coords[i,2]- loci[3,3])**2 + (coords[i,3]- loci[3,4])**2)
      d4 <- sqrt((coords[i,1]-loci[4,2])**2 + (coords[i,2]- loci[4,3])**2 + (coords[i,3]- loci[4,4])**2)
      d5 <- sqrt((coords[i,1]-loci[5,2])**2 + (coords[i,2]- loci[5,3])**2 + (coords[i,3]- loci[5,4])**2)
      d6 <- sqrt((coords[i,1]-loci[6,2])**2 + (coords[i,2]- loci[6,3])**2 + (coords[i,3]- loci[6,4])**2)
      d7 <- sqrt((coords[i,1]-loci[7,2])**2 + (coords[i,2]- loci[7,3])**2 + (coords[i,3]- loci[7,4])**2)
      d8 <- sqrt((coords[i,1]-loci[8,2])**2 + (coords[i,2]- loci[8,3])**2 + (coords[i,3]- loci[8,4])**2)
      d9 <- sqrt((coords[i,1]-loci[9,2])**2 + (coords[i,2]- loci[9,3])**2 + (coords[i,3]- loci[9,4])**2)
      d10<- sqrt((coords[i,1]-loci[10,2])**2 + (coords[i,2]-loci[10,3])**2 + (coords[i,3]-loci[10,4])**2)
      d11<- sqrt((coords[i,1]-loci[11,2])**2 + (coords[i,2]-loci[11,3])**2 + (coords[i,3]-loci[11,4])**2)
      d12<- sqrt((coords[i,1]-loci[12,2])**2 + (coords[i,2]-loci[12,3])**2 + (coords[i,3]-loci[12,4])**2)
      d13<- sqrt((coords[i,1]-loci[13,2])**2 + (coords[i,2]-loci[13,3])**2 + (coords[i,3]-loci[13,4])**2)
      d14<- sqrt((coords[i,1]-loci[14,2])**2 + (coords[i,2]-loci[14,3])**2 + (coords[i,3]-loci[14,4])**2)
      d15<- sqrt((coords[i,1]-loci[15,2])**2 + (coords[i,2]-loci[15,3])**2 + (coords[i,3]-loci[15,4])**2)
      d16<- sqrt((coords[i,1]-loci[16,2])**2 + (coords[i,2]-loci[16,3])**2 + (coords[i,3]-loci[16,4])**2)
      d17<- sqrt((coords[i,1]-loci[17,2])**2 + (coords[i,2]-loci[17,3])**2 + (coords[i,3]-loci[17,4])**2)
      d18<- sqrt((coords[i,1]-loci[18,2])**2 + (coords[i,2]-loci[18,3])**2 + (coords[i,3]-loci[18,4])**2)
      d19<- sqrt((coords[i,1]-loci[19,2])**2 + (coords[i,2]-loci[19,3])**2 + (coords[i,3]-loci[19,4])**2)
      d20<- sqrt((coords[i,1]-loci[20,2])**2 + (coords[i,2]-loci[20,3])**2 + (coords[i,3]-loci[20,4])**2)
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d1) coords[i,4]<-1
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d2) coords[i,4]<-2
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d3) coords[i,4]<-3
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d4) coords[i,4]<-4
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d5) coords[i,4]<-5
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d6) coords[i,4]<-6
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d7) coords[i,4]<-7
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d8) coords[i,4]<-8
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d9) coords[i,4]<-9
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d10) coords[i,4]<-10
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d11) coords[i,4]<-11
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d12) coords[i,4]<-12
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d13) coords[i,4]<-13
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d14) coords[i,4]<-14
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d15) coords[i,4]<-15
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d16) coords[i,4]<-16
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d17) coords[i,4]<-17
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d18) coords[i,4]<-18
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d19) coords[i,4]<-19
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d20) coords[i,4]<-20
    }

    return(list(loci,vertices,coords))
}

###################################################################
# Dodecahedron
###################################################################

dodec.gen <- function(wt){
    weight <- wt

    len <- 50000

    # loci matrix to contain all endpoints
		loci <- matrix(NA,ncol=4,nrow=20)

    psi <- (1+sqrt(5))/2

		loci[1,]  <-   c(1,1,-1,1)
		loci[2,]  <-   c(2,1,1,-1)
		loci[3,]  <-   c(3,psi,0,-1/psi)
  	loci[4,]  <-   c(4,1,1,1)
    loci[5,]  <-   c(5,-1,1,1)
    loci[6,]  <-   c(6,-1,1,-1)
    loci[7,]  <-   c(7,1/psi,psi,0)
    loci[8,]  <-   c(8,0,1/psi,psi)
    loci[9,]  <-   c(9,-psi,0,-1/psi)
    loci[10,] <-  c(10,0,-1/psi,-psi)
    loci[11,] <-  c(11,0,-1/psi,psi)
    loci[12,] <-  c(12,0,1/psi,-psi)
  	loci[13,] <-  c(13,-1/psi,psi,0)
		loci[14,] <-  c(14,-psi,0,1/psi)
		loci[15,] <-  c(15,-1,-1,1)
  	loci[16,] <-  c(16,-1/psi,-psi,0)
    loci[17,] <-  c(17,psi,0,1/psi)
    loci[18,] <-  c(18,1,-1,-1)
    loci[19,] <-  c(19,1/psi,-psi,0)
    loci[20,] <-  c(20,-1,-1,-1)

    # vertices contains all random vertex points
    vertices <- runif(len)
    vertices[which(vertices>19/20)]<- 20
    vertices[which(18/20<vertices & vertices<=19/20)]<- 19
    vertices[which(17/20<vertices & vertices<=18/20)]<- 18
    vertices[which(16/20<vertices & vertices<=17/20)]<- 17
    vertices[which(15/20<vertices & vertices<=16/20)]<- 16
    vertices[which(14/20<vertices & vertices<=15/20)]<- 15
    vertices[which(13/20<vertices & vertices<=14/20)]<- 14
    vertices[which(12/20<vertices & vertices<=13/20)]<- 13
    vertices[which(11/20<vertices & vertices<=12/20)]<- 12
    vertices[which(10/20<vertices & vertices<=11/20)]<- 11
    vertices[which(9/20<vertices & vertices<= 10/20)]<- 10
    vertices[which(8/20<vertices & vertices<= 9/20)]<- 9
    vertices[which(7/20<vertices & vertices<= 8/20)]<- 8
    vertices[which(6/20<vertices & vertices<= 7/20)]<- 7
    vertices[which(5/20<vertices & vertices<= 6/20)]<- 6
    vertices[which(4/20<vertices & vertices<= 5/20)]<- 5
    vertices[which(3/20<vertices & vertices<= 4/20)]<- 4
    vertices[which(2/20<vertices & vertices<= 3/20)]<- 3
    vertices[which(1/20<vertices & vertices<= 2/20)]<- 2
    vertices[which(vertices<=1/20)]<- 1

    coords <- matrix(NA,ncol=4,nrow=(len+1))

    coords[1,] <- c(runif(1),runif(1),runif(1),0)

		for (i in 1:len){
		  row <- i+1
      spot <- which(loci[,1]==vertices[i])
      x <- loci[spot,2]
      y <- loci[spot,3]
      z <- loci[spot,4]
      x.new <- weight*x + (1-weight)*coords[i,1]
      y.new <- weight*y + (1-weight)*coords[i,2]
      z.new <- weight*z + (1-weight)*coords[i,3]
	    coords[row,]<-c(x.new,y.new,z.new,0)
		}

        for (i in 1:(len+1)){
      d1 <- sqrt((coords[i,1]-loci[1,2])**2 + (coords[i,2]- loci[1,3])**2 + (coords[i,3]- loci[1,4])**2)
      d2 <- sqrt((coords[i,1]-loci[2,2])**2 + (coords[i,2]- loci[2,3])**2 + (coords[i,3]- loci[2,4])**2)
      d3 <- sqrt((coords[i,1]-loci[3,2])**2 + (coords[i,2]- loci[3,3])**2 + (coords[i,3]- loci[3,4])**2)
      d4 <- sqrt((coords[i,1]-loci[4,2])**2 + (coords[i,2]- loci[4,3])**2 + (coords[i,3]- loci[4,4])**2)
      d5 <- sqrt((coords[i,1]-loci[5,2])**2 + (coords[i,2]- loci[5,3])**2 + (coords[i,3]- loci[5,4])**2)
      d6 <- sqrt((coords[i,1]-loci[6,2])**2 + (coords[i,2]- loci[6,3])**2 + (coords[i,3]- loci[6,4])**2)
      d7 <- sqrt((coords[i,1]-loci[7,2])**2 + (coords[i,2]- loci[7,3])**2 + (coords[i,3]- loci[7,4])**2)
      d8 <- sqrt((coords[i,1]-loci[8,2])**2 + (coords[i,2]- loci[8,3])**2 + (coords[i,3]- loci[8,4])**2)
      d9 <- sqrt((coords[i,1]-loci[9,2])**2 + (coords[i,2]- loci[9,3])**2 + (coords[i,3]- loci[9,4])**2)
      d10<- sqrt((coords[i,1]-loci[10,2])**2 + (coords[i,2]-loci[10,3])**2 + (coords[i,3]-loci[10,4])**2)
      d11<- sqrt((coords[i,1]-loci[11,2])**2 + (coords[i,2]-loci[11,3])**2 + (coords[i,3]-loci[11,4])**2)
      d12<- sqrt((coords[i,1]-loci[12,2])**2 + (coords[i,2]-loci[12,3])**2 + (coords[i,3]-loci[12,4])**2)
      d13<- sqrt((coords[i,1]-loci[13,2])**2 + (coords[i,2]-loci[13,3])**2 + (coords[i,3]-loci[13,4])**2)
      d14<- sqrt((coords[i,1]-loci[14,2])**2 + (coords[i,2]-loci[14,3])**2 + (coords[i,3]-loci[14,4])**2)
      d15<- sqrt((coords[i,1]-loci[15,2])**2 + (coords[i,2]-loci[15,3])**2 + (coords[i,3]-loci[15,4])**2)
      d16<- sqrt((coords[i,1]-loci[16,2])**2 + (coords[i,2]-loci[16,3])**2 + (coords[i,3]-loci[16,4])**2)
      d17<- sqrt((coords[i,1]-loci[17,2])**2 + (coords[i,2]-loci[17,3])**2 + (coords[i,3]-loci[17,4])**2)
      d18<- sqrt((coords[i,1]-loci[18,2])**2 + (coords[i,2]-loci[18,3])**2 + (coords[i,3]-loci[18,4])**2)
      d19<- sqrt((coords[i,1]-loci[19,2])**2 + (coords[i,2]-loci[19,3])**2 + (coords[i,3]-loci[19,4])**2)
      d20<- sqrt((coords[i,1]-loci[20,2])**2 + (coords[i,2]-loci[20,3])**2 + (coords[i,3]-loci[20,4])**2)
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d1) coords[i,4]<-1
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d2) coords[i,4]<-2
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d3) coords[i,4]<-3
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d4) coords[i,4]<-4
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d5) coords[i,4]<-5
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d6) coords[i,4]<-6
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d7) coords[i,4]<-7
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d8) coords[i,4]<-8
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d9) coords[i,4]<-9
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d10) coords[i,4]<-10
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d11) coords[i,4]<-11
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d12) coords[i,4]<-12
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d13) coords[i,4]<-13
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d14) coords[i,4]<-14
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d15) coords[i,4]<-15
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d16) coords[i,4]<-16
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d17) coords[i,4]<-17
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d18) coords[i,4]<-18
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d19) coords[i,4]<-19
      if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d20) coords[i,4]<-20
    }

    return(list(loci,vertices,coords))
}

##############################################################################
# Shiny Server Contents
##############################################################################


shinyServer(function(input, output, session) {

  updateButton(session, "gen", style = "primary", size = "default", disabled = FALSE)

  all.list <- reactive({
        withProgress(message = 'Generating Coordinates', style = 'notification', value = 1, {
    if (input$shape == "tetra") {
      return(tetra.gen(input$dist.tetra*(input$gen>-1)))
      }
    if (input$shape == "cube") {
      return(cube.gen(input$dist.cube*(input$gen>-1)))
      }
    if (input$shape == "dodec") {
      return(dodec.gen(input$dist.dodec*(input$gen>-1)))
      }
    }) #CLOSE withProgress

  })

  output$sctPlot <- renderRglwidget({
    withProgress(message = 'Please wait up to 15 seconds', style = 'notification', value = 1, {
    try(rgl.close())
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
    loci      <- all.list()[[1]]
    vertices  <- all.list()[[2]]
    coords    <- all.list()[[3]]

    if (max(coords[,4]==4)) {
    set1 <- coords[coords[,4]==1,1:3]
    set2 <- coords[coords[,4]==2,1:3]
    set3 <- coords[coords[,4]==3,1:3]
    set4 <- coords[coords[,4]==4,1:3]

    col1 <- brewer.pal(n = 12, name = "Paired")[2]
    col2 <- brewer.pal(n = 12, name = "Paired")[4]
    col3 <- brewer.pal(n = 12, name = "Paired")[8]
    col4 <- brewer.pal(n = 12, name = "Paired")[10]

    points3d(set1[1:round(nrow(set1)*(input$pts/100),0),1],
             set1[1:round(nrow(set1)*(input$pts/100),0),2],
             set1[1:round(nrow(set1)*(input$pts/100),0),3],
             size = 1.25,col=col1)

    points3d(set2[1:round(nrow(set2)*(input$pts/100),0),1],
             set2[1:round(nrow(set2)*(input$pts/100),0),2],
             set2[1:round(nrow(set2)*(input$pts/100),0),3],
             size = 1.25,col=col2)

    points3d(set3[1:round(nrow(set3)*(input$pts/100),0),1],
             set3[1:round(nrow(set3)*(input$pts/100),0),2],
             set3[1:round(nrow(set3)*(input$pts/100),0),3],
             size = 1.25,col=col3)

    points3d(set4[1:round(nrow(set4)*(input$pts/100),0),1],
             set4[1:round(nrow(set4)*(input$pts/100),0),2],
             set4[1:round(nrow(set4)*(input$pts/100),0),3],
             size = 1.25,col=col4)
    }

    if (max(coords[,4]==20)) {
    set1 <- coords[coords[,4]==1,1:3]
    set2 <- coords[coords[,4]==2,1:3]
    set3 <- coords[coords[,4]==3,1:3]
    set4 <- coords[coords[,4]==4,1:3]
    set5 <- coords[coords[,4]==5,1:3]
    set6 <- coords[coords[,4]==6,1:3]
    set7 <- coords[coords[,4]==7,1:3]
    set8 <- coords[coords[,4]==8,1:3]
    set9 <- coords[coords[,4]==9,1:3]
    set10<- coords[coords[,4]==10,1:3]
    set11<- coords[coords[,4]==11,1:3]
    set12<- coords[coords[,4]==12,1:3]
    set13<- coords[coords[,4]==13,1:3]
    set14<- coords[coords[,4]==14,1:3]
    set15<- coords[coords[,4]==15,1:3]
    set16<- coords[coords[,4]==16,1:3]
    set17<- coords[coords[,4]==17,1:3]
    set18<- coords[coords[,4]==18,1:3]
    set19<- coords[coords[,4]==19,1:3]
    set20<- coords[coords[,4]==20,1:3]

    col1 <- brewer.pal(n = 12, name = "Paired")[2]  #dark blue
    col2 <- brewer.pal(n = 12, name = "Paired")[4]  #dark green
    col3 <- brewer.pal(n = 12, name = "Paired")[6]  #red
    col4 <- brewer.pal(n = 12, name = "Paired")[8]  #orange
    col5 <- brewer.pal(n = 11, name = "RdBu")[11]   #DarkBl
    col6 <- brewer.pal(n = 12, name = "Paired")[12] #brown
    col7 <- brewer.pal(n = 8, name = "Dark2")[8]    #grey
    col8 <- brewer.pal(n = 8, name = "Accent")[6]   #dark pink
    col20 <- col8
    col19 <- col7
    col18 <- col6
    col17 <- col5
    col16 <- col4
    col15 <- col3
    col14 <- col2
    col13 <- col1
    col12 <- brewer.pal(n = 11, name = "BrBG")[9]   #SoftBl
    col9  <- brewer.pal(n = 11, name = "RdBu")[11]  #DarkBl
    col11 <- brewer.pal(n = 11, name = "PiYG")[1]   #Maroon
    col10 <- brewer.pal(n = 11, name = "PiYG")[11]  #DarkGrn

    points3d(set1[1:round(nrow(set1)*(input$pts/100),0),1],
             set1[1:round(nrow(set1)*(input$pts/100),0),2],
             set1[1:round(nrow(set1)*(input$pts/100),0),3],
             size = 1.25,col=col1)

    points3d(set2[1:round(nrow(set2)*(input$pts/100),0),1],
             set2[1:round(nrow(set2)*(input$pts/100),0),2],
             set2[1:round(nrow(set2)*(input$pts/100),0),3],
             size = 1.25,col=col2)

    points3d(set3[1:round(nrow(set3)*(input$pts/100),0),1],
             set3[1:round(nrow(set3)*(input$pts/100),0),2],
             set3[1:round(nrow(set3)*(input$pts/100),0),3],
             size = 1.25,col=col3)

    points3d(set4[1:round(nrow(set4)*(input$pts/100),0),1],
             set4[1:round(nrow(set4)*(input$pts/100),0),2],
             set4[1:round(nrow(set4)*(input$pts/100),0),3],
             size = 1.25,col=col4)

    points3d(set5[1:round(nrow(set5)*(input$pts/100),0),1],
             set5[1:round(nrow(set5)*(input$pts/100),0),2],
             set5[1:round(nrow(set5)*(input$pts/100),0),3],
             size = 1.25,col=col5)

    points3d(set6[1:round(nrow(set6)*(input$pts/100),0),1],
             set6[1:round(nrow(set6)*(input$pts/100),0),2],
             set6[1:round(nrow(set6)*(input$pts/100),0),3],
             size = 1.25,col=col6)

    points3d(set7[1:round(nrow(set7)*(input$pts/100),0),1],
             set7[1:round(nrow(set7)*(input$pts/100),0),2],
             set7[1:round(nrow(set7)*(input$pts/100),0),3],
             size = 1.25,col=col7)

    points3d(set8[1:round(nrow(set8)*(input$pts/100),0),1],
             set8[1:round(nrow(set8)*(input$pts/100),0),2],
             set8[1:round(nrow(set8)*(input$pts/100),0),3],
             size = 1.25,col=col8)

    points3d(set9[1:round(nrow(set9)*(input$pts/100),0),1],
             set9[1:round(nrow(set9)*(input$pts/100),0),2],
             set9[1:round(nrow(set9)*(input$pts/100),0),3],
             size = 1.25,col=col9)

    points3d(set10[1:round(nrow(set10)*(input$pts/100),0),1],
             set10[1:round(nrow(set10)*(input$pts/100),0),2],
             set10[1:round(nrow(set10)*(input$pts/100),0),3],
             size = 1.25,col=col10)

    points3d(set11[1:round(nrow(set11)*(input$pts/100),0),1],
             set11[1:round(nrow(set11)*(input$pts/100),0),2],
             set11[1:round(nrow(set11)*(input$pts/100),0),3],
             size = 1.25,col=col11)

    points3d(set12[1:round(nrow(set12)*(input$pts/100),0),1],
             set12[1:round(nrow(set12)*(input$pts/100),0),2],
             set12[1:round(nrow(set12)*(input$pts/100),0),3],
             size = 1.25,col=col12)

    points3d(set13[1:round(nrow(set13)*(input$pts/100),0),1],
             set13[1:round(nrow(set13)*(input$pts/100),0),2],
             set13[1:round(nrow(set13)*(input$pts/100),0),3],
             size = 1.25,col=col13)

    points3d(set14[1:round(nrow(set14)*(input$pts/100),0),1],
             set14[1:round(nrow(set14)*(input$pts/100),0),2],
             set14[1:round(nrow(set14)*(input$pts/100),0),3],
             size = 1.25,col=col14)

    points3d(set15[1:round(nrow(set15)*(input$pts/100),0),1],
             set15[1:round(nrow(set15)*(input$pts/100),0),2],
             set15[1:round(nrow(set15)*(input$pts/100),0),3],
             size = 1.25,col=col15)

    points3d(set16[1:round(nrow(set16)*(input$pts/100),0),1],
             set16[1:round(nrow(set16)*(input$pts/100),0),2],
             set16[1:round(nrow(set16)*(input$pts/100),0),3],
             size = 1.25,col=col16)

    points3d(set17[1:round(nrow(set17)*(input$pts/100),0),1],
             set17[1:round(nrow(set17)*(input$pts/100),0),2],
             set17[1:round(nrow(set17)*(input$pts/100),0),3],
             size = 1.25,col=col17)

    points3d(set18[1:round(nrow(set18)*(input$pts/100),0),1],
             set18[1:round(nrow(set18)*(input$pts/100),0),2],
             set18[1:round(nrow(set18)*(input$pts/100),0),3],
             size = 1.25,col=col18)

    points3d(set19[1:round(nrow(set19)*(input$pts/100),0),1],
             set19[1:round(nrow(set19)*(input$pts/100),0),2],
             set19[1:round(nrow(set19)*(input$pts/100),0),3],
             size = 1.25,col=col19)

    points3d(set20[1:round(nrow(set20)*(input$pts/100),0),1],
             set20[1:round(nrow(set20)*(input$pts/100),0),2],
             set20[1:round(nrow(set20)*(input$pts/100),0),3],
             size = 1.25,col=col20)
    }
    points3d(loci[,2],loci[,3],loci[,4], size=6,col="red")
    rglwidget()
    }) #Close withProgress
    }) #Close withProgress
    })
})