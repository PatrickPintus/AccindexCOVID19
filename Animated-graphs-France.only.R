rm(list=ls())

base <- read.table(file="sp-pos-quot-fra-2021-03-10-17h20.csv",
                   sep=";",header=TRUE)
attach(base)

nobs    <- dim(base)[1]
nomvar <- attributes(base)$names

date.All <- jour[cl_age90==0]

##########################################
##########################################
### CASES
##########################################
##########################################

tests.0.d <- (T[cl_age90==0])
cases.0.d <- (P[cl_age90==0])


tests.0 <- cumsum(T[cl_age90==0])
cases.0 <- cumsum(P[cl_age90==0])

cc <- 2

for (i in 1:length(cases.0)){
    if (i<10){nnum <- paste("00",i,sep="")}
    if (i>9&i<100){nnum <- paste("0",i,sep="")}
    if (i>99){nnum <- i}
    ff <- paste("./France-Cases/cases",nnum,".png", sep = "")
    print(ff)
    png(filename = ff,
        width = 480, height = 960, units = "px", pointsize = 12,
        bg = "white")
    par(mfrow=c(2,1))
    # Figure
    cases <- cases.0.d[1:i]
    ccolor <- "blue"
    plot(cases,lwd=2,col=ccolor,
         type="o",xlim=c(1,298),ylim=c(0,70000),
         xlab="date",ylab="# cases")
    rr <- paste("day",i, sep = " ")
    print(rr)
    text(x=30,y=65000,labels=rr,cex=cc)
    dev.off()}

# convert -resize 120% -delay 20 -loop 0 `ls -v` France-cases.gif


##########################################
##########################################
### CASES & TESTS
##########################################
##########################################

tests.0.d <- (T[cl_age90==0])
cases.0.d <- (P[cl_age90==0])

tests.0 <- cumsum(T[cl_age90==0])
cases.0 <- cumsum(P[cl_age90==0])

cc <- 2

for (i in 1:length(cases.0)){
    if (i<10){nnum <- paste("00",i,sep="")}
    if (i>9&i<100){nnum <- paste("0",i,sep="")}
    if (i>99){nnum <- i}
    ff <- paste("./France-Cases-Tests/cases-tests",nnum,".png", sep = "")
    print(ff)
    png(filename = ff,
        width = 480, height = 960, units = "px", pointsize = 12,
        bg = "white")
    par(mfrow=c(2,1))
    # Figure
    cases <- cases.0.d[1:i]
    tests <- tests.0.d[1:i]
    ccolor <- "blue"
    plot(cases,lwd=2,col=ccolor,
         type="o",xlim=c(1,298),ylim=c(0,70000),
         xlab="date",ylab="# cases")
    rr <- paste("day",i, sep = " ")
    print(rr)
    text(x=30,y=65000,labels=rr,cex=cc)
    plot(tests,lwd=2,col="darkgreen",
         type="o",xlim=c(1,298),ylim=c(0,800000),
         xlab="date",ylab="# tests")
    text(x=30,y=750000,labels=rr,cex=cc)
    dev.off()}


##########################################
##########################################
### SCATTERPLOT NON NORMALISE
##########################################
##########################################

library(lpridge)

tests.0 <- cumsum(T[cl_age90==0])
cases.0 <- cumsum(P[cl_age90==0])

cc <- 2

for (i in 1:length(tests.0)){
    if (i<10){nnum <- paste("00",i,sep="")}
    if (i>9&i<100){nnum <- paste("0",i,sep="")}
    if (i>99){nnum <- i}
    ff <- paste("./France-scatterplot/Points-SP",nnum,".png", sep = "")
    print(ff)
    png(filename = ff,
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "white")
    ccolor <- "blue"
    # Figure
    tests <- tests.0[1:i]
    cases <- cases.0[1:i]
    plot(tests,cases,lwd=2,col=ccolor,
         type="o",xlim=c(0,56000000),ylim=c(0,4000000),
         xlab="Cumulated tests",ylab="Cumulated cases")
    rr <- paste("day",i, sep = " ")
    print(rr)
    text(x=7000000,y=3900000,labels=rr,cex=cc)
    dev.off()}

png(filename = "scatterplot-with-slopes",
        width = 480, height = 380, units = "px", pointsize = 12,
        bg = "white")

plot(tests.0[1:170],cases.0[1:170],lwd=2,col=ccolor,
         type="o",xlim=c(0,tests.0[170]),ylim=c(0,cases.0[170]),
         xlab="Cumulated tests",ylab="Cumulated cases")

lines(c(0,tests.0[170]),
      c(0,cases.0[170]),col="red",lwd=4)

lines(c(13000000,tests.0[170]),
      c(0,cases.0[170]),col="black",lwd=4)

    text(x=4000000,y=1200000,labels="Oct. 20, 2020",cex=cc)


dev.off()



##########################################
##########################################
### SCATTERPLOT NORMALISE
###########################################
#########################################


tests.0 <- c(0,cumsum(T[cl_age90==0]))
cases.0 <- c(0,cumsum(P[cl_age90==0]))

cc <- 2

for (i in 1:length(cases.0)){
    if (i<10){nnum <- paste("00",i,sep="")}
    if (i>9&i<100){nnum <- paste("0",i,sep="")}
    if (i>99){nnum <- i}
    ff <- paste("./France-Norm-scatterplot/N-Splot",nnum,".png", sep = "")
    print(ff)
    png(filename = ff,
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "white")
    # Figure
    tests <- tests.0[1:i]
    cases <- cases.0[1:i]
    ccolor <- "darkorchid4"
    plot(tests/max(tests),cases/max(cases),lwd=2,col=ccolor,
         type="o",xlim=c(0,1),ylim=c(0,1))
    rr <- paste("day",i, sep = " ")
    lines(c(0,1),c(0,1),lty=2,lwd=2)
    print(rr)
    text(x=.2,y=.9,labels=rr,cex=cc)
    dev.off()}



##########################################
##########################################
### MOVIE
##########################################
##########################################

library(lpridge)


tests.0 <- cumsum(T[cl_age90==0])
cases.0 <- cumsum(P[cl_age90==0])

cc <- 2

for (i in 1:92){
    if (i<10){nnum <- paste("00",i,sep="")}
    if (i>9&i<100){nnum <- paste("0",i,sep="")}
    if (i>99){nnum <- i}
    ff <- paste("./France-Cases/cases",nnum,".png", sep = "")
    print(ff)
    png(filename = ff,
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "white")
    # Figure
    tests <- tests.0[1:i]
    cases <- cases.0[1:i]
    ### Derivatives & functions
    dd <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=0,
            order=4)
    dd1 <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=50,deriv=1,
            order=4)
    function.x   <- dd$x.out
    function.est <- dd$est
    ccolor <- "blue"
    if (dd1$est[200]>1){ccolor <- "red"}
    plot(tests/max(tests),cases/max(cases),lwd=2,col=ccolor,
         type="o",xlim=c(0,1),ylim=c(0,1))
    rr <- paste("day",i, sep = " ")
    lines(c(0,1),c(0,1),lty=2,lwd=2)
    print(rr)
    text(x=.2,y=.9,labels=rr,cex=cc)
    dev.off()}

