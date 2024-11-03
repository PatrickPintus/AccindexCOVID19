rm(list=ls())

base <- read.table(file="sp-pos-quot-dep-2021-01-27-19h20.csv",
                   sep=";",header=TRUE)
attach(base)

nobs    <- dim(base)[1]
nomvar <- attributes(base)$names

TT <- tapply(T[cl_age90==0],jour[cl_age90==0],sum)
PP <- tapply(P[cl_age90==0],jour[cl_age90==0],sum)

tests <- cumsum(TT)
cases <- cumsum(PP)

tests.All <- cumsum(TT)
cases.All <- cumsum(PP)

d.tests.All <- TT
d.cases.All <- PP

date.All <- jour[cl_age90==0]

# Function moyenne mobile

moy.mob.7 <- function(p){
    mm <- rep(0,(length(p)-7))
    for (i in 7:length(p)){mm[i] <- sum(p[(i-6):i]) / 7}
    return(mm)}    

## All age groups

nn <- length(cases.All)

dderiv <- rep(0,nn)
ddd <- rep(0,nn)
ddd <- rep(0,nn)
R.1 <- rep(0,nn)
cases.T <- rep(0,nn)
tests.T <- rep(0,nn)
tw.r <- rep(0,nn)
av.r <- rep(0,nn)

for (i in 9:nn){
    ## Elasticity
    tests   <- tests.All[1:i] / tests.All[i]
    cases   <- cases.All[1:i] / cases.All[i]
    dcases  <- diff(cases)
    dtests  <- diff(tests)
    ddd[i]  <- dcases[i-1] / dtests[i-1]
    # R effectif
    dcases.A <- diff(cases.All)     
    dtests.A <- diff(tests.All)
    R.1[i]  <- mean(dcases.A[(i-4):(i-1)]) /
                    mean(dcases.A[(i-8):(i-5)])
    # Bias
    tw.r[i] <- mean(dcases.A[(i-8):(i-5)])/dtests.A[i-1]
    av.r[i] <- cases.All[i]/tests.All[i]}


plot(unique(jour),R.1,lwd=4,ylim=c(0,3),
     xlab="day",ylab="Elasticty or R",
     type="l",col="orange",cex.lab=1,cex.axis=1)
lines(R.1,lwd=4,,
     type="l",col="orange",cex.lab=1,cex.axis=1)
lines(moy.mob.7(R.1),lwd=4,ylim=c(0,3), xlab="day",
      ylab="Elasticty or R",
      type="l",col="red")
lines(ddd,lwd=4,ylim=c(0,3),
     xlab="day",ylab="Elasticty or R",
     type="l",col="blue")
lines(moy.mob.7(ddd),lwd=4,ylim=c(0,3),
     xlab="day",ylab="Elasticty or R",
     type="l",col="green")
lines(c(0,nn),c(1,1),lwd=3,lty=2)


###

dep.curfew.1 <- sort(c(08,51,55,54,57,52,88,70,25,39,71,58,05,06)) # January 2

dep.curfew.2 <- sort(c(13,84,04,18,03,21,67,68)) # January 10

dep.curfew.3 <- c(26,83) # January 12

all.dep <- seq(1,96)

remove <- c(dep.curfew.1,dep.curfew.2,dep.curfew.3)
all.dep %in% remove

dep.curfew.4  <- all.dep[! all.dep %in% remove] # All = January 16


## Dpt list

dept  <- unique(dep) 
ndep  <- length(dept)

n.curfew.1 <- length(dep.curfew.1)
n.curfew.2 <- length(dep.curfew.2)
n.curfew.3 <- length(dep.curfew.3)
n.curfew.4 <- length(dep.curfew.4)

## Elasticity All

nn <- length(P[dep=="01"&cl_age90==0])

EEdep <- matrix(0,nn,ndep)
CCdep <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    print(dept[aa])
    tests.aa <- cumsum(T[cl_age90==0&dep==dept[aa]])
    cases.aa <- cumsum(P[cl_age90==0&dep==dept[aa]])
    for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                CCdep[i,aa] <- diff(cases.aa)[i-1]
                EEdep[i,aa] <- dcases[i-1] / dtests[i-1]}}


moy.mob.3 <- function(p){
    mm <- rep(0,(length(p)-2))
    for (i in 3:length(p)){mm[i] <- sum(p[(i-2):i]) / 3}
    return(mm)}    

EE.metro <- cbind(EEdep[,1:19],EEdep[,29],EEdep[,20:28],EEdep[,31:96],EEdep[,30])


ee.curfew.1 <- rowSums(EE.metro[,dep.curfew.1])/n.curfew.1
ee.curfew.2 <- rowSums(EE.metro[,dep.curfew.2])/n.curfew.2
ee.curfew.3 <- rowSums(EE.metro[,dep.curfew.3])/n.curfew.3
ee.curfew.4 <- rowSums(EE.metro[,dep.curfew.4])/n.curfew.4

unique(jour)

# 15 dec 217
# 2 jan 235
# 10 jan 243
# 12 jan 245
# 16 jan 249

plot(ee.curfew.1[217:257],col="orange",type="l",ylab="Elasticity",ylim=c(0,2),xlab="date",xaxt="n")
axis(1, at=c(1,235-217,243-217,249-217,41),
     labels=c("Dec 15","Jan 2","Jan 10","Jan 16","Jan 26"), col.axis="black", las=0)

lines(ee.curfew.2[217:257],col="red",type="l")
lines(ee.curfew.3[217:257],col="yellow",type="l")
lines(ee.curfew.4[217:257],col="blue",type="l")
# Dates
lines(c(235-217,235-217),c(0,2),lty=2)
lines(c(243-217,243-217),c(0,2),lty=2)
lines(c(249-217,249-217),c(0,2),lty=2)
# Elasticity 1
lines(c(1,41),c(1,1),lty=2)
# Legend
legend(x=1,y=2,legend=c("group 1 (Jan 2)","group 2 (Jan 10)","group 3 (Jan 12)","group 4 (Jan 16)"),
    col=c("orange","red","yellow","blue"),lty=1,bty="n")

# A partir du 30 octobre 171

plot(ee.curfew.1[171:257],col="orange",type="l",ylab="Elasticity",ylim=c(0,3),xlab="date",xaxt="n")
axis(1, at=c(1,217-171,235-171,243-171,249-171,87),
     labels=c("Oct 30","Dec 15","Jan 2","Jan 10","Jan 16","Jan 26"), col.axis="black", las=0)

lines(ee.curfew.2[171:257],col="red",type="l")
lines(ee.curfew.3[171:257],col="green",type="l")
lines(ee.curfew.4[171:257],col="blue",type="l")
# Dates
lines(c(217-171,217-171),c(0,2),lty=2)
lines(c(235-171,235-171),c(0,2),lty=2)
lines(c(243-171,243-171),c(0,2),lty=2)
lines(c(249-171,249-171),c(0,2),lty=2)
# Elasticity 1
lines(c(1,87),c(1,1),lty=2)
# Legend
legend(x=60,y=3,legend=c("group 1 (Jan 2)","group 2 (Jan 10)","group 3 (Jan 12)","group 4 (Jan 16)"),
    col=c("orange","red","green","blue"),lty=1,bty="n")


#####################################################################"
#####################################################################"
#####################################################################"


##### By AGE GROUP > 60

nn <- length(P[dep=="01"&cl_age90==0])

EEdepO <- matrix(0,nn,ndep)
CCdepO <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    print(dept[aa])
    TT <- tapply(T[cl_age90>59&dep==dept[aa]],jour[cl_age90>59&dep==dept[aa]],sum)
    PP <- tapply(P[cl_age90>59&dep==dept[aa]],jour[cl_age90>59&dep==dept[aa]],sum)
    tests.aa <- cumsum(TT)
    cases.aa <- cumsum(PP)
    for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                CCdepO[i,aa] <- diff(cases.aa)[i-1]
                EEdepO[i,aa] <- dcases[i-1] / dtests[i-1]}}

EE.metro.O <- cbind(EEdepO[,1:19],EEdepO[,29],EEdep[,20:28],EEdepO[,31:96],EEdepO[,30])

eeO.curfew.1 <- rowSums(EE.metro.O[,dep.curfew.1])/n.curfew.1
eeO.curfew.2 <- rowSums(EE.metro.O[,dep.curfew.2])/n.curfew.2
eeO.curfew.3 <- rowSums(EE.metro.O[,dep.curfew.3])/n.curfew.3
eeO.curfew.4 <- rowSums(EE.metro.O[,dep.curfew.4])/n.curfew.4


plot(eeO.curfew.1[171:257],col="orange",type="l",ylab="Elasticity",ylim=c(0,3),xlab="date",xaxt="n")
axis(1, at=c(1,217-171,235-171,243-171,249-171,87),
     labels=c("Oct 30","Dec 15","Jan 2","Jan 10","Jan 16","Jan 26"), col.axis="black", las=0)

lines(eeO.curfew.2[171:257],col="red",type="l")
lines(eeO.curfew.3[171:257],col="green",type="l")
lines(eeO.curfew.4[171:257],col="blue",type="l")
# Dates
lines(c(217-171,217-171),c(0,2),lty=2)
lines(c(235-171,235-171),c(0,2),lty=2)
lines(c(243-171,243-171),c(0,2),lty=2)
lines(c(249-171,249-171),c(0,2),lty=2)
# Elasticity 1
lines(c(1,87),c(1,1),lty=2)
# Legend
legend(x=60,y=3,legend=c("group 1 (Jan 2)","group 2 (Jan 10)","group 3 (Jan 12)","group 4 (Jan 16)"),
    col=c("orange","red","green","blue"),lty=1,bty="n")

#####################################################################"
#####################################################################"
#####################################################################"

##### By AGE GROUP < 60

nn <- length(P[dep=="01"&cl_age90==0])

EEdepO <- matrix(0,nn,ndep)
CCdepO <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    print(dept[aa])
    TT <- tapply(T[cl_age90==0&cl_age90<60&dep==dept[aa]],
                 jour[cl_age90==0&cl_age90<60&dep==dept[aa]],sum)
    PP <- tapply(P[cl_age90==0&cl_age90<60&dep==dept[aa]],
                 jour[cl_age90==0&cl_age90<60&dep==dept[aa]],sum)
    tests.aa <- cumsum(TT)
    cases.aa <- cumsum(PP)
    for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                CCdepO[i,aa] <- diff(cases.aa)[i-1]
                EEdepO[i,aa] <- dcases[i-1] / dtests[i-1]}}

EE.metro.O <- cbind(EEdepO[,1:19],EEdepO[,29],EEdep[,20:28],EEdepO[,31:96],EEdepO[,30])

eeO.curfew.1 <- rowSums(EE.metro.O[,dep.curfew.1])/n.curfew.1
eeO.curfew.2 <- rowSums(EE.metro.O[,dep.curfew.2])/n.curfew.2
eeO.curfew.3 <- rowSums(EE.metro.O[,dep.curfew.3])/n.curfew.3
eeO.curfew.4 <- rowSums(EE.metro.O[,dep.curfew.4])/n.curfew.4


plot(eeO.curfew.1[171:257],col="orange",type="l",ylab="Elasticity",ylim=c(0,3),xlab="date",xaxt="n")
axis(1, at=c(1,217-171,235-171,243-171,249-171,87),
     labels=c("Oct 30","Dec 15","Jan 2","Jan 10","Jan 16","Jan 26"), col.axis="black", las=0)

lines(eeO.curfew.2[171:257],col="red",type="l")
lines(eeO.curfew.3[171:257],col="green",type="l")
lines(eeO.curfew.4[171:257],col="blue",type="l")
# Dates
lines(c(217-171,217-171),c(0,2),lty=2)
lines(c(235-171,235-171),c(0,2),lty=2)
lines(c(243-171,243-171),c(0,2),lty=2)
lines(c(249-171,249-171),c(0,2),lty=2)
# Elasticity 1
lines(c(1,87),c(1,1),lty=2)
# Legend
legend(x=60,y=3,legend=c("group 1 (Jan 2)","group 2 (Jan 10)","group 3 (Jan 12)","group 4 (Jan 16)"),
    col=c("orange","red","green","blue"),lty=1,bty="n")


#####################################################################"
#####################################################################"
#####################################################################"

# Short memory elasticity

# 2 jan 235

nn <- length(cases.All)

tests.All.s <- cumsum(TT[235:nn])
cases.All.s <- cumsum(PP[235:nn])

nn.s <- length(tests.All.s)

dddS <- rep(0,nn.s)



for (i in 2:nn.s){
    ## Elasticity
    tests   <- tests.All[1:i] / tests.All[i]
    cases   <- cases.All[1:i] / cases.All[i]
    dcases  <- diff(cases)
    dtests  <- diff(tests)
    dddS[i]  <- dcases[i-1] / dtests[i-1]}


plot(ddd[1:nn],lwd=1,ylim=c(0,3),
     xlab="day",ylab="Elasticty or R",
     type="l",col="blue",cex.lab=1,cex.axis=1)
lines(seq(236,nn),dddS[2:nn.s],lwd=1,ylim=c(0,3),
     xlab="day",ylab="Elasticty or R",
     type="l",col="red")

##################
##################
##################


nn <- length(P[dep=="01"&cl_age90==0])

EEdep.s <- matrix(0,nn.s,ndep)

for (aa in 1:ndep){
    print(dept[aa])
    TT <- cumsum(T[cl_age90==0&dep==dept[aa]])
    PP <- cumsum(P[cl_age90==0&dep==dept[aa]])    
    tests.aa <- TT[235:nn]
    cases.aa <- PP[235:nn] 
    for (i in 2:nn.s){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                EEdep.s[i,aa] <- dcases[i-1] / dtests[i-1]}}

EE.metro.s <- cbind(EEdep.s[,1:19],EEdep.s[,29],EEdep.s[,20:28],EEdep.s[,31:96],EEdep.s[,30])

ee.curfew.1s <- rowSums(EE.metro.s[,dep.curfew.1])/n.curfew.1
ee.curfew.2s <- rowSums(EE.metro.s[,dep.curfew.2])/n.curfew.2
ee.curfew.3s <- rowSums(EE.metro.s[,dep.curfew.3])/n.curfew.3
ee.curfew.4s <- rowSums(EE.metro.s[,dep.curfew.4])/n.curfew.4

plot(ee.curfew.1s[2:nn.s],col="orange",type="l",ylab="Elasticity",ylim=c(0,3),xlab="date",xaxt="n")
axis(1, at=c(1,7,13,22),
     labels=c("Jan 2","Jan 10","Jan 16","Jan 26"), col.axis="black", las=0)

lines(ee.curfew.2s[2:nn.s],col="red",type="l")
lines(ee.curfew.3s[2:nn.s],col="green",type="l")
lines(ee.curfew.4s[2:nn.s],col="blue",type="l")
# Dates
lines(c(7,7),c(0,2),lty=2)
lines(c(13,13),c(0,2),lty=2)
# Elasticity 1
lines(c(1,22),c(1,1),lty=2)
# Legend
legend(x=2,y=3,legend=c("group 1 (Jan 2)","group 2 (Jan 10)","group 3 (Jan 12)","group 4 (Jan 16)"),
    col=c("orange","red","green","blue"),lty=1,bty="n")




##################
##################
##################

EEEdep <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    EEEdep[,aa] <- moy.mob.3(EEdep[,aa])}

EEE.nn <- c(EEEdep[nn,1:19],
  EEEdep[nn,29],
  EEEdep[nn,20:28],
  EEEdep[nn,31:96],
  EEEdep[nn,30])



## Elasticity curfew.1

n.curfew.1 <- length(dep.curfew.1)

TT <- tapply(T[cl_age90==0&dep==dep.curfew.1],jour[cl_age90==0&dep==dep.curfew.1],sum)
PP <- tapply(P[cl_age90==0],jour[cl_age90==0],sum)



for (aa in 1:ndep){
    print(dept[aa])
    tests.aa <- cumsum(T[cl_age90==0&dep==dept[aa]])
    cases.aa <- cumsum(P[cl_age90==0&dep==dept[aa]])
    for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                CCdep[i,aa] <- diff(cases.aa)[i-1]
                EEdep[i,aa] <- dcases[i-1] / dtests[i-1]}}













