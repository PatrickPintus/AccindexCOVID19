rm(list=ls())

base <- read.table(file="sp-pos-quot-dep-2021-04-01-18h20.csv",
                   sep=";",header=TRUE)
attach(base)

nobs    <- dim(base)[1]
nomvar <- attributes(base)$names

###

nn <- length(P[dep=="01"&cl_age90==0])

dept  <- unique(dep) 
ndep  <- length(dept)
EEdep <- matrix(0,nn,ndep)
CCdep <- matrix(0,nn,ndep)
PPdep <- matrix(0,nn,ndep)


for (aa in 1:ndep){
    print(dept[aa])
    tests.aa <- cumsum(T[cl_age90==0&dep==dept[aa]])
    cases.aa <- cumsum(P[cl_age90==0&dep==dept[aa]])
    for (i in 2:nn){
           tests  <- tests.aa[1:i] / tests.aa[i]
           cases  <- cases.aa[1:i] / cases.aa[i]
           dcases <- diff(cases)
           dtests <- diff(tests)
           PPdep[i,aa] <- diff(cases.aa)[i-1]/diff(tests.aa)[i-1]
           CCdep[i,aa] <- diff(cases.aa)[i-1]
           EEdep[i,aa] <- dcases[i-1] / dtests[i-1]}}

# Last 3 days

EE.1 <- c(EEdep[(nn-2),1:19],
  EEdep[(nn-2),29],
  EEdep[(nn-2),20:28],
  EEdep[(nn-2),31:96],
  EEdep[(nn-2),30])

EE.2 <- c(EEdep[(nn-1),1:19],
  EEdep[(nn-1),29],
  EEdep[(nn-1),20:28],
  EEdep[(nn-1),31:96],
  EEdep[(nn-1),30])

EE.3 <- c(EEdep[nn,1:19],
  EEdep[nn,29],
  EEdep[nn,20:28],
  EEdep[nn,31:96],
  EEdep[nn,30])

cbind(seq(1,96),(EE.1+EE.2+EE.3)/3)


# Last date
        
EE.nn <- c(EEdep[nn,1:19],
  EEdep[nn,29],
  EEdep[nn,20:28],
  EEdep[nn,31:96],
  EEdep[nn,30])

Emin <- min(EE.nn)
Emax <- max(EE.nn)

ee.dpt.nn <- (EE.nn-min(EE.nn)) / (Emax-Emin)

round(ee.dpt.nn*100+10)

CC.nn <- c(CCdep[nn,1:19],
  CCdep[nn,29],
  CCdep[nn,20:28],
  CCdep[nn,31:96],
  CCdep[nn,30])

Cmin <- min(CC.nn)
Cmax <- max(CC.nn)

cc.dpt.nn  <- (CC.nn-min(CC.nn)) / (Cmax-Cmin)

round(cc.dpt.nn*100+10)

cbind(round(ee.dpt.nn*100+10),round(cc.dpt.nn*100+10))



# Last date with the mean over 3 dates

moy.mob.3 <- function(p){
    mm <- rep(0,(length(p)-2))
    for (i in 3:length(p)){mm[i] <- sum(p[(i-2):i]) / 3}
    return(mm)}    

EEEdep <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    EEEdep[,aa] <- moy.mob.3(EEdep[,aa])}

EEE.nn <- c(EEEdep[nn,1:19],
  EEEdep[nn,29],
  EEEdep[nn,20:28],
  EEEdep[nn,31:96],
  EEEdep[nn,30])

ccc.dpt.nn <- ((EEE.nn>=0) &(EEE.nn<.2))*100+
              ((EEE.nn>=.2)&(EEE.nn<.4))*80+
              ((EEE.nn>=.4)&(EEE.nn<.6))*60+
              ((EEE.nn>=.6)&(EEE.nn<.8))*40+
              ((EEE.nn>=.8)&(EEE.nn<1))*20+
              ((EEE.nn>=1) &(EEE.nn<2))*20+
              ((EEE.nn>=2) &(EEE.nn<3))*40+
              ((EEE.nn>=3) &(EEE.nn<4))*60+
              ((EEE.nn>=4) &(EEE.nn<5))*80+
              ((EEE.nn>=5))*100

color.dpt.nn <-EEE.nn
color.dpt.nn[EEE.nn<1]  <- "Green"
color.dpt.nn[EEE.nn>=1] <- "Red"

color.dpt.nn[is.na(EEE.nn)==TRUE] <- "Green"

############################################
############################################

dd <- 110

EEE.dd <- c(EEEdep[dd,1:19],
  EEEdep[dd,29],
  EEEdep[dd,20:28],
  EEEdep[dd,31:96],
  EEEdep[dd,30])

ccc.dpt.dd <- ((EEE.dd>=0) &(EEE.dd<.2))*100+
              ((EEE.dd>=.2)&(EEE.dd<.4))*80+
              ((EEE.dd>=.4)&(EEE.dd<.6))*60+
              ((EEE.dd>=.6)&(EEE.dd<.8))*40+
              ((EEE.dd>=.8)&(EEE.dd<1))*20+
              ((EEE.dd>=1) &(EEE.dd<2))*20+
              ((EEE.dd>=2) &(EEE.dd<3))*40+
              ((EEE.dd>=3) &(EEE.dd<4))*60+
              ((EEE.dd>=4) &(EEE.dd<5))*80+
              ((EEE.dd>=5))*100

ccc.dpt.dd[is.na(ccc.dpt.dd)==TRUE] <- 100

color.dpt.dd <-EEE.dd
color.dpt.dd[EEE.dd<1]  <- "Green"
color.dpt.dd[EEE.dd>=1] <- "Red"

color.dpt.dd[is.na(EEE.dd)==TRUE] <- "Green"

ccc.dpt.dd

color.dpt.dd

#### ALL FILES

for (dd in 3:nn){
    print(dd)
    # Elasticities in the right order
    EEE.dd <- c(EEEdep[dd,1:19],
                EEEdep[dd,29],
                EEEdep[dd,20:28],
                EEEdep[dd,31:96],
                EEEdep[dd,30])
    # Transformation en intensité
    ccc.dpt.dd <- ((EEE.dd>=0) &(EEE.dd<.2))*100+
        ((EEE.dd>=.2)&(EEE.dd<.4))*80+
        ((EEE.dd>=.4)&(EEE.dd<.6))*60+
        ((EEE.dd>=.6)&(EEE.dd<.8))*40+
        ((EEE.dd>=.8)&(EEE.dd<1))*20+
        ((EEE.dd>=1) &(EEE.dd<2))*20+
        ((EEE.dd>=2) &(EEE.dd<3))*40+
        ((EEE.dd>=3) &(EEE.dd<4))*60+
        ((EEE.dd>=4) &(EEE.dd<5))*80+
        ((EEE.dd>=5))*100
     ccc.dpt.dd[is.na(ccc.dpt.dd)==TRUE] <- 100
     # Color en fonction du seuil 1
    color.dpt.dd <-EEE.dd
    color.dpt.dd[EEE.dd<1]  <- "Green"
    color.dpt.dd[EEE.dd>=1] <- "Red"
    color.dpt.dd[is.na(EEE.dd)==TRUE] <- "Green"
    nfileI <- paste("./DptDays/MM",dd,".csv",sep="")
    nfileC <- paste("./DptDays/CC",dd,".csv",sep="")
    write.table(ccc.dpt.dd,file=nfileI,sep=" ",
                    row.names=FALSE,col.names=FALSE)
    write.table(color.dpt.dd,file=nfileC,sep=" ",
                    row.names=FALSE,col.names=FALSE,quote=FALSE)}

write.table(unique(jour)[3:nn],file="nameDD.csv",sep=";",
                    row.names=FALSE,col.names=FALSE,quote=FALSE)

############################################
4 AGE GROUPS
############################################

AG1 <- cl_age90>0 &cl_age90<=9
AG2 <- cl_age90>9 &cl_age90<=19
AG3 <- cl_age90>19

AG1 <- cl_age90>0 &cl_age90<=29
AG2 <- cl_age90>29&cl_age90<=49
AG3 <- cl_age90>49&cl_age90<=69
AG4 <- cl_age90>69

EEdep <- matrix(0,nn,ndep)
CCdep <- matrix(0,nn,ndep)
TTdep <- matrix(0,nn,ndep)


for (aa in 1:ndep){
    print(dept[aa])
    tests.aa <- cumsum(T[AG3==1&dep==dept[aa]])
    cases.aa <- cumsum(P[AG3==1&dep==dept[aa]])
    for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                CCdep[i,aa] <- diff(cases.aa)[i-1]
                TTdep[i,aa] <- diff(tests.aa)[i-1]
                EEdep[i,aa] <- dcases[i-1] / dtests[i-1]}}


EE1dep <- EEdep
CC1dep <- CCdep
TT1dep <- TTdep

EE2dep <- EEdep
CC2dep <- CCdep
TT2dep <- TTdep

EE3dep <- EEdep
CC3dep <- CCdep
TT3dep <- TTdep


D13 <- cbind(seq(1,161),EE1dep[,13],CC1dep[,13],TT1dep[,13],
                 EE2dep[,13],CC2dep[,13],TT2dep[,13],
                 EE3dep[,13],CC3dep[,13],TT3dep[,13])

      
write.csv(D13,file="data-D13-age.csv",
          row.names=FALSE)


### Sans les BdR


AG1 <- cl_age90>0 &cl_age90<=9
AG2 <- cl_age90>9 &cl_age90<=19
AG3 <- cl_age90>59 &cl_age90<=79
AG4 <- (cl_age90==0)-AG1-AG2-AG3

tests.aa <- tapply(cumsum(T[AG3==1&dep!=13&as.numeric(dep)<96]),
                   jour[AG3==1&dep!=13&as.numeric(dep)<96],
                   sum)       

cases.aa <- tapply(cumsum(P[AG3==1&dep!=13&as.numeric(dep)<96]),
                   jour[AG3==1&dep!=13&as.numeric(dep)<96],
                   sum)       

EEdep <- rep(0,nn)
CCdep <- rep(0,nn)
TTdep <- rep(0,nn)

for (i in 2:nn){
             tests  <- tests.aa[1:i] / tests.aa[i]
             cases  <- cases.aa[1:i] / cases.aa[i]
             dcases <- diff(cases)
             dtests <- diff(tests)
             CCdep[i] <- diff(cases.aa)[i-1]
             TTdep[i] <- diff(tests.aa)[i-1]
             EEdep[i] <- dcases[i-1] / dtests[i-1]}

EE131dep <- EEdep
CC131dep <- CCdep
TT111dep <- TTdep

EE132dep <- EEdep
CC132dep <- CCdep
TT132dep <- TTdep

EE3dep <- EEdep
CC3dep <- CCdep
TT3dep <- TTdep

Dno13 <- cbind(seq(1,161),EE1dep,CC1dep,TT1dep,
                 EE2dep,CC2dep,TT2dep,
                 EE3dep,CC3dep,TT3dep)

write.csv(Dno13,file="data-noD13-age.csv",
          row.names=FALSE)

##### Figure 6

ee.AG.Dpt <- function(p){
    tests.aa <- tapply(cumsum(T[p]),jour[p],sum)       
    cases.aa <- tapply(cumsum(P[p]),jour[p],sum)       
    EEl  <- rep(0,nn)
    for (i in 2:nn){
        tests  <- tests.aa[1:i] / tests.aa[i]
        cases  <- cases.aa[1:i] / cases.aa[i]
        dcases <- diff(cases)
        dtests <- diff(tests)
        EEl[i] <- dcases[i-1] / dtests[i-1]}
    return(EEl)}

### Age groups

AG1   <- cl_age90>0 &cl_age90<=9
AG2   <- cl_age90>9 &cl_age90<=19
AG3   <- cl_age90>59 &cl_age90<=79
AG4   <- (cl_age90==0)-AG1-AG2-AG3
AGAll <- (cl_age90==0)

###

moy.mob.5 <- function(p){
    mm <- rep(0,(length(p)-4))
    for (i in 5:length(p)){mm[i] <- sum(p[(i-4):i]) / 5}
    return(mm)}    


EE59All <- moy.mob.5(ee.AG.Dpt(AGAll==1&dep==59))
EE75All <- moy.mob.5(ee.AG.Dpt(AGAll==1&dep==75))
EE13All <- moy.mob.5(ee.AG.Dpt(AGAll==1&dep==13))
EE69All <- moy.mob.5(ee.AG.Dpt(AGAll==1&dep==69))

EE59A1 <- moy.mob.5(ee.AG.Dpt(AG1==1&dep==59))
EE75A1 <- moy.mob.5(ee.AG.Dpt(AG1==1&dep==75))
EE13A1 <- moy.mob.5(ee.AG.Dpt(AG1==1&dep==13))
EE69A1 <- moy.mob.5(ee.AG.Dpt(AG1==1&dep==69))

EE59A3 <- moy.mob.5(ee.AG.Dpt(AG3==1&dep==59))
EE75A3 <- moy.mob.5(ee.AG.Dpt(AG3==1&dep==75))
EE13A3 <- moy.mob.5(ee.AG.Dpt(AG3==1&dep==13))
EE69A3 <- moy.mob.5(ee.AG.Dpt(AG3==1&dep==69))

EEDA <- cbind(EE59All,EE75All,EE13All,EE69All,
              EE59A1,EE75A1,EE13A1,EE69A1,
              EE59A3,EE75A3,EE13A3,EE69A3)

max(EEDA) # > 10

### All Age groups

OUT <- cbind(seq(1,nn)/nn*14,
             EE59All*2)

write.table(round(OUT,2),file="fig6.All59.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE75All*2)

write.table(round(OUT,2),file="fig6.All75.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE13All*2)

write.table(round(OUT,2),file="fig6.All13.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE69All*2)

write.table(round(OUT,2),file="fig6.All69.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

### AG1

OUT <- cbind(seq(1,nn)/nn*14,
             EE59A1*2)

write.table(round(OUT,2),file="fig6.A159.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE75A1*2)

write.table(round(OUT,2),file="fig6.A175.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE13A1*2)

write.table(round(OUT,2),file="fig6.A113.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE69A1*2)

write.table(round(OUT,2),file="fig6.A169.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

### AG1

OUT <- cbind(seq(1,nn)/nn*14,
             EE59A3*2)

write.table(round(OUT,2),file="fig6.A359.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE75A3*2)

write.table(round(OUT,2),file="fig6.A375.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE13A3*2)

write.table(round(OUT,2),file="fig6.A313.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             EE69A3*2)

write.table(round(OUT,2),file="fig6.A369.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)


####


tests.0 <- tapply(T[cl_age90==0&dep!=59&dep!=13&dep!=75&dep!=69
                    &as.numeric(dep)<96],
                jour[cl_age90==0&dep!=59&dep!=13&dep!=75&dep!=69&
                       as.numeric(dep)<96],
                   sum)       

cases.0 <- tapply(P[cl_age90==0&dep!=59&dep!=13&dep!=75&dep!=69&
                    as.numeric(dep)<96],
               jour[cl_age90==0&dep!=59&dep!=13&dep!=75&dep!=69&
                       &as.numeric(dep)<96],
                   sum)       

tests.All <- cumsum(tests.0)
cases.All <- cumsum(cases.0)

ddd <- rep(0,nn)

for (i in 2:nn){
    tests  <- tests.All[1:i] / tests.All[i]
    cases  <- cases.All[1:i] / cases.All[i]
    dcases <- diff(cases)
    dtests <- diff(tests)
    ddd[i] <- dcases[i-1] / dtests[i-1]}


OUT <- cbind(seq(1,nn)/nn*14,
              ddd*2)

write.table(round(OUT,2),file="fig6.elasticityA.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

######################

tests.0 <- tapply(T[cl_age90==9&dep!=59&dep!=13&dep!=75&dep!=69
                    &as.numeric(dep)<96],
                jour[cl_age90==9&dep!=59&dep!=13&dep!=75&dep!=69&
                       as.numeric(dep)<96],
                   sum)       

cases.0 <- tapply(P[cl_age90==9&dep!=59&dep!=13&dep!=75&dep!=69&
                    as.numeric(dep)<96],
               jour[cl_age90==9&dep!=59&dep!=13&dep!=75&dep!=69&
                       as.numeric(dep)<96],
                   sum)       

tests.All <- cumsum(tests.0)
cases.All <- cumsum(cases.0)

ddd <- rep(0,nn)

for (i in 2:nn){
    tests  <- tests.All[1:i] / tests.All[i]
    cases  <- cases.All[1:i] / cases.All[i]
    dcases <- diff(cases)
    dtests <- diff(tests)
    ddd[i] <- dcases[i-1] / dtests[i-1]}


OUT <- cbind(seq(1,nn)/nn*14,
              moy.mob.5(ddd)*2)

write.table(round(OUT,2),file="fig6.elasticity1.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

###


AG3   <- cl_age90==69|cl_age90==79


tests.0 <- tapply(T[AG3==1&dep!=59&dep!=13&dep!=75&dep!=69&
                    as.numeric(dep)<96],
                  jour[AG3==1&dep!=59&dep!=13&dep!=75&
                       dep!=69&
                       as.numeric(dep)<96],
                   sum)       

cases.0 <- tapply(P[AG3==1&dep!=59&dep!=13&dep!=75&dep!=69&
                    as.numeric(dep)<96],
               jour[AG3==1&dep!=59&dep!=13&dep!=75&dep!=69&
                       as.numeric(dep)<96],
                   sum)       

tests.All <- cumsum(tests.0)
cases.All <- cumsum(cases.0)

ddd <- rep(0,nn)

for (i in 2:nn){
    tests  <- tests.All[1:i] / tests.All[i]
    cases  <- cases.All[1:i] / cases.All[i]
    dcases <- diff(cases)
    dtests <- diff(tests)
    ddd[i] <- dcases[i-1] / dtests[i-1]}


OUT <- cbind(seq(1,nn)/nn*14,
              moy.mob.5(ddd)*2)

write.table(round(OUT,2),file="fig6.elasticity3.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)


############################

#### TESTS 

nn <- length(P[dep=="01"&cl_age90==0])

dept  <- unique(dep) 
ndep  <- length(dept)
EEdep <- matrix(0,nn,ndep)
CCdep <- matrix(0,nn,ndep)
TTdep <- matrix(0,nn,ndep)


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
                TTdep[i,aa] <- diff(tests.aa)[i-1]
                EEdep[i,aa] <- dcases[i-1] / dtests[i-1]}}


TTTdep <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    TTTdep[,aa] <- moy.mob.3(TTdep[,aa])}

dd <- nn

TTT.dd <- c(TTTdep[dd,1:19],
  TTTdep[dd,29],
  TTTdep[dd,20:28],
  TTTdep[dd,31:96],
  TTTdep[dd,30])

ttt <- round(TTT.dd / sum(TTT.dd)*100,2)


quantile(ttt,prob=c(.2,.4,.6,.8))
0.31 0.51 0.82 1.63 

ttt.c <- (ttt<=.31)*20+
    ((ttt>.31)*(ttt<=.51))*40+
    ((ttt>.51)*(ttt<=.82))*60+
    ((ttt>.82)*(ttt<=1.63))*80+
     (ttt>1.63)*100


 [1]  80  60  40  20  20  80  60  40  20  40  40  60 100  80  20  20  60  40  20
[20]  20  60  60  20  40  80  60  60  40  80  60 100  20  80 100  80  20  80 100
[39]  40  40  40 100  40 100  80  20  20  20  80  60  60  20  40  80  20  60  80
[58]  20 100  80  20 100  80  80  40  60 100  80 100  20  80  60  60  80 100 100
[77]  80 100  40  60  40  40 100  60  60  40  40  60  20  20 100 100 100 100 100
[96]  40

## Cases

CCCdep <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    CCCdep[,aa] <- moy.mob.3(CCdep[,aa])}

dd <- 161

CCC.dd <- c(CCCdep[dd,1:19],
  CCCdep[dd,29],
  CCCdep[dd,20:28],
  CCCdep[dd,31:96],
  CCCdep[dd,30])

ccc <- round(CCC.dd / sum(CCC.dd)*100,2)


############################################
### Allocation of tests beta = 1

eee <- round(EEE.nn / sum(EEE.nn)*100,2)

quantile(ttt,prob=c(.2,.4,.6,.8))
0.83 0.95 1.07 1.22 

eee.c <- (eee<=.31)*20+
    ((eee>.31)*(eee<=.51))*40+
    ((eee>.51)*(eee<=.82))*60+
    ((eee>.82)*(eee<=1.63))*80+
     (eee>1.63)*100

exp(-sum(eee/100 * log(eee/100)))
exp(-sum(ttt/100 * log(ttt/100)))

##### EXPORT

nn <- length(P[dep=="01"&cl_age90==0])

dept   <- unique(dep) 
ndep   <- length(dept)
EEdep  <- matrix(0,nn,ndep)
CCdep  <- matrix(0,nn,ndep)
TTdep  <- matrix(0,nn,ndep)
CCCumdep <- matrix(0,nn,ndep)
TTCumdep <- matrix(0,nn,ndep)


for (aa in 1:ndep){
    print(dept[aa])
    tests.aa <- cumsum(T[cl_age90==0&dep==dept[aa]])
    cases.aa <- cumsum(P[cl_age90==0&dep==dept[aa]])
    for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                CCdep[i,aa]  <- diff(cases.aa)[i-1]
                CCCumdep[i,aa] <- cases.aa[i-1]
                TTdep[i,aa]  <- diff(tests.aa)[i-1]
                TTCumdep[i,aa] <- tests.aa[i-1]
                EEdep[i,aa]  <- dcases[i-1] / dtests[i-1]}}

EEEdep  <- matrix(0,nn,ndep)
CCCdep  <- matrix(0,nn,ndep)
TTTdep  <- matrix(0,nn,ndep)
CCCCumdep <- matrix(0,nn,ndep)
TTTCumdep <- matrix(0,nn,ndep)

for (aa in 1:ndep){
    EEEdep[,aa]    <- moy.mob.3(EEdep[,aa])
    CCCdep[,aa]    <- moy.mob.3(CCdep[,aa])
    TTTdep[,aa]    <- moy.mob.3(TTdep[,aa])}

EEE.nn <- c(EEEdep[nn,1:19],EEEdep[nn,29],EEEdep[nn,20:28],
            EEEdep[nn,31:96],EEEdep[nn,30])

CCC.nn <- c(CCCdep[nn,1:19],CCCdep[nn,29],CCCdep[nn,20:28],
            CCCdep[nn,31:96],CCCdep[nn,30])

TTT.nn <- c(TTTdep[nn,1:19],TTTdep[nn,29],TTTdep[nn,20:28],
            TTTdep[nn,31:96],TTTdep[nn,30])

CCCCum.nn <- c(CCCumdep[nn,1:19],CCCumdep[nn,29],
               CCCumdep[nn,20:28],CCCumdep[nn,31:96],
               CCCumdep[nn,30])

TTTCum.nn <- c(TTCumdep[nn,1:19],TTCumdep[nn,29],
               TTCumdep[nn,20:28],TTCumdep[nn,31:96],
               TTCumdep[nn,30])

noms.dep <- c(
"Ain",
"Aisne",
"Allier",
"Alpes-de-Haute-Provence",
"Hautes-Alpes",
"Alpes-Maritimes",
"Ardèche",
"Ardennes",
"Ariège",
"Aube",
"Aude",
"Aveyron",
"Bouches-du-Rhône",
"Calvados",
"Cantal",
"Charente",
"Charente-Maritime",
"Cher",
"Corrèze",
"Corse-du-Sud",
"Côte-d'Or",
"Côtes-d'Armor",
"Creuse",
"Dordogne",
"Doubs",
"Drôme",
"Eure",
"Eure-et-Loir",
"Finistère",
"Gard",
"Haute-Garonne",
"Gers",
"Gironde",
"Hérault",
"Ille-et-Vilaine",
"Indre",
"Indre-et-Loire",
"Isère",
"Jura",
"Landes",
"Loir-et-Cher",
"Loire",
"Haute-Loire",
"Loire-Atlantique",
"Loiret",
"Lot",
"Lot-et-Garonne",
"Lozère",
"Maine-et-Loire",
"Manche",
"Marne",
"Haute-Marne",
"Mayenne",
"Meurthe-et-Moselle",
"Meuse",
"Morbihan",
"Moselle",
"Nièvre",
"Nord",
"Oise",
"Orne",
"Pas-de-Calais",
"Puy-de-Dôme",
"Pyrénées-Atlantiques",
"Hautes-Pyrénées",
"Pyrénées-Orientales",
"Bas-Rhin",
"Haut-Rhin",
"Circonscription départementale du Rhône",
"Haute-Saône",
"Saône-et-Loire",
"Sarthe",
"Savoie",
"Haute-Savoie",
"Paris",
"Seine-Maritime",
"Seine-et-Marne",
"Yvelines",
"Deux-Sèvres",
"Somme",
"Tarn",
"Tarn-et-Garonne",
"Var",
"Vaucluse",
"Vendée",
"Vienne",
"Haute-Vienne",
"Vosges",
"Yonne",
"Territoire de Belfort",
"Essonne",
"Hauts-de-Seine",
"Seine-Saint-Denis",
"Val-de-Marne",
"Val-d'Oise",
"Haute-Corse")                                  


write.csv(cbind(EEE.nn,
                CCC.nn,
                TTT.nn,
                CCCCum.nn,
                TTTCum.nn,
                pop.w),file="data-allocation-Oct25.csv",
          row.names=FALSE)

### Poids population dpt en 2017

pop <- c(
643350,
534490,
337988,
163915,
141284,
1083310,
325712,
273579,
153153,
310020,
370260,
279206,
2024162,
694002,
145143,
352335,
644303,
304256,
241464,
157249,
533819,
598814,
118638,
413606,
539067,
511553,
601843,
433233,
909028,
744178,
1362672,
191091,
1583384,
1144892,
1060199,
222232,
606511,
1258722,
260188,
407444,
331915,
762941,
227283,
1394909,
678105,
173828,
332842,
76601,
813493,
496883,
568895,
175640,
307445,
733481,
187187,
750863,
1043522,
207182,
2604361,
824503,
283372,
1468018,
653742,
677309,
228530,
474452,
1125559,
764030,
1843319,
236659,
553595,
566506,
431174,
807360,2187526,1254378,1403997,1438266,374351,572443,
387890,258349,1058740,559479,675247,436876,374426,367673,
338291,142622,1296130,1609306,1623111,1387926,1228618,177689)

pop.w <- pop / sum(pop)

#### WEIGHTS

### weighting function

beta <- 1
pd <- CCCCum.nn / TTTCum.nn
ww <- (EEE.nn * pd)^beta / sum((EEE.nn * pd)^beta) 

exp(-sum(ww* log(ww))) # 96


www.1 <- ww

beta <- 3
pd <- CCCCum.nn / TTTCum.nn
ww <- (EEE.nn * pd)^beta / sum((EEE.nn * pd)^beta) 

www.3 <- ww


exp(-sum(www.1* log(www.1))) # 91.06
exp(-sum(www.3* log(www.3))) # 61.27

# Observed distribution of tests

tt.nn <- TTT.nn / sum(TTT.nn)

exp(-sum(tt.nn * log(tt.nn))) # 65.69
exp(-sum(pop.w * log(pop.w))) # 74.20

# Population / Tests / Beta = 1/2/3 (2 en annexe)
# Spectre pour les % 

MM <- round(
cbind(
pop.w*100,
TTT.nn*100/sum(TTT.nn),
www.1*100,
www.3*100),4)

library(xtable)
print(xtable(MM, type = "latex"), file = "weights-table.tex")

# Range 0 > 8

MM.norm <- (MM-min(MM))/(max(MM)-min(MM))
MM.norm <- MM.norm*100
MM.norm <- MM.norm*.9 +10

### Distance

KLD(TTT.nn/sum(TTT.nn),pop.w)$intrinsic.discrepancy # .039

KLD(www.1,pop.w)$intrinsic.discrepancy # .244
KLD(www.3,pop.w)$intrinsic.discrepancy # .519

KLD(www.1,TTT.nn/sum(TTT.nn))$intrinsic.discrepancy # .339
KLD(www.3,TTT.nn/sum(TTT.nn))$intrinsic.discrepancy # .516

KLD(www.1,www.3)$intrinsic.discrepancy # .185

# Population

write.table(MM.norm[,1],file="MMpop.ssv",sep=" ",
                    row.names=FALSE,col.names=FALSE)

write.table(MM.norm[,2],file="MMtests.ssv",sep=" ",
                    row.names=FALSE,col.names=FALSE)

write.table(MM.norm[,3],file="MMbeta1.ssv",sep=" ",
                    row.names=FALSE,col.names=FALSE)

write.table(MM.norm[,4],file="MMbeta3.ssv",sep=" ",
                    row.names=FALSE,col.names=FALSE)


summary(lm(CCCCum.nn~TTTCum.nn))
summary(lm(CCC.nn~TTT.nn))

bb <- summary(lm(CCC.nn~TTT.nn))$coeff[1]
aa <- summary(lm(CCC.nn~TTT.nn))$coeff[2]

plot(TTT.nn,CCC.nn,type="p",col="blue",
     pch=19,cex=2)
lines(c(0,11000),c(bb,11000*aa+bb),lwd=3)

library(LaplacesDemon)

KLD(ttt,pop.w)$intrinsic.discrepancy # .039
KLD(eee,pop.w)$intrinsic.discrepancy # .379
KLD(eee.p,pop.w)$intrinsic.discrepancy # .379

KLD(ttt,eee)$intrinsic.discrepancy # .535
KLD(ttt,eee.p)$intrinsic.discrepancy # .077


KL.div <- function(PP){
    PP1 <- PP[,1]
    PP2 <- PP[,2]
    KL  <- sum(PP1*log2(PP1/PP2))
    return(KL)}           

JJ1 <- cbind(tt.nn,pop.w)
JJ2 <- cbind(www.1,tt.nn)
JJ3 <- cbind(www.3,tt.nn)
JJ4 <- cbind(www.1,pop.w)
JJ5 <- cbind(www.3,pop.w)

KL.div(JJ1)
KL.div(JJ2)
KL.div(JJ3)

JSD.div <- function(PP){
    PP1 <- PP[,1]
    PP2 <- PP[,2]
    PPM <- (PP1+PP2) / 2
    KL1 <- KL.div(cbind(PP1,PPM))
    KL2 <- KL.div(cbind(PP2,PPM))
    print(c(KL1,KL2))
    JSD <- (KL1+KL2) / 2
    return(JSD)}


JSD.div(JJ1)
JSD.div(JJ2)
JSD.div(JJ3)
JSD.div(JJ4)
JSD.div(JJ5)

Jensen-Shannon Index:
Actual tests vs pop #[1] 0.01112125
Actual tests vs w1  #[1] 0.07997324
Actual tests vs w3  #[1] 0.134216

w1  #[1] 0.07997324
w3  #[1] 0.134216


#### Time series

Div.ww.1.t <- rep(0,160)
Div.ww.3.t <- rep(0,160)
Div.TT.t  <- rep(0,160)
Div.CC.t  <- rep(0,160)

PD.tt  <- matrix(0,160,96)

KL.ww1.TT.t <- rep(0,160)
KL.ww2.TT.t <- rep(0,160)
KL.TT.pop.t <- rep(0,160)

JS1 <- rep(0,160)
JS3 <- rep(0,160)


CumTestD <- TTCumdep
CumCasD  <- CCCumdep

for (i in 4:160){
    PD.t    <- CumCasD[(i-1),1:96] / CumTestD[(i-1),1:96]
    PD.tt[i,] <- PD.t
    elast <- EEEdep[(i-1),1:96]
    ww.1.t  <- (elast*PD.t)/sum((elast*PD.t),na.rm=TRUE)
    ww.3.t  <- (elast*PD.t)^3/sum((elast*PD.t)^3,na.rm=TRUE)
    ww.TT.t <- TTTdep[i-1,1:96] / sum(TTTdep[i-1,1:96],na.rm=TRUE)
    ww.CC.t <- CCCdep[i-1,1:96] / sum(CCCdep[i-1,1:96],na.rm=TRUE)
    Div.ww.1.t[i] <- exp(-sum(ww.1.t* log(ww.1.t),na.rm=TRUE))
    Div.ww.3.t[i] <- exp(-sum(ww.3.t* log(ww.3.t),na.rm=TRUE))
    Div.CC.t[i]   <- exp(-sum(ww.CC.t* log(ww.CC.t),na.rm=TRUE))
    Div.TT.t[i]   <- exp(-sum(ww.TT.t* log(ww.TT.t),na.rm=TRUE))
    ww1 <- ww.1.t
    ww3 <- ww.3.t
    wwT <- ww.TT.t
    ww1[is.na(ww1)==1] <- 0
    ww3[is.na(ww3)==1] <- 0
    wwT[is.na(wwT)==1] <- 0
    KL.ww1.TT.t[i] <- KLD(ww1,wwT)$intrinsic.discrepancy
    KL.ww2.TT.t[i] <- KLD(ww3,wwT)$intrinsic.discrepancy
    KL.TT.pop.t[i] <- KLD(wwT,pop.w)$intrinsic.discrepancy
    ss <- length(ww1)
    JS1[i] <- 0.5*(sum(ww1*log(ww1/ss))+sum(wwT*log(wwT / ss)))
    JS3[i] <- 0.5*(sum(ww3*log(ww3/ss))+sum(wwT*log(wwT / ss)))}




plot(Div.TT.t,col="blue",lwd=3,type="l",ylim=c(0,100))
lines(Div.ww.1.t,col="red",lwd=3,type="l",ylim=c(0,100))
lines(Div.ww.3.t,col="green",lwd=3,type="l",ylim=c(0,100))
lines(Div.ww.3.t,col="green",lwd=3,type="l",ylim=c(0,100))


plot(KL.ww1.TT.t,col="blue",lwd=3,type="l",ylim=c(0,1))
lines(KL.ww2.TT.t,col="red",lwd=3,type="l",ylim=c(0,1))
lines(KL.TT.pop.t,col="black",lwd=3,type="l",ylim=c(0,1))
    






############################################
############################################


plot(EEdep[,13],col="blue",lwd=3,cex.lab=2,cex.axis=2,
     type="l",ylim=c(0,6.8))
lines(c(0,nn),c(1,1),lty=2,lwd=3)
lines(EEdep[,75],col="red",lwd=3,type="l")
lines(EEdep[,69],col="green",lwd=3,type="l")


plot(filter(EEdep[,13],rep(1/4,4)),
     col="blue",lwd=3,cex.lab=2,cex.axis=2,
     type="l",ylim=c(0,4.2))
lines(c(0,nn),c(1,1),lty=2,lwd=3)
lines(filter(EEdep[,75],rep(1/4,4)),col="red",lwd=3,type="l")
lines(filter(EEdep[,69],rep(1/4,4)),col="green",lwd=3,type="l")
lines(filter(EEdep[,31],rep(1/4,4)),col="black",lwd=3,type="l")


tests <- cumsum(T[cl_age90==0])
cases <- cumsum(P[cl_age90==0])

tests.All <- cumsum(T[cl_age90==0])
cases.All <- cumsum(P[cl_age90==0])

d.tests.All <- T[cl_age90==0]
d.cases.All <- P[cl_age90==0]

date.All <- jour[cl_age90==0]

## Figure 1

CC <- round(cbind(seq(1,length(cases.All))/length(cases.All)*14,
      cases.All/cases.All[length(cases.All)]*10),3)

write.table(CC,file="cases.txt",sep="\t",
      row.names=FALSE,col.names=FALSE)

d.CC <- round(cbind(seq(1,length(d.cases.All))/
                    length(d.cases.All)*14,
        d.cases.All/max(d.cases.All)*10),3)

write.table(d.CC,file="d.cases.txt",sep="\t",
      row.names=FALSE,col.names=FALSE)

TT <- round(cbind(seq(1,length(tests.All))/length(tests.All)*14,
      tests.All/tests.All[length(tests.All)]*10),3)

write.table(TT,file="tests.txt",sep="\t",
      row.names=FALSE,col.names=FALSE)

d.TT <- round(cbind(seq(1,length(d.tests.All))/
                    length(d.tests.All)*14,
        d.tests.All/max(d.tests.All)*10),3)

write.table(d.TT,file="d.tests.txt",sep="\t",
      row.names=FALSE,col.names=FALSE)

### Figure 2

nn <- length(cases.All)

ddd <- rep(0,nn)
cases.T <- rep(0,nn)
tests.T <- rep(0,nn)

for (i in 2:nn){
    tests  <- tests.All[1:i] / tests.All[i]
    cases  <- cases.All[1:i] / cases.All[i]
    dcases <- diff(cases)
    dtests <- diff(tests)
    ddd[i] <- dcases[i-1] / dtests[i-1]}

cbind(seq(2,nn),
      diff(cases.All/cases.All[nn])/diff(tests.All/tests.All[nn]),
      ddd[2:nn])

# fig a until date 32 (one month)
#################################

date.All[32] # June 13 

SS <- round(cbind(tests.All[1:31]/tests.All[31],
                  cases.All[1:31]/cases.All[31])*10,2)

write.table(SS,file="fig2.a.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

# Tangent line

(1-ddd[31])*10 # 1.899

# fig a until date 62 (two month)
#################################

date.All[62] # July 13 

SS <- round(cbind(tests.All[1:61]/tests.All[61],
                  cases.All[1:61]/cases.All[61])*10,2)

write.table(SS,file="fig2.b.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

# Tangent line June 13

dd.2 <- diff(cases.All/cases.All[61])/
    diff(tests.All/tests.All[61])
dd.3 <- dd.2[1:61]

(1-dd.3[31])*10 # 2.88

c(SS[31,1],SS[31,2]) # 4.79 5.44

# Tangent line July 13

-(1-ddd[61])/ddd[61]*10 # 0.0683

# fig a until date 93 (three month)
###################################

date.All[93] # August 13 

SS <- round(cbind(tests.All[1:92]/tests.All[92],
                  cases.All[1:92]/cases.All[92])*10,2)

write.table(SS,file="fig2.c.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

# Tangent line June 13

dd.2 <- diff(cases.All/cases.All[92])/
         diff(tests.All/tests.All[92])
dd.3 <- dd.2[1:92]

(1-dd.3[31])*10 # 3.59

c(SS[31,1],SS[31,2]) # 2.40 2.45

c(SS[61,1],SS[61,2]) # 5.01 4.51

# Tangent line

-(1-ddd[92])/ddd[92]*10 # 4.51


# fig a until date 124 (four months)
###################################

date.All[124] # Septemner 13 

SS <- round(cbind(tests.All[1:123]/tests.All[123],
                  cases.All[1:123]/cases.All[123])*10,2)

write.table(SS,file="fig2.d.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

# Tangent line June 13

dd.2 <- diff(cases.All/cases.All[92])/
         diff(tests.All/tests.All[92])
dd.3 <- dd.2[1:92]

(1-dd.3[31])*10 # 3.59

c(SS[31,1],SS[31,2]) # 1.26 0.67
c(SS[61,1],SS[61,2]) # 2.62 1.24
c(SS[92,1],SS[92,2]) # 5.23 2.74

# Tangent line

-(1-ddd[123])/ddd[123]*10 # 3.89

# fig a until date 142 > End sample
###################################

date.All[142] # End sample

SS <- round(cbind(tests.All[1:141]/tests.All[141],
                  cases.All[1:141]/cases.All[141])*10,2)

write.table(SS,file="fig2.e.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

# Tangent line June 13

dd.2 <- diff(cases.All/cases.All[92])/
         diff(tests.All/tests.All[92])
dd.3 <- dd.2[1:92]

(1-dd.3[31])*10 # 3.59

c(SS[31,1],SS[31,2]) # 1.26 0.67
c(SS[61,1],SS[61,2]) # 2.62 1.24
c(SS[92,1],SS[92,2]) # 5.23 2.74
c(SS[123,1],SS[123,2]) # 5.23 2.74

# Tangent line

-(1-ddd[142])/ddd[142]*10 # 5.65

###### FIGURE 3
######################

# Positivity rate

p.rate <- cases.All / tests.All

PP <-  cbind(seq(1,length(p.rate))/length(p.rate)*14,
             p.rate/max(p.rate)*10)

write.table(round(PP,2),file="fig3.positivity.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

# Positivity rate

dderiv <- diff(cases.All) / diff(tests.All)

DDeriv <-  cbind(seq(1,length(dderiv))/length(dderiv)*14,
             dderiv/max(dderiv)*10)

write.table(round(DDeriv,2),file="fig3.derivative.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)


# Elasticity

DDD <- cbind(seq(1,length(ddd))/length(ddd)*14,
             ddd/max(ddd)*10)

write.table(round(DDD,2),file="fig3.elasticity.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

10/max(ddd) # elasticity 1 > 4.05


###### FIGURE 4
######################

AgeGroup <- unique(cl_age90)
nAge     <- length(AgeGroup) - 1 

EEage   <- matrix(0,nn,nAge)

for (aa in 1:nAge){
    print(AgeGroup[aa])
    tests.aa <- cumsum(T[cl_age90==AgeGroup[aa]])
    cases.aa <- cumsum(P[cl_age90==AgeGroup[aa]])
    for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                EEage[i,aa] <- dcases[i-1] / dtests[i-1]}}

m.EE <- max(EEage)

10/max(m.EE) # elasticity 1 > 2.56


for (aa in 1:nAge){
    OUT <- cbind(seq(1,dim(EEage)[1])/dim(EEage)[1]*14,
             EEage[,aa]/max(EEage)*10)
    ffile <- paste("fig4.age",aa,".txt",sep="")
    write.table(round(OUT,2),file=ffile,sep="\t",
            row.names=FALSE,col.names=FALSE)}

EEage89 <- rep(0,nn)
tests.aa <- cumsum(T[cl_age90==89|cl_age90==89])
cases.aa <- cumsum(P[cl_age90==89cl_age90==89])
for (i in 2:nn){
                tests  <- tests.aa[1:i] / tests.aa[i]
                cases  <- cases.aa[1:i] / cases.aa[i]
                dcases <- diff(cases)
                dtests <- diff(tests)
                EEage89[i] <- dcases[i-1] / dtests[i-1]}

OUT <- cbind(seq(1,nn)/nn*14,
             EEage89/max(EEage)*10)

write.table(round(OUT,2),file="fig4.age89.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)

OUT <- cbind(seq(1,nn)/nn*14,
             ddd/max(EEage)*10)

write.table(round(OUT,2),file="fig4.elasticity.txt",sep="\t",
            row.names=FALSE,col.names=FALSE)


###

PP <- cases.All/max(cases.All)
TT <- tests.All/max(tests.All)

ee <- rep(0,92)
tests.0 <- tests.All
cases.0 <- cases.All

for (i in 2:92){
    tests  <- tests.0[1:i] / tests.0[i]
    cases  <- cases.0[1:i] / cases.0[i]
    dcases <- diff(cases)
    dtests <- diff(tests)
    ee[i] <- dcases[i-1] / dtests[i-1]}

cbind(TT[2:92],PP[2:92],ee[2:92])


v1 <- (PP[2]-PP[1])/(TT[2]-TT[1])
v2 <- (PP[92]-PP[91])/(TT[92]-TT[91])

pd1 <- cases.All[2]/tests.All[2]
pd2 <- cases.All[91]/tests.All[91]

v1
v2
pd1
pd2
v1*pd1
v2*pd2

###


PP <- cases.J/max(cases.J)
TT <- tests.J/max(tests.J)

v1 <- (PP[2]-PP[1])/(TT[2]-TT[1])
v2 <- (PP[92]-PP[91])/(TT[92]-TT[91])

pd1 <- cases.J[2]/tests.J[2]
pd2 <- cases.J[91]/tests.J[91]

v1
v2
pd1
pd2
v1*pd1
v2*pd2

###

PP <- cases.V/max(cases.V)
TT <- tests.V/max(tests.V)

v1 <- (PP[2]-PP[1])/(TT[2]-TT[1])
v2 <- (PP[92]-PP[91])/(TT[92]-TT[91])

pd1 <- cases.V[2]/tests.V[2]
pd2 <- cases.V[91]/tests.V[91]

v1
v2
pd1
pd2
v1*pd1
v2*pd2


### Derivatives & functions

library(lpridge)

dd <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=0,
            order=4)

dd1 <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=1,
            order=4)

function.x   <- dd$x.out
function.est <- dd$est

deriv.x   <- dd1$x.out
deriv.est <- dd1$est

m.deriv.1  <- deriv.est[200]
m.deriv.5  <- mean(deriv.est[195:200])
m.deriv.20 <- mean(deriv.est[180:200])

BB.1  <- 1 - m.deriv.1
BB.5  <- 1 - m.deriv.5
BB.20 <- 1 - m.deriv.20
    
plot(function.x, function.est, col="red",type="l",lwd=2)
points(tests/max(tests),
              cases/max(cases),lwd=2)
lines(c(0,1),c(0,1))

c(0,-BB.1/m.deriv.1)
c(0,1)

plot(c(

plot(deriv.x[,i], deriv.est[,i], col="blue",lwd=2,type="l",
         ylim=c(0,2))}


write.table(cbind(tests/max(tests),
                  cases/max(cases)),
            file="points-France.txt",col.names=FALSE,
            row.names=FALSE)

write.table(cbind(function.x,function.est),
            file="function-France.txt",col.names=FALSE,
            row.names=FALSE)

write.table(cbind(deriv.x,(deriv.est-.6)/1.8),
            file="deriv-France.txt",col.names=FALSE,
            row.names=FALSE)


dev.off()


### Djeuns

cl_age90
 0  9 19 29 39 49 59 69 79 89 90 
92 92 92 92 92 92 92 92 92 92 92 


tests.J <- cumsum(T[cl_age90==9]+
                T[cl_age90==19]+
                T[cl_age90==29]+
                T[cl_age90==39]+
                T[cl_age90==49]+
                T[cl_age90==59])

cases.J <- cumsum(P[cl_age90==9]+
                  P[cl_age90==19]+
                  P[cl_age90==29]+
                  P[cl_age90==39]+
                  P[cl_age90==49]+
                  P[cl_age90==59])

tests.V <- cumsum(T[cl_age90==69]+
                  T[cl_age90==79]+
                  T[cl_age90==89]+
                  T[cl_age90==90])

cases.V <- cumsum(P[cl_age90==69]+
                P[cl_age90==79]+
                P[cl_age90==89]+
                P[cl_age90==90])


tests  <- tests.J
cases <- cases.J

### Derivatives & functions

library(lpridge)

dd <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=0,
            order=4)

dd1 <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=1,
            order=4)

function.x   <- dd$x.out
function.est <- dd$est

deriv.x   <- dd1$x.out
deriv.est <- dd1$est

m.deriv.1  <- deriv.est[200]
m.deriv.5  <- mean(deriv.est[195:200])
m.deriv.20 <- mean(deriv.est[180:200])

BB.1  <- 1 - m.deriv.1
BB.5  <- 1 - m.deriv.5
BB.20 <- 1 - m.deriv.20

c(0,-BB.1/m.deriv.1)
c(0,1)


plot(function.x, function.est, col="red",type="l",lwd=2)
points(tests/max(tests),
              cases/max(cases),lwd=2)
    
plot(deriv.x[,i], deriv.est[,i], col="blue",lwd=2,type="l",
         ylim=c(0,2))}


write.table(cbind(tests/max(tests),
                  cases/max(cases)),
            file="points-France-J.txt",col.names=FALSE,
            row.names=FALSE)

write.table(cbind(function.x,function.est),
            file="function-France-J.txt",col.names=FALSE,
            row.names=FALSE)

write.table(cbind(deriv.x,(deriv.est-.6)/1.8),
            file="deriv-France-J.txt",col.names=FALSE,
            row.names=FALSE)

### Vioks


tests  <- tests.V
cases <- cases.V

### Derivatives & functions

library(lpridge)

dd <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=0,
            order=4)

dd1 <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=1,
            order=4)

function.x   <- dd$x.out
function.est <- dd$est

deriv.x   <- dd1$x.out
deriv.est <- dd1$est

m.deriv.1  <- deriv.est[200]
m.deriv.5  <- mean(deriv.est[195:200])
m.deriv.20 <- mean(deriv.est[180:200])

BB.1  <- 1 - m.deriv.1
BB.5  <- 1 - m.deriv.5
BB.20 <- 1 - m.deriv.20

c(0,-BB.1/m.deriv.1)
c(0,1)


plot(function.x, function.est, col="red",type="l",lwd=2)
points(tests/max(tests),
              cases/max(cases),lwd=2)
    
plot(deriv.x[,i], deriv.est[,i], col="blue",lwd=2,type="l",
         ylim=c(0,2))}


write.table(cbind(tests/max(tests),
                  cases/max(cases)),
            file="points-France-V.txt",col.names=FALSE,
            row.names=FALSE)

write.table(cbind(function.x,function.est),
            file="function-France-V.txt",col.names=FALSE,
            row.names=FALSE)

write.table(cbind(deriv.x,(deriv.est-.6)/1.8),
            file="deriv-France-V.txt",col.names=FALSE,
            row.names=FALSE)



tests <- cumsum(T[cl_age90==0])
cases <- cumsum(P[cl_age90==0])

### Derivatives & functions

library(lpridge)

dda <- lpepa(tests,
            cases, bandwidth=5,deriv=0,
            order=4)
dda1 <- lpepa(tests,
            cases, bandwidth=5,deriv=1,
            order=4)

dd <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=5,deriv=0,
            order=4)

dd1 <- lpepa(tests/max(tests),
            cases/max(cases), bandwidth=50,deriv=1,
            order=4)

function.x   <- dd$x.out
function.est <- dd$est

deriv.x    <- dd1$x.out
deriv.xa    <- dda1$x.out
deriv.est  <- dd1$est
deriv.esta <-  dda1$est

m.deriv.1  <- deriv.est[200]
m.deriv.5  <- mean(deriv.est[195:200])
m.deriv.20 <- mean(deriv.est[180:200])

BB.1  <- 1 - m.deriv.1
BB.5  <- 1 - m.deriv.5
BB.20 <- 1 - m.deriv.20
    
plot(function.x, function.est, col="red",type="l",lwd=2)
points(tests/max(tests),
              cases/max(cases),lwd=2)


pdf("First-Derivativ-France-withandwithoutNormalisation.pdf",
    width=6,height=6)

par(mfrow=c(2,2))

plot(deriv.x, deriv.est, col="blue",lwd=2,type="l",
         ylim=c(0,2))

plot(deriv.xa, deriv.esta, col="red",lwd=2,type="l")

plot(tests[2:92],diff(cases)/diff(tests),col="blue",lwd=2,type="l")
plot(test[2:92]/max(tests),
     diff(cases/max(cases))/diff(tests/max(tests)),,
     col="red",lwd=2,type="l")

elasticity <- diff(cases)/diff(tests)*tests[2:92]/cases[2:92]
s.elasticity <- spline(elasticity)


pdf("Elasticities-France-withandwithoutNormalisation.pdf",
    width=6,height=6)

plot(diff(cases/max(cases))/diff(tests/max(tests)),
     col="black",lwd=2,type="l",ylim=c(0.4,2.5))
lines(elasticity,col="blue",lwd=2,type="l")


dev.off()

pdf("Four-plots-Figure.pdf",
    width=6,height=6)

par(mfrow=c(2,2))

plot(diff(cases),
     col="black",lwd=2,type="l",
     main="(P_t-P_t-1)")

plot(diff(cases)/diff(test)*tests[2:92]/cases[2:92],
     col="red",lwd=2,type="l",
     main="diff(Pt)/diff(Dt)")

plot(cases/tests,
     col="black",lwd=2,type="l",
     main="Pt/Dt")

plot(diff(tests),
     col="black",lwd=2,type="l",
     main="(D_t-D_t-1)")

dev.off()


### Inflexion point



pdf("July-inflexion-France.pdf",
    width=18,height=18)


par(mfrow=c(3,3))

dd <- 9
ee <- rep(0,dd)
rr <- rep(0,dd)

for (i in 1:dd){
kk <- 55 + i

plot(tests[1:kk] /max(tests[1:kk]),
     cases[1:kk]/max(cases[1:kk]),
     col="blue",lwd=1,type="o",cex=2,
     xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
lines(c(0,1),c(0,1),lty=2,lwd=1,cex=2)
    text(x=.2,y=.9,labels=date.All[kk],cex=2)

ee[i] <- (1-cases[kk-1]/cases[kk])/(1-tests[kk-1]/tests[kk])
rr[i] <- cases[kk]/tests[kk]

    text(x=.2,y=.8,labels=round(ee[i],4),cex=2)
    text(x=.2,y=.7,labels=round(rr[i],4),cex=2)}


dev.off()


pdf("July-inflexion-France2.pdf",
    width=18,height=18)


par(mfrow=c(3,3))

dd <- 9
ee <- rep(0,dd)
rr <- rep(0,dd)

for (i in 1:dd){
kk <- 64 + i

plot(tests[1:kk] /max(tests[1:kk]),
     cases[1:kk]/max(cases[1:kk]),
     col="blue",lwd=1,type="o",cex=2,
     xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
lines(c(0,1),c(0,1),lty=2,lwd=1,cex=2)
    text(x=.2,y=.9,labels=date.All[kk],cex=2)

ee[i] <- (1-cases[kk-1]/cases[kk])/(1-tests[kk-1]/tests[kk])
rr[i] <- cases[kk]/tests[kk]

    text(x=.2,y=.8,labels=round(ee[i],4),cex=2)
    text(x=.2,y=.7,labels=round(rr[i],4),cex=2)}


dev.off()

pdf("July-inflexion-France3.pdf",
    width=18,height=18)


par(mfrow=c(3,3))

dd <- 9
ee <- rep(0,dd)
rr <- rep(0,dd)

for (i in 1:dd){
kk <- 63 + i

plot(tests[1:kk] /max(tests[1:kk]),
     cases[1:kk]/max(cases[1:kk]),
     col="blue",lwd=1,type="o",cex=2,
     xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
lines(c(0,1),c(0,1),lty=2,lwd=1,cex=2)
    text(x=.2,y=.9,labels=date.All[kk],cex=2)

ee[i] <- (1-cases[kk-1]/cases[kk])/(1-tests[kk-1]/tests[kk])
rr[i] <- cases[kk]/tests[kk]

    text(x=.2,y=.8,labels=round(ee[i],4),cex=2)
    text(x=.2,y=.7,labels=round(rr[i],4),cex=2)}


dev.off()

pdf("July-inflexion-France-All.pdf",
    width=18,height=18)


par(mfrow=c(3,3))

dd <- 90
ee <- rep(0,dd)
rr <- rep(0,dd)

for (i in 1:dd){
kk <- 2 + i

plot(tests[1:kk] /max(tests[1:kk]),
     cases[1:kk]/max(cases[1:kk]),
     col="blue",lwd=1,type="o",cex=2,
     xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
lines(c(0,1),c(0,1),lty=2,lwd=1,cex=2)
    text(x=.2,y=.9,labels=date.All[kk],cex=2)

ee[i] <- (1-cases[kk-1]/cases[kk])/(1-tests[kk-1]/tests[kk])
rr[i] <- cases[kk]/tests[kk]

    text(x=.2,y=.8,labels=round(ee[i],4),cex=2)
    text(x=.2,y=.7,labels=round(rr[i],4),cex=2)}


dev.off()


##########################################
##########################################
### MOVIE
##########################################
##########################################

library(lpridge)


tests.0 <- cumsum(T[cl_age90==0])
cases.0 <- cumsum(P[cl_age90==0])

cc <- 2


for (i in 10:92){
    if (i<10){nnum <- paste("00",i,sep="")}
    if (i>9&i<100){nnum <- paste("0",i,sep="")}
    if (i>99){nnum <- i}
    ff <- paste("./France-Gif/Estim",nnum,".png", sep = "")
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

convert -delay 20 -loop 0 *.png subject1.gif    

convert -resize 120% -delay 20 -loop 0 `ls -v` France-Estim.gif
convert -resize 120% -delay 20  `ls -v` France-Estim.gif

ddd <- rep(0,92)
cases.T <- rep(0,92)
tests.T <- rep(0,92)

for (i in 2:92){
    tests  <- tests.0[1:i] / tests.0[i]
    cases  <- cases.0[1:i] / cases.0[i]
    dcases <- diff(cases)
    dtests <- diff(tests)
    ddd[i] <- dcases[i-1] / dtests[i-1]}

cbind(,)

cbind(seq(2,92),
      diff(cases.0/cases.0[92])/diff(tests.0/tests.0[92]),
      ddd[2:92])

pdf("Updated-Elasticities-at-date-T-France.pdf",
    width=6,height=6)

plot(ddd[2:92],col="blue",
     lwd=2,type="l",xlab="date",ylab="Elasticity",
     cex=2)
lines(c(1,92),c(1,1),lwd=2,lty=2)

dev.off()

X11()
plot(tests.0,cases.0,lwd=2,col="blue",type="o")
lines(c(tests.0[1],tests.0[92]),c(cases.0[1],cases.0[92]),
      lty=2,lwd=3)



tests.0 <- cumsum(T[cl_age90==0])
cases.0 <- cumsum(P[cl_age90==0])

cc <- 2

for (i in 10:92){
    if (i<10){nnum <- paste("00",i,sep="")}
    if (i>9&i<100){nnum <- paste("0",i,sep="")}
    if (i>99){nnum <- i}
    ff <- paste("./France-Gif2/Points",nnum,".png", sep = "")
    print(ff)
    png(filename = ff,
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "white")
    # Figure
    tests <- tests.0[1:i]
    cases <- cases.0[1:i]
    plot(tests,cases,lwd=2,col=ccolor,
         type="o",xlim=c(0,4396283),ylim=c(0,71702))
    rr <- paste("day",i, sep = " ")
    print(rr)
    text(x=500000,y=68000,labels=rr,cex=cc)
    dev.off()}



pdf("Epidemics Dynamics.pdf",
    width=12,height=12)

par(mfrow=c(2,2))

plot(cases.0,lwd=2,col="black",type="l",
     main="Cumulated cases")
    
   plot(tests.0,cases.0,lwd=2,col=ccolor,
        type="l",xlim=c(0,4400000),ylim=c(0,72000),
        main="Cumulated cases vs. Cumulated tests")

   plot(ddd[2:92],col="blue",
        lwd=2,type="l",xlab="date",ylab="Elasticity",
        cex=2,main="Elasticity at date t")
   lines(c(1,92),c(1,1),lwd=2,lty=2)


plot(tests.0,lwd=2,col="black",type="l",
     main="Cumulated tests")

dev.off()

pdf("Epidemics dynamics 2.pdf",
    width=12,height=12)

par(mfrow=c(2,2))

plot(cases.0,lwd=2,col="black",type="l",
     main="Cumulated cases")
    
   plot(cases.0/tests.0,lwd=2,col=ccolor,
        type="l",xlim=c(0,92),ylim=c(0,0.025),
        main="Positivity rate at date t")

   plot(ddd[2:92],col="blue",
        lwd=2,type="l",xlab="date",ylab="Elasticity",
        cex=2,main="Elasticity at date t")
   lines(c(1,92),c(1,1),lwd=2,lty=2)


plot(tests.0,lwd=2,col="black",type="l",
     main="Cumulated tests")

dev.off()

pdf("Epidemics dynamics 3.pdf",
    width=12,height=12)

par(mfrow=c(1,3))

   plot(cases.0/tests.0,lwd=2,col=ccolor,
        type="l",xlim=c(0,92),ylim=c(0,0.03),
        main="Positivity rate at date t")

   plot(cases.0[2:92]/tests.0[2:92]*ddd[2:92],
     lwd=2,col=ccolor,
        type="l",xlim=c(0,92),ylim=c(0,0.03),
        main="Positivity rate X elasticity at date t")

   plot(ddd[2:92],col="blue",
        lwd=2,type="l",xlab="date",ylab="Elasticity",
        cex=2,main="Elasticity at date t")
   lines(c(1,92),c(1,1),lwd=2,lty=2)


dev.off()

pdf("Epidemics dynamics Graph 1.pdf",
    width=12,height=12)

par(mfrow=c(2,2))

   plot(cases.0,lwd=2,col=ccolor,
        type="l",
        main="Cumulated cases")

   plot(tests.0/tests.0[92],cases.0/cases.0[92],
     lwd=2,col=ccolor,
        type="l",xlim=c(0,1),ylim=c(0,1),
        main="Tests against cases")

plot.new()

   plot(tests.0,lwd=2,col=ccolor,
        type="l",
        main="Cumulated tests")


dev.off()

pdf("Epidemics dynamics Graph 2.pdf",
    width=12,height=12)

par(mfrow=c(2,2))

   plot(cases.0/tests.0,lwd=2,col=ccolor,
        type="l",xlim=c(0,92),ylim=c(0,0.03),
        main="Positivity rate at date t")

   plot(cases.0[2:92]/tests.0[2:92]*ddd[2:92],
     lwd=2,col=ccolor,
        type="l",xlim=c(0,92),ylim=c(0,0.03),
        main="Positivity rate X elasticity at date t")

plot.new()
!
   plot(ddd[2:92],col="blue",
        lwd=2,type="l",xlab="date",ylab="Elasticity",
        cex=2,main="Elasticity at date t")
   lines(c(1,92),c(1,1),lwd=2,lty=2)


dev.off()


### Daily graph

daily.t <- diff(tests.All)
daily.c <- diff(cases.All)

plot(daily.t,daily.c,lwd=3,cex.lab=2,cex.axis=2,
     col="blue",type="p",cex=3)
