#############################################

# Datasets loading

#############################################

#Read dataset
wd <- setwd("H:\\.windows_settings\\Desktop\\Survival Analysis\\R workflow")

#wd_home <- setwd("/Users/francescou/Desktop/R for paper/")

SurvIn1<-read.csv(file="SurvivalInput_41751.csv", header=TRUE)   #nrow(SurvIn[SurvIn$Status==1,])
SurvIn1$Gender<-ifelse(SurvIn1$Gender=="M", 1, 0)
SurvIn1$Status<-ifelse(SurvIn1$Status=="DEATH", 1, 0)

SurvIn2<-read.csv(file="SurvivalInput_41755.csv", header=TRUE)   #nrow(SurvIn[SurvIn$Status==1,])
SurvIn2$Gender<-ifelse(SurvIn2$Gender=="M", 1, 0)
SurvIn2$Status<-ifelse(SurvIn2$Status=="DEATH", 1, 0)
SurvIn2<-as.data.frame(SurvIn2)
SurvIn2$Reserve <- NULL    #erase column

SurvIn1 <- unique(SurvIn1)
SurvIn2 <- unique(SurvIn2)

library(xtable)
######################################

# Dataset definition

######################################

#Verify deaths occurrence
###SurvIn1$EndObs<-SurvIn1$EntryYear+SurvIn1$TimeObserved
###SurvIn2$EndObs<-SurvIn2$EntryYear+SurvIn2$TimeObserved

SurvIn <- merge(SurvIn1, SurvIn2, by=c("EntryYear", "EntryAge", "TimeObserved", "EntryDuration", "Gender", "Status"), all=TRUE)

SurvIn$Benefit.x[is.na(SurvIn$Benefit.x)] <- -1
SurvIn$Benefit.y[is.na(SurvIn$Benefit.y)] <- -1

#Considering the eventuality that the common individual between these two datasets gets more benefits
SurvIn[,"Benefit"] <- apply(SurvIn[,c(7,8)], 1, max)

#Elimination of duplicated rows
SurvIn <- SurvIn[,c(1,2,3,4,5,6,9,10)]
SurvIn <- unique(SurvIn)
#####################################

# - Initial characteristics of the working dataset

## - Number of males and females
nrow(SurvIn[SurvIn$Gender==0,])

## - Total time observed
sum(SurvIn$TimeObserved)
nrow(SurvIn[(SurvIn$EntryYear+SurvIn$TimeObserved)<2000,])

## - Observed deaths
nrow(SurvIn[SurvIn$Status==1,])
nrow(SurvIn[SurvIn$Status==1 & SurvIn$Gender==0,])

## - Benefit amount histogram
hist(SurvIn$Benefit, breaks=500, main="Histogram of Benefit distribution", xlab="Benefit amount")

# Dataset cleansing

#####################################

#Erasing Exposures under 60 and adjusting the time observed, the entry year and the entry duration, erase exposures after 31/12/2009, and consider
#just male population

##Erase exposures under 60
StartingAge<-60
SurvIn <- SurvIn[!((SurvIn$EntryAge+SurvIn$TimeObserved) < StartingAge),]

##Variable adjustment
SurvIn$EntryYear[((SurvIn$EntryAge+SurvIn$TimeObserved) >= StartingAge & SurvIn$EntryAge < StartingAge)] <- SurvIn$EntryYear[((SurvIn$EntryAge+SurvIn$TimeObserved)>= StartingAge & SurvIn$EntryAge< StartingAge)] + StartingAge - SurvIn$EntryAge[((SurvIn$EntryAge+SurvIn$TimeObserved)>= StartingAge & SurvIn$EntryAge< StartingAge)]
SurvIn$EntryDuration[((SurvIn$EntryAge+SurvIn$TimeObserved) >= StartingAge & SurvIn$EntryAge < StartingAge)] <- SurvIn$EntryDuration[((SurvIn$EntryAge+SurvIn$TimeObserved) >= StartingAge & SurvIn$EntryAge < StartingAge)] + StartingAge - SurvIn$EntryAge[((SurvIn$EntryAge+SurvIn$TimeObserved)>= StartingAge & SurvIn$EntryAge < StartingAge)]

###Creation of two new variables TObs for TimeObserved and EntAge for EntryAge
SurvIn$TObs<-SurvIn$TimeObserved
SurvIn$EntAge<-SurvIn$EntryAge
SurvIn$TObs[((SurvIn$EntryAge+SurvIn$TimeObserved) >= StartingAge & SurvIn$EntryAge < StartingAge)] <- SurvIn$TimeObserved[((SurvIn$EntryAge+SurvIn$TimeObserved)>= StartingAge & SurvIn$EntryAge< StartingAge)] - StartingAge + SurvIn$EntryAge[((SurvIn$EntryAge+SurvIn$TimeObserved)>= StartingAge & SurvIn$EntryAge< StartingAge)]
SurvIn$EntAge[((SurvIn$EntryAge+SurvIn$TimeObserved) >= StartingAge & SurvIn$EntryAge < StartingAge)] <- StartingAge

##Erase exposures after 31/12/2009
SurvIn<-SurvIn[!SurvIn$EntryYear>=2010,]
SurvIn$TObs[(SurvIn$EntryYear+SurvIn$TObs)>=2010]<- 2010 - SurvIn$EntryYear[(SurvIn$EntryYear+SurvIn$TObs)>=2010]

#Indicator check for benefit size
######SurvIn$Ind<-ifelse(SurvIn$Benefit.x>=SurvIn$Benefit.y, 1, 0)

##########################################

# Further elaborations before starting

##########################################

##Erasing female population
SurvInM <- SurvIn[!SurvIn$Gender==0,]

#Creation of the holed datasets (then consider nested holes)
SurvIn$Group<-as.character(SurvIn$Group)
SurvInM$Group<-as.character(SurvInM$Group)

#Dataset with absent geodemographic profile
SurvIn$Group[is.na(SurvIn$Group)]<- -1
SurvInM$Group[is.na(SurvInM$Group)]<- -1

#Treat code 98 and 99 as missing
SurvIn$Group[SurvIn$Group=="98" | SurvIn$Group=="99"] <- -1
SurvInM$Group[SurvInM$Group=="98" | SurvInM$Group=="99"] <- -1


#Working Dataset (only complete observations)
SurvInMW <- SurvInM[SurvInM$Group != "-1",]

#####################################################################################################

# - Descriptive characteristics of the mortality experience

#####################################################################################################

sum(SurvInMW$TObs)
sum(SurvInMW$Status)
min(SurvInMW$EntryYear)
max(SurvInMW$EntryYear + SurvInMW$TObs)

sum(SurvInM$TObs)
sum(SurvInM$Status)
min(SurvInM$EntryYear)
max(SurvInM$EntryYear + SurvInM$TObs)

quantile(SurvInMW$Benefit, 0.7) 
nrow(SurvInMW[SurvInMW$Benefit < 8500,])/nrow(SurvInMW)

############################################################################################################

# - Profilers split
cov2cor(solve(Model0$hessian))
############################################################################################################

# - 1 Fit a model with alpha and beta only for the whole population

NegLogL=function(vdParameters){    #
  
  x = SurvInMW$EntAge
  t = SurvInMW$TObs
  d = SurvInMW$Status
  
  a=vdParameters[1]
  b=vdParameters[2]
  #depsilon = vdParameters[3]
  
  #Weighted log-likelihoods
  
  logL <-  -exp(a + b * x)*(exp(b*t)-1)/b + d*(a + b * (x + t))
  
  #logL <-  -exp(a + b * (x-mean(x)))*(exp(b*t)-1)/b + d*(a + b * ((x-mean(x)) + t))
  
  #logL <-  -exp(a + b * (x-60))*(exp(b*t)-1)/b + d*(a + b * ((x-60) + t))

  #Makeham
  #logL <-  -exp(a + b * x)*(exp(b*t)-1)/b - t * exp(depsilon) + d * (log(exp(a + b * (x + t)) + exp(depsilon)))
  
  return(-(sum(logL)))
}

par_init <- c(-10, 0.1)
#par_init <- c(-10, 0.1, -5)
Model0 <- nlm(NegLogL, p=par_init, typsize=par_init, hessian=T, iterlim=1000)

#Model_Mkh <- nlm(NegLogL, p=par_init, typsize=par_init, hessian=T, iterlim=1000)

beta_fixed <- Model0[[2]][2]

profiling <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "90", "91", "92")
profiling_table <- matrix(NA, 6, 3)

profiling_table[1,1] <- nrow(SurvInMW[SurvInMW$Group=="A",])
profiling_table[2,1] <- nrow(SurvInMW[SurvInMW$Group=="B",])
profiling_table[3,1] <- nrow(SurvInMW[SurvInMW$Group=="C",])
profiling_table[4,1] <- nrow(SurvInMW[SurvInMW$Group=="D",])
profiling_table[5,1] <- nrow(SurvInMW[SurvInMW$Group=="E",])
profiling_table[6,1] <- nrow(SurvInMW[SurvInMW$Group=="F",])
profiling_table[1,2] <- nrow(SurvInMW[SurvInMW$Group=="G",])
profiling_table[2,2] <- nrow(SurvInMW[SurvInMW$Group=="H",])
profiling_table[3,2] <- nrow(SurvInMW[SurvInMW$Group=="I",])
profiling_table[4,2] <- nrow(SurvInMW[SurvInMW$Group=="J",])
profiling_table[5,2] <- nrow(SurvInMW[SurvInMW$Group=="K",])
profiling_table[6,2] <- nrow(SurvInMW[SurvInMW$Group=="L",])
profiling_table[1,3] <- nrow(SurvInMW[SurvInMW$Group=="M",])
profiling_table[2,3] <- nrow(SurvInMW[SurvInMW$Group=="N",])
profiling_table[3,3] <- nrow(SurvInMW[SurvInMW$Group=="O",])
profiling_table[4,3] <- nrow(SurvInMW[SurvInMW$Group=="90",])
profiling_table[5,3] <- nrow(SurvInMW[SurvInMW$Group=="91",])
profiling_table[6,3] <- nrow(SurvInMW[SurvInMW$Group=="92",])


## - 2.1 - Subset dataset with geo-demographic observed

#Recoding - numbering for each geo-demographic
SurvInMW$Numbering <- ifelse(SurvInMW$Group=="A",1, ifelse(SurvInMW$Group=="B",2,ifelse(SurvInMW$Group=="C",3,ifelse(SurvInMW$Group=="D",4, ifelse(SurvInMW$Group=="E",5,ifelse(SurvInMW$Group=="F",6,
                            ifelse(SurvInMW$Group=="G",7, ifelse(SurvInMW$Group=="H",8,ifelse(SurvInMW$Group=="I",9,ifelse(SurvInMW$Group=="J",10,ifelse(SurvInMW$Group=="K",11,ifelse(SurvInMW$Group=="L",12,ifelse(SurvInMW$Group=="M",13,ifelse(SurvInMW$Group=="N",14,
                                  ifelse(SurvInMW$Group=="O",15,16)) )) )))))))))))


## - 2.2 - Parameter and storing
par_storage <- matrix(NA, nrow=16, ncol=2)

for (i in 1:16){
  
  NegLogL_g=function(vdParameters){    #
    
    x = SurvInMW$EntAge[SurvInMW$Numbering==i]
    t = SurvInMW$TObs[SurvInMW$Numbering==i]
    d = SurvInMW$Status[SurvInMW$Numbering==i]
    
    a = vdParameters[1]
    b = beta_fixed
    
    #Weighted log-likelihoods
    
    logL <-  -exp(a + b * x )*(exp(b*t)-1)/b + d*(a + b * (x + t) )
    return(-(sum(logL)))
  }
  
  par_init <- -10
  Model1 <- nlm(NegLogL_g, p=par_init, typsize=par_init, hessian=T, iterlim=1000)
  
  par_storage[i,1] <- Model1[[2]][1]
}

colnames(par_storage) <- c("alpha_geo", "std")
rownames(par_storage) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "9X")
par_storage <- as.data.frame(par_storage)

## - Calculate difference from the baseline F
par_storage$std <- (par_storage$alpha_geo - mean(par_storage$alpha_geo)) / sd(par_storage$alpha_geo)


# - 3 - Clustering - Ward/Calinski-Harabasz

## - 3.1 - Dendrogram
hc_geo = hclust(dist(par_storage$std), method = "ward.D2") #
plot(hc_geo, labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "9X"),
     main = "Cluster Dendrogram of Geodemographic Profiles", xlab = "MOSAIC profiles")

xtable(par_storage, digits=c(0,3,3))


# - 4 - Clustering on the base of benefit amount (use percentiles)

## - 4.1 - Parameter and storing

######## - Subsettment

B_par_storage_4 <- matrix(NA, nrow=4, ncol=1)

for (i in 1:4){
  
  q_1 <- quantile(SurvInMW$Benefit, probs = (0.25*(i-1)))
  q <- quantile(SurvInMW$Benefit, probs = (0.25*i))
  
  NegLogL_b=function(vdParameters){    #
    
    SurvBen <- SurvInMW[SurvInMW$Benefit>q_1 & SurvInMW$Benefit<=q, ]
    
    x = SurvBen$EntAge
    t = SurvBen$TObs
    d = SurvBen$Status
    
    a = vdParameters[1]
    b = beta_fixed
    
    #Weighted log-likelihoods
    
    logL <-  -exp(a + b * x)*(exp(b*t)-1)/b + d*(a + b * (x + t))
    return(-(sum(logL)))
  }
  
  par_init <- c(-10, 0)
  Model2 <- nlm(NegLogL_b, p=par_init, typsize=par_init, hessian=T, iterlim=1000)
  
  B_par_storage_4[i,1] <- Model2[[2]][1]
}

colnames(B_par_storage_4) <- c("alpha")
B_par_storage_4 <- as.data.frame(B_par_storage_4)
B_par_storage_4$std <- (B_par_storage_4$alpha - mean(B_par_storage_4$alpha))/sd(B_par_storage_4$alpha)
B_par_storage_4$quant <- quantile(SurvInMW$Benefit, probs = (0.25*c(1:4)))
rownames(B_par_storage_4) <- c(1:4)

hc_ben4 = hclust(dist((B_par_storage_4$std)), method = "ward.D2")#, , method = "ward.D"
plot(hc_ben4, main="Benefit quartiles dendrogram", xlab="Quartiles")

nrow(SurvInMW[SurvInMW$Benefit<8500,])/nrow(SurvInMW)

##########################################################################

# - Complete data comparison

##########################################################################

# - 0 Creation of DS_C, the working dataset

# Complete dataset
DS_C <- SurvInMW[,c(1,6,7,8,9,10)]
DS_C$GeoGroup <- ifelse((DS_C$Group=="A" | DS_C$Group=="B" | DS_C$Group=="C" | DS_C$Group=="D" | DS_C$Group=="E"),2, ifelse((DS_C$Group=="O" | DS_C$Group=="J" | DS_C$Group=="K" | DS_C$Group=="90" | DS_C$Group=="91" | DS_C$Group=="92"),0,1))
DS_C$BL <- ifelse(DS_C$Benefit < 8500,1,0)
DS_C$BH <- 1 - DS_C$BL
DS_C$C0 <- ifelse(DS_C$GeoGroup==0, 1, 0)
DS_C$C1 <- ifelse(DS_C$GeoGroup==1, 1, 0)
DS_C$C2 <- ifelse(DS_C$GeoGroup==2, 1, 0)

#DS_C <- as.data.frame(DS_C)

# - Socio economic characteristics table
socio_econ <- matrix(NA, 4, 3)
socio_econ[1,1] <- nrow(DS_C[DS_C$C0==1 & DS_C$BL==1,])
socio_econ[1,2] <- nrow(DS_C[DS_C$C1==1 & DS_C$BL==1,])
socio_econ[1,3] <- nrow(DS_C[DS_C$C2==1 & DS_C$BL==1,])
socio_econ[3,1] <- nrow(DS_C[DS_C$C0==1 & DS_C$BL==0,])
socio_econ[3,2] <- nrow(DS_C[DS_C$C1==1 & DS_C$BL==0,])
socio_econ[3,3] <- nrow(DS_C[DS_C$C2==1 & DS_C$BL==0,])

socio_econ[2,1] <- nrow(DS_C[DS_C$C0==1 & DS_C$BL==1,])/nrow(DS_C)
socio_econ[2,2] <- nrow(DS_C[DS_C$C1==1 & DS_C$BL==1,])/nrow(DS_C)
socio_econ[2,3] <- nrow(DS_C[DS_C$C2==1 & DS_C$BL==1,])/nrow(DS_C)
socio_econ[4,1] <- nrow(DS_C[DS_C$C0==1 & DS_C$BL==0,])/nrow(DS_C)
socio_econ[4,2] <- nrow(DS_C[DS_C$C1==1 & DS_C$BL==0,])/nrow(DS_C)
socio_econ[4,3] <- nrow(DS_C[DS_C$C2==1 & DS_C$BL==0,])/nrow(DS_C)

# - 1.1 Fit a model with alpha, beta and gamma only for the whole population

NegLogL=function(vdParameters){    #
  
  x = DS_C$EntAge
  t = DS_C$TObs
  d = DS_C$Status
  ben = DS_C$BH
  
  a=vdParameters[1]
  b=vdParameters[2]
  c=vdParameters[3]
  
  #Weighted log-likelihoods
  
  logL <-  -exp(a + b * x + c * ben)*(exp(b*t)-1)/b + d*(a + b * (x + t) + c * ben)
  return(-(sum(logL)))
}

par_init <- c(-10, 0.1, 0)
Model1 <- nlm(NegLogL, p=par_init, typsize=par_init, hessian=T, iterlim=1000)

# - 1.1 Fit a model with alpha, beta and delta1/2 only for the whole population

NegLogL=function(vdParameters){    #
  
  x = DS_C$EntAge
  t = DS_C$TObs
  d = DS_C$Status
  geo1 = DS_C$C1
  geo2 = DS_C$C2
  
  a=vdParameters[1]
  b=vdParameters[2]
  de1=vdParameters[3]
  de2=vdParameters[4]
  
  #Weighted log-likelihoods
  
  logL <-  -exp(a + b * x + geo1 * de1 + geo2 * de2)*(exp(b*t)-1)/b + d*(a + b * (x + t) + geo1 * de1 + geo2 * de2)
  return(-(sum(logL)))
}

par_init <- c(-10, 0.1, 0, 0)
Model2 <- nlm(NegLogL, p=par_init, typsize=par_init, hessian=T, iterlim=1000)

NegLogL=function(vdParameters){    #
  
  x1 = DS_C$EntAge
  t1 = DS_C$TObs
  d1 = DS_C$Status
  
  ben1 = DS_C$BH
  
  gde1 = DS_C$C1
  gde2 = DS_C$C2
  
  a = vdParameters[1] #alpha
  b = vdParameters[2] #beta
  c = vdParameters[3] #delta1
  de1 = vdParameters[4] #delta2
  de2 = vdParameters[5]
  #epsilon1 = vdParameters[6]
  
  #Weighted log-likelihoods
  
  # - Gompertz
  logL_C <-  -exp(a + b * x1 + c * ben1 + de1 * gde1 + de2 * gde2) * (exp(b * t1) - 1)/b + d1 * (a + b * (x1 + t1) + c * ben1 + de1 * gde1 + de2 * gde2) 
  
  # - Makeham
  #logL_C <-  -exp(a + b * x1 + c * ben1 + de1 * gde1 + de2 * gde2) * (exp(b * t1) - 1)/b - (t1 * exp(epsilon1)) + d1 * log( exp(a + b * (x1 + t1) + c * ben1 + de1 * gde1 + de2 * gde2) + exp(epsilon1) )
  
  return(-(sum(logL_C)))
}
st_val2 <- c(-10, 0.1, 0, 0, 0)
# - Benchmark
theta_est_BMK2 <- nlm(NegLogL, p=st_val2, typsize=st_val2, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001


table_com <- matrix(NA, 4,11)
table_com[1, 1:2] <- Model0$estimate
table_com[1, 3:5] <- 0
table_com[1, 6] <- -Model0$minimum
table_com[1,10] <- -2 * ( -Model0$minimum - 2 )
table_com[1,11] <- -2 * ( -Model0$minimum ) + 2 * log(nrow(DS_C))
table_com[2, 1:3] <- Model1$estimate
table_com[2, 4:5] <- 0
table_com[2, 6] <- -Model1$minimum
table_com[2,7] <- - 2 * (Model1$minimum - Model0$minimum)
table_com[2,8] <- 1
table_com[2,9] <- pchisq(table_com[2,7], df=table_com[2,8], lower.tail = FALSE)
table_com[2,10] <- -2 * ( -Model1$minimum - 3 )
table_com[2,11] <- -2 * ( -Model1$minimum ) + 3 * log(nrow(DS_C))
table_com[3, 1:2] <- Model2$estimate[1:2]
table_com[3, 4:5] <- Model2$estimate[3:4]
table_com[3, 3] <- 0
table_com[3, 6] <- -Model2$minimum
table_com[3,7] <- - 2 * (Model2$minimum - Model0$minimum)
table_com[3,8] <- 2
table_com[3,9] <- pchisq(table_com[3,7], df=table_com[3,8], lower.tail = FALSE)
table_com[3,10] <- -2 * ( -Model2$minimum - 4 )
table_com[3,11] <- -2 * ( -Model2$minimum ) + 4 * log(nrow(DS_C))
table_com[4, 1:5] <- theta_est_BMK2$estimate
table_com[4, 6] <- -theta_est_BMK2$minimum
table_com[4,7] <- - 2 * (theta_est_BMK2$minimum - Model0$minimum)
table_com[4,8] <- 3
table_com[4,9] <- pchisq(table_com[4,7], df=table_com[4,8], lower.tail = FALSE)
table_com[4,10] <- -2 * ( -theta_est_BMK2$minimum - 5 )
table_com[4,11] <- -2 * ( -theta_est_BMK2$minimum ) + 5 * log(nrow(DS_C))
table_com <- round(table_com, 3)

###########################################################################################################

######################################### - Exploratory data analysis - ###################################

###########################################################################################################

################################### - Crude death rates - ####################################

par(mfrow=c(1,1), mai=c(0.8,0.8, 0.1,0.1))

# - Crude rates whole population

exposures <- rep(0, 44)

# Exposures count
for (i in 1:nrow(DS_C)){ #
  for (j in 60:103){
    exposures[j-59] <- ifelse( (j >= floor(DS_C$EntAge[i]) & j < floor(DS_C$EntAge[i]+1) & (DS_C$EntAge[i]+DS_C$TObs[i]) > floor(DS_C$EntAge[i]+1)), (exposures[j-59] + floor(DS_C$EntAge[i]+1) - DS_C$EntAge[i]),
                               ifelse((j >= floor(DS_C$EntAge[i]) & j < floor(DS_C$EntAge[i]+1)), (exposures[j-59] + DS_C$TObs[i]),
                                      ifelse( (j >= floor(DS_C$EntAge[i] + DS_C$TObs[i]) & j < floor(DS_C$EntAge[i] + DS_C$TObs[i] + 1)), (exposures[j-59] + DS_C$EntAge[i]+DS_C$TObs[i] - floor(DS_C$EntAge[i]+DS_C$TObs[i])), 
                                              ifelse((j >= DS_C$EntAge[i] & j < (DS_C$EntAge[i] + DS_C$TObs[i])),exposures[j-59] +1, exposures[j-59] +0 )) ) )
  }
}

# Deaths count
deaths <- rep(0, 44)

for (i in 1:nrow(DS_C)){
  deaths[floor(DS_C$EntAge[i]+DS_C$TObs[i])-59] <- deaths[floor(DS_C$EntAge[i]+DS_C$TObs[i])-59] + DS_C$Status[i]
}

#setwd("H:\\.windows_settings\\Desktop\\Survival Analysis\\Paper_v2\\Figures")
#pdf(file="log_death_rates")
plot(c(60:103), log(deaths/exposures), xlab="Age", ylab="log-death rates", pch=20, cex=1, bty='l') #, main="Log-death rates for the whole population"

sum(exposures)
sum(DS_C$TObs) # sum(DS_C$TObs[i])
sum(deaths)
sum(DS_C$Status)

# - Crude rates low benefit

DS_C_ben_l <- DS_C[DS_C$BL==1,]

exposures_ben_l <- rep(0, 44)

# exposures_ben_l count
for (i in 1:nrow(DS_C_ben_l)){ #
  for (j in 60:103){
    exposures_ben_l[j-59] <- ifelse( (j >= floor(DS_C_ben_l$EntAge[i]) & j < floor(DS_C_ben_l$EntAge[i]+1) & (DS_C_ben_l$EntAge[i]+DS_C_ben_l$TObs[i]) > floor(DS_C_ben_l$EntAge[i]+1)), (exposures_ben_l[j-59] + floor(DS_C_ben_l$EntAge[i]+1) - DS_C_ben_l$EntAge[i]),
                                     ifelse((j >= floor(DS_C_ben_l$EntAge[i]) & j < floor(DS_C_ben_l$EntAge[i]+1)), (exposures_ben_l[j-59] + DS_C_ben_l$TObs[i]),
                                            ifelse( (j >= floor(DS_C_ben_l$EntAge[i] + DS_C_ben_l$TObs[i]) & j < floor(DS_C_ben_l$EntAge[i] + DS_C_ben_l$TObs[i] + 1)), (exposures_ben_l[j-59] + DS_C_ben_l$EntAge[i]+DS_C_ben_l$TObs[i] - floor(DS_C_ben_l$EntAge[i]+DS_C_ben_l$TObs[i])), 
                                                    ifelse((j >= DS_C_ben_l$EntAge[i] & j < (DS_C_ben_l$EntAge[i] + DS_C_ben_l$TObs[i])),exposures_ben_l[j-59] +1, exposures_ben_l[j-59] +0 )) ) )
  }
}

# Deaths count
deaths_ben_l <- rep(0, 44)

for (i in 1:nrow(DS_C_ben_l)){
  deaths_ben_l[floor(DS_C_ben_l$EntAge[i]+DS_C_ben_l$TObs[i])-59] <- deaths_ben_l[floor(DS_C_ben_l$EntAge[i]+DS_C_ben_l$TObs[i])-59] + DS_C_ben_l$Status[i]
}

plot(c(60:103), log(deaths_ben_l/exposures_ben_l), xlab="Age", main="Log-death rates for low benefit population", bty='l')

# - Crude rates high benefit

DS_C_ben_h <- DS_C[DS_C$BH==1,]

exposures_ben_h <- rep(0, 44)

# exposures_ben_h count
for (i in 1:nrow(DS_C_ben_h)){ #
  for (j in 60:103){
    exposures_ben_h[j-59] <- ifelse( (j >= floor(DS_C_ben_h$EntAge[i]) & j < floor(DS_C_ben_h$EntAge[i]+1) & (DS_C_ben_h$EntAge[i]+DS_C_ben_h$TObs[i]) > floor(DS_C_ben_h$EntAge[i]+1)), (exposures_ben_h[j-59] + floor(DS_C_ben_h$EntAge[i]+1) - DS_C_ben_h$EntAge[i]),
                                     ifelse((j >= floor(DS_C_ben_h$EntAge[i]) & j < floor(DS_C_ben_h$EntAge[i]+1)), (exposures_ben_h[j-59] + DS_C_ben_h$TObs[i]),
                                            ifelse( (j >= floor(DS_C_ben_h$EntAge[i] + DS_C_ben_h$TObs[i]) & j < floor(DS_C_ben_h$EntAge[i] + DS_C_ben_h$TObs[i] + 1)), (exposures_ben_h[j-59] + DS_C_ben_h$EntAge[i]+DS_C_ben_h$TObs[i] - floor(DS_C_ben_h$EntAge[i]+DS_C_ben_h$TObs[i])), 
                                                    ifelse((j >= DS_C_ben_h$EntAge[i] & j < (DS_C_ben_h$EntAge[i] + DS_C_ben_h$TObs[i])),exposures_ben_h[j-59] +1, exposures_ben_h[j-59] +0 )) ) )
  }
}

# Deaths count
deaths_ben_h <- rep(0, 44)

for (i in 1:nrow(DS_C_ben_h)){
  deaths_ben_h[floor(DS_C_ben_h$EntAge[i]+DS_C_ben_h$TObs[i])-59] <- deaths_ben_h[floor(DS_C_ben_h$EntAge[i]+DS_C_ben_h$TObs[i])-59] + DS_C_ben_h$Status[i]
}

plot(c(60:103), log(deaths_ben_h/exposures_ben_h), xlab="Age", main="Log-death rates for high benefit population", bty='l')

## - Log-death rates for by benefit level - save as
plot(c(60:103), log(deaths_ben_h/exposures_ben_h), pch=1, cex=0.75, xlab="Age", bty='l', ylab="log-death rates")  # , col="blue", main="Log-death rates by benefit level"
points(c(60:103), log(deaths_ben_l/exposures_ben_l), pch=16, cex=0.75) # , col="red"
legend("topleft", legend=c("Low benefit", "High benefit"), col=c("black", "black"), pch=c(16,1), cex=1, bty='n') #pch=20, 

# - Crude rates geodem 0

DS_C_geo_0 <- DS_C[DS_C$C0==1,]

exposures_geo_0 <- rep(0, 44)

# exposures_geo_0 count
for (i in 1:nrow(DS_C_geo_0)){ #
  for (j in 60:103){
    exposures_geo_0[j-59] <- ifelse( (j >= floor(DS_C_geo_0$EntAge[i]) & j < floor(DS_C_geo_0$EntAge[i]+1) & (DS_C_geo_0$EntAge[i]+DS_C_geo_0$TObs[i]) > floor(DS_C_geo_0$EntAge[i]+1)), (exposures_geo_0[j-59] + floor(DS_C_geo_0$EntAge[i]+1) - DS_C_geo_0$EntAge[i]),
                                     ifelse((j >= floor(DS_C_geo_0$EntAge[i]) & j < floor(DS_C_geo_0$EntAge[i]+1)), (exposures_geo_0[j-59] + DS_C_geo_0$TObs[i]),
                                            ifelse( (j >= floor(DS_C_geo_0$EntAge[i] + DS_C_geo_0$TObs[i]) & j < floor(DS_C_geo_0$EntAge[i] + DS_C_geo_0$TObs[i] + 1)), (exposures_geo_0[j-59] + DS_C_geo_0$EntAge[i]+DS_C_geo_0$TObs[i] - floor(DS_C_geo_0$EntAge[i]+DS_C_geo_0$TObs[i])), 
                                                    ifelse((j >= DS_C_geo_0$EntAge[i] & j < (DS_C_geo_0$EntAge[i] + DS_C_geo_0$TObs[i])),exposures_geo_0[j-59] +1, exposures_geo_0[j-59] +0 )) ) )
  }
}

# Deaths count
deaths_geo_0 <- rep(0, 44)

for (i in 1:nrow(DS_C_geo_0)){
  deaths_geo_0[floor(DS_C_geo_0$EntAge[i]+DS_C_geo_0$TObs[i])-59] <- deaths_geo_0[floor(DS_C_geo_0$EntAge[i]+DS_C_geo_0$TObs[i])-59] + DS_C_geo_0$Status[i]
}

plot(c(60:103), log(deaths_geo_0/exposures_geo_0), xlab="Age", main="log mu_x rates - Geo-dem 0")

# - Crude rates geodem 1

DS_C_geo_1 <- DS_C[DS_C$C1==1,]

exposures_geo_1 <- rep(0, 44)

# exposures_geo_1 count
for (i in 1:nrow(DS_C_geo_1)){ #
  for (j in 60:103){
    exposures_geo_1[j-59] <- ifelse( (j >= floor(DS_C_geo_1$EntAge[i]) & j < floor(DS_C_geo_1$EntAge[i]+1) & (DS_C_geo_1$EntAge[i]+DS_C_geo_1$TObs[i]) > floor(DS_C_geo_1$EntAge[i]+1)), (exposures_geo_1[j-59] + floor(DS_C_geo_1$EntAge[i]+1) - DS_C_geo_1$EntAge[i]),
                                     ifelse((j >= floor(DS_C_geo_1$EntAge[i]) & j < floor(DS_C_geo_1$EntAge[i]+1)), (exposures_geo_1[j-59] + DS_C_geo_1$TObs[i]),
                                            ifelse( (j >= floor(DS_C_geo_1$EntAge[i] + DS_C_geo_1$TObs[i]) & j < floor(DS_C_geo_1$EntAge[i] + DS_C_geo_1$TObs[i] + 1)), (exposures_geo_1[j-59] + DS_C_geo_1$EntAge[i]+DS_C_geo_1$TObs[i] - floor(DS_C_geo_1$EntAge[i]+DS_C_geo_1$TObs[i])), 
                                                    ifelse((j >= DS_C_geo_1$EntAge[i] & j < (DS_C_geo_1$EntAge[i] + DS_C_geo_1$TObs[i])),exposures_geo_1[j-59] +1, exposures_geo_1[j-59] +0 )) ) )
  }
}

# Deaths count
deaths_geo_1 <- rep(0, 44)

for (i in 1:nrow(DS_C_geo_1)){
  deaths_geo_1[floor(DS_C_geo_1$EntAge[i]+DS_C_geo_1$TObs[i])-59] <- deaths_geo_1[floor(DS_C_geo_1$EntAge[i]+DS_C_geo_1$TObs[i])-59] + DS_C_geo_1$Status[i]
}

plot(c(60:103), log(deaths_geo_1/exposures_geo_1), xlab="Age", main="log mu_x rates - Geo-dem 1")

# - Crude rates geodem 2

DS_C_geo_2 <- DS_C[DS_C$C2==1,]

exposures_geo_2 <- rep(0, 44)

# exposures_geo_2 count
for (i in 1:nrow(DS_C_geo_2)){ #
  for (j in 60:103){
    exposures_geo_2[j-59] <- ifelse( (j >= floor(DS_C_geo_2$EntAge[i]) & j < floor(DS_C_geo_2$EntAge[i]+1) & (DS_C_geo_2$EntAge[i]+DS_C_geo_2$TObs[i]) > floor(DS_C_geo_2$EntAge[i]+1)), (exposures_geo_2[j-59] + floor(DS_C_geo_2$EntAge[i]+1) - DS_C_geo_2$EntAge[i]),
                                     ifelse((j >= floor(DS_C_geo_2$EntAge[i]) & j < floor(DS_C_geo_2$EntAge[i]+1)), (exposures_geo_2[j-59] + DS_C_geo_2$TObs[i]),
                                            ifelse( (j >= floor(DS_C_geo_2$EntAge[i] + DS_C_geo_2$TObs[i]) & j < floor(DS_C_geo_2$EntAge[i] + DS_C_geo_2$TObs[i] + 1)), (exposures_geo_2[j-59] + DS_C_geo_2$EntAge[i]+DS_C_geo_2$TObs[i] - floor(DS_C_geo_2$EntAge[i]+DS_C_geo_2$TObs[i])), 
                                                    ifelse((j >= DS_C_geo_2$EntAge[i] & j < (DS_C_geo_2$EntAge[i] + DS_C_geo_2$TObs[i])),exposures_geo_2[j-59] +1, exposures_geo_2[j-59] +0 )) ) )
  }
}

# Deaths count
deaths_geo_2 <- rep(0, 44)

for (i in 1:nrow(DS_C_geo_2)){
  deaths_geo_2[floor(DS_C_geo_2$EntAge[i]+DS_C_geo_2$TObs[i])-59] <- deaths_geo_2[floor(DS_C_geo_2$EntAge[i]+DS_C_geo_2$TObs[i])-59] + DS_C_geo_2$Status[i]
}

plot(c(60:103), log(deaths_geo_2/exposures_geo_2), xlab="Age", main="log mu_x rates - Geo-dem 1")


plot(c(60:103), log(deaths_geo_0/exposures_geo_0), pch=1, cex=0.75, xlab="Age", bty='l', ylab = "log-death rates", ylim=c(-5.5,-0.2)) # , main="Log-death rates by geo-demoraphic level" , col="blue"
points(c(60:103), log(deaths_geo_1/exposures_geo_1), pch=4, cex=0.5) # , col="red"
points(c(60:103), log(deaths_geo_2/exposures_geo_2), pch=16, cex=0.75) # , col="seagreen"
legend("topleft", legend=c("Geo-demographic lev. 0", "Geo-demographic lev. 1", "Geo-demographic lev. 2"), col=c("black", "black", "black"), pch=c(1,4,16), cex=1, bty='n')

######################################## - Kaplan-Meier curve - ################################################

# - Kaplan-Meier Estimator - Full population

sam_size <- nrow(DS_C)

KM_Table_F <- matrix(NA, sam_size, 2)
KM_Table_F[,1] <- DS_C$EntAge+DS_C$TObs    # Age at exit
KM_Table_F[,2] <- DS_C$Status
KM_Table_F <- as.data.frame(KM_Table_F)
colnames(KM_Table_F) <- c("ExitAge", "Status")

KM_Table_F <- KM_Table_F[order(KM_Table_F$ExitAge),c(1,2)]
#KM_Table <- as.matrix(KM_Table)
KM_Table_F$Exp[1] <- sam_size
KM_Table_F$mu[1] <- KM_Table_F$Status[1]/KM_Table_F$Exp[1]
KM_Table_F$one_mu[1] <- 1 - KM_Table_F$mu[1]
KM_Table_F$S_t[1] <- KM_Table_F$one_mu[1]

for (i in 2:sam_size){
  KM_Table_F$Exp[i] <- KM_Table_F$Exp[i-1] - 1
  KM_Table_F$mu[i] <- KM_Table_F$Status[i]/KM_Table_F$Exp[i]
  KM_Table_F$one_mu[i] <- 1 - KM_Table_F$mu[i]
  KM_Table_F$S_t[i] <- prod(KM_Table_F$one_mu[1:i])
  
}

plot(KM_Table_F$ExitAge, KM_Table_F$S_t, type="l", xlab = "Age", ylab = "Kaplan-Meier Survival Curve", bty='l')


# - Kaplan-Meier Estimator - low benefit

sam_size <- nrow(DS_C_ben_l)

KM_Table_ben_l <- matrix(NA, sam_size, 2)
KM_Table_ben_l[,1] <- DS_C_ben_l$EntAge+DS_C_ben_l$TObs    # Age at exit
KM_Table_ben_l[,2] <- DS_C_ben_l$Status
KM_Table_ben_l <- as.data.frame(KM_Table_ben_l)
colnames(KM_Table_ben_l) <- c("ExitAge", "Status")

KM_Table_ben_l <- KM_Table_ben_l[order(KM_Table_ben_l$ExitAge),c(1,2)]
#KM_Table <- as.matrix(KM_Table)
KM_Table_ben_l$Exp[1] <- sam_size
KM_Table_ben_l$mu[1] <- KM_Table_ben_l$Status[1]/KM_Table_ben_l$Exp[1]
KM_Table_ben_l$one_mu[1] <- 1 - KM_Table_ben_l$mu[1]
KM_Table_ben_l$S_t[1] <- KM_Table_ben_l$one_mu[1]

for (i in 2:sam_size){
  KM_Table_ben_l$Exp[i] <- KM_Table_ben_l$Exp[i-1] - 1
  KM_Table_ben_l$mu[i] <- KM_Table_ben_l$Status[i]/KM_Table_ben_l$Exp[i]
  KM_Table_ben_l$one_mu[i] <- 1 - KM_Table_ben_l$mu[i]
  KM_Table_ben_l$S_t[i] <- prod(KM_Table_ben_l$one_mu[1:i])
}

# - Kaplan-Meier Estimator - high benefit

sam_size <- nrow(DS_C_ben_h)

KM_Table_ben_h <- matrix(NA, sam_size, 2)
KM_Table_ben_h[,1] <- DS_C_ben_h$EntAge+DS_C_ben_h$TObs    # Age at exit
KM_Table_ben_h[,2] <- DS_C_ben_h$Status
KM_Table_ben_h <- as.data.frame(KM_Table_ben_h)
colnames(KM_Table_ben_h) <- c("ExitAge", "Status")

KM_Table_ben_h <- KM_Table_ben_h[order(KM_Table_ben_h$ExitAge),c(1,2)]
#KM_Table <- as.matrix(KM_Table)
KM_Table_ben_h$Exp[1] <- sam_size
KM_Table_ben_h$mu[1] <- KM_Table_ben_h$Status[1]/KM_Table_ben_h$Exp[1]
KM_Table_ben_h$one_mu[1] <- 1 - KM_Table_ben_h$mu[1]
KM_Table_ben_h$S_t[1] <- KM_Table_ben_h$one_mu[1]

for (i in 2:sam_size){
  KM_Table_ben_h$Exp[i] <- KM_Table_ben_h$Exp[i-1] - 1
  KM_Table_ben_h$mu[i] <- KM_Table_ben_h$Status[i]/KM_Table_ben_h$Exp[i]
  KM_Table_ben_h$one_mu[i] <- 1 - KM_Table_ben_h$mu[i]
  KM_Table_ben_h$S_t[i] <- prod(KM_Table_ben_h$one_mu[1:i])
}

plot(KM_Table_ben_l$ExitAge, KM_Table_ben_l$S_t, type="l", xlab = "Age", ylab = "Kaplan-Meier Survival Curve", bty='l') # , main="Kaplan-Meier Curve by benefit", col="red"
lines(KM_Table_ben_h$ExitAge, KM_Table_ben_h$S_t, type="l", lty=2) # , col="blue"
legend("topright", legend=c("Low benefit", "High benefit"), col=c("black", "black"), bty='n', lty=c(1,2), cex=0.75)

# - Kaplan-Meier Estimator - geo-demographic profile 0

sam_size <- nrow(DS_C_geo_0)

KM_Table_geo_0 <- matrix(NA, sam_size, 2)
KM_Table_geo_0[,1] <- DS_C_geo_0$EntAge+DS_C_geo_0$TObs    # Age at exit
KM_Table_geo_0[,2] <- DS_C_geo_0$Status
KM_Table_geo_0 <- as.data.frame(KM_Table_geo_0)
colnames(KM_Table_geo_0) <- c("ExitAge", "Status")

KM_Table_geo_0 <- KM_Table_geo_0[order(KM_Table_geo_0$ExitAge),c(1,2)]
#KM_Table <- as.matrix(KM_Table)
KM_Table_geo_0$Exp[1] <- sam_size
KM_Table_geo_0$mu[1] <- KM_Table_geo_0$Status[1]/KM_Table_geo_0$Exp[1]
KM_Table_geo_0$one_mu[1] <- 1 - KM_Table_geo_0$mu[1]
KM_Table_geo_0$S_t[1] <- KM_Table_geo_0$one_mu[1]

for (i in 2:sam_size){
  KM_Table_geo_0$Exp[i] <- KM_Table_geo_0$Exp[i-1] - 1
  KM_Table_geo_0$mu[i] <- KM_Table_geo_0$Status[i]/KM_Table_geo_0$Exp[i]
  KM_Table_geo_0$one_mu[i] <- 1 - KM_Table_geo_0$mu[i]
  KM_Table_geo_0$S_t[i] <- prod(KM_Table_geo_0$one_mu[1:i])
}

# - Kaplan-Meier Estimator - geo-dem 1

sam_size <- nrow(DS_C_geo_1)

KM_Table_geo_1 <- matrix(NA, sam_size, 2)
KM_Table_geo_1[,1] <- DS_C_geo_1$EntAge+DS_C_geo_1$TObs    # Age at exit
KM_Table_geo_1[,2] <- DS_C_geo_1$Status
KM_Table_geo_1 <- as.data.frame(KM_Table_geo_1)
colnames(KM_Table_geo_1) <- c("ExitAge", "Status")

KM_Table_geo_1 <- KM_Table_geo_1[order(KM_Table_geo_1$ExitAge),c(1,2)]
#KM_Table <- as.matrix(KM_Table)
KM_Table_geo_1$Exp[1] <- sam_size
KM_Table_geo_1$mu[1] <- KM_Table_geo_1$Status[1]/KM_Table_geo_1$Exp[1]
KM_Table_geo_1$one_mu[1] <- 1 - KM_Table_geo_1$mu[1]
KM_Table_geo_1$S_t[1] <- KM_Table_geo_1$one_mu[1]

for (i in 2:sam_size){
  KM_Table_geo_1$Exp[i] <- KM_Table_geo_1$Exp[i-1] - 1
  KM_Table_geo_1$mu[i] <- KM_Table_geo_1$Status[i]/KM_Table_geo_1$Exp[i]
  KM_Table_geo_1$one_mu[i] <- 1 - KM_Table_geo_1$mu[i]
  KM_Table_geo_1$S_t[i] <- prod(KM_Table_geo_1$one_mu[1:i])
}

# - Kaplan-Meier Estimator - geo-dem 2

sam_size <- nrow(DS_C_geo_2)

KM_Table_geo_2 <- matrix(NA, sam_size, 2)
KM_Table_geo_2[,1] <- DS_C_geo_2$EntAge+DS_C_geo_2$TObs    # Age at exit
KM_Table_geo_2[,2] <- DS_C_geo_2$Status
KM_Table_geo_2 <- as.data.frame(KM_Table_geo_2)
colnames(KM_Table_geo_2) <- c("ExitAge", "Status")

KM_Table_geo_2 <- KM_Table_geo_2[order(KM_Table_geo_2$ExitAge),c(1,2)]
#KM_Table <- as.matrix(KM_Table)
KM_Table_geo_2$Exp[1] <- sam_size
KM_Table_geo_2$mu[1] <- KM_Table_geo_2$Status[1]/KM_Table_geo_2$Exp[1]
KM_Table_geo_2$one_mu[1] <- 1 - KM_Table_geo_2$mu[1]
KM_Table_geo_2$S_t[1] <- KM_Table_geo_2$one_mu[1]

for (i in 2:sam_size){
  KM_Table_geo_2$Exp[i] <- KM_Table_geo_2$Exp[i-1] - 1
  KM_Table_geo_2$mu[i] <- KM_Table_geo_2$Status[i]/KM_Table_geo_2$Exp[i]
  KM_Table_geo_2$one_mu[i] <- 1 - KM_Table_geo_2$mu[i]
  KM_Table_geo_2$S_t[i] <- prod(KM_Table_geo_2$one_mu[1:i])
}

plot(KM_Table_geo_0$ExitAge, KM_Table_geo_0$S_t, type="l", xlab = "Age", ylab = "Kaplan-Meier Survival Curve", bty='l') #, col="blue", main = "Kaplan-Meier Survival Curve by geo-demographic profile"
lines(KM_Table_geo_1$ExitAge, KM_Table_geo_1$S_t, type="l", lty=2) # , col="red"
lines(KM_Table_geo_2$ExitAge, KM_Table_geo_2$S_t, type="l", lty=3) # , col="seagreen"
legend("topright", legend=c("Geo-demographic 0", "Geo-demographic 1", "Geo-demographic 2"), col=c("black", "black", "black"), bty='n', lty=c(1,2,3), cex=0.75)

################## - Exposures / Deaths table - ####################################

exp_deaths <- matrix(NA, 45, 12)
#exp_deaths[,1] <- c(60:103)
exp_deaths[1:44,1] <- exposures
exp_deaths[1:44,2] <- deaths
exp_deaths[1:44,3] <- exposures_ben_l
exp_deaths[1:44,4] <- deaths_ben_l
exp_deaths[1:44,5] <- exposures_ben_h
exp_deaths[1:44,6] <- deaths_ben_h
exp_deaths[1:44,7] <- exposures_geo_0
exp_deaths[1:44,8] <- deaths_geo_0
exp_deaths[1:44,9] <- exposures_geo_1
exp_deaths[1:44,10] <- deaths_geo_1
exp_deaths[1:44,11] <- exposures_geo_2
exp_deaths[1:44,12] <- deaths_geo_2
exp_deaths[45,1:12] <- colSums(exp_deaths[1:44,1:12])
rownames(exp_deaths) <- c(60:104)

xtable(exp_deaths, digits=c(0,2,0,2,0,2,0,2,0,2,0,2,0))

##################################################################

# - Fitting

##################################################################

par_est_random_split <- matrix(NA, 10, 10)
eigenv_random_split <- matrix(NA, 10, 10)

# for (k2 in 1:10){

# row reshuffling
DS_C <- DS_C [sample(nrow(DS_C)),]

#Define two datasets split_perc% - (1-split_perc)% missingness DS1 and DS2
##splitting percentage
split_perc <- 0.5

DS1 <- DS_C[1:round(nrow(SurvInMW) * split_perc),]
DS2 <- DS_C[(round(nrow(SurvInMW)* split_perc)+1):nrow(DS_C),]

# - psi matrix creation function
psi_f = function(n){
  psi_matrix <- matrix(0, nrow=(n-1), ncol=n)
  for (i in 1:(n-1)){
    psi_matrix[i, c(1:i)] <- sqrt(i/(i+1)) / i
    psi_matrix[i,(i+1)] <- -sqrt(i/(i+1))
  }
  return(psi_matrix)
}

n <- 6
psi <- psi_f(n)    ## t(psi) %*% psi ## psi %*% t(psi)

# Negative log-likelihood function
NegLogL_ILR = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  ben = DS1$BH
  
  benL_ind = DS1$BL
  benH_ind = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  geo0_ind = DS2$C0
  geo1_ind = DS2$C1
  geo2_ind = DS2$C2
  
  a   = vdParameters[1] #alpha
  b   = vdParameters[2] #beta
  c   = vdParameters[3] #delta1
  de1 = vdParameters[4] #delta2
  de2 = vdParameters[5]
  
  pi = matrix(NA, nrow=1, ncol=5)
  pi[1] = vdParameters[6]
  pi[2] = vdParameters[7]
  pi[3] = vdParameters[8]
  pi[4] = vdParameters[9]
  pi[5] = vdParameters[10]
  
  pi_psi <- pi %*% psi
  s_pi = sum(exp(pi_psi))
  
  #Weighted log-likelihoods
  
  #logL_1 <- log( ((benL_ind * (exp(pi_psi[1])/s_pi) + benH_ind * (exp(pi_psi[2])/s_pi))                                                                                                       * (exp( -exp(a + b * x1 + c * ben )      * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c * ben      ))) + 
  #               (benL_ind * (exp(pi_psi[3])/s_pi) + benH_ind * (exp(pi_psi[4])/s_pi))                                                                                                       * (exp( -exp(a + b * x1 + c * ben + de1) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c * ben + de1))) + 
  #              (benL_ind * (exp(pi_psi[5])/s_pi) + benH_ind * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi) - (exp(pi_psi[5])/s_pi))) * (exp( -exp(a + b * x1 + c * ben + de2) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c * ben + de2))) ) /
  #             (benL_ind * ((exp(pi_psi[1])/s_pi) + (exp(pi_psi[3])/s_pi) + (exp(pi_psi[5])/s_pi)) + benH_ind * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[5])/s_pi))) ) +  
  #  benL_ind * log((exp(pi_psi[1])/s_pi) + (exp(pi_psi[3])/s_pi) + (exp(pi_psi[5])/s_pi)) + benH_ind * log(1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[5])/s_pi)) 
  
  logL_1 <- log( ((benL_ind * (exp(pi_psi[1])/s_pi) + benH_ind * (exp(pi_psi[2])/s_pi))                                                                                                       * (exp( -exp(a + b * x1 + c * ben )      * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c * ben      ))) + 
                    (benL_ind * (exp(pi_psi[3])/s_pi) + benH_ind * (exp(pi_psi[4])/s_pi))                                                                                                       * (exp( -exp(a + b * x1 + c * ben + de1) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c * ben + de1))) + 
                    (benL_ind * (exp(pi_psi[5])/s_pi) + benH_ind * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi) - (exp(pi_psi[5])/s_pi))) * (exp( -exp(a + b * x1 + c * ben + de2) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c * ben + de2))) ))
  
  #logL_2 <- log( ((geo0_ind * (exp(pi_psi[1])/s_pi) + geo1_ind * (exp(pi_psi[3])/s_pi) + geo2_ind * (exp(pi_psi[5])/s_pi))                                                                                                       * ( exp( -exp(a + b * x2     + de1 * gde1 + de2 * gde2) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2)     + de1 * gde1 + de2 * gde2))) + 
  #               (geo0_ind * (exp(pi_psi[2])/s_pi) + geo1_ind * (exp(pi_psi[4])/s_pi) + geo2_ind * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi) - (exp(pi_psi[5])/s_pi))) * ( exp( -exp(a + b * x2 + c + de1 * gde1 + de2 * gde2) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2) + c + de1 * gde1 + de2 * gde2)))) / 
  #              (geo0_ind * ((exp(pi_psi[1])/s_pi) + (exp(pi_psi[2])/s_pi)) + geo1_ind * ((exp(pi_psi[3])/s_pi) + (exp(pi_psi[4])/s_pi)) + geo2_ind * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi))) ) + 
  #  geo0_ind * log((exp(pi_psi[1])/s_pi) + (exp(pi_psi[2])/s_pi)) + geo1_ind * log((exp(pi_psi[3])/s_pi) + (exp(pi_psi[4])/s_pi)) + geo2_ind * log(1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi))
  
  logL_2 <- log( ((geo0_ind * (exp(pi_psi[1])/s_pi) + geo1_ind * (exp(pi_psi[3])/s_pi) + geo2_ind * (exp(pi_psi[5])/s_pi))                                                                                                       * ( exp( -exp(a + b * x2     + de1 * gde1 + de2 * gde2) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2)     + de1 * gde1 + de2 * gde2))) + 
                    (geo0_ind * (exp(pi_psi[2])/s_pi) + geo1_ind * (exp(pi_psi[4])/s_pi) + geo2_ind * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi) - (exp(pi_psi[5])/s_pi))) * ( exp( -exp(a + b * x2 + c + de1 * gde1 + de2 * gde2) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2) + c + de1 * gde1 + de2 * gde2)))))
  
  return(-(sum(logL_1) + sum(logL_2)))
}


## - 5.3 - Results

### - 5.3.1 - Starting values

### - Marginals and contingency table
BL_prob <- nrow(DS1[DS1$BH==0,])/nrow(DS1)
BH_prob <- 1 - BL_prob
G0_prob <- nrow(DS2[DS2$GeoGroup==0,])/nrow(DS2)
G1_prob <- nrow(DS2[DS2$GeoGroup==1,])/nrow(DS2)
G2_prob <- nrow(DS2[DS2$GeoGroup==2,])/nrow(DS2)

# - Independency case
ind_stval  <- c(BL_prob*G0_prob, BH_prob*G0_prob, BL_prob*G1_prob, BH_prob*G1_prob, BL_prob*G2_prob, BH_prob*G2_prob)
pse_tr_val <- c(nrow(DS_C[(DS_C$GeoGroup==0 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==0 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==1),])/nrow(DS_C))


#Gaussian copula contingency table for the generation of further starting values
library(mvtnorm)

ni <- c(sum(1-DS1$BH)/nrow(DS1), sum(DS1$BH)/nrow(DS1))
xi <- c(sum(DS2$C0)/nrow(DS2), sum(DS2$C1)/nrow(DS2), sum(DS2$C2)/nrow(DS2))
pi <- qnorm(ni[1], 0, 1)
lambda <- c(qnorm(xi[1], 0, 1), qnorm((xi[2]+xi[1]), 0, 1))

#Contingency table generated by the gaussian Copula
CT_gen025 <- matrix(NA, nrow=2, ncol=3)
CT_gen025[1,1] <- pmvnorm(lower=c(-Inf, -Inf), upper=c(pi, lambda[1]), mean=rep(0, 2), corr=matrix(c(1,0.25,0.25,1), ncol=2, byrow = TRUE))
CT_gen025[2,1] <- pmvnorm(lower=c(pi, -Inf), upper=c(Inf, lambda[1]), mean=rep(0, 2), corr=matrix(c(1,0.25,0.25,1), ncol=2, byrow = TRUE))
CT_gen025[1,2] <- pmvnorm(lower=c(-Inf, lambda[1]), upper=c(pi, lambda[2]), mean=rep(0, 2), corr=matrix(c(1,0.25,0.25,1), ncol=2, byrow = TRUE))
CT_gen025[2,2] <- pmvnorm(lower=c(pi, lambda[1]), upper=c(Inf, lambda[2]), mean=rep(0, 2), corr=matrix(c(1,0.25,0.25,1), ncol=2, byrow = TRUE))
CT_gen025[1,3] <- pmvnorm(lower=c(-Inf, lambda[2]), upper=c(pi, Inf), mean=rep(0, 2), corr=matrix(c(1,0.25,0.25,1), ncol=2, byrow = TRUE))
CT_gen025[2,3] <- pmvnorm(lower=c(pi, lambda[2]), upper=c(Inf, Inf), mean=rep(0, 2), corr=matrix(c(1,0.25,0.25,1), ncol=2, byrow = TRUE))

CT_gen050 <- matrix(NA, nrow=2, ncol=3)
CT_gen050[1,1] <- pmvnorm(lower=c(-Inf, -Inf), upper=c(pi, lambda[1]), mean=rep(0, 2), corr=matrix(c(1,0.50,0.50,1), ncol=2, byrow = TRUE))
CT_gen050[2,1] <- pmvnorm(lower=c(pi, -Inf), upper=c(Inf, lambda[1]), mean=rep(0, 2), corr=matrix(c(1,0.50,0.50,1), ncol=2, byrow = TRUE))
CT_gen050[1,2] <- pmvnorm(lower=c(-Inf, lambda[1]), upper=c(pi, lambda[2]), mean=rep(0, 2), corr=matrix(c(1,0.50,0.50,1), ncol=2, byrow = TRUE))
CT_gen050[2,2] <- pmvnorm(lower=c(pi, lambda[1]), upper=c(Inf, lambda[2]), mean=rep(0, 2), corr=matrix(c(1,0.50,0.50,1), ncol=2, byrow = TRUE))
CT_gen050[1,3] <- pmvnorm(lower=c(-Inf, lambda[2]), upper=c(pi, Inf), mean=rep(0, 2), corr=matrix(c(1,0.50,0.50,1), ncol=2, byrow = TRUE))
CT_gen050[2,3] <- pmvnorm(lower=c(pi, lambda[2]), upper=c(Inf, Inf), mean=rep(0, 2), corr=matrix(c(1,0.50,0.50,1), ncol=2, byrow = TRUE))

CT_gen075 <- matrix(NA, nrow=2, ncol=3)
CT_gen075[1,1] <- pmvnorm(lower=c(-Inf, -Inf), upper=c(pi, lambda[1]), mean=rep(0, 2), corr=matrix(c(1,0.75,0.75,1), ncol=2, byrow = TRUE))
CT_gen075[2,1] <- pmvnorm(lower=c(pi, -Inf), upper=c(Inf, lambda[1]), mean=rep(0, 2), corr=matrix(c(1,0.75,0.75,1), ncol=2, byrow = TRUE))
CT_gen075[1,2] <- pmvnorm(lower=c(-Inf, lambda[1]), upper=c(pi, lambda[2]), mean=rep(0, 2), corr=matrix(c(1,0.75,0.75,1), ncol=2, byrow = TRUE))
CT_gen075[2,2] <- pmvnorm(lower=c(pi, lambda[1]), upper=c(Inf, lambda[2]), mean=rep(0, 2), corr=matrix(c(1,0.75,0.75,1), ncol=2, byrow = TRUE))
CT_gen075[1,3] <- pmvnorm(lower=c(-Inf, lambda[2]), upper=c(pi, Inf), mean=rep(0, 2), corr=matrix(c(1,0.75,0.75,1), ncol=2, byrow = TRUE))
CT_gen075[2,3] <- pmvnorm(lower=c(pi, lambda[2]), upper=c(Inf, Inf), mean=rep(0, 2), corr=matrix(c(1,0.75,0.75,1), ncol=2, byrow = TRUE))

### - St. values
st_val_matrix <- matrix(NA, nrow=10, ncol=11)
st_val_matrix[1,] <- c(-10, 0.1, -0.25, -0.28, -0.5, nrow(DS_C[(DS_C$C0==1 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$C0==1 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==1),])/nrow(DS_C))
st_val_matrix[2,] <- c(-10, 0.1, -0.25, -0.28, -0.5, BL_prob*G0_prob, BH_prob*G0_prob, BL_prob*G1_prob, BH_prob*G1_prob, BL_prob*G2_prob, BH_prob*G2_prob)
st_val_matrix[3,] <- c(-10, 0.1, -0.25, -0.28, -0.5, CT_gen025[1,1], CT_gen025[2,1], CT_gen025[1,2], CT_gen025[2,2], CT_gen025[1,3], CT_gen025[2,3])
st_val_matrix[4,] <- c(-10, 0.1, -0.25, -0.28, -0.5, CT_gen050[1,1], CT_gen050[2,1], CT_gen050[1,2], CT_gen025[2,2], CT_gen050[1,3], CT_gen050[2,3])
st_val_matrix[5,] <- c(-10, 0.1, -0.25, -0.28, -0.5, CT_gen075[1,1], CT_gen075[2,1], CT_gen075[1,2], CT_gen075[2,2], CT_gen025[1,3], CT_gen075[2,3])
st_val_matrix[6,] <- c(-10, 0.1, -0, -0, -0, nrow(DS_C[(DS_C$C0==1 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$C0==1 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==1),])/nrow(DS_C))
st_val_matrix[7,] <- c(-10, 0.1, -0, -0, -0, BL_prob*G0_prob, BH_prob*G0_prob, BL_prob*G1_prob, BH_prob*G1_prob, BL_prob*G2_prob, BH_prob*G2_prob)
st_val_matrix[8,] <- c(-10, 0.1, -0, -0, -0, CT_gen025[1,1], CT_gen025[2,1], CT_gen025[1,2], CT_gen025[2,2], CT_gen025[1,3], CT_gen025[2,3])
st_val_matrix[9,] <- c(-10, 0.1, -0, -0, -0, CT_gen050[1,1], CT_gen050[2,1], CT_gen050[1,2], CT_gen025[2,2], CT_gen050[1,3], CT_gen050[2,3])
st_val_matrix[10,] <-c(-10, 0.1, -0, -0, -0, CT_gen075[1,1], CT_gen075[2,1], CT_gen075[1,2], CT_gen075[2,2], CT_gen025[1,3], CT_gen075[2,3])


### - 5.3.2 - Fitting the model

#### - Storage values
results <- matrix(NA, nrow=40, ncol=12)
colnames(results) <- c("","alpha", "beta", "gamma", "delta1", "delta2", "z1", "z2", "z3", "z4", "z5", "log-lik")

# nlm
for (i in 1:1){
  
  sv_pi <- (log(st_val_matrix[i,6:11])) %*% t(psi) 
  
  st_val <- matrix(NA, 1, 10)
  st_val[1:5] <- st_val_matrix[i, 1:5] #init_val[1:5]
  st_val[6:10] <- sv_pi
  
  # - Step 4 - Eastimate tau and zeta
  theta_est <- nlm(NegLogL_ILR, p=st_val, typsize=st_val, hessian=T, iterlim=10000, gradtol = 0.00000001) #,gradtol = 0.0000001, steptol = 0.00000001
  #eigen(theta_est$hessian) # solve(theta_est$hessian)
  
  # - Step 5 - Recalculate zeta
  pi_psi <- theta_est$estimate[6:10] %*% psi
  zeta <- exp(pi_psi)/sum(exp(pi_psi))
  
  spect_decomp <- eigen(theta_est$hessian)
  hess_mat <- -theta_est$hessian
  
  eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 10)
  
  #store results
  results[(1+4*(i-1)),2:6] <- theta_est$estimate[1:5]
  results[(1+4*(i-1)),7:11] <- zeta[1:5]
  results[(1+4*(i-1)),12] <- -theta_est$minimum
  results[(3+4*(i-1)),12] <- theta_est$code
  results[(2+4*(i-1)),2:11] <- st_val_matrix[i,1:10]
  results[(3+4*(i-1)),2:11] <- theta_est$gradient
  results[(4*i), 2:11] <- -eigenval
}

par_est_random_split[k2,1:5] <- theta_est$estimate[1:5]
par_est_random_split[k2,6:10] <- zeta[1:5]
eigenv_random_split[k2,1:10] <- -eigenval

# }

xtable(par_est_random_split)
xtable(eigenv_random_split)

############################################################################

# - Reduction points of support

############################################################################

############################# - 2 points of support - ######################

psi1 <- psi_f(4)
psi2 <- psi_f(6)

# Negative log-likelihood function - Two points of support
NegLogL_ILR_Red2 = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  ben = DS1$BH
  
  benL_ind = DS1$BL
  benH_ind = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  geo0_ind = DS2$C0
  geo1_ind = DS2$C1
  geo2_ind = DS2$C2
  
  a   = vdParameters[1] #alpha
  b   = vdParameters[2] #beta
  c   = vdParameters[3] #delta1
  
  #change pi psi
  pi1 = matrix(NA, nrow=1, ncol=3)
  pi1[1] = vdParameters[4]
  pi1[2] = vdParameters[5]
  pi1[3] = vdParameters[6]
  
  pi2 = matrix(NA, nrow=1, ncol=5)
  pi2[1] = vdParameters[7]
  pi2[2] = vdParameters[8]
  pi2[3] = vdParameters[9]
  pi2[4] = vdParameters[10]
  pi2[5] = vdParameters[11]
  
  pi1_psi1 <- pi1 %*% psi1
  s_pi1 = sum(exp(pi1_psi1))
  
  pi2_psi2 <- pi2 %*% psi2
  s_pi2 = sum(exp(pi2_psi2))
  
  #Weighted log-likelihoods
  
  logL_1 <- log( ((benL_ind * (exp(pi1_psi1[1])/s_pi1) + benH_ind * (exp(pi1_psi1[2])/s_pi1))                                                             * (exp( -exp(a + b * x1    ) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1)     ))) +                                                 
                  (benL_ind * (exp(pi1_psi1[3])/s_pi1) + benH_ind * (1 - (exp(pi1_psi1[1])/s_pi1) - (exp(pi1_psi1[2])/s_pi1) - (exp(pi1_psi1[3])/s_pi1))) * (exp( -exp(a + b * x1 + c) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1) + c )))))
  
  logL_2 <- log( ((geo0_ind * (exp(pi2_psi2[1])/s_pi2) + geo1_ind * (exp(pi2_psi2[2])/s_pi2) + geo2_ind * (exp(pi2_psi2[3])/s_pi2))                                                                                                                   * ( exp( -exp(a + b * x2    ) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2)    ))) + 
                  (geo0_ind * (exp(pi2_psi2[4])/s_pi2) + geo1_ind * (exp(pi2_psi2[5])/s_pi2) + geo2_ind * (1 - (exp(pi2_psi2[1])/s_pi2) - (exp(pi2_psi2[2])/s_pi2) - (exp(pi2_psi2[3])/s_pi2) - (exp(pi2_psi2[4])/s_pi2) - (exp(pi2_psi2[5])/s_pi2))) * ( exp( -exp(a + b * x2 + c) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2) + c)))))
  
  return(-(sum(logL_1) + sum(logL_2)))
}

# - Results

st_val_vector <- c(-10, 0.1, -0.2, 0.6, 0.05, 0.15, 0.2, 0.2, 0.25, 0.05, 0.05, 0.25, 0.2)

sv_pi1 <- (log(st_val_vector[4:7])) %*% t(psi1) 
sv_pi2 <- (log(st_val_vector[8:13])) %*% t(psi2) 

st_val <- matrix(NA, 1, 11)
st_val[1:3] <- st_val_vector[1:3] #init_val[1:5]
st_val[4:6] <- sv_pi1
st_val[7:11] <- sv_pi2

# - Step 4 - Eastimate tau and zeta
theta_est_Red2 <- nlm(NegLogL_ILR_Red2, p=st_val, typsize=st_val, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001
#eigen(theta_est$hessian) # solve(theta_est$hessian)

# - Step 5 - Recalculate zeta
pi1_psi1 <- theta_est_Red2$estimate[4:6] %*% psi1
zeta1 <- exp(pi1_psi1)/sum(exp(pi1_psi1))
pi2_psi2 <- theta_est_Red2$estimate[7:11] %*% psi2
zeta2 <- exp(pi2_psi2)/sum(exp(pi2_psi2))

spect_decomp <- eigen(theta_est_Red2$hessian)
hess_mat <- -theta_est_Red2$hessian

eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 11)

# Negative log-likelihood function - Two points of support - Structural zeros
psi1 <- psi_f(3)
psi2 <- psi_f(4)

NegLogL_ILR_Red20 = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  ben = DS1$BH
  
  benL_ind = DS1$BL
  benH_ind = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  geo0_ind = DS2$C0
  geo1_ind = DS2$C1
  geo2_ind = DS2$C2
  
  a   = vdParameters[1] #alpha
  b   = vdParameters[2] #beta
  c   = vdParameters[3] #delta1
  
  #change pi psi
  pi1 = matrix(NA, nrow=1, ncol=2)
  pi1[1] = vdParameters[4]
  pi1[2] = vdParameters[5]
  
  pi2 = matrix(NA, nrow=1, ncol=3)
  pi2[1] = vdParameters[6]
  pi2[2] = vdParameters[7]
  pi2[3] = vdParameters[8]
  
  pi1_psi1 <- pi1 %*% psi1
  s_pi1 = sum(exp(pi1_psi1))
  
  pi2_psi2 <- pi2 %*% psi2
  s_pi2 = sum(exp(pi2_psi2))
  
  #Weighted log-likelihoods
  
  logL_1 <- log( ((benL_ind * (exp(pi1_psi1[1])/s_pi1)                                      )                                  * (exp( -exp(a + b * x1    ) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1)     ))) +                                                 
                    (benL_ind * (exp(pi1_psi1[2])/s_pi1) + benH_ind * (1 - (exp(pi1_psi1[1])/s_pi1) - (exp(pi1_psi1[2])/s_pi1))) * (exp( -exp(a + b * x1 + c) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1) + c )))))
  
  logL_2 <- log( ((geo0_ind * (exp(pi2_psi2[1])/s_pi2) + geo1_ind * (exp(pi2_psi2[2])/s_pi2)              )                                                                                     * ( exp( -exp(a + b * x2    ) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2)    ))) + 
                    (                                      geo1_ind * (exp(pi2_psi2[3])/s_pi2) + geo2_ind * (1 - (exp(pi2_psi2[1])/s_pi2) - (exp(pi2_psi2[2])/s_pi2) - (exp(pi2_psi2[3])/s_pi2))) * ( exp( -exp(a + b * x2 + c) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2) + c)))))
  
  return(-(sum(logL_1) + sum(logL_2)))
}

st_val_vector0 <- c(-10, 0.1, -0.35, 0.5, 0.2, 0.3, 0.25, 0.25, 0.25, 0.25)

sv_pi1 <- (log(st_val_vector0[4:6])) %*% t(psi1) 
sv_pi2 <- (log(st_val_vector0[7:10])) %*% t(psi2) 

st_val <- matrix(NA, 1, 8)
st_val[1:3] <- st_val_vector0[1:3] #init_val[1:5]
st_val[4:5] <- sv_pi1
st_val[6:8] <- sv_pi2

# - Step 4 - Eastimate tau and zeta
theta_est_Red20 <- nlm(NegLogL_ILR_Red20, p=st_val, typsize=st_val, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001

# - Step 5 - Recalculate zeta
pi1_psi1 <- theta_est_Red20$estimate[4:5] %*% psi1
zeta1 <- exp(pi1_psi1)/sum(exp(pi1_psi1))
pi2_psi2 <- theta_est_Red20$estimate[6:8] %*% psi2
zeta2 <- exp(pi2_psi2)/sum(exp(pi2_psi2))

spect_decomp <- eigen(theta_est_Red20$hessian)

eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 8)

k20_est <- matrix(NA, 3, 9)
k20_est[1,1:3] <- theta_est_Red20$estimate[1:3]
k20_est[1,4:5] <- zeta1[1:2]
k20_est[1,6:8] <- zeta2[1:3]
k20_est[1,9] <- -theta_est_Red20$minimum
k20_est[2,1:8] <- theta_est_Red20$gradient
k20_est[3,1:8] <- eigenval
xtable(k20_est)

############################# - 3 points of support - ######################

psi1 <- psi_f(6)
psi2 <- psi_f(9)

# Negative log-likelihood function - Two points of support
NegLogL_ILR_Red3 = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  ben = DS1$BH
  
  benL_ind = DS1$BL
  benH_ind = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  geo0_ind = DS2$C0
  geo1_ind = DS2$C1
  geo2_ind = DS2$C2
  
  a  = vdParameters[1] #alpha
  b  = vdParameters[2] #beta
  c1 = vdParameters[3] #delta1
  c2 = vdParameters[4]
  
  #change pi psi
  pi1 = matrix(NA, nrow=1, ncol=5)
  pi1[1] = vdParameters[5]
  pi1[2] = vdParameters[6]
  pi1[3] = vdParameters[7]
  pi1[4] = vdParameters[8]
  pi1[5] = vdParameters[9]
  
  pi2 = matrix(NA, nrow=1, ncol=8)
  pi2[1] = vdParameters[10]
  pi2[2] = vdParameters[11]
  pi2[3] = vdParameters[12]
  pi2[4] = vdParameters[13]
  pi2[5] = vdParameters[14]
  pi2[6] = vdParameters[15]
  pi2[7] = vdParameters[16]
  pi2[8] = vdParameters[17]
  
  pi1_psi1 <- pi1 %*% psi1
  s_pi1 = sum(exp(pi1_psi1))
  
  pi2_psi2 <- pi2 %*% psi2
  s_pi2 = sum(exp(pi2_psi2))
  
  #Weighted log-likelihoods
  
  logL_1 <-  log( (benL_ind * (exp(pi1_psi1[1])/s_pi1) + benH_ind * (exp(pi1_psi1[2])/s_pi1))                                                             * (exp( -exp(a + b * x1     ) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1)     ))) +  
                    (benL_ind * (exp(pi1_psi1[3])/s_pi1) + benH_ind * (exp(pi1_psi1[4])/s_pi1))                                                             * (exp( -exp(a + b * x1 + c1) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1) + c1))) +   
                    (benL_ind * (exp(pi1_psi1[5])/s_pi1) + benH_ind * (1 - (exp(pi1_psi1[1])/s_pi1) - (exp(pi1_psi1[2])/s_pi1) - 
                                                                         (exp(pi1_psi1[3])/s_pi1) - (exp(pi1_psi1[4])/s_pi1) - (exp(pi1_psi1[5])/s_pi1))) * (exp( -exp(a + b * x1 + c2) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1) + c2)))) 
  
  logL_2 <-  log( (geo0_ind * (exp(pi2_psi2[1])/s_pi2) + geo1_ind * (exp(pi2_psi2[2])/s_pi2) + geo2_ind * (exp(pi2_psi2[3])/s_pi2)) * (exp( -exp(a + b * x2     ) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2)     ))) + 
                    (geo0_ind * (exp(pi2_psi2[4])/s_pi2) + geo1_ind * (exp(pi2_psi2[5])/s_pi2) + geo2_ind * (exp(pi2_psi2[6])/s_pi2)) * (exp( -exp(a + b * x2 + c1) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2) + c1))) +
                    (geo0_ind * (exp(pi2_psi2[7])/s_pi2) + geo1_ind * (exp(pi2_psi2[8])/s_pi2) + geo2_ind * (1 - (exp(pi2_psi2[1])/s_pi2) - (exp(pi2_psi2[2])/s_pi2) - (exp(pi2_psi2[3])/s_pi2) - (exp(pi2_psi2[4])/s_pi2) - (exp(pi2_psi2[5])/s_pi2) - (exp(pi2_psi2[6])/s_pi2) - (exp(pi2_psi2[7])/s_pi2) - (exp(pi2_psi2[8])/s_pi2)
                    ))                                                                                                              * (exp( -exp(a + b * x2 + c2) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2) + c2))))
  
  return(-(sum(logL_1) + sum(logL_2)))
}

# - Results

st_val_vector <- c(-10, 0.1, -0.2, -0.4, 0.4, 0.05, 0.15, 0.07, 0.15, 0.18, 0.15, 0.15, 0.05, 0.05, 0.2, 0.05, 0.05, 0.15, 0.15)

sv_pi1 <- (log(st_val_vector[5:10])) %*% t(psi1) 
sv_pi2 <- (log(st_val_vector[11:19])) %*% t(psi2) 

st_val <- matrix(NA, 1, 17)
st_val[1:4] <- st_val_vector[1:4] #init_val[1:5]
st_val[5:9] <- sv_pi1
st_val[10:17] <- sv_pi2

# - Step 4 - Eastimate tau and zeta
theta_est_Red3 <- nlm(NegLogL_ILR_Red3, p=st_val, typsize=st_val, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001
#eigen(theta_est$hessian) # solve(theta_est$hessian)

# - Step 5 - Recalculate zeta
pi1_psi1 <- theta_est_Red3$estimate[5:9] %*% psi1
zeta1 <- exp(pi1_psi1)/sum(exp(pi1_psi1))
pi2_psi2 <- theta_est_Red3$estimate[10:17] %*% psi2
zeta2 <- exp(pi2_psi2)/sum(exp(pi2_psi2))

spect_decomp <- eigen(theta_est_Red3$hessian)
eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 17)

## - Structural zero
# Negative log-likelihood function - Three points of support

psi1 <- psi_f(3)
psi2 <- psi_f(3)

# Negative log-likelihood function - Two points of support
NegLogL_ILR_Red30 = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  ben = DS1$BH
  
  benL_ind = DS1$BL
  benH_ind = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  geo0_ind = DS2$C0
  geo1_ind = DS2$C1
  geo2_ind = DS2$C2
  
  a  = vdParameters[1] #alpha
  b  = vdParameters[2] #beta
  c1 = vdParameters[3] #delta1
  c2 = vdParameters[4]
  
  #change pi psi
  pi1 = matrix(NA, nrow=1, ncol=2)
  pi1[1] = vdParameters[5]
  pi1[2] = vdParameters[6]
  #pi1[3] = vdParameters[7]
  
  pi2 = matrix(NA, nrow=1, ncol=2)
  pi2[1] = vdParameters[7]
  pi2[2] = vdParameters[8]
  
  pi1_psi1 <- pi1 %*% psi1
  s_pi1 = sum(exp(pi1_psi1))
  
  pi2_psi2 <- pi2 %*% psi2
  s_pi2 = sum(exp(pi2_psi2))
  
  #Weighted log-likelihoods
  
  logL_1 <-  log( (benL_ind * (exp(pi1_psi1[1])/s_pi1)                                      )       * (exp( -exp(a + b * x1     ) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1)     ))) +  
                    #(benL_ind * (exp(pi1_psi1[2])/s_pi1)                                      )       * (exp( -exp(a + b * x1 + c1) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1) + c1))) +   
                    (benL_ind * (exp(pi1_psi1[2])/s_pi1) + benH_ind * (1 - (exp(pi1_psi1[1])/s_pi1) - (exp(pi1_psi1[2])/s_pi1) 
                    )) * (exp( -exp(a + b * x1 + c2) * (exp(b * t1) - 1)/b) * exp( d1 * (a + b * (x1 + t1) + c2)))) 
  
  logL_2 <-  log( (geo0_ind * (exp(pi2_psi2[1])/s_pi2)                                                               ) * (exp( -exp(a + b * x2     ) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2)     ))) + 
                    (                                      geo1_ind * (exp(pi2_psi2[2])/s_pi2)                         ) * (exp( -exp(a + b * x2 + c1) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2) + c1))) +
                    (                                                                            geo2_ind * (1 - (exp(pi2_psi2[1])/s_pi2) - (exp(pi2_psi2[2])/s_pi2)
                    ))                                                                                                 * (exp( -exp(a + b * x2 + c2) * (exp(b * t2) - 1)/b) * exp( d2 * (a + b * (x2 + t2) + c2))))
  
  return(-(sum(logL_1) + sum(logL_2)))
}

st_val_vector0 <- c(-10, 0.1, -0.2, -0.4, 0.45, 0.25, 0.3, 0.25, 0.5, 0.25)

sv_pi1 <- (log(st_val_vector0[5:7])) %*% t(psi1) 
sv_pi2 <- (log(st_val_vector0[8:10])) %*% t(psi2) 

st_val <- matrix(NA, 1, 8)
st_val[1:4] <- st_val_vector0[1:4] #init_val[1:5]
st_val[5:6] <- sv_pi1
st_val[7:8] <- sv_pi2

# - Step 4 - Eastimate tau and zeta
theta_est_Red30 <- nlm(NegLogL_ILR_Red30, p=st_val, typsize=st_val, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001

# - Step 5 - Recalculate zeta
pi1_psi1 <- theta_est_Red30$estimate[5:6] %*% psi1
zeta1 <- exp(pi1_psi1)/sum(exp(pi1_psi1))
pi2_psi2 <- theta_est_Red30$estimate[7:8] %*% psi2
zeta2 <- exp(pi2_psi2)/sum(exp(pi2_psi2))

spect_decomp <- eigen(theta_est_Red30$hessian)

eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 8)

k30_est <- matrix(NA, 3, 9)
k30_est[1,1:4] <- theta_est_Red30$estimate[1:4]
k30_est[1,5:6] <- zeta1[1:2]
k30_est[1,7:8] <- zeta2[1:2]
k30_est[1,9] <- -theta_est_Red30$minimum
k30_est[2,1:8] <- theta_est_Red30$gradient
k30_est[3,1:8] <- -eigenval

xtable(k30_est, digits=c(2,2,2,2,2,2,2,2,2,2))

############################# - 4 points of support - ######################

psi1 <- psi_f(8)
psi2 <- psi_f(12)

# Negative log-likelihood function - Two points of support
NegLogL_ILR_Red4 = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  ben = DS1$BH
  
  benL_ind = DS1$BL
  benH_ind = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  geo0_ind = DS2$C0
  geo1_ind = DS2$C1
  geo2_ind = DS2$C2
  
  a  = vdParameters[1] #alpha
  b  = vdParameters[2] #beta
  c1 = vdParameters[3] #delta1
  c2 = vdParameters[4]
  c3 = vdParameters[5]
  
  #change pi psi
  pi1 = matrix(NA, nrow=1, ncol=7)
  pi1[1] = vdParameters[6]
  pi1[2] = vdParameters[7]
  pi1[3] = vdParameters[8]
  pi1[4] = vdParameters[9]
  pi1[5] = vdParameters[10]
  pi1[6] = vdParameters[11]
  pi1[7] = vdParameters[12]
  
  pi2 = matrix(NA, nrow=1, ncol=11)
  pi2[1] = vdParameters[13]
  pi2[2] = vdParameters[14]
  pi2[3] = vdParameters[15]
  pi2[4] = vdParameters[16]
  pi2[5] = vdParameters[17]
  pi2[6] = vdParameters[18]
  pi2[7] = vdParameters[19]
  pi2[8] = vdParameters[20]
  pi2[9] = vdParameters[21]
  pi2[10] = vdParameters[22]
  pi2[11] = vdParameters[23]
  
  pi1_psi1 <- pi1 %*% psi1
  s_pi1 = sum(exp(pi1_psi1))
  
  pi2_psi2 <- pi2 %*% psi2
  s_pi2 = sum(exp(pi2_psi2))
  
  #Weighted log-likelihoods
  
  logL_1 <-  log( (benL_ind * (exp(pi1_psi1[1])/s_pi1) + benH_ind * (exp(pi1_psi1[2])/s_pi1))                                     * (exp( -exp(a + b * x1     ) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1)     ))) +  
                    (benL_ind * (exp(pi1_psi1[3])/s_pi1) + benH_ind * (exp(pi1_psi1[4])/s_pi1))                                     * (exp( -exp(a + b * x1 + c1) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c1))) +  
                    (benL_ind * (exp(pi1_psi1[5])/s_pi1) + benH_ind * (exp(pi1_psi1[6])/s_pi1))                                     * (exp( -exp(a + b * x1 + c2) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c2))) +  
                    (benL_ind * (exp(pi1_psi1[7])/s_pi1) + benH_ind * (1 - (exp(pi1_psi1[1])/s_pi1) - (exp(pi1_psi1[2])/s_pi1) - (exp(pi1_psi1[3])/s_pi1) - (exp(pi1_psi1[4])/s_pi1) - (exp(pi1_psi1[5])/s_pi1) - (exp(pi1_psi1[6])/s_pi1) -
                                                                         (exp(pi1_psi1[7])/s_pi1)))                                 * (exp( -exp(a + b * x1 + c3) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c3)))) 
  
  logL_2 <-  log( (geo0_ind * (exp(pi2_psi2[1])/s_pi2) + geo1_ind * (exp(pi2_psi2[2])/s_pi2) + geo2_ind * (exp(pi2_psi2[3] )/s_pi2)) * (exp( -exp(a + b * x2     ) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2)     ))) + 
                    (geo0_ind * (exp(pi2_psi2[4])/s_pi2) + geo1_ind * (exp(pi2_psi2[5])/s_pi2) + geo2_ind * (exp(pi2_psi2[6])/s_pi2)) * (exp( -exp(a + b * x2 + c1) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2) + c1))) +
                    (geo0_ind * (exp(pi2_psi2[7])/s_pi2) + geo1_ind * (exp(pi2_psi2[8])/s_pi2) + geo2_ind * (exp(pi2_psi2[9])/s_pi2)) * (exp( -exp(a + b * x2 + c2) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2) + c2))) + 
                    (geo0_ind * (exp(pi2_psi2[10])/s_pi2) + geo1_ind * (exp(pi2_psi2[11])/s_pi2) + geo2_ind * (1 - (exp(pi2_psi2[1])/s_pi2) - (exp(pi2_psi2[2])/s_pi2) - (exp(pi2_psi2[3])/s_pi2) - (exp(pi2_psi2[4])/s_pi2) - (exp(pi2_psi2[5])/s_pi2) - (exp(pi2_psi2[6])/s_pi2) - (exp(pi2_psi2[7])/s_pi2) - (exp(pi2_psi2[8])/s_pi2) - (exp(pi2_psi2[9])/s_pi2) - (exp(pi2_psi2[10])/s_pi2) - (exp(pi2_psi2[11])/s_pi2)
                    ))      * (exp( -exp(a + b * x2 + c3) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2) + c3))))
  return(-(sum(logL_1) + sum(logL_2)))
}

# - Results

st_val_vector <- c(-10, 0.1, -0.2, -0.4, -0.6, 0.375, 0.025, 0.225, 0.05, 0.075, 0.1, 0.025, 0.125, 0.125, 0.125, 0.025, 0.075, 0.125, 0.04, 0.025, 0.125, 0.075, 0.025, 0.125, 0.11)

sv_pi1 <- (log(st_val_vector[6:13])) %*% t(psi1) 
sv_pi2 <- (log(st_val_vector[14:25])) %*% t(psi2) 

st_val <- matrix(NA, 1, 23)
st_val[1:5] <- st_val_vector[1:5] #init_val[1:5]
st_val[6:12] <- sv_pi1
st_val[13:23] <- sv_pi2

# - Step 4 - Eastimate tau and zeta
theta_est_Red4 <- nlm(NegLogL_ILR_Red4, p=st_val, typsize=st_val, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001
#eigen(theta_est$hessian) # solve(theta_est$hessian)

# - Step 5 - Recalculate zeta
pi1_psi1 <- theta_est_Red4$estimate[6:12] %*% psi1
zeta1 <- exp(pi1_psi1)/sum(exp(pi1_psi1))
pi2_psi2 <- theta_est_Red4$estimate[13:23] %*% psi2
zeta2 <- exp(pi2_psi2)/sum(exp(pi2_psi2))

spect_decomp <- eigen(theta_est_Red4$hessian)
eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 23)

############################# - 4 points of support with zeroes - ######################

psi1 <- psi_f(2)
psi2 <- psi_f(3)

# Negative log-likelihood function - Two points of support
NegLogL_ILR_Red40 = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  ben = DS1$BH
  
  benL_ind = DS1$BL
  benH_ind = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  geo0_ind = DS2$C0
  geo1_ind = DS2$C1
  geo2_ind = DS2$C2
  
  a  = vdParameters[1] #alpha
  b  = vdParameters[2] #beta
  c1 = vdParameters[3] #delta1
  c2 = vdParameters[4]
  c3 = vdParameters[5]
  
  #change pi psi
  pi1 = matrix(NA, nrow=1, ncol=1)
  pi1[1] = vdParameters[6]
  
  pi2 = matrix(NA, nrow=1, ncol=2)
  pi2[1] = vdParameters[7]
  pi2[2] = vdParameters[8]
  
  pi1_psi1 <- pi1 %*% psi1
  s_pi1 = sum(exp(pi1_psi1))
  
  pi2_psi2 <- pi2 %*% psi2
  s_pi2 = sum(exp(pi2_psi2))
  
  #Weighted log-likelihoods
  
  logL_1 <-  log( 
    (benL_ind * (exp(pi1_psi1[1])/s_pi1)                                      )                                     * (exp( -exp(a + b * x1 + c1) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c1))) +  
      (                                      benH_ind * (1 - (exp(pi1_psi1[1])/s_pi1)
      ))                                 * (exp( -exp(a + b * x1 + c3) * (exp(b * t1) - 1)/b) * exp(d1 * (a + b * (x1 + t1) + c3)))) 
  
  logL_2 <-  log( (geo0_ind * (exp(pi2_psi2[1])/s_pi2)                                                                             ) * (exp( -exp(a + b * x2     ) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2)     ))) + 
                    (                                      geo1_ind * (exp(pi2_psi2[2])/s_pi2)                                       ) * (exp( -exp(a + b * x2 + c2) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2) + c2))) + 
                    (                                                                             geo2_ind * (1 - (exp(pi2_psi2[1])/s_pi2) - (exp(pi2_psi2[2])/s_pi2)
                    ))      * (exp( -exp(a + b * x2 + c3) * (exp(b * t2) - 1)/b) * exp(d2 * (a + b * (x2 + t2) + c3))))
  return(-(sum(logL_1) + sum(logL_2)))
}

# - Results

st_val_vector <- c(-10, 0.1, -0.2, -0.4, -0.6, 0.75, 0.25, 0.25, 0.5, 0.25)

sv_pi1 <- (log(st_val_vector[6:7])) %*% t(psi1) 
sv_pi2 <- (log(st_val_vector[8:10])) %*% t(psi2) 

st_val <- matrix(NA, 1, 8)
st_val[1:5] <- st_val_vector[1:5] #init_val[1:5]
st_val[6] <- sv_pi1
st_val[7:8] <- sv_pi2

# - Step 4 - Eastimate tau and zeta
theta_est_Red40 <- nlm(NegLogL_ILR_Red40, p=st_val, typsize=st_val, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001
#eigen(theta_est$hessian) # solve(theta_est$hessian)

# - Step 5 - Recalculate zeta
pi1_psi1 <- theta_est_Red40$estimate[6] %*% psi1
zeta1 <- exp(pi1_psi1)/sum(exp(pi1_psi1))
pi2_psi2 <- theta_est_Red40$estimate[7:8] %*% psi2
zeta2 <- exp(pi2_psi2)/sum(exp(pi2_psi2))

spect_decomp <- eigen(theta_est_Red40$hessian)
eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 8)

k40_est <- matrix(NA, 3, 9)
k40_est[1,1:5] <- theta_est_Red40$estimate[1:5]
k40_est[1,6] <- zeta1[1]
k40_est[1,7:8] <- zeta2[1:2]
k40_est[1,9] <- -theta_est_Red40$minimum
k40_est[2,1:8] <- theta_est_Red40$gradient
k40_est[3,1:8] <- -eigenval

xtable(k40_est, digits=c(2,2,2,2,2,2,2,2,2,2))

#############################################################################

# - Model selection

#############################################################################

################################################
### - Info criteria
loglik_BC <- nrow(DS1[DS1$BL==1,]) * log(zeta1[1]) + nrow(DS1[DS1$BH==1,]) * log(zeta1[2]) + 
  nrow(DS2[DS2$C0==1,]) * log(zeta2[1]) + nrow(DS2[DS2$C1==1,]) * log(zeta2[2]) + nrow(DS2[DS2$C2==1,]) * log(zeta2[3])

AIC2 <- -2 * (-theta_est_Red20$minimum) + 2*5
AIC3 <- -2 * (-theta_est_Red30$minimum) + 2*5
AIC4 <- -2 * (-theta_est_Red40$minimum) + 2*5

BIC2 <- -2 * (-theta_est_Red20$minimum) + log(50000)*5
BIC3 <- -2 * (-theta_est_Red30$minimum) + log(50000)*5
BIC4 <- -2 * (-theta_est_Red40$minimum) + log(50000)*5

info_crit <- matrix(NA, 3, 3)
info_crit[,1] <- 2:4
info_crit[1,2] <- AIC2
info_crit[1,3] <- BIC2
info_crit[2,2] <- AIC3
info_crit[2,3] <- BIC3
info_crit[3,2] <- AIC4
info_crit[3,3] <- BIC4

xtable(info_crit)

##################################################################################

# - Separate dataset modelling

#####################################################################################

# - Complete data for starting values
NegLogL_DS1=function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  ben1 = DS1$BH
  
  a = vdParameters[1] #alpha
  b = vdParameters[2] #beta
  c = vdParameters[3] #delta1
  
  #Weighted log-likelihoods
  
  logL_DS1 <-  -exp(a + b * x1 + c * ben1) * (exp(b * t1) - 1)/b + d1 * (a + b * (x1 + t1) + c * ben1) 
  
  return(-(sum(logL_DS1)))
}
st_val_DS1 <- c(-10, 0.1, 0)
# - Benchmark
theta_est_DS1 <- nlm(NegLogL_DS1, p=st_val_DS1, typsize=st_val_DS1, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001


NegLogL_DS2=function(vdParameters){    #
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  a   = vdParameters[1] #alpha
  b   = vdParameters[2] #beta
  de1 = vdParameters[3] #delta1
  de2 = vdParameters[4]
  
  #Weighted log-likelihoods
  
  logL_DS2 <-  -exp(a + b * x2 + de1 * gde1 + de2 * gde2) * (exp(b * t2) - 1)/b + d2 * (a + b * (x2 + t2) + de1 * gde1 + de2 * gde2) 
  
  return(-(sum(logL_DS2)))
}
st_val_DS2 <- c(-10, 0.1, 0, 0)
# - Benchmark
theta_est_DS2 <- nlm(NegLogL_DS2, p=st_val_DS2, typsize=st_val_DS2, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001


###########################################################################

# - Standard errors computation by bootstrap

###########################################################################

# Negative log-likelihood function - Two points of support
NegLogL_ILR_Red40_fin = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs_BS
  d1 = DS1$Status_BS
  
  x2 = DS2$EntAge
  t2 = DS2$TObs_BS
  d2 = DS2$Status_BS
  
  blow = DS1$BL
  bhigh = DS1$BH
  
  gde1 = DS2$C1
  gde2 = DS2$C2
  
  a  = vdParameters[1] #alpha
  b  = vdParameters[2] #beta
  c1 = vdParameters[3] #delta1
  c2 = vdParameters[4]
  c3 = vdParameters[5]
  
  logL_1 <-  - exp(a + b * x1 + blow * c1 + bhigh * c3) * (exp(b * t1) - 1)/b + d1 * (a + b * (x1 + t1) + blow * c1 + bhigh * c3)
  
  logL_2 <-  - exp(a + b * x2 + gde1 * c2 + gde2 * c3) * (exp(b * t2) - 1)/b +  d2 * (a + b * (x2 + t2) + gde1 * c2 + gde2 * c3) 
  
  return(-(sum(logL_1) + sum(logL_2)))
}


alpha_hat <- theta_est_Red40$estimate[1]
beta_hat <- theta_est_Red40$estimate[2]
gamma1_hat <- theta_est_Red40$estimate[3]
gamma2_hat <- theta_est_Red40$estimate[4]
gamma3_hat <- theta_est_Red40$estimate[5]

DS1$TObs_BS <- 0
DS2$TObs_BS <- 0

DS1$Status_BS <- 0
DS2$Status_BS <- 0

bootstrap_par_est_c <- matrix(NA, 1500, 5)

n_bootstraps <- 2 * ((qnorm((0.05/10), mean = 0, sd = 1))/0.1)^2

st_val <- c(-10, 0.1, -0.2, -0.3, -0.5)

for (i in 796:1500){ #round(n_bootstraps,0)
  
  for (j in 1:nrow(DS1)){
    k1 <- log( 1 + beta_hat * rexp(1, rate=exp(alpha_hat + beta_hat * DS1$EntAge[j] + gamma1_hat * DS1$BL[j] + gamma3_hat * DS1$BH[j] ))) / beta_hat
    DS1$TObs_BS[j] <- ifelse(k1 > (2010 - DS1$EntryYear[j]), (2010 - DS1$EntryYear[j]), k1 )
    DS1$Status_BS[j] <- ifelse(k1 > (2010 - DS1$EntryYear[j]), 0, 1 )
    #DS1[j,13] <- ifelse(DS1[j,1]==1, log( 1 + beta_hat * rexp(1, rate=exp(alpha_hat + beta_hat * DS1[j, 5] + gamma1_hat * DS1[j, 7] + gamma3_hat * DS1[j, 8] ))) / beta_hat, min(DS1[j,4], log( 1 + beta_hat * rexp(1, rate=exp(alpha_hat + beta_hat * DS1[j, 5] + gamma1_hat * DS1[j, 7] + gamma3_hat * DS1[j, 8] ))) / beta_hat))
  }
  
  for (j in 1:nrow(DS2)){
    k2 <- log( 1 + beta_hat * rexp(1, rate=exp(alpha_hat + beta_hat * DS2$EntAge[j] + gamma2_hat * DS2$C1[j] + gamma3_hat * DS2$C2[j] ))) / beta_hat
    DS2$TObs_BS[j] <- ifelse(k2 > (2010 - DS2$EntryYear[j]), (2010 - DS2$EntryYear[j]), k2)
    DS2$Status_BS[j] <- ifelse(k2 > (2010 - DS2$EntryYear[j]), 0, 1 )
  }
  
  theta_est_Red40_fin <- nlm(NegLogL_ILR_Red40_fin, p=st_val, typsize=st_val, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001
  
  bootstrap_par_est_c[i,1:5] <- theta_est_Red40_fin$estimate
  
}

# - Variance/covariance matrix
MD_cov_BS <- cov(bootstrap_par_est_c)
colnames(MD_cov_BS) <- c("$alpha$", "$beta$", "$gamma_1$", "$gamma_2$", "$gamma_3$")
rownames(MD_cov_BS) <- c("$alpha$", "$beta$", "$gamma_1$", "$gamma_2$", "$gamma_3$")

xtable(MD_cov_BS)#, digits = c(8,8,8,8,8,8))

# - Correlation matrix
xtable(cov2cor(MD_cov_BS))

# - Synthesis table Missing data
par_features_MD <- matrix(NA, 5,4)
par_features_MD[,1] <- theta_est_Red40$estimate[1:5]
for (i in 1:5){
  par_features_MD[i,2] <- sqrt(MD_cov_BS[i,i])*1000
}
par_features_MD[,3] <- par_features_MD[,1] - 1.96*par_features_MD[i,2]/1000
par_features_MD[,4] <- par_features_MD[,1] + 1.96*par_features_MD[i,2]/1000  

xtable(par_features_MD)

# - MD variance/covariance with inversion
MD_inv <- solve(theta_est_Red40$hessian[1:5,1:5])
colnames(MD_inv) <- c("$alpha$", "$beta$", "$gamma_1$", "$gamma_2$", "$gamma_3$")
rownames(MD_inv) <- c("$alpha$", "$beta$", "$gamma_1$", "$gamma_2$", "$gamma_3$")
xtable(MD_inv)# , digits = c(8,8,8,8,8,8))

# - Separate modelling
DS1_cov <- solve(theta_est_DS1$hessian)
colnames(DS1_cov) <- c("$alpha$", "$beta$", "$gamma$")
rownames(DS1_cov) <- c("$alpha$", "$beta$", "$gamma$")
xtable(DS1_cov, digits = c(8,8,8,8))

# - Synthesis table DS1
par_features_DS1 <- matrix(NA, 3,4)
par_features_DS1[,1] <- theta_est_DS1$estimate[1:3]
for (i in 1:3){
  par_features_DS1[i,2] <- sqrt(DS1_cov[i,i])*1000
}
par_features_DS1[,3] <- par_features_DS1[,1] - 1.96*par_features_DS1[i,2]/1000
par_features_DS1[,4] <- par_features_DS1[,1] + 1.96*par_features_DS1[i,2]/1000  

xtable(par_features_DS1)

DS2_cov <- solve(theta_est_DS2$hessian)
colnames(DS2_cov) <- c("$alpha$", "$beta$", "$delta_1$", "$delta_2$")
rownames(DS2_cov) <- c("$alpha$", "$beta$", "$delta_1$", "$delta_2$")
xtable(DS2_cov, digits = c(8,8,8,8,8))

# - Synthesis table DS2
par_features_DS2 <- matrix(NA, 4, 4)
par_features_DS2[,1] <- theta_est_DS2$estimate[1:4]
for (i in 1:4){
  par_features_DS2[i,2] <- sqrt(DS2_cov[i,i])*1000
}
par_features_DS2[,3] <- par_features_DS2[,1] - 1.96*par_features_DS2[i,2]/1000
par_features_DS2[,4] <- par_features_DS2[,1] + 1.96*par_features_DS2[i,2]/1000  

xtable(par_features_DS2)

# - Model with alpha/beta only
M0_cov <- solve(Model0$hessian)
colnames(M0_cov) <- c("$alpha$", "$beta$")
rownames(M0_cov) <- c("$alpha$", "$beta$")
xtable(M0_cov, digits = c(8,8,8))

# - Synthesis table DS1
par_features_M0 <- matrix(NA, 2,4)
par_features_M0[,1] <- Model0$estimate[1:2]
for (i in 1:2){
  par_features_M0[i,2] <- sqrt(M0_cov[i,i])*1000
}
par_features_M0[,3] <- par_features_M0[,1] - 1.96*par_features_M0[i,2]/1000
par_features_M0[,4] <- par_features_M0[,1] + 1.96*par_features_M0[i,2]/1000  

xtable(par_features_M0)

########################################################################################################################

##################################### - Misestimation risk capital requirement - #######################################

########################################################################################################################

# - Joint dataset modelling

z <- matrix(0, 5, 1)

A <- chol(cov(bootstrap_par_est_c))
A <- t(A) # getting the lower-triangular matrix

theta_mle <- theta_est_Red40$estimate[1:5]

# - Annuity portfolio valuation
## - DS1

x1 <- DS1$EntAge
bl <- DS1$BL
bh <- DS1$BH
ben_amt <- DS1$Benefit

portfolio1_val <- matrix(NA, 9000, 2)
ann <- matrix(NA, nrow(DS1),1)

for (i in 1:9000){
  z[,1] <- rnorm(5)
  theta_sim <- theta_mle + A %*% z
  a_sim <- theta_sim[1]
  b_sim <- theta_sim[2]
  c1_sim <- theta_sim[3]
  c2_sim <- theta_sim[4]
  c3_sim <- theta_sim[5]
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.01) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + bl[j] * c1_sim + bh[j] * c3_sim) + a_sim + b_sim * (x1[j] + t) + bl[j] * c1_sim + bh[j] * c3_sim)
    }
    annuity = integrate(integrand, lower=0, upper=Inf)
    ann[j,1] <- annuity$value 
    
  }
  portfolio1_val[i,1] <- sum(ann[,1]) 
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.03) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + bl[j] * c1_sim + bh[j] * c3_sim) + a_sim + b_sim * (x1[j] + t) + bl[j] * c1_sim + bh[j] * c3_sim)
    }
    annuity = integrate(integrand, lower=0, upper=Inf)
    ann[j,1] <- annuity$value 
  }
  portfolio1_val[i,2] <- sum(ann[,1]) 
}  
  
# - Separate dataset modelling

# - DS1 

z <- matrix(0, 3, 1)

A1 <- chol(solve(theta_est_DS1$hessian))
A1 <- t(A1) # getting the lower-triangular matrix

theta1_mle <- theta_est_DS1$estimate
x1 <- DS1$EntAge
bl <- DS1$BL
bh <- DS1$BH
ben_amt <- DS1$Benefit

# - Annuity portfolio valuation
## - DS1

portfolio1_val_sep <- matrix(NA, 10000, 2)
ann <- matrix(NA, nrow(DS1),1)

for (i in 1:10000){
  z[,1] <- rnorm(3)
  theta_sim <- theta1_mle + A1 %*% z
  a_sim <- theta_sim[1]
  b_sim <- theta_sim[2]
  c_sim <- theta_sim[3]
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.01) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + c_sim * bh[j]) + a_sim + b_sim * (x1[j] + t) + c_sim * bh[j])
    }
    annuity = integrate(integrand, lower=0, upper=Inf)
    ann[j,1] <- annuity$value 
    
  }
  portfolio1_val_sep[i,1] <- sum(ann[,1]) 
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.03) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + c_sim * bh[j]) + a_sim + b_sim * (x1[j] + t) + c_sim * bh[j])
    }
    annuity = integrate(integrand, lower=0, upper=Inf)
    ann[j,1] <- annuity$value 
  }
  portfolio1_val_sep[i,2] <- sum(ann[,1]) 
}

# - Misestimation cap requirement
c_req_1 <- quantile(portfolio1_val[,1], 0.995)/mean(portfolio1_val[,1])-1
c_req_3 <- quantile(portfolio1_val[,2], 0.995)/mean(portfolio1_val[,2])-1

c_req_1_s <- quantile(portfolio1_val_sep[,1], 0.995)/mean(portfolio1_val_sep[,1])-1
c_req_3_s <- quantile(portfolio1_val_sep[,2], 0.995)/mean(portfolio1_val_sep[,2])-1

##########
# - Histograms
hist(portfolio1_val[,1], main="Missing Data Portfolio re-evaluations - rate=1%")
abline(v=quantile(portfolio1_val[,1], 0.995), col="blue")
abline(v=mean(portfolio1_val[,1]), col="red")

hist(portfolio1_val[,2], main="Missing Data Portfolio re-evaluations - rate=3%")
abline(v=quantile(portfolio1_val[,2], 0.995), col="blue")
abline(v=mean(portfolio1_val[,2]), col="red")

# - Histograms
hist(portfolio1_val[,1], main="Sep. Val. Portfolio re-evaluations - rate=1%")
abline(v=quantile(portfolio1_val_sep[,1], 0.995), col="blue")
abline(v=mean(portfolio1_val_sep[,1]), col="red")

hist(portfolio1_val[,2], main="Sep. Val. Portfolio re-evaluations - rate=3%")
abline(v=quantile(portfolio1_val_sep[,2], 0.995), col="blue")
abline(v=mean(portfolio1_val_sep[,2]), col="red")  
  
#################################################################################################################################

################ - Application to other relevant quantities, eg hazard function, and life expectancy - ############################

#################################################################################################################################

mix_par_10000 <- matrix(NA, 10000, 41)
sep_mod_gam_par_10000 <- matrix(NA, 10000, 21)
sep_mod_del_par_10000 <- matrix(NA, 10000, 31)
M0_10000 <- matrix(NA, 10000, 11)

# - Mixture parameters
z <- matrix(0, 5, 1)

A <- chol(cov(bootstrap_par_est_c))
A <- t(A) # getting the lower-triangular matrix

theta_mle <- theta_est$estimate[1:5]

for (i in 1:10000){
  z[,1] <- rnorm(5)
  mix_par_10000[i,1:5] <- theta_mle + A %*% z
  
  #low benefit
  j=0
  for (x in c(60,75,90)){
    mix_par_10000[i,6+j] <- exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,3]) # mu_x
    
    sum_p_x <- function(t){
      exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,3]) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    lif_exp = integrate(sum_p_x, lower=0, upper=120-x)
    
    mix_par_10000[i,7+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,3]) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    mix_par_10000[i,8+j] <- var_lif_exp
    
    j=j+3  
  }
  
  #High benefit and Geodem 2
  j=0
  for (x in c(60,75,90)){
    mix_par_10000[i,15+j] <- exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,5]) # mu_x

    integrand <- function(t){
      exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,5]) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    mix_par_10000[i,16+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,5]) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    mix_par_10000[i,17+j] <- var_lif_exp
    
    j=j+3  
  }
  
  #Geodem 0
  j=0
  for (x in c(60,75,90)){
    mix_par_10000[i,24+j] <- exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x ) # mu_x

    integrand <- function(t){
      exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x ) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    mix_par_10000[i,25+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x ) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    mix_par_10000[i,26+j] <- var_lif_exp
    
    j=j+3  
  }
  
  #Geodem 1
  j=0
  for (x in c(60,75,90)){
    mix_par_10000[i,33+j] <- exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,4]) # mu_x

    integrand <- function(t){
      exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,4]) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    mix_par_10000[i,34+j] <- lif_exp$value 

    sum_t_p_x <- function(t){
      2 * t * exp(-exp(mix_par_10000[i,1] + mix_par_10000[i,2] * x + mix_par_10000[i,4]) * (exp(mix_par_10000[i,2] * t) - 1) / mix_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    mix_par_10000[i,35+j] <- var_lif_exp    
    
    
    j=j+3  
  }
  
}

mix_par_10000 <- as.data.frame(mix_par_10000)
colnames(mix_par_10000) <- c("alpha", "beta", "gamma_1", "gamma_2", "gamma_3", "mu_60_BL", "e_60_BL", "var_e_60_BL", "mu_75_BL", "e_75_BL", "var_e_75_BL", "mu_90_BL", "e_90_BL", "var_e_90_BL", "mu_60_BH_C2", "e_60_BH_C2", "var_e_60_BH_C2", "mu_75_BH_C2", "e_75_BH_C2", "var_e_75_BH_C2", "mu_90_BH_C2", "e_90_BH_C2", "var_e_90_BH_C2", "mu_60_C0", "e_60_C0", "var_e_60_C0", "mu_75_C0", "e_75_C0", "var_e_75_C0", "mu_90_C0", "e_90_C0", "var_e_90_C0", "mu_60_C1", "e_60_C1", "var_e_60_C1", "mu_75_C1", "e_75_C1", "var_e_75_C1", "mu_90_C1", "e_90_C1", "var_e_90_C1" )

###########################################################################################################################################################

# - Separate modelling DS1, hence gamma
z <- matrix(0, 3, 1)

A <- chol(solve(theta_est_DS1$hessian))
A <- t(A) # getting the lower-triangular matrix

for (i in 1:10000){
  z[,1] <- rnorm(3)
  sep_mod_gam_par_10000[i,1:3] <- theta_est_DS1$estimate + A %*% z
  
  #Low benefit 0
  j=0
  for (x in c(60,75,90)){
    sep_mod_gam_par_10000[i,4+j] <- exp(sep_mod_gam_par_10000[i,1] + sep_mod_gam_par_10000[i,2] * x ) # mu_x

    integrand <- function(t){
      exp(-exp(sep_mod_gam_par_10000[i,1] + sep_mod_gam_par_10000[i,2] * x ) * (exp(sep_mod_gam_par_10000[i,2] * t) - 1) / sep_mod_gam_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    sep_mod_gam_par_10000[i,5+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(sep_mod_gam_par_10000[i,1] + sep_mod_gam_par_10000[i,2] * x ) * (exp(sep_mod_gam_par_10000[i,2] * t) - 1) / sep_mod_gam_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    sep_mod_gam_par_10000[i,6+j] <- var_lif_exp        
    
    j=j+3  
  }
  
  #high benefit
  j=0
  for (x in c(60,75,90)){
    sep_mod_gam_par_10000[i,13+j] <- exp(sep_mod_gam_par_10000[i,1] + sep_mod_gam_par_10000[i,2] * x + sep_mod_gam_par_10000[i,3]) # mu_x

    integrand <- function(t){
      exp(-exp(sep_mod_gam_par_10000[i,1] + sep_mod_gam_par_10000[i,2] * x + sep_mod_gam_par_10000[i,3]) * (exp(sep_mod_gam_par_10000[i,2] * t) - 1) / sep_mod_gam_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    sep_mod_gam_par_10000[i,14+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(sep_mod_gam_par_10000[i,1] + sep_mod_gam_par_10000[i,2] * x + sep_mod_gam_par_10000[i,3]) * (exp(sep_mod_gam_par_10000[i,2] * t) - 1) / sep_mod_gam_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    sep_mod_gam_par_10000[i,15+j] <- var_lif_exp        

    j=j+3  
  }

}

sep_mod_gam_par_10000 <- as.data.frame(sep_mod_gam_par_10000)
colnames(sep_mod_gam_par_10000) <- c("alpha", "beta", "gamma", "mu_60_BL", "e_60_BL", "var_e_60_BL", "mu_75_BL", "e_75_BL", "var_e_75_BL", "mu_90_BL", "e_90_BL", "var_e_90_BL", "mu_60_BH", "e_60_BH", "var_e_60_BH", "mu_75_BH", "e_75_BH", "var_e_75_BH", "mu_90_BH", "e_90_BH", "var_e_90_BH")

###########################################################################################################################################################

# - Separate modelling DS2, hence delta
z <- matrix(0, 4, 1)

A <- chol(solve(theta_est_DS2$hessian))
A <- t(A) # getting the lower-triangular matrix

for (i in 1:10000){
  z[,1] <- rnorm(4)
  sep_mod_del_par_10000[i,1:4] <- theta_est_DS2$estimate + A %*% z
  
  #Geodem 0
  j=0
  for (x in c(60,75,90)){
    sep_mod_del_par_10000[i,5+j] <- exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x ) # mu_x

    integrand <- function(t){
      exp(-exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x ) * (exp(sep_mod_del_par_10000[i,2] * t) - 1) / sep_mod_del_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    sep_mod_del_par_10000[i,6+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x ) * (exp(sep_mod_del_par_10000[i,2] * t) - 1) / sep_mod_del_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    sep_mod_del_par_10000[i,7+j] <- var_lif_exp        

    j=j+3  
  }
  
  #Geodem 1
  j=0
  for (x in c(60,75,90)){
    sep_mod_del_par_10000[i,14+j] <- exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x + sep_mod_del_par_10000[i,3] ) # mu_x

    integrand <- function(t){
      exp(-exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x + sep_mod_del_par_10000[i,3]  ) * (exp(sep_mod_del_par_10000[i,2] * t) - 1) / sep_mod_del_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    sep_mod_del_par_10000[i,15+j] <- lif_exp$value 

    sum_t_p_x <- function(t){
      2 * t * exp(-exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x + sep_mod_del_par_10000[i,3]  ) * (exp(sep_mod_del_par_10000[i,2] * t) - 1) / sep_mod_del_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    sep_mod_del_par_10000[i,16+j] <- var_lif_exp        
        
    j=j+3  
  }
  
  #Geodem 2
  j=0
  for (x in c(60,75,90)){
    sep_mod_del_par_10000[i,23+j] <- exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x + sep_mod_del_par_10000[i,4] ) # mu_x

    integrand <- function(t){
      exp(-exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x + sep_mod_del_par_10000[i,4]  ) * (exp(sep_mod_del_par_10000[i,2] * t) - 1) / sep_mod_del_par_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    sep_mod_del_par_10000[i,24+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(sep_mod_del_par_10000[i,1] + sep_mod_del_par_10000[i,2] * x + sep_mod_del_par_10000[i,4]  ) * (exp(sep_mod_del_par_10000[i,2] * t) - 1) / sep_mod_del_par_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    sep_mod_del_par_10000[i,25+j] <- var_lif_exp        

    j=j+3  
  }
  
}

sep_mod_del_par_10000 <- as.data.frame(sep_mod_del_par_10000)
colnames(sep_mod_del_par_10000) <- c("alpha", "beta", "delta_1", "delta_2", "mu_60_C0", "e_60_C0", "var_e_60_C0", "mu_75_C0", "e_75_C0", "var_e_75_C0", "mu_90_C0", "e_90_C0", "var_e_90_C0", "mu_60_C1", "e_60_C1", "var_e_60_C1", "mu_75_C1", "e_75_C1", "var_e_75_C1", "mu_90_C1", "e_90_C1", "var_e_90_C1", "mu_60_C2", "e_60_C2", "var_e_60_C2", "mu_75_C2", "e_75_C2", "var_e_75_C2", "mu_90_C2", "e_90_C2", "var_e_90_C2" )

###########################################################################################################################################################

# - Model with alpha and beta only
z <- matrix(0, 2, 1)

A <- chol(solve(Model0$hessian))
A <- t(A) # getting the lower-triangular matrix

for (i in 1:10000){
  z[,1] <- rnorm(2)
  M0_10000[i,1:2] <- Model0$estimate + A %*% z
  
  j=0
  for (x in c(60,75,90)){
    M0_10000[i,3+j] <- exp(M0_10000[i,1] + M0_10000[i,2] * x ) # mu_x

    integrand <- function(t){
      exp(-exp(M0_10000[i,1] + M0_10000[i,2] * x ) * (exp(M0_10000[i,2] * t) - 1) / M0_10000[i,2] )
    }
    lif_exp = integrate(integrand, lower=0, upper=120-x)
    
    M0_10000[i,4+j] <- lif_exp$value 
    
    sum_t_p_x <- function(t){
      2 * t * exp(-exp(M0_10000[i,1] + M0_10000[i,2] * x ) * (exp(M0_10000[i,2] * t) - 1) / M0_10000[i,2] )
    }
    
    sec_mom_t <- integrate(sum_t_p_x, lower=0, upper=120-x)
    
    var_lif_exp = sec_mom_t$value - (lif_exp$value)^2
    
    M0_10000[i,5+j] <- var_lif_exp        
    
    j=j+3  
  }
}

M0_10000 <- as.data.frame(M0_10000)
colnames(M0_10000) <- c("alpha", "beta", "mu_60", "e_60", "var_e_60", "mu_75", "e_75", "var_e_75", "mu_90", "e_90", "var_e_90")

################################## - expectations - ############################################################

# - Representing information

exp_mu <- matrix(NA, 5, 12)
exp_life <- matrix(NA, 5, 12)

sd_mu <- matrix(NA, 5, 12)
sd_life <- matrix(NA, 5, 12)
sd_par_life <- matrix(NA, 5, 12)

lbd_mu <- matrix(NA, 5, 12)
lbd_life <- matrix(NA, 5, 12)

ubd_mu <- matrix(NA, 5, 12)
ubd_life <- matrix(NA, 5, 12)

#mu_x
## - missing data
exp_mu[1,c(1,5,9)] <- 100 * c(mean(mix_par_10000$mu_60_BL), mean(mix_par_10000$mu_75_BL), mean(mix_par_10000$mu_90_BL))
exp_mu[2,c(1,5,9)] <- 100 * c(mean(mix_par_10000$mu_60_BH_C2), mean(mix_par_10000$mu_75_BH_C2), mean(mix_par_10000$mu_90_BH_C2))
exp_mu[3,c(1,5,9)] <- 100 * c(mean(mix_par_10000$mu_60_C0), mean(mix_par_10000$mu_75_C0), mean(mix_par_10000$mu_90_C0))
exp_mu[4,c(1,5,9)] <- 100 * c(mean(mix_par_10000$mu_60_C1), mean(mix_par_10000$mu_75_C1), mean(mix_par_10000$mu_90_C1))
exp_mu[5,c(1,5,9)] <- 100 * c(mean(mix_par_10000$mu_60_BH_C2), mean(mix_par_10000$mu_75_BH_C2), mean(mix_par_10000$mu_90_BH_C2))

## - PS1
exp_mu[1,c(2,6,10)] <- 100 * c(mean(sep_mod_gam_par_10000$mu_60_BL), mean(sep_mod_gam_par_10000$mu_75_BL), mean(sep_mod_gam_par_10000$mu_90_BL))
exp_mu[2,c(2,6,10)] <- 100 * c(mean(sep_mod_gam_par_10000$mu_60_BH), mean(sep_mod_gam_par_10000$mu_75_BH), mean(sep_mod_gam_par_10000$mu_90_BH))
exp_mu[3,c(2,6,10)] <- c(0,0,0)
exp_mu[4,c(2,6,10)] <- c(0,0,0)
exp_mu[5,c(2,6,10)] <- c(0,0,0)

## - PS2
exp_mu[1,c(3,7,11)] <- c(0,0,0)
exp_mu[2,c(3,7,11)] <- c(0,0,0)
exp_mu[3,c(3,7,11)] <- 100 * c(mean(sep_mod_del_par_10000$mu_60_C0), mean(sep_mod_del_par_10000$mu_75_C0), mean(sep_mod_del_par_10000$mu_90_C0))
exp_mu[4,c(3,7,11)] <- 100 * c(mean(sep_mod_del_par_10000$mu_60_C1), mean(sep_mod_del_par_10000$mu_75_C1), mean(sep_mod_del_par_10000$mu_90_C1))
exp_mu[5,c(3,7,11)] <- 100 * c(mean(sep_mod_del_par_10000$mu_60_C2), mean(sep_mod_del_par_10000$mu_75_C2), mean(sep_mod_del_par_10000$mu_90_C2))

## - Model 0
exp_mu[1,c(4,8,12)] <- 100 * c(mean(M0_10000$mu_60), mean(M0_10000$mu_75), mean(M0_10000$mu_90))
exp_mu[2,c(4,8,12)] <- 100 * c(mean(M0_10000$mu_60), mean(M0_10000$mu_75), mean(M0_10000$mu_90))
exp_mu[3,c(4,8,12)] <- 100 * c(mean(M0_10000$mu_60), mean(M0_10000$mu_75), mean(M0_10000$mu_90))
exp_mu[4,c(4,8,12)] <- 100 * c(mean(M0_10000$mu_60), mean(M0_10000$mu_75), mean(M0_10000$mu_90))
exp_mu[5,c(4,8,12)] <- 100 * c(mean(M0_10000$mu_60), mean(M0_10000$mu_75), mean(M0_10000$mu_90))

#e_x
## - missing data
exp_life[1,c(1,5,9)] <- c(mean(mix_par_10000$e_60_BL), mean(mix_par_10000$e_75_BL), mean(mix_par_10000$e_90_BL))
exp_life[2,c(1,5,9)] <- c(mean(mix_par_10000$e_60_BH_C2), mean(mix_par_10000$e_75_BH_C2), mean(mix_par_10000$e_90_BH_C2))
exp_life[3,c(1,5,9)] <- c(mean(mix_par_10000$e_60_C0), mean(mix_par_10000$e_75_C0), mean(mix_par_10000$e_90_C0))
exp_life[4,c(1,5,9)] <- c(mean(mix_par_10000$e_60_C1), mean(mix_par_10000$e_75_C1), mean(mix_par_10000$e_90_C1))
exp_life[5,c(1,5,9)] <- c(mean(mix_par_10000$e_60_BH_C2), mean(mix_par_10000$e_75_BH_C2), mean(mix_par_10000$e_90_BH_C2))

## - PS1
exp_life[1,c(2,6,10)] <- c(mean(sep_mod_gam_par_10000$e_60_BL), mean(sep_mod_gam_par_10000$e_75_BL), mean(sep_mod_gam_par_10000$e_90_BL))
exp_life[2,c(2,6,10)] <- c(mean(sep_mod_gam_par_10000$e_60_BH), mean(sep_mod_gam_par_10000$e_75_BH), mean(sep_mod_gam_par_10000$e_90_BH))
exp_life[3,c(2,6,10)] <- c(0,0,0)
exp_life[4,c(2,6,10)] <- c(0,0,0)
exp_life[5,c(2,6,10)] <- c(0,0,0)

## - PS2
exp_life[1,c(3,7,11)] <- c(0,0,0)
exp_life[2,c(3,7,11)] <- c(0,0,0)
exp_life[3,c(3,7,11)] <- c(mean(sep_mod_del_par_10000$e_60_C0), mean(sep_mod_del_par_10000$e_75_C0), mean(sep_mod_del_par_10000$e_90_C0))
exp_life[4,c(3,7,11)] <- c(mean(sep_mod_del_par_10000$e_60_C1), mean(sep_mod_del_par_10000$e_75_C1), mean(sep_mod_del_par_10000$e_90_C1))
exp_life[5,c(3,7,11)] <- c(mean(sep_mod_del_par_10000$e_60_C2), mean(sep_mod_del_par_10000$e_75_C2), mean(sep_mod_del_par_10000$e_90_C2))

## - Model 0
exp_life[1,c(4,8,12)] <- c(mean(M0_10000$e_60), mean(M0_10000$e_75), mean(M0_10000$e_90))
exp_life[2,c(4,8,12)] <- c(mean(M0_10000$e_60), mean(M0_10000$e_75), mean(M0_10000$e_90))
exp_life[3,c(4,8,12)] <- c(mean(M0_10000$e_60), mean(M0_10000$e_75), mean(M0_10000$e_90))
exp_life[4,c(4,8,12)] <- c(mean(M0_10000$e_60), mean(M0_10000$e_75), mean(M0_10000$e_90))
exp_life[5,c(4,8,12)] <- c(mean(M0_10000$e_60), mean(M0_10000$e_75), mean(M0_10000$e_90))

################################## - standard deviations - ############################################################

#mu_x
## - missing data
sd_mu[1,c(1,5,9)] <- 1000 * c(sd(mix_par_10000$mu_60_BL), sd(mix_par_10000$mu_75_BL), sd(mix_par_10000$mu_90_BL))
sd_mu[2,c(1,5,9)] <- 1000 * c(sd(mix_par_10000$mu_60_BH_C2), sd(mix_par_10000$mu_75_BH_C2), sd(mix_par_10000$mu_90_BH_C2))
sd_mu[3,c(1,5,9)] <- 1000 * c(sd(mix_par_10000$mu_60_C0), sd(mix_par_10000$mu_75_C0), sd(mix_par_10000$mu_90_C0))
sd_mu[4,c(1,5,9)] <- 1000 * c(sd(mix_par_10000$mu_60_C1), sd(mix_par_10000$mu_75_C1), sd(mix_par_10000$mu_90_C1))
sd_mu[5,c(1,5,9)] <- 1000 * c(sd(mix_par_10000$mu_60_BH_C2), sd(mix_par_10000$mu_75_BH_C2), sd(mix_par_10000$mu_90_BH_C2))

## - PS1
sd_mu[1,c(2,6,10)] <- 1000 * c(sd(sep_mod_gam_par_10000$mu_60_BL), sd(sep_mod_gam_par_10000$mu_75_BL), sd(sep_mod_gam_par_10000$mu_90_BL))
sd_mu[2,c(2,6,10)] <- 1000 * c(sd(sep_mod_gam_par_10000$mu_60_BH), sd(sep_mod_gam_par_10000$mu_75_BH), sd(sep_mod_gam_par_10000$mu_90_BH))
sd_mu[3,c(2,6,10)] <- c(0,0,0)
sd_mu[4,c(2,6,10)] <- c(0,0,0)
sd_mu[5,c(2,6,10)] <- c(0,0,0)

## - PS2
sd_mu[1,c(3,7,11)] <- c(0,0,0)
sd_mu[2,c(3,7,11)] <- c(0,0,0)
sd_mu[3,c(3,7,11)] <- 1000 * c(sd(sep_mod_del_par_10000$mu_60_C0), sd(sep_mod_del_par_10000$mu_75_C0), sd(sep_mod_del_par_10000$mu_90_C0))
sd_mu[4,c(3,7,11)] <- 1000 * c(sd(sep_mod_del_par_10000$mu_60_C1), sd(sep_mod_del_par_10000$mu_75_C1), sd(sep_mod_del_par_10000$mu_90_C1))
sd_mu[5,c(3,7,11)] <- 1000 * c(sd(sep_mod_del_par_10000$mu_60_C2), sd(sep_mod_del_par_10000$mu_75_C2), sd(sep_mod_del_par_10000$mu_90_C2))

## - Model 0
sd_mu[1,c(4,8,12)] <- 1000 * c(sd(M0_10000$mu_60), sd(M0_10000$mu_75), sd(M0_10000$mu_90))
sd_mu[2,c(4,8,12)] <- 1000 * c(sd(M0_10000$mu_60), sd(M0_10000$mu_75), sd(M0_10000$mu_90))
sd_mu[3,c(4,8,12)] <- 1000 * c(sd(M0_10000$mu_60), sd(M0_10000$mu_75), sd(M0_10000$mu_90))
sd_mu[4,c(4,8,12)] <- 1000 * c(sd(M0_10000$mu_60), sd(M0_10000$mu_75), sd(M0_10000$mu_90))
sd_mu[5,c(4,8,12)] <- 1000 * c(sd(M0_10000$mu_60), sd(M0_10000$mu_75), sd(M0_10000$mu_90))

#e_x
## - missing data      
sd_life[1,c(1,5,9)] <- c(sqrt(var(mix_par_10000$e_60_BL) + mean(mix_par_10000$var_e_60_BL)), sqrt(var(mix_par_10000$e_75_BL) + mean(mix_par_10000$var_e_75_BL)), sqrt(var(mix_par_10000$e_90_BL) + mean(mix_par_10000$var_e_90_BL)))
sd_life[2,c(1,5,9)] <- c(sqrt(var(mix_par_10000$e_60_BH_C2) + mean(mix_par_10000$var_e_60_BH_C2)), sqrt(var(mix_par_10000$e_75_BH_C2) + mean(mix_par_10000$var_e_75_BH_C2)), sqrt(var(mix_par_10000$e_90_BH_C2) + mean(mix_par_10000$var_e_90_BH_C2)))
sd_life[3,c(1,5,9)] <- c(sqrt(var(mix_par_10000$e_60_C0) + mean(mix_par_10000$var_e_60_C0)), sqrt(var(mix_par_10000$e_75_C0) + mean(mix_par_10000$var_e_75_C0)), sqrt(var(mix_par_10000$e_90_C0) + mean(mix_par_10000$var_e_90_C0)))
sd_life[4,c(1,5,9)] <- c(sqrt(var(mix_par_10000$e_60_C1) + mean(mix_par_10000$var_e_60_C1)), sqrt(var(mix_par_10000$e_75_C1) + mean(mix_par_10000$var_e_75_C1)), sqrt(var(mix_par_10000$e_90_C1) + mean(mix_par_10000$var_e_90_C1)))
sd_life[5,c(1,5,9)] <- c(sqrt(var(mix_par_10000$e_60_BH_C2) + mean(mix_par_10000$var_e_60_BH_C2)), sqrt(var(mix_par_10000$e_75_BH_C2) + mean(mix_par_10000$var_e_75_BH_C2)), sqrt(var(mix_par_10000$e_90_BH_C2) + mean(mix_par_10000$var_e_90_BH_C2)))

## - PS1
sd_life[1,c(2,6,10)] <- c(sqrt(var(sep_mod_gam_par_10000$e_60_BL) + mean(sep_mod_gam_par_10000$var_e_60_BL)), sqrt(var(sep_mod_gam_par_10000$e_75_BL) + mean(sep_mod_gam_par_10000$var_e_75_BL)), sqrt(var(sep_mod_gam_par_10000$e_90_BL) + mean(sep_mod_gam_par_10000$var_e_90_BL)))
sd_life[2,c(2,6,10)] <- c(sqrt(var(sep_mod_gam_par_10000$e_60_BH) + mean(sep_mod_gam_par_10000$var_e_60_BH)), sqrt(var(sep_mod_gam_par_10000$e_75_BH) + mean(sep_mod_gam_par_10000$var_e_75_BH)), sqrt(var(sep_mod_gam_par_10000$e_90_BH) + mean(sep_mod_gam_par_10000$var_e_90_BH)))
sd_life[3,c(2,6,10)] <- c(0,0,0)
sd_life[4,c(2,6,10)] <- c(0,0,0)
sd_life[5,c(2,6,10)] <- c(0,0,0)

## - PS2
sd_life[1,c(3,7,11)] <- c(0,0,0)
sd_life[2,c(3,7,11)] <- c(0,0,0)
sd_life[3,c(3,7,11)] <- c(sqrt(var(sep_mod_del_par_10000$e_60_C0) + mean(sep_mod_del_par_10000$var_e_60_C0)), sqrt(var(sep_mod_del_par_10000$e_75_C0) + mean(sep_mod_del_par_10000$var_e_75_C0)), sqrt(var(sep_mod_del_par_10000$e_90_C0) + mean(sep_mod_del_par_10000$var_e_90_C0)))
sd_life[4,c(3,7,11)] <- c(sqrt(var(sep_mod_del_par_10000$e_60_C1) + mean(sep_mod_del_par_10000$var_e_60_C1)), sqrt(var(sep_mod_del_par_10000$e_75_C1) + mean(sep_mod_del_par_10000$var_e_75_C1)), sqrt(var(sep_mod_del_par_10000$e_90_C1) + mean(sep_mod_del_par_10000$var_e_90_C1)))
sd_life[5,c(3,7,11)] <- c(sqrt(var(sep_mod_del_par_10000$e_60_C2) + mean(sep_mod_del_par_10000$var_e_60_C2)), sqrt(var(sep_mod_del_par_10000$e_75_C2) + mean(sep_mod_del_par_10000$var_e_75_C2)), sqrt(var(sep_mod_del_par_10000$e_90_C2) + mean(sep_mod_del_par_10000$var_e_90_C2)))

## - Model 0
sd_life[1,c(4,8,12)] <- c(sqrt(var(M0_10000$e_60) + mean(M0_10000$var_e_60)), sqrt(var(M0_10000$e_75) + mean(M0_10000$var_e_75)), sqrt(var(M0_10000$e_90) + mean(M0_10000$var_e_90)))
sd_life[2,c(4,8,12)] <- c(sqrt(var(M0_10000$e_60) + mean(M0_10000$var_e_60)), sqrt(var(M0_10000$e_75) + mean(M0_10000$var_e_75)), sqrt(var(M0_10000$e_90) + mean(M0_10000$var_e_90)))
sd_life[3,c(4,8,12)] <- c(sqrt(var(M0_10000$e_60) + mean(M0_10000$var_e_60)), sqrt(var(M0_10000$e_75) + mean(M0_10000$var_e_75)), sqrt(var(M0_10000$e_90) + mean(M0_10000$var_e_90)))
sd_life[4,c(4,8,12)] <- c(sqrt(var(M0_10000$e_60) + mean(M0_10000$var_e_60)), sqrt(var(M0_10000$e_75) + mean(M0_10000$var_e_75)), sqrt(var(M0_10000$e_90) + mean(M0_10000$var_e_90)))
sd_life[5,c(4,8,12)] <- c(sqrt(var(M0_10000$e_60) + mean(M0_10000$var_e_60)), sqrt(var(M0_10000$e_75) + mean(M0_10000$var_e_75)), sqrt(var(M0_10000$e_90) + mean(M0_10000$var_e_90)))

#e_x - parameter risk

## - missing data
sd_par_life[1,c(1,5,9)] <- c(sd(mix_par_10000$e_60_BL), sd(mix_par_10000$e_75_BL), sd(mix_par_10000$e_90_BL))
sd_par_life[2,c(1,5,9)] <- c(sd(mix_par_10000$e_60_BH_C2), sd(mix_par_10000$e_75_BH_C2), sd(mix_par_10000$e_90_BH_C2))
sd_par_life[3,c(1,5,9)] <- c(sd(mix_par_10000$e_60_C0), sd(mix_par_10000$e_75_C0), sd(mix_par_10000$e_90_C0))
sd_par_life[4,c(1,5,9)] <- c(sd(mix_par_10000$e_60_C1), sd(mix_par_10000$e_75_C1), sd(mix_par_10000$e_90_C1))
sd_par_life[5,c(1,5,9)] <- c(sd(mix_par_10000$e_60_BH_C2), sd(mix_par_10000$e_75_BH_C2), sd(mix_par_10000$e_90_BH_C2))

## - PS1
sd_par_life[1,c(2,6,10)] <- c(sd(sep_mod_gam_par_10000$e_60_BL), sd(sep_mod_gam_par_10000$e_75_BL), sd(sep_mod_gam_par_10000$e_90_BL))
sd_par_life[2,c(2,6,10)] <- c(sd(sep_mod_gam_par_10000$e_60_BH), sd(sep_mod_gam_par_10000$e_75_BH), sd(sep_mod_gam_par_10000$e_90_BH))
sd_par_life[3,c(2,6,10)] <- c(0,0,0)
sd_par_life[4,c(2,6,10)] <- c(0,0,0)
sd_par_life[5,c(2,6,10)] <- c(0,0,0)

## - PS2
sd_par_life[1,c(3,7,11)] <- c(0,0,0)
sd_par_life[2,c(3,7,11)] <- c(0,0,0)
sd_par_life[3,c(3,7,11)] <- c(sd(sep_mod_del_par_10000$e_60_C0), sd(sep_mod_del_par_10000$e_75_C0), sd(sep_mod_del_par_10000$e_90_C0))
sd_par_life[4,c(3,7,11)] <- c(sd(sep_mod_del_par_10000$e_60_C1), sd(sep_mod_del_par_10000$e_75_C1), sd(sep_mod_del_par_10000$e_90_C1))
sd_par_life[5,c(3,7,11)] <- c(sd(sep_mod_del_par_10000$e_60_C2), sd(sep_mod_del_par_10000$e_75_C2), sd(sep_mod_del_par_10000$e_90_C2))

## - Model 0
sd_par_life[1,c(4,8,12)] <- c(sd(M0_10000$e_60), sd(M0_10000$e_75), sd(M0_10000$e_90))
sd_par_life[2,c(4,8,12)] <- c(sd(M0_10000$e_60), sd(M0_10000$e_75), sd(M0_10000$e_90))
sd_par_life[3,c(4,8,12)] <- c(sd(M0_10000$e_60), sd(M0_10000$e_75), sd(M0_10000$e_90))
sd_par_life[4,c(4,8,12)] <- c(sd(M0_10000$e_60), sd(M0_10000$e_75), sd(M0_10000$e_90))
sd_par_life[5,c(4,8,12)] <- c(sd(M0_10000$e_60), sd(M0_10000$e_75), sd(M0_10000$e_90))

#sd e_x


################################## - low 95 bound - ############################################################

#mu_x
## - missing data
lbd_mu[1,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_BL, 0.025), quantile(mix_par_10000$mu_75_BL, 0.025), quantile(mix_par_10000$mu_90_BL, 0.025))
lbd_mu[2,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_BH_C2, 0.025), quantile(mix_par_10000$mu_75_BH_C2, 0.025), quantile(mix_par_10000$mu_90_BH_C2, 0.025))
lbd_mu[3,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_C0, 0.025), quantile(mix_par_10000$mu_75_C0, 0.025), quantile(mix_par_10000$mu_90_C0, 0.025))
lbd_mu[4,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_C1, 0.025), quantile(mix_par_10000$mu_75_C1, 0.025), quantile(mix_par_10000$mu_90_C1, 0.025))
lbd_mu[5,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_BH_C2, 0.025), quantile(mix_par_10000$mu_75_BH_C2, 0.025), quantile(mix_par_10000$mu_90_BH_C2, 0.025))

## - PS1
lbd_mu[1,c(2,6,10)] <- 100 * c(quantile(sep_mod_gam_par_10000$mu_60_BL, 0.025), quantile(sep_mod_gam_par_10000$mu_75_BL, 0.025), quantile(sep_mod_gam_par_10000$mu_90_BL, 0.025))
lbd_mu[2,c(2,6,10)] <- 100 * c(quantile(sep_mod_gam_par_10000$mu_60_BH, 0.025), quantile(sep_mod_gam_par_10000$mu_75_BH, 0.025), quantile(sep_mod_gam_par_10000$mu_90_BH, 0.025))
lbd_mu[3,c(2,6,10)] <- c(0,0,0)
lbd_mu[4,c(2,6,10)] <- c(0,0,0)
lbd_mu[5,c(2,6,10)] <- c(0,0,0)

## - PS2
lbd_mu[1,c(3,7,11)] <- c(0,0,0)
lbd_mu[2,c(3,7,11)] <- c(0,0,0)
lbd_mu[3,c(3,7,11)] <- 100 * c(quantile(sep_mod_del_par_10000$mu_60_C0, 0.025), quantile(sep_mod_del_par_10000$mu_75_C0, 0.025), quantile(sep_mod_del_par_10000$mu_90_C0, 0.025))
lbd_mu[4,c(3,7,11)] <- 100 * c(quantile(sep_mod_del_par_10000$mu_60_C1, 0.025), quantile(sep_mod_del_par_10000$mu_75_C1, 0.025), quantile(sep_mod_del_par_10000$mu_90_C1, 0.025))
lbd_mu[5,c(3,7,11)] <- 100 * c(quantile(sep_mod_del_par_10000$mu_60_C2, 0.025), quantile(sep_mod_del_par_10000$mu_75_C2, 0.025), quantile(sep_mod_del_par_10000$mu_90_C2, 0.025))

## - Model 0
lbd_mu[1,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.025), quantile(M0_10000$mu_75, 0.025), quantile(M0_10000$mu_90, 0.025))
lbd_mu[2,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.025), quantile(M0_10000$mu_75, 0.025), quantile(M0_10000$mu_90, 0.025))
lbd_mu[3,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.025), quantile(M0_10000$mu_75, 0.025), quantile(M0_10000$mu_90, 0.025))
lbd_mu[4,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.025), quantile(M0_10000$mu_75, 0.025), quantile(M0_10000$mu_90, 0.025))
lbd_mu[5,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.025), quantile(M0_10000$mu_75, 0.025), quantile(M0_10000$mu_90, 0.025))

#e_x
lbd_life <- exp_life - 1.96 * sd_life


################################## - upper 95 bound - ############################################################

#mu_x
## - missing data
ubd_mu[1,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_BL, 0.975), quantile(mix_par_10000$mu_75_BL, 0.975), quantile(mix_par_10000$mu_90_BL, 0.975))
ubd_mu[2,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_BH_C2, 0.975), quantile(mix_par_10000$mu_75_BH_C2, 0.975), quantile(mix_par_10000$mu_90_BH_C2, 0.975))
ubd_mu[3,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_C0, 0.975), quantile(mix_par_10000$mu_75_C0, 0.975), quantile(mix_par_10000$mu_90_C0, 0.975))
ubd_mu[4,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_C1, 0.975), quantile(mix_par_10000$mu_75_C1, 0.975), quantile(mix_par_10000$mu_90_C1, 0.975))
ubd_mu[5,c(1,5,9)] <- 100 * c(quantile(mix_par_10000$mu_60_BH_C2, 0.975), quantile(mix_par_10000$mu_75_BH_C2, 0.975), quantile(mix_par_10000$mu_90_BH_C2, 0.975))

## - PS1
ubd_mu[1,c(2,6,10)] <- 100 * c(quantile(sep_mod_gam_par_10000$mu_60_BL, 0.975), quantile(sep_mod_gam_par_10000$mu_75_BL, 0.975), quantile(sep_mod_gam_par_10000$mu_90_BL, 0.975))
ubd_mu[2,c(2,6,10)] <- 100 * c(quantile(sep_mod_gam_par_10000$mu_60_BH, 0.975), quantile(sep_mod_gam_par_10000$mu_75_BH, 0.975), quantile(sep_mod_gam_par_10000$mu_90_BH, 0.975))
ubd_mu[3,c(2,6,10)] <- c(0,0,0)
ubd_mu[4,c(2,6,10)] <- c(0,0,0)
ubd_mu[5,c(2,6,10)] <- c(0,0,0)

## - PS2
ubd_mu[1,c(3,7,11)] <- c(0,0,0)
ubd_mu[2,c(3,7,11)] <- c(0,0,0)
ubd_mu[3,c(3,7,11)] <- 100 * c(quantile(sep_mod_del_par_10000$mu_60_C0, 0.975), quantile(sep_mod_del_par_10000$mu_75_C0, 0.975), quantile(sep_mod_del_par_10000$mu_90_C0, 0.975))
ubd_mu[4,c(3,7,11)] <- 100 * c(quantile(sep_mod_del_par_10000$mu_60_C1, 0.975), quantile(sep_mod_del_par_10000$mu_75_C1, 0.975), quantile(sep_mod_del_par_10000$mu_90_C1, 0.975))
ubd_mu[5,c(3,7,11)] <- 100 * c(quantile(sep_mod_del_par_10000$mu_60_C2, 0.975), quantile(sep_mod_del_par_10000$mu_75_C2, 0.975), quantile(sep_mod_del_par_10000$mu_90_C2, 0.975))

## - Model 0
ubd_mu[1,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.975), quantile(M0_10000$mu_75, 0.975), quantile(M0_10000$mu_90, 0.975))
ubd_mu[2,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.975), quantile(M0_10000$mu_75, 0.975), quantile(M0_10000$mu_90, 0.975))
ubd_mu[3,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.975), quantile(M0_10000$mu_75, 0.975), quantile(M0_10000$mu_90, 0.975))
ubd_mu[4,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.975), quantile(M0_10000$mu_75, 0.975), quantile(M0_10000$mu_90, 0.975))
ubd_mu[5,c(4,8,12)] <- 100 * c(quantile(M0_10000$mu_60, 0.975), quantile(M0_10000$mu_75, 0.975), quantile(M0_10000$mu_90, 0.975))

#e_x
ubd_life <- exp_life + 1.96 * sd_life

xtable(exp_mu)

exp(theta_est_Red40$estimate[1] + 60 * theta_est_Red40$estimate[2] + theta_est_Red40$estimate[3])
exp(theta_est_DS1$estimate[1] + 60 * theta_est_DS1$estimate[2])

xtable(sd_mu)

xtable(exp_life)
xtable(sd_life)
xtable(sd_par_life)

######################################################################################################################################

################################################# - Statistical Tests of fitting - ###################################################

######################################################################################################################################


######################################################### - Cox-Miller residuals - ###################################################

# - Crude rates whole population

a <- theta_est_Red40$estimate[1]
b <- theta_est_Red40$estimate[2]
c1 <- theta_est_Red40$estimate[3]
c2 <- theta_est_Red40$estimate[4]
c3 <- theta_est_Red40$estimate[5]
x1 <- DS1$EntAge

# Deaths count
deaths <- rep(0, 45)

for (i in 1:nrow(DS_C)){
  deaths[floor(DS_C$EntAge[i]+DS_C$TObs[i])-59] <- deaths[floor(DS_C$EntAge[i]+DS_C$TObs[i])-59] + DS_C$Status[i]
}

#Age splitting algorithm

nds1 <- matrix(NA, 100000, 9) #new dataset creation
nds1 <-as.data.frame(nds1) #set as dataframe
colnames(nds1)<-c("AgeLastB", "EntAge", "TObs", "Status", "BL", "BH", "C0", "C1", "C2") #Set column names

c<-0

for (ind in 1:nrow(DS1)){
  c<-c+1
  
  #EntryAge
  nds1[c, 1] <- floor(DS1$EntAge[ind])   #since taking the age last birthday - take the floor of the entry age
  nds1[c, 2] <- DS1$EntAge[ind] #Entry age for this entry of the table
  nds1[c, 3] <- min((floor(DS1$EntAge[ind]+1)-DS1$EntAge[ind]), DS1$TObs[ind]) #taking the minimum between the (difference 
  #between the next round age and the entry age) 
  #and the time observed: it means that if I observed an 
  #individual just for only the age 1st birtday I take just 
  #the time observed
  #nds[c, 3]<-SurvIn[ind, 3] #time observed up to the age at entry, i.e. duration
  nds1[c, 4]<-ifelse(floor(DS1$EntAge[ind])!=floor(DS1$EntAge[ind]+DS1$TObs[ind]), 0, DS1$Status[ind]) #Status dead or alive
  nds1[c,5:9] <- DS1[ind, 8:12]
  
  #DropAge
  if (floor(DS1$EntAge[ind])!=floor(DS1$EntAge[ind]+DS1$TObs[ind])){ #if the individual while into observation experiences 
    #any further birthday
    c<-c+1
    nds1[c, 1]<-floor(DS1$EntAge[ind]+DS1$TObs[ind]) #here we are handling the last birthday age, hence we take the floor of the 
    #summation between the entry age and the time observed
    nds1[c, 2]<- nds1[c, 1]
    nds1[c, 3]<-DS1$EntAge[ind]+DS1$TObs[ind]-nds1[c, 1]} #the time observed in that age is given by the entry age + time observed - the
    #floor of the drop age
    #nds[c, 3]<-SurvIn[ind, 3]+SurvIn[ind, 5]-nds[c, 2]} #the duration at the last age last birthday is given by: entry duration +
  #time observed - the time observed into last age last birthday, so to have an
  #entryduration  at exact the last age last birthday
  nds1[c, 4]<-DS1$Status[ind] #status dead or alive
  nds1[c, 5:9]<-DS1[ind, 8:12] #Characteristics
  
  i<-floor(DS1$EntAge[ind]+1) #the age after the first age last birthday in case we observe an individual for more years
  repeat{
    if (i>=floor(DS1$EntAge[ind]+DS1$TObs[ind])){break} #the loop interrupts once we get the last age last birthday
    c<-c+1
    nds1[c, 1]<-i #observed age
    nds1[c, 2] <- nds1[c, 1] # entry age
    nds1[c, 3]<-1 #time observed into that age
    #nds[c, 3]<-SurvIn[ind, 3]+i-SurvIn[ind, 2] #the duration is given by: time observed + attained last birthday age - entry age
    nds1[c, 4]<-0
    nds1[c, 5:9] <- DS1[ind, 8:12] #Characteristics
    i<-i+1
  }
}
nds1<-na.omit(nds1) #erase not available row data

nds1$IntHaz <- ((exp(b * nds1$TObs) - 1) / b) * exp(a + b * nds1$EntAge + c1 * nds1$BL + c3 * nds1$BH)



nds2 <- matrix(NA, 100000, 9) #new dataset creation
nds2 <-as.data.frame(nds2) #set as dataframe
colnames(nds2)<-c("AgeLastB", "EntAge", "TObs", "Status", "BL", "BH", "C0", "C1", "C2") #Set column names

c<-0

for (ind in 1:nrow(DS2)){
  c<-c+1
  
  #EntryAge
  nds2[c, 1] <- floor(DS2$EntAge[ind])   #since taking the age last birthday - take the floor of the entry age
  nds2[c, 2] <- DS2$EntAge[ind] #Entry age for this entry of the table
  nds2[c, 3] <- min((floor(DS2$EntAge[ind]+1)-DS2$EntAge[ind]), DS2$TObs[ind]) #taking the minimum between the (difference 
  #between the next round age and the entry age) 
  #and the time observed: it means that if I observed an 
  #individual just for only the age 1st birtday I take just 
  #the time observed
  #nds[c, 3]<-SurvIn[ind, 3] #time observed up to the age at entry, i.e. duration
  nds2[c, 4]<-ifelse(floor(DS2$EntAge[ind])!=floor(DS2$EntAge[ind]+DS2$TObs[ind]), 0, DS2$Status[ind]) #Status dead or alive
  nds2[c,5:9] <- DS2[ind, 8:12]
  
  #DropAge
  if (floor(DS2$EntAge[ind])!=floor(DS2$EntAge[ind]+DS2$TObs[ind])){ #if the individual while into observation experiences 
    #any further birthday
    c<-c+1
    nds2[c, 1]<-floor(DS2$EntAge[ind]+DS2$TObs[ind]) #here we are handling the last birthday age, hence we take the floor of the 
    #summation between the entry age and the time observed
    nds2[c, 2]<- nds2[c, 1]
    nds2[c, 3]<-DS2$EntAge[ind]+DS2$TObs[ind]-nds2[c, 1]} #the time observed in that age is given by the entry age + time observed - the
  #floor of the drop age
  #nds[c, 3]<-SurvIn[ind, 3]+SurvIn[ind, 5]-nds[c, 2]} #the duration at the last age last birthday is given by: entry duration +
  #time observed - the time observed into last age last birthday, so to have an
  #entryduration  at exact the last age last birthday
  nds2[c, 4]<-DS2$Status[ind] #status dead or alive
  nds2[c, 5:9]<-DS2[ind, 8:12] #Characteristics
  
  i<-floor(DS2$EntAge[ind]+1) #the age after the first age last birthday in case we observe an individual for more years
  repeat{
    if (i>=floor(DS2$EntAge[ind]+DS2$TObs[ind])){break} #the loop interrupts once we get the last age last birthday
    c<-c+1
    nds2[c, 1]<-i #observed age
    nds2[c, 2] <- nds2[c, 1] # entry age
    nds2[c, 3]<-1 #time observed into that age
    #nds[c, 3]<-SurvIn[ind, 3]+i-SurvIn[ind, 2] #the duration is given by: time observed + attained last birthday age - entry age
    nds2[c, 4]<-0
    nds2[c, 5:9] <- DS2[ind, 8:12] #Characteristics
    i<-i+1
  }
}
nds2<-na.omit(nds2) #erase not available row data

nds2$IntHaz <- ((exp(b * nds2$TObs) - 1) / b) * exp(a + b * nds2$EntAge + c2 * nds2$C1 + c3 * nds2$C2)


nds <- rbind(nds1, nds2)


# Summing hazard functions by age
Poi_par_CM <- rep(0, 45)

for (i in 1:nrow(nds)){
  Poi_par_CM[nds[i,1]-59] <- Poi_par_CM[nds[i,1]-59] + nds$IntHaz[i]
}

# - Costruction of Poisson Deviance Residuals

Poi_dev_res <- rep(0, 44)
Poi_dev_res <- sign(deaths - Poi_par_CM) * sqrt(2 * (deaths * log(deaths/Poi_par_CM) - (deaths - Poi_par_CM)))
plot(c(60:103), Poi_dev_res, pch=16, main="Poisson Deviance Residuals by age", xlab = "Age", ylab="Deviance Residuals", ylim=c(-3,3))
abline(h=c(-2,2), col="black")

Poi_dev_res <- Poi_dev_res[!is.na(Poi_dev_res)]

# - Chi square test
chi_sq_stat <- sum(Poi_dev_res^2)
pchisq(chi_sq_stat, df=length(Poi_dev_res), lower.tail=FALSE)

# - Critical value
qchisq(0.95, length(Poi_dev_res))

## - Standardized deviations test
SDT_bands <- rep(0,8)
for (i in 1:length(Poi_dev_res)){
  SDT_bands[floor(Poi_dev_res[i]+5)] <- SDT_bands[floor(Poi_dev_res[i]+5)] + 1
}

Exp_SDT <- length(Poi_dev_res) * c(0.02, 0.14, 0.34, 0.34, 0.14, 0.02)
SDT_bands <- SDT_bands[2:7]
#Calculation of the test statistic
SD_TS <- sum(((SDT_bands - Exp_SDT)^2)/Exp_SDT)
pchisq(SD_TS, df=7, lower.tail=FALSE)


#Signs Test
positive_dev <- sum(ifelse(sign(Poi_dev_res)==1,1,0), na.rm = TRUE)
2 * pbinom(positive_dev, length(Poi_dev_res), 0.5, lower.tail=TRUE)

#Cumulative deviations
CDT_TS <- sum(deaths - Poi_par_CM) / sqrt(sum(Poi_par_CM))
2 * pnorm(CDT_TS, 0, 1, lower.tail = FALSE)



#Grouping of signs test

GoST=function(z){
  Signs<-sign(z)
  
  g_pos_c <- ifelse(Signs==1,1,0)
  g_neg_c <- ifelse(Signs==-1,1,0)
  
  for (i in 2:(length(Poi_dev_res)-1)){
    g_pos_c <- ifelse( Signs[i]==1 & Signs[i]!=Signs[i-1], g_pos_c+1, g_pos_c)
    g_neg_c <- ifelse(Signs[i]==-1 & Signs[i]!=Signs[i-1], g_neg_c+1, g_neg_c)
  }
  
  tot_groups <- g_pos_c + g_neg_c
  st_perc<-0
  t<-0
  repeat{
    if (st_perc>=0.05){break}
    t<-t+1
    st_perc<-st_perc+(factorial(g_pos_c-1)/(factorial(t-1)*factorial(g_pos_c-t)))*(factorial(g_neg_c+1)/(factorial(t)*factorial(g_neg_c+1-t)))/(factorial(tot_groups)/(factorial(g_pos_c)*factorial(g_neg_c)))
  }
  #pvalue calculation of the Steven's Test to be added
  return(list(t, g_pos_c))
}

GoST(deaths-Poi_par_CM)

##Serial Correlations Test
SerCorT=function(z, Age_Groups){
  z_avg <- mean(z)
  num1 <- 0
  den1<-z[1]^2
  for (i in 1:(Age_Groups-1)){
    num1<-num1+((z[i]-z_avg)*(z[i+1]-z_avg))
    den1<-den1+(z[i+1]-z_avg)^2
  }
  r1<-num1/(((Age_Groups-1)/Age_Groups)*den1)
  onetail_pvalue<-pnorm((r1*sqrt(Age_Groups)), 0, 1, lower.tail=FALSE)
  return(list(r1*sqrt(Age_Groups),onetail_pvalue))
}

SerCorT_m1<-SerCorT(Poi_dev_res, length(Poi_dev_res))
SerCorT_m1

##########################################################################################################################

################################### - Standardized deviations test for the whole sample - ################################

##########################################################################################################################

######################################################### - Cox-Miller residuals - ###################################################

# - Crude rates whole population

a_par <- theta_est_BMK2$estimate[1]
b_par <- theta_est_BMK2$estimate[2]
c_par <- theta_est_BMK2$estimate[3]
de1_par <- theta_est_BMK2$estimate[4]
de2_par <- theta_est_BMK2$estimate[5]
x <- DS_C$EntAge

# Deaths count
deaths <- rep(0, 44)

for (i in 1:nrow(DS_C)){
  deaths[floor(DS_C$EntAge[i]+DS_C$TObs[i])-59] <- deaths[floor(DS_C$EntAge[i]+DS_C$TObs[i])-59] + DS_C$Status[i]
}

#Age splitting algorithm

nds <- matrix(NA, 200000, 9) #new dataset creation
nds <-as.data.frame(nds) #set as dataframe
colnames(nds)<-c("AgeLastB", "EntAge", "TObs", "Status", "BL", "BH", "C0", "C1", "C2") #Set column names

c<-0

for (ind in 1:nrow(DS_C)){
  c<-c+1
  
  #EntryAge
  nds[c, 1] <- floor(DS_C$EntAge[ind])   #since taking the age last birthday - take the floor of the entry age
  nds[c, 2] <- DS_C$EntAge[ind] #Entry age for this entry of the table
  nds[c, 3] <- min((floor(DS_C$EntAge[ind]+1)-DS_C$EntAge[ind]), DS_C$TObs[ind]) #taking the minimum between the (difference 
  #between the next round age and the entry age) 
  #and the time observed: it means that if I observed an 
  #individual just for only the age 1st birtday I take just 
  #the time observed
  #nds[c, 3]<-SurvIn[ind, 3] #time observed up to the age at entry, i.e. duration
  nds[c, 4]<-ifelse(floor(DS_C$EntAge[ind])!=floor(DS_C$EntAge[ind]+DS_C$TObs[ind]), 0, DS_C$Status[ind]) #Status dead or alive
  nds[c,5:9] <- DS_C[ind, 8:12]
  
  #DropAge
  if (floor(DS_C$EntAge[ind])!=floor(DS_C$EntAge[ind]+DS_C$TObs[ind])){ #if the individual while into observation experiences 
    #any further birthday
    c<-c+1
    nds[c, 1]<-floor(DS_C$EntAge[ind]+DS_C$TObs[ind]) #here we are handling the last birthday age, hence we take the floor of the 
    #summation between the entry age and the time observed
    nds[c, 2]<- nds[c, 1]
    nds[c, 3]<-DS_C$EntAge[ind]+DS_C$TObs[ind]-nds[c, 1]} #the time observed in that age is given by the entry age + time observed - the
  #floor of the drop age
  #nds[c, 3]<-SurvIn[ind, 3]+SurvIn[ind, 5]-nds[c, 2]} #the duration at the last age last birthday is given by: entry duration +
  #time observed - the time observed into last age last birthday, so to have an
  #entryduration  at exact the last age last birthday
  nds[c, 4]<-DS_C$Status[ind] #status dead or alive
  nds[c, 5:9]<-DS_C[ind, 8:12] #Characteristics
  
  i<-floor(DS_C$EntAge[ind]+1) #the age after the first age last birthday in case we observe an individual for more years
  repeat{
    if (i>=floor(DS_C$EntAge[ind]+DS_C$TObs[ind])){break} #the loop interrupts once we get the last age last birthday
    c<-c+1
    nds[c, 1]<-i #observed age
    nds[c, 2] <- nds[c, 1] # entry age
    nds[c, 3]<-1 #time observed into that age
    #nds[c, 3]<-SurvIn[ind, 3]+i-SurvIn[ind, 2] #the duration is given by: time observed + attained last birthday age - entry age
    nds[c, 4]<-0
    nds[c, 5:9] <- DS_C[ind, 8:12] #Characteristics
    i<-i+1
  }
}
nds<-na.omit(nds) #erase not available row data

nds$IntHaz <- ((exp(b_par * nds$TObs) - 1) / b_par) * exp(a_par + b_par * nds$EntAge + c_par * nds$BH + de1_par * nds$C1 + de2_par * nds$C2)
# - Model M0
nds$IntHaz <- ((exp(Model0$estimate[2] * nds$TObs) - 1) / Model0$estimate[2]) * exp(Model0$estimate[1] + Model0$estimate[2] * nds$EntAge)


# Summing hazard functions by age
Poi_par_CM <- rep(0, 44)

for (i in 1:nrow(nds)){
  Poi_par_CM[nds[i,1]-59] <- Poi_par_CM[nds[i,1]-59] + nds$IntHaz[i]
}

# - Costruction of Poisson Deviance Residuals

Poi_dev_res <- rep(0, 44)
Poi_dev_res <- sign(deaths - Poi_par_CM) * sqrt(2 * (deaths * log(deaths/Poi_par_CM) - (deaths - Poi_par_CM)))
plot(c(60:103), Poi_dev_res, pch=16, cex=0.75, xlab = "Age", ylab="Deviance Residuals", ylim=c(-3,3)) #, main="Poisson Deviance Residuals by age"
abline(h=c(-2,2), col="black")
abline(h=0, col="black", lty=2)

########################################### - Poisson Deviance Residual by socio-economic factor - ################################################

nds_ben_l <- nds[nds$BL==1,]

# Summing hazard functions by age
Poi_par_CM_ben_l <- rep(0, 44)

for (i in 1:nrow(nds_ben_l)){
  Poi_par_CM_ben_l[nds_ben_l[i,1]-59] <- Poi_par_CM_ben_l[nds_ben_l[i,1]-59] + nds_ben_l$IntHaz[i]
}

# - Costruction of Poisson Deviance Residuals

Poi_dev_res_ben_l <- rep(0, 44)
Poi_dev_res_ben_l <- sign(deaths_ben_l - Poi_par_CM_ben_l) * sqrt(2 * (deaths_ben_l * log(deaths_ben_l/Poi_par_CM_ben_l) - (deaths_ben_l - Poi_par_CM_ben_l)))

nds_ben_h <- nds[nds$BH==1,]

# Summing hazard functions by age
Poi_par_CM_ben_h <- rep(0, 44)

for (i in 1:nrow(nds_ben_h)){
  Poi_par_CM_ben_h[nds_ben_h[i,1]-59] <- Poi_par_CM_ben_h[nds_ben_h[i,1]-59] + nds_ben_h$IntHaz[i]
}

# - Costruction of Poisson Deviance Residuals

Poi_dev_res_ben_h <- rep(0, 45)
Poi_dev_res_ben_h <- sign(deaths_ben_h - Poi_par_CM_ben_h) * sqrt(2 * (deaths_ben_h * log(deaths_ben_h/Poi_par_CM_ben_h) - (deaths_ben_h - Poi_par_CM_ben_h)))


## - Poisson Deviance Residuals by benefit level
plot(c(60:103), Poi_dev_res_ben_l, pch=1, cex=0.75, xlab="Age", ylab="Deviance Residuals", ylim=c(-3,3))  # , col="blue", main="Log-death rates by benefit level"
points(c(60:103), Poi_dev_res_ben_h, pch=16, cex=0.75) # , col="red"
abline(h=c(-2,2), col="black")
abline(h=0, col="black", lty=2)
legend("topright", legend=c("Low benefit", "High benefit"), col=c("black", "black"), pch=c(1,16), cex=0.75, bty='n') #pch=20, 

###########################################################################################################################################
nds_geo_0 <- nds[nds$C0==1,]

# Summing hazard functions by age
Poi_par_CM_geo_0 <- rep(0, 44)

for (i in 1:nrow(nds_geo_0)){
  Poi_par_CM_geo_0[nds_geo_0[i,1]-59] <- Poi_par_CM_geo_0[nds_geo_0[i,1]-59] + nds_geo_0$IntHaz[i]
}

# - Costruction of Poisson Deviance Residuals

Poi_dev_res_geo_0 <- rep(0, 44)
Poi_dev_res_geo_0 <- sign(deaths_geo_0 - Poi_par_CM_geo_0) * sqrt(2 * (deaths_geo_0 * log(deaths_geo_0/Poi_par_CM_geo_0) - (deaths_geo_0 - Poi_par_CM_geo_0)))

nds_geo_1 <- nds[nds$C1==1,]

# Summing hazard functions by age
Poi_par_CM_geo_1 <- rep(0, 44)

for (i in 1:nrow(nds_geo_1)){
  Poi_par_CM_geo_1[nds_geo_1[i,1]-59] <- Poi_par_CM_geo_1[nds_geo_1[i,1]-59] + nds_geo_1$IntHaz[i]
}

# - Costruction of Poisson Deviance Residuals

Poi_dev_res_geo_1 <- rep(0, 44)
Poi_dev_res_geo_1 <- sign(deaths_geo_1 - Poi_par_CM_geo_1) * sqrt(2 * (deaths_geo_1 * log(deaths_geo_1/Poi_par_CM_geo_1) - (deaths_geo_1 - Poi_par_CM_geo_1)))

nds_geo_2 <- nds[nds$C2==1,]

# Summing hazard functions by age
Poi_par_CM_geo_2 <- rep(0, 44)

for (i in 1:nrow(nds_geo_2)){
  Poi_par_CM_geo_2[nds_geo_2[i,1]-59] <- Poi_par_CM_geo_2[nds_geo_2[i,1]-59] + nds_geo_2$IntHaz[i]
}

# - Costruction of Poisson Deviance Residuals

Poi_dev_res_geo_2 <- rep(0, 44)
Poi_dev_res_geo_2 <- sign(deaths_geo_2 - Poi_par_CM_geo_2) * sqrt(2 * (deaths_geo_2 * log(deaths_geo_2/Poi_par_CM_geo_2) - (deaths_geo_2 - Poi_par_CM_geo_2)))

plot(c(60:103), Poi_dev_res_geo_0, pch=1, cex=0.75, xlab="Age", ylab="Deviance Residuals", ylim=c(-3,3)) # , main="Log-death rates by geo-demoraphic level" , col="blue"
points(c(60:103), Poi_dev_res_geo_1, pch=4, cex=0.5) # , col="red"
points(c(60:103), Poi_dev_res_geo_2, pch=16, cex=0.75) # , col="seagreen"
abline(h=c(-2,2), col="black")
abline(h=0, col="black", lty=2)
legend("topright", legend=c("Geo-demographic lev. 0", "Geo-demographic lev. 1", "Geo-demographic lev. 2"), col=c("black", "black", "black"), pch=c(1,4,16), cex=0.5, bty='n')

###################################################################################################################################################

Poi_dev_res <- Poi_dev_res[!is.na(Poi_dev_res)]

# - Chi square test
chi_sq_stat <- sum(Poi_dev_res^2)
pchisq(chi_sq_stat, df=length(Poi_dev_res), lower.tail=FALSE)

# - Critical value
qchisq(0.95, length(Poi_dev_res))

## - Standardized deviations test
SDT_bands <- rep(0,8)
for (i in 1:length(Poi_dev_res)){
  SDT_bands[floor(Poi_dev_res[i]+5)] <- SDT_bands[floor(Poi_dev_res[i]+5)] + 1
}

Exp_SDT <- length(Poi_dev_res) * c(0.02, 0.14, 0.34, 0.34, 0.14, 0.02)
SDT_bands <- SDT_bands[2:7]
#Calculation of the test statistic
SD_TS <- sum(((SDT_bands - Exp_SDT)^2)/Exp_SDT)
pchisq(SD_TS, df=7, lower.tail=FALSE)


#Signs Test
positive_dev <- sum(ifelse(sign(Poi_dev_res)==1,1,0), na.rm = TRUE)
2 * pbinom(positive_dev, length(Poi_dev_res), 0.5, lower.tail=TRUE)

#Cumulative deviations
CDT_TS <- sum(deaths - Poi_par_CM) / sqrt(sum(Poi_par_CM))
2 * pnorm(CDT_TS, 0, 1, lower.tail = FALSE)



#Grouping of signs test

GoST=function(z){
  Signs<-sign(z)
  
  g_pos_c <- ifelse(Signs==1,1,0)
  g_neg_c <- ifelse(Signs==-1,1,0)
  
  for(i in 2:(length(Poi_dev_res)-1)){
    g_pos_c <- ifelse( Signs[i]==1 & Signs[i]!=Signs[i-1], g_pos_c+1, g_pos_c)
    g_neg_c <- ifelse(Signs[i]==-1 & Signs[i]!=Signs[i-1], g_neg_c+1, g_neg_c)
  }
  
  tot_groups <- g_pos_c + g_neg_c
  st_perc<-0
  t<-0
  repeat{
    if (st_perc>=0.05){break}
    t<-t+1
    st_perc<-st_perc+(factorial(g_pos_c-1)/(factorial(t-1)*factorial(g_pos_c-t)))*(factorial(g_neg_c+1)/(factorial(t)*factorial(g_neg_c+1-t)))/(factorial(tot_groups)/(factorial(g_pos_c)*factorial(g_neg_c)))
  }
  #pvalue calculation of the Steven's Test to be added
  return(list(t, g_pos_c))
}

GoST(deaths-Poi_par_CM)

##Serial Correlations Test
SerCorT=function(z, Age_Groups){
  z_avg <- mean(z)
  num1 <- 0
  den1<-z[1]^2
  for (i in 1:(Age_Groups-1)){
    num1<-num1+((z[i]-z_avg)*(z[i+1]-z_avg))
    den1<-den1+(z[i+1]-z_avg)^2
  }
  r1<-num1/(((Age_Groups-1)/Age_Groups)*den1)
  onetail_pvalue<-pnorm((r1*sqrt(Age_Groups)), 0, 1, lower.tail=FALSE)
  return(list(r1*sqrt(Age_Groups),onetail_pvalue))
}

SerCorT_m1<-SerCorT(Poi_dev_res, length(Poi_dev_res))
SerCorT_m1






########################################################################################################################

##################################### - Misestimation risk capital requirement - #######################################

########################################################################################################################

## P_1-P_4

# - Joint dataset modelling

NegLogL_DS1=function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  ben1 = DS1$BH
  
  gde1 = DS1$C1
  gde2 = DS1$C2
  
  bl_ind = DS1$BL
  bh_ind = DS1$BH
  
  c0_ind = DS1$C0
  c1_ind = DS1$C1
  c2_ind = DS1$C2
  
  a = vdParameters[1] #alpha
  b = vdParameters[2] #beta
  c = vdParameters[3] #delta1
  de1 = vdParameters[4] #delta2
  de2 = vdParameters[5]
  z1 = vdParameters[6]
  z2 = vdParameters[7]
  z3 = vdParameters[8]
  z4 = vdParameters[9]
  z5 = vdParameters[10]
  
  # - Gompertz
  logL_C <-  -exp(a + b * x1 + c * ben1 + de1 * gde1 + de2 * gde2) * (exp(b * t1) - 1)/b + d1 * (a + b * (x1 + t1) + c * ben1 + de1 * gde1 + de2 * gde2) +
    bl_ind * c0_ind * log(z1) + bh_ind * c0_ind * log(z2) + bl_ind * c1_ind * log(z3) + bh_ind * c1_ind * log(z4) + bl_ind * c2_ind * log(z5) + bh_ind * c2_ind * log(1 - z1 - z2 - z3 - z4 - z5)

  return(-(sum(logL_C)))
}


st_val2 <- c(-10, 0.1, 0, 0, 0, 0.2, 0.025, 0.42, 0.12, 0.11)
# - Benchmark
theta_est_BMK2_2 <- nlm(NegLogL_DS1, p=st_val2, typsize=st_val2, hessian=T, iterlim=10000) #,gradtol = 0.0000001, steptol = 0.00000001

P1_varcov <- solve(theta_est_DS1$hessian)


z <- matrix(0, 5, 1)

A <- chol(P1_varcov)
A <- t(A) # getting the lower-triangular matrix

theta_mle_DS1 <- theta_est_DS1$estimate

# - Annuity portfolio valuation
## - DS1

x1 <- DS1$EntAge
bl1 <- DS1$BL
bh1 <- DS1$BH
c0 <- DS1$C0
c1 <- DS1$C1
c2 <- DS1$C2
ben_amt <- DS1$Benefit

portfolio1_val <- matrix(NA, 10000, 2)
ann <- matrix(NA, nrow(DS1),1)

for (i in 1:10000){
  z[,1] <- rnorm(5)
  theta_sim <- theta_mle_DS1 + A %*% z
  a_sim <- theta_sim[1]
  b_sim <- theta_sim[2]
  c_sim <- theta_sim[3]
  de1_sim <- theta_sim[4]
  de2_sim <- theta_sim[5]
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.01) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)) # + a_sim + b_sim * (x1[j] + t) + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)
    }
    annuity = integrate(integrand, lower=0, upper=(120-x1[j]))
    ann[j,1] <- annuity$value 
    
  }
  portfolio1_val[i,1] <- sum(ann[,1]) 
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.03) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)) # + a_sim + b_sim * (x1[j] + t) + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)
    }
    annuity = integrate(integrand, lower=0, upper=(120-x1[j]))
    ann[j,1] <- annuity$value 
  }
  portfolio1_val[i,2] <- sum(ann[,1]) 
}  

# - Joint dataset modelling

# - DS1 

z <- matrix(0, 10, 1)

A1 <- chol(solve(theta_est$hessian))#chol(cov(bootstrap_par_est))
A1 <- t(A1) # getting the lower-triangular matrix

theta_mle_4DS <- theta_est$estimate

# - Annuity portfolio valuation
## - DS1

portfolio1_4DS <- matrix(NA, 10000, 2)
ann <- matrix(NA, nrow(DS1),1)

for (i in 1:10000){
  z[,1] <- rnorm(10)
  theta_sim <- theta_mle_4DS + A1 %*% z
  a_sim <- theta_sim[1]
  b_sim <- theta_sim[2]
  c_sim <- theta_sim[3]
  de1_sim <- theta_sim[4]
  de2_sim <- theta_sim[5]
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.01) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)) # + a_sim + b_sim * (x1[j] + t) + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)
    }
    annuity = integrate(integrand, lower=0, upper=(120-x1[j]))
    ann[j,1] <- annuity$value 
    
  }
  portfolio1_4DS[i,1] <- sum(ann[,1]) 
  
  for(j in 1:nrow(DS1)){
    integrand <- function(t){
      DS1$Benefit[j] * exp(-log(1.03) * t -  ((exp(b_sim * t) - 1)/b_sim) * exp(a_sim + b_sim * x1[j] + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)) # + a_sim + b_sim * (x1[j] + t) + bh1[j] * c_sim + c1[j] * de1_sim + c2[j] * de2_sim)
    }
    annuity = integrate(integrand, lower=0, upper=(120-x1[j]))
    ann[j,1] <- annuity$value 
  }
  portfolio1_4DS[i,2] <- sum(ann[,1]) 
}

# - Misestimation cap requirement
c_req_1 <- quantile(portfolio1_val[,1], 0.995)/mean(portfolio1_val[,1])-1
c_req_3 <- quantile(portfolio1_val[,2], 0.995)/mean(portfolio1_val[,2])-1

c_req_1_4DS <- quantile(portfolio1_4DS[,1], 0.995)/mean(portfolio1_4DS[,1])-1
c_req_3_4DS <- quantile(portfolio1_4DS[,2], 0.995)/mean(portfolio1_4DS[,2])-1

##########
# - Histograms
hist(portfolio1_val[,1], main="Missing Data Portfolio re-evaluations - rate=1%")
abline(v=quantile(portfolio1_val[,1], 0.995), col="blue")
abline(v=mean(portfolio1_val[,1]), col="red")

hist(portfolio1_val[,2], main="Missing Data Portfolio re-evaluations - rate=3%")
abline(v=quantile(portfolio1_val[,2], 0.995), col="blue")
abline(v=mean(portfolio1_val[,2]), col="red")

# - Histograms
hist(portfolio1_val[,1], main="Sep. Val. Portfolio re-evaluations - rate=1%")
abline(v=quantile(portfolio1_val_sep[,1], 0.995), col="blue")
abline(v=mean(portfolio1_val_sep[,1]), col="red")

hist(portfolio1_val[,2], main="Sep. Val. Portfolio re-evaluations - rate=3%")
abline(v=quantile(portfolio1_val_sep[,2], 0.995), col="blue")
abline(v=mean(portfolio1_val_sep[,2]), col="red")  


############################# - Likelihood Ratio Test and Information Criteria - ####################################

# Negative log-likelihood function
NegLogL_ILR_M0full = function(vdParameters){    #
  
  x1 = DS1$EntAge
  t1 = DS1$TObs
  d1 = DS1$Status
  
  x2 = DS2$EntAge
  t2 = DS2$TObs
  d2 = DS2$Status
  
  x3 = DS3$EntAge
  t3 = DS3$TObs
  d3 = DS3$Status
  
  x4 = DS4$EntAge
  t4 = DS4$TObs
  d4 = DS4$Status  
  
  ben1 = DS1$BH
  ben2 = DS2$BH
  
  benL_ind1 = DS1$BL
  benH_ind1 = DS1$BH
  
  benL_ind2 = DS2$BL
  benH_ind2 = DS2$BH  
  
  gde11 = DS1$C1
  gde21 = DS1$C2
  
  gde13 = DS3$C1
  gde23 = DS3$C2
  
  geo0_ind1 = DS1$C0
  geo1_ind1 = DS1$C1
  geo2_ind1 = DS1$C2
  
  geo0_ind3 = DS3$C0
  geo1_ind3 = DS3$C1
  geo2_ind3 = DS3$C2
  
  a   = vdParameters[1] #alpha
  b   = vdParameters[2] #beta
  
  pi = matrix(NA, nrow=1, ncol=5)
  pi[1] = vdParameters[3]
  pi[2] = vdParameters[4]
  pi[3] = vdParameters[5]
  pi[4] = vdParameters[6]
  pi[5] = vdParameters[7]
  
  pi_psi <- pi %*% psi
  s_pi = sum(exp(pi_psi))
  
  #Weighted log-likelihoods
  
  logL_1 <- log( benL_ind1 * geo0_ind1 * (exp(pi_psi[1])/s_pi) +                                                                                               
                   benH_ind1 * geo0_ind1 * (exp(pi_psi[2])/s_pi) +                                                                                                 
                   benL_ind1 * geo1_ind1 * (exp(pi_psi[3])/s_pi) +                                                                                                
                   benH_ind1 * geo1_ind1 * (exp(pi_psi[4])/s_pi) +                                                                                   
                   benL_ind1 * geo2_ind1 * (exp(pi_psi[5])/s_pi) +                                                                                                      
                   benH_ind1 * geo2_ind1 * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi) - (exp(pi_psi[5])/s_pi))) - exp(a + b * x1) * (exp(b * t1) - 1)/b + d1 * (a + b * (x1 + t1))
  
  logL_2 <- log( benL_ind2 * (exp(pi_psi[1])/s_pi) + benH_ind2 * (exp(pi_psi[2])/s_pi) +                                                                                                   
                   benL_ind2 * (exp(pi_psi[3])/s_pi) + benH_ind2 * (exp(pi_psi[4])/s_pi) +                                                                                                   
                   benL_ind2 * (exp(pi_psi[5])/s_pi) + benH_ind2 * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi) - (exp(pi_psi[5])/s_pi)))  - exp(a + b * x2) * (exp(b * t2) - 1)/b + d2 * (a + b * (x2 + t2))
  
  logL_3 <- log( geo0_ind3 * (exp(pi_psi[1])/s_pi) + geo1_ind3 * (exp(pi_psi[3])/s_pi) + geo2_ind3 * (exp(pi_psi[5])/s_pi) +                                                                                                      
                   geo0_ind3 * (exp(pi_psi[2])/s_pi) + geo1_ind3 * (exp(pi_psi[4])/s_pi) + geo2_ind3 * (1 - (exp(pi_psi[1])/s_pi) - (exp(pi_psi[2])/s_pi) - (exp(pi_psi[3])/s_pi) - (exp(pi_psi[4])/s_pi) - (exp(pi_psi[5])/s_pi))) - exp(a + b * x3) * (exp(b * t3) - 1)/b + d3 * (a + b * (x3 + t3))
  
  logL_4 <-  -exp(a + b * x4) * (exp(b * t4) - 1)/b + d4 * (a + b * (x4 + t4))   
  
  return(-(sum(logL_1) + sum(logL_2) + sum(logL_3) + sum(logL_4)))
}

st_val_matrix <- c(-10, 0.1, nrow(DS_C[(DS_C$C0==1 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$C0==1 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==1 & DS_C$BH==1),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==0),])/nrow(DS_C), nrow(DS_C[(DS_C$GeoGroup==2 & DS_C$BH==1),])/nrow(DS_C))

sv_pi <- (log(st_val_matrix[3:8])) %*% t(psi) 

st_val <- matrix(NA, 1, 7)
st_val[1:2] <- st_val_matrix[1:2] #init_val[1:5]
st_val[3:7] <- sv_pi

# - Step 4 - Eastimate tau and zeta
Model0_full <- nlm(NegLogL_ILR_M0full, p=st_val, typsize=st_val, hessian=T, iterlim=10000, gradtol = 0.00000001) #,gradtol = 0.0000001, steptol = 0.00000001

# - Step 5 - Recalculate zeta
pi_psi <- Model0_full$estimate[3:7] %*% psi
zeta <- exp(pi_psi)/sum(exp(pi_psi))

spect_decomp <- eigen(Model0_full$hessian)
hess_mat <- -Model0_full$hessian

eigenval <- matrix(spect_decomp$values, nrow=1, ncol = 10)

is.singular.matrix(Model0_full$hessian, tol = 1e-08)

LR_Test_Stat <- -2*(theta_est$minimum - Model0_full$minimum)
pchisq(LR_Test_Stat, df=3, lower.tail=FALSE)


AIC_Mis_M0 <- 2 * (Model0_full$minimum - 7)
AIC_Mis_M3 <- 2 * (theta_est$minimum - 10)

BIC_Mis_M0 <- 2 * Model0_full$minimum - 7 * log(nrow(DS_C))
BIC_Mis_M3 <- 2 * theta_est$minimum - 10 * log(nrow(DS_C))

