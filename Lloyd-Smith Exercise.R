#Modeling Exercises

#Model this equation:
# NN[tt+1]=NN[tt]*exp(RR*(1-(NN[tt]/K))))
#Create function
logFun = function(RR,N0,ttMax,KK, PLOTFLAG = 1){
  #Initialize vector
  NN = matrix(NA, nrow =1, ncol=ttMax+1)
  NN[1] = N0
  
  #Use the for loop
  for (tt in 1:ttMax){
    NN[tt+1]=NN[tt]*exp(RR*(1-(NN[tt]/KK)))
  }
  #Plot
  
  if (PLOTFLAG==1){
    plot(1:(ttMax+1), NN, lty=2, type ='l', xlab = "time", ylab="N", col="blue")
  }
  return(NN)
}



#Part B
#N = 0
logFun(-1.05,100,10,5000)

#Approaching equilibrium
logFun(1.02,100,10,5000)

#Decaying oscillations
logFun(1.5,100,10,5000)

#Regular oscillations
logFun(2,100,10,5000)

#CHAOS!!!!!!
logFun(2.9,100,10,5000)

#Growth rate is key factor i.e. K vs. R populations



#Part C
# Six different growth rates
#Set up plot window
par(mfrow=c(2,3))

#Define r values we want to look at
RRseq = c(-1.5, -0.5, 0.5, 1, 1.5, 2)
N0 = 100
KK = 5000
ttMax = 10

#Use the for loop
   for (ii in 1:length(RRseq)){
     logFun(RR = RRseq[ii],N0,ttMax, KK)
   }



#Part D
R = 0.1
KK = 1000
nVec = logFun(R, 20, 10, KK)

nVecMin = which(nVec >= (KK/2))[1]


#Part E
#Initialize variables and create empty vector

KK = 1000
RR = seq(0.1,0.9, by=0.1)
Thalf = matrix(NA, nrow = 1, ncol = length(RR))

par(mfrow=c(1,1))
for (ii in 1:length(RR)){
  nVec =  logFun(RR[ii], 100, 50, KK,PLOTFLAG = 0)
  Thalf[ii] = which(nVec >= (KK/2))[1]
  plot(RR, Thalf, ylab = 'year')
}


#Part F and G
#Choose R and K vectors
RR = seq(0.1,0.9, by=0.1)
KK = seq(1000,5000, by = 500)

#Choose time t at which to extract values
ttCollect = 9

#Create empty matrix for every value R by every value K
BB = matrix(NA, nrow = length(RR), ncol = length(KK))

#Run for loop using function and outputing at a certain time tt for each value R and K
for (ii in 1:length(RR)){
  for (xx in 1:length(KK)){
  output =  logFun(RR[ii], 20, 10, KK[xx], 0)
  #Fill in matrix BB
  BB[ii,xx] = output[ttCollect]
  }
  #Plot
  contour(RR,KK,BB, lty = 7, col = 'dodgerblue',labcex = 0.8, xlab="R", ylab = "K", main = "Population Size (N) for Different R and K")
}
