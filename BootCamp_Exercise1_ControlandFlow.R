#Exercise 1
for (ii in 1:9){
  if (ii < 8){
    cat(" ",'\n');
  }
  else if (ii == 9){
    cat("*",'\n');
  }
}

#Exercise 2
for (ii in 1:9){
  if (ii < 8){
    cat("*&");
  }
  else if (ii == 9){
    cat("*");
  }
}

#Exercise 3

#dogs = dogs +1 for (i in 1:5)
  #Iteration 1: dogs = 10 -> 10+1 = 11
  #Iteration 2: dogs = 11 -> 11+1 = 12
  #Iteration 3: dogs = 12 -> 12+1 = 13
  #Iteration 4: dogs = 13 -> 13+1 = 14
  #Iteration 5: dogs = 14 -> 14+1 = 15

#meatloaf = meatloaf - i +1 for (i in 5:9)
  #Iteration 1(i=5): meatloaf = 0 -> 0-5+1 = -4
  #Iteration 2(i=6): meatloaf = -4 -> -4-6+1 = -9
  #Iteration 3(i=7): meatloaf = -9 -> -9-7+1 = -15
  #Iteration 4(i=8): meatloaf = -15 -> -15-8+1 = -22
  #Iteration 5(i=9): meatloaf = -22 -> -22-9+1 = -30

#bubbles = i for (i in -1:-4)
  #Iteration 1(i=-1): bubbles = -1
  #Iteration 2(i=-2): bubbles = -2
  #Iteration 3(i=-3): bubbles = -3
  #Iteration 4(i=-4): bubbles = -4


#Exercise 4
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 4 == 0){
    cat(years[ii], 'Hooray, presidential elections!', sep = '\t', fill = T)
  }
  else if(years[ii]%%2 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
  }
  }
  

#Exercise 5
bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);
interest = 0.0125;
compounded = rep(NA,length(bankAccounts))
for (i in 1:length(bankAccounts)){
  compounded[i] = interest*bankAccounts[i]+bankAccounts[i];
}


#Exercise 6
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interest <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

for (j in 1:5) {
  for (i in 1:length(bankAccounts)) {
    bankAccounts[i]=bankAccounts[i]+income[i]-fun[i]-house[i]-food[i];
    bankAccounts[i]=interest*bankAccounts[i]+bankAccounts[i];
    }
  }
  

#Exercise 7
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
years(2015:2020)
interest <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

for (j in 1:length(years)) {
  for (i in 1:length(bankAccounts)) {
    if (i%%2 & j%%2){ 
    bankAccounts[i]=bankAccounts[i]+income[i]-fun[i]-house[i]-food[i]+5;
    bankAccounts[i]=interest*bankAccounts[i]+bankAccounts[i];}
    else{
      bankAccounts[i]=bankAccounts[i]+income[i]-fun[i]-house[i]-food[i];
    bankAccounts[i]=interest*bankAccounts[i]+bankAccounts[i];
  }
  }
}


#Exercise 8
sum=0
for (ii in 1:17){
  sum <- sum +  ii
}
cat("the sum of it all is ", sum)


#Exercise 9
doubler <- function(num){
  if (ii <= -1) {
    cat(ii, " is small\n")
  }
  
  else if (ii > -1 & ii < 1){
    cat(ii, "is medium\n")
  }
  else{
    cat(ii, " is big\n")
  }
}

for (ii in-2:2){
  doubled <- doubler(ii)
  cat(doubled, "\n")
}