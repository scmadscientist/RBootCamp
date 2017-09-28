
# Set working directory:
#change this to whatever you want
setwd("c:/Users/Sammy/Documents/UCLA/Boot Camp")

# Load a data set
snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld-2.txt',header=TRUE)

#let's make a second funtion that makes use of R's built in chisq.test function

compute_chisquare_2=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  #here we use the built-in function for the chi-sq distribution:
  exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) #note, here we don't multiply by n
  chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
  return(chisq)
}


# Apply the compute_chi_square function to each snp
chisqs2=apply(snps,1,compute_chisquare_2)

# Compute p-values for each chi-square value using the pchisq function
pvals=pchisq(chisqs2,1,lower.tail=FALSE)

#How many SNP's have pvalues under certain value
p0.05 = length(which(pvals <=0.05))/length(pvals)
p0.01 = length(which(pvals <=0.01))/length(pvals)
p0.001 = length(which(pvals <=0.001))/length(pvals)

#How many SNP's were tested
num_pval = length(pvals)

#Create exp_pvalues vector
exp_pvals = rep(NA,length(num_pval))
for (i in 1:num_pval){
  exp_pvals[i] = i/num_pval
}

#Sort pvals
sort_pvals = sort(pvals, decreasing = F)

#Take logs
log_sort_pvals = -log10(sort_pvals)
log_exp_pvals = -log10(exp_pvals)

#Plot and add line
plot(log_exp_pvals, log_sort_pvals )
abline(0,1,untf = F, lwd = 5, lty = "dashed", col = "dodgerblue")

#Problem #2
zz=read.table('pheno.sim.2014-2.txt',header=TRUE)

#Find 25% quantile
low_bg = quantile(zz$glucose_mmolperL,0.25)

#Find 75% quantile
high_bg = quantile(zz$glucose_mmolperL, 0.75)

#Density plot
hist(zz$glucose_mmolperL)
abline(v=c(low_bg, high_bg), lwd = 4, lty = 'dashed', col = 'dodgerblue')
