get_heights=function(x){
  heights = rnorm(x,69,10)
  mean_heights=mean(heights)
  return(mean_heights)
}

get_heights(100)

mean_heights_100 = rep(NA,length(get_heights(100)))
for (i in 1:1000){
  mean_heights_100[i] = mean(get_heights(100));
 }

mean_heights_1000 = rep(NA,length(get_heights(1000)))
for (i in 1:1000){
  mean_heights_1000[i] = mean(get_heights(1000));
}


#plot
bins = seq(65,73,by=0.5)
counts_100 = hist(mean_heights_100,breaks=bins)$counts
counts_1000 = hist(mean_heights_1000,breaks=bins)$counts

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and margins

barplot(rbind(counts_100,counts_1000),col=c("red","blue"),beside=T,names.arg=seq(65,72.5,by=0.5),xlab="Average height (inches)",ylab="Count")
legend(6,350,c("n=100","n=1000"),col=c(2,4),lwd=4)
