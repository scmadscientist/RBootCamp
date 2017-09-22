X <- read.csv('ChlorellaGrowth.csv',comment.char='#')

# using '#' as a comment character allows us the CSV file to be self-documenting


Light <- X[,1]
rmax <- X[,2]
LogLight = log(Light)
X1 = cbind(X, LogLight)
Light1 = X1[,1]
rmax1 = X1[,2]
LogLight1 = X1[,3]


par(cex=1.5,cex.main=0.9)

plot(rmax1~LogLight1,data=X1, xlab="Log light intensity (uE/m2/s)",ylab="maximum growth rate (1/d)",pch=16) 

# xlab and ylab are x and y axis labels, pch is "plotting character"

# cex is 'character expansion' - cex=1.5 increases symbol & label sizes by 50%

# cex.main sets the character expansion for the main title of the plot 



title(main="Data from Fussmann et al. (2000) system")

fit <- lm(rmax1~LogLight1)

summary(fit); abline(fit) 

###

# Next we get the regression equation to 'display itself' on the graph

c1 <- round(fit$coef[1],digits=3) 	# intercept	

c2 <- round(fit$coef[2],digits=3) 	# slope

text(3.5,3,paste("rmax=", c1,"+", c2,"LogLight")
