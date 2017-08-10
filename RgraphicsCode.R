### Introduction to Graphics in R
### Class Notes

# Reading in the data
dat <-read.table(file="thedata.txt",header=T)
head(dat)
tail(dat)

# Exploring the data
table(dat$region)
barplot(table(dat$region))
hist(dat$age)
table(dat$gender)
barplot(table(dat$gender))
hist(dat$income)
sum(dat$income < 0)
sum(dat$income == 0)
min(dat$income)
boxplot(income ~ region,data=dat)
boxplot(income ~ gender,data=dat)
plot(dat$age,dat$income)
fit.age <- lm(income~age,data=dat)
summary(fit.age)

# Customizing a scatter plot
help(plot.default)
help(par)

# Two useful graphics functions
x11()              ### opens another graphics window
graphics.off()     ### closes all graphics devices 

# Default plot
plot(dat$age,dat$income)

# Adding axis labels and title using xlab, ylab, main tags
plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income")

# Adding axis ranges and tic mark label orientation using xlim, ylim, las tags
plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     ylim=c(-5000,15000), xlim=c(15,70),las=1)

# Changing the plotting symbol and its color using pch and col tags
plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")

# See document pch.pdf for the different plotting symbols
colors()     ### will list the colors

# Adding the regression line using the abline function
plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")

abline(fit.age)

# Adding an arbitrary straight line using the segments function
# Tag lty is the line type and lwd is the line width see help(par) for more information
# This example adds a vertical wider dotted blue line at the median of age

help(segments)

plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")
abline(fit.age)

segments(x0=median(dat$age),y0=-5500,x1=median(dat$age),y1=15500,
         lty="dotted",lwd=2,col="blue")

# Adding points to the plot using the points function
# In this example, we’re actually writing over existing points, coloring the points from region CC green
# help(points)

plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")
abline(fit.age)
segments(x0=median(dat$age),y0=-5500,x1=median(dat$age),y1=15500,
         lty="dotted",lwd=2,col="blue")

points(dat$age[dat$region=="CC"],dat$income[dat$region=="CC"],
       col="green",pch=20)

# Adding a legend to the plot using the legend function

help(legend)

plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")
abline(fit.age)
segments(x0=median(dat$age),y0=-5500,x1=median(dat$age),y1=15500,
         lty="dotted",lwd=2,col="blue")
points(dat$age[dat$region=="CC"],dat$income[dat$region=="CC"],
       col="green",pch=20)

legend(45,1000,col=c("green","red"),pch=20,
       legend=c("CC region","All other regions"))

# Saving this plot into a pdf document using the pdf function and the dev.off function

help(pdf)
help(Devices)

pdf(file="scatter.pdf",height=8,width=10)

plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")
abline(fit.age)
segments(x0=median(dat$age),y0=-5500,x1=median(dat$age),y1=15500,
         lty="dotted",lwd=2,col="blue")
points(dat$age[dat$region=="CC"],dat$income[dat$region=="CC"],
       col="green",pch=20)
legend(25,500,col=c("green","red"),pch=20,
       legend=c("CC region","All other regions"))

dev.off()

# Adding text to a plot using the text function
# First create the vectors with the information I wish to add to plot

region <- unique(dat$region)
mean.age <- NULL
for (i in 1:length(region))
{
  mean.age <- c(mean.age, mean(dat$age[dat$region==region[i]]))
}

region
mean.age

for (i in 1:length(region))
{
  mean.income <- c(mean.income, mean(dat$income[dat$region==region[i]]))
}

mean.income

dat2 <- data.frame(region=region,mean.age=mean.age,mean.income=mean.income)
dat2

# Now the plot

help(text)

plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")

text(x=mean.age, y=mean.income, labels=region, col="blue")

# Making the text smaller and bold using the cex and font tags (see help(par))

plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")

text(x=mean.age, y=mean.income, labels=region, col="blue", cex=0.8, font=2)

# Customizing the y axis using the yaxt tag and the axis function

# Plot before customizing y axis

plot(dat$age,dat$income,xlab="Age",ylab="Income",pch=20)

# Finding the y axis range
min(dat$income)
max(dat$income)

help(axis)

# Plot after customizing y axis
plot(dat$age,dat$income,xlab="Age", xlim=c(20,60),pch=20,
     ylim=c(-2000,14000), ylab="Income in Thousands",yaxt="n")

axis(side=2,at=seq(from=-2000,to=14000,by=2000),
     labels=seq(from=-2,to=14,by=2),las=1)


# Spacing and page layout
# Set placement on page via margin sizes using mai tag in par function
# Order is bottom,left,top,right
# mai 
# A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
# from (help(par)) 

par()
par()$mai

# First look at default
pdf(file="mean.plot.default.pdf",height=8, width=10)

plot(dat$age,dat$income,xlab="Age", xlim=c(20,60),pch=20,
     ylim=c(-2000,14000), ylab="Income in Thousands",yaxt="n")

axis(side=2,at=seq(from=-2000,to=14000,by=2000),
     labels=seq(from=-2,to=14,by=2),las=1)

text(x=mean.age, y=mean.income, labels=region, col="blue", cex=0.8, font=2)

dev.off()

# Now change margins

pdf(file="mean.plot.margins.pdf",height=8, width=10)

### bottom is 2, left is 2, top is 1.5, right is 1
par(mai=c(2,2,1.5,1))

plot(dat$age,dat$income,xlab="Age", xlim=c(20,60),pch=20,
     ylim=c(-2000,14000), ylab="Income in Thousands",yaxt="n")

axis(side=2,at=seq(from=-2000,to=14000,by=2000),
     labels=seq(from=-2,to=14,by=2),las=1)

text(x=mean.age, y=mean.income, labels=region, col="blue", cex=0.8, font=2)

dev.off()


# Customizing barplots
help(barplot)

barplot(table(dat$gender,dat$region))

table(dat$gender,dat$region)

temp <- table(dat$gender,dat$region)
temp
dim(temp)
colnames(temp)
rownames(temp)

# Looking at the gender distribution by region
# Using the beside tag

barplot(table(dat$gender,dat$region),beside=T,las=1,xlab="Region")

# Customizing the bars using col, angle, density tags

barplot(table(dat$gender,dat$region),beside=T,las=1,xlab="Region",
        col=c("red","green","blue"), angle=c(45,135,0), density=c(4,8,12))

# Adding the legend using legend.text tag

pdf(file="barplot.pdf",height=8,width=10)
barplot(table(dat$gender,dat$region),beside=T,las=1,xlab="Region",
        col=c("red","green","blue"), angle=c(45,135,0), density=c(4,8,12),
        legend.text=c("Female","Male","None") )
dev.off()

# Customizing the legend using the legend function

help(legend)

pdf(file="barplot2.pdf",height=8,width=10)
barplot(table(dat$gender,dat$region),beside=T,las=1,xlab="Region",
        col=c("red","green","blue"), angle=c(45,135,0), density=c(4,8,12), 
        ylim=c(0,70))

legend(3,70, fill=c("red","green","blue"), angle=c(45,135,0), density=32,
       legend= c("Female","Male","Nux") )

dev.off()

# Moving the legend and adding a title using text function

pdf(file="barplot3.pdf",height=8,width=10)

barplot(table(dat$gender,dat$region),beside=T,las=1,xlab="Region",
        col=c("red","green","blue"), angle=c(45,135,0), density=c(4,8,12), 
        ylim=c(0,70))

legend(18,60, fill=c("red","green","blue"), angle=c(45,135,0), density=32,
       legend= c("Female","Male","Nux") )

text(1,65,"Distribution of Gender by Region",pos=4)

dev.off()


# Multiple plots on the same page using par(mfrow)
# mfcol, mfrow 
# A vector of the form c(nr, nc). Subsequent figures will be drawn in an nr-by-nc array on the device by columns (mfcol), or rows (mfrow), respectively. 
# In a layout with exactly two rows and columns the base value of "cex" is reduced by a factor of 0.83: if there are three or more of either rows or columns, the reduction factor is 0.66. 
# If either of these is queried it will give the current layout, so querying cannot tell you the order the array will be filled. 
# Consider the alternatives, layout and split.screen. 
# (from help(par))

pdf(file="multiple1.pdf",height=5,width=10)

### one row by two columns
par(mfrow=c(1,2))

### left plot
plot(dat$age,dat$income,xlab="Age",ylab="Income",
     main="Relationship between Age and Income",
     pch=20,col="red")
text(x=mean.age, y=mean.income, labels=region, col="blue", cex=0.8, font=2)

### right plot
barplot(table(dat$gender,dat$region),beside=T,las=1,xlab="Region",
        col=c("red","green","blue"), ylim=c(0,70))
legend(1,60, fill=c("red","green","blue"), 
       legend= c("Female","Male","Nux"), cex=0.7 )
text(1,65,"Distribution of Gender by Region",pos=4)

dev.off()


# Using a loop
# Plot each region separately: age vs. income with regression line

pdf(file="multiple2.pdf",height=8,width=10)

### two rows by three columns
par(mfrow=c(2,3))

### get the five regions "AA" thru "FF"
region <- unique(dat$region)

### use loop to do the same thing for each region
for (i in 1:length(region))
{
  ### get data for just one region and store in "temp"
  temp <- dat[dat$region==region[i],]
  
  ### get the regression line fit
  fit <- lm(income~age,data=temp)
  
  ### plot data points
  plot(temp$age,temp$income,xlab="Age", xlim=c(20,60),pch=20,
       ylim=c(-2000,14000), ylab="Income in Thousands",yaxt="n",
       main=paste("Region ",region[i],sep=""))
  
  ### custom y-axis
  axis(side=2,at=seq(from=-2000,to=14000,by=2000),
       labels=seq(from=-2,to=14,by=2),las=1)
  
  ### add regression line
  abline(fit$coef,col="red")		    	  	
}

### close pdf file
dev.off()


# Drawing on a blank canvas and the polygon function

pdf("demo.drawing.pdf",width=10,height=10)

### blank canvas
plot(NULL,NULL, type="n", xlim=c(0,10), ylim=c(0,10), xlab="", ylab="", yaxt="n", xaxt="n", frame.plot=FALSE)

### black vertical line
segments(x0=0,x1=0,y0=0,y1=10)

### black triangle (outline)
polygon(x=c(2,4,3),y=c(3,3,5))

### dotted darkgreen sine wave
x <- seq(1,1+3*pi,0.1)
y <- sin(x) + 7
lines(x,y,col="darkgreen",lty="dotted",lwd=3)

### blue circle shaded with diagonal lines
x1 <- seq(7-sqrt(3),7+sqrt(3),.1)
x2 <- seq(7+sqrt(3),7-sqrt(3),-.1)
y1 <- sqrt(3 - (x1 - 7)^2) + 2
y2 <- -sqrt(3 - (x1 - 7)^2) + 2
polygon(x=c(x1,x2),y=c(y1,y2),density=16,col="blue")

### square, outline in black, fill in red
polygon(x=c(4,5,5,4),y=c(7.5,7.5,8.5,8.5),col="red")

dev.off()

