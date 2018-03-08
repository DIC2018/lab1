# setting current working directory to be of the file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
install.packages('rstudioapi')
install.packages('ggmap')
install.packages('maptools')
install.packages('ggplot')
install.packages('ggplot2')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### 11. Basic Plots ####



### 1. Problem 1 ###
sales1<-c(12,14,16,29,30,45,19,20,16, 19, 34, 20)
sales2<-rpois(12,34)  # random numbers, Poisson distribution, mean at 34, 12 numbers
par(bg="cornsilk")
plot(sales1, col="blue", type="o", ylim=c(0,100), xlab="Month", ylab="Sales" )
title(main="Sales by Month")
lines(sales2, type="o", pch=22, lty=2, col="red")
grid(nx=NA, ny=NULL)
legend("topright", inset=.05, c("Sales1","Sales2"), fill=c("blue","red"), horiz=TRUE)




### 2. Problem 2 ###
sales<-read.table(file.choose(), header=T)
sales  # to verify that data has been read
barplot(as.matrix(sales), main="Sales Data", ylab= "Total",beside=T, col=rainbow(5))



### 3. Problem 3 ###
fn<-boxplot(sales,col=c("orange","green"))$stats
text(1.45, fn[3,2], paste("Median =", fn[3,2]), adj=0, cex=.7)
text(0.45, fn[3,1],paste("Median =", fn[3,1]), adj=0, cex=.7)
grid(nx=NA, ny=NULL)

#### 12. Importing data into R studio ####


### 4. Problem 4 ###
fb1<-read.csv(file = "C:\\Users\\sunil\\Working\\MS\\Semester 2\\DIC\\FB.csv")
aapl1<-read.csv(file = "C:\\Users\\sunil\\Working\\MS\\Semester 2\\DIC\\AAPL.csv")
par(bg="cornsilk")
plot(aapl1$Adj.Close, col="blue", type="o", ylim=c(145,205), xlab="Days", ylab="Price" )
lines(fb1$Adj.Close, type="o", pch=22, lty=2, col="red", ylim = range(150,200))
legend("bottomright", inset=.05, c("Apple","Facebook"), fill=c("blue","red"), horiz=TRUE)
#Just study the distribution of the adjusted close of the stock price of Apple.
hist(aapl1$Adj.Close, col=rainbow(9))



### 5. Problem 5 ###
data()
library (help=datasets)
library(datasets)
library(ggplot2) # to get mpg dataset

attach(mpg)
top5 <- head(mpg)
top5
mpg_summary <- summary(mpg)
mpg_summary
#after analysis remove the data from the memory
detach(mpg)

top5_uspop <- head(uspop)
top5_uspop
plot(uspop)



### 6. Problem 6 ###
library("ggmap")
library("maptools")
library(maps)
visited <- c("SFO", "Chennai", "London", "Melbourne", "Lima,Peru", "Johannesbury, SA")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(visit.x,visit.y, col="red", pch=36)

library("ggmap")
library("maptools")
library(maps)
visited <- c("SFO", "New York", "Buffalo", "Dallas, TX")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("state", fill=TRUE, col=rainbow(50), bg="lightblue", mar=c(0,0,0,0))
points(visit.x,visit.y, col="yellow", pch=36)


### 7. Problem 7 ###
library('lattice')
splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")
splom(mtcars[c(1,3,4,6)], main="MTCARS Data")
splom(mtcars[c(1,3,4,6)], col=rainbow(3),main="MTCARS Data")

splom(rock[c(1,2,3,4)], main="ROCK Data")



### 8. Problem 8 ###
### IRIS Dataset from Kaggle: https://www.kaggle.com/leolcling/visualizing-iris-datasets-with-r-ggplot2
library('ggplot2')
library(readr)
options(repr.plot.width = 6, repr.plot.height = 6) # resize the plots
Iris <- read.csv('Iris.csv')
class(Iris)

# We could use some graph to see the relation between these features
# First we could check the relation between Sepal length and Speal width 
# we use theme_minimal() here to adjust the appearance of the plots 
ggplot(data=Iris,aes(x=SepalWidthCm, y=SepalLengthCm)) + geom_point() + theme_minimal()

#Now We would like to see this relation by each species 

#we can pass a additional aesthetic to our ggplot, which marked the species by different colors
ggplot(data=Iris,aes(x=SepalWidthCm, y=SepalLengthCm,color=Species)) + geom_point() + theme_minimal()

# On top of this scatter plot, we could add a trend line to visualize the general trend 
# we could do so by adding geom_smooth()

ggplot(data=Iris,aes(x=SepalWidthCm, y=SepalLengthCm,color=Species)) + geom_point() +geom_smooth() + theme_minimal()

# we can turn off the grey area, which is the confident interval, by passing argument se=False
ggplot(data=Iris,aes(x=SepalWidthCm, y=SepalLengthCm,color=Species)) + geom_point() +geom_smooth(se=FALSE)+
  theme_minimal()

# Or we can divide the plot into multiple plots, one for each species
# by adding facet_wrap()
options(repr.plot.width = 10, repr.plot.height = 6)# to adjust size of plots

ggplot(data=Iris,aes(x=SepalWidthCm, y=SepalLengthCm,color=Species)) +
  geom_point() + geom_smooth(se=FALSE) +facet_wrap(~Species) +theme_minimal()


#You may notice the scale of xy axis are the same for three plots.
# we can pass scale ="free_y" argument to facet_wrap() 
#(the default value of scale is 'fixed', other options are 'free', free_x')
options(repr.plot.width = 10, repr.plot.height = 5) # to adjust size of plots

ggplot(data=Iris,aes(x=SepalWidthCm, y=SepalLengthCm,color=Species)) +
  geom_point() + geom_smooth(se=FALSE) +facet_wrap(~Species,scale='free_y') +theme_minimal()


# Okay enough for scatter plot.
# we could do many other plots using ggplot2

# Let's look at boxplot
options(repr.plot.width = 5, repr.plot.height = 4)

ggplot(data=Iris,aes(x=Species, y=PetalLengthCm,color=Species)) + geom_boxplot() +theme_minimal()+
  theme(legend.position="none")


# Another option is to use violin, which could show us the distribution of the data.
options(repr.plot.width = 5, repr.plot.height = 4)
ggplot(data=Iris,aes(x=Species, y=PetalLengthCm,color=Species)) + geom_violin() +theme_minimal()+
  theme(legend.position="none")


# we can a stacked histogram by adding another aesthetic 'fill'
ggplot(data=Iris,aes(x=SepalLengthCm,fill=Species)) + geom_histogram() +theme_minimal()

# density plot
ggplot(data=Iris,aes(x=PetalWidthCm,y=PetalLengthCm,color=Species)) +geom_density2d()+ theme_minimal()
