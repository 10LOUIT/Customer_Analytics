# read data from textfile
df <- read.table(file.choose(),header=F)

# construct a data frame with the necessary columns of customer ID, transaction date, and money amount paid by a customer per transaction
#df <- as.data.frame(cbind(df[,1],df[,3],df[,2]))

# add appropriate column names for the above three column and 
names <- c("ID","Date","Amount")
names(df) <- names

#tranfer the the text column type to date type
#df[,2] <- as.Date(as.character(df[,2]),"%Y%m%d")

#df<-cbind(df,Month)

head(df)

#order the dataframe by customer ID and transcation date
df <- df[order(df$ID,df$Date),]

#get the number of rows in the dataset
nrow<-nrow(df)

#record the number of days between a customer's each transcation, say 10 days between 1st purchase and the second, 15 days between the second and the third Interval <-rep(0,times=nrow)

# record the # of a customer's each transaction say 1st transcation, 2nd transcation
Times <- rep(1,times=nrow)

# record the total number of a customer's transcations
TotalTimes <- rep(1,times=nrow)



#caculate the data for the above three vectors.
n<-2
Interval <- vector(length = nrow)
for (i in 2 : nrow){
  if (df[i,"ID"] == df[i-1,"ID"]){
  	Interval[i] <- as.numeric(difftime(df[i,"Date"],df[i-1,"Date"],units="days"))	
  	Times[i] <- n
  	n <- n+1
#   	sprintf("n: %s", 2)
#   	sprintf("i: %s", i)
  }else{
   	TotalTimes[(i-n+1) : (i-1)] <- n-1
  	n<-2
  }
}
#add the three vectors to the data frame 
df <- cbind(df, Interval,Times,TotalTimes)

head(df)

# get the matrix of customer ID ~ the customers total number of transactions
TimesByID <-as.data.frame(table(df$ID))

#get the matrix of total number of transactions ~ number of customers who have the total number
GroupByTimes <- as.data.frame(table(TimesByID$Freq))

names(GroupByTimes) <- c("Times","Customers")
 
#plot the number of the customers grouped by number of purchases
plot(GroupByTimes,xlab="Total Number of Purchases",ylab="Number of Customers",pch=19, col="blue",type="c", ann=F)
lines(GroupByTimes,xlab="Total Number of Purchases",ylab="Number of Customers",pch=19,col="red",lty=1,type="o")

# labels must be determined
# text(2,1220,"1205")
# text(3,425,"406")
# text(4,220,"208")
# text(5,170,"150")
# text(6,120,"98")
# text(12,50,"10")
# text(30,50,"1")

# ggplot line chart
# library(ggplot2)
# datn <- GroupByTimes
# datn$Times <- as.numeric(datn$Times)
# linechart <- ggplot(data=datn, aes(x=Customers, y=Times)) +
#   geom_line() +
#   geom_point()
# linechart #+ scale_y_continuous(breaks=c(2000))

# histogram
#hist(as.numeric(GroupByTimes$Customers), breaks=150, xlab = "Customer Groups", ylab = "Frequency")

# caculate the repeat purchase percentages
number_of_purchasing_customers <- nrow(TimesByID)
percentages<-round(GroupByTimes$Customers / number_of_purchasing_customers ,3)

number_of_purchases <- 15
bp=barplot(percentages[1:number_of_purchases]*100,col="blue",main="Percentage of Customers Making (x) Purchases", xlab="Number of Purchases", ylab="Repeat Purchase Rate (%)",ylim=range(0:55),axisnames=TRUE,names.arg=GroupByTimes$Times[1:number_of_purchases])

text(x=bp, y=percentages[1:number_of_purchases]*100,labels=paste(round(percentages[1:number_of_purchases]*100,2),"%"), pos=3, xpd=NA)
# filter out the customers who only made more than one purchase and their intervals between the 1st and the 2nd purchase
df2<-df[df$TotalTimes>=2 & Times==2,]

# see how many 2nd transcations
nrow(df2)

# get the mean days of customer latency
mean(df2$Interval)

# take a look at the distributions of the Customer Latency
hist(df2$Interval,main="Distribution of Customer Latency (1st - 2nd purchase)", xlab="Days", ylab="Number of 2nd Transcations", breaks=100)
