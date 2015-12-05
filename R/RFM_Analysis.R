

# add appropriate column names for the above three column and
names <- c("ID","Date","Amount")
names(df) <- names

#tranfer the the text column type to date type
# doesn't work
# df$Date <- strftime(df$Date,"%Y%m%d")

#tranfer the the text column type to date type
df[,2] <- as.Date(as.character(df[,2]),"%Y-%m-%d")

head(df)
dim(df)

#remove the rows with the duplicated IDs to see how many customers in total
uid <- df[!duplicated(df[,"ID"]),]
dim(uid)

# set the startDate and endDate, we will only analysis the records in this date range
#startDate <- as.Date("19970101","%Y%m%d") 
startDate <- as.Date("20140101","%Y%m%d")

#endDate <- as.Date("19980701","%Y%m%d")
endDate <- as.Date("20141231","%Y%m%d")

dfNew <- getDataFrame(df,startDate,endDate)
head(dfNew)
dim(dfNew)

df1 <-getIndependentScore(dfNew)
head(df1[-(2:3)])


#Draw the histograms in the R, F, and M dimensions so that we can see the distribution of customers in each RFM cell.
drawHistograms(df1)

# Let’s further find out how many customers have a total score larger than 500 or 400.
S500<-df1[df1$Total_Score>500,]
dim(S500)

S400<-df1[df1$Total_Score>400,]
dim(S400)

par(mfrow = c(1,3))
hist(dfNew$Recency)
summary(dfNew$Recency)
hist(dfNew$Frequency)
summary(dfNew$Frequency)
hist(dfNew$Monetary)
summary(dfNew$Monetary)

# set the Recency ranges as 0-120 days, 120-240 days, 240-450 days, 450-500days, and more than 500days.
r <-c(120,240,450,500)
r <-c(30,120,240,365)

# set the Frequency ranges as 0 – 2times, 2-5 times,5-8 times, 8-10 times, and more than 10 times.
f <-c(2,5,8,10)
f <-c(1,2,5,10,25,1000)

# set the Monetary ranges as 0-10 dollars, 10-20 dollars, and so on.
m <-c(50,100,250,500,2500,100000)
m <-c(10,20,30,100)

df2<-getScoreWithBreaks(df,r,f,m)
drawHistograms(df2)

# We can also calculate how many customers have a total score of more than 500 or 400.
S500<-df2[df2$Total_Score>500,]
dim(S500)
drawHistograms(df2)
S400<-df2[df2$Total_Score>400,]
dim(S400)

# Select the Target Customers
target <- df2[df2$Total_Score>=441,]
dim(target)
df1
