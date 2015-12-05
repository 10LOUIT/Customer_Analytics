### read textfile
df <- read.table(file.choose(), header = F)

# Data Provisioning
df <- read.delim(file = 'purchases_2014.txt', header = FALSE, sep = ',', dec = '.')
df <- read.delim(file = file.choose(), header = FALSE, sep = ',', dec = '.')

# construct a data frame with the necessary columns of customer ID, transaction date, and money amount paid by a customer per transaction
#  data/CDNOW_SAMPLE.txt
df <- as.data.frame(cbind(df[,1],df[,3],df[,5]))
#  data/CDNOW_MASTER.txt
df <- as.data.frame(cbind(df[,1],df[,2],df[,4]))

# change column order
df <- df[c(1,3,2)]

# without transactions with negative and zero amount
df <- df[!df[3] <= 0.99, ]
dim(df3)

# check difference
nrow(df) - nrow(df3)
head(df3)
summary(df3)

drv <- JDBC(driverClass = "com.exasol.jdbc.EXADriver", classPath = "/JDBC/exajdbc.jar", identifier.quote="")
con <- dbConnect(drv, "jdbc:exa:10.250.32.204..206:8563", "sys", "exasol")
dbListTables(con)