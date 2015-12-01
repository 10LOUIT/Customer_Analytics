### 
# Data Provisioning
df <- read.delim(file = 'purchases_2014.txt', header = FALSE, sep = ',', dec = '.')
# change column order
df <- df[c(1,3,2)]