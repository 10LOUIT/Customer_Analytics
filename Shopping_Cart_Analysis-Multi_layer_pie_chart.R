# Shopping cart analysis with R – Multi-layer pie chart
# http://analyzecore.com/2014/10/17/cart-analysis-r/

# loading libraries
library(dplyr)
library(reshape2)
library(plotrix)

# creating an example of orders
set.seed(15)
df <- data.frame(orderId=sample(c(1:1000), 5000, replace=TRUE),
                 product=sample(c('NULL','a','b','c','d'), 5000, replace=TRUE,
                                prob=c(0.15, 0.65, 0.3, 0.15, 0.1)))
df <- df[df$product!='NULL', ]

# processing initial data
# we need to be sure that product's names are unique
df$product <- paste0("#", df$product, "#")

prod.matrix <- df %>%
  # removing duplicated products from each order
  group_by(orderId, product) %>%
  arrange(product) %>%
  unique() %>%
  # combining products to cart and calculating number of products
  group_by(orderId) %>%
  summarise(cart=paste(product,collapse=";"),
            prod.num=n()) %>%
  # calculating number of carts
  group_by(cart, prod.num) %>%
  summarise(num=n()) %>%
  ungroup()

head(prod.matrix) 

# calculating total number of orders/carts
tot <- sum(prod.matrix$num)

# spliting orders for sets with 1 product and more than 1 product
one.prod <- prod.matrix %>% filter(prod.num == 1)

sev.prod <- prod.matrix %>%
filter(prod.num > 1) %>%
arrange(desc(prod.num))

# 0 circle: blank
pie(1, radius=iniR, init.angle=90, col=c('white'), border = NA, labels='')

# drawing circles from last to 2nd
for (i in length(prod):2) {
  p <- grep(prod[i], sev.prod$cart)
  col <- rep('NO', times=nrow(sev.prod))
  col[p] <- prod[i]
  floating.pie(0,0,c(sev.prod$num, tot-sum(sev.prod$num)), radius=(1+i)*iniR, startpos=pi/2, col=as.character(colors [ c(col, 'NO')]), border="#44aaff")
}

# 1 circle: orders with 1 product
floating.pie(0,0,c(tot-sum(one.prod$num),one.prod$num), radius=2*iniR, startpos=pi/2, col=as.character(colors [ c('NO',one.prod$cart)]), border="#44aaff")

# legend
legend(1.5, 2*iniR, gsub("_"," ",names(colors)[-1]), col=as.character(colors [-1]), pch=19, bty='n', ncol=1)

# 0 circle: blank
pie(1, radius=iniR, init.angle=90, col=c('white'), border = NA, labels='')

# drawing circles from last to 2nd
for (i in length(prod):2) {
  p <- grep(prod[i], sev.prod$cart)
  col <- rep('NO', times=nrow(sev.prod))
  col[p] <- prod[i]
  floating.pie(0,0,c(sev.prod$num, tot-sum(sev.prod$num)), radius=(1+i)*iniR, startpos=pi/2, col=as.character(colors [ c(col, 'NO')]), border="#44aaff")
}

# 1 circle: orders with 1 product
floating.pie(0,0,c(tot-sum(one.prod$num),one.prod$num), radius=2*iniR, startpos=pi/2, col=as.character(colors [ c('NO',one.prod$cart)]), border="#44aaff")

# legend
legend(1.5, 2*iniR, gsub("_"," ",names(colors)[-1]), col=as.character(colors [-1]), pch=19, bty='n', ncol=1)

# creating a table with the stats
stat.tab <- prod.matrix %>%
  select(-prod.num) %>%
  mutate(share=num/tot) %>%
  arrange(desc(num))

library(scales)
stat.tab$share <- percent(stat.tab$share) # converting values to percents

# adding a table with the stats
addtable2plot(-2.5, -1.5, stat.tab, bty="n", display.rownames=FALSE,
              hlines=FALSE, vlines=FALSE, title="The stats")