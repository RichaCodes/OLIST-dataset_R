
library(plyr)
library(sqldf)
library(propagate)
library(tidyverse)
library(ggplot2)
library(dplyr)

#Reading the datasets
customer<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_customers_dataset.csv", header = TRUE)
geolocation<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_geolocation_dataset.csv", header = TRUE)
order_items<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_order_items_dataset.csv", header = TRUE)
payments<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_order_payments_dataset.csv", header = TRUE)
order_reviews<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_order_reviews_dataset.csv", header = TRUE)
orders<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_orders_dataset.csv", header = TRUE)
products<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_products_dataset.csv", header = TRUE)
sellers<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/olist_sellers_dataset.csv", header = TRUE)
translations<-read.csv("file:///C:/Users/Richa Patel/Desktop/olist/product_category_name_translation.csv", header = TRUE)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Extracting number of rows and column in each dataset
dim(customer)
dim(geolocation)
dim(order_items)
dim(payments)
dim(order_reviews)
dim(orders)
dim(products)
dim(sellers)
dim(translations)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#merging orders and payments by order_id
order_pay = merge(orders, payments, by="order_id")

#ORDER SUMMARY

sprintf("Total number of orders in the database:")
length(unique(orders$order_id))
sprintf("Total number of customer in the database:")
length(unique(orders$customer_id))
order_status=sqldf("SELECT order_status,COUNT(distinct(order_id)) FROM orders GROUP BY order_status")
names(order_status)[names(order_status) == 'COUNT(distinct(order_id))'] <- 'total_count'
order_Oitems = merge(orders, order_items, by="order_id")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Order value
max=sqldf("select max(payment_value),payment_installments from payments")
min=sqldf("select min(payment_value),payment_installments from payments")
avg=sqldf("select avg(payment_value),payment_installments from payments")
median=sqldf("select median(payment_value),payment_installments from payments")
max
min
avg
median

value=sqldf("select order_id,sum(price),sum(freight_value) from order_Oitems group by order_id")
names(value)[names(value) == 'sum(price)'] <- 'price'
names(value)[names(value) == 'sum(freight_value)'] <- 'freight_value'
value=value[order(value$price,decreasing = TRUE),]
top5_orders = head(value,5)

#Freight value of orders- Distribution
fr_value=order_items$freight_value
#fr_value_sample=fr_value[sample(nrow(fr_value), "60000"), ]
fr_value_sample <- sample(fr_value, 10000, replace = FALSE)
dvalues= dnorm(fr_value_sample+1,0,1,log = TRUE)
plot(dvalues, # Plot where y = values and x = index of the value in the vector
     xaxt = "n", # Don't label the x-axis
     type = "l", # Make it a line plot
     main = "Freight Value of orders- Distribution",
     xlab= "Freight value") 

#Product value(price) of orders- Distibution
prod_value=order_items$price
#pd_value_sample=prod_value[sample(nrow(prod_value), "60000"), ]
pd_value_sample <- sample(prod_value, 60000, replace = FALSE)
dvalues= dnorm(pd_value_sample+1,0,1,log = TRUE)
plot(dvalues, # Plot where y = values and x = index of the value in the vector
     xaxt = "n", # Don't label the x-axis
     type = "l", # Make it a line plot
     main = "Product Value of orders- Distribution",
     xlab= "Product value") 

sprintf("Skewness of the transaction value:")
x=log(payments$payment_value + 1)
skewness(x)
y=log(payments$payment_value + 1)
kurtosis(x)

sprintf("Skewness of the price value:")
x=log(order_items$price + 1)
skewness(x)
y=log(order_items$price + 1)
kurtosis(x)


sprintf("Skewness of the price value:")
x=log(order_items$freight_value + 1)
skewness(x)
y=log(order_items$freight_value + 1)
kurtosis(x)
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Number of products people usually buy:
order_usual = sqldf("SELECT order_item_id,count(order_id) from order_items group by order_item_id")
head(order_usual)

x = c(order_usual$order_item_id)
y = c(order_usual$`count(order_id)`)
barplot(y,names.arg=x,
        xlab="Number of products added in orders",
        ylab="Number of order",
        col="blue",
        main="Number of products people usually order",
        border="red")


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#MOST BOUGHT PRODUCT CATEGORIES
order_product = merge(order_items,products,by="product_id")
most_product = sqldf("select product_catagory_name_english, count(order_id) from order_product group by product_catagory_name_english")
names(most_product)[names(most_product) == 'count(order_id)'] <- 'order_count'
most_product=most_product[order(most_product$order_count),]
max_10 = tail(most_product,10)

np=c('au','gt','tl','wg','hw','ca','fd','sl','hb','bt')
x = c(max_10$product_catagory_name_english)
y = c(max_10$order_count)

barplot(y,names.arg=np,
        xlab="Product Category Name",
        ylab="Order Count",
        col="blue",
        main="MOST BOUGHT PRODUCT CATEGORIES",
        ylim=c(0,10000),
        border="red")


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Order Trend by Year
trandyear = merge(orders , order_items,by = "order_id")
trand_year = sqldf("SELECT Year,Count(price) FROM trandyear GROUP BY Year")

x = c(trand_year$Year)
y = c(trand_year$`Count(price)`)
barplot(y,names.arg=x,
        xlab="Year",
        ylab="Total Transaction Value",
        col="blue",
        main="Transaction Value By Year",
        border = "red")

#Box plot for transaction over years
year<- as.integer(trandyear$Year)
price <- as.integer(trandyear$`Count(price)`)
boxplot(price ~ year, data = trandyear, xlab = "Year",
        ylab = "Transaction value", main = "Boxplot for transaction over years",
        notch = TRUE, 
        varwidth = TRUE,
        ylim=c(0,5000),
        col = c("purple"))

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Average value of transaction by day of the week
trandweekday = merge(orders, order_items, by = "order_id")
trand_weekday = sqldf("SELECT Weekday_name, avg(price) FROM trandweekday GROUP BY Weekday_name")
y = c(trand_weekday$`avg(price)`)
barplot(y,names.arg=trand_weekday$Weekday_name,
        las=2,
        xlab="Day",
        ylab="Value",
        col="#ed5569",
        main="Average Value of Transaction Value By Day of Week",
        border = "pink")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Average Value of transaction for month
trans_month = merge(orders, order_items, by = "order_id")
t_month = sqldf("SELECT Month, avg(price) FROM trans_month GROUP BY Month")
y = c(t_month$`avg(price)`)
barplot(y,names.arg=t_month$Month,
        las=2,
        xlab="Month",
        ylab="Price",
        col="#ed5569",
        main="Average Value of Transaction for month",
        border = "pink")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Total num of Orders by day of the week 
freq_weekday = sqldf("SELECT Weekday_name, count(order_id) FROM trandweekday GROUP BY Weekday_name")
y = c(freq_weekday$`count(order_id)`)
barplot(y,names.arg=freq_weekday$Weekday_name,
        xlab="Day",
        ylab="Value",
        col = hsv(seq(0,1 - 1/12,length.out = 12), 0.5 , 1),
        main="Total num of Orders by day of the week",
        border = "pink")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Order Trend by hour
trandhour = merge(orders , order_items,by = "order_id")
trand_hour = sqldf("SELECT Hour,count(order_id) FROM trandhour GROUP BY Hour")

x = c(trand_hour$Hour)
y = c(trand_hour$`count(order_id)`)
barplot(y,names.arg=x,
        xlab="Hour Of The Day",
        ylab="Order Count",
        col="blue",
        main="Freqency of Transaction Over The Hour",
        border = "red")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Frequency of orders during the hour over the day(HEATMAP)
day_hour = sqldf("SELECT Hour,Weekday_name,count(order_id) FROM trandweekday GROUP BY Hour, Weekday_name")
head(day_hour)

heatmap(day_hour, Colv = NA, Rowv = NA, scale="column")


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#STATE,CITY AND TRANSACTION VALUE
oOitem_cust = merge(customer, order_Oitems, by="customer_id")

#Average transaction value for each state
trans_state = sqldf("select customer_state, avg(price) from  oOitem_cust group by customer_state")
names(trans_state)[names(trans_state) == 'avg(price)'] <- 'Average_Price'
#trans_state$Average_Price=as.vector(trans_state$Average_Price)
#is.vector(trans_state$Average_Price)
trans_state=trans_state[order(trans_state$Average_Price),]
trans_state10 = tail(trans_state,10)
x = c(trans_state10$customer_state)
y = c(trans_state10$Average_Price)
#is.vector(y)
par(mar=rep(2,4))
barplot(y,names.arg=trans_state10$customer_state,
        
        xlab="Customer State",
        ylab="Average transaction value",
        col="blue",
        main="Average Transaction value for top 10 states",
        border="red")

#Average transaction value for each city
trans_city = sqldf("select customer_city, avg(price) from  oOitem_cust group by customer_city")
names(trans_city)[names(trans_city) == 'avg(price)'] <- 'Average_Price'
#trans_state$Average_Price=as.vector(trans_state$Average_Price)
#is.vector(trans_state$Average_Price)
np1=c('pia','ne','en','a','m','l','ib','par','ba','bl')
trans_city=trans_city[order(trans_city$Average_Price),]
trans_city10 = tail(trans_city,10)
x1 = c(trans_city10$customer_city)
y1 = c(trans_city10$Average_Price)
#is.vector(y1)
par(mar=rep(2,4))
barplot(y1,names.arg=np,
      
        xlab="Customer City",
        ylab="Average transaction value",
        col="blue",
        main="Average Transaction value for top 10 city",
        border="red")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#ORDER DELIVERY

orders$date_diff=as.Date(strptime(orders$order_delivered_customer_date, "%d-%m-%Y"))-as.Date(strptime(orders$order_purchase_timestamp, "%d-%m-%Y"))
diffNum=as.numeric(orders$date_diff)
avg_days=sqldf("select avg(date_diff) from orders")
sprintf("Average delivery days:")
avg_days
orders$date_diff=  as.numeric(as.character(orders$date_diff))
#is.numeric(orders$date_diff)
delivery = sqldf("select count(order_id),date_diff from orders group by date_diff")
names(delivery)[names(delivery) == 'date_diff'] <- 'Days'
names(delivery)[names(delivery) == 'count(order_id)'] <- 'Number_of_Orders'

x2 = c(delivery$Days)
y2 = c(delivery$Number_of_Orders)
#is.vector(y)
#par(mar=rep(2,4))
barplot(y2,names.arg=x2,
        xlab="Days",
        ylab="Number of orders",
        col="blue",
        main="Days to delivery",
        ylim=c(0,7500),
        border="red")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#PAYMENT
pay = sqldf("SELECT payment_type,COUNT(order_id) FROM payments GROUP BY payment_type")
names(pay)[names(pay) == 'COUNT(order_id)'] <- 'num_of_instances'

n=c('boleto','cc','dc','nd','vou')
x3 = c(pay$payment_type)
y3 = c(pay$num_of_instances)
#is.vector(y)
#par(mar=rep(2,4))
barplot(y3,names.arg=n,
        xlab="Payment type",
        ylab="Number of instances",
        col="blue",
        main="Mode of payment",
        border="red")

avg_value = sqldf("SELECT payment_type,avg(payment_value) FROM payments GROUP BY payment_type")

bol=payments[payments$payment_type == "boleto",]
boleto= sqldf("SELECT payment_value FROM bol")
quantile(boleto$payment_value)

cc=payments[payments$payment_type == "credit_card",]
credit_card= sqldf("SELECT payment_value FROM cc")
quantile(credit_card$payment_value)

dc=payments[payments$payment_type == "debit_card",]
debit_card= sqldf("SELECT payment_value FROM dc")
quantile(debit_card$payment_value)

#bol=payments[payments$payment_type == "boleto",]
#not_defined= sqldf("SELECT payment_value FROM nd")
#quantile(not_defined$payment_value)

vou=payments[payments$payment_type == "voucher",]
voucher= sqldf("SELECT payment_value FROM vou")
quantile(voucher$payment_value)

barplot(boleto$payment_value, ylim = c(0,7500),main="Distribution plot for boleto transaction")
barplot(credit_card$payment_value, ylim = c(0,14000),main="Distribution plot for credit card transaction")
barplot(debit_card$payment_value, ylim = c(0,4000),main="Distribution plot for debit card transaction")
#barplot(not_defined$payment_value, ylim = c(0,14000),main="Distribution plot for not defined transaction")
barplot(voucher$payment_value, ylim = c(0,3200),main="Distribution plot for voucher transaction")
payment_type <- as.integer(payments$payment_type)
payment_value <- as.integer(payments$payment_value)
#nb=c('boleto','cc','dc','nd','vou')
boxplot(payment_value ~ payment_type, data = payments, xlab = "Transaction type",
        ylab = "Transaction value", main = "Boxplot for different payment type",
        notch = TRUE, 
        varwidth = TRUE, 
        col = c("purple"))

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Analysis on sellers
Order_seller= merge(order_Oitems,sellers,by="seller_id")
sell=sqldf("select seller_id,sum(price) from Order_seller group by seller_id")
names(sell)[names(sell) == 'sum(price)'] <- 'sum_price'
sell=sell[order(sell$sum_price),]

#Top 15 sellers of OLIST
sell_top15=tail(sell,15)
x4 = c(sell_top15$seller_id)
y4 = c(sell_top15$sum_price)
#is.vector(y)
#par(mar=rep(2,4))
barplot(y4,names.arg=x4,
      
        xlab="seller id",
        ylab="sales",
        col="blue",
        main="Top 15 sellers in Olist",
        ylim=c(0,230000),
        border="red")

#Top 3 products of top 5 sellers
Items_seller= merge(order_items,sellers, by="seller_id")
Items_seller_prod = merge(Items_seller,products,by="product_id")
aprod=sqldf("select seller id,count(distinct(product_id)) from adata group by product_category_name")
sell_top5=tail(sell,5)

a=sell_top5[1,1]
b=sell_top5[2,1]
c=sell_top5[3,1]
d=sell_top5[4,1]
e=sell_top5[5,1]

adata<- filter(Items_seller_prod, Items_seller_prod$seller_id == "a[1,1]")
adataframe= tablulate(adata)
aprod=sqldf("select product_category_name,sum(price) from adata group by product_category_name")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Product Sales

product_sales = merge(products,order_items,by="product_id")
sales_table= sqldf("select product_category_name,avg(price),avg(freight_value),count(product_category_name) from product_sales group by product_category_name")
names(sales_table)[names(sales_table) == 'product_category_name'] <- 'product'
names(sales_table)[names(sales_table) == 'price'] <- ' avg_price'
names(sales_table)[names(sales_table) == 'reight_value'] <- 'avg_freight_value'
names(sales_table)[names(sales_table) == 'count(product_category_name)'] <- 'sales'

cor(sales_table$avg_freight_value,sales_table$sales)
cor(sales_table$avg_freight_value+sales_table$` avg_price`,sales_table$sales)