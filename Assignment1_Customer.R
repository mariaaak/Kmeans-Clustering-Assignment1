library(DMwR)
library(dplyr)
library(factoextra)
library(reshape)

# read data
online_retail_df <- read.csv(file="/Users/maria/Downloads/OnlineRetail.csv")

# data summary
summary(online_retail_df)

set.seed(5580)

# boxplot
meltData <- melt(online_retail_df)
boxplot(data = meltData, value ~ variable)

# The boxplot suggests that we need to remove outliers 

remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    return(y)
}

# outlier columns
data_outliers <- online_retail_df[c("InvoiceNo", "Quantity", "UnitPrice")]

# apply remove_outlier function to the data
data_outliers.removed <- lapply(data_outliers,function(x) { remove_outliers(x)})

# convert list to dataframe and remove all NA rows
data_outliers.removed <- na.omit(as.data.frame(data_outliers.removed))

# assigning an index column so we can merge both data
data_outliers.removed$index <- as.numeric(rownames(data_outliers.removed))
online_retail_df$index <-  as.numeric(rownames(online_retail_df))

data_outliers.removed = merge(x = data_outliers.removed, 
                              y = online_retail_df[c("StockCode","Description",
                                                     "InvoiceDate","CustomerID",
                                                     "Country","InvoiceDateTime",
                                                     "index")], 
                              by = "index")

# end of data cleaning part 1

# boxplot after removing outliers
meltData <- melt(data_outliers.removed[-1])
boxplot(data = meltData, value ~ variable)

# aggregation
customer.agg <-
    data_outliers.removed %>%
    group_by(CustomerID) %>%
    mutate(Revenue = sum(Quantity*UnitPrice)) %>%
    mutate(No_of_visits = length(unique(InvoiceNo))) %>%
    mutate(No_of_distinct_prod = length(unique(StockCode))) %>%
    mutate(No_of_prod = sum(Quantity)) %>%
    mutate(Average_amount_spent_per_customer = sum(Quantity*UnitPrice)/length(unique(InvoiceNo))) %>%
    select(CustomerID,Revenue,No_of_visits,No_of_distinct_prod,No_of_prod,Average_amount_spent_per_customer) %>%
    distinct()

# customer boxplots
boxplot(customer.agg)
boxplot(customer.agg$CustomerID)

# As you can see, customer with id 0 is the outlier, so we remove it

customer.agg <- customer.agg[customer.agg$CustomerID != 0,]
customer.agg <- as.data.frame(lapply(customer.agg,function(x) { remove_outliers(x)}))
customer.agg <- na.omit(customer.agg)

# customer boxplot after removing outliers
boxplot(customer.agg)

# scaling data
customer.agg.scaled <- scale(customer.agg[-1])


withinSSrange <- function(data,low,high,maxIter)
{
    withinss = array(0, dim = c(high - low + 1));
    for (i in low:high)
    {
        withinss[i - low + 1] <- kmeans(data, i, maxIter)$tot.withinss
    }
    withinss
}  


# elbow method
plot(withinSSrange(customer.agg.scaled, 1 , 50, 150)) 

# Verifying with Silhouette method

fviz_nbclust(customer.agg.scaled, kmeans, method = "gap_stat")

# perform kmeans
customer.kmeans = kmeans(customer.agg.scaled, 4, 150) 

# centroids
realCenters = unscale(customer.kmeans$centers, customer.agg.scaled)

# table with customer clusters
clustered_cust = cbind(customer.agg[-1], customer.kmeans$cluster) 

# rename cluster column
colnames(clustered_cust)[6] <- "cluster"

# cluster matrix
plot(clustered_cust[,1:5], col = customer.kmeans$cluster)

clustered_cust$CustomerId = customer.agg$CustomerID

clustered_cust %>% group_by(cluster) %>% count()
