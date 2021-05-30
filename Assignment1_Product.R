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
boxplot(data = meltData, value~variable)

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


# end of data cleansing part 1


# boxplots after removing outliers
meltData <- melt(data_outliers.removed[-1])
boxplot(data = meltData, value ~ variable)

# aggregation
product.agg <-
    data_outliers.removed %>%
    group_by(StockCode) %>%
    mutate(Revenue = sum(Quantity*UnitPrice)) %>%
    mutate(No_of_visits = length(unique(InvoiceNo))) %>%
    mutate(No_of_distinct_customers = length(unique(CustomerID))) %>%
    mutate(Total_units_sold = sum(Quantity)) %>%
    select(Revenue,No_of_visits,No_of_distinct_customers,Total_units_sold) %>%
    distinct()


product.agg<-product.agg %>% select (Revenue,No_of_visits,No_of_distinct_customers,Total_units_sold)
boxplot(product.agg[-1])

# Removing outliers

product.agg.remove_outlier <- as.data.frame(lapply(product.agg[-1],function(x) { remove_outliers(x)}))

product.agg.remove_outlier <- na.omit(product.agg.remove_outlier)

product.agg$index <- as.numeric(rownames(product.agg))
product.agg.remove_outlier$index <-  as.numeric(rownames(product.agg.remove_outlier))

product.agg.clean = merge(x = product.agg[c("StockCode", "index")], y = product.agg.remove_outlier, by = "index")

product.agg.remove_outlier<-product.agg.remove_outlier %>% select (Revenue,No_of_visits,No_of_distinct_customers,Total_units_sold)

# product boxplot after removing outliers
boxplot(product.agg.remove_outlier)

# scaling data
product.agg.scaled <- scale(product.agg.remove_outlier[-5])


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
fviz_nbclust(product.agg.scaled, kmeans, method = "wss")
plot(withinSSrange(product.agg.scaled, 1 , 50, 150)) 

# gap method
fviz_nbclust(product.agg.scaled, kmeans, method = "gap_stat")

# perform kmeans
product.kmeans = kmeans(product.agg.scaled, 4, 150)

# centroids
realCenters = unscale(product.kmeans$centers, product.agg.scaled)

# table with customer clusters
clusteredProd = cbind(product.agg.remove_outlier, product.kmeans$cluster) 

# rename cluster column
colnames(clusteredProd)[6] <- "cluster"

# cluster matrix
plot(clusteredProd[,1:4], col = product.kmeans$cluster)

Cluster <- as.data.frame(realCenters)

lapply(Cluster,function(x) {barplot(as.matrix(x),names.arg=c("cluster1","cluster2","cluster3","cluster4"),beside=T)})

Cluster<-final %>% group_by(cluster) %>% count()
