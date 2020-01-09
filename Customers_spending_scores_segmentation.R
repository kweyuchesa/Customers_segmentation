library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
library(clValid)
library(magrittr)
library(clustertend)


customer_data<-read.csv("G:\\R Language\\R project\\Customers_segmentation\\Customers_spending_scores.csv")
str(customer_data)
names(customer_data)
head(customer_data)

summary(customer_data)

##we will create a barplot and
##a piechart to show the gender distribution across our customer_data dataset

##This is the barplot and clearly female is in majority as compare to male
a=table(customer_data$Genre)
a

barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

##visualize a pie chart to observe the ratio of male and female distribution.

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
lbs
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")

##plot a histogram to view the distribution to plot the frequency of customer ages

hist(customer_data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)


boxplot(customer_data$Age, col = 3, main="Boxplot for Descriptive Analysis of Age")
        
##maximum customer ages are between 30 and 35.
##The minimum age of customers is 18, whereas, the maximum age is 70.



###we will create visualizations to analyze the annual income of the customers by Hist plot
summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

##We will plot density plot for Annual Income
plot(density(customer_data$Annual.Income..k..),
col="yellow",
main="Density Plot for Annual Income",
xlab="Annual Income Class",
ylab="Density")##This will plot only outlier graph .it will not fill colour
polygon(density(customer_data$Annual.Income..k..),
        col="#ccff00")


##we conclude that the minimum annual income of the customers is 15 and the maximum income is 137.
##People earning an average income of 70 have the highest frequency count
##in our histogram distribution. The average salary of all the customers is 60.56



##Analysing spending score of the customers
summary(customer_data$Spending.Score..1.100.)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   34.75   50.00   50.20   73.00   99.00


boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")
#In this plot horizontal=True so we have got horizontal   boxplot


hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)

##The minimum spending score is 1,maximum is 99 and the average is 50.20
##From the histogram, we conclude that customers 
##between class 40 and 50 have the highest spending score among all the classes.

# Loading data
data<-customer_data[,-c(1,2)]
head(data)

# To standarize the variables 
data = scale(data)
head(data)

# Assessing cluster tendency
# Compute Hopkins statistic for the dataset
## values > .5 means it is not clusterable
## values < .5 means it is clusterable, closer to 0 better the data

set.seed(123)
hopkins(data, n = nrow(data)-1)
#Since the H value = 0.3137406 which is below the threshold 0.5, it is clusterable

# K-mean - Determining optimal number of clusters
# NbClust Package : 30 indices to determine the number of clusters in a dataset
# If index = 'all' - run 30 indices to determine the optimal no. of clusters
# If index = "silhouette" - It is a measure to estimate the dissimilarity between clusters.
# A higher silhouette width is preferred to determine the optimal number of clusters

nb <- NbClust(data,  distance = "euclidean", min.nc=2, max.nc=15, 
              method = "kmeans",index = "silhouette")
nb$Best.nc
nb$All.index## maximum value of silhouette shows best number of clusters
##sihouette analysis is showing the best number of cluster is 6


##Best Method IV: using all 30 ways of measure
nb <- NbClust(data,  distance = "euclidean", min.nc=2, max.nc=15, 
              method = "kmeans",index = "all")



##6 proposed 4 as the best number of clusters....
##4 proposed 2 as the best number of clusters 
##4 proposed 6 as the best number of clusters
##According to the majority rule, the best number of clusters is  4 , but a/c to analyasis and observing
##internal validation test and stability validation test , Lets take number of clusteris 6
########################## Running k means #######################################

# K-Means Cluster Analysis
fit <- kmeans(data,6)
plot(data,col=fit$cluster,pch=16) 


### kmeans clustering with a better visualization

# K-means clustering
# nstart means it initiates multiple initial configuaration and reports the best one
km.res <- eclust(data, "kmeans", k =6, nstart = 25, graph = FALSE)

# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", frame.type = "norm")


####################### Clustering Validation #################################################

#Internal clustering validation, which use the internal information of the 
#clustering process to evaluate the goodness of a clustering structure. 
#It can be also used for estimating the number of clusters and the appropriate clustering algorithm.


## Connectivity - ranges from 0 to Inf; choose the one with minimum value 
## Average Silhouette width - ranges from -1 to 1; choose the one with maximum value 
## Dunn index - ranges from 0 to Inf; choose the one with maximum value 


# Internal Validation
clmethods <- c("kmeans")
internval <- clValid(data, nClust = 2:6, clMethods = clmethods, validation = "internal")

# Summary
summary(internval)
optimalScores(internval)

##connectivity(minimum value)- 6>4>5>3>2
##silhouette width(maximum value)- 6>5>4>3>2
##Dunn index(maximum value)_ 5>2>4>6>3
#                   Score Method Clusters  cluster4 position cluster6 position
#Connectivity 15.23174603 kmeans        2        4                  5
#Dunn          0.06593646 kmeans        5        3                  4
#Silhouette    0.42742815 kmeans        6        3                  1

# Stability measure Validation
clmethods <- c("kmeans")
stabval <- clValid(data, nClust = 2:6, clMethods = clmethods, validation = "stability")

# Display only optimal Scores
summary(stabval)
optimalScores(stabval)

#The average proportion of non-overlap (APN)
#The average distance (AD)
#The average distance between means (ADM)
#The figure of merit (FOM)
### Smaller values indicate stable clusters

#Optimal Scores:
  
#    Score  Method Clusters cluster4 position   cluster6 position
#APN 0.2547 kmeans 2              3                   4
#AD  1.4826 kmeans 6              3                   1
#ADM 0.7833 kmeans 2              2                   5
#FOM 0.9327 kmeans 6              3                   1

    ## You have to run kmeans clustering with a better visualization method to make it work
    
    fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())
    
    # Silhouette width of observations
    sil <- km.res$silinfo$widths[, 1:3]
    # Objects with negative silhouette
    neg_sil_index <- which(sil[, 'sil_width'] < 0) 
    sil[neg_sil_index, , drop = FALSE]
    
    
    
    set.seed(1)
    ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
      geom_point(stat = "identity", aes(color = as.factor(fit$cluster))) +
      scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "cluster 5", "cluster 6")) +
      ggtitle("Segments of spending scoreof Customers", subtitle = "Using K-means Clustering")
    
    set.seed(1)
    ggplot(customer_data, aes(x =Annual.Income..k.., y = Age)) + 
      geom_point(stat = "identity", aes(color = as.factor(fit$cluster))) +
      scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "cluster 5", "cluster 6"))) +
      ggtitle("Segments of spending score of Customers", subtitle = "Using K-means Clustering")
     summary(customer_data)
    
    ##################################################################
    
    
    # External Clustering Validation
    library(fpc)
    
    # K-Means Cluster Analysis
    fit <- kmeans(data,6)
    
    # Compute cluster stats
    Gender <- as.numeric(customer_data$Genre)
    clust_stats <- cluster.stats(d = dist(data), Gender, fit$cluster)
    
    # Corrected Rand index and VI Score
    # Rand Index should be maximized and VI score should be minimized
    clust_stats$corrected.rand ## value should be maximized
    clust_stats$vi  ## value should be minimized
    
    
    