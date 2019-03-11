
#################################################################
##    Marketing analytics                                      ##
##    Exercise in Segmentation                     	           ##
##    Cluster Analysis using US Cities Dataset                 ##
#################################################################

# free memory
rm(list = ls())
gc()


# Check the working directory
#getwd() 
#Setwd("C:/test/")
 
# Read the Data
CustomerTData<-read.csv("c:/Users/tugba.karakurt/Desktop/CustomerTransactions.csv",header=T)


# Show the first few rows of the data
head(CustomerTData)


#Show attributes  
attributes(CustomerTData)

# draw the data
#plot(CustomerTData) 
#with(CustomerTData, plot(BlackPercent, PerCapitaIncome, ylab="PerCapitaIncome", xlab="BlackPercent"))
#with(CustomerTData, plot(BlackPercent, UnemploymentRate, ylab="UnemploymentRate", xlab="BlackPercent"))


#################################################################
##  K-Means-Partional Clustering                               ##
#################################################################

# Kmeans Clustering model fitting
model <- kmeans(CustomerTData[-1:-2], 4) # k = 4
summary(model)


#Model attributes 
attributes(model) 

# Clusters:
model$cluster

# Cluster size:
model$size

# Assign cluster number to the original data:
CustomerTData<-cbind(CustomerTData,Cluster=model$cluster)

model$withinss


#################################################################
##  Elbow criterion                                            ##
#################################################################

# Function to plot Within groups sum of squares
wssplot <- function(data, nc=15, seed=1234)
  {
   wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc)
      {
       set.seed(seed)
       wss[i] <- sum(kmeans(data, centers=i)$withinss) 
      }
     plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  } 

# if one plots the percentage of variance explained by the clusters against the number of clusters, 
# the first clusters will add much information (explain a lot of variance), 
# but at some point the marginal gain will drop, giving an angle in the graph. 
# The number of clusters is chosen at this point, hence the “elbow criterion”.

 wssplot(CustomerTData[-1:-2], nc=12)


# Plotting CLusters. "Library" package allows to plotting the clusters into 2 dimensions:
#install.packages("cluster")
library(cluster)

# Plot Clusters in 2D
clusplot(CustomerTData[-1:-2], model$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# Show cities and their corresponding clusters:
table(CustomerTData[,1],model$cluster)


#################################################################
##  Hierarchical clustering                                    ##
#################################################################

#Find Hierarchical clustering using Euclidean distance and wards method in matrix.
d <- dist(CustomerTData[-1:-2], method = "euclidean") 
H_Model <- hclust(d, method="ward.D")



# display dendogram
plot(H_Model) 

# cut tree into 3 clusters
groups <- cutree(H_Model, k=3)

# show groups
table(CustomerTData[,1],groups)

# draw dendogram with red borders around the 3 clusters
rect.hclust(H_Model, k=3, border="blue") 

# Kmeans Clustering model fitting
model <- kmeans(CustomerTData[-1:-2], 3) # k = 3
summary(model)


#Model attributes 
attributes(model) 

# Clusters:
model$cluster

# Cluster size:
model$size

# Assign cluster number to the original data:
CustomerTData<-cbind(CustomerTData,Cluster=model$cluster)

model$withinss


#################################################################
##  Elbow criterion                                            ##
#################################################################

# Function to plot Within groups sum of squares
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss) 
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
} 


wssplot(CustomerTData[-1:-2], nc=12)



library(cluster)

# Plot Clusters in 2D
clusplot(CustomerTData[-1:-2], model$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# Show cities and their corresponding clusters:
table(CustomerTData[,1],model$cluster)


#################################################################
##  Hierarchical clustering                                    ##
#################################################################

#Find Hierarchical clustering using Euclidean distance and wards method in matrix.
d <- dist(CustomerTData[-1:-2], method = "euclidean") 
H_Model <- hclust(d, method="ward.D")


# display dendogram
plot(H_Model) 

# cut tree into 4 clusters
groups <- cutree(H_Model, k=3)

# show groups
table(CustomerTData[,1],groups)

# draw dendogram with red borders around the 3 clusters
rect.hclust(H_Model, k=3, border="blue") 

