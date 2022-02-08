library('psych')
library('corrplot')
library ('cluster')
library ('gmodels')
library('dplyr')
library('ggplot2')
library("beeswarm")

# Data 
dataset <- read.csv(file='/Users/bernardoaugusto/Downloads/marketing_campaign.csv', sep="\t", header=TRUE)

sapply(dataset, typeof)

colnames(dataset)

# Data Preparation
data = dataset[, c('Year_Birth', 'Education', 'Marital_Status', 'Income', 'Kidhome', 
                   'Teenhome', 'Recency', 'Complain', 'MntWines', 'MntFruits',
                   'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds', 
                   'NumWebPurchases', 'NumCatalogPurchases', 'NumStorePurchases', 'NumWebVisitsMonth')]

sum(is.na(data))

data=na.omit(data)

sum(is.na(data))

# transform into numeric 
data$Education[which(data$Education == "Basic")] <- 1
data$Education[which(data$Education == "2n Cycle")] <- 2
data$Education[which(data$Education == "Graduation")] <- 3
data$Education[which(data$Education == "Master")] <- 4
data$Education[which(data$Education == "PhD")] <- 5
as.numeric(data$Education)

# transform kidhome and teenhome into 1 variable - Parents (1 or 0)
data$Parents[which(data$Kidhome == 1 | data$Teenhome == 1)] <- 1
data$Parents[which(data$Kidhome == 0 | data$Teenhome == 0)] <- 0
as.numeric(data$Parents)

# Living with - 1 (Partner) or 0 (Alone)
data$Partner[which(data$Marital_Status == "Married" | data$Marital_Status == "Together")] <- 1
data$Partner[which(data$Marital_Status == "Single" | data$Marital_Status == "Absurd" | data$Marital_Status == "Widow" | data$Marital_Status == "YOLO" | data$Marital_Status == "Divorced")] <- 0
as.numeric(data$Partner)

# Number of Children 
data$Children=data$Kidhome+data$Teenhome
as.numeric(data$Partner)

# Family_size
data$Family_size[which(data$Partner == 1)] <- 2  
data$Family_size[which(data$Partner == 0)] <- 1 
data$Family_size<-data$Family_size+data$Children
as.numeric(data$Family_size)

# age
data$Age <- 2021 - data$Year_Birth 
as.numeric(data$Age)

data$TotalSpent <- data$MntFishProducts+data$MntFruits+data$MntMeatProducts+data$MntWines+
        data$MntSweetProducts+data$MntGoldProds

# Data cleaning
data<-data[!(data$Income==666666 | data$MntMeatProducts==1725),]
data<-data[!(data$Income==2447 | data$MntMeatProducts==1725),]
data<-filter(data, NumCatalogPurchases < 10)
data<-filter(data, Income < 150000)
data<-filter(data, NumWebVisitsMonth<13)
data<-filter(data, NumWebPurchases<20)
data<-filter(data, MntMeatProducts<1500)

# EDA
# covariance
data_covar=data[, c('Age', 'Education', 'Income', 'Kidhome', 
                              'Teenhome', 'Recency', 'Complain', 'MntWines', 'MntFruits',
                              'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds', 
                              'NumWebPurchases', 'NumCatalogPurchases', 'NumStorePurchases', 'NumWebVisitsMonth')]
round(var(data_covar),3)

#Boxplot para verificar outliers
boxplot(data$Income,
        main = "Perceptions_of_Income",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

boxplot(data$MntMeatProducts,
        main = "Perceptions_of_MntMeatProducts",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

boxplot(data$MntWines,
        main = "Perceptions_of_MntWines",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

boxplot(data$TotalSpent,
        main = "Perceptions_of_TotalSpent",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

boxplot(data$NumWebPurchases,
        main = "Perceptions_of_NumWebPurchases",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

boxplot(data$NumCatalogPurchases,
        main = "Perceptions_of_NumCatalogPurchases",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

boxplot(data$NumStorePurchases,
        main = "Perceptions_of_NumStorePurchases",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

boxplot(data$NumWebVisitsMonth,
        main = "Perceptions_of_NumWebVisitsMonth",
        col = "#4ADCD2",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE)

# scale the data
df=data[, c('Income', 'MntMeatProducts','MntFruits','MntFishProducts',
            'MntWines','MntSweetProducts','MntGoldProds', 'NumWebPurchases', 
            'NumCatalogPurchases', 'NumStorePurchases', 'NumWebVisitsMonth')]

dataZ <- data.frame(scale(df[,1:11]))

#scatterplot
plot(dataZ[,1:11], pch = 19, lower.panel = NULL)

correlation <- cor(dataZ)

par(oma = c(0, 0, 0, 0))
corrplot.mixed(correlation, 
               order = "hclust", 
               upper = "ellipse",
               mar=c(0,0,0,0)
)

#Correlation matrix
round(correlation, 3)

#Bartlett test and KMO
#Input is the correlation matrix
cortest.bartlett(correlation)
KMO(correlation)

pc11 <- principal(dataZ, nfactors=11, rotate="none", scores=TRUE)  

#Eigenvalues - Variances of the principal components 
round(pc11$values,3)

#Screeplot 
plot(pc11$values, type = "b", main = "Scree plot",
     xlab = "Number of PC", ylab = "Eigenvalue") 

# Eigenvectors - component loadings
pc11$loadings

# PC1 and PC2 have positives and negative loadings, which suggests a divide
# but PC1 has only one negative loading which can be thought as an overall 
# measure of performance (notice the size of the loadings)

#Communalities
pc11$communality

pc2 <- principal(dataZ, nfactors=2, rotate="none") 
pc2

#Let's rotate the 2 components using varimax
pc2r <- principal(dataZ, nfactors=2, rotate="varimax") 
pc2r$loadings

# RCA1 - MntEssential, MntFruits, MntFish, MntSweet (maybe have Parents)?
# RCA2 - MntIncome, MntNonEssential, MntGold (dont have Parents)?


#Communalities
round(pc2r$communality,3)

####STEP 4 - PCA####
#Compute the scores
pc2sc <- principal(dataZ, nfactors=2, rotate="none", scores=TRUE)  
round(pc2sc$scores,3)
mean(pc2sc$scores[,1])
sd(pc2sc$scores[,1])

#Adicionar os scores ao dataset
data$Minimalism <- pc2sc$scores[,1]
data$Consumerism <- pc2sc$scores[,2]
plot(data$Minimalism, data$Consumerism, pch = 19, xlab="Minimalism", ylab="Consumerism")
# scatterplot3d(data[,13:15], angle = 55, color="#682F2F")

#plot(df$idk, pch = 19, xlab="MntEssential, MntFruits, MntFish, MntSweet", ylab="MntIncome, MntNonEssential, MntGold", main = "Scores: idk vs idk2")
#text(df$idk, df$idk2-0.1, data$Income)

##### with pcas
df_pca = data[, c('Minimalism','Consumerism', 'Education', 'Marital_Status', 'Kidhome', 
                  'Teenhome', 'Recency', 'Complain', 'Parents', 'Partner', 'Family_size','Children', 'Age')]

#### CLUSTERING ####
# Hierarchical clustering - hclust
# Standardized * Euclidian * Ward.D2 
demodist <- dist(scale(df_pca[,1:2])) 
hclust_demo <- hclust(demodist,method='ward.D2')
plot(hclust_demo,label=data[,1],hang=-1)

# Cortar o dendrograma em 2 clusters
groups.k4 <- cutree(hclust_demo, k=4) 
rect.hclust(hclust_demo, k=4, border="#4ADCD2") 
aggregate((df_pca[,1:2]),list(groups.k4), mean)

#Silhouette
plot(silhouette(groups.k4,demodist)) 

###K-means
# K-Means
std_data <- scale((df_pca[,1:2]))
kmeans.k4 <- kmeans(std_data, 4,nstart=100) 
# k = 3 from hclust, nstart = initial random solutions
attributes(kmeans.k4)  # all elements of the cluster output
kmeans.k4$centers
kmeans.k4$cluster
kmeans.k4$size

#Silhouette
plot(silhouette(kmeans.k4$cluster,demodist))

#Adicionar os clusters ao dataset para perceber o que cada cluster significa (apenas para facilitar interpretacao)
data$clusters <- kmeans.k4$cluster

#PAM clustering com 2 clusters
#std_data <- scale((data[,27:28]))
#pam.k2 <- pam(std_data,2)
#table(data$Family_size,pam.k2$clustering)
#plot(data$Minimalism, data$Consumerism, type="n", xlab="Minimalism", ylab="Consumerism")

# distribution of the clusters
counts <- table(data$clusters)
barplot(counts, main="Cluster Distribution", xlab="Clusters", col=c("#001f2b","#03254c","#00719c","#3399FF"))

# Profiling
#### change colors ####
# scatter plots #
ggplot(data=data, aes(x=TotalSpent, y=Income), xName='TotalSpent',yName='Income',
       groupName="clusters")+ geom_point(aes(color = clusters))

ggplot(data=data, aes(x=Minimalism, y=Consumerism), xName='Minimalism',yName='Consumerism',
       groupName="clusters")+ geom_point(aes(color = clusters))

# swarm plots
# cluster x Income
beeswarm(Income ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x TotalSpent
beeswarm(TotalSpent ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x MntMeatProducts
beeswarm(MntMeatProducts ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x MntFruits
beeswarm(MntFruits ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x MntFishProducts
beeswarm(MntFishProducts ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x MntWines
beeswarm(MntWines ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x MntSweetProducts
beeswarm(MntSweetProducts ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x MntGoldProds
beeswarm(MntGoldProds ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

# cluster x Age
beeswarm(Age ~ clusters, data=data, col=c("#001f2b","#03254c","#00719c","#3399FF"), pch=19, method="swarm", cex=0.5)

### Parents 
tbl <- with(data, table(clusters, Parents))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Parents x Clusters")

### Children 
tbl <- with(data, table(clusters, Children))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Children x Clusters")

### Education
tbl <- with(data, table(clusters, Education))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Education x Clusters")

### Partner
tbl <- with(data, table(clusters, Partner))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Partner x Clusters")

### Family Size
tbl <- with(data, table(clusters, Family_size))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Family Size x Clusters")

### Catalog Purchases
tbl <- with(data, table(clusters, NumCatalogPurchases))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Catalog Purchases x Clusters")

### Web Purchases
tbl <- with(data, table(clusters, NumWebPurchases))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Web Purchases x Clusters")

### Store Purchases
tbl <- with(data, table(clusters, NumStorePurchases))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Store Purchases x Clusters")

### Num Web Visits Month
tbl <- with(data, table(clusters, NumWebVisitsMonth))
barplot(tbl, beside = TRUE, legend = TRUE, col=c("#001f2b","#03254c","#00719c","#3399FF"), main="Num Web Visits Month x Clusters")




