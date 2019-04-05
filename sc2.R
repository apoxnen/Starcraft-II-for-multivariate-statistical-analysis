library(MASS)

data <- read.csv("starcraft.csv")
View(data)

D <- data[c(2:6,16:21)]
head(D)

# League values 1-8: Bronze, Silver, Gold, Diamond, Master, GrandMaster, Professional leagues

# Unique values in HoursPerWeek
unique(D[,3])
# Based on these, we bin these to groups:
# 1 => 0  - 10
# 2 => 10 - 30
# 3 => 30 - 60
# 4 => 60 - 90
# 5 => 90 - 200
D$HoursPerWeek_bins = cut(data$HoursPerWeek,c(0,10,30,60,90,200))
levels(D$HoursPerWeek_bins) = c("0-10","11-30","31-60","61-90",">90")

#Unique values in APM
(unique(D[,5]))
max(unique(D[,10]))

complex_abilities <- D$ComplexAbilityUsed * D$MaxTimeStamp
complex_units <- D$ComplexUnitsMade * D$MaxTimeStamp


# Based on these, we bin APM to groups:
# 1 => 0  - 50
# 2 => 50 - 150
# 3 => 150 - 250
# 4 => 250 - 350
# 5 => 350 - 500

D$APM_bins = cut(data$APM,c(0,50,100,200,300,500))
levels(D$APM_bins) = c("0-50","50-100","100-200","200-300",">300")


#PCA
pca_cols <- D[c(1,3,5)]
D.pca <- princomp(data, cor=FALSE)

# Univariate analysis of included variables:

# League
label <- c("Bronze", "Silver", "Gold", "Diamond", "Master", "GrandMaster", "semi-professional", "professional")
cols <- c("burlywood", "azure2", "gold", "deeppink", "red", "orange", "purple", "cyan")
hist(D$LeagueIndex, breaks = 9, col=cols, labels=label, xlab = "League", xaxt="n",
     main = "Histogram of leagues")

# APM boxplot in relation to league
library(ggplot2)
league_apm <- D[c(1,5)]
boxplot(data$LeagueIndex, data$APM, col = cols)

boxplot(data$APM~data$LeagueIndex,
        data=data,
        main="Boxplot of APMs for each league",
        xlab="League",
        ylab="APM",
        col=cols,
        border="brown",
        names=label
)


#WORKING ANALYSIS:

# Clustering
cluster_vars <- data[c(2,6,16)]
# Create labels for each league
#Scatterplot:
plot(cluster_vars, panel = function(x,y) {text(x,y,labels=label,xpd=T)})

#K-means
library(cluster)

set.seed(100) #500 yields nicely differing results for 3 centers
k.mean <- kmeans(cluster_vars[,-1],centers=length(label))
table(k.mean$cluster,cluster_vars[,-1])
# zero wrong ones in the 1st category
# 95 right ones in the 2nd category

# League colors to cluster_vars
n <- nrow(cluster_vars)

cols <- rep(NA,n)
cols[cluster_vars$LeagueIndex == 1] <- "burlywood"
cols[cluster_vars$LeagueIndex == 2] <- "azure2"
cols[cluster_vars$LeagueIndex == 3] <- "gold"
cols[cluster_vars$LeagueIndex == 4] <- "deeppink"
cols[cluster_vars$LeagueIndex == 5] <- "red"
cols[cluster_vars$LeagueIndex == 6] <- "orange"
cols[cluster_vars$LeagueIndex == 7] <- "purple"
cols[cluster_vars$LeagueIndex == 8] <- "cyan"




clusplot(cluster_vars[,-1],k.mean$cluster,color=T,shade=T)
plot(cluster_vars[,-1], col=cols, pch=16)  # Perhaps just using the standard plot() is simpler

