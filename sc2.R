library(MASS)

data <- read.csv("starcraft.csv")
View(data)

D <- data[c(2:6,16:21)]
head(D)

dataset <- data[c(2,6,7,20)]
head(dataset)

# League values 1-8: Bronze, Silver, Gold, Diamond, Master, GrandMaster, Professional leagues
label <- c("Bronze", "Silver", "Gold", "Diamond", "Master", "GrandMaster", "professional", "world-class")
cols <- c("burlywood", "azure2", "gold", "deeppink", "red", "orange", "purple", "cyan")

# Scatter plot:
plot(dataset, panel = function(x,y) {text(x,y,xpd=T, col = cols)}) 

# Univariate analysis of included variables:

# League
hist(dataset$LeagueIndex, breaks = 9, col=cols, labels=label, xlab = "League", xaxt="n",
     main = "Histogram of leagues")

# Means and variances:
dataset.mean <- colMeans(dataset)
dataset.var <- lapply(dataset, function(x) {var(x)} )

# APM boxplot in relation to league
library(ggplot2)
league_apm <- D[c(1,5)]

# APM x League
boxplot(data$APM~data$LeagueIndex,
        data=data,
        main="Boxplot of APMs for each league",
        xlab="League",
        ylab="APM",
        col=cols,
        border="brown",
        names=label
)

# Select the hotkeys x League
boxplot(dataset$SelectByHotkeys~dataset$LeagueIndex,
        data=dataset,
        main="Boxplot of select by hotkey per timestamp for each league",
        xlab="League",
        ylab="Select by hotkey",
        col=cols,
        border="brown",
        names=label
)

# Select the hotkeys x League
boxplot(dataset$ComplexAbilityUsed~dataset$LeagueIndex,
        data=dataset,
        main="Boxplot of complex abilities used per timestamp for each league",
        xlab="League",
        ylab="Complex abilities used per timestamp",
        col=cols,
        border="brown",
        names=label
)


# Clustering
cluster_vars <- dataset
# Create labels for each league
#Scatterplot:
plot(cluster_vars, panel = function(x,y) {text(x,y,labels=label,xpd=T)})

#K-means
library(cluster)

set.seed(5000) #100 and 5000
k.mean <- kmeans(cluster_vars[,-1],centers=length(label))
table(k.mean$cluster,cluster_vars[,-1])

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




clusplot(cluster_vars[,-1],k.mean$cluster,main="Clustering to leagues",color=T,shade=T, labels=)
plot(cluster_vars[,1], col=cols, pch=16)  # Perhaps just using the standard plot() is simpler