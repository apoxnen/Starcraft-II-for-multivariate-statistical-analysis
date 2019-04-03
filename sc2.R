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
D$HoursPerWeek = cut(data$HoursPerWeek,c(0,10,30,60,90,200))
levels(D$HoursPerWeek) = c("0-10","11-30","31-60","61-90",">90")
hist(D$HoursPerWeek)

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

D$APM = cut(data$APM,c(0,50,100,200,300,500))
levels(D$APM) = c("0-50","50-100","100-200","200-300",">300")
hist(D$APM)

table(D$APM)
# Based on this the bins should focus more on the lower end


#PCA
pca_cols <- D[c(1,3,5)]
D.pca <- princomp(data, cor=FALSE)

# MCA
library(ca)
D.mca <- mjca(pca_cols,lambda="indicator")
summary(D.mca)

plot(D.mca, arrows=c(T,T), title("MCA")) # MCA performed on league, APM and hours played by week

test_data <- D[c(1,5,6,10)]
test_data.mca <- mjca(test_data, lambda="indicator")
summary(test_data.mca)

plot(test_data.mca, arrows=c(T,T), title("MCA")) # MCA performed on league, APM and TotalMapExp and complex ability used
