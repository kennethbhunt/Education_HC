#Data set: education.csv
#Group the 50 states into classes through a hierarchical cluster analysis, using the
#variables urban, income, teen and expend. Which would be the optimal number of
#classes? Give a relevant name to each class (cluster).

Educ <- read.csv('education.csv', stringsAsFactors = F)

Educ_work <- Educ[, -1]
Educ_work <- Educ[, -2]
Educ_work <- Educ[, -3]
Educ_work <- Educ[, -1]
Educ_work <- Educ[, -1]
Educ_work$region <-NULL

rownames(Educ_work) <-Educ$state

#compute the distance matrix 

dm <-dist(Educ_work, method="euclidean")
dm

#Create clustering model 
model <- hclust(dm, method="ward.D")

#Plot the model as a dendogram
plot(model, labels = rownames(Educ_work))


member <- cutree(model, k=3)
member
rect.hclust(model, k=3, border="red")

## Compute some summary data for each cluster 

Educ2 <- cbind(Educ, cluster3=member)
View(Educ2)

##Compue mean of each clustering variable 

library(dplyr)

educ_summ <- Educ2 %>% group_by(cluster3) %>%
  summarize_each(funs(mean), urban, income, teen, expend)
educ_summ

split(Educ2$state, factor(Educ2$cluster3))

