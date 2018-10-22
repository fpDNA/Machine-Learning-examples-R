
## Section 1: Loading Libraries and data ---------------------------------------------------------
#install.packages("kknn")
#install.packages("Stat2Data")

## Load the "data.table", "ggplot2", "car", "kknn", and "stats" libraries. 
library(data.table)
library(ggplot2)
library(car)
library(kknn)
library(stats)

## Load the "Salaries" dataset and convert it to a data.table.
data("Salaries")
Salaries <- data.table(Salaries)
Salaries

## Section 2: K-means ---------------------------------------------------------


# run k-means on data 
for_kmeans <- copy(Salaries)
for_kmeans$rank <- NULL 
for_kmeans$discipline <- NULL 
for_kmeans$sex <- NULL 
kmeans_output <- kmeans(for_kmeans, centers=3) 
Salaries[, kmeans_cluster:= factor(kmeans_output$cluster)]

#plot the result
plot_1 <- ggplot(Salaries, aes(x=yrs.since.phd, y=salary)) +
  geom_point(aes(color=kmeans_cluster))
print(plot_1)

## Repeat the process above, with a different K 

kmeans_output <- kmeans(for_kmeans, centers=5)
Salaries[, kmeans_cluster:= factor(kmeans_output$cluster)]
plot_2 <- ggplot(Salaries, aes(x=yrs.since.phd, y=salary)) +
  geom_point(aes(color=kmeans_cluster))
print(plot_2)



## Section 3: k-NN ---------------------------------------------------------

## Split the Salaries dataset into a training set with 85% of the data, and 
## a testing set with 15%. 
# shuffle data
new_order <- sample(nrow(Salaries))
Salaries <- Salaries[new_order]  # randomly shuffle data

#split data
#397*0.15
testing_set <- Salaries[1:60]
training_set <- Salaries[61:nrow(Salaries)]

#Perform k-NN
knn_output <- kknn(rank ~ ., training_set, testing_set, k = 20)
testing_set[, `:=`(predicted_rank, knn_output$fitted.values)]

# visuaizations
knn_plot <- ggplot(testing_set, aes(x = yrs.since.phd, y = salary)) + geom_point(aes(color = rank), 
                                                                         size = 5, alpha = 0.3) + geom_point(aes(color = predicted_rank)) + geom_point(data = training_set,
                                                                                                                                                          aes(color = rank), alpha = 0.4, shape = 2) + labs(title = "Predicted (dark) vs Real (opaque) rank, k=20")
print(knn_plot)

# percent_correct <- (nrow(test_set[rank==predicted_rank])/nrow(test_set))*100
#print(percent_correct)

## Run k-NN above, with a different k  

#k-NN2
knn_output1 <- kknn(rank ~ ., training_set, testing_set, k = 50)
testing_set[, `:=`(predicted_rank1, knn_output1$fitted.values)]
# higher K value, less accurate prediction.

# visualizations
knn_plot1 <- ggplot(testing_set, aes(x = yrs.since.phd, y = salary)) + geom_point(aes(color = rank), 
                                                                                 size = 5, alpha = 0.3) + geom_point(aes(color = predicted_rank1)) + geom_point(data = training_set,
                                                                                                                                                               aes(color = rank), alpha = 0.4, shape = 2) + labs(title = "Predicted (dark) vs Real (opaque) rank, k=50")
print(knn_plot1)


## Section 4: Logistic Regression ---------------------------------------------------------


## Create a new column in the Salaries dataset called is.prof, which is equal to 1
## if the faculty member is a full professor, and equal to 0 otherwise. 


new = Salaries[,is.prof:= ifelse(rank=="Prof", 1, 0)]
print(new)
## Run a logistic regression, with is.prof as the outcome variable and "yrs.since.phd"
## and "sex" as the predictor variables.

#logistic regression 

lr_is.prof <- glm(is.prof~ yrs.since.phd + sex, family = binomial, data = new)
summary(lr_is.prof)

## By how much does an additional year since Ph.D affect the probability of someone being a full professor?

#exp(0.23025)=  1.258915
#The coefficient for yrs.since.phd is 0.23025, exp(0.23025)=1.258915
#An additional year since Ph.D improves the probability of someone being a full professor by 26%.

## How much more/less likely is a man to be a professor than a woman with the same number of years since Ph.D?
##exp (-0.6777)
## 1-0.5077835
## There is a 49% reduction in the likelihood that a women is a professor with the same number of years since phd.

#plot
Salaries[, pred.is.prof:=predict(lr_is.prof, type="response")]

lr_plot <- ggplot(Salaries, aes(x=yrs.since.phd)) +
  geom_point(aes(y=is.prof, color=sex))+
  geom_line(aes(y=pred.is.prof, color= sex), size=1)

print(lr_plot)
