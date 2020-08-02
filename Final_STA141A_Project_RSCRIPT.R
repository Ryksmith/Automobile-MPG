##Read the data Auto.mpg, name the columns, attach the variables
auto.mpg <- read.table("~/Desktop/School/STA141A/Final Project/auto-mpg.data", header=FALSE)
colnames(auto.mpg) = c('mpg','cylinders','displacement','horsepower','weight','acceleration','model','origin','car_name')
attach(auto.mpg)
data = auto.mpg

##Install the necessary libraries for the R code
library(ggplot2)
library(GGally)
library(corrgram)
#install.packages("e1071")
library(e1071)
library(class)
library(MASS)
library(lattice)
#install.packages("nnet")
library(nnet)
#install.packages('ggpubr')
library(ggpubr)
#install.packages('digest')
library(digest)
library(car)
#install.packages('vcd')
library(vcd)

## Header to see the first 6 vehicles' data
head(data)

##Removing rows which have unknown horsepower to avoid errors
a = which(horsepower == '?')
data = auto.mpg[-a,]
attach(data)


##A correlogram to explore the basic relationships between the variables
corrgram(data, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, main = "Correlogram of auto_mpg.data")


##histogram of the number of cylinders, displacement of the engine, weight, acceleration(0-60mph), 
##     and MPG, to get a baseline idea of the means.
#qplot(cylinders,data = data, geom = "histogram") + geom_vline(xintercept = mean(cylinders), color = "blue" )
#qplot(displacement, data = data, geom = "histogram") + geom_vline(xintercept = mean(displacement), color = "blue")
#qplot(weight,data = data, geom="histogram") + geom_vline(xintercept = mean(weight), color = "blue" )
#qplot(acceleration, data = data, geom = "histogram") + geom_vline(xintercept = mean(acceleration, color = "blue"))
qplot(mpg, data = data, geom="histogram") + geom_vline(xintercept = mean(mpg), color = "blue" )









########Question 1:What effect does the number of cylinders, and acceleration(0-60mph) of a vehicle 
##             have on its miles per gallon(MPG)?
##A basic regression model for mpg vs cylinders and acceleration
lm1 = lm(mpg ~ cylinders + acceleration)
lm1
anova(lm1)
##The anova table shows that the ssto doesnt equal ssa + ssb. So, we are missing serious info.
plot(lm1)
##The normall QQ plot is straight for the vast majority of the data points, with the upper tail
##     becoming pronounced, so its not perfect, and the data does contain several outliers,. 
##     But the normality assumption is not strictly violated.


##Scatterplot matrix of Mpg, Acceleration, and Number of cylinders
mpgvsaccelcyl = data[,c(1,2,6)]
mpgvsaccelcyl
ggpairs(mpgvsaccelcyl)


##Correlation tests using the package ggpubr, calculating the correlation using 3 correlation formulas:
##    pearson, kendall, spearman
##Correlation between mpg and acceleration using the 3 correlation formulas
cor(mpg, acceleration, method = c("pearson"))
cor(mpg, acceleration, method = c("kendall"))
cor(mpg, acceleration, method = c("spearman"))
##Correlation test for mpg and acceleration, using the Pearson method. Interested in the p-value.
cor.test(mpg, acceleration, method=c("pearson"))


##Correlation between mpg and acceleration using the 3 correlation formulas
cor(mpg, cylinders, method = c("pearson"))
cor(mpg, cylinders, method = c("kendall"))
cor(mpg, cylinders, method = c("spearman"))
##Correlation test for mpg and cylinders, using the Pearson method. Interested in the p-value.
cor.test(mpg, cylinders, method=c("pearson"))

##Correlation between mpg and (acceleration+cylinders), using the Pearson method.
cor(mpg, cylinders+acceleration, method = c("pearson"))
##Correlation test for mpg and (acceleration+cylinders) using the Pearson method.
cor.test(mpg, cylinders+acceleration, method = c("pearson"))


##Plot of mpg vs acceleration, with color of each point being determined by the number of cylinders
qplot(x = acceleration, y = mpg, color = cylinders, data = data) + 
            stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)

##Scatterplot of MPG vs acceleration, conditional on the number of cylinders
scatterplot(mpg ~ acceleration | cylinders, data = data, lwd=2,
            main = "Scatter plot of MPG vs Acceleration By Number of Cylinders",
            xlab = "Acceleration of car(0-60mph)", ylab = "Miles per Gallon",
            legend.plot = TRUE,
            legend.coords = "topleft",
)

##Scatterplot matrix of MPG, acceleration, cylinder
scatterplotMatrix( ~ mpg + acceleration + cylinders, data = data,
                   spread=FALSE,
                   main = "Scatter plot matrix for the varibles MPG, acceleration, and cylinders.")

























#####QUESTION 2



#####LOGISTIC REGRESSION,LDA,KNN classification. FROM HW3. TRYING TO GUESS IF MPG is good.Discussion 6... HW3 sources
##Makes a new criteria, whether or not the MPG is good, which is 0 if the MPG is below the mean, and 1 if its above the mean
attach(data)
goodmpg = (mpg > mean(mpg))
class = as.numeric(goodmpg)
data.lr = data.frame(data, class)


##Sample the test and training data randomly in 70/30% split
train = sample(1:392, 274, replace=FALSE)
test = setdiff(1:392,train)

data.tr = data.lr[train,c(10,2,3,5,6)]
data.te = data.lr[test,c(10,2,3,5,6)]
#View(data.tr)
#View(data.te)
#nrow(data.tr)
#nrow(data.te)


###LDA
mpg.lda = lda(class ~ cylinders + displacement + weight + acceleration, data.tr)
mpg.lda
mpg.lda$means

mpg.pred.lda = predict(mpg.lda, data.te)
mpg.pred.lda
mpg.confusion.lda = table(true = data.te$class, predicted = mpg.pred.lda$class)
mpg.confusion.lda

mpg.pred.lda_error = (mpg.confusion.lda[1,2] + mpg.confusion.lda[2,1])/sum(mpg.confusion.lda)
mpg.pred.lda_error




####LOG REG
log_model = glm(class ~ cylinders + displacement + weight + acceleration, data.tr,
                family = binomial)
summary(log_model)


log_pred = predict(log_model, data.te, type = "response")
log_pred = (log_pred > 0.5) + 1
log_pred = log_pred - 1
log_pred
log_con = table(true = data.te$class, predicted = log_pred)
log_con

mpg.pred.glm_error = (log_con[1,2] + log_con[2,1])/sum(log_con)
mpg.pred.glm_error



###KNN k =10, 20, k=50  
knn_pred10 = knn(
  train = data.tr[,c(2,3,4,5)], 
  test  = data.te[,c(2,3,4,5)],
  cl    = data.tr$class,                   
  k     = 10
)

knn_pred20 = knn(
  train = data.tr[,c(2,3,4,5)], 
  test  = data.te[,c(2,3,4,5)],
  cl    = data.tr$class,                   
  k     = 20
)

knn_pred50 = knn(
  train = data.tr[,c(2,3,4,5)], 
  test  = data.te[,c(2,3,4,5)],
  cl    = data.tr$class,                   
  k     = 50
)

knn_pred10
knn_pred20
knn_pred50

knn_con10 = table(true = data.te$class, model = knn_pred10)
knn_con10

knn_con20 = table(true = data.te$class, model = knn_pred20)
knn_con20

knn_con50 = table(true = data.te$class, model = knn_pred50)
knn_con50

mpg.pred.knn_error10 = (knn_con10[1,2] + knn_con10[2,1])/sum(knn_con10)
mpg.pred.knn_error10

mpg.pred.knn_error20 = (knn_con20[1,2] + knn_con20[2,1])/sum(knn_con20)
mpg.pred.knn_error20

mpg.pred.knn_error50 = (knn_con50[1,2] + knn_con50[2,1])/sum(knn_con50)
mpg.pred.knn_error50

comparison = c(mpg.pred.lda_error, mpg.pred.glm_error, mpg.pred.knn_error10, mpg.pred.knn_error20, mpg.pred.knn_error50)
comparison








#############
###########
################K nearest neighbors to cluster the data.

accel_cyl = data[,c(2,6)]
attach(accel_cyl)
accel_cyl

##The number of clusters(k=1,...,10), followed by the relative vector of assignments of the observations of each k-value, followed by the within variance of that k-value of the test data-set is:
##number of rows in the data set.
n = nrow(accel_cyl)

##Function to calculate the within deviance of each cluster
withinss <- function(group, x, centers, assignments) {
  cent <- centers[group, ]
  m <- rbind(cent, x[assignments==group, 1:2])
  sum((as.matrix(dist(m))[1, ])^2)
}


##Function to predict the k cluster means of an input object
predict.kmeans <- function(object,
                           newdata,
                           method = c("centers", "classes")) {
  method <- match.arg(method)
  
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }
}


##percentage of the data to be used for the training set (70%)
train_p=0.7;
##Number of iterations
N=100
##Number of random starts
n_clust=10
within_c_n=matrix(0,n_clust,N)
within_c=vector('numeric',0)
##Rename customers_2 to dfxy for the sake of referencing discussion 9 code.
dfxy=accel_cyl

##For loop that runs 100 times for each i=1,...10
for(cl in 1:n_clust){
  for(iter in 1:N){
    ##Samples 80% of the data randomly to be used as the training set.
    train=sample(1:n,train_p*n)
    train=sort(train)
    ##Sets the remaining 20% of the data to be the test set
    test=sort(setdiff(1:n,train))
    ##Finds the mean of each of the K cluster with 20 starts, 100 iterations.
    dfxy.km_train=kmeans(dfxy[train,1:2],centers=cl, nstart = 20, iter.max = 100)
    centers <- dfxy.km_train$centers
    ##Assigns each entry to a cluster
    assignments <-  as.numeric(row.names(predict.kmeans(dfxy.km_train, (dfxy[test,1:2]))))
    within_c_n[cl,iter]=sum(sapply(seq(nrow(centers)), function(y){withinss(group=y,x = dfxy[test,1:2], centers = centers, assignments = assignments)}))
  }
  within_c[cl]=(mean(within_c_n[cl,]))
}


##Graph showing the cluster mean deviance versus the number of clusters in knn.
par(mfrow=c(1,1))
plot(within_c, type = "b")
##The within deviance is optimized when k = 3, based on the location of the elbow in the graph.










#################
#####################
##REDOING CLASSIFICATION with 3 clusters, as the knn cluster deviance graph hinted would be ideal.
attach(data)
cond_statement<- ifelse( mpg < 16, 0, ifelse(mpg > 16 & mpg < 32, 1, ifelse(mpg > 32,2,0)))
class = cond_statement
data.lr = data.frame(data, class)
attach(data.lr)



##Sample the test and training data randomly in 70/30% split
train = sample(1:392, 274, replace=FALSE)
test = setdiff(1:392,train)

data.tr = data.lr[train,c(10,2,3,5,6)]
data.te = data.lr[test,c(10,2,3,5,6)]
View(data.tr)
View(data.te)
nrow(data.tr)
nrow(data.te)


###LDA
mpg.lda = multinom(class ~ cylinders + displacement + weight + acceleration, data.tr)
mpg.lda

mpg.pred.lda = predict(mpg.lda,grouping = class,  data.te)
mpg.pred.lda
mpg.confusion.lda = table(true = data.te$class, predicted = mpg.pred.lda)
mpg.confusion.lda

mpg.pred.lda_error = (mpg.confusion.lda[1,2] + mpg.confusion.lda[1,3] + mpg.confusion.lda[2,1]+ mpg.confusion.lda[2,3]+ mpg.confusion.lda[3,1]+ mpg.confusion.lda[3,2])/sum(mpg.confusion.lda)
mpg.pred.lda_error




####LOG REG
log_model = multinom(class ~ cylinders + displacement + weight + acceleration, data.tr)
summary(log_model)


log_pred = predict(log_model, data.te, type = "class")
log_pred
log_con = table(true = data.te$class, predicted = log_pred)
log_con

mpg.pred.glm_error = (log_con[1,2]+ log_con[1,3] + log_con[2,1]+ log_con[2,3]+ log_con[3,1] + log_con[3,2])/sum(log_con)
mpg.pred.glm_error



###KNN k =10, 20, k=50  
knn_pred10 = knn(
  train = data.tr[,c(2,3,4,5)], 
  test  = data.te[,c(2,3,4,5)],
  cl    = data.tr$class,                   
  k     = 10
)

knn_pred20 = knn(
  train = data.tr[,c(2,3,4,5)], 
  test  = data.te[,c(2,3,4,5)],
  cl    = data.tr$class,                   
  k     = 20
)

knn_pred50 = knn(
  train = data.tr[,c(2,3,4,5)], 
  test  = data.te[,c(2,3,4,5)],
  cl    = data.tr$class,                   
  k     = 50
)

knn_pred10
knn_pred20
knn_pred50

knn_con10 = table(true = data.te$class, model = knn_pred10)
knn_con10

knn_con20 = table(true = data.te$class, model = knn_pred20)
knn_con20

knn_con50 = table(true = data.te$class, model = knn_pred50)
knn_con50

mpg.pred.knn_error10 = (knn_con10[1,2] + knn_con10[1,3] + knn_con10[2,1] + knn_con10[2,3] + knn_con10[3,1] + knn_con10[3,2])/sum(knn_con10)
mpg.pred.knn_error10

mpg.pred.knn_error20 = (knn_con20[1,2] +  + knn_con10[1,3] + knn_con10[2,1] + knn_con10[2,3] + knn_con10[3,1] + knn_con10[3,2])/sum(knn_con20)
mpg.pred.knn_error20

mpg.pred.knn_error50 = (knn_con50[1,2] + knn_con10[1,3] + knn_con10[2,1] + knn_con10[2,3] + knn_con10[3,1] + knn_con10[3,2])/sum(knn_con50)
mpg.pred.knn_error50

comparison = c(mpg.pred.lda_error, mpg.pred.glm_error, mpg.pred.knn_error10, mpg.pred.knn_error20, mpg.pred.knn_error50)
comparison











###################
#######Q3: What factors(number of cylinders, engine displacement, car weight, or acceleration)
###              affect the MPG of a vehicle the most?
mpg.cdwa.data = data[,c(1,2,3,5,6)]
corrgram(mpg.cdwa.data, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlogram of mpg, cylinders, engine \n displacement, car weight, and acceleration")

##Multiple linear regression model with mpg versus number of cylinders, engine displacement, weight, acceleration
lm(mpg~cylinders + displacement + weight +acceleration)
fit1 = lm(mpg ~cylinders + displacement  + weight + acceleration)
fit1
fit2 = lm(mpg~cylinders + displacement + weight)
fit2
fit3 = lm(mpg~cylinders + displacement)
fit3
fit4 = lm(mpg~cylinders)
fit4
anova(fit1,fit2,fit3,fit4)  


##Finds the correlation of mpg and cylinders, using the pearson correlation formula
cor(mpg, cylinders, method = c("pearson"))
##Correlation test for mpg and cylinders, using the Pearson method. Interested in the p-value.
cor.test(mpg, cylinders, method=c("pearson"))

##Finds the correlation of mpg and engine displacement, using the pearson correlation formula
cor(mpg, displacement, method = c("pearson"))
##Correlation test for mpg and engine displacement, using the Pearson method. Interested in the p-value.
cor.test(mpg, displacement, method=c("pearson"))

##Finds the correlation of mpg and car weight, using the pearson correlation formula
cor(mpg, weight, method = c("pearson"))
##Correlation test for mpg and weight, using the Pearson method. Interested in the p-value.
cor.test(mpg, weight, method=c("pearson"))

##Finds the correlation of mpg and acceleration, using the pearson correlation formula
cor(mpg, acceleration, method = c("pearson"))
##Correlation test for mpg and acceleration, using the Pearson method. Interested in the p-value.
cor.test(mpg, acceleration, method=c("pearson"))
