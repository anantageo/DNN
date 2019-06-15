library(MASS)
#install.packages("h2o")
library(h2o)

setwd("D:/wolsongdeep")
dir()
DataFrame <- read.csv("deepl.csv")
head(DataFrame)


#setting seed
set.seed(123)

#storing data
#data.frame<-Boston
#help("deepl.csv")

str(DataFrame)
#histogram
hist(DataFrame$PA)


#check the dimention of this data frome
dim(DataFrame)

#check first 3 row
head(DataFrame,3)

#check the summary of each variable(gives min and max of each variable)
apply(DataFrame,2, range)

#normalization data interval to [0,1] i.e. mean 0 and SD 1
maxValue<-apply(DataFrame,2,max)+
minValue<-apply(DataFrame,2,min)+
DataFrame<-as.data.frame(scale(DataFrame,center = minValue,scale = maxValue-minValue))+

#initiallization of h2o
#library(h2o)
#install.packages("h2o")
#library(h2o)
h2o.init(ip="localhost",port = 54321,max_mem_size = "2650m")

#defining x and y
y<-"PA"
x<-setdiff(colnames(DataFrame),y)
x
#creat the train and test data set
ind<-sample(1:nrow(DataFrame),5000)
trainDF<-DataFrame[ind,]
testDF<-DataFrame[-ind,]

#fitting the model
?h2o.deeplearning

#x=vector containing character names of the predictor in the model
#y=name of the response variable in the model
#activation =tanh, tanhwith dropout, rectifier, rectifierwithdropout maxout, etc
#input_dropout_ratio=fraction of features for each
#training row to be omitted in training(to do sampling)
#l1, l2 =regularization
#l1=makes weight 0
#l2=makes weight nearly zero not exactly zero
#loss="automatic", "crossentropy" (for classification only),"quadratic","absolute"
#stopping metric="auto", AUC,r2, logloss,etc
##stopping tolerance metric-based stopping criterion
###nfolds=no. of folds for crossvalidation

model<-h2o.deeplearning(x=x,
                        y=y,
                        seed = 1234,
                        training_frame = as.h2o(trainDF),
                        nfolds = 3,
                        stopping_rounds = 7,
                        epochs = 400,
                        overwrite_with_best_model = TRUE,
                        activation = "Tanh",
                        input_dropout_ratio = 0.1,
                        hidden = c(10,10,10, 10),
                        l1 = 6e-4,
                        loss = "Automatic",
                        distribution = "AUTO",
                        stopping_metric = "MSE")
#to check model description
model
plot(model)
#prediction
predictions<-as.data.frame(predict(model,as.h2o(testDF)))

predictions
str(predictions)
#so its a dataframe of whose predict column has to be accessed
#mse
sum((predictions$predict-testDF$PA)^2)/nrow(testDF)

#plotting actual vs prediction values
plot(testDF$PA, predictions$predict,col='blue',main = 'Real vs Predicted',
     pch=1, 
     cex=0.9,
     type = "p",xlab = "Actual", ylab = "Predicted")
abline(0,1,col="black")
h2o.shutdown(prompt = FALSE)





#plotting actual vs prediction values
plot(predictions$predict,testDF$PA, col='blue',main = 'Real vs Predicted',
     pch=1, 
     cex=0.9,
     type = "p",xlab = "Predicted", ylab = "Actual")
abline(0,1,col="black")
h2o.shutdown(prompt = FALSE)
