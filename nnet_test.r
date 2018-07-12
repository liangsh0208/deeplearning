library(nnet)
library(mlbench)
set.seed(1)
ir<- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets<- class.ind(c(rep("s",50),rep("c",50),rep("v",50)))
samp<- c(sample(1:50,25),sample(51:100,25),sample(101:150,25))

ir1<- nnet(ir[samp,],
           targets[samp,],
           size = 2,
           rang = 0.1,
           softmax = F,
           censored = T,
           decay = 5e-4,
           maxit = 200
           )

test.cl<- function(true,pred){
  true<- max.col(true)
  cres<- max.col(pred)
  table(true,cres)
}

test.cl(targets[-samp,],predict(ir1,ir[-samp,]))

data("iris")
set.seed(2)
ind = sample(2,nrow(iris),replace = TRUE,prob = c(0.7,0.3))
trainset = iris[ind == 1,]
testset = iris[ind == 2,]
iris.nn = nnet(Species ~ .,data = trainset,size = 2,rang = 0.1,decay = 5e-4,maxit = 200)
summary(iris.nn)
iris.predict = predict(iris.nn,testset,type = "class")
nn.table = table(testset$Species,iris.predict)
nn.table
