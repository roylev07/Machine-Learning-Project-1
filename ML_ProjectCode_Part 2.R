
library(ggplot2)
library(gridExtra)
library(clv)
library(mvtnorm) 
library(cluster)  
library(stats)
library(corrplot)
library(mice)
library(reshape2)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(knitr)
library(rmarkdown)
library(qcc)
library(nnet)
library(NeuralNetTools)
library(devtools)
library(gtable)
library(stringr)
library(tibble)
library(rpart)
library(rpart.plot)
library(clue)
library(flexclust)
library(cclust)
library(class)
library(DeducerExtras)
library(writexl)
library(e1071)
#-----------------------------------------------------------------------------------

filePath=choose.files() 
Bank_Data_Original<-read.csv(filePath,header=TRUE)


#  complete unknown data

Bank_Data_Original[Bank_Data_Original=="unknown"] <- NA
Bank_Data_Original <- mice(data=Bank_Data_Original, m=5, method="pmm", maxit=50, seed=50)
Bank_Data_Original <- complete(Bank_Data_Original)

Bank_Data <- Bank_Data_Original

# month to quarters
Bank_Data$month <- as.character(Bank_Data$month)
Bank_Data$month[Bank_Data$month=="jan" | Bank_Data$month=="feb" | Bank_Data$month=="mar"] <- 1
Bank_Data$month[Bank_Data$month=="apr" | Bank_Data$month=="may" | Bank_Data$month=="jun"] <- 2
Bank_Data$month[Bank_Data$month=="jul" | Bank_Data$month=="aug" | Bank_Data$month=="sep" ] <- 3
Bank_Data$month[Bank_Data$month=="oct" | Bank_Data$month=="nov" | Bank_Data$month=="dec"  ] <- 4
Bank_Data$month <- as.factor(Bank_Data$month)

# pdays to categories
Bank_Data$pdays <- findInterval(Bank_Data$pdays,c(-1,0,181,366,1000))
Bank_Data$pdays[Bank_Data$pdays==4] <- "OverOneYear"
Bank_Data$pdays[Bank_Data$pdays==3 ] <- "OneYear"
Bank_Data$pdays[Bank_Data$pdays==2] <- "HalfYear"
Bank_Data$pdays[Bank_Data$pdays==1] <- "NoContact"


# Dummy variables

#pdays
Bank_Data$pdays_D1 <- ifelse(Bank_Data$pdays=="HalfYear",1,-1)
Bank_Data$pdays_D2 <- ifelse(Bank_Data$pdays=="OneYear",1,-1)
Bank_Data$pdays_D3 <- ifelse(Bank_Data$pdays=="OverOneYear",1,-1)

Bank_Data$pdays_D1 <- as.factor(Bank_Data$pdays_D1)
Bank_Data$pdays_D2 <- as.factor(Bank_Data$pdays_D2)
Bank_Data$pdays_D3 <- as.factor(Bank_Data$pdays_D3)

# job
Bank_Data$job_D1 <- ifelse(Bank_Data$job=="blue-collar",1,-1)
Bank_Data$job_D2 <- ifelse(Bank_Data$job=="management",1,-1)
Bank_Data$job_D3 <- ifelse(Bank_Data$job=="technician",1,-1)

Bank_Data$job_D1 <- as.factor(Bank_Data$job_D1)
Bank_Data$job_D2 <- as.factor(Bank_Data$job_D2)
Bank_Data$job_D3 <- as.factor(Bank_Data$job_D3)

#marital
Bank_Data$marital_D1 <- ifelse(Bank_Data$marital=="married",1,-1)
Bank_Data$marital_D2 <- ifelse(Bank_Data$marital=="divorced",1,-1)

Bank_Data$marital_D1 <- as.factor(Bank_Data$marital_D1)
Bank_Data$marital_D2 <- as.factor(Bank_Data$marital_D2)

#education
Bank_Data$education_D1 <- ifelse(Bank_Data$education=="secondary",1,-1)
Bank_Data$education_D2 <- ifelse(Bank_Data$education=="tertiary",1,-1)

Bank_Data$education_D1 <- as.factor(Bank_Data$education_D1)
Bank_Data$education_D2 <- as.factor(Bank_Data$education_D2)

#month
Bank_Data$Quarter_D1 <- ifelse(Bank_Data$month=="2",1,-1)
Bank_Data$Quarter_D2 <- ifelse(Bank_Data$month=="3",1,-1)
Bank_Data$Quarter_D3 <- ifelse(Bank_Data$month=="4",1,-1)

Bank_Data$Quarter_D1 <- as.factor(Bank_Data$Quarter_D1)
Bank_Data$Quarter_D2 <- as.factor(Bank_Data$Quarter_D2)
Bank_Data$Quarter_D3 <- as.factor(Bank_Data$Quarter_D3)

#poutcome
Bank_Data$poutcome_D1 <- ifelse(Bank_Data$poutcome=="failure",1,-1)
Bank_Data$poutcome_D2 <- ifelse(Bank_Data$poutcome=="other",1,-1)

Bank_Data$poutcome_D1 <- as.factor(Bank_Data$poutcome_D1)
Bank_Data$poutcome_D2 <- as.factor(Bank_Data$poutcome_D2)

#housing
Bank_Data$housing_D1 <- ifelse(Bank_Data$housing=="yes",1,-1)

Bank_Data$housing_D1 <- as.factor(Bank_Data$housing_D1)

#loan
Bank_Data$loan_D1 <- ifelse(Bank_Data$loan=="yes",1,-1)

Bank_Data$loan_D1 <- as.factor(Bank_Data$loan_D1)



# columns deletion
 deletion <- c(2,3,4,6,7,9,11,13)
 Bank_Data <- Bank_Data[,-deletion]
 y <- Bank_Data$y
 Bank_Data <- Bank_Data[,-6]
 Bank_Data <- as.data.frame(cbind(Bank_Data,y))
 
 # scaling
 
 Bank_Data$age <- scale(Bank_Data$age)
 Bank_Data$balance <- scale(Bank_Data$balance)
 Bank_Data$day <- scale(Bank_Data$day)
 Bank_Data$campaign <- scale(Bank_Data$campaign)
 Bank_Data$previous <- scale(Bank_Data$previous)
 
 
#-------------------------------------------------------------------
 
 # divide to train and test sets 
 
 set.seed(123)
 Rows_Numbers_for_Test_Set <- sample(1:nrow(Bank_Data),0.2*nrow(Bank_Data),replace = FALSE)
 
 Train_Set <- Bank_Data[-Rows_Numbers_for_Test_Set,]
 Test_Set <-Bank_Data[Rows_Numbers_for_Test_Set,]
 
# K-fold configuration
n.folds <- 5
samples <- sample(1:n.folds, nrow(Train_Set), replace=T)
Train_and_Val <- cbind(Train_Set, samples)
k_fold_sets <- list()
for(fold in 1:n.folds){
  k_fold_sets[[fold]] <- Train_and_Val[Train_and_Val$samples==fold,1:ncol(Train_and_Val)-1]
}
 
k_folds_Train_sets <- list()
k_folds_Valid_sets <- list()
 
k_folds_Train_sets[[1]] <- rbind(k_fold_sets[[2]],k_fold_sets[[3]],k_fold_sets[[4]],k_fold_sets[[5]])
k_folds_Train_sets[[2]] <- rbind(k_fold_sets[[1]],k_fold_sets[[3]],k_fold_sets[[4]],k_fold_sets[[5]])
k_folds_Train_sets[[3]] <- rbind(k_fold_sets[[1]],k_fold_sets[[2]],k_fold_sets[[4]],k_fold_sets[[5]])
k_folds_Train_sets[[4]] <- rbind(k_fold_sets[[1]],k_fold_sets[[2]],k_fold_sets[[3]],k_fold_sets[[5]])
k_folds_Train_sets[[5]] <- rbind(k_fold_sets[[1]],k_fold_sets[[2]],k_fold_sets[[3]],k_fold_sets[[4]])

k_folds_Valid_sets[[1]] <- k_fold_sets[[1]]
k_folds_Valid_sets[[2]] <- k_fold_sets[[2]]
k_folds_Valid_sets[[3]] <- k_fold_sets[[3]]
k_folds_Valid_sets[[4]] <- k_fold_sets[[4]]
k_folds_Valid_sets[[5]] <- k_fold_sets[[5]]
 
 #--------------------------------------------------------------------

#--------------------------------------------------

# neural networks

#-------------------------------------------------- 
 # 1 

set.seed(123)
nn <- nnet(x=Train_Set[,1:22], y=class.ind(Train_Set[,23]), size=1, linout=FALSE, softmax=T) # train
preds_nn_train <- factor(predict(nn, newdata=Train_Set[,1:22], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=Test_Set[,1:22], type='class')) # prediction for test set

nn_default_train_percision <- sum(preds_nn_train==Train_Set$y)/nrow(Train_Set)
nn_default_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)

plotnet(nn)
summary(nn)
olden(nn)
# olden(nn, out_var=Train_Set[,23])

# 2


#nnum <- seq(1,101,5)
nnum <- c(1,2,3,4,5,6,7,8,9,10)
acc  <- matrix(0,length(nnum),1)
i    <- 1 
for(neurons in nnum){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    
    nn <- nnet(x=k_folds_Train_sets[[fold]][,1:22], y=class.ind(k_folds_Train_sets[[fold]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000) 
    
    preds_nn_val <- factor(predict(nn, newdata=k_folds_Valid_sets[[fold]][,1:22], type='class'))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==k_folds_Valid_sets[[fold]]$y)/nrow(k_folds_Valid_sets[[fold]]))
  }
  acc[i] <-  temp_total_precision/n.folds
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=acc)) + geom_line(aes(x,y), color='purple') + 
  xlab("Number of Neurons") + ylab("Validation Accuracy")


print(acc)

neurons <- nnum[which.max(acc)] # find the size with maximum accuracy
valid_percision_after_neurons_num <- max(acc)
set.seed(123)
nn <- nnet(x=Train_Set[,1:22], y=class.ind(Train_Set$y), size=neurons, linout=FALSE, softmax=T) # train
preds_nn_train <- factor(predict(nn, newdata=Train_Set[,1:22], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=Test_Set[,1:22], type='class')) # prediction for test set
nn_after_neuronsNum_train_percision <- sum(preds_nn_train==Train_Set$y)/nrow(Train_Set)
nn_after_neuronsNum_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)


# 3


# configuration 1- weight decay

nnum <- seq(0,1,0.05)
decay_val  <- matrix(0,length(nnum),1)
i    <- 1 
for(n_decay in nnum){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    nn <- nnet(x=k_folds_Train_sets[[fold]][,1:22], y=class.ind(k_folds_Train_sets[[fold]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, decay = n_decay ) 
    
    preds_nn_val <- factor(predict(nn, newdata=k_folds_Valid_sets[[fold]][,1:22], type='class'))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==k_folds_Valid_sets[[fold]]$y)/nrow(k_folds_Valid_sets[[fold]]))
  }
  decay_val[i] <-  temp_total_precision/n.folds
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=decay_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("Weight Decay") + ylab("Validation Accuracy")

weight_decay <- nnum[which.max(decay_val)]
nn_Configure1_valid_percision <- max(decay_val)
nn <- nnet(x=k_folds_Train_sets[[1]][,1:22], y=class.ind(k_folds_Train_sets[[1]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, decay = weight_decay ) 
print(nn)
plotnet(nn)

# curr_fold <- 2
# nn <- nnet(x=k_folds_Train_sets[[curr_fold]][,1:22], y=class.ind(k_folds_Train_sets[[curr_fold]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, decay = weight_decay ) 
# preds_nn_train <- factor(predict(nn, newdata=k_folds_Train_sets[[curr_fold]][,1:22], type='class')) # prediction for train set
# preds_nn_valid <- factor(predict(nn, newdata=k_folds_Valid_sets[[curr_fold]][,1:22], type='class'))
# preds_nn_test  <- factor(predict(nn, newdata=Test_Set[,1:22], type='class')) # prediction for test set
# nn_Configure1_train_percision <- sum(preds_nn_train==k_folds_Train_sets[[curr_fold]]$y)/nrow(k_folds_Train_sets[[curr_fold]])
# nn_Configure1_valid_percision <- sum(preds_nn_valid==k_folds_Valid_sets[[curr_fold]]$y)/nrow(k_folds_Valid_sets[[curr_fold]])
# nn_Configure1_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)

set.seed(123)
nn <- nnet(x=Train_Set[,1:22], y=class.ind(Train_Set[,23]), size=neurons, linout=FALSE, softmax=T, decay = weight_decay ) # train
preds_nn_train <- factor(predict(nn, newdata=Train_Set[,1:22], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=Test_Set[,1:22], type='class')) # prediction for test set
nn_Configure1_train_percision <- sum(preds_nn_train==Train_Set$y)/nrow(Train_Set)
nn_Configure1_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)


# configuration 2- iteration number

nnum <- seq(10,300,10)
iteration_val  <- matrix(0,length(nnum),1)
i    <- 1 
for(n_iteration in nnum){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    nn <- nnet(x=k_folds_Train_sets[[fold]][,1:22], y=class.ind(k_folds_Train_sets[[fold]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, maxit = n_iteration ) 
    
    preds_nn_val <- factor(predict(nn, newdata=k_folds_Valid_sets[[fold]][,1:22], type='class'))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==k_folds_Valid_sets[[fold]]$y)/nrow(k_folds_Valid_sets[[fold]]))
  }
  iteration_val[i] <-  temp_total_precision/n.folds
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=iteration_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("Weight Decay") + ylab("Validation Accuracy")

iteration_number <- nnum[which.max(iteration_val)]
nn_Configure2_valid_percision <- max(iteration_val)
nn <- nnet(x=k_folds_Train_sets[[1]][,1:22], y=class.ind(k_folds_Train_sets[[1]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, maxit = iteration_number ) 
print(nn)
plotnet(nn)

set.seed(123)
nn <- nnet(x=Train_Set[,1:22], y=class.ind(Train_Set[,23]), size=neurons, linout=FALSE, softmax=T, maxit = iteration_number ) # train
preds_nn_train <- factor(predict(nn, newdata=Train_Set[,1:22], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=Test_Set[,1:22], type='class')) # prediction for test set
nn_Configure2_train_percision <- sum(preds_nn_train==Train_Set$y)/nrow(Train_Set)
nn_Configure2_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)

# configuration 3- combind between configuration 1 and 2

# finding the iteration number given weight decay
nnum <- seq(10,300,10)
iteration_val  <- matrix(0,length(nnum),1)
i    <- 1 
for(n_iteration in nnum){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    nn <- nnet(x=k_folds_Train_sets[[fold]][,1:22], y=class.ind(k_folds_Train_sets[[fold]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, maxit = n_iteration, decay = weight_decay ) 
    
    preds_nn_val <- factor(predict(nn, newdata=k_folds_Valid_sets[[fold]][,1:22], type='class'))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==k_folds_Valid_sets[[fold]]$y)/nrow(k_folds_Valid_sets[[fold]]))
  }
  iteration_val[i] <-  temp_total_precision/n.folds
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=iteration_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("Iteration Num (Given Decay)") + ylab("Validation Accuracy")

iteration_number <- nnum[which.max(iteration_val)]

set.seed(123)
nn <- nnet(x=Train_Set[,1:22], y=class.ind(Train_Set[,23]), size=neurons, linout=FALSE, softmax=T, maxit = iteration_number, decay = weight_decay ) # train
preds_nn_train <- factor(predict(nn, newdata=Train_Set[,1:22], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=Test_Set[,1:22], type='class')) # prediction for test set
nn_Configure3A_train_percision <- sum(preds_nn_train==Train_Set$y)/nrow(Train_Set)
nn_Configure3A_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)

# finding weight decay given iteration number
nnum <- seq(0,1,0.05)
iteration_number <- 40
decay_val  <- matrix(0,length(nnum),1)
i    <- 1 
for(n_decay in nnum){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    nn <- nnet(x=k_folds_Train_sets[[fold]][,1:22], y=class.ind(k_folds_Train_sets[[fold]]$y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000,maxit = iteration_number, decay = n_decay ) 
    
    preds_nn_val <- factor(predict(nn, newdata=k_folds_Valid_sets[[fold]][,1:22], type='class'))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==k_folds_Valid_sets[[fold]]$y)/nrow(k_folds_Valid_sets[[fold]]))
  }
  decay_val[i] <-  temp_total_precision/n.folds
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=decay_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("Weight Decay (Given Iteration Num)") + ylab("Validation Accuracy")

weight_decay <- nnum[which.max(decay_val)]

set.seed(123)
nn <- nnet(x=Train_Set[,1:22], y=class.ind(Train_Set[,23]), size=neurons, linout=FALSE, softmax=T, maxit = iteration_number, decay = weight_decay ) # train
preds_nn_train <- factor(predict(nn, newdata=Train_Set[,1:22], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=Test_Set[,1:22], type='class')) # prediction for test set
nn_Configure3B_train_percision <- sum(preds_nn_train==Train_Set$y)/nrow(Train_Set)
nn_Configure3B_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)

## The Chosen Model

set.seed(123)
nn_chosen <- nnet(x=Train_Set[,1:22], y=class.ind(Train_Set[,23]), size=2, linout=FALSE, softmax=T, maxit = 40, decay = 0.6 ) # train
preds_nn_train <- factor(predict(nn_chosen, newdata=Train_Set[,1:22], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn_chosen, newdata=Test_Set[,1:22], type='class')) # prediction for test set
nn_Configure3B_train_percision <- sum(preds_nn_train==Train_Set$y)/nrow(Train_Set)
nn_Configure3B_test_percision <- sum(preds_nn_test==Test_Set$y)/nrow(Test_Set)

#confusion matrix- Chosen Model
table(prediction = preds_nn_test, true_values = Test_Set$y) 

#------------------------------------------------------

# Decision Trees 

#------------------------------------------------------

Bank_Data_Trees <- Bank_Data_Original

# making the catagorial variables 

# month to quarters
Bank_Data_Trees$month <- as.character(Bank_Data_Trees$month)
Bank_Data_Trees$month[Bank_Data_Trees$month=="jan" | Bank_Data_Trees$month=="feb" | Bank_Data_Trees$month=="mar"] <- 1
Bank_Data_Trees$month[Bank_Data_Trees$month=="apr" | Bank_Data_Trees$month=="may" | Bank_Data_Trees$month=="jun"] <- 2
Bank_Data_Trees$month[Bank_Data_Trees$month=="jul" | Bank_Data_Trees$month=="aug" | Bank_Data_Trees$month=="sep" ] <- 3
Bank_Data_Trees$month[Bank_Data_Trees$month=="oct" | Bank_Data_Trees$month=="nov" | Bank_Data_Trees$month=="dec"  ] <- 4
Bank_Data_Trees$month <- as.factor(Bank_Data_Trees$month)

# pdays to categories
Bank_Data_Trees$pdays <- findInterval(Bank_Data_Trees$pdays,c(-1,0,181,366,1000))
Bank_Data_Trees$pdays[Bank_Data_Trees$pdays==4] <- "OverOneYear"
Bank_Data_Trees$pdays[Bank_Data_Trees$pdays==3 ] <- "OneYear"
Bank_Data_Trees$pdays[Bank_Data_Trees$pdays==2] <- "HalfYear"
Bank_Data_Trees$pdays[Bank_Data_Trees$pdays==1] <- "NoContact"


# divide to train and test sets 

set.seed(123)
Rows_Numbers_for_Test_Set <- sample(1:nrow(Bank_Data),0.2*nrow(Bank_Data),replace = FALSE)

Train_Set_Tree <- Bank_Data_Trees[-Rows_Numbers_for_Test_Set,]
Test_Set_Tree <-Bank_Data_Trees[Rows_Numbers_for_Test_Set,]

# 1

set.seed(123)
tree <- rpart(formula=Train_Set_Tree$y~., data = Train_Set_Tree, method = 'class', parm=list(split='information'),cp=0)
preds.tree.train <- predict(tree, newdata = Train_Set_Tree[,1:13], type='class')
preds.tree.test <- predict(tree, newdata = Test_Set_Tree[,1:13], type='class')
tree_default_train_precision <- (sum(preds.tree.train==Train_Set_Tree$y))/nrow(Train_Set_Tree)
tree_default_test_precision <- (sum(preds.tree.test==Test_Set_Tree$y))/nrow(Test_Set_Tree)

plot(tree)
text(tree)

# 2

printcp(tree)

plotcp(tree)
opt <- which.min(tree$cptable[,"xerror"]) # get the index of complexity parameter (cp) with lowest xerror
cp <- tree$cptable[opt, "CP"] # get its value

pruned_model <- prune(tree,cp)
preds.tree.train <- predict(pruned_model, newdata = Train_Set_Tree[,1:13], type='class')
preds.tree.test <- predict(pruned_model, newdata = Test_Set_Tree[,1:13], type='class')
tree_default_train_precision <- (sum(preds.tree.train==Train_Set_Tree$y))/nrow(Train_Set_Tree)
tree_default_test_precision <- (sum(preds.tree.test==Test_Set_Tree$y))/nrow(Test_Set_Tree)

plot(pruned_model)
text(pruned_model)

rpart.plot(pruned_model)
#rpart.plot(pruned_model, extra = 104, box.palette = "GnBu", branch.lty=3, shadow.col = "gray", nn=TRUE)

# just for visulaization
pruned_model2 <- prune(tree,0.02)
rpart.plot(pruned_model2)


# 3

#cp=0
pruned_model_cp0 <- prune(tree,0)
preds.tree.train <- predict(pruned_model_cp0, newdata = Train_Set_Tree[,1:13], type='class')
preds.tree.test <- predict(pruned_model_cp0, newdata = Test_Set_Tree[,1:13], type='class')
pruned_model_cp0_train_precision <- (sum(preds.tree.train==Train_Set_Tree$y))/nrow(Train_Set_Tree)
pruned_model_cp0_test_precision <- (sum(preds.tree.test==Test_Set_Tree$y))/nrow(Test_Set_Tree)

#cp with maximal validation error
cp_index<- which.max(tree$cptable[,"xerror"])
cp_maximal_validation_error <- tree$cptable[cp_index, "CP"]

pruned_model_maximal_valid_error_cp <- prune(tree,cp_maximal_validation_error)
preds.tree.train <- predict(pruned_model_maximal_valid_error_cp, newdata = Train_Set_Tree[,1:13], type='class')
preds.tree.test <- predict(pruned_model_maximal_valid_error_cp, newdata = Test_Set_Tree[,1:13], type='class')
pruned_model_cp_maximal_validation_error_train_precision <- (sum(preds.tree.train==Train_Set_Tree$y))/nrow(Train_Set_Tree)
pruned_model_cp_maximal_validation_error_test_precision <- (sum(preds.tree.test==Test_Set_Tree$y))/nrow(Test_Set_Tree)

# cp second best
cp_second_best <- 0.00411946

pruned_model_cp_second_best <- prune(tree,cp_second_best)
preds.tree.train <- predict(pruned_model_cp_second_best, newdata = Train_Set_Tree[,1:13], type='class')
preds.tree.test <- predict(pruned_model_cp_second_best, newdata = Test_Set_Tree[,1:13], type='class')
pruned_model_cp_second_best_train_precision <- (sum(preds.tree.train==Train_Set_Tree$y))/nrow(Train_Set_Tree)
pruned_model_cp_second_best_test_precision <- (sum(preds.tree.test==Test_Set_Tree$y))/nrow(Test_Set_Tree)


# cp 2 splits
pruned_model_2_splits <- prune(tree,0.015105)
preds.tree.train <- predict(pruned_model_2_splits, newdata = Train_Set_Tree[,1:13], type='class')
preds.tree.test <- predict(pruned_model_2_splits, newdata = Test_Set_Tree[,1:13], type='class')
pruned_model_2_splits_train_precision <- (sum(preds.tree.train==Train_Set_Tree$y))/nrow(Train_Set_Tree)
pruned_model_2_splits_test_precision <- (sum(preds.tree.test==Test_Set_Tree$y))/nrow(Test_Set_Tree)


#------------------------------------------------------

# K-Means 

#------------------------------------------------------

# 1

Bank_Data_K_Means <- Train_Set[,-23]



Train_Set_Original <- Bank_Data_Original[-Rows_Numbers_for_Test_Set,]
Test_Set_Original <-Bank_Data_Original[Rows_Numbers_for_Test_Set,]

# 2

k <- 2
set.seed(123)
clust_data <- kmeans(Bank_Data_K_Means, centers=k)
Bank_Data_K_Means$clust <- factor(clust_data$cluster)

Bank_Data_K_Means$clust_0_1 <- ifelse(Bank_Data_K_Means$clust==1,0,1)
k_means_default_train_percision <- sum(Bank_Data_K_Means$clust_0_1==Train_Set$y)/nrow(Train_Set)

# 3 

Bank_Data_K_Means <- Bank_Data_K_Means[,-24]
Bank_Data_K_Means <- data.matrix(Bank_Data_K_Means)

scatt_data    <- cls.scatt.data((Bank_Data_K_Means), clust=clust_data$cluster, dist='euclidean')
dunn_train          <- clv.Dunn(scatt_data, 'centroid', 'centroid')
DB_train            <- clv.Davies.Bouldin(scatt_data, 'centroid', 'centroid')




p1 <- ggplot(Train_Set_Original ,aes(x= Train_Set_Original$age, y=Train_Set_Original$day,  color=ifelse(clust_data$cluster=="1","Green","Red") ,size=10 )) + geom_point(shape = ifelse(Train_Set_Original$y==1,"Y","N")) + guides(color=F, size=F) 

grid.arrange(p1)


#plots

Bank_Data_K_Means <- as.data.frame(Bank_Data_K_Means)

# education

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$job,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$marital , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$education, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#pdays

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$job,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$marital , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$pdays, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#age

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$job,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$marital , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$age, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)

#job

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$marital , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$job, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#marital

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$marital, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#balance

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$balance, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)

#housing

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$housing, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)

#loan

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$loan, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#day

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$day, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#month

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$month, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#campaign

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$campaign, y=Train_Set_Original$previous,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#previous

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$poutcome , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$previous, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


#poutcome

p1 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$pdays,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p2 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$previous , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p3 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$balance,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p4 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$marital,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p5 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$day,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p6 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$education,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

p7 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$age,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p8 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$job , color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p9 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$housing,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p10 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$loan,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p11 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$month,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
p12 <- ggplot(Bank_Data_K_Means, aes(x=Train_Set_Original$poutcome, y=Train_Set_Original$campaign,  color=clust_data$cluster, size=10)) + geom_point() + guides(color=F, size=F) 
grid.arrange(p7, p8, p9, p10, p11, p12, ncol=3)


## FINDING THE BEST K

Bank_Data_K_Means <- Train_Set[,-23]
Bank_Data_K_Means <- data.matrix(Bank_Data_K_Means)

# Dunn & DB

set.seed(123)
dunn <- c(); DB <- c(); K <- 9
for(k in 2:K){
  clust_data    <- kmeans(Bank_Data_K_Means, centers=k)
  scatt_data    <- cls.scatt.data(Bank_Data_K_Means, clust=clust_data$cluster, dist='euclidean')
  dunn          <- c(dunn, clv.Dunn(scatt_data, 'centroid', 'centroid'))
  DB            <- c(DB,   clv.Davies.Bouldin(scatt_data, 'centroid', 'centroid'))
}

clust_metrics <- data.frame(K = rep(seq(2,K,1),2), value = c(dunn, DB), metric = c(rep('Dunn',K-1), rep('DB',K-1)))
ggplot(clust_metrics, aes(x=K, y=value, color=factor(metric))) + geom_point() + geom_line()

k <- c(2,3,4,5,6,7,8,9)
print(cbind(k,DB,dunn, DB-dunn))



# kmeans results 


clust_data_2 <-  kcca(Bank_Data_K_Means[,-23], k=2, kccaFamily("kmeans"))
pred_train <- predict(clust_data_2)
pred_test <- predict(clust_data_2, newdata=data.matrix(Test_Set[,-23]))
pred_test <- pred_test-1


#--- Result for test ---#

k_means_default_test_percision <- sum(pred_test==Test_Set$y)/nrow(Test_Set)



#------------------------------------------------------

# SVM

#------------------------------------------------------

## classification mode
# default with factor response:
model_svm <- svm(as.factor(Train_Set$y) ~ ., data = Train_Set)

 

print(model_svm)
summary(model_svm)

# train perciosion
pred_train_svm <- predict(model_svm, Train_Set[,1:22])
pred_train_svm <- c(pred_train_svm)
pred_train_svm <- pred_train_svm-1
svm_train_perciosin <- sum(pred_train_svm==Train_Set$y)/nrow(Train_Set)

# test perciosion
pred_test_svm <- predict(model_svm, Test_Set[,1:22])
pred_test_svm <- c(pred_test_svm)
pred_test_svm <- pred_test_svm-1
svm_test_perciosin <- sum(pred_test_svm==Test_Set$y)/nrow(Test_Set)


# confusion table:
table(pred_test_svm, Test_Set$y)





#------------------------------------------------------

# Predictions for X-Test

#------------------------------------------------------


## data preperation

filePath=choose.files() 
X_Test_Original<-read.csv(filePath,header=TRUE)

X_Test <- X_Test_Original

# delete varibles 
delete <- c(5,9,16)
X_Test <- X_Test[,-delete]

#  complete unknown data

X_Test[X_Test=="unknown"] <- NA
X_Test <- mice(data=X_Test, m=5, method="pmm", maxit=50, seed=50)
X_Test <- complete(X_Test)



# month to quarters
X_Test$month <- as.character(X_Test$month)
X_Test$month[X_Test$month=="jan" | X_Test$month=="feb" | X_Test$month=="mar"] <- 1
X_Test$month[X_Test$month=="apr" | X_Test$month=="may" | X_Test$month=="jun"] <- 2
X_Test$month[X_Test$month=="jul" | X_Test$month=="aug" | X_Test$month=="sep" ] <- 3
X_Test$month[X_Test$month=="oct" | X_Test$month=="nov" | X_Test$month=="dec"  ] <- 4
X_Test$month <- as.factor(X_Test$month)

# pdays to categories
X_Test$pdays <- findInterval(X_Test$pdays,c(-1,0,181,366,1000))
X_Test$pdays[X_Test$pdays==4] <- "OverOneYear"
X_Test$pdays[X_Test$pdays==3 ] <- "OneYear"
X_Test$pdays[X_Test$pdays==2] <- "HalfYear"
X_Test$pdays[X_Test$pdays==1] <- "NoContact"


# Dummy variables

#pdays
X_Test$pdays_D1 <- ifelse(X_Test$pdays=="HalfYear",1,-1)
X_Test$pdays_D2 <- ifelse(X_Test$pdays=="OneYear",1,-1)
X_Test$pdays_D3 <- ifelse(X_Test$pdays=="OverOneYear",1,-1)

X_Test$pdays_D1 <- as.factor(X_Test$pdays_D1)
X_Test$pdays_D2 <- as.factor(X_Test$pdays_D2)
X_Test$pdays_D3 <- as.factor(X_Test$pdays_D3)

# job
X_Test$job_D1 <- ifelse(X_Test$job=="blue-collar",1,-1)
X_Test$job_D2 <- ifelse(X_Test$job=="management",1,-1)
X_Test$job_D3 <- ifelse(X_Test$job=="technician",1,-1)

X_Test$job_D1 <- as.factor(X_Test$job_D1)
X_Test$job_D2 <- as.factor(X_Test$job_D2)
X_Test$job_D3 <- as.factor(X_Test$job_D3)

#marital
X_Test$marital_D1 <- ifelse(X_Test$marital=="married",1,-1)
X_Test$marital_D2 <- ifelse(X_Test$marital=="divorced",1,-1)

X_Test$marital_D1 <- as.factor(X_Test$marital_D1)
X_Test$marital_D2 <- as.factor(X_Test$marital_D2)

#education
X_Test$education_D1 <- ifelse(X_Test$education=="secondary",1,-1)
X_Test$education_D2 <- ifelse(X_Test$education=="tertiary",1,-1)

X_Test$education_D1 <- as.factor(X_Test$education_D1)
X_Test$education_D2 <- as.factor(X_Test$education_D2)

#month
X_Test$Quarter_D1 <- ifelse(X_Test$month=="2",1,-1)
X_Test$Quarter_D2 <- ifelse(X_Test$month=="3",1,-1)
X_Test$Quarter_D3 <- ifelse(X_Test$month=="4",1,-1)

X_Test$Quarter_D1 <- as.factor(X_Test$Quarter_D1)
X_Test$Quarter_D2 <- as.factor(X_Test$Quarter_D2)
X_Test$Quarter_D3 <- as.factor(X_Test$Quarter_D3)

#poutcome
X_Test$poutcome_D1 <- ifelse(X_Test$poutcome=="failure",1,-1)
X_Test$poutcome_D2 <- ifelse(X_Test$poutcome=="other",1,-1)

X_Test$poutcome_D1 <- as.factor(X_Test$poutcome_D1)
X_Test$poutcome_D2 <- as.factor(X_Test$poutcome_D2)

#housing
X_Test$housing_D1 <- ifelse(X_Test$housing=="yes",1,-1)

X_Test$housing_D1 <- as.factor(X_Test$housing_D1)

#loan
X_Test$loan_D1 <- ifelse(X_Test$loan=="yes",1,-1)

X_Test$loan_D1 <- as.factor(X_Test$loan_D1)



# columns deletion
deletion <- c(2,3,4,6,7,9,11,13)
X_Test <- X_Test[,-deletion]




# scaling

X_Test$age <- scale(X_Test$age)
X_Test$balance <- scale(X_Test$balance)
X_Test$day <- scale(X_Test$day)
X_Test$campaign <- scale(X_Test$campaign)
X_Test$previous <- scale(X_Test$previous)


#-----------------------------------------------


# prediction

preds_X_Test <- factor(predict(nn_chosen, newdata=X_Test, type='class'))
Results <- as.data.frame(preds_X_Test)


path=choose.files() 
write_xlsx(x = Results, path = path, col_names = TRUE)
