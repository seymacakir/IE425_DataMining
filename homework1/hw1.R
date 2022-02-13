# Seyma Cakir 
# 5/5/2021
# 2017402024
# IE 425 HW-1

library("caTools")
library("tree")
library("rpart")
library("rpart.plot")
library("data.table")


# upload data 
data("spam", package = "kernlab")
mydata <- data.table(spam)
class(mydata)
str(mydata)
summary(mydata$type)
set.seed(1000)

#split data by randomly and calculate rates of spam
sample <- sample(1:4601,3221)
randomtrain <- spam[sample,]
randomtest <- spam[-sample,]
randomtrain.rate <- table(randomtrain$type)
randomtest.rate <- table(randomtest$type)
# table of train sample spam-nonspam rate
randomtrain.rate
# spam rate of random train sample
randomtrain.rate["spam"]/( randomtrain.rate["spam"] + randomtrain.rate["nonspam"])
# 0.3880782 
# table of  test sample spam-nonspam
randomtest.rate
# spam rate of random test sample
randomtest.rate["spam"]/( randomtest.rate["spam"] + randomtest.rate["nonspam"])
# 0.407971 


# split equal rate and use as the data while creating tree to make learn better 
set.seed(1000)
split=sample.split(spam$type,SplitRatio=0.7)
train=subset(spam,split==TRUE)
test=subset(spam,split==FALSE)
# rate of train sample
summary(train$type)
# nonspam    spam 
# 1952    1269 
# 1269 / 3221 = 0.393977
summary(test$type)
# nonspam    spam 
# 836     544 
# 544 / (836+544) = 0.3942029


#create rpart tree 

rpart_training_tree <- rpart(type ~ . , data = train,  method = "class", minsplit = 2, minbucket = 1, cp = 0)
rpart_training_tree$cptable

# max nsplit = 214
# tree size = leaf nodes = 214 + 1 = 215


# predict with rpart package max tree
pred_rpart_tree <- predict(rpart_training_tree, newdata = test, type = "class" )
table(actual = test$type,predicted = pred_rpart_tree)

# spam = 1 = positive nonspam = 0 = negative
#          predicted
#actual    nonspam spam
#nonspam     761   75
#spam         58  486
#TN= 761 TP= 486 FN= 58 FP= 75

# error = (FP + FN ) / TOTAL   
 (58 + 75 )/ (1380)
# 0.09637681

# FALSE POSITIVE RATE = (FP)/ (TN + FP)
75 /(761 + 75 )
# 0.08971292

# FALSE NEGATIVE RATE = (FN) / (TP + FN)
58/(486+58)
#0.1066176


# rpart packages tree cv 

rpart_training_tree$cptable
# I get the min error rate value from table and find the min cp value 
min(rpart_training_tree$cptable[,"xerror"])
# 0.214342
min_cp = rpart_training_tree$cptable[which.min(rpart_training_tree$cptable[,"xerror"]),"CP"]
min_cp
# 0.0009193591
# since there is natural error in the  error rate, I sum up the error rate and std 
minerror <- min(rpart_training_tree$cptable[,"xerror"]) + rpart_training_tree$cptable[which.min(rpart_training_tree$cptable[,"xerror"]),"xstd"]
minerror
# 0.2267775

# from the table the min tree size lower than min error + std has nsplit = 31 and the optimum cp value 
# 16 0.0021013922     31 0.1347517730 "0.2253743" 0.01272126
opt_cp <- rpart_training_tree$cptable[16,"CP"]


# optimal tree with rpart package 
opttree <- prune(rpart_training_tree, cp = opt_cp )
opttree$cptable
rpart.plot(opttree)

# prediction with rpart package  optimal tree
pred_opttree <- predict(opttree, newdata = test, type = "class")
table(actual = test$type,predicted = pred_opttree)
# spam = 1 = positive nonspam = 0 = negative
#         predicted
# actual    nonspam spam
# nonspam     786   50
# spam         57  487

# TN = 786 FP = 50  FN= 57 TP = 487

# ERROR = (FP + FN ) / TOTAL 
(50 + 57)/ 1380
# 0.07753623

# FALSE POSITIVE RATE = FP / (FP + TN )
50/(786+50)
# 0.05980861

# FALSE NEGATIVE RATE = FN / (FN + TP)
57/(487+57)
# 0.1047794

# the error rate, false negative rate, and false positive rate of optimum tree is smaller than max tree


#create tree with tree package

tree_package_tree <- tree(type ~. ,train, mincut = 1, minsize = 2, mindev = 0)
summary(tree_package_tree)
# number of terminal(leaf) nodes = max tree size = 187


# predict with  tree package max tree

pred_treepack_tree<- predict(tree_package_tree, newdata = test, type = "class" )
table( actual = test$type, predicted = pred_treepack_tree)
# spam = 1 = positive nonspam = 0 = negative
#         predicted
# actual    nonspam spam
# nonspam     775   61
# spam         46  498

# TN = 775  FP = 61  FN= 46  TP = 498

# ERROR = (FP + FN ) / TOTAL 
(46+61)/(1380)
# 0.07753623

# FALSE POSITIVE RATE = FP / (FP + TN )
61/(775+61)
# 0.07296651

# FALSE NEGATIVE RATE = FN / (FN + TP)
46/(498+46)
# 0.08455882

# tree package tree cv 
tree_package_cv = cv.tree(tree_package_tree, K= 10)
tree_package_cv
min(tree_package_cv$dev)
# min deviation is 1743.646 
plot(tree_package_cv$size ,tree_package_cv$dev ,type="b")
plot(tree_package_cv$k, tree_package_cv$dev ,type="b")
# by observing graph and table, we can get the smaller tree by the same deviation
bestsize <- min(tree_package_cv$size[tree_package_cv$dev==min(tree_package_cv$dev)])
bestsize
# 14


# optimal tree with tree package 

opttree1 <-  prune.tree(tree_package_tree, best = bestsize)
summary(opttree1)
plot(opttree1)
text(opttree1, cex=.8)


# prediction with tree package  optimal tree
pred_opttree1 <- predict(opttree1, newdata = test,type = "class" )
table(actual = test$type,predicted = pred_opttree1)
# spam = 1 = positive nonspam = 0 = negative
#         predicted
# actual    nonspam spam
# nonspam     775   61
# spam         65  479

# TN = 775  FP =61  FN=65  TP = 479

# ERROR = (FP + FN ) / TOTAL 
(61 + 65 ) / 1380
#   0.09130435

# FALSE POSITIVE RATE = FP / (FP + TN )
61/(775+61)
# 0.07296651

# FALSE NEGATIVE RATE = FN / (FN + TP)
65/(479+65) 
#  0.1194853

# the false positive rate of optimum tree is equal to max tree, however, error rate and false negative rate is bigger. 

# the optimal tree by rpart package has smaller error rate, FPR and FNR rate too. 


# extras 

# rpart package optimal tree
# precision 
(487 / (487 + 50))
#recall/sensivity 
487/(487 + 57 )
#specificity 
786 / (786 + 50)
#accuracy  
(786 + 487) / 1380

# tree package optimal tree
#precision 
479 / (479 + 61)
#recallsensivity 
479 / (479 + 65)
#specificity 
775 / (775 + 61 )
#accuracy 
 (775 + 479) / 1380


 



