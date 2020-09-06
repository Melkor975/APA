###############################
##### REGRESSI??? LOG???STICA######
###############################
set.seed(43)
library(MASS)
library(nnet)

#1r carregar ntest i ntrain

n <- nrow(Dtrain)
num_learn <- 1000
learn <- head(data.training,n=num_learn)

num_test <- 5000
test <- head(data.test,n=num_test)

learn$V11 <- factor(learn$V11, labels=c("Nothing in hand",
                                        "One pair",
                                        "Two pairs",
                                        "Three of a kind",
                                        "Straight",
                                        "Flush",
                                        "Full house",
                                        "Four of a kind",
                                        "Straight flush",
                                        "Royal flush"))

#learn$V11 <- factor(learn$V11, labels=c("0","1","2","3","4","5","6","7","8"))
#amb una label menys del que tocaria funciona (no se pk)


model.logreg <- glm (V11~., data=learn, family=binomial)

model.logreg2 <- step(model.logreg)

exp(model.logreg2$coefficients)

# Calculation of apparent error in the training set (learn)

glfpred=NULL
glfpred[model.logreg2$fitted.values<0.5]=0
glfpred[model.logreg2$fitted.values>=0.5 & model.logreg2$fitted.values<1.5]=1
glfpred[model.logreg2$fitted.values>=1.5 & model.logreg2$fitted.values<2.5]=2
glfpred[model.logreg2$fitted.values>=2.5 & model.logreg2$fitted.values<3.5]=3
glfpred[model.logreg2$fitted.values>=3.5 & model.logreg2$fitted.values<4.5]=4
glfpred[model.logreg2$fitted.values>=4.5 & model.logreg2$fitted.values<5.5]=5
glfpred[model.logreg2$fitted.values>=5.5 & model.logreg2$fitted.values<6.5]=6
glfpred[model.logreg2$fitted.values>=6.5 & model.logreg2$fitted.values<7.5]=7
glfpred[model.logreg2$fitted.values>=7.5 & model.logreg2$fitted.values<8.5]=8
glfpred[model.logreg2$fitted.values>=8.5]=9

table(learn$V11,glfpred)

(error_rate.learn <- 100*(1-sum(diag(table(learn$V11,glfpred)))/num_learn))

# Estimation of prediction error using the test set

glft = predict(model.logreg2, newdata=test) 

pt = 1/(1+exp(-glft))

glfpredt=NULL
glfpredt[pt<0.5]=0
glfpredt[pt>=0.5 & pt<1.5]=1
glfpredt[pt>=1.5 & pt<2.5]=2
glfpredt[pt>=2.5 & pt<3.5]=3
glfpredt[pt>=3.5 & pt<4.5]=4
glfpredt[pt>=4.5 & pt<5.5]=5
glfpredt[pt>=5.5 & pt<6.5]=6
glfpredt[pt>=6.5 & pt<7.5]=7
glfpredt[pt>=7.5 & pt<8.5]=8
glfpredt[pt>=8.5]=9

table(test$V11,glfpredt)

(error_rate.test <- 100*(1-sum(diag(table(test$V11,glfpredt)))/num_test))