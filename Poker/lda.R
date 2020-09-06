#install.packages('klaR')
#install.packages('mlbench')
#install.packages('e1071')
#install.packages('class')
#install.packages('kernlab')


options(repr.plot.width=6, repr.plot.height=6)
library(MASS)

col.names = c("S1", "C1", "S2", "C2", "S3", "C3", "S4", "C4", "S5", "C5", "CLASS");
################
# AGAFAR DADES #
################
data.csv.train <- read.csv(file = file.choose(), header = FALSE, col.names = col.names);
data.csv.test  <- read.csv(file = file.choose(),  header = FALSE, col.names = col.names);

data.matrix.train <- matrix(as.integer(unlist(data.csv.train[, -11])), ncol = 10);
data.matrix.test  <- matrix(as.integer(unlist(data.csv.test [, -11])), ncol = 10);

colnames(data.matrix.train) <- col.names[-11];
colnames(data.matrix.test ) <- col.names[-11];

data.classes.train <- factor(data.csv.train[, 11]);
data.classes.test  <- factor(data.csv.test [, 11]);

data.raw.train <- list(x = data.matrix.train, classes = data.classes.train);
data.raw.test  <- list(x = data.matrix.test , classes = data.classes.test );

data.train <- as.data.frame(data.raw.train);
data.test  <- as.data.frame(data.raw.test );
##############################################################################


Dtrain <- head(data.train, n = 25010)
Dtest <- head(data.test, n = 5000)

summary(Dtrain)

lda.model <- lda(classes~.,data=Dtrain)
lda.model


options(repr.plot.width=6, repr.plot.height=6)
plot(lda.model) #triga bastant


poker.pred <- predict(lda.model)
table(Dtrain$classes,poker.pred$class)

#plot(poker.pred$x,type="n")
#text(poker.pred$x,labels=as.character(rownames(poker.pred$x)),col=as.integer(Dtrain$classes))
#legend('bottomleft', c("Nothing in hand",
#                       "One pair",
#                       "Two pairs",
#                       "Three of a kind",
#                       "Straight",
#                       "Flush",
#                       "Full house",
#                       "Four of a kind",
#                      "Straight flush",
#                       "Royal flush"), lty=1, col=c('black', 'red', 'green','yellow','blue'), bty='n', cex=.6)


lda.predcv <- update(lda.model,CV=TRUE)
head(lda.predcv$posterior)
print(table(Dtrain$classes,lda.predcv$class))

summary(lda.predcv$class)


