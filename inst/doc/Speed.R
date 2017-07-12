### R code from vignette source 'Speed.Rnw'

###################################################
### code chunk number 1: data
###################################################
library(ElemStatLearn)
data(spam)
is.label <- names(spam) == "spam"
X <- as.matrix(spam[,!is.label])
y <- spam[,is.label]
set.seed(1)
train.i <- sample(nrow(spam), nrow(spam)/2)
sets <- 
  list(train=list(features=X[train.i, ], label=y[train.i]),
       test=list(features=X[-train.i, ], label=y[-train.i]))
str(sets)


###################################################
### code chunk number 2: model
###################################################
library(glmnet)
system.time({
  fit <- cv.glmnet(sets$train$features, sets$train$label, family="binomial")
})


###################################################
### code chunk number 3: test-ROC-time
###################################################
library(WeightedROC)
library(ROCR)
library(pROC)
set <- sets$test
guess <- predict(fit, set$features)
if(require(microbenchmark)){
  microbenchmark(WeightedROC={
    wroc <- WeightedROC(guess, set$label)
  }, ROCR={
    pred <- prediction(guess, set$label)
    perf <- performance(pred, "tpr", "fpr")
  }, pROC={
    proc <- roc(set$label, guess, algorithm=2)
  })
}else{
  wroc <- WeightedROC(guess, set$label)
  pred <- prediction(guess, set$label)
  perf <- performance(pred, "tpr", "fpr")
  proc <- roc(set$label, guess, algorithm=2)
}


###################################################
### code chunk number 4: test-ROC-curves
###################################################
perfDF <- function(p){
  data.frame(FPR=p@x.values[[1]], TPR=p@y.values[[1]], package="ROCR")
}
procDF <- function(p){
  data.frame(FPR=1-p$specificities, TPR=p$sensitivities, package="pROC")
}
roc.curves <- 
  rbind(data.frame(wroc[,c("FPR", "TPR")], package="WeightedROC"),
        perfDF(perf), procDF(proc))
library(ggplot2)
rocPlot <- ggplot()+
  geom_path(aes(FPR, TPR, color=package, linetype=package),
            data=roc.curves, size=1)+
  coord_equal()
print(rocPlot)



###################################################
### code chunk number 5: scaling
###################################################
if(require(microbenchmark)){
  stats.by.size.expr <- list()
  ms.by.size <- list()
  for(size in c(400, 800, 1200, 1500, 2000, 2300)){
    indices <- seq(1, length(set$label), l=size)
    y <- set$label[indices]
    y.hat <- guess[indices]
    this.size <- microbenchmark(WeightedROC={
      wroc <- WeightedROC(y.hat, y)
    }, ROCR={
      pred <- prediction(y.hat, y)
      perf <- performance(pred, "tpr", "fpr")
    }, pROC.1={
      proc <- roc(y, y.hat, algorithm=1)
    }, pROC.2={
      proc <- roc(y, y.hat, algorithm=2)
    })
    this.size$milliseconds <- this.size$time/1e6
    ms.by.size[[paste(size)]] <- data.frame(size, this.size)
    this.by.expr <- split(this.size, this.size$expr)
    for(expr in names(this.by.expr)){
      stats <- with(this.by.expr[[expr]], {
        data.frame(median=median(milliseconds),
                   q25=quantile(milliseconds, 0.25),
                   q75=quantile(milliseconds, 0.75))
      })
      stats.by.size.expr[[paste(size, expr)]] <- data.frame(size, expr, stats)
    }
  }
  ms <- do.call(rbind, ms.by.size)
  algo.stats <- do.call(rbind, stats.by.size.expr)
  timePlot <- ggplot()+
    geom_ribbon(aes(size, ymin=q25, ymax=q75, fill=expr), 
                data=algo.stats, alpha=1/2)+
    geom_line(aes(size, median, color=expr), data=algo.stats, size=2)+
    geom_point(aes(size, milliseconds, color=expr), data=ms, pch=1)+
    ylab("milliseconds")+
    xlab("data set size")+
    ggtitle("mean +/- 1 standard deviation")
  print(timePlot)
}


