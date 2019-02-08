
# function to generate data
draw3 = function(d,ss1,ss2,nonsparse,mu1,mu2,sd1,sd2){
  
  n21 = nonsparse
  
  # class 2
  # use rnorm as data not correlated (signficantly faster than mvnorm)
  y1a = matrix(rnorm(n=ss2*n21,
                     mean=mu1,
                     sd=sd1),nrow=ss2,ncol=n21,byrow=FALSE)
  y1b = matrix(rnorm(n=ss2*(d-n21),
                     mean=0,
                     sd=1),nrow=ss2,ncol=d-n21,byrow=FALSE)
  y1 = cbind(y1a,y1b)
  
  # class 1
  x1a = matrix(rnorm(n=ss1*n21,
                     mean=mu2,
                     sd=sd2),nrow=ss1,ncol=n21,byrow=FALSE)
  x1b = matrix(rnorm(n=ss1*(d-n21),
                     mean=0,
                     sd=1),nrow=ss1,ncol=d-n21,byrow=FALSE)
  x1 = cbind(x1a,x1b)
  
  sdata = rbind(y1,x1)
  return(sdata=sdata)
}


## generate data for simulations
# number of simulations
iters = 100


# setup parallel backend to use 8 processors
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(1011)

# all scenarios -- d=5000, ss1=100, ss2=100, mu1=0, sd1=1, sd2=1
# scenario 1 -- mu2 = 3, nonsparse = 10 
train_a1 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=10,
        mu1=0,mu2=3,sd=1,sd2=1)
}
test_a1 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=10,
        mu1=0,mu2=3,sd=1,sd2=1)
}

# scenario 2 -- mu2 = 3, nonsparse = 6 
train_a2 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=6,
        mu1=0,mu2=3,sd=1,sd2=1)
}
test_a2 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=6,
        mu1=0,mu2=3,sd=1,sd2=1)
}

# scenario 3 -- mu2 = 3, nonsparse = 2 
train_a3 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=2,
        mu1=0,mu2=3,sd=1,sd2=1)
}
test_a3 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=2,
        mu1=0,mu2=3,sd=1,sd2=1)
}

# scenario 4 -- mu2 = 1, nonsparse = 10 
train_a4 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=10,
        mu1=0,mu2=1,sd=1,sd2=1)
}
test_a4 = foreach(icount(iters)) %dopar% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=10,
        mu1=0,mu2=1,sd=1,sd2=1)
}
stopCluster(cl)
registerDoSEQ()

#####################################################################

## principal component analysis simulations

# PCA for Scenario 1
pc_a1_p2=prcomp(train_a1[[1]][,1:2],scale=T,retx=T)
pc_a1_p10=prcomp(train_a1[[1]][,1:10],scale=T,retx=T)
pc_a1_p100=prcomp(train_a1[[1]][,1:100],scale=T,retx=T)
pc_a1_p200=prcomp(train_a1[[1]][,1:200],scale=T,retx=T)
pc_a1_p1000=prcomp(train_a1[[1]][,1:1000],scale=T,retx=T)
pc_a1_p5000=prcomp(train_a1[[1]][,1:5000],scale=T,retx=T)

# PCA for Scenario 2
pc_a2_p2=prcomp(train_a2[[1]][,1:2],scale=T,retx=T)
pc_a2_p10=prcomp(train_a2[[1]][,1:10],scale=T,retx=T)
pc_a2_p100=prcomp(train_a2[[1]][,1:100],scale=T,retx=T)
pc_a2_p200=prcomp(train_a2[[1]][,1:200],scale=T,retx=T)
pc_a2_p1000=prcomp(train_a2[[1]][,1:1000],scale=T,retx=T)
pc_a2_p5000=prcomp(train_a2[[1]][,1:5000],scale=T,retx=T)

# PCA for Scenario 3
pc_a3_p2=prcomp(train_a3[[1]][,1:2],scale=T,retx=T)
pc_a3_p10=prcomp(train_a3[[1]][,1:10],scale=T,retx=T)
pc_a3_p100=prcomp(train_a3[[1]][,1:100],scale=T,retx=T)
pc_a3_p200=prcomp(train_a3[[1]][,1:200],scale=T,retx=T)
pc_a3_p1000=prcomp(train_a3[[1]][,1:1000],scale=T,retx=T)
pc_a3_p5000=prcomp(train_a3[[1]][,1:5000],scale=T,retx=T)

# PCA for Scenario 4
pc_a4_p2=prcomp(train_a4[[1]][,1:2],scale=T,retx=T)
pc_a4_p10=prcomp(train_a4[[1]][,1:10],scale=T,retx=T)
pc_a4_p100=prcomp(train_a4[[1]][,1:100],scale=T,retx=T)
pc_a4_p200=prcomp(train_a4[[1]][,1:200],scale=T,retx=T)
pc_a4_p1000=prcomp(train_a4[[1]][,1:1000],scale=T,retx=T)
pc_a4_p5000=prcomp(train_a4[[1]][,1:5000],scale=T,retx=T)

#####################################################################

### random forest simulations
library(randomForest)

# create classifier for random forest 
run.rft = function(train,test,n,d){
  s1 = list()
  s2 = list()
  for (j in 1:n){
    s1[[j]] = matrix(,nrow=1,ncol=length(a))
    s2[[j]] = matrix(,nrow=1,ncol=length(a))
    for (i in 1:length(a)){
      rf = randomForest(y=as.factor(g),
                        x=train[[j]][,1:a[i]],
                        ntrees=1000,
                        importance=T,
                        proximity=T)
      s1[[j]][,i] = 1 - (sum(rf$confusion[1,1],
                             rf$confusion[2,2])/
                           (length(train[[j]][,1])))
      s2[[j]][,i] = 1 - (sum(diag(table(predict(
        rf,test[[j]][,1:a[i]]),g)))/
          (length(test[[j]][,1])))
    }
  }
  return(list(s1=s1,s2=s2))
}

# make groups
g = c(rep(1,100),rep(2,100))


# every predictor between 2 and 100
a = seq(2,100,1)

rf_a1_detail = run.rft(train=train_a1,test=test_a1,n=100,d=max(a))
rf_a2_detail = run.rft(train=train_a2,test=test_a2,n=100,d=max(a))
rf_a3_detail = run.rft(train=train_a3,test=test_a3,n=100,d=max(a))
rf_a4_detail = run.rft(train=train_a4,test=test_a4,n=100,d=max(a))




# every 10th predictor between 2 and 5000
a = seq(0,5000,10)
a[1] = 2

rf_a1 = run.rft(train=train_a1,test=test_a1,n=100,d=max(a))
rf_a2 = run.rft(train=train_a2,test=test_a2,n=100,d=max(a))
rf_a3 = run.rft(train=train_a3,test=test_a3,n=100,d=max(a))
rf_a4 = run.rft(train=train_a4,test=test_a4,n=100,d=max(a))

#####################################################################

### support vector machine simulations
library(e1071)

# create classifier for SVM
run.svm = function(train,test,n){
  t1 = list()
  t2 = list()
  for (j in 1:n){
    t1[[j]] = matrix(,nrow=1,ncol=length(a))
    t2[[j]] = matrix(,nrow=1,ncol=length(a))
    for (i in 1:length(a)){
      sv = svm(y=as.factor(g),
               x=train[[j]][,1:a[i]],
               kernel="linear",
               cost=0.01)
      t1[[j]][,i] = 1 - ((sum(diag(table(g, predict(sv)))))/
                           (length(train[[j]][,1])))
      t2[[j]][,i] = 1-(sum(diag(table(predict(
        sv,test[[j]][,1:a[i]]),g)))/
          (length(test[[j]][,1])))
    }
  }
  return(list(t1=t1,t2=t2))
}


# make groups
g = c(rep(1,100),rep(2,100))


# every predictor between 2 and 100
a = seq(2,100,1)

svm_a1_detail = run.svm(train=train_a1,test=test_a1,n=100)
svm_a2_detail = run.svm(train=train_a2,test=test_a2,n=100)
svm_a3_detail = run.svm(train=train_a3,test=test_a3,n=100)
svm_a4_detail = run.svm(train=train_a4,test=test_a4,n=100)


# every 10th predictor between 2 and 5000
a = seq(0,5000,10)
a[1] = 2

svm_a1 = run.svm(train=train_a1,test=test_a1,n=100)
svm_a2 = run.svm(train=train_a2,test=test_a2,n=100)
svm_a3 = run.svm(train=train_a3,test=test_a3,n=100)
svm_a4 = run.svm(train=train_a4,test=test_a4,n=100)

#####################################################################

### boosted classification tree simulations
library(gbm)

# create classifier for boosting
run.gbm = function(train,test,n,d){
  u1 = list()
  u2 = list()
  for (j in 1:n){
    u1[[j]] = matrix(,nrow=1,ncol=length(a))
    u2[[j]] = matrix(,nrow=1,ncol=length(a))
    for (i in 1:length(a)){
      bt = gbm.fit(y=g,
                   x=train[[j]][,1:a[i]],
                   n.trees=10000,
                   verbose=FALSE)
      u1[[j]][,i] = 1-(sum(diag(table(ifelse(bt$fit<0,0,1),g)))/
                         (length(train[[j]][,1])))
      u2[[j]][,i] = 1-(sum(diag(table(ifelse((predict(
        bt,test[[j]][,1:a[i]],n.trees=10000))
        <0,0,1),g)))/(length(test[[j]][,1])))
    }
  }
  return(list(u1=u1,u2=u2
  ))
}

# make groups
g = c(rep(0,100),rep(1,100))

# every predictor between 2 and 100
a = seq(2,100,1)

gbm_a1_detail = run.gbm(train=train_a1,test=test_a1,n=100,d=max(a))
gbm_a2_detail = run.gbm(train=train_a2,test=test_a2,n=100,d=max(a))
gbm_a3_detail = run.gbm(train=train_a3,test=test_a3,n=100,d=max(a))
gbm_a4_detail = run.gbm(train=train_a4,test=test_a4,n=100,d=max(a))


# every 10th predictor between 2 and 5000
a = seq(0,5000,10)
a[1] = 2

gbm_a1 = run.gbm(train=train_a1,test=test_a1,n=100,d=max(a))
gbm_a2 = run.gbm(train=train_a2,test=test_a2,n=100,d=max(a))
gbm_a3 = run.gbm(train=train_a3,test=test_a3,n=100,d=max(a))
gbm_a4 = run.gbm(train=train_a4,test=test_a4,n=100,d=max(a))

#####################################################################

### Results 

## RF
# test error for every predictor between 2 and 100
t2 = do.call(rbind,rf_a1_detail$s2)
u2 = do.call(rbind,rf_a2_detail$s2)
v2 = do.call(rbind,rf_a3_detail$s2)
w2 = do.call(rbind,rf_a4_detail$s2)

t2.p = apply(t2,2,quantile,probs=c(.1,.5,.9))
u2.p = apply(u2,2,quantile,probs=c(.1,.5,.9))
v2.p = apply(v2,2,quantile,probs=c(.1,.5,.9))
w2.p = apply(w2,2,quantile,probs=c(.1,.5,.9))

# scenario 1
# 10, 50, and 90 percentile test error between p=1 and 10
t2.p[1,1:10]
t2.p[2,1:10]
t2.p[3,1:10]

# scenario 2
# 10, 50, and 90 percentile test error between p=1 and 10
u2.p[1,1:10]
u2.p[2,1:10]
u2.p[3,1:10]

# scenario 3
# 10, 50, and 90 percentile test error between p=1 and 10
v2.p[1,1:10]
v2.p[2,1:10]
v2.p[3,1:10]

# scenario 4
# 10, 50, and 90 percentile test error between p=1 and 10
w2.p[1,1:10]
w2.p[2,1:10]
w2.p[3,1:10]


# test error for every 10th predictor between 2 and 5000
t2 = do.call(rbind,rf_a1$s2)
u2 = do.call(rbind,rf_a2$s2)
v2 = do.call(rbind,rf_a3$s2)
w2 = do.call(rbind,rf_a4$s2)

t2.p_rf = apply(t2,2,quantile,probs=c(.1,.5,.9))
u2.p_rf = apply(u2,2,quantile,probs=c(.1,.5,.9))
v2.p_rf = apply(v2,2,quantile,probs=c(.1,.5,.9))
w2.p_rf = apply(w2,2,quantile,probs=c(.1,.5,.9))

# scenario 1
# 10, 50, and 90 percentile test error at p=5000
t2.p_rf[1,501]
t2.p_rf[2,501]
t2.p_rf[3,501]

# scenario 2
# 10, 50, and 90 percentile test error at p=5000
u2.p_rf[1,501]
u2.p_rf[2,501]
u2.p_rf[3,501]

# scenario 3
# 10, 50, and 90 percentile test error at p=5000
v2.p_rf[1,501]
v2.p_rf[2,501]
v2.p_rf[3,501]

# scenario 4
# 10, 50, and 90 percentile test error at p=5000
w2.p_rf[1,501]
w2.p_rf[2,501]
w2.p_rf[3,501]



## SVM
# test error for every predictor between 2 and 100
t2 = do.call(rbind,svm_a1_detail$t2)
u2 = do.call(rbind,svm_a2_detail$t2)
v2 = do.call(rbind,svm_a3_detail$t2)
w2 = do.call(rbind,svm_a4_detail$t2)

t2.p = apply(t2,2,quantile,probs=c(.1,.5,.9))
u2.p = apply(u2,2,quantile,probs=c(.1,.5,.9))
v2.p = apply(v2,2,quantile,probs=c(.1,.5,.9))
w2.p = apply(w2,2,quantile,probs=c(.1,.5,.9))

# scenario 1
# 10, 50, and 90 percentile test error between p=1 and 10
t2.p[1,1:10]
t2.p[2,1:10]
t2.p[3,1:10]

# scenario 2
# 10, 50, and 90 percentile test error between p=1 and 10
u2.p[1,1:10]
u2.p[2,1:10]
u2.p[3,1:10]

# scenario 3
# 10, 50, and 90 percentile test error between p=1 and 10
v2.p[1,1:10]
v2.p[2,1:10]
v2.p[3,1:10]

# scenario 4
# 10, 50, and 90 percentile test error between p=1 and 10
w2.p[1,1:10]
w2.p[2,1:10]
w2.p[3,1:10]


# test error for every 10th predictor between 2 and 5000
t2 = do.call(rbind,svm_a1$t2)
u2 = do.call(rbind,svm_a2$t2)
v2 = do.call(rbind,svm_a3$t2)
w2 = do.call(rbind,svm_a4$t2)

t2.p_svm = apply(t2,2,quantile,probs=c(.1,.5,.9))
u2.p_svm = apply(u2,2,quantile,probs=c(.1,.5,.9))
v2.p_svm = apply(v2,2,quantile,probs=c(.1,.5,.9))
w2.p_svm = apply(w2,2,quantile,probs=c(.1,.5,.9))

# scenario 1
# 10, 50, and 90 percentile test error at p=5000
t2.p_svm[1,501]
t2.p_svm[2,501]
t2.p_svm[3,501]

# scenario 2
# 10, 50, and 90 percentile test error at p=5000
u2.p_svm[1,501]
u2.p_svm[2,501]
u2.p_svm[3,501]

# scenario 3
# 10, 50, and 90 percentile test error at p=5000
v2.p_svm[1,501]
v2.p_svm[2,501]
v2.p_svm[3,501]

# scenario 4
# 10, 50, and 90 percentile test error at p=5000
w2.p_svm[1,501]
w2.p_svm[2,501]
w2.p_svm[3,501]



## GBM
# test error for every predictor between 2 and 100
t2 = do.call(rbind,gbm_a1_detail$u2)
u2 = do.call(rbind,gbm_a2_detail$u2)
v2 = do.call(rbind,gbm_a3_detail$u2)
w2 = do.call(rbind,gbm_a4_detail$u2)

t2.p = apply(t2,2,quantile,probs=c(.1,.5,.9))
u2.p = apply(u2,2,quantile,probs=c(.1,.5,.9))
v2.p = apply(v2,2,quantile,probs=c(.1,.5,.9))
w2.p = apply(w2,2,quantile,probs=c(.1,.5,.9))

# scenario 1
# 10, 50, and 90 percentile test error between p=1 and 10
t2.p[1,1:10]
t2.p[2,1:10]
t2.p[3,1:10]

# scenario 2
# 10, 50, and 90 percentile test error between p=1 and 10
u2.p[1,1:10]
u2.p[2,1:10]
u2.p[3,1:10]

# scenario 3
# 10, 50, and 90 percentile test error between p=1 and 10
v2.p[1,1:10]
v2.p[2,1:10]
v2.p[3,1:10]

# scenario 4
# 10, 50, and 90 percentile test error between p=1 and 10
w2.p[1,1:10]
w2.p[2,1:10]
w2.p[3,1:10]


# test error for every 10th predictor between 2 and 5000
t2 = do.call(rbind,gbm_a1$u2)
u2 = do.call(rbind,gbm_a2$u2)
v2 = do.call(rbind,gbm_a3$u2)
w2 = do.call(rbind,gbm_a4$u2)

t2.p_gbm = apply(t2,2,quantile,probs=c(.1,.5,.9))
u2.p_gbm = apply(u2,2,quantile,probs=c(.1,.5,.9))
v2.p_gbm = apply(v2,2,quantile,probs=c(.1,.5,.9))
w2.p_gbm = apply(w2,2,quantile,probs=c(.1,.5,.9))

# scenario 1
# 10, 50, and 90 percentile test error at p=5000
t2.p_gbm[1,501]
t2.p_gbm[2,501]
t2.p_gbm[3,501]

# scenario 2
# 10, 50, and 90 percentile test error at p=5000
u2.p_gbm[1,501]
u2.p_gbm[2,501]
u2.p_gbm[3,501]

# scenario 3
# 10, 50, and 90 percentile test error at p=5000
v2.p_gbm[1,501]
v2.p_gbm[2,501]
v2.p_gbm[3,501]

# scenario 4
# 10, 50, and 90 percentile test error at p=5000
w2.p_gbm[1,501]
w2.p_gbm[2,501]
w2.p_gbm[3,501]

#####################################################################

