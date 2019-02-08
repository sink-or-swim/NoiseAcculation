
#####################################################################

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


### generate data for simulations

## vary N where value of nonsparse elements = 1 
# number of simulations
iters = 30

# setup parallel backend to use 8 processors
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(1011)
# vary N  (mu=1,mu2=0,sd1=1,sd2=1,nonsparse=10)
# n=500
train_n500_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=250,ss2=250,nonsparse=10,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_n500_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=250,ss2=250,nonsparse=10,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

# n=1000
train_n1000_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=500,ss2=500,nonsparse=10,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

test_n1000_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=500,ss2=500,nonsparse=10,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

# n=5000
train_n5000_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=2500,ss2=2500,nonsparse=10,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_n5000_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=2500,ss2=2500,nonsparse=10,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
stopCluster(cl)
registerDoSEQ()


## vary N where value of nonsparse elements = 1/sqrt(m)
# number of simulations
iters = 30

# setup parallel backend to use 8 processors
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(1011)
# vary N  (mu=1,mu2=0,sd1=1,sd2=1,nonsparse=10)
# n=200
train_n200_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}
test_n200_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}

# n=500
train_n500_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=250,ss2=250,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}
test_n500_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=250,ss2=250,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}

# n=1000
train_n1000_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=500,ss2=500,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}
test_n1000_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=500,ss2=500,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}

# n=5000
train_n5000_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=2500,ss2=2500,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}
test_n5000_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=2500,ss2=2500,nonsparse=10,
        mu1=0,mu2=1/sqrt(10),sd1=1,sd2=1)
}
stopCluster(cl)
registerDoSEQ()



## vary m where value of nonsparse elements = 1
# number of simulations
iters = 30

# setup parallel backend to use 8 processors
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(1011)

# vary m  (mu=1,sd1=1,sd2=1,n1=100,n2=100)
# nonsparse elements=5
train_s5_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=5,mu1=0,mu2=1
        ,sd1=1,sd2=1)
}
test_s5_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=5,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

# nonsparse elements=20
train_s20_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=20,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_s20_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=20,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

# nonsparse elements=30
train_s30_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=30,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_s30_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=30,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

stopCluster(cl)
registerDoSEQ()


## vary m where value of nonsparse elements = 1/sqrt(m)
# number of simulations
iters = 30

# setup parallel backend to use 8 processors
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(1011)

# vary m  (mu=1,sd1=1,sd2=1,n1=100,n2=100)
# nonsparse elements=5
train_s5_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=5,mu1=0,
        mu2=1/sqrt(5),sd1=1,sd2=1)
}
test_s5_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=5,mu1=0,
        mu2=1/sqrt(5),sd1=1,sd2=1)
}

# nonsparse elements=10
train_s10_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=5,mu1=0,
        mu2=1/sqrt(10),sd1=1,sd2=1)
}
test_s10_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=5,mu1=0,
        mu2=1/sqrt(10),sd1=1,sd2=1)
}

# nonsparse elements=20
train_s20_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=20,mu1=0,
        mu2=1/sqrt(20),sd1=1,sd2=1)
}
test_s20_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=20,mu1=0,
        mu2=1/sqrt(20),sd1=1,sd2=1)
}

# nonsparse elements=30
train_s30_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=30,mu1=0,
        mu2=1/sqrt(30),sd1=1,sd2=1)
}
test_s30_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=30,mu1=0,
        mu2=1/sqrt(30),sd1=1,sd2=1)
}

stopCluster(cl)
registerDoSEQ()



## vary m : pmax  where value of nonsparse elements = 1
# number of simulations
iters = 30

# setup parallel backend to use 8 processors
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(1011)

# vary m and p (mu=1,mu2=0,sd1=1,sd2=1,n=200)

# p=15000, s=30 
train_p15000s30_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=15000,ss1=100,ss2=100,nonsparse=30,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_p15000s30_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=15000,ss1=100,ss2=100,nonsparse=30,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

# p=25000, s=50 
train_p25000s50_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=25000,ss1=100,ss2=100,nonsparse=50,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_p25000s50_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=25000,ss1=100,ss2=100,nonsparse=50,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

# p=35000, s=70
train_p35000s70_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=35000,ss1=100,ss2=100,nonsparse=70,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_p35000s70_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=35000,ss1=100,ss2=100,nonsparse=70,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

# p=45000, s=90
train_p45000s90_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=45000,ss1=100,ss2=100,nonsparse=90,mu1=0,mu2=1,
        sd1=1,sd2=1)
}
test_p45000s90_b1 = foreach(i=1:iters) %dorng% {
  draw3(d=45000,ss1=100,ss2=100,nonsparse=90,mu1=0,mu2=1,
        sd1=1,sd2=1)
}

stopCluster(cl)
registerDoSEQ()


## vary m : pmax  where value of nonsparse elements = 1/sqrt(m)
# number of simulations
iters = 30

# setup parallel backend to use 8 processors
cl <- makeCluster(8)
registerDoParallel(cl)

set.seed(1011)

# vary m and P (mu=1,mu2=0,sd1=1,sd2=1,n=200)
# p=5000, s=10 
train_p5000s10_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=20,mu1=0,
        mu2=1/sqrt(10),sd1=1,sd2=1)
}



test_p5000s10_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=5000,ss1=100,ss2=100,nonsparse=20,mu1=0,
        mu2=1/sqrt(10),sd1=1,sd2=1)
}

# p=15000, s=30 
train_p15000s30_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=15000,ss1=100,ss2=100,nonsparse=30,mu1=0,
        mu2=1/sqrt(30),sd1=1,sd2=1)
}
test_p15000s30_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=15000,ss1=100,ss2=100,nonsparse=30,mu1=0,
        mu2=1/sqrt(30),sd1=1,sd2=1)
}

# p=25000, s=50 
train_p25000s50_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=25000,ss1=100,ss2=100,nonsparse=50,mu1=0,
        mu2=1/sqrt(50),sd1=1,sd2=1)
}
test_p25000s50_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=25000,ss1=100,ss2=100,nonsparse=50,mu1=0,
        mu2=1/sqrt(50),sd1=1,sd2=1)
}

# p=35000, s=70
train_p35000s70_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=35000,ss1=100,ss2=100,nonsparse=70,mu1=0,
        mu2=1/sqrt(70),sd1=1,sd2=1)
}
test_p35000s70_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=35000,ss1=100,ss2=100,nonsparse=70,mu1=0,
        mu2=1/sqrt(70),sd1=1,sd2=1)
}

# p=45000, s=90
train_p45000s90_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=45000,ss1=100,ss2=100,nonsparse=90,mu1=0,
        mu2=1/sqrt(90),sd1=1,sd2=1)
}
test_p45000s90_b2 = foreach(i=1:iters) %dorng% {
  draw3(d=45000,ss1=100,ss2=100,nonsparse=90,mu1=0,
        mu2=1/sqrt(90),sd1=1,sd2=1)
}

stopCluster(cl)
registerDoSEQ()

#####################################################################

### simulations to vary sample size

## vary N  where value of nonsparse elements = 1

# every 100th predictor between 2 and 5000
a1 = seq(0,100,1)
a2 = seq(0,5000,100)
a = c(a1[3:101],a2[3:51])

g = c(rep(1,250),rep(2,250))
a1_n500_b1 = run.rft(train=train_n500_b1,test=test_n500_b1, 
                     n=30,d=max(a))


g = c(rep(1,500),rep(2,500))
a1_n1000_b1  = run.rft(train=train_n1000_b1,test=test_n1000_b1, 
                       n=30,d=max(a))

g = c(rep(1,2500),rep(2,2500))
a1_n5000_b1  = run.rft(train=train_n5000_b1,test=test_n5000_b1, 
                       n=30,d=max(a))


## vary N  where value of nonsparse elements = 1/sqrt(m)
# every 100th predictor between 2 and 5000
a1 = seq(0,100,1)
a2 = seq(0,5000,100)
a = c(a1[3:101],a2[3:51])

g = c(rep(1,100),rep(2,100))
a1_n200_b2  = run.rft(train=train_n200_b2,test=test_n200_b2, 
                      n=30,d=max(a))

g = c(rep(1,250),rep(2,250))
a1_n500_b2  = run.rft(train=train_n500_b2,test=test_n500_b2, 
                      n=30,d=max(a))

g = c(rep(1,500),rep(2,500))
a1_n1000_b2  = run.rft(train=train_n1000_b2,test=test_n1000_b2, 
                       n=30,d=max(a))

g = c(rep(1,2500),rep(2,2500))
a1_n5000_b2  = run.rft(train=train_n5000_b2,test=test_n5000_b2, 
                       n=30,d=max(a))



### simulations to vary signal strength
# every 100th predictor between 2 and 5000
a1 = seq(0,100,1)
a2 = seq(0,5000,100)
a = c(a1[3:101],a2[3:51])

g = c(rep(0,100),rep(1,100))


## vary m where value of nonsparse elements = 1
a1_s5_b1 = run.rft(train=train_s5_b1,test=test_s5_b1 
                   ,n=30,d=max(a))
a1_s20_b1 = run.rft(train=train_s20_b1,test=test_s20_b1, 
                    n=30,d=max(a))
a1_s30_b1 = run.rft(train=train_s30_b1,test=test_s30_b1, 
                    n=30,d=max(a))

## vary m where value of nonsparse elements = 1/sqrt(m)
a1_s5_b2 = run.rft(train=train_s5_b2,test=test_s5_b2, 
                   n=30,d=max(a))
a1_s10_b2 = run.rft(train=train_s10_b2,test=test_s10_b2, 
                    n=30,d=max(a))
a1_s20_b2 = run.rft(train=train_s20_b2,test=test_s20_b2, 
                    n=30,d=max(a))
a1_s30_b2 = run.rft(train=train_s30_b2,test=test_s30_b2, 
                    n=30,d=max(a))



### simulations to vary m : pmax  where value of 
### nonsparse elements = 1
g = c(rep(0,100),rep(1,100))

# m : pmax = 30 : 15000 for every 100th predictor, p = 2 -15000
a1 = seq(0,100,1)
a2 = seq(0,15000,100)
a = c(a1[3:101],a2[3:151])
a1_p15000s30_b1 = run.rft(train=train_p15000s30_b1,
                          test=test_p15000s30_b1 n=30,d=max(a))

# m : pmax = 50 : 25000 for every 100th predictor, p = 2 - 25000
a1 = seq(0,100,1)
a2 = seq(0,25000,100)
a = c(a1[3:101],a2[3:251])
a1_p25000s50_b1 = run.rft(train=train_p25000s50_b1,
                          test=test_p25000s50_b1,n=30,d=max(a))

# m : pmax = 70 : 35000 for every 100th predictor, p = 2 - 35000
a1 = seq(0,100,1)
a2 = seq(0,35000,100)
a = c(a1[3:101],a2[3:351])

a1_p35000s70_b1 = run.rft(train=train_p35000s70_b1,
                          test=test_p35000s70_b1,n=30,d=max(a))

# m : pmax = 90 : 45000 for every 100th predictor p = 2 - 45000
a1 = seq(0,100,1)
a2 = seq(0,45000,100)
a = c(a1[3:101],a2[3:451])
a1_p45000s90_b1 = run.rft(train=train_p45000s90_b1,
                          test=test_p45000s90_b1,n=30,d=max(a))


### simulations to vary m : pmax where value of 
### nonsparse elements = 1/sqrt(m)
g = c(rep(0,100),rep(1,100))

# m : pmax = 10 : 5000 for every 100th predictor, p = 2 - 5000
a1 = seq(0,100,1)
a2 = seq(0,5000,100)
a = c(a1[3:101],a2[3:151])
a1_p5000s10_b1 = run.rft(train=train_p5000s10_b2,
                         test=test_p5000s10_b2 n=30,d=max(a))

# m : pmax = 30 : 15000 for every 100th predictor,  p = 2 - 25000
a1 = seq(0,100,1)
a2 = seq(0,15000,100)
a = c(a1[3:101],a2[3:151])
a1_p15000s30_b2 = run.rft(train=train_p15000s30_b2,
                          test=test_p15000s30_b2 n=30,d=max(a))

# m : pmax = 50 : 25000 for every 100th predictor, p = 2 - 25000
a1 = seq(0,100,1)
a2 = seq(0,25000,100)
a = c(a1[3:101],a2[3:251])
a1_p25000s50_b2 = run.rft(train=train_p25000s50_b2,
                          test=test_p25000s50_b2,n=30,d=max(a))

# m : pmax = 70 : 35000 for every 100th predictor, p = 2 - 35000
a1 = seq(0,100,1)
a2 = seq(0,35000,100)
a = c(a1[3:101],a2[3:351])
a1_p35000s70_b2 = run.rft(train=train_p35000s70_b2,
                          test=test_p35000s70_b2,n=30,d=max(a))

# m : pmax = 90 : 45000 for every 100th predictor,  p = 2 - 45000
a1 = seq(0,100,1)
a2 = seq(0,45000,100)
a = c(a1[3:101],a2[3:451])
a1_p45000s90_b2 = run.rft(train=train_p45000s90_b2,
                          test=test_p45000s90_b2,n=30,d=max(a))

#####################################################################

### Results 

## make baseline for value of nonsparse elements = 1
b1_train1 = do.call(rbind,rf_a4_detail$s1)
b1_test1= do.call(rbind,rf_a4_detail$s2)

b1 = seq(0,5000,10)
rf_a4_foo.test = list()
rf_a4_use.test = list()
for(j in 1:100){
  rf_a4_foo.test[[j]] = rf_a4$s2[[j]][12:501]
  i = 1
  rf_a4_use.test[[j]] = rf_a4_foo.test[[j]][1:(i+9)==(i+9)]
}
b1_test2 = do.call(rbind,rf_a4_use.test)

tq.test2a = apply(b1_test1,2,median)
tq.test2b = apply(b1_test2,2,median)
base.test.50= c(tq.test2a,tq.test2b)

nq.test1.p1 = apply(t.test2a,2,quantile,probs=c(.1,.9))
nq.test1.p2 = apply(t.test2b,2,quantile,probs=c(.1,.9))
base.test.10 = c(nq.test1.p1[1,],nq.test1.p2[1,])
base.test.90 = c(nq.test1.p1[2,],nq.test1.p2[2,])



## Vary N

a1 = seq(0,100,1)
a2 = seq(0,5000,100)
a = c(a1[3:101],a2[3:51])


## nonsparse elements = 1
s.test2  = do.call(rbind,a1_n500_b1$s2)
s.test3  = do.call(rbind,a1_n1000_b1$s2)
s.test4  = do.call(rbind,a1_n5000_b1$s2)

n500_b1.p = apply(s.test2,2, quantile,probs=c(.1,.5,.9))
n1000_b1.p = apply(s.test3,2, quantile,probs=c(.1,.5,.9))
n5000_b1.p = apply(s.test4,2, quantile,probs=c(.1,.5,.9))

base.test.50[1:9]
base.test.10[1:9]
base.test.90[1:9]
base.test.50[148]
base.test.10[148]
base.test.90[148]

n500_b1.p[2,1:9]
n500_b1.p[1,1:9]
n500_b1.p[3,1:9]
n500_b1.p[2,148]
n500_b1.p[1,148]
n500_b1.p[3,148]

n1000_b1.p[2,1:9]
n1000_b1.p[1,1:9]
n1000_b1.p[3,1:9]
n1000_b1.p[2,148]
n1000_b1.p[1,148]
n1000_b1.p[3,148]

n5000_b1.p[2,1:9]
n5000_b1.p[1,1:9]
n5000_b1.p[3,1:9]
n5000_b1.p[2,148]
n5000_b1.p[1,148]
n5000_b1.p[3,148]


## nonsparse elements = 1/sqrt(m)
t.test1  = do.call(rbind,a1_n200_b2$s2)
t.test2  = do.call(rbind,a1_n500_b2$s2)
t.test3  = do.call(rbind,a1_n1000_b2$s2)
t.test4  = do.call(rbind,a1_n5000_b2$s2)

n100_b2.p = apply(t.test1,2, quantile,probs=c(.1,.5,.9))
n500_b2.p = apply(t.test2,2, quantile,probs=c(.1,.5,.9))
n1000_b2.p = apply(t.test3,2, quantile,probs=c(.1,.5,.9))
n5000_b2.p = apply(t.test4,2, quantile,probs=c(.1,.5,.9))

n200_b2.p[2,1:9]
n200_b2.p[1,1:9]
n200_b2.p[3,1:9]
n200_b2.p[2,148]
n200_b2.p[1,148]
n200_b2.p[3,148]

n500_b2.p[2,1:9]
n500_b2.p[1,1:9]
n500_b2.p[3,1:9]
n500_b2.p[2,148]
n500_b2.p[1,148]
n500_b2.p[3,148]

n1000_b2.p[2,1:9]
n1000_b2.p[1,1:9]
n1000_b2.p[3,1:9]
n1000_b2.p[2,148]
n1000_b2.p[1,148]
n1000_b2.p[3,148]

n5000_b2.p[2,1:9]
n5000_b2.p[1,1:9]
n5000_b2.p[3,1:9]
n5000_b2.p[2,148]
n5000_b2.p[1,148]
n5000_b2.p[3,148]



## Vary m

## nonsparse elements = 1
u.test1 = do.call(rbind,a1_s5_b1$s2)
u.test3 = do.call(rbind,a1_s20_b1$s2)
u.test4 = do.call(rbind,a1_s30_b1$s2)

s5_b1.p = apply(u.test1,2,quantile,probs=c(.1,.5,.9))
s20_b1.p = apply(u.test3,2, quantile,probs=c(.1,.5,.9))
s30_b1.p = apply(u.test4,2, quantile,probs=c(.1,.5,.9))

s5_b1.p[1,1:9]
s5_b1.p[3,1:9]
s5_b1.p[2,148]
s5_b1.p[1,148]
s5_b1.p[3,148]

base.test.50[1:9]
base.test.10[1:9]
base.test.90[1:9]
base.test.50[148]
base.test.10[148]
base.test.90[148]

s20_b1.p[1,1:9]
s20_b1.p[3,1:9]
s20_b1.p[2,148]
s20_b1.p[1,148]
s20_b1.p[3,148]

s30_b1.p[1,1:9]
s30_b1.p[3,1:9]
s30_b1.p[2,148]
s30_b1.p[1,148]
s30_b1.p[3,148]


## nonsparse elements = 1/sqrt(m)
v.test1 = do.call(rbind,a1_s5_b2$s2)
v.test2 = do.call(rbind,a1_s10_b2$s2)
v.test3 = do.call(rbind,a1_s20_b2$s2)
v.test4 = do.call(rbind,a1_s30_b2$s2)

s5_b2.p = apply(v.test1,2,quantile,probs=c(.1,.5,.9))
s10_b2.p = apply(v.test3,2, quantile,probs=c(.1,.5,.9))
s20_b2.p = apply(v.test3,2, quantile,probs=c(.1,.5,.9))
s30_b2.p = apply(v.test4,2, quantile,probs=c(.1,.5,.9))

s5_b2.p[1,1:9]
s5_b2.p[3,1:9]
s5_b2.p[2,148]
s5_b2.p[1,148]
s5_b2.p[3,148]

s10_b2.p[1,1:9]
s10_b2.p[3,1:9]
s10_b2.p[2,148]
s10_b2.p[1,148]
s10_b2.p[3,148]

s20_b2.p[1,1:9]
s20_b2.p[3,1:9]
s20_b2.p[2,148]
s20_b2.p[1,148]
s20_b2.p[3,148]

s30_b2.p[1,1:9]
s30_b2.p[3,1:9]
s30_b2.p[2,148]
s30_b2.p[1,148]
s30_b2.p[3,148]



## Vary m : pmax

## nonsparse elements = 1
w.test2  = do.call(rbind,a1_p15000s30_b1$s2)
w.test3  = do.call(rbind,a1_p25000s50_b1$s2)
w.test4  = do.call(rbind, a1_p35000s70_b1$s2)
w.test5  = do.call(rbind, a1_p45000s90_b1$s2)

s30p15000_b1.p = apply(w.test2,2, quantile,probs=c(.1,.5,.9))
s50p25000_b1.p = apply(w.test3,2, quantile,probs=c(.1,.5,.9))
s70p35000_b1.p = apply(w.test4,2, quantile,probs=c(.1,.5,.9))
s90p45000_b1.p = apply(w.test5,2,quantile,probs=c(.1,.5,.9))

base.test.50[1:9]
base.test.10[1:9]
base.test.90[1:9]
base.test.50[148]
base.test.10[148]
base.test.90[148]

s30p15000_b1.p [2,1:9]
s30p15000_b1.p [1,1:9]
s30p15000_b1.p [3,1:9]
s30p15000_b1.p [2,248]
s30p15000_b1.p [1,248]
s30p15000_b1.p [3,248]

s50p25000_b1.p [2,1:9]
s50p25000_b1.p [1,1:9]
s50p25000_b1.p [3,1:9]
s50p25000_b1.p [2,248]
s50p25000_b1.p [1,248]
s50p25000_b1.p [3,248]

s70p35000_b1.p [2,1:9]
s70p35000_b1.p [1,1:9]
s70p35000_b1.p [3,1:9]
s70p35000_b1.p [2,248]
s70p35000_b1.p [1,248]
s70p35000_b1.p [3,248]

s90p45000_b1.p [2,1:9]
s90p45000_b1.p [1,1:9]
s90p45000_b1.p [3,1:9]
s90p45000_b1.p [2,248]
s90p45000_b1.p [1,248]
s90p45000_b1.p [3,248]


## nonsparse elements = 1/sqrt(m)
x.test1 = do.call(rbind,a1_p15000s30_b2$s2)
x.test2 = do.call(rbind,a1_p15000s30_b2$s2)
x.test3 = do.call(rbind,a1_p25000s50_b2$s2)
x.test4 = do.call(rbind, a1_p35000s70_b2$s2)
x.test5 = do.call(rbind, a1_p45000s90_b2$s2)

s10p5000_b2.p = apply(x.test1,2, quantile,probs=c(.1,.5,.9))
s30p15000_b2.p = apply(x.test2,2, quantile,probs=c(.1,.5,.9))
s50p25000_b2.p = apply(x.test3,2, quantile,probs=c(.1,.5,.9))
s70p35000_b2.p = apply(x.test4,2, quantile,probs=c(.1,.5,.9))
s90p45000_b2.p = apply(x.test5,2,quantile,probs=c(.1,.5,.9))

s10p5000_b2.p [2,1:9]
s10p5000_b2.p [1,1:9]
s10p5000_b2.p [3,1:9]
s10p5000_b2.p [2,248]
s10p5000_b2.p [1,248]
s10p5000_b2.p [3,248]

s30p15000_b2.p [2,1:9]
s30p15000_b2.p [1,1:9]
s30p15000_b2.p [3,1:9]
s30p15000_b2.p [2,248]
s30p15000_b2.p [1,248]
s30p15000_b2.p [3,248]

s50p25000_b2.p [2,1:9]
s50p25000_b2.p [1,1:9]
s50p25000_b2.p [3,1:9]
s50p25000_b2.p [2,248]
s50p25000_b2.p [1,248]
s50p25000_b2.p [3,248]

s70p35000_b2.p [2,1:9]
s70p35000_b2.p [1,1:9]
s70p35000_b2.p [3,1:9]
s70p35000_b2.p [2,248]
s70p35000_b2.p [1,248]
s70p35000_b2.p [3,248]

s90p45000_b2.p [2,1:9]
s90p45000_b2.p [1,1:9]
s90p45000_b2.p [3,1:9]
s90p45000_b2.p [2,248]
s90p45000_b2.p [1,248]
s90p45000_b2.p [3,248]

#####################################################################

