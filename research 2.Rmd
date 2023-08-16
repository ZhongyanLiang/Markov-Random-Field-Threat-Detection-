---
title: "research"
author: "Zhongyan Liang"
date: "3/11/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
setwd("~/Desktop/research")
BALT = read.csv("Baltimore_911_Calls_for_Service.csv")

Ntotal = dim(BALT)[1]           # Ntotal = 2831903 calls

# B = BALT[ (Ntotal - 1000000) : Ntotal , ]
B = na.omit(BALT)
attach(B)

B$month = as.numeric(substr(B$callDateTime,start=1,stop=2))
B$day   = as.numeric(substr(B$callDateTime,start=4,stop=5))
B$year  = as.numeric(substr(B$callDateTime,start=7,stop=10))
B$hour  = as.numeric(substr(B$callDateTime,start=12,stop=13))
B$min   = as.numeric(substr(B$callDateTime,start=15,stop=16))
B$sec   = as.numeric(substr(B$callDateTime,start=18,stop=19))
B$ampm  = substr(B$callDateTime,start=21,stop=22)
B$hour = B$hour*(B$hour < 12) + 12*(B$hour < 12 & B$ampm=="PM") + 12*(B$hour == 12 & B$ampm=="PM")
B$t = B$sec + 60* B$min + 3600* B$hour

B = B[B$year==2017, ]; B = na.omit(B);
B = B[B$month==8, ]; B = na.omit(B);
B = B[B$day==18, ]; B = na.omit(B);  		# Aug 18, 2017
attach(B)

B$lat   = as.numeric(substr(B$location,start=2,stop=7))
B$long = abs(as.numeric(substr(B$location,start=13,stop=19)))

h = 2.5;
y1 = quantile(B$lat,.25 ,na.rm = TRUE) - h*IQR(B$lat ,na.rm = TRUE)
y2 = quantile(B$lat,.75 ,na.rm = TRUE) + h*IQR(B$lat ,na.rm = TRUE)
x1 = quantile(B$long,.25 ,na.rm = TRUE) - h*IQR(B$long ,na.rm = TRUE)
x2 = quantile(B$long,.75 ,na.rm = TRUE) + h*IQR(B$long ,na.rm = TRUE)

Z = as.numeric(B$lat > y1 & B$lat < y2 & B$long > x1 & B$long < x2)

B$latitude = B$lat
B$longitude = -B$long
B = B[Z==1,]

table(B$priority)
dim(B)

B$level = 1*( B$priority=="Non-Emergency") + 3*( B$priority=="Low") + 4*( B$priority=="Medium") + 2*( B$priority=="High") + 6*( B$priority=="Emergency") 
# 1 = black, 2 = red, 3 = green, 4 = blue, 6 = maroon

plot(B$longitude, B$latitude,xlim=c(-x2,-x1),ylim=c(y1,y2))

points(B$longitude, B$latitude,xlim=c(-x2,-x1),ylim=c(y1,y2),col=B$level, pch=20)

hist(B$t/3600,main=" ",xlab="Hour")

xx = c(-76.67,-76.57); yy = c(39.27,39.32);

plot(B$longitude, B$latitude,xlim=xx,ylim=yy)
points(B$longitude, B$latitude,xlim=xx,ylim=yy,col=B$level, pch=20)
lines(xx,c(1,1)*yy[1] ,lwd=3); lines(xx,c(1,1)*yy[2] ,lwd=3); lines(xx[1]*c(1,1),yy ,lwd=3); lines(xx[2]*c(1,1),yy ,lwd=3);

plot(B$longitude, B$latitude,xlim=xx,ylim=yy)
points(B$longitude, B$latitude,xlim=xx,ylim=yy,col=B$level, pch=16)
lines(xx,c(1,1)*yy[1] ,lwd=3); lines(xx,c(1,1)*yy[2] ,lwd=3); lines(xx[1]*c(1,1),yy ,lwd=3); lines(xx[2]*c(1,1),yy ,lwd=3);

# Grid
Nlines = 50; DeltaH = (xx[2]-xx[1])/(Nlines+1); DeltaV = (yy[2]-yy[1])/(Nlines+1);

for (k in 0 : (Nlines)){ lines(xx,c(1,1)*(yy[1]+k*DeltaV)); lines(c(1,1)*(xx[1]+k*DeltaH),yy); }
lines(xx,c(1,1)*yy[1] ,lwd=3); lines(xx,c(1,1)*yy[2] ,lwd=3); lines(xx[1]*c(1,1),yy ,lwd=3); lines(xx[2]*c(1,1),yy ,lwd=3);

```

```{r}
library(tidyverse)
mat <- matrix(rep(0, 51*51), nrow = 51)
lat <- B$latitude
long <- B$longitude
df <- data.frame(lat, long) %>% filter(lat <= yy[2] & lat >= yy[1] & long <= xx[2] & long >= xx[1])
# grid lines
vlines <- hlines <-  cbind (rep(NA, 51),rep(NA, 51))
for (k in 0 : (Nlines)){
  hlines[k+1,1] <- yy[1]+k*DeltaV # lower
  hlines[k+1,2] <- yy[1]+(k+1)*DeltaV # upper
  vlines[k+1,1] <- xx[1]+k*DeltaH
  vlines[k+1,2] <- xx[1]+(k+1)*DeltaH
}
# locate
for(i in 1:nrow(df)){
  row <- which.min(ifelse(df$lat[i] - hlines[,1] >= 0, df$lat[i] - hlines[,1], NA))
  col <- which.min(ifelse(df$long[i] - vlines[,1] >= 0, df$long[i] - vlines[,1], NA))
  mat[row,col] <- 1
}
# 
a <- b <- c <- d <- e <- f <- g <- h <- k <- matrix(NA,(50-2+1),(50-2+1))
for(i in 2:50){
  for(j in 2:50){
    a[i-1,j-1] <- mat[i+1,j-1]
    b[i-1,j-1] <- mat[i+1,j]
    c[i-1,j-1] <- mat[i+1,j+1]
    d[i-1,j-1] <- mat[i,j-1]
    e[i-1,j-1] <- mat[i,j]
    f[i-1,j-1] <- mat[i,j+1]
    g[i-1,j-1] <- mat[i-1,j-1]
    h[i-1,j-1] <- mat[i-1,j]
    k[i-1,j-1] <- mat[i-1,j+1]
  }
}
Na <- a+b+c+d+e+f+g+h+k
Nl <- a*b+b*c+d*e+e*f+g*h+h*k
Nm <- a*d+d*g+b*e+e*h+c*f+f*k
Nn <- a*e+b*f+d*h+e*k
Np <- d*b+e*c+g*e+h*f
Ne <- d*b*e + e*c*f + g*e*h + h*f*k
Nz <- d*a*b + e*b*c + g*d*e + h*e*f
Nt <- a*b*e + b*c*f + d*e*h + e*f*k
Ni <- d*a*e + e*b*f + g*d*h + h*e*k
Nk <- a*b*d*e + e*b*c*f + d*e*g*h + e*f*h*k
for(i in 2:50){
  for(j in 2:50){
    e[i-1,j-1] <- 1 - mat[i,j]
  }
}
Ma <- a+b+c+d+e+f+g+h+k
Ml <- a*b+b*c+d*e+e*f+g*h+h*k
Mm <- a*d+d*g+b*e+e*h+c*f+f*k
Mn <- a*e+b*f+d*h+e*k
Mp <- d*b+e*c+g*e+h*f
Me <- d*b*e + e*c*f + g*e*h + h*f*k
Mz <- d*a*b + e*b*c + g*d*e + h*e*f
Mt <- a*b*e + b*c*f + d*e*h + e*f*k
Mi <- d*a*e + e*b*f + g*d*h + h*e*k
Mk <- a*b*d*e + e*b*c*f + d*e*g*h + e*f*h*k
N <- cbind(t(Na),t(Nl),t(Nm),t(Nn),t(Np),t(Ne),t(Nz),t(Nt),t(Ni),t(Nk))
M <- cbind(t(Ma),t(Ml),t(Mm),t(Mn),t(Mp),t(Me),t(Mz),t(Mt),t(Mi),t(Mk))
DIFF= M-N
DIFF
#logistic regression
fit=glm(mat[2:50,]~DIFF,family = binomial)
```


```{r}
# Pseudo likelihood:
IsingPL <- function(
  x, # Vector or data frame containing data
 graph, thresholds, beta, responses = c(0L,1L))
{
  stopifnot(isSymmetric(graph))  
  stopifnot(length(responses)==2)
  if (any(diag(graph)!=0))
  {
    diag(graph) <- 0
    warning("Diagonal set to 0")
  }
  N <- nrow(graph)
  
  # If x is vector, turn it into a matrix:
  if (is.vector(x)){
    x <- t(x)
  }
  
  if (is.data.frame(x)){
    x <- as.matrix(x)
  }
  
  stopifnot(is.matrix(x))
  
  # Compute pseudo likelihood:
  PL <- PseudoLikelihood(x, graph, thresholds, beta, responses, logis = TRUE)
  
  
  return(PL)
}
# Optimisation:
EstimateIsingPL <- function(data, responses, beta = 1, ...){
  if (missing(responses)){
    responses <- sort(unique(c(data)))
  }

  if (length(responses) != 2){
    stop("Binary data required")
  }
  
  # Optimisation function:
  optimFun <- function(par, data){
    Ni <- ncol(data)
    stopifnot(length(par) == (Ni*(Ni-1)/2)+Ni)
    graph <- matrix(0,Ni, Ni)
    graph[upper.tri(graph)] <- par[1:(Ni*(Ni-1)/2)]
    graph[lower.tri(graph)] <- t(graph)[lower.tri(graph)]
    thresholds <- par[(Ni*(Ni-1)/2+1):length(par)]
    -2*IsingPL(data, graph, thresholds, 1, responses = sort(unique(c(data))))
  }
# Run optimizer:
  Ni <- ncol(data)
#   optimRes <- optim(rep(0,(Ni*(Ni-1)/2)+Ni), optimFun, data = Data, ...)
  optimRes <- nlm(optimFun, rep(0,(Ni*(Ni-1)/2)+Ni), data = data, ...)
  
  # Cunstruct graph and thresholds:
  par <- optimRes$estimate
  graph <- matrix(0,Ni, Ni)
  graph[upper.tri(graph)] <- par[1:(Ni*(Ni-1)/2)]
  graph[lower.tri(graph)] <- t(graph)[lower.tri(graph)]
  thresholds <- par[(Ni*(Ni-1)/2+1):length(par)]
  
  return(list(
    graph = graph,
    thresholds = thresholds,
    results = optimRes))
}
```

```{r}
# suppose we have a sequence of values from unknown Bernoulli variable
p.parameter <- 0.8
sequence <- rbinom(10, 1, p.parameter)
# Given the sequence, we want to estimate the value of the parameter, p, which is not known to us.
# To find the parameter, First, we want to define a function that specifies the probability of our entire data set.
likelihood <- function(sequence, p.parameter)
{
  likelihood <- 1
 
  for (i in 1:length(sequence))
  {
    if (sequence[i] == 1)
    {
      likelihood <- likelihood * p.parameter
    }
    else
    {
      likelihood <- likelihood * (1 - p.parameter)
    }
  }
 
  return(likelihood)
}
# To do maximum likelihood estimation, we therefore only need to use an optimization function to maximize this function.
mle.results <- optimize(function(p) {likelihood(sequence, p)},
                        interval = c(0, 1),
                        maximum = TRUE)
mle.results                       
# When data is too big then we need log-liklihood and maximize it
log.likelihood <- function(sequence, p)
{
  log.likelihood <- 0
 
  for (i in 1:length(sequence))
  {
    if (sequence[i] == 1)
    {
      log.likelihood <- log.likelihood + log(p)
    }
    else
    {
      log.likelihood <- log.likelihood + log(1 - p)
    }
  }
 
  return(log.likelihood)
}
log.likelihood.results <- optimize(function(p) {log.likelihood(sequence, p)},
                                     interval = c(0, 1),
                                     maximum = TRUE)
# A more general form of log-likelihood function for any dataset or model
log.likelihood2 <- function(sequence.as.data.frame, likelihood.function, parameters)
{
  log.likelihood2 <- 0
 
  for (i in 1:nrow(sequence.as.data.frame))
  {
    log.likelihood2 <- log.likelihood2 + log(likelihood.function(sequence.as.data.frame[i,], parameters))
  }
 
  return(log.likelihood2)
}
log.likelihood.results2 <- optimize(function(p) {log.likelihood2(sequence.as.data.frame, likelihood.function, parameters)},interval = c(0, 1),maximum = TRUE)
```

```{r}
library(CRF)
#We set the parameters for Markov chain model:
n.nodes <- 10
n.states <- 2
prior.prob <- c(0.8, 0.2)
trans.prob <- matrix(0, nrow=2, ncol=2)
trans.prob[1,] <- c(0.95, 0.05)
trans.prob[2,] <- c(0.05, 0.95)
#The Markov chain consists of 10 nodes and there are 2 states for each node. The prior probability is:
prior.prob
#and the transition probability is:
trans.prob
#Then we constructed the adjacent matrix of chain:
adj <- matrix(0, n.nodes, n.nodes)
for (i in 1:(n.nodes-1))
{
adj[i, i+1] <- 1
}
# Now we can build the CRF object for Markov chain model:
mc <- make.crf(adj, n.states)
# and set the parameters:
mc$node.pot[1,] <- prior.prob
for (i in 1:mc$n.edges)
{
mc$edge.pot[[i]] <- trans.prob
}
# We generated 10000 samples from the Markov chain model and displayed the first 10 samples:
mc.samples <- sample.chain(mc, 10000)
mc.samples[1:10, ]
# In order to learn Markov random field model from generated data, we first built another CRF object:
mrf.new <- make.crf(adj, n.states)
# and created the paramter structure:
mrf.new <- make.features(mrf.new)
mrf.new <- make.par(mrf.new, 4)
# We only need 4 paramters in the MRF model, one for prior probability and three for transition probability,
# since the probabilities are summed to one.
mrf.new$node.par[1,1,1] <- 1
for (i in 1:mrf.new$n.edges)
{
mrf.new$edge.par[[i]][1,1,1] <- 2
mrf.new$edge.par[[i]][1,2,1] <- 3
mrf.new$edge.par[[i]][2,1,1] <- 4
}
# Then we trained the model using train.mrf function:
# Train the MRF model to estimate the parameters
mrf.new <- train.mrf(mrf.new, mc.samples)
```


