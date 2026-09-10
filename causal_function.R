# install required packages
# install.packages("stargazer")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("gridExtra")
# install.packages("AER")
# install.packages("MASS")
# install.packages("glmnet")
# install.packages("simstudy")
# install.packages("randomForest")
# install.packages("grf")
# install.packages("lubridate")

# load required packages
library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)
library(lubridate)
library(stargazer)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(AER)
library(MASS)
library(glmnet)
library(simstudy)
library(randomForest)
library(grf)
library(lubridate)

###############################################################

## Controlled / Fixed Effects Regression

# set constants:
# n = number of data points; n.time.periods = years of data; 
# n.products = number or products; B = true regression coeff
sim.fixed.effects.df<-function(n=10^4,n.time.periods=10,
                               n.products=5,B=2) {
  set.seed(30)
  #set variables; note X depends on fixed effects & other controls
  Time.FE<-paste("Year",rep((year(Sys.Date())-n.time.periods+1):
                              year(Sys.Date()),
                            times=ceiling(n/n.time.periods))[1:n])
  Product.FE<-rep(paste("Product",LETTERS[1:n.products]),
                  each=ceiling(n/n.products))[1:n]
  Customer.Rating<-round(pmax(pmin(rnorm(n=n,mean=5,sd=1),5),1),2)
  Customer.Age<-round(rnorm(n=n,mean=30,sd=4),0)
  Total.Purchases<-round(100*Customer.Rating+rnorm(n=n,mean=10),0)
  e3<-rnorm(n=n,sd=sd(Customer.Rating))
  Customer.Spend<-round(500+B*Customer.Rating+10*Customer.Age+10*
                          as.numeric(as.factor(Time.FE))+
                          20*as.numeric(as.factor(Product.FE))+e3,0)
  dat<-data.frame(Customer.Spend,Customer.Rating,Customer.Age,
                  Product.FE,Time.FE,Total.Purchases)
  return(dat)
}

###############################################################

## Regression Discontinuity

# set constants:
# cutoff = number between 0 and 100 to set discontinuity
# mu = mean customer spend; sd = standard dev of customer spend
# treatment.eff = causal impact of intervention point cutoff
sim.reg.discontinuity.df<-function(cutoff=70,mu=20,sigma=5,
                                   treatment.eff=25) {
  set.seed(30)
  dat<-data.frame("Lead.Score"=seq(from=0,to=100,by=1))
  dat$Add.Support<-dat$Lead.Score>=cutoff
  dat$Counterfactual<-dat$Lead.Score*rnorm(n=nrow(dat),mean=mu,
                                           sd=sigma)/10
  dat$Customer.Spend[!dat$Add.Support]<-
    dat$Counterfactual[!dat$Add.Support]
  dat$Customer.Spend[dat$Add.Support]<-
    dat$Lead.Score[dat$Add.Support]*rnorm(n=sum(dat$Add.Support),
                                          mean=mu+treatment.eff,
                                          sd=sigma)/10
  return(dat)
}

###############################################################

## Difference in Difference

# set constants:
# mu1 = mean of base group / US
# sigma1 = standard dev of base group / US
# mu2 = mean of treatment group / AU
# sigma2 = standard dev of treatment group / AU
# time.change = change in group mean over time
# causal.effect = causal impact of intervention in post period
# and treatment group
sim.diff.in.diff.df<-function(mu1=100,mu2=200,sigma1=25,sigma2=25,
                              time.change=200,causal.effect=100) {
  set.seed(30)
  dat<-data.frame("Time"=rep(seq(from=as.Date("2018-01-01"),
                                 to=as.Date("2020-01-01"),
                                 by="month"),times=2))
  dat$Period<-ifelse(dat$Time<"2019-01-01","Pre.Price.Change",
                     "Post.Price.Change")
  dat$Country<-rep(c("US","AU"),each=nrow(dat)/2)
  dat$Revenue<-c(rnorm(sum(dat$Time<"2019-01-01")/2,mu1,sigma1),
                 rnorm(sum(dat$Time>="2019-01-01")/2,
                       mu1+time.change,sigma1),
                 rnorm(sum(dat$Time<"2019-01-01")/2,mu2,sigma2),
                 rnorm(sum(dat$Time>="2019-01-01")/2,
                       mu2+time.change+causal.effect,sigma2))
  dat$Counterfactual<-dat$Revenue
  dat$Counterfactual[dat$Period=="Post.Price.Change"&
                       dat$Country=="US"
  ]<-rnorm(sum(dat$Time>="2019-01-01")/2,
           mu2+time.change,sigma2)
  dat$Period<-relevel(factor(dat$Period),ref="Pre.Price.Change")
  dat$Country<-relevel(factor(dat$Country),ref="US")
  return(dat)
}

###############################################################

## Instrumental Variable

# set constants:
# n = number of observations
# latent.prob.impact = impact of latent variable on both X
# and Y propensity
# intervention.impact = impact of instrument variable on both X 
# propensity (assume impact on Y is zero)
sim.iv.df<-function(n=10^4,latent.prob.impact=0.25,
                    intervention.impact=0.25) {
  set.seed(30)
  dat<-data.frame("Received.Email"=sample(c(0,1),size=n,replace=T))
  dat$Unobs.Motivation<-rnorm(n=n)
  dat$Use.Mobile.App<-sapply(1:n,function(r) {
    sample(c(0,1),size=1,prob=c(0.5-latent.prob.impact*
                                  (dat$Unobs.Motivation[r]>0)-
                                  intervention.impact*
                                  (dat$Received.Email[r]==1),
                                0.5+latent.prob.impact*
                                  (dat$Unobs.Motivation[r]>0)+
                                  intervention.impact*
                                  (dat$Received.Email[r]==1)))
  })
  dat$Retention<-sapply(1:n,function(r) {
    sample(c(0,1),size=1,
           prob=c(0.5-latent.prob.impact*
                    (dat$Unobs.Motivation[r]>0),
                  0.5+latent.prob.impact*
                    (dat$Unobs.Motivation[r]>0)))
  })
  return(dat)
}

###############################################################

## Double Selection

# set constants:
# n = number of observations
# N.Coeff = number of control coefficients to simulate data for
# B = causal impact of treatment on outcome
sim.double.selection.df<-function(n=10^3,N.Coeff=5*10^2,B=2) {
  set.seed(30)
  #mean of regression coefficients for control variables
  beta.C.mu<-25
  beta.C.sigma<-10
  #number of nonzero control coefficients
  beta.C.n.zero<-10
  #simulate control variable values as correlated variables
  C.mu<-rep(1,N.Coeff)
  C.var<-rnorm(N.Coeff,mean=0,sd=0.1)^2
  C.rho<-0.5
  C<-as.data.frame.matrix(genCorGen(n=n,nvars=N.Coeff,
                                    params1=C.mu,params2=C.var,
                                    dist='normal',rho=C.rho,
                                    corstr='ar1',
                                    wide='True'))[,-1]
  #simulate beta coefficients for control variables
  #and set portion of them to zero
  betaC<-rnorm(N.Coeff,mean=beta.C.mu,sd=beta.C.sigma)
  betaC[beta.C.n.zero:N.Coeff]<-0
  #generate treatment indicator and randomize
  Social.Proof.Variant<-rep(0,n)
  Social.Proof.Variant[0:(n/2)]<-1
  Social.Proof.Variant<-sample(Social.Proof.Variant)
  #simulate random noise
  e<-rnorm(n)
  #generate outcome variable
  Customer.Value<-B*Social.Proof.Variant+data.matrix(C)%*%betaC+e
  dat<-data.frame(Customer.Value,Social.Proof.Variant,C)
  return(dat)
}

###############################################################

## Causal Forests

# set constants:
# n = number of observations
# N.Coeff = number of control coefficients to simulate data for
# N.groups = number of groups want to estimate causal impact for
sim.causal.forest.df<-function(n=5*10^3,N.Coeff=5) {
  set.seed(30)
  N.groups<-4
  beta<-rep(c(1:N.groups),each=n/N.groups)*5
  var.group<-factor(beta)
  levels(var.group)<-c("Google","Instagram","Twitter","Bing")
  var.group<-relevel(var.group,ref="Google")
  C.mu<-rep(0,N.Coeff)
  C.rho<-0.5
  C.var<-rnorm(N.Coeff,mean=1,sd=1)^2
  beta.C.mu.sigma<-10
  C<-as.data.frame.matrix(genCorGen(n=n,nvars=N.Coeff,
                                    params1=C.mu,params2=C.var,
                                    dist='normal',rho=C.rho,
                                    corstr='ar1',
                                    wide='True'))[,-1]
  betaC<-rnorm(N.Coeff,mean=beta.C.mu.sigma,sd=beta.C.mu.sigma)
  Discount<-rep(0,n)
  Discount[0:(n/2)]<-1
  Discount<-sample(Discount)
  e<-rnorm(n)
  Revenue<-200+beta*Discount+data.matrix(C)%*%betaC+e
  dat<-data.frame(Revenue,Discount,C,
                  "Registration.Source"=var.group)
  return(dat)
}

###############################################################