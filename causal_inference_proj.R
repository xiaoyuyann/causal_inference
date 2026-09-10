---
  title: "Rhyme Project Causal Inference - Filled In Notebook"
output: 
  html_document:
  toc: true
number_sections: false
code_folding: hide
theme: cosmo
date: "`r format(Sys.time(), '%d %B, %Y')`"
editor_options: 
  chunk_output_type: console
---
  
  ## Load Necessary Packages & Helper Functions
  
  ```{r setup, include=FALSE}
setwd("~/CausalInference/causal_inference")
source('causal_function.R')
print("Setup Complete")
```

## Controlled / Fixed Effects Regression

```{r}
# function to simulate data set
dat<-sim.fixed.effects.df()

# explore data
colnames(dat)

head(dat)
```

```{r}
# customer Spend vs. satisfaction
g1<-dat %>%
  ggplot(aes(Customer.Rating,Customer.Spend,fill="A")) +
  geom_point() +
  theme_economist() +
  scale_fill_economist()

g1
cor(dat$Customer.Rating,dat$Customer.Spend)
```

```{r}
# customer Spend vs. time
g2<-dat %>%
  ggplot(aes(Time.FE,Customer.Spend,fill="A")) +
  geom_boxplot() +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position="none",axis.text.x=element_text(angle=45,
                                                        vjust=0.5))

# customer spend vs. product
g3<-dat %>%
  ggplot(aes(Product.FE,Customer.Spend,fill="A")) +
  geom_boxplot() +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position="none",axis.text.x=element_text(angle=45,
                                                        vjust=0.5))

# Customer spend vs. customer Age
g4<-dat %>%
  ggplot(aes(Customer.Age,Customer.Spend,fill="A")) +
  geom_point() +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position="none")

# Customer spend vs. total purchases
g5<-dat %>%
  ggplot(aes(Total.Purchases,Customer.Spend,fill="A")) +
  geom_point() +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position="none")

# combind all plots
grid.arrange(g2,g3,g4,g5,nrow=2)

# observe differences across time & products & customer age
# need to include in regression to control for their effects
```

```{r warning=F}
# compare controlled reg/fixed effect models

# naive regression; no controls
model1<-lm(Customer.Spend~Customer.Rating,data=dat)

# control for customer age only
model2<-lm(Customer.Spend~Customer.Rating+Customer.Age,data=dat)

# control for product and time fixed effects only
model3<-lm(Customer.Spend~Customer.Rating+Product.FE+Time.FE,data=dat)

# full controls; and included variable bias of total purchases
model4<-lm(Customer.Spend~Customer.Rating+Customer.Age+Product.FE+Time.FE+Total.Purchases,data=dat)

# full controls; no included variable bias
model5<-lm(Customer.Spend~Customer.Rating+Customer.Age+Product.FE+Time.FE,data=dat)
```

```{r}
# see here for more info on stargazer:
# (1) https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
# (2) https://www.jakeruss.com/cheatsheets/stargazer/

# view coefficient of interest in each regression model
stargazer(model1,model2,model3,model4,model5,type="text",
          style="aer",omit=c("Constant","Customer.Age",
                             "Product.FE","Time.FE",
                             "Total.Purchases"),
          column.labels=c("Y~X","Y~X+C","Y~X+FE","Y~X+C+FE+IVB",
                          "Y~X+C+FE"),
          dep.var.labels="Controlled / Fixed Effects Regression",
          omit.stat=c("f","ser","rsq","n"),
          notes=c("True Coef on X = 2"),
          add.lines=list(c("Add. Controls","No","Yes","No",
                           "Yes","Yes"),
                         c("Fixed effects","No","No","Yes",
                           "Yes","Yes"),
                         c("Included Variable Bias","No","No","No",
                           "Yes","No")))
```

## Regression Discontinuity

```{r}
# function to simulate data set
dat<-sim.reg.discontinuity.df()

# explore data
colnames(dat)

head(dat)
```

```{r}
# regression discontinuity plot
dat %>%
  ggplot(aes(Lead.Score,Customer.Spend,color=Add.Support)) +
  geom_line(lwd=2) +
  geom_line(aes(Lead.Score,Counterfactual),lty=2,lwd=2) +
  geom_vline(xintercept=dat$Lead.Score[sum(!dat$Add.Support)],
             lty=2,lwd=2) +
  xlab("Lead Score") +
  ylab("Customer Spend") +
  theme_economist() +
  scale_color_economist() +
  theme(legend.position="none")
```

```{r warning=F}
# fit regression discontinuity model
model1<-lm(Customer.Spend~Lead.Score+I(Lead.Score>=70)+Lead.Score:I(Lead.Score>=70),data=dat)
```

```{r}
# view regression discontinuity model
stargazer(model1,type="text",style="aer",
          column.labels=c("Y~X+I(X>Cutoff)+X*I(X>Cutoff)"),
          dep.var.labels="Regression Discontinuity",
          omit.stat=c("f","ser","rsq","n","adj.rsq"),
          intercept.bottom=F)

# causal impact is difference in regression lines at cutoff 
# I(X>Cutoff)+X*I(X>Cutoff)
coef(model1)["I(Lead.Score >= 70)TRUE"]+coef(model1)["Lead.Score:I(Lead.Score >= 70)TRUE"]*70
```

## Difference in Difference


```{r}
# function to simulate data set
dat<-sim.diff.in.diff.df()

# explore data
colnames(dat)

head(dat)
```

```{r}
# difference in difference plot
dat %>%
  ggplot(aes(Time,Revenue,color=Country)) +
  geom_line(lwd=2) +
  geom_line(aes(Time,Counterfactual),lty=2,lwd=2) +
  xlab("Time") +
  ylab("Revenue") +
  theme_economist() +
  scale_color_economist()
```

```{r warning=F}
# fit difference in difference model
model1<-lm(Revenue~Period+Country+Period:Country,data=dat)

# Note what the estimated revenue from the model is in each scenario:
# Rev in US Pre Price change = Intercept (Period, Country, Interaction all 0)
# Rev in AU Pre Price change = Intercept + Country (Period, Interaction all 0)
# Rev in US Post Price change = Intercept + Period (Country, Interaction all 0)
# Rev in AU Post Price change = Intercept + Period + Country + Interaction
# Diff in Diff = (Rev in AU Post Price change - Rev in AU Pre Price change) - (Rev in US Post Price change - Rev in US Pre Price change)
#              = (Intercept + Period + Country + Interaction - Intercept + Country) - (Intercept + Period - Intercept)
#              = Interaction
```

```{r}
# view difference in difference model
stargazer(model1,type="text",style="aer",
          column.labels=c("Y~Post+G+Post*G"),
          dep.var.labels="Difference in Difference",
          omit.stat=c("f","ser","rsq","n","adj.rsq"),
          notes=c("Causal Impact = 100"),intercept.bottom=F)
```

## Instrumental Variable

```{r warning=F}
# function to simulate data set
dat<-sim.iv.df()

# explore data
colnames(dat)

head(dat)

#users who use the mobile app have higher motivation; those who retain also have higher retention
#this biases a naive regression
tapply(dat$Unobs.Motivation,dat$Use.Mobile.App,mean)
tapply(dat$Unobs.Motivation,dat$Retention,mean)
```

```{r warning=F}
# fit IV model

# naive regression
model1<-lm(Retention~Use.Mobile.App,data=dat)

# first stage regression
model2<-lm(Use.Mobile.App~Received.Email,data=dat)

# second stage regression
model3<-lm(Retention~predict(model2),data=dat)

# two stage least squares for IV
model4<-ivreg(Retention~Use.Mobile.App|Received.Email,data=dat)

```

```{r}
# compare all models
stargazer(model1,model2,model3,model4,type="text",style="aer",
          column.labels=c("Y~X","X~Z","Y~Xhat","IV"),
          omit=c("Constant"),
          dep.var.labels=c("Retention","Use.Mobile.App",
                           "Retention","Retention"),
          covariate.labels=c("Use.Mobile.App","Received.Email",
                             "Use.Mobile.App.Hat"),
          model.names=F,omit.stat=c("ser","rsq","n","adj.rsq"),
          intercept.bottom=F)
```

## Double Selection

```{r warning=F}
# function to simulate data set
dat<-sim.double.selection.df()

# explore data
dim(dat)

colnames(dat[,1:10])

head(dat[,1:10])
```

```{r warning=F}
# fit double selection model

# isolate control variables
C<-dat[,-which(colnames(dat)%in%c("Customer.Value","Social.Proof.Variant"))]
C<-as.matrix(C)

# fit lasso regressing outcome on control variables
y.glmnet.model<-cv.glmnet(C,dat$Customer.Value)

# extract nonzero coefficients from lasso above
# use lambda min CV within 1 se (select less coeff)
predict(y.glmnet.model,s="lambda.1se",type="nonzero")
nonzero<-unlist(predict(y.glmnet.model,s="lambda.1se",type="nonzero"))
Y.on.C<-colnames(C)[nonzero]
Y.on.C

# fit lasso regressing treatment on control variables
x.glmnet.model<-cv.glmnet(C,dat$Social.Proof.Variant)

# extract nonzero coefficients from lasso above
# use lambda min CV within 1 se (select less coeff)
predict(x.glmnet.model,s="lambda.1se",type="nonzero")
nonzero<-unlist(predict(x.glmnet.model,s="lambda.1se",type="nonzero"))
X.on.C<-colnames(C)[nonzero]
X.on.C

# combine two sets of nonzero coefficients to get unique nonzero 
# coefficients across models
var.union<-unique(c(Y.on.C,X.on.C))

# count number of nonzero variables
length(var.union)
var.union

# use all nonzero coefficients + treatment indicator
# in double selection regression
double.selection<-lm(Customer.Value~.,dat[,c("Customer.Value","Social.Proof.Variant",var.union)])
```

```{r}
# compare naive model, full model, and double selection

# naive regression
naive.regression<-lm(Customer.Value~Social.Proof.Variant,data=dat)

# regression with full controls
full.model<-lm(Customer.Value~.,data=dat)
```

```{r}
# compare all models
stargazer(naive.regression,full.model,double.selection,type="text",style="aer",
          column.labels=c("No Controls","All Controls",
                          "Double Selection"),
          dep.var.labels=c(""),
          covariate.labels=c("Social.Proof.Variant"),
          omit=c("V[0-9]","Constant"),
          model.names=F,omit.stat=c("ser","rsq","n","adj.rsq","F"),
          notes=c("Causal Impact = 2"))
```

## Causal Forests

```{r}
# function to simulate data set
dat<-sim.causal.forest.df()

# explore data
colnames(dat)

head(dat)

table(dat$Registration.Source)
```

```{r warning=F}
# regular OLS
model1<-lm(Revenue~.,data=dat[,-which(colnames(dat)=="Registration.Source")])

# OLS with interactions for heterogeneous treatments by source
model2<-lm(Revenue~.+Discount*Registration.Source,data=dat)
```

```{r}
# compare OLS models
stargazer(model1,model2,type="text",style="aer",
          column.labels=c("All Controls",
                          "All Controls + Group Interactions"),
          dep.var.labels=c("","",""),
          covariate.labels=c("Discount",
                             "Discount x Instagram",
                             "Discount x Twitter",
                             "Discount x Bing"),
          omit=c("V","Constant","^Registration.Source"),
          model.names=F,omit.stat=c("ser","rsq","n","adj.rsq"))
```

```{r warning=F}
# fit causal forest
X<-model.matrix(~.,data=dat[,-which(colnames(dat)%in%c("Revenue","Discount"))])
cf<-causal_forest(X=X,Y=dat$Revenue,W=dat$Discount)
```

```{r}
# obtain causal forest model predictions
pred<-predict(cf)$predictions

# view average causal forest model predictions by source
# is estimate of heterogeneous treatment effect
tapply(pred,dat$Registration.Source,mean)

# extract variable importance from causal forest model
cf %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable=colnames(X)) %>% 
  arrange(desc(V1))

# plot distribution of causal forest model predictions by sources
data.frame("est"=pred,"Registration.Source"=dat$Registration.Source) %>%
  ggplot(aes(Registration.Source,pred,fill=Registration.Source)) +
  geom_boxplot() +
  xlab("Registration Source") +
  ylab("Estimated Treatment") +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position="none")
```
