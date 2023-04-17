# required libraries
library("readxl")
library("ggplot2")
library("patchwork")
library("woeBinning")
library("stats")
library("car")
library("ROCR")
library("lmtest")
library("corrplot")

# loading data
data <- read_excel('Home_Eq_Dataset.xlsx')
head(data)
df <- data
#df <- na.omit(data) # firstly I remove all rows, where is at least on NA

# saving variables as factors or numerics
df$BAD2 <- as.factor(df$BAD)
levels(df$BAD2) <- c("Good", "Bad")
df$REASON <- as.factor(df$REASON)
df$JOB <- as.factor(df$JOB)

df$YOJ <- as.double(df$YOJ)
df$VALUE <- as.integer(df$VALUE)
df$LOAN <- as.integer(df$LOAN)
df$CLAGE <- as.double(df$CLAGE)
df$DEBTINC <- as.double(df$DEBTINC)

df <- as.data.frame(df)
head(df)


# descriptive stats

ggplot(df, aes(BAD2)) +
  geom_bar(fill = "#4EB25A") +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,7000,500))  +
  scale_x_discrete(labels = c("Good","Bad"))+
  ggtitle("Count of Good and Bad")


# omit NA

ggplot(na.omit(df), aes(BAD2)) +
  geom_bar(fill = "#4EB25A") +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(breaks=seq(0,7000,500))  +
  scale_x_discrete(labels = c("Good","Bad"))+
  ggtitle("Count of Good and Bad - no NA")



# Binning, !save df as dataframe


binning <- woe.binning(df = df[,-1], 
                       target.var = "BAD2",
                       pred.var = df, 
                       min.perc.total = 0.05, 
                       stop.limit = 0.08, 
                       event.class = 'Bad', 
                       min.perc.class = 0.01)
woe.binning.table(binning)


df_binned <- woe.binning.deploy(df, binning, min.iv.total = 0.04) # recommended IV is 4%
df_binned <- df_binned[,-(2:14)]

# removed variables -> MORTDUE, REASON


# Univariate analysis

# DEBTINC

DEBTINC <- data.frame(woe.binning.table(binning)$`WOE Table for DEBTINC`[,c(1,2,8)])
ggplot(data = DEBTINC , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = DEBTINC , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_debtinc <- glm(BAD ~ DEBTINC.binned, family = 'binomial', data = df_binned)

score_debtinc <- predict(model_debtinc, type="response", newdata = df_binned)
pred_debtinc <- prediction(score_debtinc, df_binned$BAD)
perf_debtinc <- performance(pred_debtinc,"tpr","fpr")
plot(perf_debtinc)

auc_debtinc <- performance(pred_debtinc, measure = "auc")
auroc_debtinc <- auc_debtinc@y.values[[1]]
gini_debtinc <- (2*auroc_debtinc-1)
gini_debtinc

#-------------------------------------------------------------------------------------------------------------

# DELINQ

DELINQ <- data.frame(woe.binning.table(binning)$`WOE Table for DELINQ`[,c(1,2,8)])
ggplot(data = DELINQ , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = DELINQ , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_delinq <- glm(BAD ~ DELINQ.binned, family = 'binomial', data = df_binned)

score_delinq <- predict(model_delinq, type="response", newdata = df_binned)
pred_delinq <- prediction(score_delinq, df_binned$BAD)
perf_delinq <- performance(pred_delinq,"tpr","fpr")
plot(perf_delinq)

auc_delinq <- performance(pred_delinq, measure = "auc")
auroc_delinq <- auc_delinq@y.values[[1]]
gini_delinq <- (2*auroc_delinq-1)
gini_delinq


#-------------------------------------------------------------------------------------------------------------

# DEROG

DEROG <- data.frame(woe.binning.table(binning)$`WOE Table for DEROG`[,c(1,2,8)])
ggplot(data = DEROG , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = DEROG , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_derog <- glm(BAD ~ DEROG.binned, family = 'binomial', data = df_binned)

score_derog <- predict(model_derog, type="response", newdata = df_binned)
pred_derog <- prediction(score_derog, df_binned$BAD)
perf_derog <- performance(pred_derog,"tpr","fpr")
plot(perf_derog)

auc_derog <- performance(pred_derog, measure = "auc")
auroc_derog <- auc_derog@y.values[[1]]
gini_derog <- (2*auroc_derog-1)
gini_derog


#-------------------------------------------------------------------------------------------------------------

# CLAGE

CLAGE <- data.frame(woe.binning.table(binning)$`WOE Table for CLAGE`[,c(1,2,8)])
ggplot(data = CLAGE , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = CLAGE , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_clage <- glm(BAD ~ CLAGE.binned, family = 'binomial', data = df_binned)

score_clage <- predict(model_clage, type="response", newdata = df_binned)
pred_clage <- prediction(score_clage, df_binned$BAD)
perf_clage <- performance(pred_clage,"tpr","fpr")
plot(perf_clage)

auc_clage <- performance(pred_clage, measure = "auc")
auroc_clage <- auc_clage@y.values[[1]]
gini_clage <- (2*auroc_clage-1)
gini_clage


#-------------------------------------------------------------------------------------------------------------

# NINQ

NINQ <- data.frame(woe.binning.table(binning)$`WOE Table for NINQ`[,c(1,2,8)])
ggplot(data = NINQ , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = NINQ , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_ninq <- glm(BAD ~ NINQ.binned, family = 'binomial', data = df_binned)

score_ninq <- predict(model_ninq, type="response", newdata = df_binned)
pred_ninq <- prediction(score_ninq, df_binned$BAD)
perf_ninq <- performance(pred_ninq,"tpr","fpr")
plot(perf_ninq)

auc_ninq <- performance(pred_ninq, measure = "auc")
auroc_ninq <- auc_ninq@y.values[[1]]
gini_ninq <- (2*auroc_ninq-1)
gini_ninq


#-------------------------------------------------------------------------------------------------------------

# YOJ

YOJ <- data.frame(woe.binning.table(binning)$`WOE Table for YOJ`[,c(1,2,8)])
ggplot(data = YOJ , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = YOJ , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_yoj <- glm(BAD ~ YOJ.binned, family = 'binomial', data = df_binned)

score_yoj <- predict(model_yoj, type="response", newdata = df_binned)
pred_yoj <- prediction(score_yoj, df_binned$BAD)
perf_yoj <- performance(pred_yoj,"tpr","fpr")
plot(perf_yoj)

auc_yoj <- performance(pred_yoj, measure = "auc")
auroc_yoj <- auc_yoj@y.values[[1]]
gini_yoj <- (2*auroc_yoj-1)
gini_yoj



#-------------------------------------------------------------------------------------------------------------

# VALUE

VALUE <- data.frame(woe.binning.table(binning)$`WOE Table for VALUE`[,c(1,2,8)])
ggplot(data = VALUE , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = VALUE , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_value <- glm(BAD ~ VALUE.binned, family = 'binomial', data = df_binned)

score_value <- predict(model_value, type="response", newdata = df_binned)
pred_value <- prediction(score_value, df_binned$BAD)
perf_value <- performance(pred_value,"tpr","fpr")
plot(perf_value)

auc_value <- performance(pred_value, measure = "auc")
auroc_value <- auc_value@y.values[[1]]
gini_value <- (2*auroc_value-1)
gini_value


#-------------------------------------------------------------------------------------------------------------

# CLNO

CLNO <- data.frame(woe.binning.table(binning)$`WOE Table for CLNO`[,c(1,2,8)])
ggplot(data = CLNO , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = CLNO , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_clno <- glm(BAD ~ CLNO.binned, family = 'binomial', data = df_binned)

score_clno <- predict(model_clno, type="response", newdata = df_binned)
pred_clno <- prediction(score_clno, df_binned$BAD)
perf_clno <- performance(pred_clno,"tpr","fpr")
plot(perf_clno)

auc_clno <- performance(pred_clno, measure = "auc")
auroc_clno <- auc_clno@y.values[[1]]
gini_clno <- (2*auroc_clno-1)
gini_clno


#-------------------------------------------------------------------------------------------------------------

# JOB

JOB <- data.frame(woe.binning.table(binning)$`WOE Table for JOB`[,c(1,2,8)])
ggplot(data = JOB , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = JOB , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_job <- glm(BAD ~ JOB.binned, family = 'binomial', data = df_binned)

score_job <- predict(model_job, type="response", newdata = df_binned)
pred_job <- prediction(score_job, df_binned$BAD)
perf_job <- performance(pred_job,"tpr","fpr")
plot(perf_job)

auc_job <- performance(pred_job, measure = "auc")
auroc_job <- auc_job@y.values[[1]]
gini_job <- (2*auroc_job-1)
gini_job



#-------------------------------------------------------------------------------------------------------------

# LOAN

LOAN <- data.frame(woe.binning.table(binning)$`WOE Table for LOAN`[,c(1,2,8)])
ggplot(data = LOAN , aes(x=Final.Bin, y=Total.Count, color=Final.Bin, fill=Final.Bin)) + 
  geom_bar (stat = "identity")+
  coord_cartesian(ylim = c(0, 25721))+
  ggplot(data = LOAN , aes(x=Final.Bin, y=Bad.Rate,color=Final.Bin)) + 
  geom_point (stat = "identity",size=3)+
  coord_cartesian(ylim = c(0,20))

model_loan <- glm(BAD ~ LOAN.binned, family = 'binomial', data = df_binned)

score_loan <- predict(model_loan, type="response", newdata = df_binned)
pred_loan <- prediction(score_loan, df_binned$BAD)
perf_loan <- performance(pred_loan,"tpr","fpr")
plot(perf_loan)

auc_loan <- performance(pred_loan, measure = "auc")
auroc_loan <- auc_loan@y.values[[1]]
gini_loan <- (2*auroc_loan-1)
gini_loan



#-------------------------------------------------------------------------------------------------------------




# Correlation

dfx <- woe.binning.deploy(df, binning, min.iv.total = 0.04, add.woe.or.dum.var="woe")
dfx <- dfx[,c( 
               "woe.CLNO.binned",
               "woe.DEBTINC.binned",
               "woe.DELINQ.binned",
               "woe.DEROG.binned",
               "woe.JOB.binned",
               "woe.LOAN.binned",
               "woe.NINQ.binned",
               "woe.VALUE.binned",
               "woe.YOJ.binned"
)]

colnames(dfx) <- c('CLNO', 'DEBTINC', 'DELINQ', 'DEROG', 'JOB', 'LOAN', 'NINQ', 'VALUE', 'YOJ')
round(cor(dfx), 3)

corrplot(cor(dfx), method = "number")


# MODEL BUILDING


# Splitting data set into in-sample and out-of-sample
library('caret')

set.seed(999)

# split data into 70% training and 30% testing datasets
df_binned <- df_binned[,-10]
colnames(df_binned) <- c('BAD', 'DEBTINC', 'DELINQ', 'VALUE', 'DEROG', 'NINQ', 'LOAN', 'JOB', 'YOJ', 'CLNO')
i <- createDataPartition(df_binned$BAD, p = 0.7, list = FALSE)
train <- df_binned[i, ]
test <- df_binned[-i, ]


# model building - Logistic regression
model <- glm(BAD ~ DELINQ + DEROG  + DEBTINC + NINQ + YOJ + VALUE + CLNO + JOB + LOAN,
             family = 'binomial', data = train)
summary(model)

# YOJ removed
model <- glm(BAD ~ DELINQ + DEROG  + DEBTINC + NINQ  + VALUE + CLNO + JOB + LOAN,
             family = 'binomial', data = train)
summary(model)


# stepwise selection
model <- step(model, direction="both", trace=0)
summary(model)

# Multicolinearity test

vif(model)


# In-sample prediction
model_score_in <- predict(model, type="response", newdata = train) 
model_pred_in <- prediction(model_score_in, train$BAD)
model_perf_in <- performance(model_pred_in, "tpr","fpr")
plot(model_perf_in)

model_auc_in <- performance(model_pred_in, measure = "auc")
model_auc_in@y.values[[1]]
model_auroc_in <- round(model_auc_in@y.values[[1]]*100,2)
model_gini_in <- (2 * model_auroc_in - 100)
model_gini_in

# Out-of-sample prediction
model_score_out <- predict(model, type="response", newdata = test) 
model_pred_out <- prediction(model_score_out, test$BAD)
model_perf_out <- performance(model_pred_out,"tpr","fpr")
plot(model_perf_out)

model_auc_out <- performance(model_pred_out, measure = "auc")
model_auc_out@y.values[[1]]
model_auroc_out <- round(model_auc_out@y.values[[1]]*100,2)
model_gini_out <- (2 * model_auroc_out - 100)
model_gini_out

# ----------------------------------Alternative model - DECISION TREE--------------------------------------------
library('rpart')



model_tree <- rpart(BAD ~ DELINQ + DEROG  + DEBTINC + NINQ + YOJ + VALUE + CLNO + JOB + LOAN,
             method = 'class', data = train)
summary(model_tree)

model_tree <- rpart(BAD ~ DEBTINC + DELINQ +  VALUE  +   YOJ  +  CLNO +  DEROG  +  NINQ,
             method = 'class', data = train)
summary(model_tree)

# plot tree
library('rattle')
fancyRpartPlot(model_tree)
rpart.plot(model_tree)


# In-sample prediction
model_score_in_tree <- predict(model_tree, type="prob", newdata = train)[,2]
model_pred_in_tree <- prediction(model_score_in_tree, train$BAD)
model_perf_in_tree <- performance(model_pred_in_tree, "tpr","fpr")
plot(model_perf_in_tree)

model_auc_in_tree <- performance(model_pred_in_tree, measure = "auc")
model_auc_in_tree@y.values[[1]]
model_auroc_in_tree <- round(model_auc_in_tree@y.values[[1]]*100,2)
model_gini_in_tree <- (2 * model_auroc_in_tree - 100)
model_gini_in_tree

# Out-of-sample prediction
model_score_out_tree <- predict(model_tree, type="prob", newdata = test)[,2]
model_pred_out_tree <- prediction(model_score_out_tree, test$BAD)
model_perf_out_tree <- performance(model_pred_out_tree,"tpr","fpr")
plot(model_perf_out_tree)

model_auc_out_tree <- performance(model_pred_out_tree, measure = "auc")
model_auc_out_tree@y.values[[1]]
model_auroc_out_tree <- round(model_auc_out_tree@y.values[[1]]*100,2)
model_gini_out_tree <- (2 * model_auroc_out_tree - 100)
model_gini_out_tree





















