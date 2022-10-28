install.packages(c("caret","tidyverse", "party", "SuperLearner", "glmnet", "randomForest", "ggplot2", "RhpcBLASctl"))

library(SuperLearner)
library(party)
library(caret)
library(tidyverse)


# Full Mediation (CHAIN)Example EXERCISE 1A (can also use rbinom(n, size, prob))

Ua = rnorm(60000)
Ub = rnorm(60000)
Uc = rnorm(60000)

A = Ua
B = 0.5 * A + Ub 
C = 0.3 * B + Uc 
data = data.frame(cbind(A,B,C))

mod = lm(C ~ A + B, data=data)
summary(mod)

mod = lm(C ~ A, data=data)
summary(mod)

# Simulating the 'intervention' 
A1 = data.frame(cbind(C, rep(1, length(A))))
A0 = data.frame(cbind(C, rep(0, length(A))))
colnames(A1) <- c('C', 'A')
colnames(A0) <- c('C', 'A')

Cdo1 = predict(mod, newdata = A1)
Cdo0 = predict(mod, newdata = A0)

Cdiff = Cdo1 - Cdo0
mean(Cdiff)

# Confounding (fork) Example 1B

Uac = rnorm(60000)
Ubs = rnorm(60000)
Umh = rnorm(60000)

AC = Uac
BS = 0.4 * AC + Ubs 
MH = 0.2 * AC + Umh 
data = data.frame(cbind(AC, BS, MH))

mod = lm(MH ~ BS, data=data)
summary(mod)

mod = lm(MH ~ BS + AC, data=data)
summary(mod)


# Simulating the 'intervention' 
BS1 = data.frame(cbind(MH, rep(1, length(MH)), AC))
BS0 = data.frame(cbind(MH, rep(0, length(MH)), AC))
colnames(BS1) <- c('MH', 'BS', 'AC')
colnames(BS0) <- c('MH', 'BS', 'AC')

MHdo1 = predict(mod, newdata = BS1)
MHdo0 = predict(mod, newdata = BS0)

MHdiff = MHdo1 - MHdo0
mean(MHdiff)

# Collider Example 1C

Ugl = rnorm(60000)
Ush = rnorm(60000)
Uas = rnorm(60000)

GL = Ugl
AS = Uas
SH = 0.4 * GL + 0.3 * AS + Ush 
data = data.frame(cbind(GL, AS, SH))

mod = lm(AS ~ GL, data=data)
summary(mod)

mod = lm(AS ~ GL + SH, data=data)
summary(mod)

# Simulating the 'intervention' 
mod = lm(AS ~ GL, data=data)  # rerun to make sure we use correct model

GL1 = data.frame(cbind(AS, rep(1, length(AS))))
GL0 = data.frame(cbind(AS, rep(0, length(AS))))
colnames(GL1) <- c('AS', 'GL')
colnames(GL0) <- c('AS', 'GL')



ASdo1 = predict(mod, newdata = GL1)
ASdo0 = predict(mod, newdata = GL0)

ASdiff = ASdo1 - ASdo0
mean(ASdiff)


# SEM Example
library(lavaan)

Ua = rnorm(60000)
Ub = rnorm(60000)
Uc = rnorm(60000)
Ud = rnorm(60000)

A = Ua
B = 0.5*A + Ub
C = 0.3*A + Uc
D = 0.2*B + 0.5*C + Ud

data = data.frame(cbind(A,B,C,D))

mod = 'D ~ B + C
    B ~ A
    C ~ A'


s = sem(mod, data=data)
summary(s)



# Collider Example 1D

Ucs = rnorm(60000)
Ufs = rnorm(60000)
Uiq = rnorm(60000)
Ums = rnorm(60000)

CS = Ucs
IQ = Uiq
MS = 0.5 * IQ - 0.3 * CS + Ums
FS = 0.5 * IQ - 0.4 * CS + Ufs
data = data.frame(cbind(CS, IQ, MS, FS))

mod = lm(MS ~ CS, data=data)
summary(mod)

mod = lm(MS ~ CS + FS, data=data)
summary(mod)


# Simulating the 'intervention' 
mod = lm(MS ~ CS, data=data)  # rerun to make sure we use correct model

CS1 = data.frame(cbind(MS, rep(1, length(CS))))
CS0 = data.frame(cbind(MS, rep(0, length(CS))))
colnames(CS1) <- c('MS', 'CS')
colnames(CS0) <- c('MS', 'CS')

MSdo1 = predict(mod, newdata = CS1)
MSdo0 = predict(mod, newdata = CS0)

MSdiff = MSdo1 - MSdo0
mean(MSdiff)



# MEDIATION Example 1E

Ut = rnorm(60000)
Uy = rnorm(60000)
Ubp = rnorm(60000)


T = Ut
BP = 0.2 * T + Ubp
Y = -0.3*T + 0.4*BP

data = data.frame(cbind(T, BP, Y))

mod = lm(Y ~ T, data=data)  # total effect
summary(mod)

mod = lm(Y ~ T + BP, data=data)  # effect of BP
summary(mod)

mod = lm(BP ~ T , data=data)  # effect of T on BP (can be deduced anyway)
summary(mod)


# Simulating the 'interventions' for each effect 
mod = lm(Y ~ T, data=data)  # total effect

T1 = data.frame(cbind(Y, rep(1, length(Y))))
T0 = data.frame(cbind(Y, rep(0, length(Y))))
colnames(T1) <- c('Y', 'T')
colnames(T0) <- c('Y', 'T')

Ydo1 = predict(mod, newdata = T1)
Ydo0 = predict(mod, newdata = T0)

Ydiff = Ydo1 - Ydo0
mean(Ydiff)


mod = lm(Y ~ T + BP, data=data)  #  effect of BP on Y

BP1 = data.frame(cbind(Y, rep(1, length(Y)), T))
BP0 = data.frame(cbind(Y, rep(0, length(Y)), T))
colnames(BP1) <- c('Y', 'BP', 'T')
colnames(BP0) <- c('Y', 'BP', 'T')

Ydo1 = predict(mod, newdata = BP1)
Ydo0 = predict(mod, newdata = BP0)

Ydiff = Ydo1 - Ydo0
mean(Ydiff)



# CAUSAL DISCOVERY, EXAMPLE 3A

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("graph")
BiocManager::install("RBGL")
install.packages("causalDisco")
BiocManager::install("Rgraphviz")
library(graph)
library(RBGL)
library(pcalg)
library(causalDisco)
library(Rgraphviz)

x1 <- rnorm(1000)
x2 <- rnorm(1000)
x3 <- x1 + x2 + rnorm(1000)
d <- data.frame(x1, x2, x3)
cmat <- cor(d)
n = nrow(d)
v = colnames(d)

pc.fit <- pc(suffStat = list(C = cor(d), n = n),
             indepTest = gaussCItest,
             alpha=0.01, labels = v, verbose = TRUE)

plot(pc.fit, main = "Estimated CPDAG")



# CAUSAL DISCOVERY, EXAMPLE 3B

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("graph")
BiocManager::install("RBGL")
install.packages("causalDisco")
BiocManager::install("Rgraphviz")
library(graph)
library(RBGL)
library(pcalg)
library(causalDisco)
library(Rgraphviz)


x1 <- rnorm(1000)
x2 <- x1 +rnorm(1000)
x3 <- x2 + rnorm(1000)
d <- data.frame(x1, x2, x3)
cmat <- cor(d)

n = nrow(d)
v = colnames(d)

pc.fit <- pc(suffStat = list(C = cor(d), n = n),
             indepTest = gaussCItest,
             alpha=0.01, labels = v, verbose = TRUE)

plot(pc.fit, main = "Estimated CPDAG")



# CAUSAL DISCOVERY, EXAMPLE 3C

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("graph")
BiocManager::install("RBGL")
install.packages("causalDisco")
BiocManager::install("Rgraphviz")
library(graph)
library(RBGL)
library(pcalg)
library(causalDisco)
library(Rgraphviz)

u = rnorm(1000)
x1 <- u + rnorm(1000)
x2 <- u + rnorm(1000)
d <- data.frame(x1, x2)
cmat <- cor(d)

n = nrow(d)
v = colnames(d)

pc.fit <- pc(suffStat = list(C = cor(d), n = n),
             indepTest = gaussCItest,
             alpha=0.01, labels = v, verbose = TRUE)

plot(pc.fit, main = "Estimated CPDAG")





##### TRAIN TEST SPLITTING WITH RANDOM FOREST

Ucs = rnorm(1000)
Ufs = rnorm(1000)
Uiq = rnorm(1000)
Ums = rnorm(1000)

CS = Ucs
IQ = Uiq
FS = 0.5 * IQ - 0.4 * CS + Ufs
outcome = 0.5 * IQ - 0.3 * CS + Ums


data = data.frame(CS, IQ, FS, outcome)


intrain <- sample(nrow(data), round(0.6*nrow(data)))

all_train_split <- data[intrain,]
all_test_split <- data[-intrain,]

forest<- ranger(outcome ~ ., data = all_train_split)

preds_train <- predict(forest, newdata = all_train_split)
preds_test <- predict(forest, newdata = all_test_split)

mse_train <- mean((all_train_split$outcome - preds_train)^2)
mse_test <- mean((all_test_split$outcome - preds_test)^2)

mae_train <- mean(abs(all_train_split$outcome - preds_train))
mae_test <- mean(abs(all_test_split$outcome - preds_test))




##### TRAIN VALIDATION TEST SPLITTING WITH RANDOM FOREST
Ucs = rnorm(1000)
Ufs = rnorm(1000)
Uiq = rnorm(1000)
Ums = rnorm(1000)

CS = Ucs
IQ = Uiq
FS = 0.5 * IQ - 0.4 * CS + Ufs
outcome = 0.5 * IQ - 0.3 * CS + Ums

data = data.frame(CS, IQ, FS, outcome)


intrainval <- intrain <- sample(nrow(data), round(0.8*nrow(data))) 

all_trainval_split <- data[intrainval,]
all_test_split <- data[-intrainval,]

intrain <- intrain <-  sample(nrow(all_trainval_split), round(0.75*nrow(all_trainval_split)))

all_train_split <- all_trainval_split[intrain,]
all_val_split <- all_trainval_split[-intrain,]


rf <- cforest(outcome ~ ., data = all_train_split)
lr <- lm(outcome ~ ., data = all_train_split)
dt <- ctree(outcome ~ ., data = all_train_split)

rf_preds_train <- predict(rf, newdata = all_train_split)
lr_preds_train <- predict(lr, newdata = all_train_split)
dt_preds_train <- predict(dt, newdata = all_train_split)

rf_preds_val <- predict(rf, newdata = all_val_split)
lr_preds_val <- predict(lr, newdata = all_val_split)
dt_preds_val <- predict(dt, newdata = all_val_split)

rf_mse_train <- mean((all_train_split$outcome - rf_preds_train)^2)
rf_mse_val <- mean((all_val_split$outcome - rf_preds_val)^2)
lr_mse_train <- mean((all_train_split$outcome - lr_preds_train)^2)
lr_mse_val <- mean((all_val_split$outcome - lr_preds_val)^2)
dt_mse_train <- mean((all_train_split$outcome - dt_preds_train)^2)
dt_mse_val <- mean((all_val_split$outcome - dt_preds_val)^2)

lr_preds_test <- predict(lr, newdata = all_test_split)
lr_mse_test <- mean((all_test_split$outcome - lr_preds_test)^2)



##### K-FOLD CROSS VALIDATION 

Ucs = rnorm(1000)
Ufs = rnorm(1000)
Uiq = rnorm(1000)
Ums = rnorm(1000)

CS = Ucs
IQ = Uiq
FS = 0.5 * IQ - 0.4 * CS + Ufs
outcome = 0.5 * IQ - 0.3 * CS + Ums

data = data.frame(CS, IQ, FS, outcome)

k <- 5

data <- mutate(data, kfolds = sample(1:k,size = nrow(data), replace = TRUE))


cv_func <- function(current_fold, data){
  train <- data[data$kfolds != current_fold,][1:dim(data)[2]-1]
  test <- data[data$kfolds == current_fold,][1:dim(data)[2]-1]
  
  mod <- lm(outcome ~ ., data = train)
  
  tree_preds_test = data.frame(predict(mod, newdata = test))
  colnames(tree_preds_test) = 'pred'
  tree_preds_test$true = test$outcome
  return(tree_preds_test)
}

# boucle a travers les plis et collecte des predictions
results = data.frame()
for (current_fold in c(1,2,3,4,5)){
  result = cv_func(current_fold, data)
  results <- rbind(results, result)
}

mean((results$pred - results$true)^2)

# EXAMPLE 2D Simple Prediction with importances
library(randomForest)
N = 1000
Ux = rnorm(N)
Uy = rnorm(N)
Ui = rnorm(N)
Uc1 = rnorm(N)
Uc2 = rnorm(N)
Uc3 = rnorm(N)
Uc4 = rnorm(N)
Uc5 = rnorm(N)
Um = rnorm(N)
Ur1 = rnorm(N)
Ur2 = rnorm(N)
Ud = rnorm(N)

C1 = Uc1
C2 = Uc2
C3 = Uc3
C5 = 0.7 * C2 + Uc5
I = Ui
R1 = Ur1
R2 = Ur2
X = 0.2*I + 0.1*C1 +0.2*C2 + Ux
M = 0.6 * X + 0.2 * C1 + 0.2 * C3 + Um
Y = 0.1 * R1 + 0.4 * R2 + 0.1 * C3 + 0.8 * M +0.4*X + 0.2*C5 + Uy
C4 = 0.4 * X + 0.3 * Y + Uc4
D = 0.4 * Y + Uy

data = data.frame(C1, C2, C3, C5, I, R1, R2, X, M, Y, C4, D)

intrain <- sample(nrow(data), round(0.6*nrow(data)))

all_train_split <- data[intrain,]
all_test_split <- data[-intrain,]

forest<- randomForest(Y ~ ., data = all_train_split, mtry = 3,
                      importance = TRUE, na.action = na.omit)

preds_test <- predict(forest, newdata = all_test_split)

mse_test <- mean((all_test_split$Y - preds_test)^2)
mse_baseline <- mean((mean(all_test_split$Y) - preds_test)^2)

print(forest)
varImpPlot(forest)



# EXAMPLE 2E Plugin estimation with RF and importances
library(randomForest)
N = 15000
Ux = rnorm(N)
Uy = rnorm(N)
Ui = rnorm(N)
Uc1 = rnorm(N)
Uc2 = rnorm(N)
Uc3 = rnorm(N)
Uc4 = rnorm(N)
Uc5 = rnorm(N)
Um = rnorm(N)
Ur1 = rnorm(N)
Ur2 = rnorm(N)
Ud = rnorm(N)

C1 = Uc1
C2 = Uc2
C3 = Uc3
C5 = 0.7 * C2 + Uc5
I = Ui
R1 = Ur1
R2 = Ur2
X = 0.2*I + 0.1*C1 +0.2*C2 + Ux
M = 0.6 * X + 0.2 * C1 + 0.2 * C3 + Um
Y = 0.1 * R1 + 0.4 * R2 + 0.1 * C3 + 0.8 * M +0.4*X + 0.2*C5 + Uy
C4 = 0.4 * X + 0.3 * Y + Uc4
D = 0.4 * Y + Uy

data = data.frame(C1, C2, C3, C5, I, R1, R2, X, M, Y, C4, D)

intrain <- sample(nrow(data), round(0.8*nrow(data)))

all_train_split <- data[intrain,]
all_test_split <- data[-intrain,]

forest<- randomForest(Y ~ R1+R2+X+C1+C5, data = all_train_split, mtry = 3,
                      importance = TRUE, na.action = na.omit)

preds_test <- predict(forest, newdata = all_test_split)

mse_test <- mean((all_test_split$Y - preds_test)^2)
mse_baseline <- mean((mean(all_test_split$Y) - preds_test)^2)

print(forest)
varImpPlot(forest)

data_do1 = data
data_do0 = data
data_do1$X = 1
data_do0$X = 0

preds_do1 <- predict(forest, newdata = data_do1)
preds_do0 <- predict(forest, newdata = data_do0)

diff_Y = mean(preds_do1 - preds_do0)
diff_Y

lin_mod = lm(Y~R1+R1+X+C1+C5, data=data)
summary(lin_mod)



##### SUPER LEARNING Example 2F 
library(SuperLearner)
library(polspline)
N = 2000
Ux = rnorm(N)
Uy = rnorm(N)
Ui = rnorm(N)
Uc1 = rnorm(N)
Uc2 = rnorm(N)
Uc3 = rnorm(N)
Uc4 = rnorm(N)
Uc5 = rnorm(N)
Um = rnorm(N)
Ur1 = rnorm(N)
Ur2 = rnorm(N)
Ud = rnorm(N)

C1 = Uc1
C2 = Uc2
C3 = Uc3
C5 = 0.7 * C2 + Uc5
I = Ui
R1 = Ur1
R2 = Ur2
X = 0.2*I + 0.1*C1 +0.2*C2 + Ux
M = 0.6 * X + 0.2 * C1 + 0.2 * C3 + Um
Y = 0.1 * R1 + 0.4 * R2 + 0.1 * C3 + 0.8 * M +0.4*X + 0.2*C5 + Uy
C4 = 0.4 * X + 0.3 * Y + Uc4
D = 0.4 * Y + Uy

predictors = data.frame(cbind(X, C1, C5, R1, R2))
outcome = Y

listWrappers()

cv_sl = CV.SuperLearner(Y = outcome, X = predictors, family = gaussian(),
                        # For a real analysis we would use V = 10.
                        cvControl = list(V = 10), innerCvControl = list(list(V=6)),
                        SL.library = c("SL.mean", "SL.lm", "SL.randomForest", "SL.polymars"))

summary(cv_sl)
c = coef(cv_sl)
av_c = colMeans(c)

plot( 
  Y[ cv_sl$folds[[1]] ],
  predict( cv_sl$AllSL[[1]] )$pred,
  xlab = "Actual", ylab = "Predicted",
  main = "First fold"
)  

preds =  data.frame()
gts = data.frame()
for (k in c(1:10)){

  pred = predict( cv_sl$AllSL[[k]] )$pred
  preds = rbind(preds, pred)
  
  gt = Y[ cv_sl$folds[[k]]]
  gts = rbind(gts, gt)
}

lm_mod = SuperLearner(Y = outcome, X = predictors, family = gaussian(), SL.library = "SL.lm")
poly_mod = SuperLearner(Y = outcome, X = predictors, family = gaussian(), SL.library = "SL.polymars")

predictors_do1 = predictors
predictors_do0 = predictors
predictors_do1$X = 1
predictors_do0$X = 0

preds_lm_do1 = predict(lm_mod, predictors_do1  )$pred
preds_poly_do1 = predict(poly_mod, predictors_do1 )$pred
preds_lm_do0 = predict(lm_mod, predictors_do0  )$pred
preds_poly_do0 = predict(poly_mod, predictors_do0 )$pred


combined_do1 = 0.9673 * preds_lm_do1 + 0.03268 * preds_poly_do1
combined_do0 = 0.9673 * preds_lm_do0 + 0.03268 * preds_poly_do0

diff_Y = mean(combined_do1 - combined_do0)
diff_Y


# NONLINEAR DGP EXAMPLE

dgp <- function(N){
  Ux = rnorm(N)
  Uy = rnorm(N)
  Uc = rnorm(N)
  
  C = Uc
  X = Ux + 0.6 * C
  Y = 0.2*X + 0.5 * X^2 + 0.2*X^3 + 0.3 * C + Uy
  
  Y1 = 0.2 + 0.5 + 0.2 + 0.3*C + Uy
  Y0 = 0.3*C + Uy
  ATE = mean(Y1-Y0)
  
  data = data.frame(C,X,Y)
  
  return(list(data, ATE))
}

q = dgp(1000000)
q[2]


##### SUPER LEARNING WITH TARGETED LEARNING AND NONLINEAR DGP Example 2G
# https://tlverse.org/tlverse-handbook/setup.html 
devtools::install_github("tlverse/tlverse")
devtools::install_github("tlverse/sl3@devel")
install.packages("Rsolnp")
install.packages("ranger")
library(tmle3)
library(sl3)
library(data.table)


processed <- process_missing(data, node_list)
data <- processed$data
node_list <- processed$node_list

sl3_list_learners(properties = "continuous")
sl3_list_learners(properties = "binomial")
# choose base learners
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_rf <- make_learner(Lrnr_ranger)

# define metalearners appropriate to data types
ls_metalearner <- make_learner(Lrnr_nnls)
bn_metalearner <- make_learner(Lrnr_solnp)

Q <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_rf),
  metalearner = ls_metalearner
)
G <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_rf),
  metalearner = bn_metalearner
)
learner_list <- list(A = G, Y = Q)



generateData <- function(n){
  # adapted from https://migariane.github.io/TMLE.nb.html
  w1 <- rbinom(n, size=1, prob=0.5)
  w2 <- rbinom(n, size=1, prob=0.65)
  w3 <- round(runif(n, min=0, max=4), digits=3)
  w4 <- round(runif(n, min=0, max=5), digits=3)
  A  <- rbinom(n, size=1, prob= plogis(-0.4 + 0.2*w2 + 0.15*w3 + 0.2*w4 + 0.15*w2*w4))
  # counterfactual
  Y.1 <- -1 + 1 -0.1*w1 + 0.3*w2 + 0.25*w3 + 0.2*w4 + 0.15*w2*w4
  Y.0 <- -1 + 0 -0.1*w1 + 0.3*w2 + 0.25*w3 + 0.2*w4 + 0.15*w2*w4
  # Observed outcome
  Y <- Y.1*A + Y.0*(1 - A)
  # return data.frame
  data.frame(w1, w2, w3, w4, A, Y, Y.1, Y.0)
}


data <- generateData(n=100000)
True_Psi <- mean(data$Y.1-data$Y.0);

data <- generateData(n=1000)

node_list <- list(
  W = c(
    "w1", "w2","w3","w4"
  ),
  A = "A",
  Y = "Y"
)

mod = lm(Y~ A+w1+w2+w3+w4, data=data)
summary(mod)

ate_spec <- tmle_ATE(
  treatment_level = 1,
  control_level = 0
)

tmle_fit <- tmle3(ate_spec, data, node_list, learner_list)
print(tmle_fit)
estimates <- tmle_fit$summary$psi_transformed
estimates


