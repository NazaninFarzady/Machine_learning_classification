args = commandArgs(trailingOnly = TRUE)

install.packages("data.table", repos = "http://cran.us.r-project.org")
install.packages("caTools",repos = "http://cran.us.r-project.org")
install.packages("caret",repos = "http://cran.us.r-project.org")
install.packages("pROC",repos = "http://cran.us.r-project.org")
install.packages("class",repos = "http://cran.us.r-project.org")
install.packages("randomForest",repos = "http://cran.us.r-project.org")
install.packages("tictoc",repos = "http://cran.us.r-project.org")
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
install.packages("e1071",repos = "http://cran.us.r-project.org")

library("data.table");library("caTools");library("caret");library("pROC");library("class");
library("randomForest");library("tictoc");library("tidyverse");library("e1071")

dir.create(args[1])
sink(paste0(args[1],"/log_lr.txt"),append = T)

tic()

print("preprocessing the data")
dat <- list.files(args[2], full.names = TRUE, recursive = TRUE) %>%
  map_df(read_csv) %>%
  mutate(classifier = if_else(sleep_state >= 1, 1, 0)) %>%
  select(activity_index,br,hr,time_stamp,classifier)
dat$clock_proxy <- cos(dat$time_stamp %% (24*60*60)/(60*60) * 15 * pi/180)
dat <- na.omit(dat)
dat$classifier <-  factor(dat$classifier)
print("printing structure of data")
str(dat)
print("printing summary of data")
summary(dat)


######### Parameter combination 1: hr ###########

tic()
print("starting parameter combination 1: Heart_rate")
parameter_comb <- "hr";para_num = 1
df_lr <- select(dat, hr, classifier)

print("normalizing and splitting the data")
final.df_lr <- scale(df_lr[,1:length(df_lr)-1]) %>% cbind(df_lr[length(df_lr)]) 
set.seed(101) 
split = sample.split(final.df_lr$classifier, SplitRatio = 0.75)
df_lr.train = subset(final.df_lr, split == TRUE) 
df_lr.test = subset(final.df_lr, split == FALSE) 

print("modelling with linear regression of parameter combination 1")
model_lr <- glm(formula=classifier ~ ., family = binomial(link='logit'), data = df_lr.train) # train model
if(para_num == 4){saveRDS(model_lr, paste0(args[1],"/",parameter_comb,"_model_lr.rds"));print("saving rds_lr")}
pred_lr <- predict(model_lr, newdata = df_lr.test, type = "response") # To predict our log regression model on our testing dataset
pred.results <- ifelse(pred_lr > 0.5, 1, 0)# to calculate from the predicted values
accuracy_lr_hr <- (1 - mean(pred.results != df_lr.test$classifier)) # check accuracy
y_pred <- factor(pred.results, levels=c(0, 1)) # re factor if not error may error
Lr <- confusionMatrix(y_pred, df_lr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_lr_hr  <- roc(df_lr.test$classifier, pred_lr)
print("getting auc results")
auc_lr_hr <- auc(df_lr.test$classifier, pred_lr) # Logistic Regression
print("generating sensitivity and specificity")
Lr_hr <-as.data.frame(Lr$byClass) #Logistic Regression
sensitiviy_lr_hr <- Lr_hr["Sensitivity",]
specificity_lr_hr <- Lr_hr["Specificity",]
toc()

######### Parameter combination 2: hr+ai ###########

tic()
print("starting parameter combination 2: Activity_index & Heart_rate")
parameter_comb <- "ai_hr";para_num = 2
df_lr <- select(dat, activity_index, hr, classifier)

print("normalizing and splitting the data")
final.df_lr <- scale(df_lr[,1:length(df_lr)-1]) %>% cbind(df_lr[length(df_lr)]) 
set.seed(101) 
split = sample.split(final.df_lr$classifier, SplitRatio = 0.75)
df_lr.train = subset(final.df_lr, split == TRUE) 
df_lr.test = subset(final.df_lr, split == FALSE) 

print("modelling with linear regression of parameter combination 2")
model_lr <- glm(formula=classifier ~ ., family = binomial(link='logit'), data = df_lr.train) # train model
if(para_num == 4){saveRDS(model_lr, paste0(args[1],"/",parameter_comb,"_model_lr.rds"));print("saving rds_lr")}
pred_lr <- predict(model_lr, newdata = df_lr.test, type = "response") # To predict our log regression model on our testing dataset
pred.results <- ifelse(pred_lr > 0.5, 1, 0)# to calculate from the predicted values
accuracy_lr_hr_ai <- (1 - mean(pred.results != df_lr.test$classifier)) # check accuracy
y_pred <- factor(pred.results, levels=c(0, 1)) # re factor if not error may error
Lr <- confusionMatrix(y_pred, df_lr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_lr_hr_ai  <- roc(df_lr.test$classifier, pred_lr)
print("getting auc results")
auc_lr_hr_ai <- auc(df_lr.test$classifier, pred_lr) # Logistic Regression
print("generating sensitivity and specificity")
Lr_hr_ai <-as.data.frame(Lr$byClass) #Logistic Regression
sensitiviy_lr_hr_ai <- Lr_hr_ai["Sensitivity",]
specificity_lr_hr_ai <- Lr_hr_ai["Specificity",]

toc()

######### Parameter combination 3: hr+ai+cp ###########

tic()
print("starting parameter combination 3: Activity_index & Heart_rate & clock_proxy")
parameter_comb <- "ai_hr_cp";para_num = 3
df_lr <- select(dat, activity_index, hr, clock_proxy, classifier)

print("normalizing and splitting the data")
final.df_lr <- scale(df_lr[,1:length(df_lr)-1]) %>% cbind(df_lr[length(df_lr)]) 
set.seed(101) 
split = sample.split(final.df_lr$classifier, SplitRatio = 0.75)
df_lr.train = subset(final.df_lr, split == TRUE) 
df_lr.test = subset(final.df_lr, split == FALSE) 

print("modelling with linear regression of parameter combination 3")
model_lr <- glm(formula=classifier ~ ., family = binomial(link='logit'), data = df_lr.train) # train model
if(para_num == 4){saveRDS(model_lr, paste0(args[1],"/",parameter_comb,"_model_lr.rds"));print("saving rds_lr")}
pred_lr <- predict(model_lr, newdata = df_lr.test, type = "response") # To predict our log regression model on our testing dataset
pred.results <- ifelse(pred_lr > 0.5, 1, 0)# to calculate from the predicted values
accuracy_lr_hr_ai_cp <- (1 - mean(pred.results != df_lr.test$classifier)) # check accuracy
y_pred <- factor(pred.results, levels=c(0, 1)) # re factor if not error may error
Lr <- confusionMatrix(y_pred, df_lr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_lr_hr_ai_cp  <- roc(df_lr.test$classifier, pred_lr)
print("getting auc results")
auc_lr_hr_ai_cp <- auc(df_lr.test$classifier, pred_lr) # Logistic Regression
print("generating sensitivity and specificity")
Lr_hr_ai_cp <-as.data.frame(Lr$byClass) #Logistic Regression
sensitiviy_lr_hr_ai_cp <- Lr_hr_ai_cp["Sensitivity",]
specificity_lr_hr_ai_cp <- Lr_hr_ai_cp["Specificity",]

toc()

######### Parameter combination 4: hr_ai_br ###########

tic()
print("starting parameter combination 4: Activity_index & Heart_rate & Breathing_rate")
parameter_comb <- "ai_hr_br";para_num = 3
df_lr <- select(dat, activity_index, hr, br, classifier)
print("normalizing and splitting the data")
final.df_lr <- scale(df_lr[,1:length(df_lr)-1]) %>% cbind(df_lr[length(df_lr)]) 
set.seed(101) 
split = sample.split(final.df_lr$classifier, SplitRatio = 0.75)
df_lr.train = subset(final.df_lr, split == TRUE) 
df_lr.test = subset(final.df_lr, split == FALSE) 

print("modelling with linear regression of parameter combination 4")
model_lr <- glm(formula=classifier ~ ., family = binomial(link='logit'), data = df_lr.train) # train model
if(para_num == 4){saveRDS(model_lr, paste0(args[1],"/",parameter_comb,"_model_lr.rds"));print("saving rds_lr")}
pred_lr <- predict(model_lr, newdata = df_lr.test, type = "response") # To predict our log regression model on our testing dataset
pred.results <- ifelse(pred_lr > 0.5, 1, 0)# to calculate from the predicted values
accuracy_lr_hr_ai_br <- (1 - mean(pred.results != df_lr.test$classifier)) # check accuracy
y_pred <- factor(pred.results, levels=c(0, 1)) # re factor if not error may error
Lr <- confusionMatrix(y_pred, df_lr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_lr_hr_ai_br  <- roc(df_lr.test$classifier, pred_lr)
print("getting auc results")
auc_lr_hr_ai_br <- auc(df_lr.test$classifier, pred_lr) # Logistic Regression
print("generating sensitivity and specificity")
Lr_hr_ai_br <-as.data.frame(Lr$byClass) #Logistic Regression
sensitiviy_lr_hr_ai_br <- Lr_hr_ai_br["Sensitivity",]
specificity_lr_hr_ai_br <- Lr_hr_ai_br["Specificity",]
toc()

######### Parameter combination 5: hr_ai_br_cp ###########


tic()
print("starting parameter combination 5: Activity_index & Heart_rate & clock_proxy & Breathing_rate")
parameter_comb <- "ai_hr_cp_br";para_num = 4
df_lr <- select(dat, activity_index, hr, clock_proxy, br, classifier)
print("normalizing and splitting the data")
final.df_lr <- scale(df_lr[,1:length(df_lr)-1]) %>% cbind(df_lr[length(df_lr)]) 
set.seed(101) 
split = sample.split(final.df_lr$classifier, SplitRatio = 0.75)
df_lr.train = subset(final.df_lr, split == TRUE) 
df_lr.test = subset(final.df_lr, split == FALSE) 

print("modelling with linear regression of parameter combination 5")
model_lr <- glm(formula=classifier ~ ., family = binomial(link='logit'), data = df_lr.train) # train model
if(para_num == 4){saveRDS(model_lr, paste0(args[1],"/",parameter_comb,"_model_lr.rds"));print("saving rds_lr")}
pred_lr <- predict(model_lr, newdata = df_lr.test, type = "response") # To predict our log regression model on our testing dataset
pred.results <- ifelse(pred_lr > 0.5, 1, 0)# to calculate from the predicted values
accuracy_lr_hr_ai_cp_br <- (1 - mean(pred.results != df_lr.test$classifier)) # check accuracy
y_pred <- factor(pred.results, levels=c(0, 1)) # re factor if not error may error
Lr <- confusionMatrix(y_pred, df_lr.test$classifier, positive = "1", mode = "everything")

print("plotting roc curves")
roc_lr_hr_ai_cp_br  <- roc(df_lr.test$classifier, pred_lr)
print("getting auc results")
auc_lr_hr_ai_cp_br <- auc(df_lr.test$classifier, pred_lr) # Logistic Regression
print("generating sensitivity and specificity")
Lr_hr_ai_cp_br <-as.data.frame(Lr$byClass) #Logistic Regression
sensitiviy_lr_hr_ai_cp_br <- Lr_hr_ai_cp_br["Sensitivity",]
specificity_lr_hr_ai_cp_br <- Lr_hr_ai_cp_br["Specificity",]
toc()
  
  

print("saving results") 
values <- c(accuracy_lr_hr,accuracy_lr_hr_ai,accuracy_lr_hr_ai_cp,accuracy_lr_hr_ai_br,accuracy_lr_hr_ai_cp_br,
            auc_lr_hr,auc_lr_hr_ai,auc_lr_hr_ai_cp,auc_lr_hr_ai_br,auc_lr_hr_ai_cp_br,
            sensitiviy_lr_hr,sensitiviy_lr_hr_ai,sensitiviy_lr_hr_ai_cp,sensitiviy_lr_hr_ai_br,sensitiviy_lr_hr_ai_cp_br,
            specificity_lr_hr,specificity_lr_hr_ai,specificity_lr_hr_ai_cp,specificity_lr_hr_ai_br,specificity_lr_hr_ai_cp_br)
names(values) <- c("accuracy_lr_hr","accuracy_lr_hr_ai","accuracy_lr_hr_ai_cp","accuracy_lr_hr_ai_br","accuracy_lr_hr_ai_cp_br",
                   "auc_lr_hr","auc_lr_hr_ai","auc_lr_hr_ai_cp","auc_lr_hr_ai_br","auc_lr_hr_ai_cp_br",
                  "sensitiviy_lr_hr","sensitiviy_lr_hr_ai","sensitiviy_lr_hr_ai_cp","sensitiviy_lr_hr_ai_br","sensitiviy_lr_hr_ai_cp_br",
                  "specificity_lr_hr","specificity_lr_hr_ai","specificity_lr_hr_ai_cp","specificity_lr_hr_ai_br","specificity_lr_hr_ai_cp_br")
result <- as.data.frame(t(as.matrix(values)))
write.csv(result, paste0(args[1],"/values_lr.csv")) 

print("preparing the roc graph")
all.roc <- list("lr_HR" = roc_lr_hr, "lr_HR+AI" = roc_lr_hr_ai, "lr_HR+AI+CP" = roc_lr_hr_ai_cp,"lr_HR+AI+BR" = roc_lr_hr_ai_br,"lr_HR+AI+CP+BR" = roc_lr_hr_ai_cp_br)
gg <- ggroc(all.roc, size = 0.8,legacy.axes = TRUE) + # finally, plot graph using ggroc()
  theme_classic() +
  labs(title  = paste0("ROC graphs for the parameters combinations for linear regression"),
       x = "Fraction of wake scored as sleep(FPR)",
       y = "Fraction of sleep scored as sleep(TPR)",
       colour = "Classifiers") 
gg <- ggsave(paste0(args[1],"/roc_lr.png"))

sink()