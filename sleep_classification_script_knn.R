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
sink(paste0(args[1],"/log_knn.txt"),append = T)

tic()

print("preprocessing the data")
dat <- list.files(args[2], full.names = TRUE, recursive = TRUE) %>%
  map_df(read_csv) %>%
  mutate(clock_proxy = cos(time_stamp %% (24*60*60)/(60*60) * 15 * pi/180)) %>%
  mutate(classifier = if_else(sleep_state >= 1, 1, 0)) %>%
  select(activity_index,br,hr,clock_proxy,classifier)
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
df_knn <- select(dat, hr, classifier)


print("normalizing and splitting the data")
final.df_knn <- scale(df_knn[,1:length(df_knn)-1]) %>% cbind(df_knn[length(df_knn)]) 
final.df_knn <- na.omit(final.df_knn)
set.seed(101) 
split = sample.split(final.df_knn$classifier, SplitRatio = 0.75)
df_knn.train = subset(final.df_knn, split == TRUE)
df_knn.train[,"hr"] <- jitter(df_knn.train[,"hr"])
df_knn.test = subset(final.df_knn, split == FALSE) 
df_knn.test[,"hr"] <- jitter(df_knn.test[,"hr"])

print("modelling with knn of parameter combination 1")
pred_knn <- knn(df_knn.train[-(para_num+1)], df_knn.test[-(para_num+1)], cl = df_knn.train$classifier, k=21, use.all = FALSE) 
if(para_num == 4){saveRDS(pred_knn, paste0(args[1],"/",parameter_comb,"_pred_knn.rds"));print("saving rds_knn")}
accuracy_knn_hr <- (1 - mean(df_knn.test$classifier != pred_knn)) # accuracy of knn
t_pred <- factor(df_knn.test$classifier, levels=c(0, 1)) # re factor
knn <- confusionMatrix(pred_knn, t_pred, positive = "1", mode = "everything")


print("plotting roc curves")
roc_knn_hr  <- roc(df_knn.test$classifier, as.numeric(pred_knn))
print("getting auc results")
auc_knn_hr <- auc(df_knn.test$classifier, as.numeric(pred_knn)) # Logistic Regression
print("generating sensitivity and specificity")
knn_hr <-as.data.frame(knn$byClass) #Logistic Regression
sensitiviy_knn_hr <- knn_hr["Sensitivity",]
specificity_knn_hr <- knn_hr["Specificity",]
toc()

######### Parameter combination 2: hr+ai ###########

tic()
print("starting parameter combination 2: Activity_index & Heart_rate")
parameter_comb <- "ai_hr";para_num = 2
df_knn <- select(dat, activity_index, hr, classifier)

print("normalizing and splitting the data")
final.df_knn <- scale(df_knn[,1:length(df_knn)-1]) %>% cbind(df_knn[length(df_knn)]) 
final.df_knn <- na.omit(final.df_knn)
set.seed(101) 
split = sample.split(final.df_knn$classifier, SplitRatio = 0.75)
df_knn.train = subset(final.df_knn, split == TRUE)
df_knn.train[,"hr"] <- jitter(df_knn.train[,"hr"])
df_knn.test = subset(final.df_knn, split == FALSE)
df_knn.test[,"hr"] <- jitter(df_knn.test[,"hr"])

print("modelling with knn of parameter combination 1")
pred_knn <- knn(df_knn.train[-(para_num+1)], df_knn.test[-(para_num+1)], cl = df_knn.train$classifier, k=21, use.all = FALSE) 
if(para_num == 4){saveRDS(pred_knn, paste0(args[1],"/",parameter_comb,"_pred_knn.rds"));print("saving rds_knn")}
accuracy_knn_hr_ai <- (1 - mean(df_knn.test$classifier != pred_knn)) # accuracy of knn
t_pred <- factor(df_knn.test$classifier, levels=c(0, 1)) # re factor
knn <- confusionMatrix(pred_knn, t_pred, positive = "1", mode = "everything")

print("plotting roc curves")
roc_knn_hr_ai  <- roc(df_knn.test$classifier, as.numeric(pred_knn))
print("getting auc results")
auc_knn_hr_ai <- auc(df_knn.test$classifier, as.numeric(pred_knn)) # Logistic Regression
print("generating sensitivity and specificity")
knn_hr_ai <-as.data.frame(knn$byClass) #Logistic Regression
sensitiviy_knn_hr_ai <- knn_hr_ai["Sensitivity",]
specificity_knn_hr_ai <- knn_hr_ai["Specificity",]

toc()

######### Parameter combination 3: hr+ai+cp ###########

tic()
print("starting parameter combination 3: Activity_index & Heart_rate & clock_proxy")
parameter_comb <- "ai_hr_cp";para_num = 3
df_knn <- select(dat, activity_index, hr, clock_proxy, classifier)

print("normalizing and splitting the data")
final.df_knn <- scale(df_knn[,1:length(df_knn)-1]) %>% cbind(df_knn[length(df_knn)]) 
final.df_knn <- na.omit(final.df_knn)
set.seed(101) 
split = sample.split(final.df_knn$classifier, SplitRatio = 0.75)
df_knn.train = subset(final.df_knn, split == TRUE)
df_knn.train[,"hr"] <- jitter(df_knn.train[,"hr"])
df_knn.test = subset(final.df_knn, split == FALSE)
df_knn.test[,"hr"] <- jitter(df_knn.test[,"hr"])

print("modelling with knn of parameter combination 1")
pred_knn <- knn(df_knn.train[-(para_num+1)], df_knn.test[-(para_num+1)], cl = df_knn.train$classifier, k=21, use.all = FALSE) 
if(para_num == 4){saveRDS(pred_knn, paste0(args[1],"/",parameter_comb,"_pred_knn.rds"));print("saving rds_knn")}
accuracy_knn_hr_ai_cp <- (1 - mean(df_knn.test$classifier != pred_knn)) # accuracy of knn
t_pred <- factor(df_knn.test$classifier, levels=c(0, 1)) # re factor
knn <- confusionMatrix(pred_knn, t_pred, positive = "1", mode = "everything")

print("plotting roc curves")
roc_knn_hr_ai_cp  <- roc(df_knn.test$classifier, as.numeric(pred_knn))
print("getting auc results")
auc_knn_hr_ai_cp <- auc(df_knn.test$classifier, as.numeric(pred_knn)) # Logistic Regression
print("generating sensitivity and specificity")
knn_hr_ai_cp <-as.data.frame(knn$byClass) #Logistic Regression
sensitiviy_knn_hr_ai_cp <- knn_hr_ai_cp["Sensitivity",]
specificity_knn_hr_ai_cp <- knn_hr_ai_cp["Specificity",]

toc()

######### Parameter combination 4: hr_ai_br ###########

tic()
print("starting parameter combination 4: Activity_index & Heart_rate & Breathing_rate")
parameter_comb <- "ai_hr_br";para_num = 3
df_knn <- select(dat, activity_index, hr, br, classifier)
print("normalizing and splitting the data")
final.df_knn <- scale(df_knn[,1:length(df_knn)-1]) %>% cbind(df_knn[length(df_knn)]) 
final.df_knn <- na.omit(final.df_knn)
set.seed(101) 
split = sample.split(final.df_knn$classifier, SplitRatio = 0.75)
df_knn.train = subset(final.df_knn, split == TRUE)
df_knn.train[,"hr"] <- jitter(df_knn.train[,"hr"])
df_knn.test = subset(final.df_knn, split == FALSE)
df_knn.test[,"hr"] <- jitter(df_knn.test[,"hr"])

print("modelling with knn of parameter combination 1")
pred_knn <- knn(df_knn.train[-(para_num+1)], df_knn.test[-(para_num+1)], cl = df_knn.train$classifier, k=21, use.all = FALSE) 
if(para_num == 4){saveRDS(pred_knn, paste0(args[1],"/",parameter_comb,"_pred_knn.rds"));print("saving rds_knn")}
accuracy_knn_hr_ai_br <- (1 - mean(df_knn.test$classifier != pred_knn)) # accuracy of knn
t_pred <- factor(df_knn.test$classifier, levels=c(0, 1)) # re factor
knn <- confusionMatrix(pred_knn, t_pred, positive = "1", mode = "everything")

print("plotting roc curves")
roc_knn_hr_ai_br  <- roc(df_knn.test$classifier, as.numeric(pred_knn))
print("getting auc results")
auc_knn_hr_ai_br <- auc(df_knn.test$classifier, as.numeric(pred_knn)) # Logistic Regression
print("generating sensitivity and specificity")
knn_hr_ai_br <-as.data.frame(knn$byClass) #Logistic Regression
sensitiviy_knn_hr_ai_br <- knn_hr_ai_br["Sensitivity",]
specificity_knn_hr_ai_br <- knn_hr_ai_br["Specificity",]
toc()

######### Parameter combination 5: hr_ai_br_cp ###########


tic()
print("starting parameter combination 5: Activity_index & Heart_rate & clock_proxy & Breathing_rate")
parameter_comb <- "ai_hr_cp_br";para_num = 4
df_knn <- select(dat, activity_index, hr, clock_proxy, br, classifier)
print("normalizing and splitting the data")
final.df_knn <- scale(df_knn[,1:length(df_knn)-1]) %>% cbind(df_knn[length(df_knn)]) 
final.df_knn <- na.omit(final.df_knn)
set.seed(101) 
split = sample.split(final.df_knn$classifier, SplitRatio = 0.75)
df_knn.train = subset(final.df_knn, split == TRUE)
df_knn.train[,"hr"] <- jitter(df_knn.train[,"hr"])
df_knn.test = subset(final.df_knn, split == FALSE)
df_knn.test[,"hr"] <- jitter(df_knn.test[,"hr"])

print("modelling with knn of parameter combination 1")
pred_knn <- knn(df_knn.train[-(para_num+1)], df_knn.test[-(para_num+1)], cl = df_knn.train$classifier, k=21, use.all = FALSE) 
if(para_num == 4){saveRDS(pred_knn, paste0(args[1],"/",parameter_comb,"_pred_knn.rds"));print("saving rds_knn")}
accuracy_knn_hr_ai_cp_br <- (1 - mean(df_knn.test$classifier != pred_knn)) # accuracy of knn
t_pred <- factor(df_knn.test$classifier, levels=c(0, 1)) # re factor
knn <- confusionMatrix(pred_knn, t_pred, positive = "1", mode = "everything")

print("plotting roc curves")
roc_knn_hr_ai_cp_br  <- roc(df_knn.test$classifier, as.numeric(pred_knn))
print("getting auc results")
auc_knn_hr_ai_cp_br <- auc(df_knn.test$classifier, as.numeric(pred_knn)) # Logistic Regression
print("generating sensitivity and specificity")
knn_hr_ai_cp_br <-as.data.frame(knn$byClass) #Logistic Regression
sensitiviy_knn_hr_ai_cp_br <- knn_hr_ai_cp_br["Sensitivity",]
specificity_knn_hr_ai_cp_br <- knn_hr_ai_cp_br["Specificity",]
toc()



print("saving results") 
values <- c(accuracy_knn_hr,accuracy_knn_hr_ai,accuracy_knn_hr_ai_cp,accuracy_knn_hr_ai_br,accuracy_knn_hr_ai_cp_br,
            auc_knn_hr,auc_knn_hr_ai,auc_knn_hr_ai_cp,auc_knn_hr_ai_br,auc_knn_hr_ai_cp_br,
            sensitiviy_knn_hr,sensitiviy_knn_hr_ai,sensitiviy_knn_hr_ai_cp,sensitiviy_knn_hr_ai_br,sensitiviy_knn_hr_ai_cp_br,
            specificity_knn_hr,specificity_knn_hr_ai,specificity_knn_hr_ai_cp,specificity_knn_hr_ai_br,specificity_knn_hr_ai_cp_br)
names(values) <- c("accuracy_knn_hr","accuracy_knn_hr_ai","accuracy_knn_hr_ai_cp","accuracy_knn_hr_ai_br","accuracy_knn_hr_ai_cp_br",
                   "auc_knn_hr","auc_knn_hr_ai","auc_knn_hr_ai_cp","auc_knn_hr_ai_br","auc_knn_hr_ai_cp_br",
                   "sensitiviy_knn_hr","sensitiviy_knn_hr_ai","sensitiviy_knn_hr_ai_cp","sensitiviy_knn_hr_ai_br","sensitiviy_knn_hr_ai_cp_br",
                   "specificity_knn_hr","specificity_knn_hr_ai","specificity_knn_hr_ai_cp","specificity_knn_hr_ai_br","specificity_knn_hr_ai_cp_br")
result <- as.data.frame(t(as.matrix(values)))
write.csv(result, paste0(args[1],"/values_knn.csv")) 

print("preparing the roc graph")
all.roc <- list("knn_HR" = roc_knn_hr, "knn_HR+AI" = roc_knn_hr_ai, "knn_HR+AI+CP" = roc_knn_hr_ai_cp,"knn_HR+AI+BR" = roc_knn_hr_ai_br,"knn_HR+AI+CP+BR" = roc_knn_hr_ai_cp_br)
gg <- ggroc(all.roc, size = 0.8,legacy.axes = TRUE) + # finally, plot graph using ggroc()
  theme_classic() +
  labs(title  = paste0("ROC graphs for the parameters combinations for linear regression"),
       x = "Fraction of wake scored as sleep(FPR)",
       y = "Fraction of sleep scored as sleep(TPR)",
       colour = "Classifiers") 
gg <- ggsave(paste0(args[1],"/roc_knn.png"))

sink()