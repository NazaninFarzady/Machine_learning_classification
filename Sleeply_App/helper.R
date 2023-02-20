##########################
#                        #
#    Global Variables    #
#                        #
#########################


# data used for training KNN 

df_train <- readRDS("data/model_data_knn.rds")
downsampleDF <- readRDS("data/model_data_knn_down.rds")


# define 'not in' function
'%ni%' <- Negate('%in%')  


# column name to check with for the prediction process
fixed_pred <- c("accel_z", "hr","br", "cp")
