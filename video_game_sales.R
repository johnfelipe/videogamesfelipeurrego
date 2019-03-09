

# Preprocessing

RMSE <- function(y, g)
  return(sqrt(mean( (y-g)**2 )))

# List of packages we need
listOfPackages <- list("randomForest",
                       "kableExtra",
                       "tidyverse",
                       "corrplot",
                       "ggplot2",
                       "caret",
                       "knitr")

# Function that loads and installs if necessary indicated packages
UsePackages <- function(listOfPackages) {
  for (p in listOfPackages){
    # if (!is.element(p, installed.packages()[,1]))
    #   install.packages(p)
    require(p, character.only = TRUE)
  }
}
UsePackages(listOfPackages)

# precision
prec <- 4




# Read and clean the data

# read the data
the_data <- read_csv("input/Video_Games_Sales_as_at_22_Dec_2016.csv",
                     col_types = "cciccddddddidicc")
# structure of the data
str(the_data, give.attr=FALSE)


# The dataset consists of 16719 observations and 16 variables. Three of the variables are sales in different regions of the world and their sum is equal to the global sales variable which is the label that we are going to predict, so we remove region sales.


# look at variables that have a big number of unique values (more than 1 %)
the_data %>% select_if(is.character) %>%
  summarise_all(function(x) length(unique(x))/n()) %>% select_if(function(x) x > 0.01)
# there are too many unique values to be used

# plot histrogram of the Developer Feature
ggplot(the_data %>% filter(complete.cases(.))) +
  geom_bar(aes(Developer), col="tomato") + ggtitle("Histogram of Developer Feature")

# so we also remove Name and Developer features
the_data <- the_data %>%
  select(-c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales",
            "Name", "Publisher", "Developer")) %>%
  filter(complete.cases(.))
# data dimension
the_data %>% dim






# Summary and Data Visualisation

# summary
summary(the_data)

# histrograms of numeric variables
df <- the_data %>% select_if(is.numeric) %>% gather
df2 <- df %>% group_by(key) %>% filter(key=="Year_of_Release" | value < quantile(value,0.9))
ggplot(df2, aes(value, fill=key)) +
  facet_wrap(~key, scales="free", ncol=2) +
  geom_histogram(bins=60) + theme(legend.position='none') + ylab(NULL) + xlab(NULL)

# histrograms of character variables
df <- the_data %>% select_if(is.character) %>% gather
ggplot(df, aes(value, fill=key)) +
  facet_wrap(~key, scales="free", ncol=1) +
  geom_bar() + theme(legend.position='none') + ylab(NULL) + xlab(NULL)

# plot correlations
corrplot(cor(the_data %>% select_if(is.numeric)), diag=FALSE, title="Correlation Matirx")

# liberate memory
remove(df, df2)












# # Feature Selection and Feature Engineering

# Feature Selection means that we remove irrelevant variables that only add noise.
# Feature Engineering means that we add new variables



# categorical variable to dummy variables
dummy <- dummyVars(~ ., data = the_data, fullRank = TRUE)
dummy_data <- predict(dummy, the_data) %>% as_tibble
names(dummy_data) <- gsub("\\+|-", "", names(dummy_data))

# variables that are correlated to Global_Sales the most
as_tibble(cor(dummy_data)) %>% select("Global_Sales") %>%
  mutate(Varibale=names(dummy_data)) %>%
  filter(Global_Sales > 0.04 & Varibale!="Global_Sales") %>% arrange(desc(Global_Sales)) %>%
  mutate(Varibale=factor(Varibale, levels=Varibale)) %>%
  ggplot() + geom_col(aes(Varibale, Global_Sales), fill=I("blue"), col=I("red"), alpha=I(.2))





## Feature Importances

# It shows which variables play the  in predicting the Global Sales as if we use Random Forest Model

# numeric features
model_RandomForest <- randomForest(Global_Sales ~ .,
                                   data  = the_data %>% select_if(is.numeric),
                                   ntree = 100, keep.forest = FALSE, importance = TRUE)
varImpPlot(model_RandomForest, main="Feature Importances of Numeric Features")




set.seed(1)
# all features
model_RandomForest <- randomForest(Global_Sales~ .,
                                   data = dummy_data,
                                   ntree = 100, keep.forest = FALSE, importance = TRUE)
varImpPlot(model_RandomForest, main="Feature Importance of All Features")



# After looking at the correlation matrix and the Feature Importances, we can say what features are little relevant to the Global Sales and probably only add noise.



# 15 the most important features accroding to two metrics
IncMSE_15 <- sort(model_RandomForest$importance[,1], decreasing=TRUE)[1:15] %>% names
IncNod_15 <- sort(model_RandomForest$importance[,2], decreasing=TRUE)[1:15] %>% names
# how many features are in both list?
sum(IncMSE_15 %in% IncNod_15)
# importance lists are almost the same
dummy_data_short <- dummy_data %>% select(Global_Sales, IncMSE_15)





  

  






# # An example of overfitting

# If we try to predict Global Sales by a polynomial function of the Critic Count, we can overfit the system if we choose too high polynomial degree.


# we choose a small part of data
N <- 200
set.seed(1)
df <- the_data[sample(1:nrow(the_data), N),]

# train and validation
valid_df <- df[1:floor(N/3),]
train_df <- df[ceiling(N/3):N,]
RMSEs <- tibble()
for (i in 1:6){
  suppressWarnings(
    my_lm <- lm(Global_Sales ~ poly(Critic_Count, i, raw=TRUE), train_df)
  )
  suppressWarnings(
    prediction <- predict(my_lm, valid_df)
  )
  RMSEs <- bind_rows(RMSEs,
                     tibble("degree" = i, "rmse" = RMSE(valid_df$Global_Sales, prediction)))
}
# plot RMSE as a function of degree of polynomial
ggplot(RMSEs, aes(degree, rmse)) + geom_line(col="#FF9999", size=2) +
  ggtitle("RMSE vs polynomial degree")


# We see that the optimal degree of polynomial function is 2 and not higher.



# # Train and Prediction
# 
# A function that does the training, cross validation, parameter tuning and test on the validation set for a list of models.
# Training, cross validation and parameter tuning are carried out by 'train' function from 'caret' package.
# The cross validation is specified in 'train_control' variable.
# The parameter tuning is carried out by default with a default grid of parameters (which depends on the model).
# Although we can customise the grid if we feel that the problem is not a standard one


my_ML <- function(train_data, valid_data, models, rmse_path, custom_tuning=FALSE){
  
  # # set the random generator state
  # set.seed(1)
  
  # if custom_tuning=FALSE, we use a default grid of parameters for the tuning
  if(!custom_tuning)
    grids <- NULL
  
  # data frame to stock valdiation rmse of models
  df_rmse <- tibble()
  # list to stock trained models (its coefficients and train errors)
  list_models <- list()
  
  # loop by models
  for (model_name in models){
    # print(model_name)
    # time measure
    start.time <- Sys.time()
    
    # train the model on the train set
    suppressWarnings(
      my_model <- train(Global_Sales ~ .,
                        method       = model_name,
                        data         = train_data,
                        trControl    = train_control,
                        tuneGrid     = grids[[model_name]])
    )
    
    # predict on the validation set
    suppressWarnings(
      prediction <- predict(my_model, valid_data)
    )
    
    # calculate prediction RMSE
    df_rmse <- df_rmse %>% bind_rows(list("model"= model_name,
                                          "rmse" = RMSE(valid_data$Global_Sales, prediction),
                                          "time" = signif(Sys.time() - start.time, prec)))
    # add the model to the list of models
    list_models[[model_name]] <- my_model
  }
  # save errors on the disk
  df_rmse %>% write_csv(rmse_path)
  
  ## train error analysis
  dotplot(resamples(list_models))
  
  # return valdiation rmse
  return(df_rmse)
}




# Create train and validation set (10% of the data)


set.seed(1)
# separate data into train and valdiation sets randomly, 90 and 10%
valid_index <- createDataPartition(y = the_data$Global_Sales, p = 0.1, list = FALSE)
train_data  <- dummy_data[-valid_index,]
valid_data  <- dummy_data[valid_index,]
# Short set of features
train_data_short  <- dummy_data_short[-valid_index,]
valid_data_short  <- dummy_data_short[valid_index,]



# Settings


# list of models
models <- c("lm", "glmnet", "svmRadial", "rpart", "rf", "xgbTree")

# range of parameters for the customised parameters tuning
grids                <- list()
grids[["lm"]]        <- NULL
grids[["glmnet"]]    <- expand.grid(alpha     = c(0, .5, 1),
                                    lambda    = c(.1, 1))
grids[["svmRadial"]] <- expand.grid(sigma     = c(.05, .045),
                                    C         = c(2.2))
grids[["rpart"]]     <- expand.grid(cp        = c(.05, .2))
grids[["rf"]]        <- expand.grid(.mtry     = c(1, 5, 15))
grids[["xgbTree"]]   <- expand.grid(nrounds   = c(10, 100),
                                    max_depth = c(5, 10, 15),
                                    eta       = c(.4, .1, .01),
                                    gamma     = c(0, 1, 3),
                                    subsample = c(.8, 1),
                                    colsample_bytree = 1,
                                    min_child_weight = 1)

# inialise the Cross-Valdiation (10-Cross-Section Validation)
train_control  <- trainControl(method = "cv", number = 10)





## All features


### Default Parameters Tuning


rmse_path <- "output/rmse.csv"
results <- my_ML(train_data, valid_data, models, rmse_path, custom_tuning=FALSE)
results %>% mutate(rmse=signif(rmse,prec))




### Customised Parameters Tuning


rmse_path_t <- "output/rmse_tuning.csv"
results <- my_ML(train_data, valid_data, models, rmse_path_t, custom_tuning=TRUE)
results %>% mutate(rmse=signif(rmse,prec))




## Short set of features


### Default Parameters Tuning


rmse_path_s <- "output/rmse_short.csv"
results <- my_ML(train_data_short, valid_data_short, models, rmse_path_s, custom_tuning=FALSE)
results %>% mutate(rmse=signif(rmse,prec))


### Customised Parameters Tuning

rmse_path_st <- "output/rmse_short_tuning.csv"
results <- my_ML(train_data_short, valid_data_short, models, rmse_path_st, custom_tuning=TRUE)
results %>% mutate(rmse=signif(rmse,prec))




# Results

results <- suppressMessages(
  read_csv("output/rmse.csv") %>%
    bind_rows(
      read_csv("output/rmse_tuning.csv") %>% mutate(model = paste0(model,"_t"))
    ) %>%
    bind_rows(
      read_csv("output/rmse_short.csv") %>% mutate(model = paste0(model,"_s"))
    ) %>%
    bind_rows(
      read_csv("output/rmse_short_tuning.csv") %>% mutate(model = paste0(model,"_st"))
    ) %>% 
    arrange(rmse) %>% 
    mutate("rmse" = signif(rmse,prec))
)

# all rmse
results

# the smallest error
results %>% slice(which.min(rmse))




# Conclusion

# As we see, models with the lowest RMSE are Random Forest and XGBoost which are both Ensemble Learning Methods and have a high degree of freedom (so they are able to fit a complex data).
# Ensemble Learning Method is a model that being a strong learner consists of an aggregation of a big number of weak learners, in the case of Random Forest and XGBoost, Decision Trees. The difference in models is the way of aggregating.


# best models
results %>% slice(1:5)


# The best model is XGBoost trained on reduced set of features with customised tuning grid. Reduced set of features allows to exclude the noises by taking into account only the pertinent information. Customising the parameter tuning permits to exploit all the capacities of the model. It worth mentioning that XGBoost is one of the most used models in Kaggle competitions.
# As second model comes Random Forest on reduced set of features, it performs a bit badlier with customised tuning, but in any case better than XGBoost with default tuning. In terms of computational time, the both models are quite the same.

