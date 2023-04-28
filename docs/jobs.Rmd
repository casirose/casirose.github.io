---
title: "Self Reflection"
author: "Roshan Shrestha"
date: "2023-02-27"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Introduction #

My growth in this course has always been progressive. I enjoyed the Modeling and Regression course as it helped me refreshed my knowledge and concept with the statistical concepts. Through this course, I believe I have built a strong foundation with statistical analysis and statistical modeling. Furthermore, I got the opportunity to implement the models and concept in R. This is my fourth semester using R and my coding journey with R has been progressively improved as well and I feel I'm pretty much proficient in using R now. 

I started with doing the import of data, pre-processing my dataset and running some exploratory data analysis for the dataset in R. I have also performed some exploratory analysis over here as many of my classmate also wanted to see which skills were frequently preferred, optimal days of job postings, the top skill set for the data analysis role, and the locations of the job posting. I have also included my visualization in my final project so that everyone can benefit from the analysis I have made on the job postings.


```{r}
library(dplyr)
```

```{r}
jobs <- read.csv("gsearch_jobs1.csv", row.names = 1)
```

I further performed some pre-processing on my datset to get the desired datset to run my model. 
```{r}
# replace string values in location column
jobs$location <- gsub("\\+.*", "", jobs$location)

```

```{r}
library(qdap)
skill_frequecny <- freq_terms(jobs$description_tokens)
```

```{r}
skill_frequecny
```


```{r}
library(plotly)

analysis_skills <- data.frame("Categorie" = rownames(skill_frequecny), skill_frequecny)
data <- analysis_skills[, c('WORD', 'FREQ')]

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

fig <- plot_ly(data, labels = ~WORD, values = ~FREQ, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(FREQ),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
                      #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE)
fig <- fig %>% layout(title = 'Pie Chart of on demand Data Analysis skills',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
```



```{r}
library(plotly)
set.seed(1)

Skills <- data.frame("Categorie" = rownames(skill_frequecny), skill_frequecny)
data <- Skills[, c('WORD', 'FREQ')]

# Generate 20 random colors
colors <- paste0("rgb(", round(runif(20, 0, 255)), ",",
                 round(runif(20, 0, 255)), ",",
                 round(runif(20, 0, 255)), ")")

fig <- plot_ly(data, x = ~WORD, y = ~FREQ, type = 'bar',
               marker = list(color = colors))

fig <- fig %>% layout(title = 'Top 20 Skills set in Data Analysis Jobs',
                      xaxis = list(title = 'Skills '),
                      yaxis = list(title = 'Frequency'))

fig

```




```{r}
job_python <- read.csv("job_python.csv", row.names = 1)
cleanjobs_salary <- filter(job_python, job_python$salary_standardized != 'NA')
```



```{r}
library(ggplot2)
library(plotly)
library(lubridate)

# Convert date column to date object
job_python$date_time <- ymd_hms(job_python$date_time)

# Create a new column with the day of the week
job_python$day <- weekdays(job_python$date_time)


# Create a new column for day of the week using base R weekdays() function
job_python$day <- weekdays(job_python$date_time)

# Aggregate job counts by day of the week
job_counts <- aggregate(title ~ day, data = job_python, FUN = length)

# Create the plot using ggplot2
job_counts_plot <- ggplot(data = job_counts, aes(x = day, y = title, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  labs(title = "Job Posts by Day of the Week", x = "Day of the Week", y = "Number of Job Posts") +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  theme_minimal()

# Create an interactive plot using plotly
ggplotly(job_counts_plot)



```



```{r}
library(tidyr)
Jobs_lon_lat <- read.csv("Jobs_lon_lat.csv", row.names = 1)
# Create a new data frame with the separated columns
Jobs_lon_lat_new <- Jobs_lon_lat %>%
  separate(location, into = c("city", "state"), sep = ", ")

```

```{r}
library(dplyr)

# group by location and count frequency
Jobs_lon_lat_freq <- Jobs_lon_lat %>% 
  group_by(location) %>% 
  summarise(freq = n()) 

# join the frequency count to Jobs_lon_lat
Jobs_lon_lat_with_freq <- left_join(Jobs_lon_lat, Jobs_lon_lat_freq, by = "location")

```


```{r}
library(ggplot2)
library(maps)
library(plotly)

# Filter Jobs_lon_lat dataframe to only include locations within the USA
Jobs_lon_lat_USA <- Jobs_lon_lat_with_freq[Jobs_lon_lat_with_freq$longitude > -125 &                                   Jobs_lon_lat_with_freq$longitude < -66 &                                   Jobs_lon_lat_with_freq$latitude > 25 &                                   Jobs_lon_lat_with_freq$latitude < 50, ]

# Create a US map using ggplot2 and maps
USA_map <- map_data("state")
p <- ggplot() +
  geom_polygon(data = USA_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = Jobs_lon_lat_USA, aes(x = longitude, y = latitude, label = location, text = paste("Number of Jobs postings: ", freq)), alpha = 0.5, size = 1, color = "red") +
  coord_map() +
  labs(title = "Job Postings by Location in USA")

ggplotly(p)



```


```{r}
# Aggregate job counts by location and sort in descending order
job_counts <- Jobs_lon_lat_USA %>%
  group_by(location) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(30, count)

# Create the plot using ggplot2
job_counts_plot <- ggplot(data = job_counts, aes(x = location, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Job Posts by Location", x = "Location", y = "Number of Job Posts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Create an interactive plot using plotly
ggplotly(job_counts_plot)

```


Removing the [] from description_tokens
```{r}
clean_salary_desp <- filter(cleanjobs_salary, cleanjobs_salary$description_tokens != '[]')
```

```{r}
library(qdap)
skill_frequecny <- freq_terms(cleanjobs_salary$description_tokens)
```



# How I met the Course Objectives #

The course objectives were fairly simple and straightforward where we had to mainly work with the statistical modeling and the statistical analysis of the data and how each statistical measure helped us in identifying the best model. Conducting and selecting the best model, I guess was one of the toughest part where we have to understand and look at each variable but I believe that integral part has built a strong foundation on me to determine and apply appropriate models in specific data context.


# Objective 1: Determine and apply the appropriate generalized linear model for a specific data context #

After completing the exploratory data analysis, I worked on creating a selected dataset for my model. The below code shows how I created it for my model.

```{r}
library(dplyr)
library(caret)
pred_data <- clean_salary_desp[, c("salary_standardized", "airflow" ,"alteryx" ,"apl" , "asp.net","assembly","atlassian","aurora","aws" ,"azure"               ,"bash"               
 ,"bigquery"            ,"bitbucket"           ,"c"                ,   "c.."                , "c.c.."              
 ,"cobol"               ,"cognos"              ,"crystal"         ,    "css"                , "dart"               
 ,"datarobot"           ,"dax"                 ,"docker"           ,   "dplyr"              , "excel"              
 ,"fortran"             ,"gcp"                 ,"gdpr"              ,  "ggplot2"            , "git"                
  ,"github"             , "gitlab"              ,"go"                ,  "golang"              ,"graphql"            
 ,"groovy"              ,"hadoop"              ,"html"               , "java"               , "javascript"         
 ,"jira"                ,"jquery"              ,"js"                 , "julia"              , "jupyter"            
 ,"keras"               ,"linux"               ,"linux.unix"         , "looker"             , "matlab"             
  ,"matplotlib"         , "microstrategy"     ,  "mongo"              , "mongodb"           ,  "mssql"              
  ,"mxnet"               ,"mysql"              , "nltk"               , "no.sql"            ,  "node"               
  ,"node.js"             ,"nosql"              , "nuix"               , "numpy"              , "outlook"            
  ,"pandas"              ,"perl"                ,"php"                , "pl.sql"              ,"plotly"             
 ,"postgres"            ,"postgresql"          ,"power_bi"           , "powerpoint"          ,"powerpoints"        
 ,"powershell"          ,"pyspark"            , "python"             , "pytorch"            , "qlik"               
, "r"                   ,"redis"              , "redshift"           , "rshiny"             , "ruby"               
 ,"rust"                ,"sap"                , "sas"                , "scala"               ,"scikit.learn"       
 ,"seaborn"             ,"selenium"            ,"sharepoint"         , "shell"              , "snowflake"          
 ,"solidity"            ,"spark"             ,  "splunk"             , "spreadsheet"        , "spss"               
 ,"sql"                 ,"ssis"               , "ssrs"               , "swift"              , "t.sql"              
 ,"tableau"             ,"tensorflow"         , "terminal"            ,"tidyr"              , "twilio"             
 ,"typescript"          ,"unix"               , "unix.linux"          ,"vb.net"             , "vba"                
 ,"visio"               ,"visual_basic"       , "vue"                , "vue.js"             , "word"  )]


```

As I further did some cleaning of my data because it had little information which wouldn't be useful in predicting a model. So, the below code calculates the column sums and it selects the column of pred_data except those with a sum of 0. Hence, this code removes any columns from pred_data that have sum less than 4. 

```{r}

# calculate the column sums and find the indices of the columns with a sum of 0
zero_cols <- which(colSums(pred_data) < 4)

# select all columns except those with a sum of 0
pred_data <- pred_data[, -zero_cols]

```

**Logistic Regression**

Logistic Regression is used to fit the sigmoid function to the data which estimates the probability of particular observation belonging to positive class which is salary_binary. Similarly, the logistic regression is trained on the training data and is used to predict the binary outcome variable for the test data. 

With logistic regression, you can predict the probability of an individual having a certain skill or not, based on the other skills they possess. This can be useful for tasks such as predicting the likelihood of a candidate being a good fit for a job based on their skillset, or identifying which skills are most important for a particular role.

I have implemented the below code for the logistic regression and accuracy is calculated to see the performance of the model.
```{r}
pred_data1 <- pred_data

# Calculate the mean salary
mean_salary <- mean(pred_data1$salary_standardized)

# Set salary to 0 or 1 based on the mean
pred_data1$salary_binary <- ifelse(pred_data1$salary_standardized < mean_salary, 0,
                                   ifelse(pred_data1$salary_standardized >= mean_salary, 1, NA))

# Split data into train and test sets
train_index <- sample(nrow(pred_data1), 0.7 * nrow(pred_data1))
train_data <- pred_data1[train_index, ]
test_data <- pred_data1[-train_index, ]

# Build logistic regression model
log_model <- glm(salary_binary ~ ., data = train_data, family = "binomial")

# Make predictions on the test set
predictions <- predict(log_model, newdata = test_data, type = "response")

# Calculate accuracy
threshold <- 0.5
predicted_classes <- ifelse(predictions > threshold, 1, 0)
accuracy <- mean(predicted_classes == test_data$salary_binary)

# Print accuracy
print(paste0("Accuracy: ", round(accuracy, 2)))

```

**Linear Regression**

I have also performed linear regression in my final project where the linear regression is used to model the relationship between the salary_standardized variable and the other variables. It uses the ```lm``` function to fit the linear regression model by finding the best-fit line that minimizes the sum of the squared difference between the observed values of the dependent variable and the predicted value of the model.

This code divides the pred_data dataset into a training set and a test set. It then uses the glm() function to create a logistic regression model and the predict() function to make predictions on the test set. The predict() function gives the projected odds of each person having a salary above or below the threshold when the type = "response" argument is used.

```{r}

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(pred_data$salary_standardized, p = .8, list = FALSE, times = 1)
trainData <- pred_data[trainIndex, ]
testData <- pred_data[-trainIndex, ]

# Create linear model
linear_model <- lm(salary_standardized ~ ., data = trainData)

# Predict on test data
predictions <- predict(linear_model, newdata = testData)
```


```{r}

# Load required libraries
library(dplyr)
library(tidyr)
library(broom)
library(stats)

# Fit linear model
linear_model <- lm(salary_standardized ~ ., data = pred_data)

# View summary of model
linm_sum <- summary(linear_model)
linm_sum

```

**Training my model with my Final Project Dataset**

I have performed the below code to see how my linear model is performing and here to test my function and the model, I have only selected sql skills to see the predicted salary and we can see that the predicted salary value is $98812.01

```{r}
# Let's assume that the user inputs the skills as a vector of strings
input_skills <- c("sql")

# Create an empty data frame with the same columns as pred_data
input_data <- data.frame(matrix(ncol = ncol(pred_data), nrow = 1))
colnames(input_data) <- colnames(pred_data)


# Set the values of the input data frame based on the user's input skills
for (skill in input_skills) {
  input_data[[skill]] <- 1
}

input_data[is.na(input_data)] <- 0


# Convert the input data frame to the same format as train_data
input_data_scaled <- data.frame(input_data[, -1])


# colnames(input_data_scaled) <- colnames(train_data_scaled)

# Use the trained model to predict the salary
predicted_salary <- predict(linear_model, newdata = input_data_scaled)

# Print the predicted salary
cat("Predicted salary: $", round(predicted_salary, 2), "\n")
```

```{r}
# Calculate RMSE
library(Metrics)
rmse <- rmse(testData$salary, predictions)
rmse
```


To calculate the accuracy of a linear regression model in R, I use the coefficient of determination (R-squared) metric. R-squared measures how well the model fits the data and ranges from 0 to 1, with higher values indicating a better fit.

```{r}
# R-squared value
rsq <- summary(linear_model)$r.squared
print(rsq)

```


```{r}
library(Metrics)

# Predict on test data
predictions <- predict(linear_model, newdata = testData)

# Calculate MAE
mae <- MAE(testData$salary, predictions)
print(paste0("MAE: ", round(mae, 2)))

```



**Linear Discriminant Analysis**

I have also tried implementing the linear discriminant analysis in my final project dataset through which I obtain a summary of my model's result. Discriminant Analysis is a statistical technique which is used for predicting the categorical outcomes based on a set of the predictor variable.

```{r}
#discriminant Analysis
library(MASS)

# Subset the data to exclude the outcome variable
predictors <- pred_data[, -which(names(pred_data) == "job_category")]

# Fit the discriminant analysis model
discriminant <- lda(salary_standardized ~ ., data = pred_data)

summary(discriminant)

```


# Objective 2: Describe probability as a foundation of statistical modeling, inlcuding inference and maximum likelihood estinamtion #

**Confidence Interval**

I have implemented the confidence interval for the linear model where it calculates the confidence interval for the coefficient of the linear regression model. Through the below confidence interval, it helps in identifying the uncertainty and significance of the relationship between the predictor variable and the dependent variable in the linear regression model.


```{r}
# Calculate confidence intervals for coefficients
ci <- confint(linear_model)
ci

```

**Maximum Likelihood Estimates**

Finding the parameter values that maximize the likelihood of receiving the observed data under a specific statistical model is done statistically using the maximum likelihood estimation approach. The MLE estimates for linear regression represent the coefficient values that give the observed data the highest likelihood. I tried implementing the function for the maximum likelihood coefficient to find out the coefficient from the linear regression model.

```{r}
# Calculate maximum likelihood estimates for coefficients
lm_mle <- coef(linear_model)
lm_mle
```                                             

# Objective 3: Conduct model selection for a set of candidate models. #

To look for the model selection,we could refer to various **goodness-of-fit** measures such as R-square, adjusted R-square, Akaike Information Criterion (AIC) or Bayesian Information Criterion(BIC). I implemented the statistical modeling through the below code which provides various statistics foe each coefficient. The tidy format helps in easily manipulating, visualizing, and analyzing the model output. Furthermore, with the values we get from the *AIC* and *BIC*, it helps in assessing and comparing different models based on their goodness of fit and complexity. 


```{r}
library(broom)

tidy(linear_model)
```

```{r}
glance(linear_model)
```

**Cross-Validation**

Cross-validation is used to for estimating the performance of the predictive model. Cross-validation involves diving the data into two parts. The validation set is used to estimate the model's performance on new or the unseen data. We mainly use cross-validation to avoid the overfitting as implementing the cross-validation helps in getting more accurate estimate of the model's performance.

Here, I have implemented the cross validation where I have set up a 10-fold cross validation which means that the data will be split into 10 equal parts and the model will be trained on 9 parts and tested on the remainig part. 


```{r}
# training a linear regression model using 10-fold cross-validation
control <- trainControl(method = "cv", number = 10)
Fit <- train(salary_standardized ~ ., data = trainData, method = "lm", metric = "RMSE", trControl = control)
# pred_data$salary_standardized

print(Fit)
```
From the above output, we can compute that the EMSE is the measure of the average difference between the predicted value and the actual value. Similarly, by looking at the R-squared value, mean absolute error, RMSE we can have a model selection based on that. The R-squared measures the proportion of the variance while the mean absolute error measure the absolute difference between the predicted and actual values.


**Calculating RMSE**

This code calculates the root mean square between the predicted salaries and the actual salaries in the rest dataset. The difference between the predicted and actual salaries is first squared, then the mean of these squared differences is taken, and finally the square root of this mean is calculated. 

```{r}
# Calculate RMSE
mse <- mean((predictions - testData$salary_standardized)^2)
rmse <- sqrt(mse)
rmse
```


**Bootstrapping**


```{r}
library(caret)

# Identify near zero variance predictors in the data frame
nzv <- nearZeroVar(pred_data, saveMetrics = TRUE)

# Remove the identified near zero variance predictors
pred_data <- pred_data[, !nzv$nzv]

lda_model <- lda(salary_standardized ~ ., data = pred_data)

```


```{r}
#Bootstrapping
library(MASS)
library(boot)

# Subset the data to exclude the outcome variable
predictors <- pred_data[, -which(names(pred_data) == "job_category")]


lda_coef <- function(data, index) {
  fit <- lda(salary_standardized ~ ., data = data[index, ])
  coef(fit)
}

# Use bootstrapping to estimate the standard errors of the coefficients
set.seed(123) # for reproducibility
boot_lda <- boot(data = pred_data, statistic = lda_coef, R = 1000)

plot(boot_lda)


```





# Objective 4: Communicate the results of statistical models to a general audience #

Communicating the result plays an important role and when communicating the result, it should be very precise so that the audience can easily understand it. It is better to use plain language and while there might be some of the technical concepts and technical terminology, it is better to provide context to the audience. I'm a great believer of visual aids such as graph, chart, table which could easily grab an attention of the audience and they can easily understand the findings.

The below plot is one of the visual aid for my linear model. It is a residual plot which is used to check the assumption of the linear model. The horizontal axix represents the predicted values and the vertical axis represents the residuals or the difference between the observed values and the predicted values. As this plot shows a random scatter points we can compute that my linear model met the assumption.


```{r}

R_fit <- augment(linear_model)

# plot fitted values and residuals
ggplot(data = R_fit, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals")
```

The below code is another method of communicating the result to the audience which is through the table data. This helps us in showing the importance of each variable as per the ranking.

```{r}
Imp <- varImp(Fit, scale = FALSE)
print(Imp)
```

```{r}
plot(Imp)
```

This code below creates a scatter plot of predicted vs the actual values. It helps the audience in getting an understanding of how the model is performing and the diagonal line to the plot represents the prediction.


```{r}
# Create scatter plot of predicted vs. actual values
plot(predictions, testData$salary_standardized, xlab = "Predicted DEBT_N", ylab = "Actual DEBT_N")

# Add a diagonal line to show perfect predictions
abline(0, 1, col = "Blue")

```

I have also used a QQ plot in my final project to have a overview of how my model is performing and to check the assumptions for my linear model. QQ plot helps in visually comparing the distribution of the dataset which is also useful for identifying deviations from normality, detecting outliers, adn assessign the overall shape of the data distribution.

```{r}
# Get the residuals
residuals <- residuals(linear_model)

#  QQ plot
qqnorm(residuals)
qqline(residuals)

```


# Objective 5: Use programming software (i.e. R) to fit and assess statistical models #

Apart from implementing linear regression, logistic regression and discriminant analysis, I also tried implementing other various machine learning algorith in my dataset to see how each different machine learning models performed.

**Random Forest**

I implemented the below code to see how it predicts for the regression analysis. I have used the randomForest function to fit the random forest model and it then calculates the mean absolute error to evaluate the model's performance.


```{r}
library(randomForest)
set.seed(123)
# Split data into train and test sets
train_index <- sample(nrow(pred_data), 0.7 * nrow(pred_data))
train_data <- pred_data[train_index, ]
test_data <- pred_data[-train_index, ]

# Fit random forest model
rf_model <- randomForest(salary_standardized ~ ., data = train_data, ntree = 75, mtry = sqrt(ncol(train_data) - 1))

# Make predictions on the test data
predictions <- predict(rf_model, test_data)

# Calculate the mean absolute error
mae <- mean(abs(predictions - test_data$salary_standardized))
print(paste0("MAE: ", round(mae, 2)))

```

**XGBOOST**

I also tried implementing the XGBoost and tried to see the same result on how the model performed through fitting the model and finding the mean absolute error.

```{r}
library(xgboost)

# Split data into train and test sets
train_index <- sample(nrow(pred_data), 0.7 * nrow(pred_data))
train_data <- pred_data[train_index, ]
test_data <- pred_data[-train_index, ]

# Set up xgboost matrix
xg_train <- xgb.DMatrix(as.matrix(train_data[, -1]), label = train_data$salary_standardized)
xg_test <- xgb.DMatrix(as.matrix(test_data[, -1]), label = test_data$salary_standardized)

# Define hyperparameters
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 3,
  nthread = 4
)

# Train the model
xgb_model <- xgb.train(
  params = params,
  data = xg_train,
  nrounds = 1000,
  watchlist = list(train = xg_train, test = xg_test),
  early_stopping_rounds = 10,
  verbose = 1
)

# Make predictions on the test data
predictions <- predict(xgb_model, xg_test)

# Calculate the mean absolute error
mae <- mean(abs(predictions - test_data$salary_standardized))
print(paste0("MAE: ", round(mae, 2)))

```
I also tried implementing my cod of the XGBoost to predict the salary of the skills for the sql which showed a slightly different salary than that of the linear model.


```{r}

# Let's assume that the user inputs the skills as a vector of strings
input_skills <- c("sql")

# Create an empty data frame with the same columns as pred_data
input_data <- data.frame(matrix(ncol = ncol(pred_data), nrow = 1))
colnames(input_data) <- colnames(pred_data)


# Set the values of the input data frame based on the user's input skills
for (skill in input_skills) {
  input_data[[skill]] <- 1
}

input_data[is.na(input_data)] <- 0


# Convert the input data frame to the same format as train_data
input_data_scaled <- data.frame(input_data[, -1])


# Predict salary using xgboost model
input_xg <- xgb.DMatrix(as.matrix(input_data_scaled))
predicted_salary <- predict(xgb_model, input_xg)

# Print predicted salarys
print(paste0("Predicted Salary: ", round(predicted_salary, 2)))
```



**Support vector machine**
```{r}
library(e1071)

# Split data into train and test sets
train_index <- sample(nrow(pred_data), 0.7 * nrow(pred_data))
train_data <- pred_data[train_index, ]
test_data <- pred_data[-train_index, ]

# Train SVM model
svm_model <- svm(salary_standardized ~ ., data = train_data)

# Make predictions on the test data
predictions <- predict(svm_model, test_data)

# Calculate the mean absolute error
mae <- mean(abs(predictions - test_data$salary_standardized))
print(paste0("MAE: ", round(mae, 2)))

```


**Support Vector Regression (SVR)**
```{r}
library(e1071)

# Split data into train and test sets
train_index <- sample(nrow(pred_data), 0.7 * nrow(pred_data))
train_data <- pred_data[train_index, ]
test_data <- pred_data[-train_index, ]

# Define the formula for the SVR
formula <- as.formula("salary_standardized ~ .")

# Train the SVR model
svm_model <- svm(formula, data=train_data, kernel="radial")

# Make predictions on the test data
predictions <- predict(svm_model, test_data)

# Calculate the mean absolute error
mae <- mean(abs(predictions - test_data$salary_standardized))
print(paste0("MAE: ", round(mae, 2)))

```

Among all the machine learning models I tried Support Vector machine had the lower mean absolute error.


# Conclusion #

I believe I have met all the objective of this course as this course has been a great learning experience for me to hone my skills and brush my theroteical concepts regarding the statistical modeling. I have attached my codes and the description on hiw I have met all the objective of the course above.


