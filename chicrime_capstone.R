####Dataset packages and file downloads. ######
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("kntr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubdridate", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(readr)
library(knitr)
library(scales)
library(lubridate)
library(randomForest)

options(timeout = 120)

##Chicago Crimes and Chicago Weather data set download. May take a couple minutes to download####

url <- "https://github.com/ccraig88/capstone-chicrime/archive/refs/heads/main.zip"
temp <- tempfile()
download.file(url,temp)
unzip(temp, "capstone-chicrime-main/chicago_crimes_v2.csv")

data <- read.csv("capstone-chicrime-main/chicago_crimes_v2.csv", header = TRUE, sep = ",")

unlink(temp)

temp2 <- tempfile()
download.file(url,temp2)
unzip(temp2, "capstone-chicrime-main/chiweather.csv")
weather <- read.csv("capstone-chicrime-main/chiweather.csv", header = TRUE, sep = ",")

unlink(temp)

set.seed(1, sample.kind="Rounding")


## Reviewing the structure of the Chicago Crimes data set in order to see what variables exist at a high level ##

str(data)

## This will produce a line chart showing the total number of crimes reported by date ##

data %>% mutate(new_date = as.Date(new_date), month_year = format(as.Date(new_date), "%Y-%m")) %>% 
  group_by(new_date) %>% summarize(n = n())  %>%
  ggplot(aes(new_date,n)) + geom_line(color = "blue") + theme(axis.text.x = element_text(angle = 90)) + scale_x_date() + 
  ggtitle("Reported Crimes in Chicago") + labs(x = "Date", y = "Count")

###Findings: Trend upwards in summer months, downward in winter months. 2023 is partial. Remove


### Remove 2023 from the data, group by month and plot total number of crimes reported by month ##

data <- data %>% filter(Year != 2023)

data %>% mutate(new_date = as.Date(new_date)) %>% mutate(month = month(new_date, label = TRUE)) %>% group_by(month) %>% summarize(total = n()) %>% ggplot(aes(month, total)) + geom_line(group = 1) + 
  ggtitle("Reported Crimes in Chicago by Month: 2021 - 2022") + labs(x = "Month", y = "Count")


## Findings: October, surprisingly, has the most amount of crime in this 2 year period. Summer months, as expected, show higher total crime


## This will produce a histogram of the total number of crimes reported in the Chicago Crimes Dataset ##

data %>% group_by(new_date) %>% summarize(n = n()) %>% ggplot(aes(n)) + geom_histogram(bins = 20)+ 
  ggtitle("Reported Crimes by Day Histogram: 2021 - 2022") + labs(x = "Total Number of Crimes per Day")


## Findings: Normal distribution



## This will produce a horizontal bar chart showing the types of crime in descending order by most reported ##

data %>% group_by(Primary.Type) %>% summarize(n = n())  %>% 
  ggplot(aes(x= reorder(Primary.Type,+n),n)) + geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "Crime Primary Type", y = "Count") + ggtitle("Chicago Crimes: 2021 - 2022") + 
  scale_y_continuous(labels = label_comma())

## Findings: Theft, battery, criminal damage are top 3. Type of crime varies


## This bar chart will show the amount of crime reports by district, sorted in descending order ##

data %>% group_by(District) %>% summarize(n = n()) %>% arrange(desc(n)) %>% 
  ggplot(aes(x = reorder(as.character(District),+n),n)) + geom_bar(stat = "identity")+ 
  ggtitle("Reported Crimes by Chicago District: 2021 - 2022") + labs(x = "District", y = "Count")


## Findings: District 6 has the most amount of total crime. District 31 has the least


## This will add a date attribute and look at the structure of the weather dataset ##

weather <- weather %>% unite(date, c(YEAR,MO,DY), sep = "-", remove = FALSE) %>% 
  mutate(date = as.Date(date))

str(weather)



## Shows Time Series line graph showing Temps in Chicago ##

weather %>% ggplot(aes(date, TEMP)) + geom_line()+ theme(axis.text.x = element_text(angle = 90)) + scale_x_date() + 
  ggtitle("Chicago Temperatures") + labs(x = "Date", y = "Temperature (in Celcius)")


## Findings: As expected, warm in summer, cold in winter. Not particularly useful


## This will show a histogram of Chicago temps ##

weather %>% ggplot(aes(TEMP)) + geom_histogram(bins = 50) + 
  ggtitle("Chicago Temperatures Histogram") + labs(x = "Temperature (in Celcius)")


## Findings: Bi-modal. Makes sense: Chicago has long winters and warm summers



## This bar chart shows the average temperatures by month in the weather dataset ##

weather %>% mutate(month = month(date, label = TRUE)) %>% group_by(month) %>% summarize(avg_temp = mean(TEMP)) %>% ggplot(aes(month, avg_temp)) + geom_bar(stat = "identity") + 
  ggtitle("Chicago Average Temperature by Month") + labs(x = "Month", y = "Temperature (in Celcius)")


## Findings: See average temps by month. Not insightful. No surprises with this. 

## I am going to group both data sets by date and join on date. Then will create a scatter plot ##

# This groups the weather data by date #
weather_grouped <- weather %>% group_by(date) %>% summarize(temp_high = max(TEMP))

# This groups the crimes data set by date #
data_grouped <- data %>% group_by(new_date) %>% summarize(total = n()) %>% mutate(new_date = as.Date(new_date))

# This will merge both by date and create a new table named "merged" #

merged <- weather_grouped %>% rename("new_date" = date) %>% left_join(data_grouped, by = join_by(new_date))

# This creates a scatter plot on the merged table. Points represent days #

merged %>% mutate(month = month(new_date, label = TRUE)) %>% ggplot(aes(total,temp_high, color = month, group = 1)) + geom_point() + geom_smooth() + 
  ggtitle("Daily High Temps x Total Crime in Chicago") + labs(x = "Total Daily Crimes", y = "Daily High Temperature (in Celsius)")

## Findings: Appears to be correlation

## This will show what the correlation coefficient is between Daily High Temps and Total Crime ##

merged %>% filter(!is.na(total)) %>% summarize(Correlation_Coefficient = cor(total, temp_high))


## Grouping Weather data by month and creating new table taking the average temperature, precipitation, humidity, wind speed, and atmospheric pressure ##

weather_data <- weather %>% mutate(month = month(date, label = TRUE)) %>% group_by(month) %>% 
  summarize(TEMP = mean(TEMP), PRCP = mean(PRCP), HMDT = mean(HMDT), 
            WND_SPD = mean(WND_SPD), ATM_PRESS = mean(ATM_PRESS))

## Grouping the crimes dataset and joining with the grouped weather dataset

compiled <- data %>% mutate(new_date = as.Date(new_date)) %>% mutate(month = month(new_date, label = TRUE)) %>% 
  group_by(month, Primary.Type, District) %>% summarize(total = n()) %>% left_join(weather_data, by = 'month') 

## Preview the Compiled Data

head(compiled)

####### PREDICTION USING RMSE#####

## This creates the RMSE function we will use for our prediction

RMSE <- function(true_crime, predicted_crime) {
  sqrt(mean((true_crime - predicted_crime)^2))
}

## This partitions the data into Test and Train Sets

y <- compiled$total
index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- compiled[index, ]
train_set <- compiled[-index, ]



# Model 1

# Calculates average
mu <- mean(train_set$total)

# First Model simply takes average mu and applies to RMSE
model1_rmse <- RMSE(test_set$total, mu)

# Create a tibble with results
model1_results <- tibble(Method = "Avg", RMSE = model1_rmse)
model1_results

# Model 2

# Creates averages by month
month_averages <- train_set %>% group_by(month) %>% summarize(b_month = mean(total - mu))

# Adds monthly averages to the prediction
prediction <- test_set %>% left_join(month_averages, by = 'month') %>% mutate(predicted = mu+ b_month) %>% pull(predicted)

# Calculates RMSE
month_rmse <- RMSE(test_set$total, prediction)

# Create a tibble with results

model2_results <- tibble(Method = "Month", RMSE = month_rmse)
model2_results

# Model 3

# Creates averages by crime type
type_averages <- train_set %>% left_join(month_averages, by = 'month') %>% 
  group_by(Primary.Type) %>% summarize(b_type = mean(total - mu - b_month))

# Add crime type averages to prediction
prediction <- test_set %>% left_join(month_averages, by = 'month') %>% 
  left_join(type_averages, by = 'Primary.Type') %>% mutate(predicted = mu + b_month+b_type) %>%
  pull(predicted)

# Calculates RMSE
type_rmse <- RMSE(test_set$total, prediction)

# Create a tibble with results
model3_results <- tibble(Method = "Type", RMSE = type_rmse)
model3_results


# Model 4

# Create averages by district
district_averages <- train_set %>% left_join(month_averages, by = 'month') %>%
  left_join(type_averages, by = 'Primary.Type') %>% group_by(District) %>% 
  summarize(b_district = mean(total - mu - b_month - b_type))

# Add district averages to prediction
prediction <- test_set %>% left_join(month_averages, by = 'month') %>% 
  left_join(type_averages, by = 'Primary.Type') %>% left_join(district_averages, by = 'District') %>%
  mutate(predicted = mu + b_month + b_type + b_district)  %>% pull(predicted)

# Calculates RMSE
district_rmse <- RMSE(test_set$total, prediction)

# Create a tibble with results
model4_results <- tibble(Method = "District", RMSE = district_rmse)
model4_results

# Model 5

# Creates averages by temperature
temp_averages <- train_set %>% left_join(month_averages, by = 'month') %>% left_join(type_averages, by = 'Primary.Type') %>%
  left_join(district_averages, by = 'District') %>% group_by(round = round(TEMP)) %>% 
  summarize(b_temp = mean(total - mu - b_month - b_type - b_district))

# Adds temperature averages to prediction
# IMPORTANT: Set the minimum predicted value to be 0 (there can't be negative crimes)
prediction <- test_set %>% left_join(month_averages, by = 'month') %>% 
  left_join(type_averages, by = 'Primary.Type') %>% left_join(district_averages, by = 'District') %>%
  mutate(round = round(TEMP)) %>% left_join(temp_averages, by = 'round') %>% 
  mutate(predicted = mu + b_month + b_type + b_district + b_temp) %>% 
  mutate(predicted = ifelse(predicted < 0,0,predicted)) %>% pull(predicted)

#Calculates RMSE
temp_rmse <- RMSE(test_set$total, prediction)

# Create a tibble with results
model5_results <- tibble(Method = "Temperature", RMSE = temp_rmse)
model5_results

# Compiles RMSE Results

# Creates a summary table with the 5 RMSE

models <- c("1. Avg","2. + Month", "3. + Type", "4. + District", "5. + Temp")
rmse_combined <- c(model1_rmse,month_rmse,type_rmse,district_rmse,temp_rmse)

summary_table <- data.frame(models,rmse_combined)
colnames(summary_table) <- c("Models", "RMSE")
summary_table

# Visually see how the prediction performed vs test set

## Create a table "test" that shows our prediction

test <- test_set %>% left_join(month_averages, by = 'month') %>% 
  left_join(type_averages, by = 'Primary.Type') %>% left_join(district_averages, by = 'District') %>%
  mutate(round = round(TEMP)) %>% left_join(temp_averages, by = 'round') %>% 
  mutate(predicted = mu + b_month + b_type + b_district + b_temp) %>% 
  mutate(predicted = ifelse(predicted < 0,0,predicted))

## Join table "test" with test set and plot actual vs prediction

test %>% group_by(month) %>% summarize(actual = sum(total), predicted = sum(predicted)) %>% 
  ggplot(aes(x = month)) + geom_line(aes(y=actual, group = 1), color = "blue") + 
  geom_line(aes(y = predicted, group = 1), color = "red", linetype = "dashed") + 
  ggtitle("Predicted Crimes (Red) vs Actual Crimes (Blue)") + labs(x = "Month", y = "Total Number of Crimes")



### Review Random Forest to see if this is a better prediction ###

set.seed(1)

# Fit Random Forest

fit <- randomForest(total~ Primary.Type+District+month+TEMP, data = train_set)

#Plot Random Forest
plot(fit)

##Findings: Accuracy stabilizes around 100 trees

# Apply Random Forest to Test Set


prediction <- predict(fit, newdata = test_set)

#Applying Prediction to Test_set and Plotting

test_set %>% ungroup() %>% mutate(y_hat = prediction) %>% group_by(month) %>% summarize(actual = sum(total), predicted = sum(y_hat)) %>% 
  ggplot(aes(x = month)) + geom_line(aes(y=actual, group = 1), color = "blue") + 
  geom_line(aes(y = predicted, group = 1), color = "red", linetype = "dashed") + 
  ggtitle("Predicted Crimes (Red using Random Forest) vs Actual Crimes (Blue)") + labs(x = "Month", y = "Total Number of Crimes") 

## RMSE from Random Forest: Does it improve?
test_set <- test_set %>% ungroup() %>% mutate(y_hat = prediction)

sqrt(mean((test_set$y_hat - test_set$total)^2))

#Findings: Surprisingly no, the RMSE calculated in our first modeling is better
