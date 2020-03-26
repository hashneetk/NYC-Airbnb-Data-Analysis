# Final class project
# Name:Hashneet Kaur
# Last Modified - 
# --------------------------------------------------------
# Import the libraries used in the project
#------------------------------------------------------------
  library(dplyr) 
  library(corrplot)
  library(plotly)
  library(ggplot2)
  library(lubridate)
  library(tidyverse)
library(lubridate)

# ---------------------------------------------------------------
# Step 1: Import the data in CSV and clean the data for further steps
#----------------------------------------------------------------

setwd("~/RProjects/new-york-city-airbnb-open-data")  #set working directory
data_abnyc <- read.csv("AB_NYC_2019.csv")

#Examine the data in the dataset
head(data_abnyc)
summary(data_abnyc)

# id                                         name          host_id                 host_name    
# Min.   :    2539   Hillside Hotel                  :   18   Min.   :     2438   Michael     :  417  
# 1st Qu.: 9471945   Home away from home             :   17   1st Qu.:  7822033   David       :  403  
# Median :19677284                                   :   16   Median : 30793816   Sonder (NYC):  327  
# Mean   :19017143   New york Multi-unit building    :   16   Mean   : 67620011   John        :  294  
# 3rd Qu.:29152178   Brooklyn Apartment              :   12   3rd Qu.:107434423   Alex        :  279  
# Max.   :36487245   Loft Suite @ The Box House Hotel:   11   Max.   :274321313   Blueground  :  232  
# (Other)                         :48805                       (Other)     :46943  
# neighbourhood_group            neighbourhood      latitude       longitude                room_type    
# Bronx        : 1091    Williamsburg      : 3920   Min.   :40.50   Min.   :-74.24   Entire home/apt:25409  
# Brooklyn     :20104    Bedford-Stuyvesant: 3714   1st Qu.:40.69   1st Qu.:-73.98   Private room   :22326  
# Manhattan    :21661    Harlem            : 2658   Median :40.72   Median :-73.96   Shared room    : 1160  
# Queens       : 5666    Bushwick          : 2465   Mean   :40.73   Mean   :-73.95                          
# Staten Island:  373    Upper West Side   : 1971   3rd Qu.:40.76   3rd Qu.:-73.94                          
# Hell's Kitchen    : 1958   Max.   :40.91   Max.   :-73.71                          
#                        (Other)           :32209                                                           
#     price         minimum_nights    number_of_reviews     last_review    reviews_per_month
#  Min.   :    0.0   Min.   :   1.00   Min.   :  0.00              :10052   Min.   : 0.010   
# 1st Qu.:   69.0   1st Qu.:   1.00   1st Qu.:  1.00    2019-06-23: 1413   1st Qu.: 0.190   
# Median :  106.0   Median :   3.00   Median :  5.00    2019-07-01: 1359   Median : 0.720   
# Mean   :  152.7   Mean   :   7.03   Mean   : 23.27    2019-06-30: 1341   Mean   : 1.373   
# 3rd Qu.:  175.0   3rd Qu.:   5.00   3rd Qu.: 24.00    2019-06-24:  875   3rd Qu.: 2.020   
# Max.   :10000.0   Max.   :1250.00   Max.   :629.00    2019-07-07:  718   Max.   :58.500   
#                                                       (Other)   :33137   NA's   :10052    
# calculated_host_listings_count availability_365
# Min.   :  1.000                Min.   :  0.0   
# 1st Qu.:  1.000                1st Qu.:  0.0   
# Median :  1.000                Median : 45.0   
# Mean   :  7.144                Mean   :112.8   
# 3rd Qu.:  2.000                3rd Qu.:227.0   
# Max.   :327.000                Max.   :365.0  

# From the summary of the dataset we can see that the column 'last_review' has some blank rows and the column 
# 'reviews_per_month' has NA's. The column last_review will not be used in our analyses so we will be dropping this
# column in the next step. For the NA values in reviews_per_month - we can replace the NA's with 0

data_abnyc[is.na(data_abnyc)] <- 0

# Since we will not be using the columns id, host_name and last_review column, these columns can be dropped.

data_abnyc$id <- NULL
data_abnyc$host_name <- NULL
data_abnyc$last_review <- NULL

# Take a look at the final list of columns and check the number of observations 

glimpse(data_abnyc)

# Observations: 48,895
# Variables: 13
# $ name                           <fct> 
# $ host_id                        <int> 
# $ neighbourhood_group            <fct> 
# $ neighbourhood                  <fct> 
# $ latitude                       <dbl> 
# $ longitude                      <dbl> 
# $ room_type                      <fct> 
# $ price                          <int> 
# $ minimum_nights                 <int> 
# $ number_of_reviews              <int>
# $ reviews_per_month              <dbl> 
# $ calculated_host_listings_count <int> 
# $ availability_365               <int> 

# With this our data preparation is done. 

#-------------------------------------------------------------------------------
# Step 2 - Study the data further and create the exploratory graphs for the data
#-------------------------------------------------------------------------------

# Check the unique neighborhood groups

unique(data_abnyc$neighbourhood_group)

#[1] Brooklyn      Manhattan     Queens        Staten Island Bronx        
# Levels: Bronx Brooklyn Manhattan Queens Staten Island

unique(data_abnyc$neighbourhood)

# Let's check the density distribution of the price.

ggplot(data_abnyc, aes(price)) +
  geom_histogram(bins = 20, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Price Distribution") +
  theme(axis.title = element_text(), axis.title.x = "element_text()") +
  geom_vline(data = data_abnyc, xintercept = round(mean(data_abnyc$price),2), size = 2, linetype = 3)

# This distriubution graph is very skewed so let's plot the graph by distributing it to log 10

ggplot(data_abnyc, aes(price)) +
  geom_histogram(bins = 20, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Price Distribution with log10 Distribution") +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(data = data_abnyc, xintercept = round(mean(data_abnyc$price),2), size = 2, linetype = 3) +
  scale_x_log10() +
  annotate("label", x = 1700, y = 0.75,label = paste("Mean price = ", paste0(round(mean(data_abnyc$price), 2), "$")),
           color =  "#32CD32", size = 8)

# This graph shows that the mean price for all the neighborhoods combined.

# Now let's see the mean price of each neighborhood

airbnb_nh <- data_abnyc %>%
  group_by(neighbourhood_group) %>%
  summarise(price = round(mean(price),2))

#neighbourhood_group price
# <fct>               <dbl>
# 1 Bronx                87.5
# 2 Brooklyn            124. 
# 3 Manhattan           197. 
# 4 Queens               99.5
# 5 Staten Island       115. 

ggplot(data_abnyc, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Distribution of price based on the neighborhood")+
  
  geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
  geom_text(data = airbnb_nh,y = 1.5, aes(x = price + 1400, label = paste("Mean  = ",price)), color = "darkgreen", size = 4) +
  facet_wrap(~neighbourhood_group) +
  scale_x_log10() 

# From these graphs we can see that Manhattan has the largest mean price and the price distribution. 
# It is followed by Brooklyn and Staten Island.
#--------------------------------------------------------------------------------------------------
# Now let's study the percentile price distribution across different neighborhood groups
#-----------------------------------------------------------------------------------------------
price_manhattan <- data_abnyc %>%
  select(neighbourhood_group,price) %>%
  filter(neighbourhood_group == "Manhattan")
x<-quantile(price_manhattan$price, probs=seq(0,1,0.25), na.rm=TRUE)

#Output:

# 0%   25%   50%   75%  100% 
# 0    95   150   220 10000 
# The percentiles show that there are some extreme values with price as 10,000

price_Bronx <- data_abnyc %>%
  select(neighbourhood_group,price) %>%
  filter(neighbourhood_group == "Bronx")
x<-quantile(price_manhattan$price, probs=seq(0,1,0.25), na.rm=TRUE)

#Output:
# 0%  25%  50%  75% 100% 
# 0   45   65   99 2500 
# The percentiles show that there are listings with an outlying price value as 2500

price_Brooklyn <- data_abnyc %>%
  select(neighbourhood_group,price) %>%
  filter(neighbourhood_group == "Brooklyn")
x<-quantile(price_manhattan$price, probs=seq(0,1,0.25), na.rm=TRUE)

#Output:
# 0%  25%  50%  75% 100% 
# 0   45   65   99 2500 

# The percentiles show that there are listings with an outlying price value as 2500

price_Queens <- data_abnyc %>%
  select(neighbourhood_group,price) %>%
  filter(neighbourhood_group == "Queens")
x<-quantile(price_manhattan$price, probs=seq(0,1,0.25), na.rm=TRUE)

# Output:
# 0%  25%  50%  75% 100% 
# 0   45   65   99 2500 
# The percentiles show that there are listings with an outlying price value as 2500

price_Staten_Island <- data_abnyc %>%
  select(neighbourhood_group,price) %>%
  filter(neighbourhood_group == "Staten_Island")
x<-quantile(price_manhattan$price, probs=seq(0,1,0.25), na.rm=TRUE)

# Output:
# 0%  25%  50%  75% 100% 
# 0   45   65   99 2500 
# The percentiles show that there are listings with an outlying price value as 2500

# From the percentile price distribution across different listed neighborhood groups we can see that all of
# listings are below 500$ with some outliers. We will be removing these outliers to create a price prediction model

#--------------------------------------------------------------------------------------
# Explore the data based on the room type
#--------------------------------------------------------------------------------------

#Check unique room types

unique(data_abnyc$room_type)

# [1] Private room    Entire home/apt Shared room    
# Levels: Entire home/apt Private room Shared room

ggplot(data_abnyc, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Price by Room Type") +
  geom_hline(yintercept = mean(data_abnyc$price), color = "purple", linetype = 2)

# As expected ,from these plots it is visible that the highest mean price is for 'Entire home/apt
# and lowest for shared rooms.
#-----------------------------------------------------------------
# Graphs for price distribution based on the neighborhood group and room type 
# combined.

data_abnyc %>% arrange(desc(price)) %>% top_n(10)  %>%  
  ggplot(aes(x = price, fill = neighbourhood_group)) +
  geom_histogram(bins = 50) +
  scale_x_log10() + 
  ggtitle("Summary of price distributions") +
  facet_wrap(~room_type + neighbourhood_group)
#--------------------------------------------------------------------
# Let's examine the number of listings based on the neighborhood groups

data_abnyc %>% group_by(neighbourhood_group) %>% tally() %>% 
  ggplot(aes(x = reorder(neighbourhood_group, n), n)) +
  geom_bar(stat = "identity", fill = "purple") +
  ggtitle("Number of listings by neighbourhood group") +
  geom_text(aes(x = neighbourhood_group, y = 5000, label = paste0(n)),
                colour = "black", size = 4) + 
  coord_flip()

# The maximum number of listings are in Manhattan, 
# followed by Brooklyn and Queens.
#----------------------------------------------------------------------
# Let's now see if there is some relation between Price and availability 

ggplot(data_abnyc, aes(availability_365, price)) +
  geom_point(alpha = 0.2, color = "slateblue") +
  geom_density(stat = "identity", alpha = 0.2) +
  xlab("Availability during year") +
  ylab("Price") +
  ggtitle("Relationship between availability",
          subtitle = "there is not clear relationship") 

#-----------------------------------------------------------------------
# Plot different neighbourhood groups based on the longitude and latitude values in the data

p<-ggplot(data_abnyc, aes(x=longitude, y=latitude, color=neighbourhood_group)) + 
  geom_point(size=.1) 
p + scale_color_brewer(palette = "Paired")

#---------------------------------------------------------------
# Correlation Matrix
#---------------------------------------------------------------

data_abny_cor <- data_abnyc[, sapply(data_abnyc, is.numeric)]
data_abny_cor<- data_abny_cor[complete.cases(data_abny_cor), ]
correlation_matrix <- cor(data_abny_cor, method = "spearman")
corrplot(correlation_matrix, method = "shade")

#------------------------------------------------------------------
# Supervised Machine Learning Algorithm
#-------------------------------------------------------------------
# Set seed and divide the dataset in training and test dataset

n <- nrow(data_abnyc)           # Number of observations
ntrain <- round(n*0.6)    # 60% for training set
set.seed(300)  
tindex <- sample(n, ntrain)   # Create an index

train_abnyc <- data_abnyc[tindex,]   # Create training set
nrow(train_abnyc)
#Output for number of rows in training set:
#[1] 29337

# During the EDA process we saw that there are some outlying values for prices.
# Also, all the prices apart from these outlying values are below 500$. So to get a better
# model for training we will remove the records that either have price as 0 or have prices with outlying values 

train_abnyc <- train_abnyc %>% filter((price>0) &(price<500))
nrow(train_abnyc)
#Output after removing unwanted data in training set:
#[1] 28605

test_abnyc <- new_auto[-tindex,]   # Create test dataset

#-------------------------------------------------------------------
# Create the first Linear Regression model
#-------------------------------------------------------------------

lm1 <- lm(price ~ latitude + longitude + room_type + minimum_nights  + availability_365 + neighbourhood_group, data = train_abnyc)
summary(lm1)

# Call:
#  lm(formula = price ~ latitude + longitude + room_type + minimum_nights + 
#       availability_365 + neighbourhood_group, data = train_abnyc)
#
# Residuals:
#  Min      1Q  Median      3Q     Max 
# -192.37  -38.45  -11.20   20.19  417.55 
#
# Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                      -2.068e+04  1.193e+03  -17.335  < 2e-16 ***
#  latitude                         -1.104e+02  1.167e+01   -9.461  < 2e-16 ***
#  longitude                        -3.427e+02  1.335e+01  -25.674  < 2e-16 ***
#  room_typePrivate room            -8.668e+01  8.029e-01 -107.966  < 2e-16 ***
#  room_typeShared room             -1.106e+02  2.574e+00  -42.954  < 2e-16 ***
#  minimum_nights                   -1.801e-01  1.832e-02   -9.832  < 2e-16 ***
#  availability_365                  8.241e-02  3.024e-03   27.250  < 2e-16 ***
#  neighbourhood_groupBrooklyn      -1.748e+01  3.224e+00   -5.421 5.98e-08 ***
#  neighbourhood_groupManhattan      2.267e+01  2.916e+00    7.775 7.80e-15 ***
#  neighbourhood_groupQueens         7.475e-01  3.106e+00    0.241     0.81    
#  neighbourhood_groupStaten Island -1.065e+02  6.160e+00  -17.283  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 65.17 on 28594 degrees of freedom
# Multiple R-squared:  0.4194,	Adjusted R-squared:  0.4192 
# F-statistic:  2066 on 10 and 28594 DF,  p-value: < 2.2e-16

#Let's try plotting this linear regression model.

par(mfrow=c(1,1))
plot(lm1)

# None of the plots represent a good linear regression model.

# The Residuals vs Fitted graph doesn't show a linear relationship
# In the the Normal Q-Q graph also, the values are far away from the linear regression line.
# In the Scale-Location graph the values are far more spread on the upper side. The values are not spread equally

# This model is not a good model as the median residual error is -11.20 which is very far away from 0. 
# The value of R-squared is also not good as it is less than 0.5

#-------------------------------------------------------------------------------------------------------
# Create second linear regression model using logrithimic transformationf or price
#--------------------------------------------------------------------------------------------------------

lm2 <- lm(log(price) ~ latitude + longitude + room_type + minimum_nights  + availability_365 + neighbourhood_group, data = train_abnyc)
summary(lm2)

# Call:
#  lm(formula = log(price) ~ latitude + longitude + room_type + 
#       minimum_nights + availability_365 + neighbourhood_group, 
#      data = train_abnyc)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -2.87348 -0.28066 -0.02938  0.24789  2.06662 

# Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                      -1.787e+02  7.768e+00  -23.009  < 2e-16 ***
#  latitude                         -5.078e-01  7.596e-02   -6.685 2.35e-11 ***
#  longitude                        -2.763e+00  8.691e-02  -31.790  < 2e-16 ***
#  room_typePrivate room            -7.230e-01  5.227e-03 -138.303  < 2e-16 ***
#  room_typeShared room             -1.101e+00  1.676e-02  -65.694  < 2e-16 ***
#  minimum_nights                   -1.550e-03  1.193e-04  -12.998  < 2e-16 ***
#  availability_365                  5.429e-04  1.969e-05   27.571  < 2e-16 ***
#  neighbourhood_groupBrooklyn      -2.568e-02  2.099e-02   -1.224    0.221    
#  neighbourhood_groupManhattan      2.563e-01  1.898e-02   13.501  < 2e-16 ***
#  neighbourhood_groupQueens         8.828e-02  2.022e-02    4.366 1.27e-05 ***
#  neighbourhood_groupStaten Island -7.267e-01  4.011e-02  -18.118  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4243 on 28594 degrees of freedom
# Multiple R-squared:  0.5387,	Adjusted R-squared:  0.5385 
# F-statistic:  3339 on 10 and 28594 DF,  p-value: < 2.2e-16

#Let's try plotting this linear regression model.

par(mfrow=c(1,1))
plot(lm2)

# The Residuals vs Fitted graph shows a linear relationship
# The Normal Q-Q graph has most of the values spread along the linear regression line with some outliers.

# This model is much better than the previous model as the median residual error is -0.02938 which is really close to 0.
# Also, the R-sqaured value is 0.5387 which is better than the previous model.

#--------------------------------------------------------------------------------------------------
# Predict values for the test set
#--------------------------------------------------------------------------------------------------

# Remove the outlying values for price from the test dataset as we did from training dataset

test_abnyc <- data_abnyc[-tindex,]   # Create test dataset
nrow(test_abnyc)
#[1] 19558

test_abnyc <- test_abnyc %>% filter((price>0) &(price<500))
nrow(test_abnyc)
#[1] 19044

pred_price <- predict(lm2, newdata=test_abnyc, type="response")
pred_values <- exp(pred_price)

# Calculate the root mean square value for predicted model

RMSE_regression <- sqrt(mean( (test_abnyc$price - pred_values)**2 ))

#sum of squared deviations of predicted values (predicted using regression) from the mean value
SSE <- sum((test_abnyc$price - pred_values)**2)

#sum of squared deviations of actual values from predicted values
SSR <- sum((pred_values - mean(test_abnyc$price)) ** 2)

#Calculate the R-squared value
R2 <- 1 - SSE/(SSE + SSR)

# Create a table for comparing the observed values with predicted values

regression_results <- tibble(
  obs = test_abnyc$price,
  pred = pred_values,
  diff = pred - obs,
  abs_diff = abs(pred - obs),
  neighbourhood = test_abnyc$neighbourhood,
  group = test_abnyc$neighbourhood_group,
  type = test_abnyc$room_type
)

# Let's plot the predicted values against the actual values

regression_plot <- regression_results %>% 
  ggplot(aes(obs, pred)) +
  geom_point(alpha = 0.1, aes(text = paste("\nGroup:", group, "\nType:", type,
                                           "\nPrice diff = ", diff))) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Observed vs predicted price values",
          subtitle = "Linear regression model") + 
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = 2)  +
  facet_wrap(~type)

ggplotly(regression_plot)





















