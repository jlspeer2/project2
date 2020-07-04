ST 558 Project 2
================
Jessica Speer
July 3, 2020

Introduction
------------

In this report, the "Online News Popularity" Data Set is used from the UCI Machine Learning Repository. The data includes variables that describe certain features regarding articles that were published by Mashable over a course of two years. The outcome variable is the number of social media "shares", which is a metric for popularity.

We will be creating two models, a linear regression model and a random forest model. We will use the continuous outcome for the linear regression model, and a binary version for the random forest (&lt; 1400 and ≥ 1400).

Data
----

The variables have been narrowed down in a general program [here](st558proj2.md). The final variables are: `average_token_length`, `kw_avg_avg`, `global_subjectivity`, `avg_negative_polarity`, `self_reference_avg_sharess`, `data_channel_is_entertainment`, `data_channel_is_tech`, `data_channel_is_world`.

Here is a brief description of each variable:

-   `average_token_length`: Average length of the words in the content
-   `kw_avg_avg`: Avg. keyword (avg. shares)
-   `global_subjectivity`: Text subjectivity
-   `avg_negative_polarity`: Avg. polarity of negative words
-   `self_reference_avg_sharess`: Avg. shares of referenced articles in Mashable
-   `data_channel_is_entertainment`: Is data channel 'Entertainment'?
-   `data_channel_is_tech`: Is data channel 'Tech'?
-   `data_channel_is_world`: Is data channel 'World'?

Summarizations
--------------

### Load and Prep the Data.

``` r
#Read in data
data<-read.csv("C:\\Users\\jessi\\Documents\\ST 558\\data\\OnlineNewsPopularity.csv", header=T)
#Generate categorical outcome variable
data$sharescat[data$shares < 1400] <- 0
data$sharescat[data$shares >= 1400] <- 1
data$sharescat <- as.factor(data$sharescat)
#Create single day variable
data$day[data$weekday_is_monday==1] <- "monday"
data$day[data$weekday_is_tuesday==1] <- "tuesday"
data$day[data$weekday_is_wednesday==1] <- "wednesday"
data$day[data$weekday_is_thursday==1] <- "thursday"
data$day[data$weekday_is_friday==1] <- "friday"
data$day[data$weekday_is_saturday==1] <- "saturday"
data$day[data$weekday_is_sunday==1] <- "sunday"
#Filter according to day of week
data<-filter(data, day==params$day)
#Split data into training and test sets
set.seed(1)
train <- sample(1:nrow(data), size = nrow(data)*0.7)
test <- dplyr::setdiff(1:nrow(data), train)
dataTrain <- data[train, ]
dataTest <- data[test, ]
```

### Histograms and Plots (Continuous Vars)

``` r
par(mfrow=c(2,3))
hist(dataTrain$shares, col="dark blue", main="Hist: Shares")
hist(dataTrain$average_token_length, col="dark blue", main="Hist: Avg Token Length")
hist(dataTrain$kw_avg_avg, col="dark blue", main="Hist: Keyword Avg")
hist(dataTrain$global_subjectivity, col="dark blue", main="Hist: Global Subj.")
hist(dataTrain$self_reference_avg_sharess, col="dark blue", main="Hist: Self Ref.")

par(mfrow=c(2,3))
```

![](tuesday_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
plot(dataTrain$shares, col="dark blue", main="Plot: Shares")
plot(dataTrain$average_token_length, col="dark blue", main="Plot: Avg Token Length")
plot(dataTrain$kw_avg_avg, col="dark blue", main="Plot: Keyword Avg")
plot(dataTrain$global_subjectivity, col="dark blue", main="Plot: Global Subj.")
plot(dataTrain$self_reference_avg_sharess, col="dark blue", main="Plot:Self Ref")
```

![](tuesday_files/figure-markdown_github/unnamed-chunk-2-2.png)

### Bar Plots (Binary Vars)

``` r
dataTrain$sharescat<-as.factor(dataTrain$sharescat)
dataTrain$data_channel_is_entertainment<-as.factor(dataTrain$data_channel_is_entertainment)
dataTrain$data_channel_is_tech<-as.factor(dataTrain$data_channel_is_tech)
dataTrain$data_channel_is_world<-as.factor(dataTrain$data_channel_is_world)
sharecounts<-table(dataTrain$sharescat)
entcounts<-table(dataTrain$data_channel_is_entertainment)
techcounts<-table(dataTrain$data_channel_is_tech)
worldcounts<-table(dataTrain$data_channel_is_world)
par(mfrow=c(2,2))
barplot(sharecounts, col="dark blue", main="Shares (Categorical)")
barplot(entcounts, col="dark blue", main="Data Channel: Entertainment")
barplot(techcounts, col="dark blue", main="Data Channel: Tech")
barplot(worldcounts, col="dark blue", main="Data Channel: World")
```

![](tuesday_files/figure-markdown_github/unnamed-chunk-3-1.png)

Models
------

### Linear Regression

``` r
dataTrain$data_channel_is_entertainment<-as.numeric(dataTrain$data_channel_is_entertainment)
dataTrain$data_channel_is_tech<-as.numeric(dataTrain$data_channel_is_tech)
dataTrain$data_channel_is_world<-as.numeric(dataTrain$data_channel_is_world)
#Fit linear regression model on training data
lm_fit<-train(shares ~ average_token_length + kw_avg_avg + global_subjectivity + avg_negative_polarity +
                self_reference_avg_sharess + data_channel_is_entertainment + data_channel_is_tech +
                data_channel_is_world, data=dataTrain, method="lm")

summary(lm_fit)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -25234  -2159  -1376   -402 437207 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    3.879e+03  1.462e+03   2.653  0.00801 ** 
    ## average_token_length          -3.759e+02  2.175e+02  -1.728  0.08400 .  
    ## kw_avg_avg                     6.079e-01  1.187e-01   5.123 3.12e-07 ***
    ## global_subjectivity            3.971e+03  1.666e+03   2.384  0.01714 *  
    ## avg_negative_polarity         -1.894e+02  1.247e+03  -0.152  0.87926    
    ## self_reference_avg_sharess     1.551e-02  5.716e-03   2.713  0.00669 ** 
    ## data_channel_is_entertainment -9.124e+02  4.009e+02  -2.276  0.02288 *  
    ## data_channel_is_tech          -6.002e+02  3.908e+02  -1.536  0.12459    
    ## data_channel_is_world         -8.194e+02  3.995e+02  -2.051  0.04030 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9947 on 5164 degrees of freedom
    ## Multiple R-squared:  0.01636,    Adjusted R-squared:  0.01483 
    ## F-statistic: 10.73 on 8 and 5164 DF,  p-value: 4.331e-15

``` r
#make predictions on test data
test_pred <- predict(lm_fit, newdata = dataTest)
#calculate and plot residuals
residuals<-dataTest$shares - test_pred
summary(abs(residuals))
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##     11.28   3046.08   3841.78   4570.38   4672.88 204478.78

``` r
plot(test_pred, dataTest$shares, col="dark blue")
```

![](tuesday_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
plot(residuals, col="dark blue")
```

![](tuesday_files/figure-markdown_github/unnamed-chunk-4-2.png)

Fit random forest
=================

``` r
set.seed(16)
#set 10-fold CV
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#Fit random forest model on training data
rf_fit <- train(sharescat ~ average_token_length + kw_avg_avg + avg_negative_polarity + self_reference_avg_sharess + data_channel_is_entertainment + data_channel_is_tech + data_channel_is_world, data = dataTrain, method = "rf", trControl=trctrl, preProcess = c("center", "scale"))
rf_fit
```

    ## Random Forest 
    ## 
    ## 5173 samples
    ##    7 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (7), scaled (7) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 4656, 4657, 4655, 4655, 4655, 4656, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##   2     0.6209212  0.2416904
    ##   4     0.6028176  0.2048596
    ##   7     0.6022954  0.2037151
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 2.

``` r
#make predictions on test data
test_pred <- predict(rf_fit, newdata = dataTest)
res <- confusionMatrix(test_pred, dataTest$sharescat)
res
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 581 366
    ##          1 512 758
    ##                                           
    ##                Accuracy : 0.604           
    ##                  95% CI : (0.5833, 0.6244)
    ##     No Information Rate : 0.507           
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.2063          
    ##                                           
    ##  Mcnemar's Test P-Value : 9.905e-07       
    ##                                           
    ##             Sensitivity : 0.5316          
    ##             Specificity : 0.6744          
    ##          Pos Pred Value : 0.6135          
    ##          Neg Pred Value : 0.5969          
    ##              Prevalence : 0.4930          
    ##          Detection Rate : 0.2621          
    ##    Detection Prevalence : 0.4272          
    ##       Balanced Accuracy : 0.6030          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
#misclassification rate
1-sum(diag(res$table))/sum(res$table)
```

    ## [1] 0.3960307
