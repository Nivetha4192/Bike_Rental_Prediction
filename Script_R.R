###############################
#       PROJECT STARTS        #
###############################



###Required Libraries###
library(plyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(grid)
library(corrgram)
library(car)
library(gam)
library(reshape)
library(grid)
library(outliers)
library(e1071)
library(DMwR)
library(tree)
library(MASS)
library(randomForest)




#############################
#     DATA EXPLORATION      #
#############################
rent_df = read.csv('day.csv',header = T) #loading the dataset
str(rent_df) #data structure
dim(rent_df) #data dimension
missing_values <- rent_df %>% 
                  summarize_all(funs(sum(is.na(.))/n())) #missing value analysis
rent_df$instant <- NULL 
rent_df$dteday <- NULL


rent_df_analysis <- rent_df #for analysis and adding new variables





###############################
#   DATA TRANSFORMATIONs      #
###############################

#Outlier Analysis on Required variables
ggplot(stack(rent_df), aes(x = ind, y = values)) +
         geom_boxplot() + coord_flip() +
         ggtitle("Boxplot with Oultiers")

summary(rent_df$casual) #summary of the obtained outliered feature

#Outlier Treatment
x <- rent_df$casual
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
rent_df$casual[rent_df$casual < (qnt[1] - H)] <- caps[1] 
#replacing extreme low whisker with 25% quartile (Flooring)
rent_df$casual[rent_df$casual > (qnt[2] + H)] <- caps[2]
#replacing extreme high whisker with 75% quartile (Capping)

ggplot(stack(rent_df), aes(x = ind, y = values)) +
  geom_boxplot() + coord_flip() +
  ggtitle("Boxplot without Oultiers")

#Creating new variable for day_type
rent_df$day.type=""
rent_df$day.type[rent_df$holiday==0 & rent_df$workingday==0]= 1 #weekend
rent_df$day.type[rent_df$holiday==1]= 2 #holiday
rent_df$day.type[rent_df$holiday==0 & rent_df$workingday==1]= 3 #working day
as.numeric(rent_df$day.type)
rent_df$workingday<- NULL
rent_df$holiday <- NULL


#De-Normalizing temperature and absolute temperature 
#for analysing on temperature scale

#Given,
#normalized  temperature
#t_min = -8, t_max = +39
#n_temp = t-t_min / t_max - t_min
#therefore,
#t = n_temp * (t_max - t_min) + t_min

t_min = -8 #given value
rent_df_analysis$raw_temp <- ((rent_df_analysis$temp * 47) + t_min) #new feature de-norm. temp
#where = t_max - t_min = 39 - (-8)  = 47

at_min = -16  #given value
at_max = 50  #given value
rent_df_analysis$raw_atemp <- ((rent_df_analysis$atemp * (at_max - at_min) + at_min))

rent_df_analysis$raw_humidity <- (rent_df_analysis$hum * 100) #de-norm humidity

#feature for de-normalized windspeed
rent_df_analysis$raw_wind <- (rent_df_analysis$windspeed * 67) #de-norm windspeed 

#Creating new Variable for temperature type
rent_df_analysis$temp_type <- cut(rent_df_analysis$raw_temp,
                                  breaks = c(-Inf,10,20,39),
                                  labels = c("Cold","Moderate","Hot"))
as.numeric(rent_df_analysis$temp_type)
rent_df$temp_type <- rent_df_analysis$temp_type #adding it to the main dataset
rent_df$temp <- NULL #removing them because adding the temperature category
rent_df$atemp <- NULL 







#################################
#     DATA VISUALIZATIONS       #
#################################

#--------Bi-Variate Analysis--------------#



#--------Hypothesis Testing With Visualizations------




#-------------  Hypothesis 1---------------
#Is there relation between the weekdays and bike rentals?
summary(aov(cnt ~ as.factor(weekday), data = rent_df)) 
TukeyHSD(aov(cnt ~ as.factor(weekday), data = rent_df))
#Conclusion: P-value is 0.583, means we cant reject the null hypothesis

#------------------- Plot 1 -----------------------
week_avg <- rent_df %>%
  group_by(weekday) %>%
  summarize(mean_size = mean(cnt,na.rm = T)) 
  as.data.frame %>%
  write.table() #grouping data with weekdays

p1 <- ggplot() + theme_bw() +
  geom_line(aes(y = mean_size, x = weekday), size=1.3, data = week_avg,
            stat="identity",colour = 'deeppink') +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(0,6,1)) +
  labs(x="Days", y="Bike Rents") +
  ggtitle("Daily Rental Average") +
  scale_colour_manual(values=colour) 
p1 #lineplot for daily average rents 






#-------------  Hypothesis 2---------------
#Is there relation between season and bike rentals?
summary(aov(cnt ~ as.factor(season), data = rent_df)) 
TukeyHSD(aov(cnt ~ as.factor(season), data = rent_df))
#Conclusion: P-value < 0.05, means we accept the null hypothesis and theres' relation



#------------------- Plot 2 -----------------------

season_avg <- rent_df %>%
  group_by(season) %>%
  summarize(mean_size = mean(cnt,na.rm = T)) 
  as.data.frame %>%
  write.table() #seasonal data grouping 
    
p2 <- ggplot() + theme_bw() +
  geom_line(aes(y = mean_size, x = season), size=1.3, data = season_avg,
            stat="identity",colour = 'deeppink') +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1,4,1)) +
  labs(x="Seasons", y="Bike Rents") +
  ggtitle("Seasonal Rental Average") +
  scale_colour_manual(values=colour)
p2 #line plots for average monthly rentals 








#-------------  Hypothesis 3---------------
#Is there relation between months and bike rentals?
summary(aov(cnt ~ as.factor(mnth), data = rent_df)) 
TukeyHSD(aov(cnt ~ as.factor(mnth), data = rent_df))
#Conclusion: P-value < 0.05, means we accept the null hypothesis and theres' relation
#------------------- Plot 3 -----------------------

monthly_avg <- rent_df %>%
  group_by(mnth,yr) %>%
  summarize(mean_size = mean(cnt,na.rm = T)) 
  as.data.frame %>%
  write.table() #grouped month and year data

p3 <- ggplot(monthly_avg, aes(x = mnth,y = mean_size,color = as.factor(yr))) +
         geom_smooth(method = "loess", fill = NA, size = 1.5) +
         theme_light(base_size = 10) +
         theme(legend.position="bottom", legend.direction="horizontal",
         legend.title = element_blank()) +
         xlab("Months") +
         ylab("Number of Bike Rentals") +
         ggtitle("Yearwise Average Monthly Rentals") +
         scale_x_continuous(breaks=seq(1,12,1)) +
         theme(plot.title = element_text(size = 11, face="bold"))
p3 #lineplot for average rents per months for year 2011 and 2012








#-------------------Hypothesis 4 ----------
#Is there a relation between the day_type and the rental count?
summary(aov(cnt ~ as.factor(day.type), data = rent_df)) 

#------------------Plot 4----------
daytype_avg <- rent_df %>%
  group_by(day.type,yr) %>%
  summarize(mean_size = mean(cnt,na.rm = T)) 
  as.data.frame %>%
  write.table() #grouped daytypes and years

p4 <- ggplot(daytype_avg, aes(x = as.numeric(day.type),y = mean_size,color = as.factor(yr))) +
    geom_smooth(method = "loess", fill = NA, size = 1.5) +
    theme_light(base_size = 10) +
    theme(legend.position="bottom", legend.direction="horizontal",
          legend.title = element_blank()) +
    xlab("Day Type") +
    ylab("Number of Bike Rentals") +
    ggtitle("Day Type Average Rentals") +
    scale_x_continuous(breaks=seq(1,3,1)) +
    theme(plot.title = element_text(size = 11, face="bold"))
p4 #lineplot for average rents on the basis of dayt for year 2011 and 2012
  














#-------------- Hypothesis 5-----
#Is the temperature related with the bike rentals
t.test(rent_df_analysis$temp, rent_df_analysis$cnt, paired = FALSE)
#Conclusion: The p-value < 0.05 and the null hypothesis is rejected
summary(aov(cnt ~ as.factor(temp_type), data = rent_df_analysis)) 
#------------------Plot 5----------
temptype_avg <- rent_df_analysis %>%
  group_by(temp_type,mnth) %>%
  summarize(mean_size = mean(cnt,na.rm = T)) 
  as.data.frame %>%
  write.table() #grouped daytypes and years

p5 <- ggplot() + theme_bw() +
    geom_line(aes(y = mean_size, x = as.numeric(temp_type)), size=1.3, data = temptype_avg,
              stat="identity",colour = 'deeppink') +
    theme(legend.position="bottom", legend.direction="horizontal",
          legend.title = element_blank()) +
    scale_x_continuous(breaks=seq(1,3,1)) +
    labs(x="Temperature Type", y="Bike Rents") +
    ggtitle("Temperature Rental Average") +
    scale_colour_manual(values=colour)
p5 
  
  

















#-------------- Hypothesis 6-----
#Is the humidity related with the bike rentals
t.test(rent_df$hum,rent_df$cnt, paired = FALSE)
#Conclusion: The p-value < 0.05 and the null hypothesis is rejected
  












#--------------Hypothesis 7--------
#Is there any difference between normal temperature and the feeled temp.
#Two sampled t-tests between normal and feeled temp.
t.test(x = rent_df$temp, y = rent_df$atemp, alternative = "two.sided")
#Conclusion: p-Value < 0.05 and the null hypothesis can't be rejected

#--------------Plot 7------------
hist(rent_df_analysis$raw_temp, yaxt = "n", xaxt = "n", xlab = "",
     ylab = "", main = "Two Sample t-test", 
     xlim = c(5, 40), col = rgb(0, 0, 1, alpha = .1))
text(x = 50, y = 140, paste("Mean real Temp.\n",round(mean(rent_df_analysis$raw_temp), 2), sep = ""), col = "blue")
abline(v = mean(rent_df_analysis$raw_temp), lty = 1,
       col = rgb(0, 0, 1, alpha = 1), lwd = 4)

par(new = T)
hist(rent_df_analysis$raw_atemp, yaxt = "n", xaxt = "n", xlab = "",
     ylab = "", main = "", xlim = c(5, 40), col = rgb(1, 0, 0, alpha = .1))

abline(v = mean(rent_df_analysis$raw_atemp), lty = 1,
       col = rgb(1, 0, 0, alpha = 1), lwd = 4)


mtext(text = "Alternative Hypothesis is confirmed true difference in means is not equal to 0", line = 0, side = 3)
#Plot represents there is no significant mean difference 
#and the null hypothesis can't be rejected












#------------------- Plot 8 -----------------------
#Displaying mean on the plot
grob1 <- grobTree(textGrob(paste0("Mean temp: ",mean(rent_df_analysis$raw_temp)), 
                          x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))


#Histogram plot for temperatures
p6 <- ggplot(rent_df_analysis, aes(x=raw_temp) ) +
      geom_histogram(color = "goldenrod",fill="yellow",binwidth = 1) +
      labs(x = "Temperature in °C",y = "No. of Days")+
      ggtitle("Varying Temperatures") +
      geom_density(alpha=0.6)+
      geom_vline(aes(xintercept=mean(raw_temp)),
                 color="blue", linetype="dashed", size=1)+
      annotation_custom(grob1)
p6




#------------------- Plot 9 -----------------------
#text on plots
grob2 <- grobTree(textGrob(paste0("Mean temp: ",mean(rent_df_analysis$raw_atemp)), 
                           x=0.05,  y=0.95, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))


#Histogram plot for temperatures
p7 <- ggplot(rent_df_analysis, aes(x=raw_atemp))+
  geom_histogram(color = "goldenrod",fill="yellow",binwidth = 1) +
  labs(x = "Temperature in °C",y = "No. of Days")+
  ggtitle("Varying Absolute Temperatures") +
  geom_density(alpha=0.6)+
  geom_vline(aes(xintercept=mean(raw_atemp)),
             color="blue", linetype="dashed", size=1) +
  annotation_custom(grob2)
p7


#------------------- Plot 10 -----------------------
#Text on plot
grob3 <- grobTree(textGrob(paste0("Mean Humidity: ",mean(rent_df_analysis$raw_humidity)), 
                           x=0.020,  y=0.70, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))

#Histogram plot for temperatures with mean
p8 <- ggplot(rent_df_analysis, aes(x=raw_humidity))+
  geom_histogram(color = "goldenrod",fill="yellow",binwidth = 1) +
  labs(x = "Humidity",y = "No. of Days")+
  ggtitle("Varying Humidity Conditions") +
  geom_density(alpha=0.6)+
  geom_vline(aes(xintercept=mean(raw_humidity)),
                 color="blue", linetype="dashed", size=1) +
  annotation_custom(grob3)
p8



#------------------- Plot 11 -----------------------

#No. of registered users in both years
p9 <- ggplot(rent_df,aes(x = as.factor(yr),y = registered)) +
      geom_boxplot(color = "goldenrod2",fill = "yellow") +
      scale_x_discrete(labels =  c(2011,2012))+
      labs(x = "Years", y = "No. of Registered Users") +
      ggtitle("Yearly Index of Registered Users")
p9


#------------------- Plot 12 -----------------------
#No. of casual users in both years
p10 <- ggplot(rent_df,aes(x = as.factor(yr),y = casual)) +
  geom_boxplot(color = "goldenrod2",fill = "yellow") +
  scale_x_discrete(labels =  c(2011,2012))+
  labs(x = "Years", y = "No. of Casual Users") +
  ggtitle("Yearly Index of Casual Users")
p10
#Conclusion: The casual users decreased in 2012 which means the company increase its'
#Users' base

#------------------ Plot 13 -------------------------

#Casual Users in holidays
p11 <- ggplot(rent_df,aes(x = as.factor(holiday),y = casual)) +
  geom_boxplot(color = "goldenrod2",fill = "yellow") +
  scale_x_discrete(labels =  c("Holiday","Not Holiday"))+
  labs(x = "Holidays", y = "No. of Casual Users") +
  ggtitle("Casual Users index in Holidays")
p11
#Conclusion: There are more casual users in the holidays
#------------------ Plot 14 -------------------------

#Casual Users in holidays
p12 <- ggplot(rent_df,aes(x = as.factor(holiday),y = registered)) +
  geom_boxplot(color = "goldenrod2",fill = "yellow") +
  scale_x_discrete(labels =  c("Holiday","Not Holiday"))+
  labs(x = "Holidays", y = "No. of Registered Users") +
  ggtitle("Registered Users index in Holidays")
p12
#Conclusion: High Demand in Holidays for Registered Users

#------------------ Plot 15 -------------------------

#Daily Casual Users
p13 <- ggplot(rent_df,aes(x = as.factor(weekday),y = casual)) +
  geom_boxplot(color = "goldenrod2",fill = "yellow") +
  scale_x_discrete(labels =  c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday",
                               "Sunday"))+
  labs(x = "Weekdays", y = "No. of Casual Users") +
  ggtitle("Daily Casual Users Index")
p13


#------------------ Plot 16 -------------------------

#Daily Casual Users
p14 <- ggplot(rent_df,aes(x = as.factor(weekday),y = registered)) +
  geom_boxplot(color = "goldenrod2",fill = "yellow") +
  scale_x_discrete(labels =  c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday",
                               "Sunday"))+
  labs(x = "Weekdays", y = "No. of Registered Users") +
  ggtitle("Daily Registered Users Index")
p14

#-------------------Plot 17 ------------------------
#correlation plot
corrgram(rent_df, order = F, lower.panel = panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")












#################################
#       MODEL DEVELOPMENT       #
#################################

#-----------model 1----------

#Linear Regression for all the features
model1 <- lm(formula = cnt~ .-registered-casual,rent_df)
summary(model1)
vif(model1, threshold = 4, verbose = TRUE)

#Parameter Tuning

#----------- tuned model ----------
tuned_model <- lm(formula = cnt ~ .-registered-casual-season-mnth, data = rent_df)
summary(tuned_model) #removed variables with VIF > 3         
vif(tuned_model, threshold = 4, verbose = TRUE)

#-----------tuned model 2----------
tuned_model2 <- lm(formula = cnt ~ .-registered-casual-season-mnth-weathersit-hum, data = rent_df)
summary(tuned_model2) #removed variables with VIF > 1.5 
vif(tuned_model2, threshold = 4, verbose = TRUE)

#------model validation-----
#Linear Regression Model Development with model 1 variables as these combination
#gives a good r-squared value than other models
data_lm = rent_df[,c("season","yr","mnth","weekday","weathersit",
                     "hum","windspeed","day.type","temp_type","cnt")]

#Train- Test Splitting
n = nrow(data_lm)
set.seed(1234)
indx <- sample(2,nrow(data_lm),replace = T, prob = c(0.7,0.3))
lm_train <- data_lm[indx == 1,]
lm_test <- data_lm[indx == 2,]


lm_train_model = lm(cnt~.,lm_train)


pred = predict(lm_train_model,lm_test[,-10]) #prediction on the test data

actuals_preds <- data.frame(cbind(actuals=lm_test$cnt, predicteds=pred))
correlation_accuracy <- cor(actuals_preds) #actual vs predicted correlation

correlation_accuracy 
#accuracy = 89.88%

rmse_tain = sqrt(mean(lm_train_model$residuals^2))
rmse_tain

DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)
#rmse_test = 82.03
#mape = 1.97%











#-----------model 2---------
#Decision Tree Regressor
dt_train <- data_lm[indx == 1,]
dt_test <- data_lm[indx == 2,]


#training model
dt_regression = tree(cnt~ .,dt_train)
summary(tree.regression)
plot(dt_regression)
text(dt_regression, pretty=0)

#test predictions
dt_pred = predict(dt_regression,dt_test[,-10]) 


#actual vs predicted correlation
dt_actuals_preds <- data.frame(cbind(actuals=dt_test$cnt, predicteds=dt_pred))
dt_correlation_accuracy <- cor(dt_actuals_preds) 
dt_correlation_accuracy 


#-----------------Model Validation-----
#RMSE 
dt_rmse = sqrt(mean((dt_pred - dt_test[,10])^2))
dt_rmse

DMwR::regr.eval(dt_actuals_preds$actuals, dt_actuals_preds$predicteds)


################################
#         PROJECT ENDS
################################



