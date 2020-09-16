#We load the dataset using the 'import dataset' option in RStudio and then perform 
#all our calculations and solutions

bodytemp <- read.csv("S:/dataset/ASCII Comma/Chapter 11/bodytemp.txt")
#Number of Males in Dataset
n <- 65
#Number of Females in Dataset
m <- 65

#---------------------------------------------------------------------------------

# EXERCISE A 
#-------------
# In this exercise we study if there are differences in body temperatures.

#Storing the body temperatures of males and females in separate variables

#Creating a Variable with male body temperatures
m_bt <- bodytemp[bodytemp$gender==1,]

#Creating a Variable with female body temperatures
f_bt <- bodytemp[bodytemp$gender==2,]


#-----------------------------------------------------------
#Part (i) - Form a 95% confidence interval for the difference
# of mean body temperatures between males and females.
#-----------------------------------------------------------

#Calculating the mean of the body temperatures of each of the data sets
mean_mbt <- mean(m_bt$temperature)
mean_fbt <- mean(f_bt$temperature)

#Calculating the variances of the body temperatures of each of the data sets
var_mbt <- var(m_bt$temperature)
var_fbt <- var(f_bt$temperature)

#Calculating the pooled variance of body temperatures of both males and females together
pooled_var_bt <- pooled_var(m_bt$temperature,f_bt$temperature)

#Calculating the value of t based on alpha and degrees of freedom and 
#storing it in a varibale for reusability. 
#The degrees of freedom = 128 = n+m-2 and a 0.975 interval for alpha = 0.05
t_bt <- qt(0.975,df = 128)

#Calculation of 95% Confidence Interval by recreating formula in R
ci_plus_bt <- (mean_mbt - mean_fbt) + (t_bt * sqrt(pooled_var_bt)* sqrt((n+m)/(n*m)))
ci_minus_bt <- (mean_mbt - mean_fbt) - (t_bt * sqrt(pooled_var_bt)* sqrt((n+m)/(n*m)))

#Alternate two sample t-test solution that directly leverages an inbuilt function and yields similar result to the confidence intervals calculated above.
t.test(bodytemp$temperature ~ bodytemp$gender, data = bodytemp)

#Plotting Normal Probability Plots separately for both the genders for analysis

#Normal Probability Plot for Male Body Temperatures
qqnorm(m_bt$temperature, col = 'green', lwd = 2, main = 'Normal Probability Plot : Male Body Temperature')
#Fitting a reference line to the plot 
qqline(m_bt$temperature)

#Normal Probability Plot for female Body Temperatures
qqnorm(f_bt$temperature, col = 'purple', lwd = 2, main = 'Normal Probability Plot : Female Body Temperature')
#Fitting a reference line to the plot 
qqline(f_bt$temperature)



#-----------------------------------------------------------
#Part (ii) - Use a parametric test to compare the body temperatures
#-----------------------------------------------------------

#We use the below Formula for the calculation of the Test Statistic variable
test_statistic_bt <- (mean_mbt - mean_fbt)/(sqrt(pooled_var_bt)* sqrt((n+m)/(n*m)))

#Performing the pooled two sample t-test to find the exact p-value via inbuilt functions
bt_param <- t.test(bodytemp$temperature ~ bodytemp$gender, data = bodytemp)

#Printing the value of the test statistic. This is an alternative solution to the 
#above formula to automatically calculate the statistic value isntead of the formula created manually above
bt_param$statistic

#Printing the p-value of the parametric test
p_val_param_bt <- bt_param$p.value




#-----------------------------------------------------------
#Part (iii) - Use a nonparametric test to compare the body temperatures
#-----------------------------------------------------------

#Ranking all the values in the data set for the rank sum test
rank_all <- rank(c(m_bt$temperature, f_bt$temperature), ties.method = "average")

#The rank-sum of male body temperatures as they are the first 65 values
rank_sum_m_bt <- sum(rank_all[1:65])

#The rank-sum of female body temperatures as they are the last 65 values
rank_sum_f_bt <- sum(rank_all[66:130])

#Calculation of the rejection region with z = 1.96
rejection_region_bt <- ((n/2)*(n+m+1)) - (1.96 * sqrt((n*m*(n+m+1))/(12)))

#Calculating the p-value for the non-parametric test
bt_nonparam <- wilcox.test(m_bt$temperature, f_bt$temperature, correct = F)

#Printing the p-value of the non parametric test
p_val_nonparam_bt <- bt_nonparam$p.value



#---------------------------------------------------------------------------------

# EXERCISE B 
#-------------
# In this exercise we study if there are differences in heart rates.

#Storing the heart rates of males and females in separate variables

#Creating a Variable with male Heart Rates
m_hr <- bodytemp[bodytemp$gender==1,]

#Creating a Variable with female Heart Rates
f_hr <- bodytemp[bodytemp$gender==2,]


#-----------------------------------------------------------
#Part (i) - Form a 95% confidence interval for the difference
# of mean heart rates between males and females.
#-----------------------------------------------------------

#Calculating the mean of hearts rates of each of the data sets
mean_mhr <- mean(m_hr$rate)
mean_fhr <- mean(f_hr$rate)

#Calculating the variances of heart rates of each of the data sets
var_mhr <- var(m_hr$rate)
var_fhr <- var(f_hr$rate)

#Calculating the degree of freedom using the below formula. (Variables explicitly mentioned in report):
df_hr = (((var_mhr/n)+(var_fhr/m))^2)/((((var_mhr/n)^2)/(n-1)) + (((var_fhr/m)^2)/(m-1)))

#Calculating the value of t based on alpha and degrees of freedom and 
#storing it in a varibale for reusability. 
#The degrees of freedom = 116.7 as calculated above and a 0.975 interval for alpha = 0.05
t_hr <- qt(0.975,df = df_hr)


#Calculation of 95% Confidence Interval by recreating formula in R
ci_plus_hr <- (mean_mhr - mean_fhr) + (t_hr * sqrt((var_mhr/n)+ (var_fhr/m)))
ci_minus_hr <- (mean_mhr - mean_fhr) - (t_hr * sqrt((var_mhr/n)+ (var_fhr/m)))


#Alternate two sample t-test solution that directly leverages an inbuilt function and yields similar result to the confidence intervals calculated above.
t.test(bodytemp$rate ~ bodytemp$gender, data = bodytemp)

#Plotting Normal Probability Plots separately for both the genders for analysis

#Normal Probability Plot for Male heart rates
qqnorm(m_hr$rate, col = 'green', lwd = 2, main = 'Normal Probability Plot : Male Heart Rates')
#Fitting a reference line to the plot 
qqline(m_hr$rate)

#Normal Probability Plot for female heart rates
qqnorm(f_hr$rate, col = 'purple', lwd = 2, main = 'Normal Probability Plot : Female Heart Rates')
#Fitting a reference line to the plot 
qqline(f_hr$rate)



#-----------------------------------------------------------
#Part (ii) - Use a parametric test to compare the heart rates 
#-----------------------------------------------------------

#Formula for the calculation of the Test Statistic
test_statistic_hr <- (mean_mhr - mean_fhr)/(sqrt((var_mhr/n)+ (var_fhr/m)))

#Performing the pooled two sample t-test to find the exact p-value via inbuilt functions
hr_param <- t.test(bodytemp$rate ~ bodytemp$gender, data = bodytemp)

#Printing the value of the test statistic which will be similar to the manually calculated value above
hr_param$statistic

#Printing the p-value of the parametric test
p_val_param_hr <- hr_param$p.value



#-----------------------------------------------------------
#Part (iii) - Use a nonparametric test to compare the heart rates
#-----------------------------------------------------------

#Ranking all the values in the data set for the rank sum test
rank_hr <- rank(c(m_hr$rate, f_hr$rate), ties.method = "average")

#The rank-sum of male body temperatures as they are the first 65 values
rank_sum_m_hr <- sum(rank_hr[1:65])

#The rank-sum of female body temperatures as they are the last 65 values
rank_sum_f_hr <- sum(rank_hr[66:130])

#Calculation of the rejection region
rejection_region_hr <- ((n/2)*(n+m+1)) - (1.96 * sqrt((n*m*(n+m+1))/(12)))

#Calculating the p-value for the non-parametric test
hr_nonparam <- wilcox.test(m_hr$rate, f_hr$rate, correct = F)

#Printing the p-value of the non parametric test
p_val_nonparam_hr <- hr_nonparam$p.value
