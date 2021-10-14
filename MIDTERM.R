Exam #1

--------------------Question #1--------------------
#Form a hypothesis test of the form, “people with various educational qualifications in Region have different fraction vaxxed compared with other Region.”
#choose different ways to operationalize educational qualification (compare above some level with below that level, but what level?) and different regions
#Please provide estimate, standard error, t-stat and a p-value for the hypothesis test and a confidence interval. 
#Write a short paragraph explaining the test (carefully noting what is the null hypothesis) and explaining the results of that test.

I want to test if people with atleast an associates degree in the Northeast have a different fraction vaxxed compared with people in the South. 

Null hypothesis: There is no difference between the fration of people vaxxed in the Northeast who have atleast an associates degree and the fraction of people vaxxed in the South who hold atleast an associates.  
Alternative Hypothesis: There is a difference between the fration of people vaxxed in the Northeast who have atleast an associates degree and the fraction of people vaxxed in the South who hold atleast an associates. 

Significance Level = 0.05
#This means that there is a 5% chance that I can accept the alternative hypothesis when the null hypothesis is actually true. 
#The smaller the significance level, the greater the burden of proof needed to reject the null hypothesis, or in other words, to support the alternative hypothesis.

Recevied vaccination and have atleast an associates: NORTHEAST
mean: 2039
stadard deviation: 1044.27
standard error: 602.909

Received vaccination and have atleast an associates: SOUTH
mean:3973.66667
sd: 1967.404
standard error: 1135.8815

observed difference: -1934.667
Standard Deviation of Difference : 1285.9728

Degrees of Freedom = 3
95% Confidence Interval for the Difference ( -6027.1468 , 2157.8128 )
Test Statistic t = -1.5044 
Population 1 ≠ Population 2: P-Value = 0.2296 
Population 1 < Population 2: P-Value = 0.8852 
Population 1 > Population 2: P-Value = 0.1148 
p-value = .229626

Overall, we can reject the null that there is no difference between the fration of people vaxxed in the Northeast who have atleast an associates degree and the fraction of people vaxxed in the South who hold atleast an associates. 
Having rejected the null hypothesis, we can accept the alternative hypothesis that there is a difference between the fration of people vaxxed in the Northeast who have atleast an associates degree and the fraction of people vaxxed in the South who hold atleast an associates.

--------------------Question #2--------------------
#Form a hypothesis test of the form, “people with various educational qualifications who are one or more gender ID have different fraction vaxxed compared with another gender ID.”

I want to test if people with atleast a bachelors degree and are female will have a different fraction vaxxed than people with atleast a bachelors and identify as transgender. 

Null hypothesis:There is no difference in the fraction of people vaxxed between people with a bachelors degree who are female and people with atleast a bachelors and are transgender. 
Alternative Hypothesis: There is a difference in the fraction of people vaxxed between people with a bachelors degree who are female and people with a bachelors and are transgender.

Significance Level = 0.05
#This means that there is a 5% chance that I can accept the alternative hypothesis when the null hypothesis is actually true. 
#The smaller the significance level, the greater the burden of proof needed to reject the null hypothesis, or in other words, to support the alternative hypothesis.

fraction of people vaxxed who have at least a bachelors and are female:
  mean: 8939
stadard deviation:586.898
standard error: 415

fraction of people vaxxed who have at least a bachelors and are transgender:
  mean: 35
stadard deviation: 9.8994
standard error: 7


observed difference: 8904

95% Confidence Interval for the Difference (-93763.42,93941.42)
Test Statistic t = 0.0121 
Population 1 ≠ Population 2: P-Value = 0.9922 
p-value = .991444

Overall, the p-value is not signifcant at our level of signifciance: 0.05, so we reject the null hypothesis and accept the alternative hypothesis:
  That there is a difference in the fraction of people vaxxed between people with a bachelors degree who are female and people with a bachelors and are transgender.
--------------------Question #3--------------------

load("/Users/mahrukh.jaura/Downloads/Household_Pulse_data.RData")
attach(Household_Pulse_data)

I think it would be interesting to compare the vaccination rate between people who eat out at restaurants and have private health insurance versus people who have had Covid and are not vaccinated yet. 
#Choose a subgroup of the sample to consider and provide summary statistics of that subgroup. Explain why this subgroup is interesting.

Some cool figures I came across as I was trying to figure out how to create a subset:
  summary(EEDUC)
less than hs      some hs   HS diploma    some coll    assoc deg     bach deg      adv deg 
411          936         7857        14596         7508        20075        17731 
summary(PRIVHLTH)
has private health ins  no private health ins                     NA 
46869                  11275                  10970 
summary(eat_in_restaurant)
NA eat at restaurant indoors                        no 
7217                     32405                     29492
summary(RENTCUR)
NA current on rent  behind on rent 
56572           11239            1303 
summary(GENID_DESCRIBE)
NA        male      female transgender       other 
1131       26796       40263         202         722 

Honestly, I am not sure how to create a subset for this particular group but I have been trying variations of the following: 
  Private_H_Restaurant <- subset(Household_Pulse_data, (Household_Pulse_data$PRIVHLTH)  & (Household_Pulse_data$eat_in_restaurant)
                                 If this subset worked, I think it would be interesting because during Covid times, the number of people who eat out at restaurants has decreased
                                 significantly and I would be interested in knowing whether having private health insurance makes a difference in a persons decision to eat out. 
                                 
                                 Form a hypothesis test about an interesting variable, explore whether your chosen subgroup differs from the rest of sample.
                                 To form the hypothesis test, I would use the following code
                                 model_temp1 <- lm(INCOME ~ AGE + White + Black + Hispanic +NotHispanic + EEDUC + eat_in_restaurant + PRIVHLTH)
                                 summary(model_temp1)
                                 confint(model_temp1)
                                 
                                 This regression would give me the variables that are significant, which I would be able to decipher if the p-value falls within the
                                 confidence intervals. With that information, I would be able to reject or fail to reject the null hypothesis and establish whether there
                                 is any connection between my dependent and independent variables. 
                                 
                                 Using a k-nn classifier, can you find relevant information to predict an interesting outcome? 
                                   I would like to predict what state someone is from!
                                   
                                   State_predict <- factor((EST_ST), levels=c(1),labels = c("states"))
                                 norm_varb <- function(X_in) {
                                   (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
                                 }
                                 ----------List of variables I will use to try and predict commute----------
                                   
                                   norm_PRIVHLTH <- norm_varb(PRIVHLTH)
                                 norm_eat_in_restaurant <- norm_varb(eat_in_restaurant)
                                 norm_PUBHLTH <- norm_varb(PUBHLTH)
                                 norm_PUBHLTH <- norm_varb(INCOME)
                                 norm_CURFOODSUF <- norm_varb(CURFOODSUF)
                                 
                                 data_use_prelim <- data.frame(norm_PRIVHLTH,norm_eat_in_restaurant, norm_PUBHLTH, norm_INCOME, norm_CURFOODSUF)
                                 good_obs_data_use <- complete.cases(data_use_prelim,State_predict)
                                 dat_use <- subset(data_use_prelim,good_obs_data_use)
                                 y_use <- subset(state_predict,good_obs_data_use)
                                 
                                 set.seed(12345)
                                 NN_obs <- sum(good_obs_data_use == 1)
                                 select1 <- (runif(NN_obs) < 0.9)
                                 train_data <- subset(dat_use,select1)
                                 test_data <- subset(dat_use,(!select1))
                                 cl_data <- y_use[select1]
                                 true_data <- y_use[!select1]
                                 
                                 summary(cl_data)
                                 prop.table(summary(cl_data))
                                 summary(train_data)
                                 
                                 require(class)
                                 for (indx in seq(1, 9, by= 2)) {
                                   pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
                                   num_correct_labels <- sum(pred_borough == true_data)
                                   correct_rate <- num_correct_labels/length(true_data)
                                   print(c(indx,correct_rate))
                                 }
                                 
                                 
                                 Mahrukh Jaura
                                 October 14th, 2021
                                 
                                 