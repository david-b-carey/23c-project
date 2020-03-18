# Partners: Jerry Yang, Mike Bocker, and David Carey

# Point #1: A .csv file with the dataset.
# Point #2: A long, well-commented script that loads the dataset, explores it, and
# does all the analysis.
# Point #3: A shorter .Rmd with compiled .pdf or .html file that presents highlights
# in ten minutes.
# Point #4: A one-page handout that explains the dataset and summarizes the analysis.

# The following analysis concerns a dataset on annual expenditures in Vietnamese
# households of varying demographic characteristics. The dataset
# was retrieved from https://vincentarelbundock.github.io/Rdatasets/datasets.html and
# is described as "Medical Expenses in Viet-nam (household Level)".

# If not yet downloaded, download the ggplot2 library by running the following line,
# which is commented out:
# install.packages("ggplot2")
# Load up the ggplot2 library
library("ggplot2")

# Similarly, download/load up the stats4 library with the following lines:
# install.packages("stats4")
library("stats4")

# Load the .csv file into a data frame.
df <- read.csv("VietNamH.csv"); head(df);  nrow(df)
# Point #5: A dataframe.
# Point #6: At least two categorical or logical columns.
# Point #7: At least two numeric columns.
# Point #8: At least 20 rows.
# Point #9: A data set with lots of columns, allowing comparison of many different
# variables.
# Point #10: A data set that is so large that it can be used as a population from which
# samples are taken.

# We first check the data frame for missing values.
sapply(df, function(x) all(sum((is.na(x))) == 0))

# This shows that the lnexp12m column is the only column with NA entries, so we examine
# this. Extracting the rows with such values, we see that they also always have 0 in the
# lnmed column.
na_rows <- df[is.na(df$lnexp12m),]; head(na_rows)
sum (na_rows$lnmed)  == 0

# In fact, we see that every entry has the same value for these columns.
nrow(df[(df$lnmed == df$lnexp12m),]) == nrow(df)

# Hence, we decide to assign a value of 0 to all NA values.
df[is.na(df)] <- 0; head(df); nrow(df)

# One of our primary interests with this dataset is whether any demographic variables
# correlate with the ratio between a household’s medical expenditure and total spending,
# as a possible indicator of things like disproportionate financial stress. Hence, we
# create a vector for this ratio. Note that since the dataset stores various expenditures
# as their natural log, dividing these columns will not give us the desired proportion,
# but rather resembles the change of base formula. Instead, we take the exponential of
# the two columns and then divide them.
ratio_mt <- exp(df$lnmed) / exp(df$lntotal); head(ratio_mt)

# First, let's observe the histogram of this ratio.
hist(ratio_mt, breaks = "Sturges", freq = NULL)
# Point #11: A histogram.

# Note that some of these values are greater than 1, meaning that a household’s medical
# expenditure is greater than its total expenditure. That is no good! For the purposes of this 
# analysis, the total expenditure should be at least equal to the sum of food and medical 
# expenditures, and likely greater.

# Hence, we remove the 286 rows for which the sum of the food and medical
# expenditures exceeds total household expenditure. Much of our analysis to follow
# deals with the ratio of medical expenditure to total expenditure, and rows such as
# these serve as stark outliers to the data we observe. Fortunately, just 286 out of
# 5999 of rows have this odd property, so removing them results in a relatively
# negligible decrease in sample size..
df <- df[exp(df$lnmed) + exp(df$lnrlfood) < exp(df$lntotal),]
head(df); nrow(df)

# Now that we have a dataframe primed for analysis, we can extract the columns,
# adding additional columns for the exponential of the numeric columns with logs.

# Extract the columns, adding additional columns for the exponential of the numeric columns with logs.
# Gender of household head:
sex <- df$sex; head(sex)
# Age of household head:
age <- df$age; head(age)
# Number of years of schooling of household head:
educyr <- df$educyr; head(educyr)
# Logical column for whether household is a farm:
farm <- df$farm; head(farm)
# Logical column for whether household is urban:
urban <- df$urban; head(urban)
# Number of people in household:
hhsize <- df$hhsize; head(hhsize)
# Log of household total expenditure:
lntotal <- df$lntotal; head(lntotal)
# Household total expenditure:
HTE <- exp(df$lntotal); head(HTE)
# Log of household medical expenditure:
lnmed <- df$lnmed; head(lnmed)
# Household medical expenditure:
HME <- exp(df$lnmed); head(HME)
# Log of household food expenditure:
lnrlfood <- df$lnrlfood; head(lnrlfood)
# Household food expenditure:
HFE <- exp(df$lnrlfood); head(HFE)
# Identical to log of household total expenditure:
lnexp12m <- df$lnexp12m; head(lnexp12m)
# Commune number of participant's home:
commune <- df$commune; head(commune)

# It will also be helpful to extract two-level categorical columns (e.g. “yes”/ “no”)
# as logical vectors.
# True/1 for female:
isfemale <- (as.numeric(sex == "female")); head(isfemale)
# True/1 for farm:
hasfarm <- (as.numeric(farm == "yes")); head(hasfarm)
# True/1 for urban:
isurban <- (as.numeric(urban == "yes")); head(isurban)

# In addition, let's make a new vector for the ratio of medical to total expenditure
# now that our dataset is appropriately-adjusted.
ratio_mt <- HME / HTE; head(ratio_mt)

# To begin, we create a histogram of total expenditures (NOT ln of total expenditures).
hist(HTE, breaks = 'fd', freq = FALSE, col = 'gray')
# By inspection, we observe that this histogram appears to resemble a gamma
# distribution and quickly derive parameters that produce a good fit.
curve(dgamma(x, 3.6, rate = 0.0003),  col = 'red', lwd = 3, add = TRUE)
# Point # 12: A probability density graph overlaid on a histogram.

# When we attempted to test the accuracy of these parameters with chi square, we ran
# into an error with this code:
Func_Expec <- function(x) x*dgamma(x, shape = 3.6, rate = 0.0003) 
Expected <- integrate(Func_Expec, lower = 0, upper = Inf); Expected
Func_Var <- function(x) ((x-Expected)^2)*dgamma(x, shape = 3.6, rate = 0.0003)
variance <- integrate(Func_Var, lower = 0, upper = Inf); variance
# We were unable to integrate to find the expected value and variance of the
# distribution given these parameters. After conducting research into the error, it was
# determined that the integration most likely diverges and is therefore not a good fit
# for household total expenditure data, the expected value/mean of which should be:
mean(HTE) # 14817.26
# Point #13: Appropriate use of integration to calculate a significant result.
# Point #14: Appropriate use of R functions for a probability distribution other than
# binomial, normal, or chi-square.

# Next, we create histogram of the natural log of total expenditures
hist(lntotal, breaks = 'fd', freq = FALSE, col = 'gray')
# This resembles a normal distribution
mu <- mean(lntotal); mu
sigma <- sd(lntotal);sigma
curve(dnorm(x, mu, sigma), add = TRUE, col = 'red', lwd = 3)
# Now we can use the central limit theorem to test parameters of this normal distribution.
n <- 50 # Sample size.
N <- 1000; xbars <- numeric(N) # N is the number of samples we take
for (i in 1:N) xbars[i] <- mean(sample(lntotal,n))
# Plot a histogram of the sample means
hist(xbars, breaks = "FD", probability = TRUE, col = 'magenta') 
# Calculate the mean and standard deviation of the sample mean
mu2 <- mean(xbars); mu2; mu # The mean of the sample means is accurate to two
                            # decimal places.
sigma2 <- sd(xbars); sigma2
# Overlay a normal distribution function with these parameters.
curve(dnorm(x, mu2, sigma2),  add = TRUE, col = "black", lwd = 3) 
# Now we want to create a confidence interval for our sample means to show the variance
# of the sample means from our true mean.
x.add <- qnorm(0.975)*(sigma2); x.add 
L <- mu2 - x.add; L
U <- mu2 + x.add; U
curve(dnorm(x,mu2,sigma2), from = 8.9, to = 9.8)
abline(v = c(mu2-x.add,mu2+x.add), col = "red")
abline(v = mu, col = 'blue')
# Now we can take the difference between the true mean and U and L to show that we are
# 95% confident that the sample mean will be within this difference of the true mean.
MaxUpperdiff <- abs(U - mu); MaxUpperdiff
MaxLowerdiff <- abs(L - mu); MaxLowerdiff
# Thus we we can say with 95% confidence that our sample mean will be within
# approximately 0.19... of our true mean, 9.341561. This shows that the mean of our
# data is consistent with CLT and we can assume it is correct. We can also test the
# goodness of fit, by using the built in function qqnorm. If the points in our Q-Q plot
# fall seemingly along a straight line, we know that the normal distribution function
# is a good fit for our data.
qqnorm(lntotal) # And we know that the normal distribution is a good fit for our
                # lntotal data.
# Point #15: Calculation of a confidence interval.
# Point #16: Comparison of analysis by classical methods (chi-square, CLT) and
# simulation methods.

# From these two results, we assume that the purpose of taking the natural log of
# the expenditures was to make the data easier to model. However, for the purpose
# of our project we do not want to use the natural log. This is because we wish to
# analyze the ratio of expenditures and if we were to use the natural log of these
# columns, we would end up with inaccurate results. Thus, we will take the exponent
# of the expenditure columns to calculate the ratio of medical expenditures to total
# expenditures. Additionally, because we will be using the ratio, we will be reducing
# the range of values similar to what the natural log was functioning to do for the
# expenditures. 

# Now we turn to looking for a correlation between the expenditure ratio and demographic
# variables. For some preliminary analysis, we look at the quantitative data (household
# size, age of the household head, and years of education of the household head) and
# attempt to fit a linear regression model to the ratio of medical expenditures to total
# expenditures.

# Using the multiple linear regression model built into R, we will analyze the variables
# household size, age, and years of education as independent variables against the ratio
# of medical expenditures to total expenditures of each household as the dependent
# variable.
regression <- lm(ratio_mt~age+educyr+hhsize);regression
summary(regression)
# The r-squared value of this regression is extremely small, and after plotting a
# scatter plot of the ratio of medical expenditures to total expenditures data, it is
# obvious that this data is not anywhere close to linear, and we therefore cannot use
# a linear regression model for this data.

# We can also see this if we plot the ratio of medical to total expenditures against
# each of the independent variables.
plot(age,ratio_mt)
plot(educyr, ratio_mt)
plot(hhsize, ratio_mt)
# As we suspected, it would not make sense to use regression analysis on this data. 
# Point # 17: Use of linear regression.

# We now turn to looking for a correlation between the expenditure ratio and demographic
# variables. Since we have many data points, a contingency table with the probabilities
# of different conditions is a good starting point. Note that we want the probabilities
# of each column to sum to 1, since otherwise a significant difference in the count of
# different conditions will make relationships hard to discern.

# Construct a function for building contingency tables (with probabilities):
ct_func <- function(fst, snd) {
  prop.table (table (fst, snd), margin = 2) * 100
}

# As a start, we separate data points into those with a medical spending ratio greater
# than average and otherwise.

highratio <- ratio_mt >= mean(ratio_mt)
# On initial glance, there does not seem to be a correlation, as the conditions do not
# seem to impact the percentage of high ratio households.
ct_func (isfemale, highratio)
ct_func (isurban, highratio)
ct_func (hasfarm, highratio)

# However, it is worth noting that at least the urban/farm conditions have discrepate
# numbers of households with expenditures greater than average.
# Point #18: A contingency table.

# Since the ratio of medical expenditure appears to relate to total expenditure, this
# warrants further investigation.
highspender <- HTE >= mean (HTE)
ct_func (isurban, highspender)
ct_func (hasfarm, highspender)
ct_func (isfemale, highspender)
c4 <- ct_func (urban, isfemale); c4 # A lot higher percentage of female household heads in
                                  # urban environment.
c5 <- ct_func (farm, isfemale); c5 # In this case as well. 
# Point #19: Analysis of a contingency table.
# Point #20: Professional-looking software engineering (e.g defining and using your
# own functions).

# Let’s calculate the averages using this function.
meandiff <- function (c1, cNum) {
  AvgT <- sum(cNum*(c1 == TRUE))/sum(c1 == TRUE); AvgT
  AvgF <- sum(cNum*(c1 == FALSE))/sum(c1 == FALSE); AvgF
  ObservedDiff <- AvgT - AvgF
}

m1 <- meandiff (hasfarm, ratio_mt); m1
m2 <- meandiff (isfemale, ratio_mt); m2
m3 <- meandiff (isurban, ratio_mt); m3

# Note that there is a difference here, though it seems rather insignificant. Let’s
# look deeper with a permutation test, defined in the function below.
permu_func <- function (c1, cNum, right_tail = FALSE) {
  # Calculate the observed difference
  AvgT <- sum(cNum*(c1 == TRUE))/sum(c1 == TRUE); AvgT
  AvgF <- sum(cNum*(c1 == FALSE))/sum(c1 == FALSE); AvgF
  ObservedDiff <- AvgT - AvgF
  # To investigate this difference further, we will conduct a one-sided p test.
  # Construct a permuted T/F column for cLog.
  cLog <- sample(c1)
  # Replace with a random sample and calculate the new observed difference.
  # The result should be as likely to be positive/negative.
  RAvgT <- sum(cNum*(cLog == TRUE))/sum(cLog == TRUE); RAvgT
  RAvgF <- sum(cNum*(cLog == FALSE))/sum(cLog == FALSE); RAvgF
  RObservedDiff <- RAvgT - RAvgF
  # Repeat this process 1000 times. Note that this process should take a few moments.
  N <- 50000
  Diffs <- numeric(N)
  for (i in 1:N){
    cLogPermuted <- sample(c1)
    RAvgT <- sum(cNum*(cLogPermuted == TRUE))/sum(cLogPermuted == TRUE)
    RAvgF <- sum(cNum*(cLogPermuted == FALSE))/sum(cLogPermuted == FALSE)
    Diffs[i] <- RAvgT - RAvgF
  }
  # Take the average of the differences with the random samples. This should be extremely close to 0.
  AvgDiffs <- mean(Diffs)
  # Return the p-value for a two-sided test.
  if (right_tail == TRUE) {return ((sum(Diffs <= ObservedDiff) + 1)/(N + 1))}
  else {return ((sum(Diffs >= ObservedDiff) + 1)/(N + 1))}
}

# Applying this to the concerned relationships, we actually get significant p-values.
# Note that this process takes a few moments.

# Left tail test since the observed difference in mean is positive.
permu_func (hasfarm, ratio_mt) # p-value significantly less than 0.05
# Left tail test since the observed difference in mean is positive.
permu_func (isfemale, ratio_mt) # p-value significantly less than 0.05
# Right tail test since the observed difference in mean is negative.
permu_func (isurban, ratio_mt, right_tail = TRUE) # p-value significantly less than 0.05
# Point #21: A permutation test.

# To test the results of the permutation tests, we conduct chi square tests for each
# relationship.
# Conduct a chi square test for urban, farm, and sex vs the proportion of medical to
# total expenditures. The ratio needs to be in the form of categorical data, so we
# will use the "highratio" that we have previously defined.
# Use the chi square function to compare urban and farm households to the ratio of
# medical expenditures.
urbandata <- table(isurban, highratio);urbandata
chisq.test(urbandata)
# p-value is significanlty less than 0.05
# Point #22: A p-value or other statistic based on a distribution function.
farmdata <- table(hasfarm, highratio);farmdata
chisq.test(farmdata)
# p-value is significantly less than 0.05
femaleata <- table(isfemale, highratio);femaleata
chisq.test(femaleata)
# p-value is less than 0.05 
# These p-values confirm the results of the permutation test and further support the
# hypothesis that the ratio medical expenditures to total expenditures is not
# independent of the farm, urban, and sex of household head variables.
# Point #23: Use of theoretical knowledge of chi-square, gamma, or beta distributions.

# We can also find the covariance matrix.
df_cov <- df
df_cov$sex <- isfemale
df_cov$farm <- hasfarm
df_cov$urban <- isurban
df_cov
cor(df_cov)
# Point #24: Appropriate use of covariance or correlation.

# But, could there still be a relationship between medical/total expenditure and other
# columns, such as education?
# Let’s construct a barplot representing the average of the medical/total expenditure 
# ratio of the rows in bins of education years for household heads:
EducYrsToRatioMT <- data.frame("EducYrs" = c("01 - 04", "05 - 08", "09 - 12",
                                             "13 - 16", "17+"),
                               "RatioMT" =
                                 c(mean(ratio_mt[educyr < 5]),
                                   mean(ratio_mt[educyr > 4 & educyr < 9]),
                                   mean(ratio_mt[educyr > 8 & educyr < 13]),
                                   mean(ratio_mt[educyr > 12 & educyr < 17]),
                                   mean(ratio_mt[educyr > 16])))
ggplot(data=EducYrsToRatioMT, aes(x=EducYrs, y=RatioMT)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue")
# As greater amounts of education are associated with lower medical/total expenditure
# ratio averages, we have reason to believe that education, and perhaps other
# factors, may relate to this ratio.
# Point #25: Nicely labeled graphics using ggplot, with good use of color, line styles,
# etc., that tell a convincing story.

# Construct a function for constructing visually appealing boxplots side-by-side:
bPlotBuilder <- function (cat, num) {
  BuiltPlot <- ggplot(df, aes(x = cat, y = num)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                 outlier.size = 1)
  return(BuiltPlot)
}

# Construct side by side boxplots for male and female medical/total expenditure ratios
bPlotBuilder (sex, ratio_mt) + xlab("sex") + ylab("ratio_mt")
# Construct side by side boxplots for farm/no farm medical/total expenditure ratios
bPlotBuilder (farm, ratio_mt) + xlab("farm") + ylab("ratio_mt")
# Construct side by side boxplots for urban/not urban medical/total expenditure ratios
bPlotBuilder (urban, ratio_mt) + xlab("urban") + ylab("ratio_mt")
# Point #26: A graphical display that is different from those in the class scripts.

# We can also do logistic regression.
linear_regression <- function (c1, c2) {
  MLL<- function(alpha, beta) {
    -sum( log( exp(alpha+beta*c1)/(1+exp(alpha+beta*c1)))*c2
          + log(1/(1+exp(alpha+beta*c1)))*(1-c2))
  }
  
  # Run the model with an initial guess.
  results<-mle(MLL, start = list(alpha = 0, beta = 0))
  results@coef
  
  # Plot our results.
  plot(c1, c2)
  curve(exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
}
linear_regression (exp(lntotal), isfemale)
linear_regression (exp(lnmed), isfemale)
linear_regression (lnmed, isfemale)
linear_regression (lntotal, isfemale)
linear_regression (lnmed/lntotal, isfemale)
linear_regression (lnmed/lntotal, isurban)
linear_regression (lnmed, isurban)
linear_regression (lntotal, isurban) 
# Can actually fit a logistical regression curve to this that looks better. However,
# this is not true when we take the exponential of lntotal. 
linear_regression (lntotal, isurban) 
# All in all, there is nothing particular to note from these regressions.

# What if financial circumstances affect our observations on these relationships to
# the ratio between medical expenditure and total expenditure?

# Let's divide up the data into five bins of increasing non-medical expenditure
# per capita.
quant <- quantile ((HTE-HME)/hhsize, probs = seq(0, 1, 0.2))
bin1 <- df[(HTE-HME)/hhsize < quant[2] & (HTE-HME)/hhsize >= quant[1],]; head(bin1)
bin2 <- df[(HTE-HME)/hhsize < quant[3] & (HTE-HME)/hhsize >= quant[2],]; head(bin2)
bin3 <- df[(HTE-HME)/hhsize < quant[4] & (HTE-HME)/hhsize >= quant[3],]; head(bin3)
bin4 <- df[(HTE-HME)/hhsize < quant[5] & (HTE-HME)/hhsize >= quant[4],]; head(bin4)
bin5 <- df[(HTE-HME)/hhsize < quant[6] & (HTE-HME)/hhsize >= quant[5],]; head(bin5)

# First, let's consider the relationship between being female and the ratio
# between medical expenditure and total expenditure in each bin. We will do
# left tail tests to adhere to our previous observations.
ma <- meandiff (as.numeric(bin1$sex == "female"), exp(bin1$lnmed)/exp(bin1$lntotal)); ma
permu_func (as.numeric(bin1$sex == "female"), exp(bin1$lnmed)/exp(bin1$lntotal))
mb <- meandiff (as.numeric(bin2$sex == "female"), exp(bin2$lnmed)/exp(bin2$lntotal)); mb
permu_func (as.numeric(bin2$sex == "female"), exp(bin2$lnmed)/exp(bin2$lntotal))
mc <- meandiff (as.numeric(bin3$sex == "female"), exp(bin3$lnmed)/exp(bin3$lntotal)); mc
permu_func (as.numeric(bin3$sex == "female"), exp(bin3$lnmed)/exp(bin3$lntotal))
md <- meandiff (as.numeric(bin4$sex == "female"), exp(bin4$lnmed)/exp(bin4$lntotal)); md
permu_func (as.numeric(bin4$sex == "female"), exp(bin4$lnmed)/exp(bin4$lntotal), right_tail = TRUE)
me <- meandiff (as.numeric(bin5$sex == "female"), exp(bin5$lnmed)/exp(bin5$lntotal)); me
permu_func (as.numeric(bin5$sex == "female"), exp(bin5$lnmed)/exp(bin5$lntotal))
# We see that there is statistcal significance for the first three bins and the
# final bin. For the fourth bin, since the mean difference was negative, we did a
# right tail test instead, but the difference in this case was not significant.


# Next, let's consider the relationship between living in an urban area and the ratio
# between medical expenditure and total expenditure.
mf <- meandiff (as.numeric(bin1$urban == "yes"), exp(bin1$lnmed)/exp(bin1$lntotal)); mf
permu_func (as.numeric(bin1$urban == "yes"), exp(bin1$lnmed)/exp(bin1$lntotal))
mg <- meandiff (as.numeric(bin2$urban == "yes"), exp(bin2$lnmed)/exp(bin2$lntotal)); mg
permu_func (as.numeric(bin2$urban == "yes"), exp(bin2$lnmed)/exp(bin2$lntotal))
mh <- meandiff (as.numeric(bin3$urban == "yes"), exp(bin3$lnmed)/exp(bin3$lntotal)); mh
permu_func (as.numeric(bin3$urban == "yes"), exp(bin3$lnmed)/exp(bin3$lntotal))
mi <- meandiff (as.numeric(bin4$urban == "yes"), exp(bin4$lnmed)/exp(bin4$lntotal)); mi
permu_func (as.numeric(bin4$urban == "yes"), exp(bin4$lnmed)/exp(bin4$lntotal))
mj <- meandiff (as.numeric(bin5$urban == "yes"), exp(bin5$lnmed)/exp(bin5$lntotal)); mj
permu_func (as.numeric(bin5$urban == "yes"), exp(bin5$lnmed)/exp(bin5$lntotal))
# Doing all left tail tests, we see that there is statistical significance for the
# first three bins, but not for the fourth and fifth, which correlate to high
# non-medical expenditure per capita.

# Finally, let's consider the relationship between living on a farm and the ratio
# between medical expenditure and total expenditure in each bin.
mk <- meandiff (as.numeric(bin1$farm == "yes"), exp(bin1$lnmed)/exp(bin1$lntotal)); mk
permu_func (as.numeric(bin1$farm == "yes"), exp(bin1$lnmed)/exp(bin1$lntotal), right_tail = TRUE)
ml <- meandiff (as.numeric(bin2$farm == "yes"), exp(bin2$lnmed)/exp(bin2$lntotal)); ml
permu_func (as.numeric(bin2$farm == "yes"), exp(bin2$lnmed)/exp(bin2$lntotal), right_tail = TRUE)
mm <- meandiff (as.numeric(bin3$farm == "yes"), exp(bin3$lnmed)/exp(bin3$lntotal)); mm
permu_func (as.numeric(bin3$farm == "yes"), exp(bin3$lnmed)/exp(bin3$lntotal), right_tail = TRUE)
mn <- meandiff (as.numeric(bin4$farm == "yes"), exp(bin4$lnmed)/exp(bin4$lntotal)); mn
permu_func (as.numeric(bin4$farm == "yes"), exp(bin4$lnmed)/exp(bin4$lntotal))
mo <- meandiff (as.numeric(bin5$farm == "yes"), exp(bin5$lnmed)/exp(bin5$lntotal)); mo
permu_func (as.numeric(bin5$farm == "yes"), exp(bin5$lnmed)/exp(bin5$lntotal), right_tail = TRUE)
# Doing a right tail test for the fourth bin and a left tail test for the others, we
# only find statistically significant results for the first three bins.
# Point #27: Appropriate use of quantiles to compare distributions.