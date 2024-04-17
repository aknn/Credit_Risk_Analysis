# *********************************************************************
# CHAPTER 3 EXPLORATORY DATA ANALYSIS
# *********************************************************************

remove(list=ls())

install.packages("moments")
install.packages("gmodels")
install.packages("vcd")
install.packages("mixtools")
library("moments")
library("gmodels")
library("vcd")
library("mixtools")

# for skewness and kurtosis
install.packages("e1071")
library(e1071)

mortgage <-read.csv("mortgage.csv")
attach(mortgage)

# *********************************************************************
# ONE DIMENSIONAL ANALYSIS
# *********************************************************************

# Observed Frequencies and Empirical Distributions
# Initialize vectors
Frequency <- numeric()
Percent <- numeric()
Cum_Frequency <- numeric()
Cum_Percent <- numeric()

total_rows <- nrow(mortgage)  # Total number of rows
total_non_na <- sum(!is.na(mortgage$default_time))  # Total non-NA entries

for(i in seq_along(defaut_indicator)){
  temp <- mortgage[mortgage$default_time == defaut_indicator[i],]
  Frequency[i] <- nrow(temp)
  Percent[i] <- round((Frequency[i] / total_non_na) * 100, 2)  # Calculate based on non-NA only
  
  if (i == 1){
    Cum_Frequency[i] <- Frequency[i]
    Cum_Percent[i] <- Percent[i]
  } else {
    Cum_Frequency[i] <- Cum_Frequency[i-1] + Frequency[i]
    Cum_Percent[i] <- Cum_Percent[i-1] + Percent[i]
  }
}

# Create the results data frame, handling NA in 'defaut_indicator' appropriately
results <- data.frame(defaut_indicator = ifelse(is.na(defaut_indicator), "NA", as.character(defaut_indicator)),
                      Frequency, Percent, Cum_Frequency, Cum_Percent)

# Print results
print(results)

hist(FICO_orig_time, freq = FALSE, breaks = 100, 
     main = "Distr. of Fico at Orig", xlab = "Fico at Orig")

plot.ecdf(FICO_orig_time, main = "Cum. Distr. of Fico at Orig", 
          xlab = "Fico at Orig", ylab = "Cum Prob",pch = ".")

hist(LTV_orig_time, freq = FALSE, breaks = 100, 
     main = "Distr. of LTV at Orig", xlab = "LTV at Orig")
max(LTV_orig_time)
hist(LTV_orig_time, freq = FALSE, breaks = 100, xlim = c(50,110),
     main = "Distr. of LTV at Orig", xlab = "LTV at Orig")

plot.ecdf(LTV_orig_time, main = "Cum. Distr. of LTV at Orig", 
          xlab = "LTV at Orig", ylab = "Cum Prob", verticals= TRUE, pch = ".")

# Location Measures
# Mode Function as R does not have one 
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
proc_means <- function(x) {
  N <- length(na.omit(x))
  Mean <- mean(x, na.rm = TRUE)
  Median <- median(x, na.rm = TRUE)
  Mode <- get_mode(x)
  Pct001 <- quantile(x, 0.01, na.rm = TRUE)
  Pct099 <- quantile(x, 0.99, na.rm = TRUE)
  return(c(N, Mean, Median, Mode, Pct001, Pct099))
}

library(dplyr)

var_names <- c("default_time", "FICO_orig_time", "LTV_orig_time")
loc_measures <- list()

for (var_name in var_names) {
  loc_measures[[var_name]] <- proc_means(mortgage[[var_name]])
}

loc_measures <- bind_rows(loc_measures, .id = "Variable") %>%
  rename_with(~c("Variable", "N", "Mean", "Median", "Mode", "Pct001", "Pct099")) %>%
  mutate(across(c(Mean, Median, Mode, Pct001, Pct099), round, 2))

print(loc_measures)

# Generate a Q-Q plot
qqnorm(mortgage$FICO_orig_time, 
       xlim = c(-6,6), ylim = c(200,1200), 
       main = "QQ plot for FICO at Orig", 
       xlab = "Normal Quantile", ylab = "FICO at Orig")
qqline(mortgage$FICO_orig_time) 

qqnorm(mortgage$LTV_orig_time, 
       xlim = c(-6,6), ylim = c(0,250), 
       main = "QQ plot for LTV at Orig", 
       xlab = "Normal Quantile", ylab = "LTV at Orig")
qqline(mortgage$LTV_orig_time) 


# Dispersion measures
proc_means_ext <- function(x) {
  N <- length(na.omit(x))
  Minimum <- min(x, na.rm = TRUE)
  Maximum <- max(x, na.rm = TRUE)
  Range <- diff(range(x, na.rm = TRUE))
  QuantileRange <- diff(quantile(x, c(0.25, 0.75), na.rm = TRUE))
  Var <- var(x, na.rm = TRUE)
  SD <- sqrt(Var)
  CoeffVar <- SD / mean(x, na.rm = TRUE) * 100
  # Return a named vector
  return(c(N = N, Min = Minimum, Max = Maximum, Range = Range, 
           QuantRange = QuantileRange, Var = Var, SD = SD, CoeffVar = CoeffVar))
}

library(dplyr)

var_names <- c("default_time", "FICO_orig_time", "LTV_orig_time")
disp_measures <- list()

# Loop over each variable name and apply the dispersion measure function
for (var_name in var_names) {
  disp_measures[[var_name]] <- proc_means_ext(mortgage[[var_name]])
}

# Convert the list to a data frame
disp_measures_df <- bind_rows(disp_measures, .id = "Variable")

# Print the data frame
print(disp_measures_df)


# *********************************************************************
# TWO DIMENSIONAL DATA ANALYSIS
# *********************************************************************

# Joint Empirical Distributions
# We create a two-dimensional frequency table
# Creating a factor variable with 5 levels based on quantiles
quantiles <- quantile(mortgage$FICO_orig_time, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
FICO_orig_time_factor <- cut(mortgage$FICO_orig_time, breaks = quantiles, include.lowest = TRUE, labels = FALSE)

# Checking the result
table(FICO_orig_time_factor)  # This will show the distribution across the created categories

# crosstable object is in the library "gmodels"
CrossTable(mortgage$default_time, FICO_orig_time_factor, 
           prop.t = TRUE,prop.r = TRUE, prop.c = TRUE)


boxplot(FICO_orig_time ~ default_time, 
        data = mortgage, 
        range = 0, 
        xlab = "defaut time", ylab = "FICO at Orig Time", 
        main = "Distribution of Fico_orig_time and by default_time")
means <- tapply(mortgage$FICO_orig_time, mortgage$default_time, mean)
points(means, pch = 18)


boxplot(LTV_orig_time ~ default_time, 
        data = mortgage, 
        range = 0, 
        xlab = "defaut time", ylab = "LTV at Orig Time", 
        main = "Distribution of LTV_orig_time and by default_time")
means <- tapply(mortgage$LTV_orig_time, mortgage$default_time, mean)
points(means, pch = 18)

# Correlation Measures
# Chi-sqaure, phi, contingency coeff's and Creamer's V
tab <- xtabs(~ FICO_orig_time_factor + mortgage$default_time)

# assocstats object is in the library("vcd")
assocstats(tab)

# Create a stratified sample equal to 1% of the initial dataset
set.seed(12345)
sample_Fico <- sample(mortgage$FICO_orig_time, size = 0.01*nrow(mortgage), replace = FALSE)
sample_LTV  <- sample(mortgage$LTV_orig_time,  size = 0.01*nrow(mortgage), replace = F)
length(sample_Fico)

# and run various correlation tests.
#cor.test(mortgage$FICO_orig_time,mortgage$LTV_orig_time , method = "pearson")
cor.test(sample_Fico, sample_LTV, method = "pearson")
cor.test(sample_Fico, sample_LTV, method = "spearman", exact = F)
cor.test(sample_Fico, sample_LTV, method = "kendall")

# plot the sample data and overlay a ellipse chart to check the relationship
# ellipse object is in the library("mixtools")
smpl_data <- cbind(sample_Fico, sample_LTV)
plot(smpl_data, xlab = "Fico", ylab = "LTV", main = "Scatter PLot")
ellipse(mu = colMeans(smpl_data), sigma = cov(smpl_data), alpha = 0.1,
        npoints = 250, lwd = 2, col = "blue")
ellipse(mu = colMeans(smpl_data), sigma = cov(smpl_data), alpha = 0.2,
        npoints = 250, lwd = 2, col = "green")
ellipse(mu = colMeans(smpl_data), sigma = cov(smpl_data), alpha = 0.3,
        npoints = 250, lwd = 2, col = "red")
legend(x = "topleft", y.intersp = 0.5, 
       cex = 0.9, title ="prediction elipses",
       legend = c("90%", "80%", "70%"), bty="n", lty = c(1,2,3), lwd=1, horiz = T)

# *********************************************************************
# HIGHLIGHTS OF INDUCTIVE STATISTICS
# *********************************************************************
# Once parameters are estimated, they will not match the true value,
# so now we compute confidence intervalsand Hypothesis testing

# Confidence Intervals
proc_Univariate<- function(x){
  N      <- length(x)
  Mean   <- mean(x, na.rm = TRUE)
  Var    <- var(x,  na.rm = TRUE)
  SD     <- sqrt(Var)
  Lower_Conf<- c(Mean - qnorm(0.99)*SD/sqrt(N))
  Upper_Conf<- c(Mean + qnorm(0.99)*SD/sqrt(N))
  result <-matrix(round(c(Mean,Var,SD,Lower_Conf,Upper_Conf),4),ncol = 5, nrow = 1 )
  colnames(result)  <- c("Mean","Var","SD","Lower_Conf","Upper_Conf")
  print(result)
}

proc_Univariate(mortgage$LTV_orig_time)

# Hypothesis Testing
t.test(mortgage$LTV_orig_time, mu = 60,       alternative = "two.sided")
# We reject the Null Hypothesis H0 : mean = 60, as p-value < 2.2e-16
t.test(mortgage$LTV_orig_time, mu = 78.97546, alternative = "two.sided")
# We cannot reject the Null Hypothesis H0 : mean = 78.97546 as p-value = 1

# *********************************************************************
# Not in the Book, but I personally like to show in a plot
# defaulted vs non defaulted loans. 
# If the pool is too large, I create a subset of it
# *********************************************************************
def  <- mortgage[mortgage$default_time == 1,]
surv <- mortgage[mortgage$default_time == 0,]
nrow(def)
nrow(surv)

plot(def$FICO_orig_time, def$LTV_orig_time, xlab = "Fico", ylab = "LTV", 
     main = "Scatter PLot", col = "red", pch = 18, xlim = c(300, 900))

points(surv$FICO_orig_time, surv$LTV_orig_time, 
       xlab = "Fico", ylab = "LTV", main = "Scatter PLot", col="blue",pch = 1)

legend(x = "topleft", y.intersp = 0.8, 
       cex = 0.8, title ="SubSet (50%) of Mortgage Data",
       legend = c("Defaulted", "No Defauted"), bty="n", col = c("red", "blue"),
       pch = c(18,1),lty)
