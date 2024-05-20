#Steveen Vargas
#CS 555
##Term Projec

#install.packages("ggplot2")
library(ggplot2)

#Step 1 - Import and Inspect the data
getwd()
setwd("C:/Users/st114/OneDrive/Documents/Spring BU 2024/CS 555/Term Project/Code/Final Submition")
getwd()
df = read.csv("totalSleepData.csv")
#head(df)
# What are the data types
str(df)
# How large is the dataset 
nrow(df)
#is it missing data?
colSums(is.na(df))

dim(df)
#there are 622 rows and 13 columns

#What values are the ones who are empty from  each column?
# sleepStartTimestampGMT 
# 0 
# sleepEndTimestampGMT 
# 0 
# calendarDate 
# 0 
# sleepWindowConfirmationType 
# 0 
# deepSleepSeconds 
# 7 
# lightSleepSeconds 
# 7 
# remSleepSeconds 
# 48 
# awakeSleepSeconds 
# 7 
# unmeasurableSeconds 
# 7 
# averageRespiration 
# 112 
# lowestRespiration 
# 112 
# highestRespiration 
# 0 
# retro 


#STEP 2:
#Remove variables with high null counts or 
#low predictive power

dfnew <- df[, !(names(df) %in% c(
                                               "averageRespiration",
                                               "lowestRespiration",
                                               "highestRespiration",
                                               "retro"
                                               ))]
colSums(is.na(dfnew))
dim(dfnew)
#df.cleaned <- na.omit(df.dropped.columns)

# Replace NAs with 0s using replace()
dfnew$deepSleepSeconds <- replace(dfnew$deepSleepSeconds, is.na(dfnew$deepSleepSeconds), 0)
dfnew$lightSleepSeconds <- replace(dfnew$lightSleepSeconds, is.na(dfnew$lightSleepSeconds),0)
dfnew$remSleepSeconds <- replace(dfnew$remSleepSeconds, is.na(dfnew$remSleepSeconds),0)
dfnew$awakeSleepSeconds <- replace(dfnew$awakeSleepSeconds, is.na(dfnew$awakeSleepSeconds),0)
dfnew$unmeasurableSeconds <- replace(dfnew$unmeasurableSeconds, is.na(dfnew$unmeasurableSeconds),0)

#dfnew <- replace(dfnew$deepSleepSeconds, is.na(dfnew), 0)

##We are going to fill in each variable with it's mean when we have an empty space
#this will allow us to reduce the number of zeros in our dataset.

mean.deepSleepSeconds = mean(dfnew$deepSleepSeconds)
mean.deepSleepSeconds

mean.lightSleepSeconds = mean(dfnew$lightSleepSeconds)
mean.lightSleepSeconds

mean.remSleepSeconds = mean(dfnew$remSleepSeconds)
mean.remSleepSeconds

mean.awakeSleepSeconds = mean(dfnew$awakeSleepSeconds)
mean.awakeSleepSeconds

mean.unmeasurableSeconds = mean(dfnew$unmeasurableSeconds)
mean.unmeasurableSeconds

#now, we are going to replace any zeros that we have in the data with the
#averages

dfnew$deepSleepSeconds = replace(dfnew$deepSleepSeconds,dfnew$deepSleepSeconds== 0, mean.deepSleepSeconds)
dfnew$lightSleepSeconds = replace(dfnew$lightSleepSeconds, dfnew$lightSleepSeconds == 0, mean.lightSleepSeconds)
dfnew$remSleepSeconds = replace(dfnew$remSleepSeconds, dfnew$remSleepSeconds == 0, mean.remSleepSeconds)
dfnew$awakeSleepSeconds = replace(dfnew$awakeSleepSeconds, dfnew$awakeSleepSeconds == 0 , mean.awakeSleepSeconds)
dfnew$unmeasurableSeconds = replace(dfnew$unmeasurableSeconds, dfnew$awakeSleepSeconds == 0, mean.unmeasurableSeconds)

mean.deepSleepSeconds = mean(dfnew$deepSleepSeconds)
mean.deepSleepSeconds

mean.lightSleepSeconds = mean(dfnew$lightSleepSeconds)
mean.lightSleepSeconds

mean.remSleepSeconds = mean(dfnew$remSleepSeconds)
mean.remSleepSeconds

mean.awakeSleepSeconds = mean(dfnew$awakeSleepSeconds)
mean.awakeSleepSeconds

mean.unmeasurableSeconds = mean(dfnew$unmeasurableSeconds)
mean.unmeasurableSeconds

#summaries for each sleeping data
summary(dfnew$deepSleepSeconds)
summary(dfnew$lightSleepSeconds)
summary(dfnew$remSleepSeconds)
summary(dfnew$awakeSleepSeconds)
summary(dfnew$unmeasurableSeconds)

#standard deviation
sd(dfnew$deepSleepSeconds)
sd(dfnew$lightSleepSeconds)
sd(dfnew$remSleepSeconds)
sd(dfnew$awakeSleepSeconds)
sd(dfnew$unmeasurableSeconds)

#We are going to get Average Hours

mean.deepSleepSeconds/3600

mean.lightSleepSeconds/3600

mean.remSleepSeconds/3600

mean.awakeSleepSeconds/3600

mean.unmeasurableSeconds/3600

#checking for Marging or Error
n = 333
sd = 2.86
z = 1.960
moe = z * (sd/sqrt(n))
moe


dfnew$totalSleep = dfnew$deepSleepSeconds + 
                   dfnew$lightSleepSeconds +
                   dfnew$remSleepSeconds +
                   dfnew$awakeSleepSeconds +
                   dfnew$unmeasurableSeconds
dfnew$totalSleep
mean(dfnew$totalSleep)

dfnew$SleepHours = dfnew$totalSleep/3600
dfnew$SleepHours
mean(dfnew$SleepHours)

summary(dfnew$SleepHours)

model = lm(dfnew$SleepHours ~ dfnew$deepSleepSeconds + 
             dfnew$lightSleepSeconds +
             dfnew$remSleepSeconds +
             dfnew$awakeSleepSeconds +
             dfnew$unmeasurableSeconds, data = dfnew)

summary(model)

head(dfnew)

# write.csv(dfnew, file = "sleepFinal.csv", row.names = FALSE)
# list.files()

# I merged  the files successfully so I will be commenting these lines out
#so there are no errors


# Sample data frames
# dfUSD = read.csv("totalUDSFinal.csv")
# dim(dfUSD)
# 
# dfSleep <- dfnew
# head(dfSleep)

# merged_df <- read.csv("merged_df.csv")
# #merged_df <- merge(dfUSD, dfSleep, by = "calendarDate", all = FALSE)
# 
# 
# head(merged_df)
# dim(merged_df)
# str(merged_df)
# 
# # write.csv(merged_df, file = "merged_df.csv", row.names = FALSE)
# # list.files()

merged_df = read.csv("merged_df.csv")
nrow(merged_df)

#checking scatterplots we
pairs(~ merged_df$totalKilocalories +
                    merged_df$activeKilocalories +
                    merged_df$totalSteps +
                    merged_df$wellnessDistanceMeters +
                    merged_df$highlyActiveSeconds +
                    merged_df$activeSeconds +
                    merged_df$maxAvgHeartRate +
                    merged_df$SleepHours)

pairs(~merged_df$SleepHours +
        merged_df$remSleepSeconds+
        merged_df$deepSleepSeconds+
        merged_df$awakeSleepSeconds +
        merged_df$lightSleepSeconds) 

pairs(~merged_df$totalKilocalories +
        merged_df$activeKilocalories +
        merged_df$totalSteps +
        merged_df$wellnessDistanceMeters +
        merged_df$highlyActiveSeconds +
        merged_df$SleepHours +
        merged_df$remSleepSeconds+
        merged_df$lightSleepSeconds
)

#Finding correlation between sleep hours and light sleepseconds
cor(merged_df$SleepHours, merged_df$lightSleepSeconds)
#finding correlation between sleep hours and remsleep
cor(merged_df$SleepHours,merged_df$remSleepSeconds)
#Finding correlation between remSleep seconds and light sleep seconds
cor(merged_df$remSleepSeconds, merged_df$lightSleepSeconds)

#sleephours and light sleep seconds
m = lm(merged_df$SleepHours ~ merged_df$lightSleepSeconds, data = merged_df)
m
summary(m)

# plot(merged_df$lightSleepSeconds, merged_df$SleepHours, col="blue", pch=16,
#      xlab ='Light Sleep in Seconds',
#      ylab='Hours of Sleep per night',
#      main = 'Relationship between Hours of Sleep and Light sleep in seconds')

ggplot(merged_df, aes(x = merged_df$lightSleepSeconds, y = merged_df$SleepHours)) +
  geom_point(color = "blue", shape = 16) +
  labs(
    x = "Light Sleep in Seconds",
    y = "Hours of Sleep per Night",
    title = "Relationship between Hours of Sleep and Light Sleep in Seconds"
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#sleephours and rem sleep seconds

m1 = lm(SleepHours~ remSleepSeconds, data = merged_df)
m1
summary(m1)
coefficients(m1)

ggplot(merged_df, aes(x = merged_df$remSleepSeconds, y = merged_df$SleepHours)) +
  geom_point(color = "black", shape = 16) +
  labs(
    x = "Rem Sleep in Seconds",
    y = "Hours of Sleep per Night",
    title = "Relationship between Hours of Sleep and Rem Sleep in Seconds"
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()


m2 = lm(merged_df$remSleepSeconds ~ merged_df$lightSleepSeconds, data = merged_df)
m2
summary(m2)
# plot(merged_df$lightSleepSeconds, merged_df$remSleepSeconds, col="red", pch=16,
#      xlab ='Total light Sleep in Seconds',
#      ylab='Total Rem Sleep in Seconds',
#      main = 'Relationship between Rem Sleep in seconds and Light Sleep in Seconds')

# ggplot(merged_df, aes(x = lightSleepSeconds, y = remSleepSeconds)) +
#   geom_point(color = "red", shape = 16) +
#   labs(
#     x = "Total Light Sleep in Seconds",
#     y = "Total REM Sleep in Seconds",
#     title = "Relationship between REM Sleep and Light Sleep in Seconds"
#   ) +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_minimal()


m3 = lm(merged_df$SleepHours ~ merged_df$lightSleepSeconds + merged_df$remSleepSeconds, data = merged_df)
m3
summary(m3)
coefficients(m3)

# ggplot(merged_df, aes(x = lightSleepSeconds, y = SleepHours)) +
#   geom_point(color = "blue", shape = 16) +
#   labs(
#     x = "Light Sleep in Seconds",
#     y = "Hours of Sleep per Night",
#     title = "Relationship between Hours of Sleep and Light Sleep"
#   ) +
#   geom_abline(intercept = coef(m3)[1], slope = coef(m3)[2], color = "red") +
#   geom_abline(intercept = coef(m3)[1], slope = coef(m3)[3], color = "green") +
#   theme_minimal()


ggplot(merged_df, aes(x = lightSleepSeconds, y = SleepHours, color = remSleepSeconds)) +
  geom_point(shape = 16) +
  labs(
    x = "Light Sleep in Seconds",
    y = "Hours of Sleep per Night",
    title = "Relationship between Hours of Sleep, Light Sleep, and REM Sleep"
  ) +
  scale_color_continuous(name = "REM Sleep Seconds") +
  geom_abline(intercept = coef(m3)[1], slope = coef(m3)[2], color = "red") +
  geom_abline(intercept = coef(m3)[1], slope = coef(m3)[3], color = "blue") +
  theme_minimal()

#trying to have one line from the m3 regression model
beta0 <- 0.8810771608
beta1_lightSleep <- 0.0002733296
beta2_remSleep <- 0.0002489192

# Generate x values (predictor variable)
x_values <- seq(min(merged_df$lightSleepSeconds), max(merged_df$lightSleepSeconds), length.out = 100)

# Calculate y values (predicted values) based on the linear equation
y_values <- beta0 + beta1_lightSleep * x_values + beta2_remSleep * x_values

# Create a data frame for the line
line_data <- data.frame(x = x_values, y = y_values)

# Create a scatterplot with ggplot2
ggplot(merged_df, aes(x = lightSleepSeconds, y = SleepHours)) +
  geom_point(color = "blue", shape = 16) +  # Add actual data points
  labs(
    x = "Light Sleep in Seconds",
    y = "Hours of Sleep per Night",
    title = "Relationship between Hours of Sleep, Light Sleep, and REM Sleep"
  ) +
  geom_line(data = line_data, aes(x = x, y = y), color = "red") +  # Add the line
  theme_minimal()



#LIGHT SLEEP COMPARISON

ggplot(merged_df, aes(x = merged_df$lightSleepSeconds, y = merged_df$SleepHours)) +
  geom_point(color = "blue", shape = 16) +
  labs(
    x = "Light Sleep in Seconds",
    y = "Hours of Sleep per Night",
    title = "Relationship between Hours of Sleep and Light Sleep in Seconds"
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(data = line_data, aes(x = x, y = y), color = "red") + #adding the line
  theme_minimal()

#REM SLEEP COMPARISON
ggplot(merged_df, aes(x = merged_df$remSleepSeconds, y = merged_df$SleepHours)) +
  geom_point(color = "green", shape = 16) +
  labs(
    x = "Rem Sleep in Seconds",
    y = "Hours of Sleep per Night",
    title = "Relationship between Hours of Sleep and REM Sleep in Seconds"
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(data = line_data, aes(x = x, y = y), color = "red") + #adding the line
  theme_minimal()














#breaking the data by month
library(lubridate)
merged_df$month = month(mdy(merged_df$calendarDate),label = T)

merged_df$year = year(mdy(merged_df$calendarDate))

ggplot(merged_df, aes(x = merged_df$month, y = merged_df$SleepHours, group = month, color = month)) +
  geom_point() +
  labs(x = "Month", y = "Hours of Sleep", title = "Hours of Sleep Over Time (Grouped by Month)")

ggplot(merged_df, aes(x = merged_df$year, y = merged_df$SleepHours, group = year, color = year)) +
  geom_point() +
  labs(x = "Year", y = "Hours of Sleep", title = "Hours of Sleep Over Time (Grouped by Year)")


# 3 year comparison of the data
ggplot(merged_df, aes(x = merged_df$month, y = merged_df$SleepHours, group = month, color = month)) +
  geom_point() +
  labs(x = "Month", y = "Hours of Sleep", title = "Hours of Sleep Over Time (Grouped by Month)") +
  facet_wrap(~ year, scales = "free_x", nrow = 1)

#year 2022
data_22 = merged_df[merged_df$year == 2022,]
data_22$year

ggplot(data_22, aes(x = month, y = SleepHours, group = month, color = month)) +
  geom_point() +
  labs(x = "Month", y = "Hours of Sleep", title = "Hours of Sleep Over Time (2022)")

#year 2023
data_23 = merged_df[merged_df$year == 2023,]
data_23$year

ggplot(data_23, aes(x = month, y = SleepHours, group = month, color = month)) +
  geom_point() +
  labs(x = "Month", y = "Hours of Sleep", title = "Hours of Sleep Over Time (2023)")

#year 2024
data_24 = merged_df[merged_df$year == 2024,]
data_24$year
ggplot(data_24, aes(x = month, y = SleepHours, group = month, color = month)) +
  geom_point() +
  labs(x = "Month", y = "Hours of Sleep", title = "Hours of Sleep Over Time (2024)")

