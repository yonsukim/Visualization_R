
install.packages("readxl")
library(ggplot2)
library(readxl)


########################  GI-ED (8/3/23) #################################

GI <- read_excel("C:/Users/Dongwoo/Dropbox/Research/GI-ED/Data/data_20230803.xlsx")
View(GI)

# Below, both work
GI$time <- as.Date(GI$time, format = "%m/%d/%Y")
GI$time <- as.Date(paste0(GI$time, "-01-01"))
class(GI$time)
cutoff <- as.Date("2017/7/01")


## Segmented trend for # ED visits

ggplot(GI, aes(time, nau_r)) +
  labs(x = "Year") + labs(y = "CHS ED visits (per 100,000 ED visits") +
#  geom_line() + 
  geom_point() +
  geom_smooth(aes(group = time >= cutoff), method = "lm")



## Segmented trend for rate of ED visits

ggplot(TS, aes(time, opiw_r)) +
  labs(x = "Year") + labs(y = "Rate") +
  geom_line() +
  geom_smooth(aes(group = time >= cutoff), method = "lm")




################################ Opioid (8/3/23) #################################


TS <- read_excel("C:/Users/Dongwoo/Dropbox/Research/Cannabis_Opioid/Data/TS.xlsx")
View(TS)

TS$time <- as.Date(TS$time, format = "%m/%d/%Y")
TS$time <- as.Date(paste0(TS$time, "-01-01"))
class(TS$time)
cutoff <- as.Date("2017/7/01")


## Segmented trend for # ED visits

ggplot(TS, aes(time, OPIW)) +
  labs(x = "Year") + labs(y = "ED visits") +
  geom_line() +
  geom_smooth(aes(group = time >= cutoff), method = "lm")



## Segmented trend for rate of ED visits

ggplot(TS, aes(time, opiw_r)) +
  labs(x = "Year") + labs(y = "Rate") +
  geom_line() +
  scale_x_continuous(breaks = seq(min(TS$time), max(TS$time), 1)) +
  geom_smooth(aes(group = time >= cutoff), method = "lm")




################################ Example (Stackoverflow) #################################
# https://stackoverflow.com/questions/68127752/adding-two-separate-trend-lines-for-specific-time-period-in-ggplot

sns1 <- read.table(text = txt, sep = " ", header = TRUE)
sns1$date <- as.Date(sns1$date, format = "%m/%d/%Y")


txt <- "date value
11/14/2020 18.57
11/21/2020 19.62
11/28/2020 21.81
12/5/2020 21.24
12/12/2020 22.32
12/19/2020 20.79
12/26/2020 21.18
1/2/2021 21.38
1/9/2021 21.22
1/16/2021 20.45
1/23/2021 19.11
1/30/2021 20.74"

sns1 <- read.table(text = txt, sep = " ", header = TRUE)
sns1$date <- as.Date(sns1$date, format = "%m/%d/%Y")

cutoff <- as.Date("2020/12/26")

ggplot(sns1, aes(date, value)) +
  geom_line() +
  geom_smooth(aes(group = date >= cutoff),
              method = "lm")
#> `geom_smooth()` using formula 'y ~ x'



