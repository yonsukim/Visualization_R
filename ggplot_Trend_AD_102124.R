### Trend AD  10/21/24 ###  



##################### Packages to be installed #####################

install.packages("readxl")
library(readxl)

install.packages("broom")
library(broom)

install.packages("ggeffects")
library(ggeffects)

install.packages("tidyverse")
library(tidyverse)
update.packages("tidyverse")


install.packages("margins")
library(margins)
data(mtcars)

#####################################################################

library(ggplot2) 
library(dplyr) 
tips <- read.csv("C:\\Users\\kimy89\\Dropbox\\Research\\Github\\Stats-in-R\\tips.csv")

##################### Trend Data ##########################################

AD <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_DM_Trend\\Results\\Asthma_DM_Trend_092924.xlsx", sheet="RStudio") 
View(AD)

# If the date is in "YYYY-MM" format, convert it to Date class (in Excel)

# Plot the monthly time series with a trend line

ggplot(AD, aes(x = DATE, y = ASW)) +
  geom_line() +         # Plot the time series line
  labs(title = "Monthly Time Series with Trend Line", x = "Month", y = "white")


########## Figure 1: Asthma per 100K ##############

ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = ASB, color="African American"), size=0.5, lty=1) +  
  geom_line(aes(y = ASW, color="White"), size=0.5, lty=1) +  
  geom_line(aes(y = ASH, color="Hispanic"), size=0.5, lty=2) +  
  geom_line(aes(y = ASA, color="Asian"), size=0.9, lty=3) +  
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  labs( x = "", y = "", color = "", linetype="") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))



########## Figure 2: DA per 100K ##############

########## B-W
ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DAB, color="African American"), size=0.5, lty=1) +  
  geom_line(aes(y = DAW, color="White"), size=0.7, lty=2) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "", y = "", color = "", linetype="") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))

########## H-W
ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DAH, color="Hispanic"), size=0.5, lty=1) +  
  geom_line(aes(y = DAW, color="White"), size=0.7, lty=2) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "", y = "", color = "", linetype="") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))

########## A-W
ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DAA, color="Asian"), size=0.5, lty=1) +  
  geom_line(aes(y = DAW, color="White"), size=0.7, lty=2) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "", y = "", color = "", linetype="") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))



########## Figure 3: Diff ##############

########## B-W + A-W + H-W

ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DIFFA, color="Asian-White"), size=0.5, lty=3) +  
  geom_smooth(aes(y = DIFFA, color="Asian-White"), method = "lm", se = FALSE, linetype = "solid", size=0.75) +  # Trend line for Series 1
  geom_line(aes(y = DIFFB, color="African American-White"), size=0.5, lty=3) +  
  geom_smooth(aes(y = DIFFB, color="African American-White"), method = "lm", se = FALSE, linetype = "longdash", size=0.9) +  # Trend line for Series 1
  geom_line(aes(y = DIFFH, color="Hispanic-White"), size=0.5, lty=3) +  
  geom_smooth(aes(y = DIFFH, color="Hispanic-White"), method = "lm", se = FALSE, linetype = "twodash", size=0.9) +  # Trend line for Series 1
  scale_linetype_manual(values = c("solid", "dotted")) +
  # scale_color_manual(values = c("blue4", "green4", "re")) 
  #scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  labs( x = "", y = "", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))

# 10/31/2024 - Updated

