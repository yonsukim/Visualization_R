### Forecasting - CA  3/2/25 ###  



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
tips <- read.csv("C:\\Users\\ ID \\Dropbox\\Research\\Github\\Stats-in-R\\tips.csv")

##################### Trend Data ##########################################

CA <- read_excel("C:\\Users\\ ID \\Dropbox\\Research\\Asthma_Cannabis\\data\\Forecast_CA_030225.xlsx", sheet="TS") 
View(CA)

CA65 <- read_excel("C:\\Users\\ ID \\Dropbox\\Research\\Asthma_Cannabis\\data\\Forecast_CA_030225.xlsx", sheet="TS_65") 
View(CA65)

# If the date is in "YYYY-MM" format, convert it to Date class (in Excel)



########## Figure 1-1: Aged 1-19 ##############


ggplot(CA, aes(x = t)) +
  geom_line(aes(y = a1, color="Actual"), size=1, lty=1) +  
  geom_line(aes(y = p1, color="Predicted"), size=0.5, lty=1) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(title = "Aged 0-19", x = "Year", y = "Hospitalization", color = "", linetype="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))


########## Figure 1-2: Aged 20-44 ##############

ggplot(CA, aes(x = t)) +
  geom_line(aes(y = a2, color="Actual"), size=1, lty=1) +  
  geom_line(aes(y = p2, color="Predicted"), size=0.5, lty=1) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(title = "Aged 20-44", x = "Year", y = "Hospitalization", color = "", linetype="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))

########## Figure 1-3: Aged 45-64 ##############

ggplot(CA, aes(x = t)) +
  geom_line(aes(y = a3, color="Actual"), size=1, lty=1) +  
  geom_line(aes(y = p3, color="Predicted"), size=0.5, lty=1) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(title = "Aged 45-64", x = "Year", y = "Hospitalization", color = "", linetype="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))


####### Figure 1-4: Aged 65 + (after Jan. 2010) ############

ggplot(CA65, aes(x = t)) +
  geom_line(aes(y = a4, color="Actual"), size=1, lty=1) +  
  geom_line(aes(y = p4, color="Predicted"), size=0.5, lty=1) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(title = "Aged 65+", x = "Year", y = "Hospitalization", color = "", linetype="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))
 

########## Figure 1-4: Aged 65 + ##############

ggplot(CA, aes(x = t)) +
  geom_line(aes(y = a4, color="Actual"), size=1, lty=1) +  
  geom_line(aes(y = p4, color="Predicted"), size=0.5, lty=1) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(title = "Aged 65+", x = "Year", y = "Hospitalization", color = "", linetype="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))
 
 
#### End - 3/2/2025 #####









ggplot(CA, aes(x = t)) +
  geom_line(aes(y = a1, color="Actual"), size=0.5, lty=1) +  
  geom_line(aes(y = p1, color="Predicted"), size=0.5, lty=1) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "Year", y = "Hospitalization", color = "", linetype="") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))


ggplot(CA, aes(x = t)) +
  geom_line(aes(y = a1, color="Actual"), size=1, lty=1) +  
  geom_line(aes(y = p1, color="Predicted"), size=0.5, lty=1) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(title = "Aged 0-19", x = "Year", y = "Hospitalization", color = "", linetype="") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "gray"))




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

