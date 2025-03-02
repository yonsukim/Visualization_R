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




