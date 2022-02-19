
library(ggplot2)

# Sample 1
boxLabels = c("African American     IP/OP", 
              "African American   IP/PCP", 
              "African American    ED/OP", 
              "African American ED/PCP",
              "Male                           IP/OP", 
              "Male                        IP/PCP", 
              "Male                         ED/OP", 
              "Male                      ED/PCP",
              "Ages(6-10)                IP/OP", 
              "Ages(6-10)             IP/PCP", 
              "Ages(6-10)              ED/OP", 
              "Ages(6-10)           ED/PCP",
              "Income(1Q)              IP/OP", 
              "Income(1Q)           IP/PCP", 
              "Income(1Q)            ED/OP", 
              "Income(1Q)         ED/PCP",
              "Poverty(>=20%)      IP/OP", 
              "Poverty(>=20%)   IP/PCP", 
              "Poverty(>=20%)    ED/OP", 
              "Poverty(>=20%) ED/PCP"
)

df <- data.frame(yAxis = length(boxLabels):1, 
                 boxCILow =  c(1.36, 1.31, 1.14, 1.15, 
                               1.18, 1.17, 1.17, 1.15,  
                               2.11, 2.05, 2.12, 2.03, 
                               1.01, 1.02, 1.27, 1.29, 
                               1.03, 1.02, 0.88, 0.91), 
                 boxOdds =   c(1.56, 1.53, 1.39, 1.42,
                               1.27, 1.26, 1.29, 1.28,
                               2.42, 2.35, 2.56, 2.48,
                               1.22, 1.22, 1.66, 1.68,
                               1.37, 1.37, 1.35, 1.43), 
                 boxCIHigh = c(1.80, 1.78, 1.70, 1.76,
                               1.37, 1.36, 1.43, 1.43,
                               2.76, 2.71, 3.10, 3.03,
                               1.46, 1.48, 2.17, 2.20,
                               1.83, 1.85, 2.06, 2.23))


(p <- ggplot(df, aes(x = boxOdds, y = boxLabels)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .5, color = "gray40") +
    geom_point(size = 3.0, color = "midnight blue") +
    coord_trans(x = scales:::exp_trans(2)) +
    
    scale_x_sqrt(breaks = seq(0.6, 3.2, 0.2), labels = seq(0.6, 3.2, 0.2),
                       limits = c(0.7,3.2)) +

            theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "", size = 5, hjust = 0) + 
    ggtitle("")
) 


  

# Sample 2
boxLabels = c("African American",
              "African American     IP/OP", 
              "African American   IP/PCP", 
              "African American    ED/OP", 
              "African American ED/PCP",
              "Male                           ", 
              "Male                           IP/OP", 
              "Male                        IP/PCP", 
              "Male                         ED/OP", 
              "Male                      ED/PCP",
              "Ages(6-10)                ", 
              "Ages(6-10)                IP/OP", 
              "Ages(6-10)             IP/PCP", 
              "Ages(6-10)              ED/OP", 
              "Ages(6-10)           ED/PCP",
              "Income(1Q)              ", 
              "Income(1Q)              IP/OP", 
              "Income(1Q)           IP/PCP", 
              "Income(1Q)            ED/OP", 
              "Income(1Q)         ED/PCP",
              "Poverty(>=20%)      ", 
              "Poverty(>=20%)      IP/OP", 
              "Poverty(>=20%)   IP/PCP", 
              "Poverty(>=20%)    ED/OP", 
              "Poverty(>=20%) ED/PCP"
)



df2$yAxis <- factor(df$yAxis,levels = c(
  "African American",
  "African American     IP/OP", 
  "African American   IP/PCP", 
  "African American    ED/OP", 
  "African American ED/PCP",
  "Male                           ", 
  "Male                           IP/OP", 
  "Male                        IP/PCP", 
  "Male                         ED/OP", 
  "Male                      ED/PCP",
  "Ages(6-10)                ", 
  "Ages(6-10)                IP/OP", 
  "Ages(6-10)             IP/PCP", 
  "Ages(6-10)              ED/OP", 
  "Ages(6-10)           ED/PCP",
  "Income(1Q)              ", 
  "Income(1Q)              IP/OP", 
  "Income(1Q)           IP/PCP", 
  "Income(1Q)            ED/OP", 
  "Income(1Q)         ED/PCP",
  "Poverty(>=20%)      ", 
  "Poverty(>=20%)      IP/OP", 
  "Poverty(>=20%)   IP/PCP", 
  "Poverty(>=20%)    ED/OP", 
  "Poverty(>=20%) ED/PCP"
))
              
              
              
# Revision (from  here)


df2 <- data.frame(yAxis = 
                   c(
                     "African American",
                     "African American     IP/OP", 
                     "African American   IP/PCP", 
                     "African American    ED/OP", 
                     "African American ED/PCP",
                     "Male                           ", 
                     "Male                           IP/OP", 
                     "Male                        IP/PCP", 
                     "Male                         ED/OP", 
                     "Male                      ED/PCP",
                     "Ages(6-10)                ", 
                     "Ages(6-10)                IP/OP", 
                     "Ages(6-10)             IP/PCP", 
                     "Ages(6-10)              ED/OP", 
                     "Ages(6-10)           ED/PCP",
                     "Income(1Q)              ", 
                     "Income(1Q)              IP/OP", 
                     "Income(1Q)           IP/PCP", 
                     "Income(1Q)            ED/OP", 
                     "Income(1Q)         ED/PCP",
                     "Poverty(>=20%)      ", 
                     "Poverty(>=20%)      IP/OP", 
                     "Poverty(>=20%)   IP/PCP", 
                     "Poverty(>=20%)    ED/OP", 
                     "Poverty(>=20%) ED/PCP"),
                 boxCILow =  c(0, 1.36, 1.31, 1.14, 1.15, 
                               0, 1.18, 1.17, 1.17, 1.15,  
                               0,2.11, 2.05, 2.12, 2.03, 
                               0,1.01, 1.02, 1.27, 1.29, 
                               0,1.03, 1.02, 0.88, 0.91), 
                 boxOdds =   c(0, 1.56, 1.53, 1.39, 1.42,
                               0, 1.27, 1.26, 1.29, 1.28,
                               0,2.42, 2.35, 2.56, 2.48,
                               0,1.22, 1.22, 1.66, 1.68,
                               0,1.37, 1.37, 1.35, 1.43), 
                 boxCIHigh = c(0, 1.80, 1.78, 1.70, 1.76, 
                               0, 1.37, 1.36, 1.43, 1.43,
                               0,2.76, 2.71, 3.10, 3.03,
                               0,1.46, 1.48, 2.17, 2.20,
                               0,1.83, 1.85, 2.06, 2.23))


# View(df2)

# Reverse order
df2$yAxis <- factor(df2$yAxis,levels=rev(unique(df2$yAxis)))



(p <- ggplot(df2, aes(x = boxOdds, y =yAxis)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_bar(stat = "identity") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .5, color = "gray40") +
    geom_point(size = 3.0, color = "midnight blue") +
    coord_trans(x = scales:::exp_trans(2)) +
    
    scale_x_sqrt(breaks = seq(0.6, 3.2, 0.2), labels = seq(0.6, 3.2, 0.2),
                 limits = c(0.7,3.2)) +
    
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "", size = 5, hjust = 0) + 
    ggtitle("Test")
) 
  

# Revision 3



df3 <- data.frame(yAxis = 
                    c(
                      "IP/OP", 
                      "IP/PCP", 
                      "ED/OP", 
                      "ED/PCP",
                      "IP/OP", 
                      "IP/PCP", 
                      "ED/OP", 
                      "ED/PCP",
                      "IP/OP", 
                      "IP/PCP", 
                      "ED/OP", 
                      "ED/PCP",
                      "IP/OP", 
                      "IP/PCP", 
                      "ED/OP", 
                      "ED/PCP",
                      "IP/OP", 
                      "IP/PCP", 
                      "ED/OP", 
                      "ED/PCP"),
                  grouping = c("African American","African American","African American","African American",
                               "Male","Male","Male","Male",
                               "Ages (6-10)","Ages (6-10)","Ages (6-10)","Ages (6-10)",
                               "Median Household Income (<Q1)","Median Household Income (<Q1)","Median Household Income (<Q1)","Median Household Income (<Q1)",  
                               "Poverty rate (>=20%)","Poverty rate (>=20%)","Poverty rate (>=20%)","Poverty rate (>=20%)"),
                  boxCILow =  c(1.36, 1.31, 1.14, 1.15, 
                                1.18, 1.17, 1.17, 1.15,  
                                2.11, 2.05, 2.12, 2.03, 
                                1.01, 1.02, 1.27, 1.29, 
                                1.03, 1.02, 0.88, 0.91), 
                  boxOdds =   c(1.56, 1.53, 1.39, 1.42,
                                1.27, 1.26, 1.29, 1.28,
                                2.42, 2.35, 2.56, 2.48,
                                1.22, 1.22, 1.66, 1.68,
                                1.37, 1.37, 1.35, 1.43), 
                  boxCIHigh = c(1.80, 1.78, 1.70, 1.76, 
                                1.37, 1.36, 1.43, 1.43,
                                2.76, 2.71, 3.10, 3.03,
                                1.46, 1.48, 2.17, 2.20,
                                1.83, 1.85, 2.06, 2.23))


# View(df3)



# Reverse order
df3$yAxis <- factor(df3$yAxis,levels=rev(unique(df3$yAxis)))



(p <- ggplot(df3, aes(x = boxOdds, y =yAxis)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_bar(stat = "identity") +
    facet_wrap(~grouping, scales="free") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(size = 2.5, color = "blue") +
    coord_trans(x = scales:::exp_trans(2)) +
    
    scale_x_sqrt(breaks = seq(0.6, 3.2, 0.4), labels = seq(0.6, 3.2, 0.4),
                 limits = c(0.7,3.2)) +
    
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio (log scale)") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "Model p < 0.001", size = 5, hjust = 0) + 
    ggtitle("")
) 



# grouping
# facet_wrap(~grouping, scales="free") +

# Adjust scale of x Axis  
# scale_x_sqrt(breaks = seq(0.6, 3.2, 0.4), labels = seq(0.6, 3.2, 0.4),
             


# This is most recently updated (7/24/20)
(p <- ggplot(df3, aes(x = boxOdds, y =yAxis)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_bar(stat = "identity") +
    facet_wrap(~grouping, scales="free") +

    geom_errorbarh(data = filter(df3, yAxis=="IP/OP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df3, yAxis=="IP/OP"), size = 3, color = "midnight blue") +

    geom_errorbarh(data = filter(df3, yAxis=="IP/PCP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df3, yAxis=="IP/PCP"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df3, yAxis=="ED/OP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df3, yAxis=="ED/OP"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df3, yAxis=="ED/PCP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df3, yAxis=="ED/PCP"), size = 3, color = "purple") +
    
    coord_trans(x = scales:::exp_trans(2)) +
    
    scale_x_sqrt(breaks = seq(0.6, 3.2, 0.4), labels = seq(0.6, 3.2, 0.4),
                 limits = c(0.7,3.2)) +
    
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds ratio (log scale)") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "Model p < 0.001", size = 5, hjust = 0) + 
    ggtitle("")
) 



# Different point color
# Use filter function
# https://www.jscarlton.net/post/2017-04-05multipledotsggplot/










# Order x axis

library(tidyverse)
library(scales)

data(tips, package = "reshape2")

View(tips)

tips %>% 
  count(day) %>% 
  mutate(perc = n / nrow(tips)) -> tips2


tips2$day <- factor(tips2$day,levels = c("Thur", "Fri", "Sat", "Sun"))

ggplot(tips2, aes(x = day, y = perc)) + geom_bar(stat = "identity")







# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_errorbar





# Bar graphs

df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
head(df)

ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme_minimal()

# Inside bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=1.6, color="white", size=3.5)+
  theme_minimal()


# VIIP 
df2 <- data.frame(supp=rep(c("Threnshold MY2018", "Threnshold MY2015"), each=3),
                  Measure=rep(c("CWP", "AMR", "AAB"),2),
                  Rate=c('26.55%', '57.25%', '33.22%', '19.81%', '52.28%', '29.30%'))
head(df2)


ggplot(data=df2, aes(x=Measure, y=Rate, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Rate), vjust=-0.3, size=3.5,
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()




ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme_minimal()




####################################### Contorller medications added #######################################
### 04/19/21



df4 <- data.frame(yAxis = 
                    c(
                      "ED/OP", 
                      "ED/PCP", 
                      "IP/OP", 
                      "IP/PCP",
                      "ED/OP", 
                      "ED/PCP", 
                      "IP/OP", 
                      "IP/PCP",
                      "ED/OP", 
                      "ED/PCP", 
                      "IP/OP", 
                      "IP/PCP",
                      "ED/OP", 
                      "ED/PCP", 
                      "IP/OP", 
                      "IP/PCP",
                      "ED/OP", 
                      "ED/PCP", 
                      "IP/OP", 
                      "IP/PCP",
                      "ED/OP", 
                      "ED/PCP", 
                      "IP/OP", 
                      "IP/PCP"),
                  grouping = c("African American","African American","African American","African American",
                               "Male","Male","Male","Male",
                               "Ages (0-5)","Ages (0-5)","Ages (0-5)","Ages (0-5)",
                               "AMR (<0.5)","AMR (<0.5)","AMR (<0.5)","AMR (<0.5)",
                               "Median Household Income (<Q1)","Median Household Income (<Q1)","Median Household Income (<Q1)","Median Household Income (<Q1)",  
                               "Poverty rate (>=20%)","Poverty rate (>=20%)","Poverty rate (>=20%)","Poverty rate (>=20%)"),
                  boxCILow =  c(1.08, 1.10, 1.30, 1.26, 
                                1.13, 1.11, 1.13, 1.12,  
                                2.13, 1.90, 2.38, 2.15, 
                                1.78, 1.93, 2.06, 2.28,  
                                1.38, 1.39, 1.09, 1.10, 
                                0.95, 0.99, 1.10, 1.08), 
                  boxOdds =   c(1.32, 1.36, 1.50, 1.47,
                                1.25, 1.24, 1.22, 1.21,
                                2.65, 2.39, 2.77, 2.52,  
                                1.97, 2.14, 2.22, 2.45,
                                1.78, 1.80, 1.30, 1.31,
                                1.47, 1.59, 1.48, 1.47), 
                  boxCIHigh = c(1.62, 1.68, 1.73, 1.71, 
                                1.39, 1.38, 1.32, 1.31,
                                3.29, 3.01, 3.22, 2.96,  
                                2.17, 2.37, 2.38, 2.64,
                                2.31, 2.33, 1.54, 1.57,
                                2.29, 2.53, 1.99, 2.00))


# View(df4)



# Reverse order
df4$yAxis <- factor(df4$yAxis,levels=rev(unique(df4$yAxis)))


(p <- ggplot(df4, aes(x = boxOdds, y =yAxis)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_bar(stat = "identity") +
    facet_wrap(~grouping, scales="free") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(size = 2.5, color = "blue") +
    coord_trans(x = scales:::exp_trans(2)) +
    
    scale_x_sqrt(breaks = seq(0.6, 3.2, 0.4), labels = seq(0.6, 3.2, 0.4),
                 limits = c(0.7,3.2)) +
    
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds Ratio (log scale)") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "Model p < 0.001", size = 5, hjust = 0) + 
    ggtitle("")
) 



# grouping
# facet_wrap(~grouping, scales="free") +

# Adjust scale of x Axis  
# scale_x_sqrt(breaks = seq(0.6, 3.2, 0.4), labels = seq(0.6, 3.2, 0.4),



# This is most recently updated (4/19/20)/ AMR updated (06/23/21)


(p <- ggplot(df4, aes(x = boxOdds, y =yAxis)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_bar(stat = "identity") +
    facet_wrap(~grouping, scales="free") +
    
    geom_errorbarh(data = filter(df4, yAxis=="IP/OP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df4, yAxis=="IP/OP"), size = 3, color = "midnight blue") +
    
    geom_errorbarh(data = filter(df4, yAxis=="IP/PCP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df4, yAxis=="IP/PCP"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df4, yAxis=="ED/OP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df4, yAxis=="ED/OP"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df4, yAxis=="ED/PCP"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df4, yAxis=="ED/PCP"), size = 3, color = "purple") +
    
    coord_trans(x = scales:::exp_trans(2)) +
    

        scale_x_sqrt(breaks = seq(0.6, 3.4, 0.4), labels = seq(0.6, 3.4, 0.4),
                 limits = c(0.8,3.4)) +
    
        theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Odds Ratio (log scale)") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "Model p < 0.001", size = 5, hjust = 0) + 
    ggtitle("")
) 


