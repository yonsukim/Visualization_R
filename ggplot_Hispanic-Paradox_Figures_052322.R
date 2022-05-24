
library(ggplot2)


library(tidyverse)
library(lubridate)

####################################### HISPANIC PARADOX #######################################
### 05/23/22



boxLabels = c("ED6 (All)","ED6 (Mexican)","ED6 (Non-Mexican)",
              "ED12 (All)","ED12 (Mexican)","ED12 (Non-Mexican)",
              "IP6 (All))","IP6 (Mexican)","IP6 (Non-Mexican)",
              "IP12 (All)","IP12 (Mexican)","IP12 (Non-Mexican)",
              "OP6 (All)","OP6 (Mexican)","OP6 (Non-Mexican)",
              "OP12 (All)","OP12 (Mexican)","OP12 (Non-Mexican)")

df_lang <- data.frame(yAxis = length(boxLabels):1, 
                  boxCILow =  c(0.65, 0.63, 0.62, 0.66, 0.62, 0.66,
                                0.48, 0.54, 0.11, 0.60, 0.62, 0.27,
                                1.04, 1.04, 0.87, 0.97, 0.97, 0.81),
                  boxOdds =   c(0.78, 0.74, 0.99, 0.76, 0.72, 0.94,
                                0.68, 0.79, 0.30, 0.78, 0.82, 0.51,
                                1.14, 1.14, 1.13, 1.05, 1.06, 1.01),
                  boxCIHigh = c(0.93, 0.93, 1.58, 0.87, 0.83, 1.34, 
                                0.98, 1.17, 0.80, 1.00, 1.08, 0.95,
                                1.24, 1.26, 1.45, 1.13, 1.15, 1.26))

# View(df_lang)

# Reverse order
df_lang$yAxis <- factor(df_lang$yAxis,levels=rev(unique(df_lang$yAxis)))

(p <- ggplot(df_lang, aes(x = boxOdds, y = boxLabels)) + 

         geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .5, color = "gray40") +
        geom_point(size = 3.0, color = "midnight blue") +
        coord_trans(x = scales:::exp_trans(2)) +
    
        scale_x_sqrt(breaks = seq(0.1, 1.8, 0.2), labels = seq(0.1, 1.8, 0.2),
                     limits = c(0.1,1.8)) +
        
        theme_bw()+
        theme(panel.grid.minor = element_blank()) +
        ylab("") +
        xlab("Odds ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("")
) 



##### ED ONLY



df_ED <- data.frame(yAxis = c("6M(All)", "6M(Mexican)", "6M(Non-MX)", 
                              "12M(All)","12M(Mexican)", "12M(Non-MX)"), 
                    boxCILow =  c(0.65, 0.63, 0.62, 0.66, 0.62, 0.66),
                    boxOdds =   c(0.78, 0.74, 0.99, 0.76, 0.72, 0.94),
                    boxCIHigh = c(0.93, 0.93, 1.58, 0.87, 0.83, 1.34))
# View(df_ED)

(p <- ggplot(df_ED, aes(x = boxOdds, y = yAxis)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        scale_y_discrete(limits = unique(rev(df_ED$yAxis))) + # Reverse order #

        geom_errorbarh(data = filter(df_ED, yAxis=="6M(All)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="6M(All)"), size = 4.0, height = .15, color = "midnight blue") +

        geom_errorbarh(data = filter(df_ED, yAxis=="6M(Mexican)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="6M(Mexican)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="6M(Non-MX)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_ED, yAxis=="6M(Non-MX)"), shape=2, size = 4.0, height = .15, color = "red") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="12M(All)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="12M(All)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="12M(Mexican)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="12M(Mexican)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="12M(Non-MX)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_ED, yAxis=="12M(Non-MX)"), shape=2, size = 4.0, height = .15, color = "red") +
        
        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.6, 1.6, 0.2), labels = seq(0.6, 1.6, 0.2),
                     limits = c(0.6, 1.6)) +
        theme_bw()+ theme(panel.grid.minor = element_blank()) +
        ylab("") +  xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("ED Visits"))
 

## ED #2


df_ED <- data.frame(yAxis = c("All(6m)", "Mexican(6m)", "Non-MX(6m)", 
                              "All(12m)","Mexican(12m)", "Non-MX(12m)"), 
                    boxCILow =  c(0.65, 0.63, 0.62, 0.66, 0.62, 0.66),
                    boxOdds =   c(0.78, 0.74, 0.99, 0.76, 0.72, 0.94),
                    boxCIHigh = c(0.93, 0.93, 1.58, 0.87, 0.83, 1.34))
# View(df_ED)

(p <- ggplot(df_ED, aes(x = boxOdds, y = yAxis)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        scale_y_discrete(limits = unique(rev(df_ED$yAxis))) + # Reverse order #
        
        geom_errorbarh(data = filter(df_ED, yAxis=="All(6m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="All(6m)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="Mexican(6m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="Mexican(6m)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="Non-MX(6m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_ED, yAxis=="Non-MX(6m)"), shape=2, size = 4.0, height = .15, color = "red") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="All(12m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="All(12m)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="Mexican(12m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="Mexican(12m)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="Non-MX(12m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_ED, yAxis=="Non-MX(12m)"), shape=2, size = 4.0, height = .15, color = "red") +
        
        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.6, 1.6, 0.2), labels = seq(0.6, 1.6, 0.2),
                     limits = c(0.6, 1.6)) +
        theme_bw()+ theme(panel.grid.minor = element_blank()) +
        ylab("") +  xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("ED Visits"))




### ED #3

df_ED <- data.frame(yAxis = c("6M(All)", "6M(Mexican)", "6M(Non-MX)", 
                              "12M(All)","12M(Mexican)", "12M(Non-MX)"), 
                    boxCILow =  c(0.65, 0.63, 0.62, 0.66, 0.62, 0.66),
                    boxOdds =   c(0.78, 0.74, 0.99, 0.76, 0.72, 0.94),
                    boxCIHigh = c(0.93, 0.93, 1.58, 0.87, 0.83, 1.34))
# View(df_ED)

(p <- ggplot(df_ED, aes(x = boxOdds, y = yAxis)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        
        geom_errorbarh(data = filter(df_ED, yAxis=="6M(All)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="6M(All)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="6M(Mexican)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="6M(Mexican)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="6M(Non-MX)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_ED, yAxis=="6M(Non-MX)"), shape=2, size = 4.0, height = .15, color = "red") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="12M(All)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="12M(All)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="12M(Mexican)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED, yAxis=="12M(Mexican)"), size = 4.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED, yAxis=="12M(Non-MX)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_ED, yAxis=="12M(Non-MX)"), shape=2, size = 4.0, height = .15, color = "red") +
        
        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.6, 1.6, 0.2), labels = seq(0.6, 1.6, 0.2),
                     limits = c(0.6, 1.6)) +
        theme_bw()+ theme(panel.grid.minor = element_blank()) +
        ylab("") +  xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("ED Visits"))






#### ED by month (Final)

## 6m

df_EDM <- data.frame(yAxis = c("All Hispanics", "Mexican", "Non-Mexican"), 
                    boxCILow =  c(0.65, 0.63, 0.62),
                    boxOdds =   c(0.78, 0.74, 0.99),
                    boxCIHigh = c(0.93, 0.93, 1.58))

# View(df_EDM)

(p <- ggplot(df_EDM, aes(x = boxOdds, y = yAxis)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        scale_y_discrete(limits = unique(rev(df_EDM$yAxis))) + # Reverse order #
        
        geom_errorbarh(data = filter(df_EDM, yAxis=="All Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_EDM, yAxis=="All Hispanics"), size = 5.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_EDM, yAxis=="Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_EDM, yAxis=="Mexican"), size = 5.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_EDM, yAxis=="Non-Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_EDM, yAxis=="Non-Mexican"), shape=2, size = 5.0, height = .15, color = "red") +
        

        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.6, 1.6, 0.2), labels = seq(0.6, 1.6, 0.2),
                     limits = c(0.6, 1.6)) +
        theme_bw()+ theme(panel.grid.minor = element_blank()) +
        ylab("") +  xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("ED Visits - 6 Months"))


## 12m

df_ED12 <- data.frame(yAxis = c("All Hispanics", "Mexican", "Non-Mexican"), 
                     boxCILow =  c(0.66, 0.62, 0.66),
                     boxOdds =   c(0.76, 0.72, 0.94),
                     boxCIHigh = c(0.87, 0.83, 1.34))

(p <- ggplot(df_ED12, aes(x = boxOdds, y = yAxis)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        scale_y_discrete(limits = unique(rev(df_ED12$yAxis))) + # Reverse order #
        
        geom_errorbarh(data = filter(df_ED12, yAxis=="All Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED12, yAxis=="All Hispanics"), size = 5.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED12, yAxis=="Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .5,  height = .15,  color = "gray40") +
        geom_point(data =     filter(df_ED12, yAxis=="Mexican"), size = 5.0, height = .15, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_ED12, yAxis=="Non-Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_ED12, yAxis=="Non-Mexican"), shape=2, size = 5.0, height = .15, color = "red") +
        
        
        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.6, 1.6, 0.2), labels = seq(0.6, 1.6, 0.2),
                     limits = c(0.6, 1.6)) +
        theme_bw()+ theme(panel.grid.minor = element_blank()) +
        ylab("") +  xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("12 Months"))






##### OP ONLY



df_OP <- data.frame(yAxis = c("6M_All", "6M_Mexican", "6M_Non-MX", 
                              "12M_All","12M_Mexican", "12M_Non-MX"), 
                    boxCILow =  c(1.04, 1.04, 0.87, 0.97, 0.97, 0.81),
                    boxOdds =   c(1.14, 1.14, 1.13, 1.05, 1.06, 1.01),
                    boxCIHigh = c(1.24, 1.26, 1.45, 1.13, 1.15, 1.26))

# View(df_OP)

(p <- ggplot(df_OP, aes(x = boxOdds, y = yAxis)) +  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        
        geom_errorbarh(data = filter(df_OP, yAxis=="6M_All"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP, yAxis=="6M_All"), size = 4.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="6M_Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP, yAxis=="6M_Mexican"), size = 4.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="6M_Non-MX"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="6M_Non-MX"), shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="12M_All"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="12M_All"),  shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="12M_Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="12M_Mexican"), shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="12M_Non-MX"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="12M_Non-MX"), shape=2, size = 4.0, color = "red") +

        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.8, 1.6, 0.2), labels = seq(0.8, 1.6, 0.2),
                     limits = c(0.8, 1.6)) +
        theme_bw()+
        theme(panel.grid.minor = element_blank()) +
        ylab("") +
        xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("Outpatient Visits"))
 

## OP #2

df_OP <- data.frame(yAxis = c("All(6m)", "Mexican(6m)", "Non-MX(6m)", 
                              "All(12m)","Mexican(12m)", "Non-MX(12m)"), 
                    boxCILow =  c(1.04, 1.04, 0.87, 0.97, 0.97, 0.81),
                    boxOdds =   c(1.14, 1.14, 1.13, 1.05, 1.06, 1.01),
                    boxCIHigh = c(1.24, 1.26, 1.45, 1.13, 1.15, 1.26))

# View(df_OP)

(p <- ggplot(df_OP, aes(x = boxOdds, y = yAxis)) +  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        
        geom_errorbarh(data = filter(df_OP, yAxis=="All(6m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP, yAxis=="All(6m)"), size = 4.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="Mexican(6m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP, yAxis=="Mexican(6m)"), size = 4.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="Non-MX(6m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="Non-MX(6m)"), shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="All(12m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="All(12m)"),  shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="Mexican(12m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="Mexican(12m)"), shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="Non-MX(12m)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="Non-MX(12m)"), shape=2, size = 4.0, color = "red") +
        
        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.8, 1.6, 0.2), labels = seq(0.8, 1.6, 0.2),
                     limits = c(0.8, 1.6)) +
        theme_bw()+
        theme(panel.grid.minor = element_blank()) +
        ylab("") +
        xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("Outpatient Visits"))



## OP #3


df_OP <- data.frame(yAxis = c("6M(All)", "6M(Mexican)", "6M(Non-MX)", 
                              "12M(All)","12M(Mexican)", "12M(Non-MX)"), 
                    boxCILow =  c(1.04, 1.04, 0.87, 0.97, 0.97, 0.81),
                    boxOdds =   c(1.14, 1.14, 1.13, 1.05, 1.06, 1.01),
                    boxCIHigh = c(1.24, 1.26, 1.45, 1.13, 1.15, 1.26))

# View(df_OP)
df_OP$yAxis <- factor(df_OP$yAxis,levels=rev(unique(df_OP$yAxis)))

ggplot(df, aes(x = distanceRemaining, y = reorder(position, desc(position))))

(p <- ggplot(df_OP, aes(x = boxOdds, y = yAxis)) +  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        scale_y_discrete(limits = unique(rev(df_OP$yAxis))) + # Reverse order #

        geom_errorbarh(data = filter(df_OP, yAxis=="6M(All)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP, yAxis=="6M(All)"), size = 4.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="6M(Mexican)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP, yAxis=="6M(Mexican)"), size = 4.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="6M(Non-MX)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="6M(Non-MX)"), shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="12M(All)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="12M(All)"),  shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="12M(Mexican)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="12M(Mexican)"), shape=2, size = 4.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP, yAxis=="12M(Non-MX)"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP, yAxis=="12M(Non-MX)"), shape=2, size = 4.0, color = "red") +
        
        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.8, 1.6, 0.2), labels = seq(0.8, 1.6, 0.2),
                     limits = c(0.8, 1.6)) +
        theme_bw()+
        theme(panel.grid.minor = element_blank()) +
        ylab("") +
        xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("Outpatient Visits"))


## OP by month (Final)

### 6 months
df_OP6 <- data.frame(yAxis = c("All Hispanics", "Mexican", "Non-Mexican"), 
                    boxCILow =  c(1.04, 1.04, 0.87),
                    boxOdds =   c(1.14, 1.14, 1.13),
                    boxCIHigh = c(1.24, 1.26, 1.45))

(p <- ggplot(df_OP6, aes(x = boxOdds, y = yAxis)) +  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        scale_y_discrete(limits = unique(rev(df_OP6$yAxis))) + # Reverse order #
        
        geom_errorbarh(data = filter(df_OP6, yAxis=="All Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP6, yAxis=="All Hispanics"), size = 5.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP6, yAxis=="Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
        geom_point(data =     filter(df_OP6, yAxis=="Mexican"), size = 5.0, color = "midnight blue") +
        
        geom_errorbarh(data = filter(df_OP6, yAxis=="Non-Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP6, yAxis=="Non-Mexican"), shape=2, size = 5.0, color = "red") +
        

        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.8, 1.6, 0.2), labels = seq(0.8, 1.6, 0.2),
                     limits = c(0.8, 1.6)) +
        theme_bw()+
        theme(panel.grid.minor = element_blank()) +
        ylab("") +
        xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("Outpatient Visits - 6 Months"))


### 12 months

df_OP12 <- data.frame(yAxis = c("All Hispanics", "Mexican", "Non-Mexican"), 
                     boxCILow =  c(0.97, 0.97, 0.81),
                     boxOdds =   c(1.05, 1.06, 1.01),
                     boxCIHigh = c(1.13, 1.15, 1.26))

(p <- ggplot(df_OP12, aes(x = boxOdds, y = yAxis)) +  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        scale_y_discrete(limits = unique(rev(df_OP12$yAxis))) + # Reverse order #

        geom_errorbarh(data = filter(df_OP12, yAxis=="All Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP12, yAxis=="All Hispanics"),  shape=2, size = 5.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP12, yAxis=="Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP12, yAxis=="Mexican"), shape=2, size = 5.0, color = "red") +
        
        geom_errorbarh(data = filter(df_OP12, yAxis=="Non-Mexican"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
        geom_point(data =     filter(df_OP12, yAxis=="Non-Mexican"), shape=2, size = 5.0, color = "red") +

            
        coord_trans(x = scales:::exp_trans(2)) +
        scale_x_sqrt(breaks = seq(0.8, 1.6, 0.2), labels = seq(0.8, 1.6, 0.2),
                     limits = c(0.8, 1.6)) +
        theme_bw()+
        theme(panel.grid.minor = element_blank()) +
        ylab("") +
        xlab("Odds Ratio") +
        annotate(geom = "text", y =1.1, x = log10(1.5), 
                 label = "", size = 5, hjust = 0) + 
        ggtitle("12 Months"))

### END



