
library(ggplot2)



####################################### AMR - Cut-point figures #######################################
### 02/18/22



df_AMR <- data.frame(yAxis = 
                       c(
                         "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", 
                         "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", 
                         "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", 
                         "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9" 
                       ),
                     grouping = c("Asians","Asians","Asians","Asians","Asians","Asians","Asians","Asians","Asians",
                                  "African Americans","African Americans","African Americans","African Americans",
                                  "African Americans","African Americans","African Americans","African Americans",
                                  "African Americans",
                                  "Hispanics","Hispanics","Hispanics","Hispanics","Hispanics",
                                  "Hispanics","Hispanics","Hispanics","Hispanics",
                                  "Whites","Whites","Whites","Whites","Whites","Whites","Whites","Whites","Whites"),
                     boxCILow =  c(0.32, 0.36, 0.30, 0.42, 0.31, 0.25, 0.21, 0.21, 0.21,
                                   0.35, 0.36, 0.34, 0.40, 0.27, 0.28, 0.30, 0.26, 0.21,
                                   0.50, 0.49, 0.48, 0.52, 0.51, 0.50, 0.47, 0.34, 0.27,
                                   0.24, 0.25, 0.24, 0.31, 0.29, 0.36, 0.21, 0.20, 0.21),
                     boxOdds =   c(0.98, 1.11, 0.88, 1.24, 0.90, 0.75, 0.54, 0.54, 0.47,
                                   0.57, 0.60, 0.56, 0.66, 0.48, 0.51, 0.58, 0.61, 0.39,
                                   0.67, 0.65, 0.64, 0.69, 0.68, 0.67, 0.65, 0.53, 0.47,  
                                   0.49, 0.52, 0.48, 0.64, 0.61, 0.76, 0.52, 0.58, 0.71),
                     boxCIHigh = c(3.01, 3.40, 2.60, 3.64, 2.65, 2.30, 1.99, 2.53, 3.80, 
                                   0.95, 0.99, 0.93, 1.10, 0.85, 0.93, 1.13, 1.46, 1.63,
                                   0.90, 0.86, 0.85, 0.91, 0.90, 0.90, 0.91, 0.82, 0.84,  
                                   1.01, 1.06, 0.99, 1.31, 1.29, 1.61, 1.29, 1.71, 2.40))


# View(df_AMR)



# Reverse order
df_AMR$yAxis <- factor(df_AMR$yAxis,levels=rev(unique(df_AMR$yAxis)))



## 6 months ##


(p <- ggplot(df_AMR, aes(x = boxOdds, y =yAxis)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_bar(stat = "identity") +
    facet_wrap(~grouping, scales="free") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.1" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.1" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.2" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.2" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.3" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.3" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.4" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.4" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.5" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.5" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.6" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.6" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.7" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.7" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.8" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.8" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.9" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.9" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.1" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.1"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.2" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.2"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.3" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.3"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.4" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.4"& grouping=="African Americans"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.5" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.5"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.6" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.6"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.7" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.7" & grouping=="African Americans"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.8" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.8" & grouping=="African Americans"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.9" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.9"& grouping=="African Americans"), size = 3, color = "dark orange") +
    
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.1" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.1"& grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.2" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.2" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.3" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.3" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.4" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.4" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.5" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.5" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.6" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.6" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.7" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.7" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.8" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.8" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.9" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.9" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.1" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.1"& grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.2" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.2" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.3" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR, yAxis=="0.3" & grouping=="Whites"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.4" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.4" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.5" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.5" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.6" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.6"& grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.7" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.7"& grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.8" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.8" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR, yAxis=="0.9" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR, yAxis=="0.9" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    
    scale_x_sqrt(breaks = seq(0.2, 4.0, 0.4), labels = seq(0.2, 4.0, 0.4),
                 limits = c(0.2, 4.0)) +
    
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("Cut-point") +
    xlab("Odds Ratio (log scale)") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "", size = 5, hjust = 0) + 
    ggtitle("6 months")
) 




## 12 months


df_AMR12 <- data.frame(yAxis = 
                         c(
                           "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", 
                           "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", 
                           "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", 
                           "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9" 
                         ),
                       grouping = c("Asians","Asians","Asians","Asians","Asians","Asians","Asians","Asians","Asians",
                                    "African Americans","African Americans","African Americans","African Americans",
                                    "African Americans","African Americans","African Americans","African Americans",
                                    "African Americans",
                                    "Hispanics","Hispanics","Hispanics","Hispanics","Hispanics",
                                    "Hispanics","Hispanics","Hispanics","Hispanics",
                                    "Whites","Whites","Whites","Whites","Whites","Whites","Whites","Whites","Whites"),
                       boxCILow =  c(0.36, 0.41, 0.34, 0.48, 0.38, 0.32, 0.22, 0.21, 0.21,
                                     0.41, 0.43, 0.41, 0.45, 0.36, 0.33, 0.38, 0.29, 0.21,
                                     0.60, 0.55, 0.53, 0.53, 0.49, 0.50, 0.44, 0.35, 0.24,
                                     0.38, 0.40, 0.40, 0.53, 0.45, 0.43, 0.30, 0.24, 0.21),
                       boxOdds =   c(1.08, 1.24, 0.99, 1.38, 1.06, 0.91, 0.72, 0.50, 0.42,
                                     0.60, 0.62, 0.60, 0.66, 0.55, 0.51, 0.62, 0.55, 0.38,
                                     0.76, 0.70, 0.67, 0.66, 0.61, 0.63, 0.58, 0.50, 0.39,  
                                     0.67, 0.71, 0.70, 0.92, 0.78, 0.77, 0.58, 0.55, 0.48),
                       boxCIHigh = c(3.27, 3.74, 2.86, 3.99, 3.00, 2.63, 2.35, 2.29, 3.36, 
                                     0.88, 0.91, 0.88, 0.97, 0.83, 0.79, 1.01, 1.10, 1.07,
                                     0.96, 0.87, 0.83, 0.82, 0.77, 0.79, 0.76, 0.70, 0.60,  
                                     1.19, 1.25, 1.23, 1.61, 1.37, 1.37, 1.14, 1.26, 1.39))


# View(df_AMR)



# Reverse order
df_AMR12$yAxis <- factor(df_AMR12$yAxis,levels=rev(unique(df_AMR$yAxis)))




(p <- ggplot(df_AMR12, aes(x = boxOdds, y =yAxis)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_bar(stat = "identity") +
    facet_wrap(~grouping, scales="free") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.1" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.1" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.2" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.2" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.3" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.3" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.4" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.4" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.5" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.5" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.6" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.6" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.7" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.7" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.8" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.8" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.9" & grouping=="Asians"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.9" & grouping=="Asians"), size = 3, color = "dark orange") +
    
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.1" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.1"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.2" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.2"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.3" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.3"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.4" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.4"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.5" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.5"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.6" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.6"& grouping=="African Americans"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.7" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.7" & grouping=="African Americans"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.8" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.8" & grouping=="African Americans"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.9" & grouping=="African Americans"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.9"& grouping=="African Americans"), size = 3, color = "dark orange") +
    
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.1" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.1"& grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.2" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.2" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.3" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.3" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.4" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.4" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.5" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.5" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.6" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.6" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.7" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.7" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.8" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.8" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.9" & grouping=="Hispanics"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40") +
    geom_point(data = filter(df_AMR12, yAxis=="0.9" & grouping=="Hispanics"), size = 3, color = "dark green") +
    
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.1" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.1"& grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.2" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.2" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.3" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.3" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.4" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.4" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.5" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.5" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.6" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.6"& grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.7" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.7"& grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.8" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.8" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    geom_errorbarh(data = filter(df_AMR12, yAxis=="0.9" & grouping=="Whites"), aes(xmax = boxCIHigh, xmin = boxCILow), size = .05, height = .15, color = "gray40", linetype = "dashed") +
    geom_point(data = filter(df_AMR12, yAxis=="0.9" & grouping=="Whites"), size = 3, color = "dark orange") +
    
    
    scale_x_sqrt(breaks = seq(0.2, 4.0, 0.4), labels = seq(0.2, 4.0, 0.4),
                 limits = c(0.2, 4.0)) +
    
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("Cut-point") +
    xlab("Odds Ratio (log scale)") +
    annotate(geom = "text", y =1.1, x = log10(1.5), 
             label = "", size = 5, hjust = 0) + 
    ggtitle("12 months")
) 






