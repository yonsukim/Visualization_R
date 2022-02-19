

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







