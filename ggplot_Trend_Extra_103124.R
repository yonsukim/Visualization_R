

ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DIFFB), size=0.7, lty=2) +  
  geom_smooth(aes(y = DIFFB), method = "lm", se = FALSE, linetype = "solid") +  # Trend line for Series 1
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "Year", y = "Difference", color = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"))


ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DIFFA), size=0.7, lty=2) +  
  geom_smooth(aes(y = DIFFA), method = "lm", se = FALSE, linetype = "solid") +  # Trend line for Series 1
  scale_linetype_manual(values = c("solid", "dotted")) +
  #scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  labs( x = "Year", y = "Difference", color = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"))


ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DIFFH), size=0.7, lty=2) +  
  geom_smooth(aes(y = DIFFH), method = "lm", se = FALSE, linetype = "solid") +  # Trend line for Series 1
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "Year", y = "Difference", color = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"))



geom_smooth(method = "lm", se=F ) +
  geom_line(aes(y = DIFFH, color="Hispanic-White"), size=0.7, lty=2) +  
  geom_line(aes(y = DIFFA, color="Asian-White"), size=0.7, lty=2) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "Year", y = "Count per 100K", color = "", linetype="") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"))


ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DIFFB, color="African American-White"), size=0.5, lty=1) +  
  geom_line(aes(y = DIFFH, color="Hispanic-White"), size=0.7, lty=2) +  
  geom_line(aes(y = DIFFA, color="Asian-White"), size=0.7, lty=2) +  
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs( x = "Year", y = "Count per 100K", color = "", linetype="") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"))




########## Figure 2: T2DM per 100K ##############

ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = DMB, color="African American"), size=0.5, lty=1) +  
  geom_line(aes(y = DMW, color="White"), size=0.5, lty=1) +  
  geom_line(aes(y = DMH, color="Hispanic"), size=0.5, lty=2) +  
  geom_line(aes(y = DMA, color="Asian"), size=0.9, lty=3) +  
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  labs( x = "Year", y = "Count per 100K", color = "", linetype="") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"))












ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = ASB, linetype="African American"), size=0.5) +  
  geom_line(aes(y = ASW, linetype="White"), size=0.5) +  
  geom_line(aes(y = ASH, linetype="Hispanic"), size=0.7) +  
  geom_line(aes(y = ASA, linetype="Asian"), size=0.7) +  
  scale_color_manual(values = c("African American" = "blue4", "White" = "red2", "HIspanic" = "brown3", "Asian" = "green4")) +  # Custom colors for lines
  scale_linetype_manual(values = c("solid", "dotted", "dotted", "solid")) +
  labs( x = "Year", y = "Count per 100K", color = "", linetype="") +
  theme_minimal() 




scale_color_manual(values = c("Hispanic" = "brown3", "Asian" = "green4")) +  # Custom colors for lines
  
  
  
  # Sample data
  df <- data.frame(
    date = seq(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month"),
    value1 = c(10, 15, 9, 17, 12, 14, 18, 20, 15, 13, 16, 19),
    value2 = c(12, 18, 14, 19, 11, 17, 21, 23, 17, 16, 14, 20)
  )

ggplot(df, aes(x = date)) +
  geom_line(aes(y = value1, color = "Series 1", linetype = "Solid"), size = 1.5) +
  geom_line(aes(y = value2, color = "Series 2", linetype = "Dashed"), size = 1.5) +
  scale_color_manual(values = c("Series 1" = "blue", "Series 2" = "red")) +  # Custom colors for lines
  scale_linetype_manual(values = c("Solid" = "solid", "Dashed" = "dashed")) +  # Custom linetypes
  labs(title = "Time Series Plot with Custom Colors and Linetypes", x = "Date", y = "Value") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),  # Remove major grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor grid lines
        axis.line = element_line(colour = "black"))


,  # Keep axis lines
panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
panel.grid.minor.x = element_blank())  # Remove vertical minor grid lines, keep horizontal


theme_minimal() +
  theme(panel.grid.minor = element_blank()),   # Remove major grid lines
panel.grid.minor = element_blank()) 



ggplot(df, aes(x = date)) +
  geom_line(aes(y = value1, color = "Series 1", linetype = "Solid"), size = 1.5) +
  geom_line(aes(y = value2, color = "Series 2", linetype = "Dashed"), size = 1.5) +
  scale_linetype_manual(values = c("Solid" = "solid", "Dashed" = "dashed")) +  # Custom linetypes
  labs(title = "Time Series Plot with Custom Colors and Linetypes", x = "Date", y = "Value") +
  theme_minimal()




ggplot(AD, aes(x = DATE)) +
  geom_line(aes(y = ASW, linetype="White"), size=1) +  
  geom_line(aes(y = ASB, linetype="African American"), size=0.5) +  
  geom_line(aes(y = ASH, linetype="Hispanic"), size=1) +  
  geom_line(aes(y = ASA, linetype="Asian"), size=1) +  
  scale_color_manual(values = c("red4","blue4", "green4", "purple4"))  +
  labs( x = "Year", y = "Count per 100K") +
  theme_minimal()


# 10/21/2024



geom_line(aes(y = ASH, linetype="Hispanic"), color = "green4", Ity=3) +  
  geom_line(aes(y = ASB, color = "African American")) +
  geom_line(aes(y = ASH, color = "Hispanic")) +
  geom_line(aes(y = ASA, color = "Asian")) +
  scale_color_manual(name = "Legend", 
                     values = c("White" = "blue4", "African American" = "red4", "Hispanic" = "green4", "African American" = "purple4")) + 
  scale_linetype_manual(values = c("solid", "dotted", "longdash", "dotdash")) +
  
  
  labs(title = "Combined Time Series Trend", x = "Date", y = "Value") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +  # Customize x-axis
  scale_color_manual(name = "Legend", values = c("Series 1" = "blue", "Series 2" = "red")) + 
  theme_minimal()

ggplot(df, aes(x = date)) +
  geom_line(aes(y = value1, linetype = "Solid"), color = "blue", lty = 1) +  # Solid line
  geom_line(aes(y = value2, linetype = "Dashed"), color = "red", lty = 2) +  # Dashed line
  labs(title = "Different Line Types in ggplot", x = "Date", y = "Value") +
  theme_minimal() +
  scale_linetype_manual(name = "Line Type", values = c("Solid" = 1, "Dashed" = 2))







# Plot the monthly time series with a trend line
ggplot(AD, aes(x = Date, y = ASW)) +
  geom_line() +         # Plot the time series line
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear trend line
  labs(title = "Monthly Time Series with Trend Line", x = "Month", y = "Value") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +  # Customize x-axis format
  theme_minimal()





ggplot(data=AD, aes(x=Date, y=ASW, group=1)) +
  # geom_line()+ 
  geom_point()

ggplot(AD) +  
  aes(x = yrmo, y = ASW) +   
  # geom_smooth(aes(linetype = Race), method = "lm",  se = FALSE) + 
  xlab("Month") + ylab("# Per 100,000") +
  # scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c(" Intermittent", "", "", "Severe")) +
  scale_y_continuous(breaks = c(45000, 50000, 55000, 60000, 65000)) +
  # scale_color_manual(values = c("red4", "blue4")) +
  # scale_linetype_manual(values = c("solid", "dotted")) +
  ggtitle("AD") +theme(plot.title = element_text(hjust=0.5)) +
  #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) 




qplot(x = ASW, y = yrmo, facets = ~Race, data = IP3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = IP3, color = Race) +  geom_smooth(method = "lm") 

## Cost
IP2 <- IP[IP$Race == "3-Black" | IP$Race == "4-White" , ]
IP3 <- IP2[IP2$Sev == 1 | IP2$Sev == "4" ,]
#IP3 <- IP2[IP2$Sev != 0 ,]
#IP4 <- IP3[IP3$Tcost <= 50000,]

IP3["Race"][IP3["Race"] == "3-Black"] <- "African Americans"
IP3["Race"][IP3["Race"] == "4-White"] <- "Whites"
IP3["Race"][IP3["Race"] == "5-Hispanic"] <- "Hispanics"



#ggplot(tips) +  
#aes(x = total_bill, y = tip, color = sex) +   geom_point(color = "grey") +   geom_smooth(method = "lm", se=F )

ggplot(IP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm", se=F )
#aes(x = total_bill, y = tip, color = sex) +   geom_point(color = "grey") +   geom_smooth(method = "lm")


# BEST 
## Delete se=FALSE to include CI band

ggplot(IP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(aes(linetype = Race), method = "lm",  se = FALSE) + xlab("Asthma Severity") + ylab("Direct Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c(" Intermittent", "", "", "Severe")) +
  scale_y_continuous(breaks = c(45000, 50000, 55000, 60000, 65000), labels = c("$45,000", "$50,000", "$55,000", "$60,000", "$65,000")) +
  scale_color_manual(values = c("red4", "blue4")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  ggtitle("IP") +theme(plot.title = element_text(hjust=0.5)) +
  #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) 







