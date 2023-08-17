
install.packages("readxl")
library(ggplot2)
library(readxl)


########################  GI-ED (8/3/23) #################################


GI <- read_excel("C:/Users/.../Dropbox/Research/GI-ED/Data/data_20230803.xlsx")
View(GI)

# Creating ED visits per 100,000
GI$naut <- GI$nau_r*1000


# Mean for Pre and Post
condition1 <- GI$RCM ==0
pre <- subset(GI, condition1)
View(pre)

condition2 <- GI$RCM ==1
post <- subset(GI, condition2)
View(post)

average01 <- mean(GI$NAU)
average02 <- mean(pre$NAU)
average03 <- mean(post$NAU)

average11 <- mean(GI$naut)
average12 <- mean(pre$naut)
average13 <- mean(post$naut)


# Below, both work
GI$time <- as.Date(GI$time, format = "%m/%d/%Y")
GI$time <- as.Date(paste0(GI$time, "-01-01"))
class(GI$time)
cutoff <- as.Date("2017/7/01")


## Segmented trend for # ED visits

ggplot(GI, aes(time, naut)) +
  labs(x = "Year") + labs(y = "CHS ED visits (per 100,000 ED visits)" ) +
#  geom_line() + 
  geom_point() +
  geom_smooth(aes(group = time >= cutoff), method = "lm") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) 


ggplot(GI, aes(time, naut)) +
  labs(x = "Year") + labs(y = "CHS ED visits (per 100,000 ED visits)" )+
  geom_line() + 
# geom_point() +
  geom_smooth(aes(group = time >= cutoff), method = "lm") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) 




################################ Opioid (8/3/23) #################################


# TS <- read_excel("C:/Users/.../Dropbox/Research/Cannabis_Opioid/Data/TS.xlsx")
TS <- read_excel("C:/Users/Dongwoo/Dropbox/Research/Cannabis_Opioid/Data/TS.xlsx", sheet="TS_20230816")
View(TS)

TS$time <- as.Date(TS$time, format = "%m/%d/%Y")
TS$time <- as.Date(paste0(TS$time, "-01-01"))
class(TS$time)
cutoff <- as.Date("2016/10/01")



## Segmented trend for # ED visits

### ED visits per 100k

ggplot(TS, aes(time, opi_t)) +
   labs(x = "Year") + labs(y = "Opiod ED visits (per 100K)") +
  geom_line(aes(y=opi_t), color="darkblue", linetype='solid') +
  geom_point(aes(y=opi_t), color='darkblue', size=3) +  
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) 


## Count

ggplot(TS, aes(time, OPIW)) +
  labs(x = "Year") + labs(y = "# Opioid ED Visits") +
  geom_line(aes(y=OPIW), color="darkblue", linetype='solid', lwd=0.5) +
  geom_point(color='darkblue', size=3) +  
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7)



### Differencing 

## per 100k

ggplot(TS, aes(time, opi_d)) +
  labs(x = "Year") + 
  #theme(axis.title.x = element_text(face = "bold")) +
  labs(y = "ED Visits per 100K (Differencing)") +
  #theme(axis.title.y = element_text(face = "bold")) +
  geom_line(aes(y=opi_d), color="darkblue", linetype='solid', lwd=0.5) +
  geom_point(color='darkblue', size=3) +  
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7)


## count

ggplot(TS, aes(time, OPIW_d)) +
  labs(x = "Year") + labs(y = "# opioid ED Visits (Differencing)") +
  geom_line(aes(y=OPIW_d), color="darkblue", linetype='solid', lwd=0.5) +
  geom_point(color='darkblue', size=3) +  
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7)







## Trends of multiple symptoms

# NIC is too high

ggplot(TS, aes(time, opi_i, can_i, alc_i, nic_i, sed_i, coc_i, show_i)) +
  labs(x = "Year") + labs(y = "ED Visits per 100K") +
  geom_line(aes(y=opi_i), color="darkred") +
  #geom_line(aes(y=can_i), color="blue") +
  geom_line(aes(y=alc_i), color="green") +
  #geom_line(aes(y=nic_i), color="red") +
  #geom_line(aes(y=coc_i), color="darkgreen") +
  #geom_line(aes(y=sho_i), color="black") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )



ggplot(TS, aes(time, can_t, opi_t)) +
  labs(x = "Year") + labs(y = "ED Visits per 100K") +
  geom_line(aes(y=opi_t), color="darkred") +
  geom_line(aes(y=can_t), color="darkblue") +
  geom_line(aes(y=sho_t), color="darkblue") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )




ggplot(TS, aes(time, can_t, opi_t)) +
  labs(x = "Year") + labs(y = "ED Visits per 100K") +
  geom_line(aes(y=nic_t), color="darkred") +
  geom_line(aes(y=alc_t), color="darkblue") +
  geom_line(aes(y=coc_t), color="darkblue") +
  geom_line(aes(y=sed_t), color="darkblue") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )



ggplot(TS, aes(time, OPIW, CAN, ALC, SED, COC)) +
  labs(x = "Year") + labs(y = "ED Visits per 100K") +
  geom_line(aes(y=OPIW), color="darkred") +
  geom_line(aes(y=CANW), color="darkblue") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )




ggplot(TS, aes(time, OPIW, CAN, ALC, SED, COC)) +
  labs(x = "Year") + labs(y = "ED Visits per 100K") +
  geom_line(aes(y=canw_t), color="darkblue") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )




ggplot(TS, aes(time, OPIW, CAN, NIC, ALC, SED, COC)) +
  labs(x = "Year") + labs(y = "# ED visits") +
  geom_line() +
  geom_smooth(aes(group = time >= cutoff), method = "lm") +
  # Remove CI
  #  geom_smooth(se=F, aes(group = time >= cutoff), method = "lm") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
  # Change background and grid color
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )




## Segmented trend for # ED visits

ggplot(TS, aes(time, OPIW)) +
   labs(x = "Year") + labs(y = "Opiod ED visits") +
   geom_line() +
   geom_smooth(aes(group = time >= cutoff), method = "lm") +
# Remove CI
#  geom_smooth(se=F, aes(group = time >= cutoff), method = "lm") +
   geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
# Change background and grid color
   theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )



## Segmented trend for rate of ED visits

# my_Plot <-
ggplot(TS, aes(time, opiw_r)) +
   labs(x = "Year") + labs(y = "Opioid ED visit (per 100,000 ED visits)" ) +
   geom_line() +
   geom_smooth(aes(group = time >= cutoff), method = "lm") +
   geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) +
   theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
   )


# remove se=F to remove CI areas

# my_plot + geom_smooth(color = "black")


## Cannabis   
ggplot(TS, aes(time, canw_r)) +
  labs(x = "Year") + labs(y = "Opioid ED visits (per 100,000 ED visits") +
  geom_line() +
  geom_smooth(aes(group = time >= cutoff), method = "lm") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) 


ggplot(TS, aes(time, CANW)) +
  labs(x = "Year") + labs(y = "Opioid ED visits (per 100,000 ED visits") +
  geom_line() +
  geom_smooth(aes(group = time >= cutoff), method = "lm") +
  geom_vline(xintercept = cutoff, linetype="dashed", color = "darkred", size=0.7) 

   







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



