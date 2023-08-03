library(ggplot2)

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