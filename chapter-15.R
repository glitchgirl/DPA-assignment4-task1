install.packages(c("forecast", "tseries", "xts", "ggplot2",
                 "patchwork", "directlabels"))

library(xts)
sales = c(18, 33, 41,  7, 34, 35, 24, 25, 24, 21, 25, 20, 
          22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
date = seq(as.Date("2018/1/1"), as.Date("2019/12/1"), by="month")

sales.xts <- xts(sales, date)
