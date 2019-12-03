library(tidyverse)
library(dplyr)
#Filter København
cities.data <- filter(cities.data, city == 'Copenhagen')

#cities.data <- cities.data[sample(nrow(cities.data), 100), ]

#Modeller
linreg1 <- lm(log(price) ~ log(size) + cond + selling.period + year.construct + balcony + renovation + year.sale, data = cities.data)

linreg0 <- lm(log(price) ~ log(size) + year.construct, year.sale, data = cities.data)

anova(linreg1, linreg0)

#Værdier
n <- 1182
dfmod1 <- n - 38
dfmod0 <- n - 6

#Funktionsværdier for pdf for F-dist
dist_df <- data.frame(
  x = seq(0.5, 1.5, by = 0.001),
  pdf = df(x = seq(0.5, 1.5, 0.001), df1 = dfmod1, df2 = dfmod0)
)

#Kritisk værdi
upper = qf(0.95, df1 = dfmod1, df2 = dfmod0);upper

#Cdf'en for F-dist
dist_qf <- data.frame(
  x2 = seq(0, 1, by = 0.01),
  cdf = qf(p = seq(0, 1, 0.01), df1 = dfmod1, df2 = dfmod0)
)

#Plot af cdf
ggplot(aes(x = x2, y = cdf), data = dist_qf) + 
  geom_line()

#Plot af pdf for F-dist og kritisk punkt
ggplot(aes(x = x, y = pdf), data = dist_df) + 
  geom_line() +
  #Markere signifikans niveau
  geom_area(data=dist_df[which(dist_df$x > upper),], aes(x=x, y=pdf), fill = "red", alpha = 0.6)

#F-statistic
#Residualer
RSS_model1 <- anova(linreg1,linreg0)$RSS[1]
RSS_model0 <- anova(linreg1,linreg0)$RSS[2]
#Selve F-statistiken
Fstat <- ((RSS_model1 - RSS_model0)/(dfmod1 - dfmod0))/((RSS_model1)/(dfmod1)); Fstat

#p-værdi
p <- pf(q = Fstat, df1 = dfmod0, df2 = dfmod1, lower.tail=F); p
#Det burde give
anova(linreg1, linreg0)$Pr[2]

#For at tjekke at den kritiske værdi er rigtig
pf(q = upper, df1 = dfmod1, df2 = dfmod0, lower.tail=F, log.p = F)



