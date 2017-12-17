setwd("C:/Users/angel/Desktop/github")
library(readr)
library(dplyr)
Dengue2 <- read_csv("C:/Users/angel/Desktop/github/Dengue.csv")
View(Dengue)

cy = Dengue %>% group_by(year) %>% summarise(count=sum(count))
cor(cy$count,cy$year)
cm = Dengue %>% group_by(month) %>% summarise(count=sum(count))
cor(cm$count,cm$month)

result1 = lm(cy$count ~ cy$year)
plot(cy$year, cy$count, pch = 16, cex = 1.3, col = "blue",
     xlab = "Year", ylab = "Number of Patient")
abline(result1)
result2 = lm(cm$count ~ cm$month)
plot(cm$month, cm$count, pch = 16, cex = 1.3, col = "blue",
     xlab = "Month", ylab = "Number of Patient")
abline(result2)


cg = Dengue %>% group_by(year,month,gender) %>% summarise(count=sum(count))
require(ggplot2)
ggplot(data = cg, aes(x = gender, y = log10(count))) +
  geom_boxplot() + coord_flip() +
  labs( y = 'Log10 of Patient Number', x = 'gender')
t.test(count ~ gender, data = cg)

ca = Dengue %>% group_by(year,month,age) %>% summarise(count=sum(count))
ca$age <- factor(ca$age, levels = c('0','1','2','3','4',
                                            '5-9','10-14','15-19',
                                            '20-24','25-29','30-34',
                                            '35-39','40-44','45-49',
                                            '50-54','55-59','60-64',
                                            '65-69','70+'))
tapply(ca$count, ca$age, mean)
library(Hmisc)
ggplot(data = ca, 
       aes(x = age, y = count)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(500, 660, by = 20)) +
  geom_hline(yintercept = mean(ca$age) , 
             linetype = 'dotted') +
  labs(x = '年齡層', y = '每月得登革熱的人數') +
  coord_flip()
anova(a <- lm(count ~ age, data = ca))

result = lm(cm$count ~ cm$month)
plot(cm$month, cm$count, pch = 16, cex = 1.3, col = "blue",
     xlab = "Month", ylab = "Number of Patient")
abline(result)

cc = Dengue %>% group_by(year,month,city) %>% summarise(count=sum(count))
anova(c <- lm(count ~ city, data = cc))
ggplot(data = cc, 
       aes(x = city, y = count)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(500, 660, by = 20)) +
  geom_hline(yintercept = mean(cc$city) , 
             linetype = 'dotted') +
  labs(x = '縣市', y = '每月得登革熱的人數') +
  coord_flip()

Dengue = Dengue[,-1]
library(e1071)
attach(Dengue)
Dengue$city = as.factor(city)
Dengue$gender = as.factor(gender)
Dengue$foreign = as.factor(foreign)
Dengue$age = as.factor(age)
testID = sample(1:nrow(Dengue),100,replace=FALSE)
x = subset(Dengue[testID,],select = -foreign)
y = Dengue$foreign[testID]
svm_model = svm(foreign ~., data = Dengue[-testID,])
pred = predict(svm_model, x)
t = table(pred,y)
ac = (t[1,1]+t1[2,2])/100
ac
