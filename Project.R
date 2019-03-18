library(ggplot2)
data1 = read.csv('BA_PROJECT.csv')
data1  = data1[,-c(1,3,4,5,7,8,10,11,12,34)]
typeof(data1$Country)
data1$logGDP = log(data1$GDP..constant.2010.US..)
plot1 <- ggplot(data1, aes(x=logGDP, y=Duration)) +
  geom_bar(stat = "identity") 
plot1

plot(r, data1$Duration)
