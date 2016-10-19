setwd("~/Documents/CFPS2012adult")
#行数nrows
data <- read.csv("adult2012.csv", sep = ",", header = T)
library("caret")
y<- data$QN12012
#删去近似于常量的变量 1744->382
zerovar <- nearZeroVar(data,names=F)
newdata1<-data[,-zerovar]
#delete high correlation features，计算相关系数矩阵（descrCorr（i,j）表示newdata1第列和第j列的相关性）
descrCorr <- cor(newdata1) 
highCorr <- findCorrelation(descrCorr, cutoff=0.80) 
newdata2 <- newdata1[, -highCorr]

Process <- preProcess(newdata2)
newdata3 <- predict(Process, newdata2)
# 用sbf函数实施过滤方法，这里是用随机森林来评价变量的重要性 
data.filter <- sbf(newdata3,y, sbfControl = sbfControl(functions=rfSBF, verbose=F, method='cv')) 
# 根据上面的过滤器筛选出67个变量 
x <- newdata3[data.filter$optVariables] 
Profile$optVariables
# 再用rfe函数实施封装方法，建立的模型仍是随机森林 
profile <- rfe(x,y, sizes = c(10,20,30,50,60), rfeControl = rfeControl(functions=rfFuncs ,method='cv'))
 # 将结果绘图，发现20-30个变量的模型精度最高 
plot(profile,type=c('o','g'))



# #========================
# type <- function(x) 
# {
#   if (x < 1500 || x == 1500) return ("0~1500");
#   if (x < 3000 || x == 3000) return ("1500~3000");
#   return ("3000~");
# }
# data$type <- sapply(data$price,type)

# #=====================
# library("ggplot2")

# cate <- read.csv("app_category_all", sep = "\t", header = F)
# str(cate)
# colnames(cate) <- c("package", "category")
# p <- ggplot(data = cate)
# p + geom_bar(aes(x = category))
# p + geom_bar(aes(x = category, fill = category))
# p <- last_plot()
# p + coord_flip()

# ggplot(cate)+geom_bar(aes(x=factor(1), fill=category))+coord_polar(theta = "y")


# #========================
# p <- ggplot(data)
# p + geom_histogram(aes(x = price))

# p + geom_density(aes(x = price))
# p + geom_density(aes(x = price, color = type))
# p + geom_density(aes(x = price, fill = type))
# p + geom_density(aes(x = price, fill = type)) + scale_x_log10()

# #=========================================
# data$ft = data$ft_wifi + data$ft_mobile
# str(data)
# data$ft <- data$ft/(1000 * 60 * 60)
# data$ft <- round(data$ft, 3)
# str(data$ft)
# summary(data$ft)
# p <- ggplot(data = data, aes(x = price, y = ft))
# p + geom_point()+ stat_smooth()
# p + stat_smooth()
# p + stat_density2d(aes(fill = ..level..), geom= "polygon") + scale_fill_continuous(high='darkred',low='darkgreen')



# #========================
# p <- ggplot(data)
# p + geom_point(aes(x = price, y = count))
# p + geom_point(aes(x = price, y = count)) + scale_x_log10()

# p <- ggplot(data, aes(x = type, y = count))
# p + geom_boxplot()
# last_plot() + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white");





# system.time(sapply(data$price,type))
# testtime <- function(x) {
#   for (i in 1:nrow(x)) {
#     x$type[i] = type(x$price[i]);
#   }
# }
# system.time(testtime(data))

