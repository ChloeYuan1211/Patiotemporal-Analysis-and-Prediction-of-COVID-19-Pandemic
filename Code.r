# 导入所需的库
library(tidyverse)  # 数据处理
library(lubridate)  # 处理日期时间数据
library(spdep)      # 空间数据处理
library(ggplot2)    # 数据可视化
library(readxl)
shanghai <- read_excel("E:/shanghai.xlsx")
# 导入数据
covid_data <- shanghai
# 将日期转换为日期格式
covid_data$Date <- as.Date(covid_data$Date)
# 进行自相关分析
# 计算上海市确诊病例数的自相关系数
acf_result <- acf(covid_data$总人数, lag.max = 10, plot = FALSE)
# 绘制自相关系数图
plot(acf_result, main = "Autocorrelation of Confirmed Cases in Shanghai", ylab = "Autocorrelation", xlab = "Lag")
# 进行密度估计
# 绘制上海市确诊病例数的密度估计图
ggplot(covid_data, aes(x = 总人数)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Estimation of Confirmed Cases in Shanghai", x = "Confirmed Cases", y = "Density")
library(reshape2)
library(ggplot2)
library(gganimate)
library(gifski)
library(rlang)
library(dplyr)
library(segmented)
#剔除N/A数据
Raw_Data <- na.omit(covid_data)
#计算每日每个区的累计数据，保存为Acc_Raw_Data
N <- dim(Raw_Data)[1]
Acc_Raw_Data <- Raw_Data
for(i in 2:N){Acc_Raw_Data[i,-1] <- Acc_Raw_Data[i-1,-1]+Raw_Data[i,-1]}
#用melt函数整理数据，命名为Acc_Data，包含日期、分区和累计数目三列变量
Acc_Data <- melt(Acc_Raw_Data,id="Date")
#用dplyr包对数据进行降序排序，生成新变量rank，整理后的数据保存为Acc_Data_New
Acc_Data_New <- Acc_Data %>% group_by(Date) %>% arrange(desc(value)) %>% 
  mutate(rank=row_number()) %>% ungroup()
#计算上海市每日新增人数total，将日期和total数据保存为Fit_Data
Raw_Data$total <- rowSums(Raw_Data[,2:17])
Fit_Data <- data.frame(Date=Raw_Data$Date,x=c(1:34),total=Raw_Data$total)
#画各区累计增长人数的动态条形图
#画简单静态条形图
plot1 <- ggplot(Acc_Data_New,aes(x=desc(rank),y=value,group=variable))+
  geom_bar(stat = "identity",aes(fill=as.factor(variable),color=as.factor(variable)))
#转换横轴和纵轴
plot1 <- plot1+coord_flip()
#添加数字标签
plot1 <- plot1+geom_text(aes(label=value,vjust=0.5,hjust=0,color=factor(variable)))
#添加分区标签
plot1 <- plot1+geom_text(aes(y=0,label=paste(variable,""),color=factor(variable)),
                         vjust=0.5,hjust=1,family="Songti SC",size=4)
#设置图表格式
plot1 <- plot1+theme(text = element_text(family="Songti SC"))
plot1 <- plot1+theme_classic()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),
        axis.title.x = element_blank(),axis.text.x = element_blank(),
        axis.text.y=element_blank(),axis.title.y = element_blank(),
        legend.position = "none",)
#生成动画
animate(plot1+transition_states(Date,transition_length = 2,state_length = 1),
        renderer = gifski_renderer(),width=1100,height=500,nframes = 200,fps = 8)
#画各区累计增长人数的静态柱状图
plot2 <- ggplot(Acc_Data,aes(x=Date,y=value,fill=variable))
plot2 <- plot2+geom_bar(stat = "identity",position = "stack")+facet_wrap(~variable)
plot2 <- plot2+theme_light()
#设置显示中文标签
plot2 <- plot2+theme(text = element_text(family = "Songti SC"))
#设置坐标轴标签的格式
plot2 <- plot2+theme(axis.text.x = element_text(family = "Times New Roman"),
            axis.text.y = element_text(family = "Times New Roman"))
#设置图例的格式
plot2 <- plot2+theme(legend.title = element_blank(),legend.text = element_text(size=10))
#设置横纵标签，图表标题等
plot2 <- plot2+labs(x="日期",y="累计增长人数",
                    title = "上海市各区累计新增病例数量统计图",tag = "Fig.2")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15),
        plot.tag = element_text(family = "Times New Roman"))
plot2
#画散点图
plot4 <- ggplot(Fit_Data,aes(x=x,y=total))+geom_point(size=3,colour="#f6d04d",alpha=0.7)
plot4 <- plot4+theme_light()
#设置显示中文标签
plot4 <- plot4+theme(text = element_text(family = "Songti SC"))
#设置坐标轴标签
plot4 <- plot4+scale_y_continuous(breaks = seq(0,30000,5000))+
  scale_x_continuous(breaks = seq(0,34,2))
plot4 <- plot4+theme(axis.text.x = element_text(family = "Times New Roman"),
                     axis.text.y = element_text(family = "Times New Roman"))
plot4 <- plot4+theme(axis.text.x = element_text(size=8,hjust = 0.5,vjust = 0.5))
#设置横纵标签，图表标题等
plot4 <- plot4+labs(x=element_blank(),y="每日新增人数",
                    title = "上海市每日新增病例数量统计图",tag = "Fig.4")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15),
        plot.tag = element_text(family = "Times New Roman"))
plot4
#用二次函数模型进行拟合
fit <- lm(total~x+I(x^2),data = Fit_Data)
#查看估计参数和显著性检验，三个参数均显著，R^2=0.8398
summary(fit)
#根据fit创建函数表达式f
f <- expression(y=-68.425*x^2+2117.742*x+7427.475)
#求f的一阶导数df
df <- deriv(f,"x",function.arg = TRUE)
#用分段函数模型进行拟合
model <- lm(total~x,data = Fit_Data)
segmented_fit <- segmented(model,seg.Z = ~x,psi = 15)
#查看回归方程显著性检验，估计出的拐点为10.335，R^2=0.8948
summary(segmented_fit)
#查看估计的斜率和截距
slope(segmented_fit)
intercept(segmented_fit)
#在图中添加二次函数拟合曲线
plot5 <- plot4+stat_smooth(method = "lm",formula = y~poly(x,2),se=TRUE,color="#716e77")
#在图中添加分段函数拟合曲线
plot5 <- plot5+stat_smooth(method = "gam",formula = y ~ x + I((x - 10.335) * (x > 10.335)),
                           se=TRUE,color="#4695d6")+
  geom_vline(xintercept = 10.335, linetype = 2, color = "#4695d6")
#添加二次函数表达式和R^2值
plot5 <- plot5+annotate("text",x=26,y=25000,parse=TRUE,
               label="y==-68.425*x^2+2117.742*x+7427.475",
               size=4.5,family="Times New Roman",color="#716e77")
plot5 <- plot5+annotate("text",x=26,y=22500,parse=TRUE,label="R^2==0.8398",
                        size=4.5,family="Times New Roman",color="#716e77")
#添加分段函数表达式和R^2值
plot5 <- plot5+annotate("text",x=6,y=5000,parse=TRUE,
                        label="y1==2391.5*x+3829.8",
                        size=4.5,family="Times New Roman",color="#4695d6")
plot5 <- plot5+annotate("text",x=6,y=2500,parse=TRUE,
                        label="y2==-954.84*x+38415",
                        size=4.5,family="Times New Roman",color="#4695d6")
plot5 <- plot5+annotate("text",x=6,y=0,parse=TRUE,label="R^2==0.8948",
                        size=4.5,family="Times New Roman",color="#4695d6")
#设置图表标题和标签等
plot5 <- plot5+labs(title = "上海市每日新增病例数量模型拟合图",tag = "Fig.5")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15),
        plot.tag = element_text(family = "Times New Roman"))
plot5
#ARIMA
#用4/10到5/4的数据进行时间序列分析
data<-shanghai[,c("Date","总人数")]
data$Date <-as.Date(as.character(data$Date),"%Y-%m-%d")
data1<-data[10:34,]
#时间序列图
ggplot(data1)+
  geom_line(aes(Date,总人数),size=0.5)+
scale_x_date(date_labels = "%m-%d", date_breaks = "1 day")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size=6))+
  labs(title = "上海市每日新增病例数的时间序列",
         x = "日期", 
       y = "新增人数")+
    theme(    
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())
#数据处理
ts<-xts(data1$总人数,order.by=data1$Date)
#ADF test 平稳性检验
summary(ur.df(ts,type="drift"))
#由于统计量等于0.173，大于5% 显著性水平下的临界值 -1.96， 因而我们可以认为序列是非平稳的。
#进行模型拆分，变为平稳
ts_diff <- diff(ts)
ts_diff<-na.omit(ts_diff)
summary(ur.df(ts_diff,type="drift"))
#由于统计量等于 -4.719，小于5% 显著性水平下的临界值 -1.96， 因而我们可以认为序列是平稳的。
#Ljung-Box
Box.test(ts,type="Ljung-Box",fitdf = 0, lag = 24)
#从上述结果我们得知，检验的 p 值<0.05，所以我们，认为序列不是白噪声序列。
#ACF 和PACF图
acf(ts_diff)
pacf(ts_diff)
#建立ARIMA模型
model<-auto.arima(diff(ts),stationary = T,seasonal = F)
summary(model)
#一阶拆分后模型ARIMA(0,0,1)
#ts数据的模型确定
model_new<- Arima(ts,order = c(0,1,1))
#模型系数和置信区间如下
model_new
confint(model_new)
#模型诊断
tsdiag(model_new)
#白噪声p > 0.05，则说明是白噪声序列，是纯随机性序列。
#动态预测，预测未来5天数据
 forecast(model_new,h=5)
 plot( forecast(model_new,h=5),col=2)
 #预测未来5天的数据
