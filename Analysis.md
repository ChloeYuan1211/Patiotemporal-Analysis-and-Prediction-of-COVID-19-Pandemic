[TOC]

## 1. Abstract

This project aims to explore and predict the spread of the Covid-19 pandemic using spatiotemporal data analysis and data mining techniques.
I analyzed the Covid-19 dataset provided by the Shanghai Municipal Health Commission, employing various methods including descriptive exploratory analysis, model fitting, and machine learning to reveal the spatiotemporal patterns of the epidemic's spread.

## 2. Date source
Given China's vastness, we've chosen Shanghai—a leading economic hub known for dynamism, transparency, and innovation—for our study. Its COVID-19 data offers significant insights for model development. With China's outbreak under control since April 2022, our project focuses on data from April 1st to May 4th, a period rich in epidemic information.

Our data are from a dataset from ‘Shanghai-COVID-2022’ in GitHub, which is compiled using information from news reports provided by the Shanghai Municipal Health Commission.

## 3.	Processing
### 3.1 Exploratory spatiotemporal data analysis
In this part, I used methods such as autocorrelation analysis and density estimation to analyze the spatiotemporal patterns in the data. I explored information such as trends in epidemic transmission, high-risk areas, and spatiotemporal correlations.

The results of autocorrelation analysis show the autocorrelation coefficients of the total population sequence in the epidemic data at different lag stages. The autocorrelation coefficient of lag 0 is 1, indicating a complete correlation between oneself and oneself. This is because the daily number of confirmed cases has the strongest correlation with oneself, which is a fundamental property of autocorrelation analysis. The autocorrelation coefficient of lag 1 is 0.852, indicating a strong positive correlation between the current number of confirmed cases and the number of confirmed cases from the previous day. This indicates that the number of confirmed cases on the previous day has a significant impact on the number of confirmed cases on that day. Similarly, as the number of lag periods increases, the autocorrelation coefficient gradually decreases, indicating that the correlation between the current number of confirmed cases and the number of confirmed cases at a longer time point in the past is becoming weaker. This autocorrelation analysis result indicates that there is a certain time correlation between the number of confirmed cases in the epidemic data, that is, the number of confirmed cases in the past period will have an impact on the current number of confirmed cases, but as the time interval increases, the correlation gradually weakens (shown in figure 1)

<div style="text-align: center">
  <img src="https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%201%20Autocorrelation%20of%20Confirmed%20Cases%20in%20Shanghai.png" alt="figure5" style="width: 70%; height: auto;">
</div>


<div style="text-align: center">
  <img src="https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%202%20Density%20Estimation%20of%20Confirmed%20Cases%20in%20Shanghai.png" alt="figure5" style="width: 70%; height: auto;">
</div>

```r
# Import the required libraries
library(tidyverse)  
library(lubridate)  
library(spdep)      
library(ggplot2)   
library(readxl)
library(tseries)
library(xts)
library(urca)
library(forecast)
```

```r
# Import the data
shanghai <- read_excel("shanghai.xlsx")
colnames(shanghai)<-c("Date","Xuhui","Songjiang","Huangpu","Jiading","Hongkou","Yangpu","Putuo","Qingpu","Jingan", "Jinshan","Fengxian","Chongming","Changning","Baoshan","minhang","Pudong new","Total")
covid_data <- shanghai
covid_data$Date <- as.Date(covid_data$Date)
```
```r
# Perform autocorrelation analysis
acf_result <- acf(covid_data$Total, lag.max = 10, plot = FALSE)
plot(acf_result, main = "Autocorrelation of Confirmed Cases in Shanghai", ylab = "Autocorrelation", xlab = "Lag")

# Perform density estimation
ggplot(covid_data, aes(x = Total)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Estimation of Confirmed Cases in Shanghai", x = "Confirmed Cases", y = "Density")


```

### 3.2 Data organization and visualization
To effectively illustrate Shanghai's daily pandemic scenario during this period, I have visualized the daily pandemic data. I have read the pandemic district data for Shanghai, removed missing data, and defined the date data as a date type. I calculated the cumulative data for each district on a given day and reconstructed and sorted the data. I also calculated the daily new data for Shanghai and plotted a dynamic chart showing the cumulative growth of daily data for each district in Shanghai.

![figure3](https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%203%20Dynamic%20chart%20of%20daily%20cumulative%20growth%20data%20for%20each%20district%20in%20Shanghai..gif) 

From the chart, it can be observed that the Pudong New Area has the most severe pandemic situation, with the number of infected individuals far exceeding those in other districts. Minhang District follows with the next most serious situation; Huangpu District and Baoshan District have relatively faster growth rates. The districts with the mildest pandemic situations are Fengxian District, Chongming District, and Jinshan District, followed by Qingpu District and Changning District.

<div style="text-align: center">
  <img src="https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%204%20Statistical%20plot%20of%20Cumulative%20Number%20of%20New%20Cases%20in%20Each%20District.png" alt="figure5" style="width: 70%; height: auto;">
</div>

This chart showed that from April 1st to May 4th, the cumulative increase in the number of infected individuals in Shanghai has exceeded 500000. The period from April 12th to April 20th saw the fastest growth, while the growth rate after April 25th was relatively small.

```r
# Import the required libraries
library(reshape2)
library(ggplot2)
library(gganimate)
library(gifski)
library(rlang)
library(dplyr)
library(segmented)
```
```r
# Remove N/A data
Raw_Data <- na.omit(covid_data)

# Calculate the cumulative data
N <- dim(Raw_Data)[1]
Acc_Raw_Data <- Raw_Data
for(i in 2:N){Acc_Raw_Data[i,-1] <- Acc_Raw_Data[i-1,-1]+Raw_Data[i,-1]}

# tidy the data
Acc_Data <- melt(Acc_Raw_Data,id="Date")

# sort the data
Acc_Data_New <- Acc_Data %>% group_by(Date) %>% arrange(desc(value)) %>% 
  mutate(rank=row_number()) %>% ungroup()

# Calculate the new daily cases
Raw_Data$total <- rowSums(Raw_Data[,2:17])
Fit_Data <- data.frame(Date=Raw_Data$Date,x=c(1:34),total=Raw_Data$total)
```
```r
# Draw the dynamic bar chart
plot1 <- ggplot(Acc_Data_New,aes(x=desc(rank),y=value,group=variable))+
  geom_bar(stat = "identity",aes(fill=as.factor(variable),color=as.factor(variable)))
plot1 <- plot1+coord_flip()

plot1 <- plot1+geom_text(aes(label=value,vjust=0.5,hjust=0,color=factor(variable)))
plot1 <- plot1+geom_text(aes(y=0,label=paste(variable,""),color=factor(variable)),
                         vjust=0.5,hjust=1,size=4)

plot1 <- plot1+theme_classic()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),
        axis.title.x = element_blank(),axis.text.x = element_blank(),
        axis.text.y=element_blank(),axis.title.y = element_blank(),
        legend.position = "none",)

animate(plot1+transition_states(Date,transition_length = 2,state_length = 1),
        renderer = gifski_renderer(),width=1100,height=500,nframes = 200,fps = 8)
```
```r
# Draw the static bar chart of the cumulative increase in each district
colnames(Acc_Raw_Data)<-c("Date","Xuhui distric","Songjiang distric","Huangpu distric","Jiading distric","Hongkou distric","Yangpu distric","Putuo distric","Qingpu distric","Jingan distric", "Jinshan distric","Fengxian distric","Chongming distric","Changning distric","Baoshan distric","minhang distric","Pudong new distric","Total")
Acc_Data <- melt(Acc_Raw_Data,id="Date")
Acc_Data$Date<-as.Date(as.character(Acc_Data$Date),"%Y-%m-%d")
plot2 <- ggplot(Acc_Data,aes(x=Date,y=value,fill=variable))+
  scale_x_date(date_labels = "%m-%d", date_breaks = "10 days")
plot2 <- plot2+geom_bar(stat = "identity",position = "stack")+facet_wrap(~variable)
plot2 <- plot2+theme_light()


plot2 <- plot2+theme(legend.title = element_blank(),legend.text = element_text(size=10))

plot2 <- plot2+labs(x="Date",y="Cumulative increase",
                    title = "Statistical plot of Cumulative Number of New Cases in Each District of Shanghai City ",tag = "Fig.2")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 15))

plot2

```

### 3.3 Model fitting and prediction
Next, I drew a scatter plot of daily new data in Shanghai (Figure 5) and selected an appropriate mathematical model.<br>
<div style="text-align: center">
  <img src="https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%205%20Statistical%20plot%20of%20Daily%20Cumulative%20Number%20of%20New%20Cases%20in%20Shanghai.png" alt="figure5" style="width: 70%; height: auto;">
</div><br>

The test results for the regression equation and parameters are significant. This quadratic function model can explain about 84% of the variation in the original data. To calculate the derivative of the quadratic function mentioned above, x=15 or x=16, i.e. April 15th or April 16th, can be used as the turning point of this epidemic. This piecewise function model can explain about 89% of the variation in the original data, and its estimated result is x=10, which means around April 10th is the turning point of the epidemic. Visualized the model. (Figure 6)

<div style="text-align: center">
  <img src="https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%206%20Model%20Fitting%20of%20Daily%20Cumulative%20Number%20of%20New%20Cases.png" alt="figure5" style="width: 70%; height: auto;">
</div><br>


Based on the analysis above, it can be concluded that the piecewise function model fits data more accurately than the quadratic function model. Consequently, the turning point of the current epidemic in Shanghai is more likely to occur around April 10th, which is the first half of April. The daily increase in population reaches its peak before and after April 10th, and then progressively declining after that. Following the appearance of the turning point, there was a minor rebound on April 13, 16, and 22, but the daily increase progressively declined until April 28.

```r
# Draw scatter plot
plot4 <- ggplot(Fit_Data, aes(x=x, y=total)) + geom_point(size=3, colour="#f6d04d", alpha=0.7)
plot4 <- plot4 + theme_light()

plot4 <- plot4 + theme(text = element_text(family = "Songti SC"))

plot4 <- plot4 + scale_y_continuous(breaks = seq(0, 30000, 5000)) +
  scale_x_continuous(breaks = seq(0, 34, 2))
plot4 <- plot4 + theme(axis.text.x = element_text(family = "Times New Roman"),
                     axis.text.y = element_text(family = "Times New Roman"))
plot4 <- plot4 + theme(axis.text.x = element_text(size=8, hjust = 0.5, vjust = 0.5))
plot4 <- plot4 + labs(x=element_blank(), y="Daily New Cases",
                    title = "Shanghai Daily New Case Count Statistics", tag = "Fig.4") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.tag = element_text(family = "Times New Roman"))
plot4
```
```r
# Fit with quadratic model
fit <- lm(total ~ x + I(x^2), data = Fit_Data)

# View estimated parameters and significance test
summary(fit)

# Create function expression f
f <- expression(y = -68.425 * x^2 + 2117.742 * x + 7427.475)
df <- deriv(f, "x", function.arg = TRUE)
```
```r
# Fit with piecewise function model
model <- lm(total ~ x, data = Fit_Data)
segmented_fit <- segmented(model, seg.Z = ~x, psi = 15)

# View regression equation significance test
summary(segmented_fit)

slope(segmented_fit)
intercept(segmented_fit)

# Add function curve to the plot
plot5 <- plot4 + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se=TRUE, color="#716e77")
plot5 <- plot5 + stat_smooth(method = "gam", formula = y ~ x + I((x - 10.335) * (x > 10.335)),
                           se=TRUE, color="#4695d6") +
  geom_vline(xintercept = 10.335, linetype = 2, color = "#4695d6")
plot5 <- plot5 + annotate("text", x=26, y=25000, parse=TRUE,
               label="y=-68.425*x^2+2117.742*x+7427.475",
               size=4.5, family="Times New Roman", color="#716e77")
plot5 <- plot5 + annotate("text", x=26, y=22500, parse=TRUE, label="R^2=0.8398",
                        size=4.5, family="Times New Roman", color="#716e77")

plot5 <- plot5 + annotate("text", x=6, y=5000, parse=TRUE,
                        label="y1=2391.5*x+3829.8",
                        size=4.5, family="Times New Roman", color="#4695d6")
plot5 <- plot5 + annotate("text", x=6, y=2500, parse=TRUE,
                        label="y2=-954.84*x+38415",
                        size=4.5, family="Times New Roman", color="#4695d6")
plot5 <- plot5 + annotate("text", x=6, y=0, parse=TRUE, label="R^2=0.8948",
                        size=4.5, family="Times New Roman", color="#4695d6")
plot5 <- plot5 + labs(title = "Shanghai Daily New Case Count Model Fitting Chart", tag = "Fig.5") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.tag = element_text(family = "Times New Roman"))
plot5
```
