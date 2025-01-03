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

![image](https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%201%20Autocorrelation%20of%20Confirmed%20Cases%20in%20Shanghai.png)

![image](https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%202%20Density%20Estimation%20of%20Confirmed%20Cases%20in%20Shanghai.png)

```{r}
# Import the required libraries
library(tidyverse)  # For data manipulation
library(lubridate)  # For handling date and time data
library(spdep)      # For spatial data processing
library(ggplot2)    # For data visualization
library(readxl)
library(tseries)
library(xts)
library(urca)
library(forecast)
shanghai <- read_excel("shanghai.xlsx")
# Import the data

colnames(shanghai)<-c("Date","Xuhui","Songjiang","Huangpu","Jiading","Hongkou","Yangpu","Putuo","Qingpu","Jingan", "Jinshan","Fengxian","Chongming","Changning","Baoshan","minhang","Pudong new","Total")
covid_data <- shanghai
# Convert the date column to date format
covid_data$Date <- as.Date(covid_data$Date)

# Perform autocorrelation analysis
# Calculate the autocorrelation coefficients for the total number of confirmed cases in Shanghai
acf_result <- acf(covid_data$Total, lag.max = 10, plot = FALSE)

# Plot the autocorrelation coefficients
plot(acf_result, main = "Autocorrelation of Confirmed Cases in Shanghai", ylab = "Autocorrelation", xlab = "Lag")

# Perform density estimation
# Plot the density estimation for the total number of confirmed cases in Shanghai
ggplot(covid_data, aes(x = Total)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Estimation of Confirmed Cases in Shanghai", x = "Confirmed Cases", y = "Density")


```

### 3.2 Data organization and visualization
To effectively illustrate Shanghai's daily pandemic scenario during this period, I have visualized the daily pandemic data. I have read the pandemic district data for Shanghai, removed missing data, and defined the date data as a date type. I calculated the cumulative data for each district on a given day and reconstructed and sorted the data. I also calculated the daily new data for Shanghai and plotted a dynamic chart showing the cumulative growth of daily data for each district in Shanghai.

![image](https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%203%20Dynamic%20chart%20of%20daily%20cumulative%20growth%20data%20for%20each%20district%20in%20Shanghai..gif)

From the chart, it can be observed that the Pudong New Area has the most severe pandemic situation, with the number of infected individuals far exceeding those in other districts. Minhang District follows with the next most serious situation; Huangpu District and Baoshan District have relatively faster growth rates. The districts with the mildest pandemic situations are Fengxian District, Chongming District, and Jinshan District, followed by Qingpu District and Changning District.

![image](https://github.com/ChloeYuan1211/Patiotemporal-Analysis-and-Prediction-of-COVID-19-Pandemic/blob/main/image/figure%204%20Statistical%20plot%20of%20Cumulative%20Number%20of%20New%20Cases%20in%20Each%20District.png)
This chart showed that from April 1st to May 4th, the cumulative increase in the number of infected individuals in Shanghai has exceeded 500000. The period from April 12th to April 20th saw the fastest growth, while the growth rate after April 25th was relatively small.


