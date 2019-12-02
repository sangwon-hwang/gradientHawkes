# install.packages("gapminder")
# install.packages("plyr")

library(plyr)    
library(ggplot2) 
library(gapminder) 

getPlot <- function(df1, df2) {
  
  tmep_ggplot <- ggplot() + 
    geom_line(aes(x = 1:120, y = df1$magnitude, color = "c1", linetype="lt1"), size = 0.6)  +
    geom_line(aes(x = 1:120, y = c(df2$magnitude[1:90], rep(x = NA, times = 30)), color = "c2", linetype="lt2"), size = 0.6) +
    geom_line(aes(x = 1:120, y = c(rep(x = NA, times = 89), df2$magnitude[90:120]), color = "c3", linetype="lt3" ), size = 0.6) +
    scale_colour_manual(name = "section",
                        label = c("observed", "fitted", "forecast"),
                        values = c(c1 = "#F8966D", c2 = "#00BA38", c3="#619CFF")) +   
    scale_linetype_manual(name = "section", 
                          label = c("observed", "fitted", "forecast"),
                          values = c(lt1 = "twodash", lt2 = "solid", lt3 = "solid")) +
    xlab('Time') +
    ylab('') +
    theme_light() +
    theme(legend.title = element_blank(),
          legend.text = element_text(face="bold", color="black", size=10),
          legend.position = c(0.12, 0.88)) 
  
  tmep_ggplot
}


# data load
observed_dec_df <- read.csv(file='./data/observed_dec_df.csv')
observed_inc_df <- read.csv(file='./data/observed_inc_df.csv')

NM_dec_df <- read.csv(file='./data/NM_dec_df.csv')
CG_dec_df <- read.csv(file='./data/CG_dec_df.csv')
BFGS_dec_df <- read.csv(file='./data/BFGS_dec_df.csv')
SANN_dec_df <- read.csv(file='./data/SANN_dec_df.csv')

NM_inc_df <- read.csv(file='./data/NM_inc_df.csv')
CG_inc_df <- read.csv(file='./data/CG_inc_df.csv')
BFGS_inc_df <- read.csv(file='./data/BFGS_inc_df.csv')
SANN_inc_df <- read.csv(file='./data/SANN_inc_df.csv')

hip_dec_0.2 <- read.csv(file='./data/hip_dec_0.2.csv')
hip_dec_0.4 <- read.csv(file='./data/hip_dec_0.4.csv')
hip_dec_0.6 <- read.csv(file='./data/hip_dec_0.6.csv')
hip_dec_0.8 <- read.csv(file='./data/hip_dec_0.8.csv')

hip_inc_0.2 <- read.csv(file='./data/hip_inc_0.2.csv')
hip_inc_0.4 <- read.csv(file='./data/hip_inc_0.4.csv')
hip_inc_0.6 <- read.csv(file='./data/hip_inc_0.6.csv')
hip_inc_0.8 <- read.csv(file='./data/hip_inc_0.8.csv')

# plot data 설정
# 감소구간
HIP_0.2_dec_plot <- data.frame(observed_dec_df$magnitude, hip_dec_0.2$magnitude); colnames(HIP_0.2_dec_plot) <- c("Observed","HIP")
LH_NM_dec_plot <- data.frame(observed_dec_df$magnitude, NM_dec_df$magnitude); colnames(LH_NM_dec_plot) <- c("Observed","LSTM-HAWKES")

HIP_0.8_dec_plot <- data.frame(observed_dec_df$magnitude, hip_dec_0.8$magnitude); colnames(HIP_0.8_dec_plot) <- c("Observed","HIP")
LH_CG_dec_plot <- data.frame(observed_dec_df$magnitude, CG_dec_df$magnitude); colnames(LH_CG_dec_plot) <- c("Observed","LSTM-HAWKES")

# 증가구간
HIP_0.4_inc_plot <- data.frame(observed_inc_df$magnitude, hip_inc_0.4$magnitude); colnames(HIP_0.4_inc_plot) <- c("Observed","HIP")
LH_BFGS_inc_plot <- data.frame(observed_inc_df$magnitude, BFGS_inc_df$magnitude); colnames(LH_BFGS_inc_plot) <- c("Observed","LSTM-HAWKES")

HIP_0.8_inc_plot <- data.frame(observed_inc_df$magnitude, hip_inc_0.8$magnitude); colnames(HIP_0.8_inc_plot) <- c("Observed","HIP")
LH_SANN_inc_plot <- data.frame(observed_inc_df$magnitude, SANN_inc_df$magnitude); colnames(LH_SANN_inc_plot) <- c("Observed","LSTM-HAWKES")

# getPlot
# hip_dec_0.2 - NM_dec_df
hip_dec_0.2_plot <- getPlot(observed_dec_df, hip_dec_0.2)  
NM_dec_df_plot <- getPlot(observed_dec_df, NM_dec_df)  
# hip_dec_0.8 - CG_dec_df
hip_dec_0.8_plot <- getPlot(observed_dec_df, hip_dec_0.8)  
NM_dec_df_plot <- getPlot(observed_dec_df, CG_dec_df)  

# hip_inc_0.4 - BFGS_inc_df 
hip_inc_0.4_plot <- getPlot(observed_inc_df, hip_inc_0.4)  
BFGS_inc_df_plot <- getPlot(observed_inc_df, BFGS_inc_df)  
# hip_inc_0.6 - SANN_inc_df
hip_inc_0.6_plot <- getPlot(observed_inc_df, hip_inc_0.6)  
SANN_inc_df_plot <- getPlot(observed_inc_df, SANN_inc_df)  

# library(scales)
  show_col(hue_pal()(1)) # #F8766D
  show_col(hue_pal()(2)) # #00BFC4 
  show_col(hue_pal()(3)) # #F8766D #00BA38 #619CFF
  show_col(hue_pal()(4))
  show_col(hue_pal()(5))

# Evaluation: MSE, MAE, Score, RR, Accuracy

# MSE 감소구간
# LSTM-HAWKES
forecast_MSE(observed_dec_df$magnitude[91:120], NM_dec_df$magnitude[91:120])
forecast_MSE(observed_dec_df$magnitude[91:120], CG_dec_df$magnitude[91:120])
forecast_MSE(observed_dec_df$magnitude[91:120], BFGS_dec_df$magnitude[91:120])
forecast_MSE(observed_dec_df$magnitude[91:120], SANN_dec_df$magnitude[91:120])
# HIP
forecast_MSE(observed_dec_df$magnitude[91:120], hip_dec_0.2$magnitude[91:120])
forecast_MSE(observed_dec_df$magnitude[91:120], hip_dec_0.4$magnitude[91:120])
forecast_MSE(observed_dec_df$magnitude[91:120], hip_dec_0.6$magnitude[91:120])
forecast_MSE(observed_dec_df$magnitude[91:120], hip_dec_0.8$magnitude[91:120])
# MSE 증가구간
# LSTM-HAWKES
forecast_MSE(observed_inc_df$magnitude[91:120], NM_inc_df$magnitude[91:120])
forecast_MSE(observed_inc_df$magnitude[91:120], CG_inc_df$magnitude[91:120])
forecast_MSE(observed_inc_df$magnitude[91:120], BFGS_inc_df$magnitude[91:120])
forecast_MSE(observed_inc_df$magnitude[91:120], SANN_inc_df$magnitude[91:120])
# HIP
forecast_MSE(observed_inc_df$magnitude[91:120], hip_inc_0.2$magnitude[91:120])
forecast_MSE(observed_inc_df$magnitude[91:120], hip_inc_0.4$magnitude[91:120])
forecast_MSE(observed_inc_df$magnitude[91:120], hip_inc_0.6$magnitude[91:120])
forecast_MSE(observed_inc_df$magnitude[91:120], hip_inc_0.8$magnitude[91:120])

# MAE 감소구간
# LSTM-HAWKES
forecast_MAE(observed_dec_df$magnitude[91:120], NM_dec_df$magnitude[91:120])
forecast_MAE(observed_dec_df$magnitude[91:120], CG_dec_df$magnitude[91:120])
forecast_MAE(observed_dec_df$magnitude[91:120], BFGS_dec_df$magnitude[91:120])
forecast_MAE(observed_dec_df$magnitude[91:120], SANN_dec_df$magnitude[91:120])
# HIP
forecast_MAE(observed_dec_df$magnitude[91:120], hip_dec_0.2$magnitude[91:120])
forecast_MAE(observed_dec_df$magnitude[91:120], hip_dec_0.4$magnitude[91:120])
forecast_MAE(observed_dec_df$magnitude[91:120], hip_dec_0.6$magnitude[91:120])
forecast_MAE(observed_dec_df$magnitude[91:120], hip_dec_0.8$magnitude[91:120])
# MAE 증가구간
# LSTM-HAWKES
forecast_MAE(observed_inc_df$magnitude[91:120], NM_inc_df$magnitude[91:120])
forecast_MAE(observed_inc_df$magnitude[91:120], CG_inc_df$magnitude[91:120])
forecast_MAE(observed_inc_df$magnitude[91:120], BFGS_inc_df$magnitude[91:120])
forecast_MAE(observed_inc_df$magnitude[91:120], SANN_inc_df$magnitude[91:120])
# HIP
forecast_MAE(observed_inc_df$magnitude[91:120], hip_inc_0.2$magnitude[91:120])
forecast_MAE(observed_inc_df$magnitude[91:120], hip_inc_0.4$magnitude[91:120])
forecast_MAE(observed_inc_df$magnitude[91:120], hip_inc_0.6$magnitude[91:120])
forecast_MAE(observed_inc_df$magnitude[91:120], hip_inc_0.8$magnitude[91:120])

# Score 감소구간
# LSTM-HAWKES
forecast_score(observed_dec_df_02$magnitude[91:120], NM_dec_df_02$magnitude[91:120], sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
forecast_score(observed_dec_df_02$magnitude[91:120], CG_dec_df_02$magnitude[91:120], sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
forecast_score(observed_dec_df_02$magnitude[91:120], BFGS_dec_df_02$magnitude[91:120],  sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
forecast_score(observed_dec_df_02$magnitude[91:120], SANN_dec_df_02$magnitude[91:120],  sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
# HIP
forecast_score(observed_dec_df_02$magnitude[91:120], hip_dec_0.2$magnitude[91:120], sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
forecast_score(observed_dec_df_02$magnitude[91:120], hip_dec_0.4$magnitude[91:120], sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
forecast_score(observed_dec_df_02$magnitude[91:120], hip_dec_0.6$magnitude[91:120], sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
forecast_score(observed_dec_df_02$magnitude[91:120], hip_dec_0.8$magnitude[91:120], sd(observed_dec_df_02$magnitude[91:120]), sd(observed_dec_df_02$magnitude[91:120]))
# Score 증가구간
# LSTM-HAWKES
forecast_score(observed_inc_df$magnitude[91:120], NM_inc_df$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))
forecast_score(observed_inc_df$magnitude[91:120], CG_inc_df$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))
forecast_score(observed_inc_df$magnitude[91:120], BFGS_inc_df$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))
forecast_score(observed_inc_df$magnitude[91:120], SANN_inc_df$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))
# HIP
forecast_score(observed_inc_df$magnitude[91:120], hip_inc_0.2$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))
forecast_score(observed_inc_df$magnitude[91:120], hip_inc_0.4$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))
forecast_score(observed_inc_df$magnitude[91:120], hip_inc_0.6$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))
forecast_score(observed_inc_df$magnitude[91:120], hip_inc_0.8$magnitude[91:120], sd(observed_inc_df$magnitude[91:120]), sd(observed_inc_df$magnitude[91:120]))

# Accuracy 감소구간
# LSTM-HAWKES
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], NM_dec_df_02$magnitude[91:120]))
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], CG_dec_df_02$magnitude[91:120]))
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], BFGS_dec_df_02$magnitude[91:120]))
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], SANN_dec_df_02$magnitude[91:120]))
# HIP
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], hip_dec_0.2$magnitude[91:120]))
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], hip_dec_0.4$magnitude[91:120]))
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], hip_dec_0.6$magnitude[91:120]))
mean(forecast_accuracy(observed_dec_df_02$magnitude[91:120], hip_dec_0.8$magnitude[91:120]))
# Accuracy 증가구간
# LSTM-HAWKES
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], NM_inc_df$magnitude[91:120]))
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], CG_inc_df$magnitude[91:120]))
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], BFGS_inc_df$magnitude[91:120]))
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], SANN_inc_df$magnitude[91:120]))
# HIP
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], hip_inc_0.2$magnitude[91:120]))
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], hip_inc_0.4$magnitude[91:120]))
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], hip_inc_0.6$magnitude[91:120]))
mean(forecast_accuracy(observed_inc_df$magnitude[91:120], hip_inc_0.8$magnitude[91:120]))

(0.47156334984 + 0.611019829268 + 0.529253882051 + 0.513768650091)/4
(0.129119961892 + 0.221343827219 + 0.223197268941 + 0.415708086648)/4
(0.876378522657 + 0.884247506238 + 0.882539547104 + 0.885143767461)/4
(0.448025631377 + 0.826826816444 + 0.849668911174 + 0.842043318233)/4

# MAPE감소구간
# LSTM-HAWKES
mean(abs(observed_dec_df_02$magnitude[91:120] - NM_dec_df_02$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
mean(abs(observed_dec_df_02$magnitude[91:120] - CG_dec_df_02$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
mean(abs(observed_dec_df_02$magnitude[91:120] - BFGS_dec_df_02$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
mean(abs(observed_dec_df_02$magnitude[91:120] - SANN_dec_df_02$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
# HIP
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.2$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.4$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.6$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.8$magnitude[91:120])/(observed_dec_df_02$magnitude[91:120]))
# MAPE증가구간
# LSTM-HAWKES
mean(abs(observed_inc_df$magnitude[91:120] - NM_inc_df$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))
mean(abs(observed_inc_df$magnitude[91:120] - CG_inc_df$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))
mean(abs(observed_inc_df$magnitude[91:120] - BFGS_inc_df$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))
mean(abs(observed_inc_df$magnitude[91:120] - SANN_inc_df$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))
# HIP
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.2$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.4$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.6$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.8$magnitude[91:120])/(observed_inc_df$magnitude[91:120]))

# SMAPE감소구간
# LSTM-HAWKES
mean(abs(observed_dec_df_02$magnitude[91:120] - NM_dec_df_02$magnitude[91:120]) / ( (NM_dec_df_02$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
mean(abs(observed_dec_df_02$magnitude[91:120] - CG_dec_df_02$magnitude[91:120]) / ( (observed_dec_df_02$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
mean(abs(observed_dec_df_02$magnitude[91:120] - BFGS_dec_df_02$magnitude[91:120]) / ( (observed_dec_df_02$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
mean(abs(observed_dec_df_02$magnitude[91:120] - SANN_dec_df_02$magnitude[91:120]) / ( (observed_dec_df_02$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
# HIP
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.2$magnitude[91:120])/( (hip_dec_0.2$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.4$magnitude[91:120])/( (hip_dec_0.4$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.6$magnitude[91:120])/( (hip_dec_0.6$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
mean(abs(observed_dec_df_02$magnitude[91:120] - hip_dec_0.8$magnitude[91:120])/( (hip_dec_0.8$magnitude[91:120] + observed_dec_df_02$magnitude[91:120])/2))
# SMAPE증가구간
# LSTM-HAWKES
mean(abs(observed_inc_df$magnitude[91:120] - NM_inc_df$magnitude[91:120])/( (NM_inc_df$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2 ))
mean(abs(observed_inc_df$magnitude[91:120] - CG_inc_df$magnitude[91:120])/( (CG_inc_df$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2 ))
mean(abs(observed_inc_df$magnitude[91:120] - BFGS_inc_df$magnitude[91:120])/( (BFGS_inc_df$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2 ))
mean(abs(observed_inc_df$magnitude[91:120] - SANN_inc_df$magnitude[91:120])/( (SANN_inc_df$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2 ))
# HIP
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.2$magnitude[91:120])/( (hip_inc_0.2$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2))
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.4$magnitude[91:120])/( (hip_inc_0.4$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2))
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.6$magnitude[91:120])/( (hip_inc_0.6$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2))
mean(abs(observed_inc_df$magnitude[91:120] - hip_inc_0.8$magnitude[91:120])/( (hip_inc_0.8$magnitude[91:120] + observed_inc_df$magnitude[91:120]) /2))




