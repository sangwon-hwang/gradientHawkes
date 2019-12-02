# install.packages("plotly")
library(plotly) 

# MAE graph
MAE_csv <- read.csv(file='/Users/sangwonhwang/R_Projects/8.Gradient_Hawkes/data/MAE.csv')

ggplot(MAE_csv, aes(x=algorithm, y=MAE, color=algorithm))+
  geom_boxplot()+
  theme_light()

MASE_graph <- ggplot(data=MAE_csv, aes(x=algorithm, y= MAE, group=algorithm, color=algorithm))+
  geom_boxplot(width=0.3) #0066CC #00BFC4 #0066CC #F8766D

MASE_graph+scale_color_manual(values=c("#E40000", "#7CAE00", "#A0A0A0", "#0386C7"))+
  theme_light()+
  theme(
    axis.text.y = element_text(face="bold", color="black", size=9),
    axis.text.x = element_blank(),
    axis.title = element_text(face="bold", color="black", size=9),
    legend.position = 'none'
    # legend.title = element_text(face="bold", color="black", size=9),
    # legend.text = element_text(face="bold", color="black", size=9)
    # legend.key = element_rect(color = "red", fill = "white"), legend.key.size = unit(1,"cm")
  ) +  
  annotate("text", x = c(1,2,2.8,3.73) 
           , y = c(17, 7, 17, 6.5)
           , label = c('1st section HIP','1st section LSTM-HAWKES', '2nd section HIP', '2nd section LSTM-HAWKES')
           , colour = "black", size = 3.3, fontface="bold") # plot에 글자를 추가합니다.


