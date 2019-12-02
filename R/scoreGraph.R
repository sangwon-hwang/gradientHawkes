library(ggplot2) 

# score graph
scores_dec <- read.csv(file='./data/scores_dec.csv')
scores_inc <- read.csv(file='./data/scores_inc.csv')

breaks_values_dec <- pretty(scores_dec$scores)
breaks_values_inc <- pretty(scores_inc$scores)

# dec score graph
scores_dec_ggplot <- scores_dec %>%
  ggplot(aes(x = reorder(Algorithm, scores), y = scores, fill = distance))+
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(x = "", y = "score of error")+
  scale_y_continuous(breaks = breaks_values_dec,
                     labels = abs(breaks_values_dec))+
  # geom_text(aes(label = Algorithm), nudge_x = 0, nudge_y = 0, color = "black") +
  # geom_text(aes(label = Algorithm), position = position_dodge(10))+
  theme_light(base_size = 10)

scores_dec_ggplot + 
  theme(
    axis.text.x = element_text(face="bold", color="black", size=9),
    axis.text.y = element_blank(),
    axis.title = element_text(face="bold", color="black", size=9),
    legend.title = element_text(face="bold", color="black", size=9),
    legend.text = element_text(face="bold", color="black", size=9),
    legend.position = c(0.89, 0.2) 
    # legend.key = element_rect(color = "red", fill = "white"), legend.key.size = unit(1,"cm")
  )+
  annotate("text", x = scores_dec_ggplot$data$Algorithm , y = 70
           , label = scores_dec_ggplot$data$Algorithm, hjust = 0
           , colour = "black", size = 3.2, fontface="bold")    # plot에 글자를 추가합니다. 

# inc score graph
scores_inc_ggplot <- scores_inc %>%
  ggplot(aes(x = reorder(Algorithm, scores), y = scores, fill = distance))+
  geom_bar(width = 0.8, stat = "identity")+
  coord_flip() +
  labs(x = "", y = "score of error")+
  scale_y_continuous(breaks = breaks_values_inc,
                     labels = abs(breaks_values_inc))+
  theme_light(base_size = 10)

scores_inc_ggplot + 
  theme(
    axis.text.x = element_text(face="bold", color="black", size=9),
    axis.text.y = element_blank(),
    axis.title = element_text(face="bold", color="black", size=9)
    
    # legend.title = element_text(face="bold", color="black", size=9),
    # legend.text = element_text(face="bold", color="black", size=9)
    # legend.key = element_rect(color = "red", fill = "white"), legend.key.size = unit(1,"cm")
  )+
  annotate("text", x = scores_inc_ggplot$data$Algorithm , y = 10
           , label = scores_inc_ggplot$data$Algorithm, hjust = 1
           , colour = "black", size = 3.2, fontface="bold")    # plot에 글자를 추가합니다. 

scores_dec_ggplot
scores_inc_ggplot

ggsave("./scores_dec-0.6-1.png", scores_dec_ggplot, scale = 0.8, dpi = 300)
ggsave("./basic_plot.jpg", scores_inc_ggplot, scale = 0.8, dpi = 300) 
