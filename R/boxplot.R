box_plot <- function(qdat, x){
  
  p1 <- ggplot(qdat, aes_string(x = "Time", y = x, color = "Cohort")) +
  geom_rect(xmin = 0.4, xmax = 2.5,
            ymin = -Inf, ymax = Inf,
            fill ='white',
            inherit.aes = F)+
  geom_jitter(alpha = 0.6)+
  geom_boxplot(alpha = 0.5) +
  scale_color_manual(values = c("#7570B3", "#1F78B4"))+
  stat_compare_means(comparisons = list(1,2))+
  theme_bw() + 
  xlab("") +
  ylab(paste0(x, " \n(CLR-transformed RA)"))+scale_y_continuous(expand = c(0.1,0.1))+
  theme(panel.grid=element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+facet_wrap(.~Cohort, scales = "free", nrow = 1)
  return(p1)
}
