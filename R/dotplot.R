## ready to add

test <- function(){
  all <- ggpubr::ggdotplot(fo_phe, x="Time", y ="GMHI", fill = "Group" ,add = "jitter",alpha=0.6,size = 0.5, binwidth = 1/6)+
    stat_compare_means(comparisons = list(c(1, 2), c(1,3), c(2,3)), method="wilcox.test")+
    scale_fill_manual(values = group_clor)+
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 45),legend.position = "none")+xlab("")+
    ylab("TG_GMHI")+facet_grid(.~Group)
}

