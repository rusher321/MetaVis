rocPlot_v <- function (response, predict, label = "AUC = ") 
{
    roc1 <- pROC::roc(response, predict)
    roc1.ci <- round(as.numeric(ci.auc(response, predict)), 2) * 100
    title1 <- paste0(label, roc1.ci[2], "% (", roc1.ci[1], 
        "-", roc1.ci[3], "%)")
    rocdat1 <- data.frame(roc1$sensitivities*100, (1 - roc1$specificities)*100, 
        title1)
    names(rocdat1) <- c("sen", "spe", "group")
    label1 <- title1
    annosize <- 5.5
    p <- ggplot(rocdat1, aes(x = spe, y = sen, colour = group, 
        group = group)) + xlab("100-Specificity (%)") + ylab("Sensitivity(%)") + 
        geom_path(size = 1.5) +
        #mytheme + 
        theme(legend.position = c(0.7, 
        0.25)) + scale_colour_manual(values = c("#ff6768"), 
        labels = c(label1)) + guides(colour = guide_legend(title = NULL)) + 
        geom_abline(colour = "black")
    return(p)
}
