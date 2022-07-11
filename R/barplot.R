#' Barplot with percentage label
#'
#' @param dat dataframe
#' @param x x
#' @param group fill group
#' @param yaxies If display sample number (n) or percentage (p), default=p
#'
#' @return
#' @export
#'
#' @examples
BarplotPercentage <- function(dat,x,group,yaxies="p"){
  dat$group1 <- dat[,x]
  dat$group2 <- dat[,group]
  dat <- dat %>%
    dplyr::select(group1,group2) %>%
    group_by(group1) %>% count(TargetGroup = group2, .drop=F) %>%
    mutate(
      pct = prop.table(n),
      lab.p = paste0(round(pct * 100, 1), "%"),
      y.p = pct/2 + c(rev(cumsum(rev(pct))[-length(levels(TargetGroup))]), 0),
      lab.n = n,
      y.n = n/2 + c(rev(cumsum(rev(n))[-length(levels(TargetGroup))]), 0))
  y = ifelse(yaxies=="n","n","pct")
  ylab = ifelse(yaxies=="n","Number","Percentage")
  ggplot(dat,aes_string(x = "group1", y = y, fill = "TargetGroup")) +
    geom_bar(stat = "identity") +
    geom_text(aes_string(y=ifelse(ylab=="Number","y.n","y.p"),label = "lab.n")) +
    scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
    xlab("") + ylab(ylab)+
    theme_bw()
}
