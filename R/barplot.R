#' Barplot with percentage label
#'
#' @param dat dataframe
#' @param x x
#' @param group fill group
#' @param displayN If display sample number, default=FALSE
#'
#' @return
#' @export
#'
#' @examples
BarplotPercentage <- function(dat,x,group,displayN=FALSE){
  dat <- dat %>%
    group_by_at(x) %>%
    count(Group = get(group)) %>%
    mutate(
      pct = prop.table(n),
      lab.p = paste0(round(pct*100,1),"%"),
      y.p = pct/2 + c(rev(cumsum(rev(pct))[-length(levels(Group))]), 0),
      lab.n = n,
      y.n = n/2+c(rev(cumsum(rev(n))[-length(levels(Group))]), 0)
    )
  y = ifelse(displayN,"n","pct")
  ylab = ifelse(displayN,"Number","Percentage")
  ggplot(dat,aes_string(x = x, y = y, fill = "Group")) +
    geom_bar(stat = "identity") +
    geom_text(aes_string(y=ifelse(displayN,"y.n","y.p"),label = "lab.n")) +
    scale_y_continuous(expand = expansion(mult = c(0,0)))+
    xlab("") + ylab(ylab)+
    theme_bw()
}
