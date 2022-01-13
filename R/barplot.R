#' Barplot with percentage label
#'
#' @param dat dataframe
#' @param x x
#' @param group fill group
#'
#' @return
#' @export
#'
#' @examples
BarplotPercentage <- function(dat,x,group){
  dat %>%
    group_by_at(x) %>%
    count(Group = get(group)) %>%
    mutate(
      pct = prop.table(n),
      lab = paste0(round(pct*100,1),"%"),
      y = pct/2 + c(cumsum(rev(pct))[-length(pct)],0)
    ) %>%
    ggplot(aes_string(x = x, y = "pct", fill = "Group")) +
    geom_bar(stat = "identity") +
    geom_text(aes(y=y,label = lab)) +
    scale_y_continuous(expand = expansion(mult = c(0,0)))+
    xlab("") + ylab("Percentage")+
    theme_bw()
}
