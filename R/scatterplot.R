#' scatterplot with SCC
#' customized scatterplot
#'
#' @param dat dataframe, input
#' @param x string, lab for x
#' @param y string, lab for y
#' @param group string, lab for grouping, default NULL
#' @param PCC if permform ppcor::pcor.test
#' @param adj string, lab for confounder in pcor.test
#'
#' @return
#' @export
#'
#' @examples
scatterplotWithSCC <- function (dat, x, y, group = NULL,PCC=F,adj=NULL)
{
  dat <- dat[!is.na(dat[, x]), , drop = F]
  s0 <- cor.test(dat[,x],dat[,y],method = "s")
  if(PCC){
    if(is.null(adj)){
      stop("If perform PCC, adj must be support")
    }
    dat <- dat[,c(x,y,adj,group),drop=F]
    dat <- na.omit(dat)
    adj <- dat[,adj,drop=F]
    for(i in 1:ncol(adj)){
      adj[,i] <- as.numeric(adj[,i])
    }

    s0 <- ppcor::pcor.test(dat[, x,drop=F], dat[, y,drop=F],adj, method = "s")
  }

  if (is.null(group)) {
    lab <- paste0("scc rho = ", round(s0$estimate, 3), "; p = ",
                  formatC(s0$p.value, digits = 2))
    p <- ggplot(dat, aes_string(x, y)) +
      geom_point(size = 0.8, alpha = 0.5) +
      geom_smooth(method = "lm", se = F) +
      annotate("text", x = -Inf, y = Inf, vjust = 1.2,
               hjust = 0, label = lab, size = 3) + theme_bw()
  }
  else {
    lst_levels <- levels(as.factor(dat[, group]))
    lab <- c()
    for(l in lst_levels){
      id <- dat[,group]==l
      a <- cor.test(dat[id,x],dat[id,y],method = "s")
      if(PCC){
        a <- ppcor::pcor.test(dat[id,x,drop=F],dat[id,y,drop=F],adj[id,,drop=F],method = "s")
      }
      lab <- c(lab,paste0(l," scc rho=",round(a$estimate,3),"; p=",formatC(a$p.value, digits = 2)))
    }
    lab <- paste(lab,collapse = "\n")
    lab <- paste0(paste0("Totol rho = ", round(s0$estimate, 3), "; p = ",formatC(s0$p.value, digits = 2)),"\n",lab)
    p <- ggplot(dat, aes_string(x, y, color = group)) +
      geom_point(size = 0.8, alpha = 0.5) +
      geom_smooth(method = "lm",se = F) +
      geom_smooth(data = dat, method = "lm",se = F, aes_string(x, y), color = "black") +
      annotate("text", x = -Inf, y = Inf, vjust = 1.2, hjust = 0, label = lab,size = 3) +
      theme_bw()
  }
  p
}
