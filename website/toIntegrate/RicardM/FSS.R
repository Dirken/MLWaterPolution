library(FSelector)

### mejorarla para que los métodos acepten params opcionales (...), 
### y que se pueda partir de un subset inicial para FW y BW

evaluator.accuracy.LDA <- function (subset) 
{
  cat(length(subset), subset)
  
  lda.tmp <- lda (as.simple.formula(subset, FSS.target.name), data=FSS.data, CV=TRUE)#, prior=c(0.7,0.3))
  tab <- table(lda.tmp$class, FSS.target)
  acc <- sum(tab[row(tab)==col(tab)])/sum(tab)
  
  print(paste(round(acc*100,digits=2),"%",sep=""))
  acc
}

evaluator.accuracy.QDA <- function (subset) 
{
  cat(length(subset), subset)
  
  qda.tmp <- qda (as.simple.formula(subset, FSS.target.name), data=FSS.data, CV=TRUE)#,  prior=c(0.7,0.3))
  tab <- table(qda.tmp$class, FSS.target)
  acc <- sum(tab[row(tab)==col(tab)])/sum(tab)
  
  print(paste(round(acc*100,digits=2),"%",sep=""))
  acc
}

bounded.exh.search <- function (attributes, eval.fun, sizeset) 
{
    len = length(attributes)
    if (len == 0) 
        stop("Attributes not specified")
    eval.fun = match.fun(eval.fun)
    best = list(result = -Inf, attrs = rep(0, len))
    for (size in sizeset) 
    {
        child_comb = combn(1:len, size)
        for (i in 1:dim(child_comb)[2]) {
            subset = rep(0, len)
            subset[child_comb[, i]] = 1
            result = eval.fun(attributes[as.logical(subset)])
            if (result > best$result) {
                best$result = result
                best$attrs = subset
            }
        }
    }
    return(list(vars=attributes[as.logical(best$attrs)], acc=best$result))
}