ConvertExpWeibull <- function (model, scale=0, conf.level = 0.95) 
{
  level <- 1 - conf.level
  qa <- qnorm(1 - level/2)
  Int.Only <- (nrow(summary(model)$table) == 2)
  sigma <- summary(model)$scale
  mu <- summary(model)$coef[1]
  k = length(summary(model)$coef) - 1
  if ((scale==1)|(!Int.Only)) {
    alpha <- summary(model)$coef[2:(k + 1)]
  }
  lambda <- exp(-mu/sigma)
  alpha2 <- 1/sigma
  tmp <- c(lambda, alpha2)
  names(tmp) <- c("lambda", "alpha")
  if ((scale==1)|(!Int.Only)) {
    beta <- -alpha/sigma
    tmp <- c(lambda, alpha2, beta)
    names(tmp) <- c("lambda", "alpha", names(summary(model)$coef[2:(k + 
                                                                      1)]))
  }
  var1 <- summary(model)$var
  var.mu <- diag(var1)[1]
  var.sigma = ifelse(scale==1 , 0,  var1[(k + 2), (k + 2)] * exp(2 * log(sigma)))
  if ((scale==1)|(!Int.Only)) {
    var.alpha <- var1[2:(k + 1), 2:(k + 1)]
    if (k > 1) {
      var.alpha <- diag(var.alpha)
    }
    se.alpha <- sqrt(var.alpha)
  }
  if (scale!=1) {cov.mu.sigma <- var1[(k + 2), 1] * sigma}
  var.alpha2 <- var.sigma/(sigma^4)
  var.lambda = ifelse(scale==1, exp(-2 * mu) * var.mu,  exp(-2 * mu/sigma) * ((var.mu/(sigma^2)) - 
                ((2 * mu/(sigma^3)) * cov.mu.sigma) + ((mu^2)/(sigma^4)) * var.sigma))
  var <- c(sqrt(var.lambda), sqrt(var.alpha2))
  if ((scale!=1)&(!Int.Only))  {
    cov.alpha.sigma <- var1[2:(k + 1), (k + 2)] * sigma
  }  
  var.beta = ifelse(scale==1, var.alpha ,(1/(sigma^2)) * (var.alpha - (2 * alpha/sigma) * 
                                   (cov.alpha.sigma) + (((alpha/sigma)^2) * var.sigma)))
  se.beta <- sqrt(var.beta)
  var <- c(sqrt(var.lambda), sqrt(var.alpha2), se.beta)
  HR <- cbind(HR = exp(beta), LB = exp(beta - qa * se.beta), UB = exp(beta + qa * se.beta))
  rownames(HR) <- names(summary(model)$coef[2:(k + 1)])
  ETR <- cbind(ETR = exp(alpha), LB = exp(alpha - qa * 
                                              se.alpha), UB = exp(alpha + qa * se.alpha))
  rownames(HR) <- names(summary(model)$coef[2:(k + 1)])
  tmp1 <- rbind(tmp, var)
  rownames(tmp1) <- c("Estimate", "SE")
  ret <- list(vars = t(tmp1))
  if  ((scale==1)|(!Int.Only)) {
    ret$HR <- HR
    ret$ETR <- ETR
  }
  return(ret)
}
