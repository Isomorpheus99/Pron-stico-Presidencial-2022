library(DirichletReg)
library(Multinom)
library(R2jags)
library(stapip)
x<-c(2.038692, 26.566863, 36.866570,  3.362382,  2.356497,  1.710683, 11.603464, 11.147883,  4.105958)
p<-runif(9, 0, 0.5)
n<-length(p)
prop=p/sum(p)
DirichletModel <- function() {
  for (i in 1:N) {
    y[i] ~ dmulti(alpha,9)  # each response is Dirichlet with fixed parameter alpha
  }
  prop=c(0.13789545, 0.03887265, 0.07766462, 0.18677830,
         0.05305901, 0.19239250, 0.13273548, 0.02629886,
         0.15430314)
  alpha ~ ddirch(prop)    # prior for alpha
}
DirichletModel
Dirichlet_jags <- 
  jags(
    data = list(y = x, N = length(x)),
    model.file = DirichletModel,
    parameters.to.save = c("alpha")
  )

