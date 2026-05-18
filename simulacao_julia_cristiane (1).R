K = 100
rej = 0
for(k in 1:K){
t = 4
b = 5
N = t*b
mu = 20
tau = c(0.5,0.1,-0.2,-0.4) # efeitos de tratamento
beta = c(2,3,-4,-1) # efeitos de bloco
sigma2 = 20 #variancia do erro experimental
erro = rnorm(N,0,sigma2)
for(i in 1:t){
  for(j in 1:b){
    y = mu + tau + beta + erro
  }
}
trat = c(rep(1,b),rep(2,b),rep(3,b),rep(4,b))
bloco = c(rep(1,t),rep(2,t),rep(3,t),rep(4,t),rep(5,t))

bic = anova(aov(y ~ factor(trat) + factor(bloco)))
rej[k] = 0 # indicadora se rejeitou ou nao H0
}
