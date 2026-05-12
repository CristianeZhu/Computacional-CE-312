
#### Qual teste usar?
# média e proporção: z
# variãncia: qui-quadrado
# coeficiente de regressão: t
#várias médias: F(anova)

# IC: estimativa ± valor crítico × EP
# EP= desvio/sqrt(n)
# Estatística de teste: estimativa - valor sob H0/ erro padrão
# P-valor: Prob valor calculado (ex: pnorm(valor_calc)), se for bilaterial é só multiplicar por 2 e  por módulo. Rejeitar se - valor < 0.05

#### Erros tipo 1 
# Fixar alpha, definir o teste de hipótese e a estatística de teste
# Definir H0
# Calcular a estatística de teste
# Calcular o valor crítico
# Verificar se valor calculado > valor crítico
# mean(rejeitar)
# Interpretação: Cerca de % dos testes rejeitaram  H0 mesmo H0 sendo verdadeiro

#### Erros tipo 2 
# Fixar alpha, definir o teste de hipótese e a estatística de teste
# Definir H1
# Calcular a estatística de teste
# Calcular o valor crítico
# Verificar se valor calculado < valor crítico
# mean(não rejeitar)
# Interpretação: Cerca de % dos testes não rejeitaram H0 mesmo H0 sendo falsa

#### Taxa de cobetura
# Se eu repetir o experimento muitas vezes, o IC de 95% para a variância contém a variância verdadeira em aproximadamente 95% dos casos?
# Calcular os intervalos de confiança
# Verificar se o valor está dentro do intervalo
# Verificar quão robusto é, ver se erealmente cobriu a porcentagem esperada

###### Poder
# Rejeitar H0 dado que ela é falsa
# probabilidade de detectar diferença real quando existe



### Soma de variáveis aleatórias

# Soma de normais vira normal com media e variancia sendo a sooma delas
# Soma de exponenciais vira gamma(k, lambda) sendo k nº de somas
# SOma de poisson é poisson com parametros sendo a soma doslambda
# Soma de bernoulli é uma binomial

lam <- rgamma(1e5, 3, 1)
y <- rpois(1e5, lam = lam) ## distribuição de y|lambda, logo, y tem distribuição binomial negativa (3,1/2)
nb(r= alpha, p = beta/(beta+1))




##################### Erros tipo 1 e 2, poder

## P erro tipo 1 considerei que as médias são iguais, 0
n1 <- 20
n2 <- 20
k <- 10000
sim <- replicate(k, {
  y1 <- rnorm(n1)
  y2 <- rnorm(n2)
  teste <- t.test(y1,y2)
  teste$p.value < 0.05
})

mean(sim) ## aprox 5% das simulaçoes rejeitou h0 mesmo ele sendo vdd


## P erro tipo 2 considerei que as médias são diferentes
sim2 <- replicate(k, {
  y1 <- rnorm(n1)
  y2 <- rnorm(n2,0.5,1)
  teste <- t.test(y1,y2)
  teste$p.value >= 0.05
})
mean(sim2)

## Cerca de 65% dos testes não H0 mesmoo ela sendo falsa


## Poder
1 - mean(sim2)

## Cerca de 35% dos testes rejeitam H0 quando ela é falsa

medias <-c (0, 0.2, 0.4, 0.6,0.8, 1)
sim3 <- function(mu){
  rejeita <- replicate(k,{
  y1 <- rnorm(n1)
  y2 <- rnorm(n2,mu,1)
  teste <- t.test(y1,y2)
  teste$p.value < 0.05
  })
  mean(rejeita)}
poderes <- sapply(medias, sim3)

resultado <- data.frame(
  media = medias,
  poder = poderes
)

resultado
plot(resultado$media, resultado$poder,
     type = "l",
     xlab = "Média verdadeira",
     ylab = "Poder do teste",
     main = "Curva de poder",
     ylim = c(0, 1))

# O teste apresenta baixa capacidade de detectar diferenças pequenas entre as médias, 
#porém seu poder aumenta rapidamente conforme o tamanho do efeito cresce, aproximando-se de 1 para diferenças maiores.

# Se a média verdadeira for 1, o teste rejeita H0 praticamente em 100% dos experimentos, isto é, se a média vdd fosse 1, o teste sempre conseguiria detectar q n é 0