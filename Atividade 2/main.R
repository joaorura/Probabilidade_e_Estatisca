# Funcoes uteis


z_func= function (media, desvio) {
  return (function(a) {
    (a - media) / desvio
  })
}


# Primeira Questão

a_1 = pbinom(5, size=40, prob=0.12) - pbinom(2, size=40, prob=0.12)

b_1 = pbinom(1, size=40, prob=0.12, lower.tail=FALSE)

c_1 = pbinom(3, size=40, prob=0.12)

d_1 = pbinom(1, size=40, prob=0.12)

e_1 = pbinom(0, size=40, prob=0.12, lower.tail=FALSE)

# Segunda Questão

a_2 = c()
for (i in 1:4) {a_2[i] = c(dbinom(i-1, size=3, prob=0.6))}

# Terceura Questão

a_3 = dpois(4, lambda=3)

b_3 = dpois(0, lambda=3)

c_3 = dpois(20, lambda=3*7)

# 4

a_4 = 15 * 0.75

b_4 = sqrt(15 * 0.75 * 0.25)

c_4 = dbinom(15, size=15, prob=0.75)

d_4 = dbinom(10, size=15, prob=0.75)

e_4 = pbinom(12, size=15, prob=0.75, lower.tail=FALSE)

# 5

z = z_func(850, 40)

aux_0 = z(1000)
aux_1 = z(700)
a_5 = pnorm(aux_0) - pnorm(aux_1)

aux_0 = z(800)
b_5 = pnorm(aux_0, lower.tail=FALSE)

aux_0 = z(750)
c_5 = pnorm(aux_0)

x = seq(0, 850*2, length=850*5)
y = dnorm(x, mean=850, sd=40)

# d
plot(x, y, type="l",
           lwd=2,
           col="blue",
           ylab="Probabilidade",
           main="Funcao probabilidade")

# 6

a_6 = dbinom(4, size=6, prob=0.5)

b_6 = pbinom(1, size=6, prob=0.5, lower.tail=FALSE)

c_6 = pbinom(3, size=6, prob=0.5)


# 7

z = z_func(4000, 200)

aux_0 = z(4250)
aux_1 = z(3600)
a_8 = pnorm(aux_0) - pnorm(aux_1)

