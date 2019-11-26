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

a_7 = dbinom(4, size=30, prob=0.2)
b_7 = pbinom(2, size=30, prob=0.2, lower.tail=FALSE)

# 8

z = z_func(4000, 200)

aux_0 = z(4250)
aux_1 = z(3600)
a_8 = pnorm(aux_0) - pnorm(aux_1)

aux_0 = z(3400)
b_8 = pnorm(aux_0)

aux_0 = z(4636)
c_8 = pnorm(aux_0, lower.tail=FALSE)

# d

x = seq(0, 4000*2, length=4000*5)
y = dnorm(x, mean=4000, sd=200)

# d
plot(x, y, type="l",
     lwd=2,
     col="blue",
     ylab="Probabilidade",
     main="Funcao probabilidade")

# 9

a_9 = qnorm(0.04, 1000, 100)

x = seq(0, 1000*2, length=1000*5)
y = dnorm(x, mean=1000, sd=100)

# d
plot(x, y, type="l",
     lwd=2,
     col="blue",
     ylab="Probabilidade",
     main="Funcao probabilidade")

# 10

a_10 = pbinom(3, size=6, prob=0.2, lower.tail=FALSE)

# 11

a_11 = dbinom(3, size=1000, prob=0.001)

# 12

a_12 = dbinom(12, size=30, prob=0.5)

b_12 = pbinom(20, size=30, prob=0.5, lower.tail=FALSE)

# 13

aux_0 = c(0, 0.15, 0.5, 0.2, 0.15)
aux_1 = 0
a_13 = c()
for(i in 1:length(aux_0)) {
  aux_1 = aux_1 + aux_0[i]
  a_13[i] = c(qnorm(aux_1, 5, 0.9))
}

# Resposta da 13 -> Intervalos entre os valores de a_13

# 14

z = z_func(1000, 10)

a_14 = pnorm(z(990))

b_14 = pnorm(z(1020)) - pnorm(z(980))

aux_0 = pnorm(z(1002), lower.tail=FALSE)

c_14 = pbinom(4, size=10, prob=aux_0)

# 15

z = z_func(45, 20)

a_15 = pnorm(z(49), lower.tail=FALSE) + pnorm(z(29))
a_15 = a_15 * 50

# 16

z = z_func(2, 0.01)
a_16 = pnorm(z(1.97)) + pnorm(z(2.03), lower.tail=FALSE)

#17

a_17 = qnorm(0.05, 8, 1.8)

#18

a_18 = pexp(5, 0.2, lower.tail=FALSE)

b_18 = pexp(4, 0.2)

c_18 = pexp(8, 0.2) - pexp(3,0.2)

# 19

z = z_func(129, 14)

a_19 = pnorm(z(129 + 2*14), lower.tail=FALSE)

b_19 = pnorm(z(100))

c_19 = qnorm(0.95, 129, 14)

d_19 = pnorm(z(199), lower.tail=FALSE)
# 20

z = z_func(600, 100)
a_20 = pnorm(z(312)) * 10000