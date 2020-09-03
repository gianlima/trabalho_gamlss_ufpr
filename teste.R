setwd("C:/Users/Elen/Desktop/Gamlss")

library(gamlss)

dados <- read.csv2("Dados.csv")

str(dados)
summary(dados)

ajuste <- fitDist(idhm, data=dados, type="realline")
head(ajuste$fits)

truehist(dados$idhm, xlab="SEILA", col="white") # SN2 NO PE PE2

m.NO <- gamlssML(idhm, data=dados, family=NO)
m.PE <- gamlssML(idhm, data=dados, family=PE)
m.PE2 <- gamlssML(idhm, data=dados, family=PE2)

xx <- seq(min(dados$idhm), max(dados$idhm), length.out=100)

ll.SN2 <- dSN2(xx, mu=ajuste$mu, sigma=ajuste$sigma, nu=ajuste$nu)
ll.NO <- dNO(xx, mu=m.NO$mu, sigma=m.NO$sigma)
ll.PE <- dPE(xx, mu=m.PE$mu, sigma=m.PE$sigma, nu = m.PE$nu)
ll.PE2 <- dPE2(xx, mu=m.PE2$mu, sigma=m.PE2$sigma, nu = m.PE2$nu)

lines(xx, ll.SN2, lwd=2)
lines(xx, ll.NO, col=2, lwd=2)
lines(xx, ll.PE, col=4 , lwd=2);
lines(xx, ll.PE2, col=6, lwd=2)

legend("topleft", col=c(1,2,4,6), legend = c("SN2", "NO", "PE", "PE2"),
       lty = 1, bty = "n", lwd = 2)

