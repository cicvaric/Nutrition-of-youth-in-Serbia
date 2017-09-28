attach(domaci)

domaci.pol<-split(domaci,domaci$Pol,drop=FALSE)
domaci.Fizicka.aktivnost<-split(domaci, domaci$Fizicka.aktivnost,drop=FALSE)
domaci.voda<-split(domaci,domaci$Voda,drop=FALSE)
par(mfrow=c(1,2))
pie(c(nrow(domaci.pol$`0`),nrow(domaci.pol$`1`)), labels=c("muski","zenski"), main="Pol")
pie(c(nrow(domaci.Fizicka.aktivnost$`0`),nrow(domaci.Fizicka.aktivnost$`1`)),labels = c("ne","da"),main="Da li se bave sportom")
bmi_muskarci<-domaci.pol$`0`$Tezina/(domaci.pol$`0`$Visina/100)^2
bmi_muskarci
bmi_zene<-domaci.pol$`1`$Tezina/(domaci.pol$`1`$Visina/100)^2
bmi_zene
par(mfrow=c(1,2))
barplot(bmi_muskarci)
barplot(bmi_zene)
#zavisnost tezine u odnosu na bavljenje sportom ispitanika (prost linearan model)

plot(Minuti.sporta,Tezina) #dijagram rasprsivanja
b_ocena <- cov(Tezina, Minuti.sporta)/(sd(Minuti.sporta)^2)
a_ocena <- mean(Tezina)-b_ocena*mean(Minuti.sporta)
a_ocena
b_ocena
reziduali <- a_ocena + b_ocena*Minuti.sporta - Tezina
qqnorm(reziduali)
qqline(reziduali)
x<-lm(Tezina~Minuti.sporta)
summary(x) 
cor.test(Minuti.sporta,Tezina)

#zavisnost tezine od visine,starosti,bavljenja sportom i pola 
#racunamo parametre visestruke linearne regresije
y<-lm(Tezina~Minuti.sporta+Visina+Godine+Pol)
summary(y)


summary(lm(Tezina~Minuti.sporta+Visina+Godine+Pol))$r.squared
#koeficijent determinacije definisanog modela je 0.313 sto znaci da 31.3% promenljiva Tezina objasnjena promenljivima 
#Minuti.sporta,Visina,Godina sto smo i dobili vec gore funkcijom summary
#predvidjanje
newdata <- data.frame(Minuti.sporta=60, Visina=180, Godine=18, Pol=1)
predict(lm(Tezina ~ Minuti.sporta + Visina + Godine), newdata)
#predvideli smo da zena koja je stara 18 godina, visoka 180cm i bavi se 60 minuta sportom treba da ima oko 68kg

#logisticka regresija (da li se ljudi zdravo hrane u odnosu na unos vode i kalorija i predvidjanje)

model<-glm(Ishrana~Kalorije + Voda,family = binomial)
summary(model)
coef(model)
confint(model)
p.X <- predict(model)
y <- rep(0, length(Ishrana))
y[p.X>0.5] <- 1
newdata <- data.frame(Kalorije=2000, Voda=3000)
predict(model,newdata)
plot(Kalorije[Ishrana==0], Voda[Ishrana==0], xlab = "Kalorije", ylab = "Voda", xlim= c(0,7000), ylim=c(0,7000))
points(Kalorije[Ishrana==1], Voda[Ishrana==1], pch = 20)

#test normalnosti
shapiro.test(Kalorije)
qqnorm(Kalorije)
shapiro.test(Tezina)
qqnorm(Tezina)

#test nezavisnosti
wilcox.test(Kalorije~Pol, data=domaci)
wilcox.test(Visina~Ishrana, data=domaci)

#test slucajnosti
install.packages("randtests")
library(randtests)
turning.point.test(Kalorije)
turning.point.test(Voda)
#drugi nacin
testzaokreta<-function(x,alfa)
{
  n=length(x)
  s=0
  for(i in 1:(n-2))
  {
    a=(x[i+1]-x[i])*(x[i+2]-x[i+1])
    if(a<0) s=s+1
  }
  m=(2*(n-2))/3
  dz=(16*n-29)/90
  cat("srednja vrednost :",m," uzoracka disperzija: ", dz,"\n")
  
  print("vrednost test statistike je:")
  print(s)
  print(" standardizovana vrednost test statistike je:")
  print((s-m)/sqrt(dz))
  c=qnorm(p=(1-(alfa/2)))
  cat((1-alfa)*100,"%-tni interval poverenja stand stat je (",-c," ,",c,")")
}
testzaokreta(Kalorije, 0.05)

