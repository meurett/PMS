# ETUDE DESCRIPTIVE :

# Donnees :

cuve1<-sort(c(2.007,4.440,2.086,2.428,2.057,2.107,2.190,2.325,3.950,2.551,2.284,2.373,2.464,2.261,2.408,2.910,2.775,3.551,3.643,2.017,2.767,2.334,2.699,3.163,2.748,5.404,2.601,2.970,6.416,2.217))
cuve2<-sort(c(3.698,2.536,2.006,2.207,2.222,2.591,2.889,2.236,5.437,2.406,2.498,3.643,2.185,2.661,2.281,2.237,2.362,2.512,2.139,2.745,2.026,2.035,2.821,2.102,2.319))
cuve3<-sort(c(2.059,3.270,2.438,3.070,2.451,3.134,2.551,2.781,3.029,2.863,2.638,3.770,2.103,3.334,2.660,2.229,3.100,2.673,2.782,2.720,2.449,2.800,3.197,2.662,2.840,2.738,3.722,2.921))
ncuve1<-length(cuve1)
ncuve2<-length(cuve2)
ncuve3<-length(cuve3)

# D apres la loi de Sturges, on a k=6 pour les 3 cuves

# Calcul des bornes inférieure et supérieure.

a0_cuve1<-cuve1[1]-0.025*(cuve1[ncuve1]-cuve1[1])
a6_cuve1<-cuve1[ncuve1]+0.025*(cuve1[ncuve1]-cuve1[1])
a0_cuve2<-cuve2[1]-0.025*(cuve2[ncuve2]-cuve2[1])
a6_cuve2<-cuve2[ncuve2]+0.025*(cuve2[ncuve2]-cuve2[1])
a0_cuve3<-cuve3[1]-0.025*(cuve3[ncuve3]-cuve3[1])
a6_cuve3<-cuve3[ncuve3]+0.025*(cuve3[ncuve3]-cuve3[1])

# Largeur des classes

h_cuve1<-(a6_cuve1-a0_cuve1)/6
h_cuve2<-(a6_cuve2-a0_cuve2)/6
h_cuve3<-(a6_cuve3-a0_cuve3)/6

# Bornes des classes : on partage [a0_cuve[i],a6_cuve[i]] en 6 intervalles
# de largeur h.

bornes_cuve1<-seq(a0_cuve1,a6_cuve1,h_cuve1)
bornes_cuve2<-seq(a0_cuve2,a6_cuve2,h_cuve2)
bornes_cuve3<-seq(a0_cuve3,a6_cuve3,h_cuve3)

# Summary des 3 ensembles de données

summary(cuve1)
summary(cuve2)
summary(cuve3)

# Histogrammes corespondant à classe de même largeur

hist(cuve1, prob=T, breaks=bornes_cuve1)
hist(cuve2, prob=T, breaks=bornes_cuve2)
hist(cuve3, prob=T, breaks=bornes_cuve3)

# Fonction de calcul d'histogramme à classes de même effectif (tiré de l'exo 2, fiche 1)

histoeff <- function(x, xlim=NULL, ...)
{
  sx <- sort(x)
  n <- length(x)
  k <- round(log(n)/log(2)+1)
  rangex <- max(x)-min(x)
  breaks <- c(min(x)-0.025*rangex, quantile(x, seq(1, k-1)/k), max(x)+0.025*rangex)
  col <- 0
  if (is.null(xlim)) xlim<-c(breaks[1], breaks[k+1])
  hist(x, breaks=breaks, col=col, xlim=xlim, probability=T, ...)
}

# Histogrammes corespondant à classe de même effectif

histoeff(cuve1)
histoeff(cuve2)
histoeff(cuve3)

# Valeurs pour Y = ln (X/2)

y_cuve1 <- log(cuve1[1:29]/2)
y_cuve2 <- log(cuve2[1:24]/2)
y_cuve3 <- log(cuve3[1:27]/2)

# Graphe de probabilité et droite des moindres carrés

# Cuve 1  =>  pas de valeurs abherrante
# donc on se permet de calculer le coeff a
# avec la R.G | on compare avec une autre méthode
# de calcul qui consiste à calculer 1/moyenne(ln(x/2))
# puisqu'on sait que pour la loi exponentielle,
# X(barre)n est un estimateur sans biais de E[X]
# et que E[X] vaut 1/a pour une loi exp(a):

plot(log(sort(cuve1)[1:29]),log(1-seq(1:29)/30))
abs1 <- log(sort(cuve1))[1:29]
ord1 <- log(1-seq(1:29)/30)
reg1 <- lm(ord1~abs1)
lines(abs1, fitted.values(reg1))
a_obtenu_pour_cave1_par_RG <- -reg1$coefficients[2]

a_obtenu_par_moyenne_pour_cave1 <- 1/mean(y_cuve1)

a_obtenu_pour_cave1_par_RG
a_obtenu_par_moyenne_pour_cave1
# L'ecart relatif est très faible; il est de l'ordre de 2% :
ecart_relatif_a1 <- abs((a_obtenu_pour_cave1_par_RG - a_obtenu_par_moyenne_pour_cave1)/a_obtenu_par_moyenne_pour_cave1)*100
ecart_relatif_a1

# Cuve 2  =>  il y a une valeur abherrante
# de la meme manière on se permet de calculer
# le coeff a avec la R.G | on compare avec l'évaluation
# par 1/moyenne(ln(x/2))

plot(log(sort(cuve2)[1:24]),log(1-seq(1:24)/25))
abs2 <- log(sort(cuve2))[1:24]
ord2<- log(1-seq(1:24)/25)
reg2 <- lm(ord2~abs2)
lines(abs2, fitted.values(reg2))
a_obtenu_pour_cave2_par_RG <- -reg2$coefficients[2]

a_obtenu_par_moyenne_pour_cave2 <- 1/mean(y_cuve2)

a_obtenu_pour_cave2_par_RG
a_obtenu_par_moyenne_pour_cave2
# L'ecart relatif est très faible; il est de l'ordre de moins de 1% :
ecart_relatif_a2 <- abs((a_obtenu_pour_cave2_par_RG - a_obtenu_par_moyenne_pour_cave2)/a_obtenu_par_moyenne_pour_cave2)*100
ecart_relatif_a2

# Cuve 3

plot(log(sort(cuve3)[1:27]),log(1-seq(1:27)/28))
abs3 <- log(sort(cuve3))[1:27]
ord3 <- log(1-seq(1:27)/28)
reg3 <- lm(ord3~abs3)
lines(abs3, fitted.values(reg3))

#  =>  on s'écarte plus du modèle DONC on va
# chercher avec une loi qui se rapproche plus du modèle
# ici, on peut poser l'hypothèse de la loi normale
# N(m,sigma²):

plot(sort(cuve3)[1:27],qnorm(seq(1:27)/28))
abs3_normale <- sort(cuve3)[1:27]
ord3_normale <- qnorm(seq(1:27)/28)
reg3_normale <- lm(ord3_normale~abs3_normale)
lines(abs3_normale, fitted.values(reg3_normale))

# Estimation de m et sigma^2 par moments ou maximum de 
# vraisemblance
m_exp<-mean(cuve3)
m_exp
sigma_carre_exp<-var(cuve3)
sigma_carre_exp

# Estimation de m et sigma^2 par le graphe de probabilités
reg3_normale <- lm(qnorm(seq(1:(ncuve3-1))/ncuve3)~cuve3[1:(ncuve3-1)])
sigma_carre_theorique <- (1/reg3_normale$coefficients[2])^2
sigma_carre_theorique
m_theorique <- -reg3_normale$coefficients[1]/reg3_normale$coefficients[2]
m_theorique

# Avec ce modèle de loi normale, l'ecart relatif est très faible pour les deux
# variables m et sigma² ; respectivement ~1% et ~3% :
ecart_relatif_m <- abs((m_exp-m_theorique)/m_theorique)*100
ecart_relatif_m
ecart_relatif_sigma_carre <- abs((sigma_carre_exp-sigma_carre_theorique)/sigma_carre_theorique)*100
ecart_relatif_sigma_carre