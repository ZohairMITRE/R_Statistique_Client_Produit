4+8
(6+5)/2
4*2
4
library(foreign, lib.loc = "C:/Program Files/R/R-4.2.1/library")
detach("package:foreign", unload = TRUE)
sqrt(9)
n <- 5
n
n <- 3
n
n <- 1949+2000
n
ls
ls()
n<-2
p <- zohair
p <- "zohair"
vile <- "casa"
ls()
ls.str
ls.str
ls.str()
rm vile
rm <- vile
ls.str()
rm(vile)
ls.str()
rm(rm)
v <- c(1,2,3,5,8)
v
print(v)
v.length()
length(v)
v <- c(1,"zohair","frida",1949,"layj3al rabi y7an 3lik")
v
v <- c(1,1,1,1,1,1) c(2,2,2,2,2,2,2)
u <- c(1,1949,22,NA)
u
u <- c(1,1949,22,"NA")
u
u <- c(1,1949,22,NA)
u
t <- c(1,1949,NA,TRUE)
t
t <- c(1,1949,NA,F)
t
prod(t)
prod(v)
v <- c(1,2,3,4,5,6)
prod(v)
range(v)
v <- c(1,10)
prod(v)
range(v)
means(v)
mean(v)
mydataset <- data.frame(Site=("A","B","C"),Season=("Winter","Summer","Spring","Fall",PH=(4.5,9.3,2.6,7.1)))
mydataset <- data.frame(Site=('A','B','C'),Season=("Winter","Summer","Spring","Fall",PH=(4.5,9.3,2.6,7.1)))
mydataset <- data.frame(Site=('A','B','C'),Season=("Winter","Summer","Spring","Fall"),PH=(4.5,9.3,2.6,7.1))
mydataset <- data.frame(Site=c('A','B','C'),Season=c("Winter","Summer","Spring","Fall"),PH=c(4.5,9.3,2.6,7.1))
mydataset <- data.frame(Site=c('A','B','C','D') , Season = c("Winter","Summer","Spring","Fall"),PH = c(4.5,9.3,2.6,7.1))
mydataset
mydataset$Site
mydataset$PH
v <- c(1.5,"raja",1949)
v
mydataset
mydataset2 <- data.frame(site=c(1,2,3,4,5),site2=c("un","deux","trois"))
mydataset2 <- data.frame(site=c(1,2,3,4,5),site2=c("un","deux","trois","quatre","cinq"))
mydataset2
length(mydataset)
length(mydataset2)
mydataset[2]
mydataset[2?2]
mydataset[2,2]
mydataset[2,0]
mydataset[2,4]
mydataset[2,3]
mydataset[2,1]
mydataset[1,1]
mydataset[3,4]
mydataset[2,4]
mydataset[2,3]
mydataset
mydataset[1,3]
mydataset[1]
mydataset[2]
mydataset[3]
mydataset[3,2]
mydataset
mydataset2
mydataset["PH">5]
mydataset[PH>5]
mydataset[mydataset$PH>5]
mydataset[mydataset$PH>5,]
mydataset[mydataset$PH>,5]
mydataset[mydataset$PH<,5]
mydataset[mydataset$PH<5,]
mydataset[mydataset$PH>5,]
mydataset(mydataset$PH>5, & mydataset$Season="summer")
mydataset(mydataset$PH>5,  mydataset$Season="summer")
mydataset(mydataset$PH>5,  mydataset$Season=="summer")
mydataset(mydataset$PH>5, , mydataset$Season=="summer")
mydataset(mydataset$PH>5, mydataset$Season=="summer",site)
mydataset(mydataset$PH>5, "site")
mydataset(mydataset$PH>5, "Site")
mydataset(mydataset$Site=="casa")
mydataset(mydataset$Site="casa")
mydataset(mydataset$Site="a")
mydataset(mydataset$Site="A")
mydataset(mydataset$Site=="A")
mydataset(mydataset$Site=="A","PH")
mydataset[mydataset$Site=="A","PH"]
mydataset[mydataset$PH>5, "site"]
mydataset[mydataset$PH>5, mydataset$Season=="summer",site]
mydataset[mydataset$PH>5, mydataset$Season=="summer"]
mydataset[mydataset$PH>5,, mydataset$Season=="summer"]
tree1
View(mydataset)
plot(tree1)
View(mydataset)
setwd("C:/Users/ssd.zohair/Desktop/Modules_MBDS/R/TP_R_Statistique_produit_client")
str(produit)
# Chargement des donnees
produit <- read.csv("Data Produit.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
str(produit)
# Creation des ensembles d'apprentissage et de test
produit_EA <- produit[1:400,]
produit_ET <- produit[401:600,]
# Suppression de la variable ID de l'ensemble d'apprentissage
produit_EA <- subset(produit_EA, select=-ID)
# Installation/m-a-j des librairies
install.packages("rpart")
install.packages("C50")
install.packages("tree")
# Apprentissage arbre
tree1 <- rpart(Produit~., produit_EA)
library(rpart, lib.loc = "C:/Program Files/R/R-4.2.2/library")
library(rpart)
library(tree)
library(C50)
# Apprentissage arbre
tree1 <- rpart(Produit~., produit_EA)
# Affichage graphique : tracage des arcs par la fonction plot.rpart()
plot(tree1)
# Ajout du texte au graphique par la fonction text.rpart()
text(tree1, pretty = 0)
# Ajout du texte au graphique par la fonction text.rpart()
text(tree1, pretty = 50)
# Ajout du texte au graphique par la fonction text.rpart()
text(tree1, pretty = 10)
# Ajout du texte au graphique par la fonction text.rpart()
text(tree1, pretty = 0)
# Ajout du texte au graphique par la fonction text.rpart()
text(tree1, pretty = 0)
# Apprentissage arbre
tree2 <- C5.0(Produit~., produit_EA)
# Affichage graphique (arcs et texte) par la fonction plot.C5.0()
plot(tree2, type="simple")
# Apprentissage arbre
tree3 <- tree(Produit~., data=produit_EA)
# Affichage graphique : tracage des arcs par la fonction plot.tree()
plot(tree3)
# Ajout du texte au graphique par la fonction text.tree()
text(tree3, pretty=0)
# Application de l'arbre 'tree1' a l'ensemble de test 'produit_ET'
test_tree1 <- predict(tree1, produit_ET, type="class")
# Application de l'arbre 'tree2' a l'ensemble de test 'produit_ET'
test_tree2 <- predict(tree2, produit_ET, type="class")
# Application de l'arbre 'tree3' a l'ensemble de test 'produit_ET'
test_tree3 <- predict(tree3, produit_ET, type="class")
# Comparaison des repartitions des predictions
table(test_tree1)
table(test_tree2)
table(test_tree3)
# Ajout des predictions de 'tree1' comme  nouvelle colonne 'Tree1' dans 'produit_ET'
produit_ET$Tree1 <- test_tree1
# Ajout des predictions de 'tree3' comme  nouvelle colonne 'Tree3' dans 'produit_ET'
produit_ET$Tree3 <- test_tree3
# Ajout des predictions de 'tree1' comme  nouvelle colonne 'Tree1' dans 'produit_ET'
produit_ET$Tree1 <- test_tree1
# Ajout des predictions de 'tree2' comme  nouvelle colonne 'Tree2' dans 'produit_ET'
produit_ET$Tree2 <- test_tree2
# Ajout des predictions de 'tree3' comme  nouvelle colonne 'Tree3' dans 'produit_ET'
produit_ET$Tree3 <- test_tree3
# Affichage de la classe reelle et des classes predites
View(produit_ET[,c("Produit", "Tree1", "Tree2", "Tree3")])
# Calcul du taux de succes : nombre de succes sur nombre d'exemples de test
taux_succes1 <- nrow(produit_ET[produit_ET$Produit==produit_ET$Tree1,])/nrow(produit_ET)
taux_succes1
taux_succes2 <- nrow(produit_ET[produit_ET$Produit==produit_ET$Tree2,])/nrow(produit_ET)
taux_succes2
taux_succes3 <- nrow(produit_ET[produit_ET$Produit==produit_ET$Tree3,])/nrow(produit_ET)
taux_succes3
# Chargement des donnees a predire dans un data frame 'produit_PR'
produit_PR <- read.csv("Data Produit Prospects.csv", sep=",", dec=".", header=T, stringsAsFactors = TRUE)
# Application de l'arbre 'tree2' a l'ensemble de prospects 'produit_PR'
class_tree2 <- predict(tree2, produit_PR, type="class")
class_tree2
table(class_tree2)
# Ajout des resultat dans une colonne Prediction au data frame produit_PR
produit_PR$Prediction <- class_tree2
View(produit_PR)
# Ajout des resultat dans une colonne Prediction au data frame produit_PR
produit_PR$Prediction <- class_tree2
View(produit_PR)
# Enregistrement du fichier de resultats au format csv
write.table(produit_PR, file='resultats.csv', sep="\t", dec=",", row.names = F)
# Chargement des donnees
produit <- read.csv("Data Produit.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
str(produit)
View(produit)
table(produit$Produit)
# Construction des ensembles d'apprentissage et de test
produit_EA <- produit[1:400,]
produit_ET <- produit[401:600,]
# Suppression de la variable ID
produit_EA <- subset(produit_EA, select = -ID)
# Affichages des ensembles et des distributions de valeurs des variables
View(produit_EA)
View(produit_ET)
summary(produit_EA)
summary(produit_ET)
# Installation et activation de la librairie requise
install.packages("rpart")
library(rpart)
install.packages("rpart")
# Construction de l'arbre de decision 'tree1'
tree1 <- rpart(Produit~., produit_EA)
# Affichage de l'arbre par 'tree1' par plot.rpart() et text.rpart()
plot(tree1)
text(tree1, pretty=0)
# Affichage textuel de l'arbre de decision
print(tree1)
# Installation et activation de la librairie requise
install.packages("rpart.plot")
library(rpart.plot)
# Affichage par d?faut d'un arbre rpart par la fonction prp()
prp(tree1)
# Affichage par d?faut d'un arbre rpart par la fonction prp()
prp(tree1)
# Valeurs sur les arcs, classe en couleur et proportion de la classe majoritaire dans chaque noeud
prp(tree1, type=4, extra=8, box.col=c("tomato", "darkturquoise")[tree1$frame$yval])
# Valeurs sur les arcs, classe en couleur et effectifs de chaque classe dans chaque noeud
prp(tree1, type=4, extra=1, box.col=c("tomato", "darkturquoise")[tree1$frame$yval])
# Choix automatique des couleurs et intensit? proportionnelle ? la proportion de la classe majoritaire
prp(tree1, type=4, extra=8, box.palette = "auto")
prp(tree1, type=4, extra=1, box.palette = "auto")
# Generation de la classe pr?dite pour chaque exemple de test pour l'arbre 'tree1'
test_tree1 <- predict(tree1, produit_ET, type="class")
# Generation des probabilites pour chaque exemple de test pour l'arbre 'tree1'
prob_tree1 <- predict(tree1, produit_ET, type="prob")
# Affichage des deux vecteurs de probabilites generes
print(prob_tree1)
# Affichage du vecteur de probabilites de prediction 'Oui'
prob_tree1[,2]
# Affichage du vecteur de probabilites de prediction 'Non'
prob_tree1[,1]
# Construction d'un data frame contenant classe reelle, prediction et probabilit?s des predictions
df_result1 <- data.frame(produit_ET$Produit, test_tree1, prob_tree1[,2], prob_tree1[,1])
View(df_result1)
# Rennomage des colonnes afin d'en faciliter la lecture et les manipulations
colnames(df_result1) = list("Classe","Prediction", "P(Oui)", "P(Non)")
View(df_result1)
# Quartiles et moyenne des probabilites des predictions 'Oui' pour l'arbre 'tree1'
summary(df_result1[df_result1$Prediction=="Oui", "P(Oui)"])
# Quartiles et moyenne des probabilites des predictions 'Non' pour l'arbre 'tree1'
summary(df_result1[df_result1$Prediction=="Non", "P(Non)"])
savehistory("C:/Users/ssd.zohair/Desktop/Modules_MBDS/R/TP_R_Statistique_produit_client/Mon_TD4.Rhistory")
