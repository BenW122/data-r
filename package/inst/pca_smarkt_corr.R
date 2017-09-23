### Addendum - PCA Supermarkt
###
### Frage: Warum sind Tankstelle und Parkplatz in einer Hauptkomponente?
###
### Die ersten Datensätze sind ein wenig misleading. Man sehe sich dazu
### die Korellation zwischen Tankstelle und Parkplätzen an: 0.65. Das ist 
### die zweithöchste im gesamten Datensatz. Es gibt also nur eine einzige 
### Merkmalkombination, die eine noch "bessere" Abhängigkeit aufweist,
### nämlich Atmosphäre und Freundlichkeit mit 0.69.

# Daten

load(url("http://vault.data-r.com/data/pca_smarkt.rda"))
smd <- na.omit(smarkt[, 6:25])
items <- c("Nichtlebensmittel", "offene Kassen", "Expresskassen", "Babyeinrichtungen", "Tankstelle", "Restaurant", "Stammkundenrabatt", "Parkplätze", "Standort", "Kundenservice", "Sonderangebote", "Freundlichkeit", "Atmosphäre", "Einpackhilfe",  "Schlangen Kassen", "Preise", "Qual.Frischprod.",  "Qual.verp.Prod.", "Qual.Wagen", "Zustellung")
colnames(smd) <- items

# Korellation

cor(smd$Tankstelle, smd$Parkplätze)
sorted.cor <- sort(cor(smd), decreasing = TRUE)
unique(sorted.cor[which(sorted.cor < 1)])[1:20]

which(cor(smd) == max(sorted.cor[which(sorted.cor < 1)]), arr.ind = TRUE)
cor(smd$Atmosphäre, smd$Freundlichkeit)

# Regression

fit <- lm(Tankstelle ~ Parkplätze, data=smd)
summary(fit)

plot(smd$Tankstelle, smd$Parkplätze)
abline(fit$coefficients[1], fit$coefficients[2])
