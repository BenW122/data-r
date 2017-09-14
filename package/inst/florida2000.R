load(url("http://vault.data-r.com/data/florida2000.rda"))

palm_beach <- 50

plot(Buchanan ~ Bush, data = Florida2000, xlab = "Stimmen für Bush", ylab = "Stimmen für Buchanen")
points(Buchanan[palm_beach] ~ Bush[palm_beach], data = Florida2000, pch = 19, col = "red")
points(Buchanan[palm_beach] ~ Bush[palm_beach], data = Florida2000)
text( Florida2000$Bush[palm_beach], Florida2000$Buchanan[palm_beach],row.names(Florida2000)[palm_beach], pos = 4)

plot(Buchanan ~ Bush, data = Florida2000, xlab = "Stimmen für Bush", ylab = "Stimmen für Buchanen")

model <- lm(Buchanan ~ Bush, data = Florida2000[-palm_beach,])
summary(model)

buch_pred <- predict(model, newdata = Florida2000[palm_beach,])

abline(model,lwd=2)
buch <- c(Florida2000$Buchanan[palm_beach], buch_pred)
bush <- rep(Florida2000$Bush[palm_beach], 2)
lines(buch ~ bush)

points(Buchanan[palm_beach] ~ Bush[palm_beach], data = Florida2000, pch = 19, col = "red")
points(Buchanan[palm_beach] ~ Bush[palm_beach], data = Florida2000)
text( Florida2000$Bush[palm_beach], Florida2000$Buchanan[palm_beach],row.names(Florida2000)[palm_beach], pos = 4)
