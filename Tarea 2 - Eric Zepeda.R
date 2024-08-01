setwd("./")
#load("/Gauss.rda")
Y = Gauss[,1]
X = Gauss[,2]
e <- rnorm(length(X), mean = 0, sd = 1)


start <- Sys.time()
start_values <- list(B1 = 96, B2 = 0.009 , B3 = 103, B4 = 106, B5 = 18, B6 = 72, B7 = 151, B8 = 18)
#start_values <- list(B1 = 100, B2 = 1 , B3 = 3, B4 = 1, B5 = 155, B6 = 17, B7 = 1, B8 = 16)
#start_values <- list(B1 = 1, B2 = 1 , B3 = 1, B4 = 1, B5 = 1, B6 = 1, B7 = 1, B8 = 1)
Y2<- Y+e


nlregression = Y2 ~ B1 * exp(-B2 * X) + B3*exp(-((X - B4)^2)/(B5^2)) + B6*exp((-(X-B7)^2)/(B8^2)) 

modelo <-  nls(nlregression, data = data.frame(X, Y2), start = start_values, algorithm = "plinear")

##################

plot(X, Y, main = "Modelo de Regresión no lineal", xlab = "X", ylab = "Y", pch = 19, col = "blue")

# Genera predicciones para el plot
y_pred <- predict(modelo, newdata = data.frame(x = X))

# Interpolamos dichas predicciones de acuerdo a lo obtenido
lines(X, y_pred, col = "red", lwd = 2)

# Leyenda
legend("topright", legend = c("Datos", "Modelo Ajustado"), col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))
##################################################################

summary(modelo)
deviance(modelo)
print( Sys.time() - start )
