setwd("C:/Users/kzep/Documents/R/Tarea 1 glm")
load("finney.rda")
library("car")
library(stats)
library(matlib)
plot(finney[,1], finney[,2], 
     xlab = "Volumen", ylab = "Tasa",
     main = "Dispersión de los datos")

# Puntos con valor 0 en la tercera columna
points(finney[finney[, 3] == 0, 1], finney[finney[, 3] == 0, 2], 
       pch = 19, col = "red")

# Puntos con valor 1 en la tercera columna
points(finney[finney[, 3] == 1, 1], finney[finney[, 3] == 1, 2], 
       pch = 19, col = "black")

legend("topright", legend = c("0", "1"), 
       pch = 19, col = c("red","black"), bty = "n",title = "Respuesta")


# Fit a logistic regression model
model <- glm(finney[,3] ~ log(finney[,1]) + log(finney[,2]), family = binomial)

# Summarize the model
summary(model)


cook_m <- cooks.distance(model)
hat_m <- hatvalues(model)
dffits_m <- dffits(model)
covratio_m <- covratio(model)
dfbetas_m <- dfbetas(model)
rstudent_m <- rstudent(model)
rstandard_m <- rstandard(model)


#plot(hat_m)
#### DFFITS ############

plot(abs(dffits_m), 
     ylab = "|DFFITS|",
     main = "")

# Add a horizontal line at y = 9/39
abline(h = 3*(3/39)^0.5, col = "red", lty = 2)

# Add labels to the data points
text(x = seq_along(dffits_m), 
     y = abs(dffits_m), 
     labels = as.character(seq_along(dffits_m)), 
     pos = 3, 
     offset = 0.2, cex = 0.5)




### COOK DISTANCE        ####
plot(cooks.distance(model), 
     ylab = "Distancia de Cook",
     main = "")


# Add labels to the data points
text(x = seq_along(cooks.distance(model)), y= cooks.distance(model),
     labels = as.character(seq_along(cooks.distance(model))), 
     pos = 3, 
     offset = 0.2, cex = 0.5)
### COVRATIO        ####

plot(abs(covratio_m - 1), 
     ylab = "|COVRATIO-1|",
     main = "")

# Add a horizontal line at y = 9/39
abline(h = 9/39, col = "red", lty = 2)

# Add labels to the data points
text(x = seq_along(covratio_m), 
     y = abs(covratio_m - 1), 
     labels = as.character(seq_along(covratio_m)), 
     pos = 3, 
     offset = 0.2, cex = 0.5)
######## DFFITS       #################





######################


plot(rstudent_m)
plot(rstandard_m)





############# Sin 4           ##########################
df_4 <-  finney[-4, ] 
model_4 <- glm(df_4[,3] ~ log(df_4[,1]) + log(df_4[,2]), family = binomial)
summary(model_4)



#############   Sin 18        ############################
df_18 <-  finney[-18, ] 
model_18 <- glm(df_18[,3] ~ log(df_18[,1]) + log(df_18[,2]), family = binomial)
summary(model_18)



####### Sin 4 y 18        ################################
df_418 <-  finney[-c(4, 18), ] 
model_418 <- glm(df_418[,3] ~ log(df_418[,1]) + log(df_418[,2]), family = binomial)
summary(model_418)

#######################
w <- model$weights
W <- diag(w)
X <- model.matrix(model)
n<- nrow(X)
p <- ncol(X)
H<- solve(t(X)%*%W %*%X)
HH <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
res <- residuals(model, "pearson")
rp <- diag(res)
W_TEST <- X%*%inv(t(X)%*%X)%*%t(X)