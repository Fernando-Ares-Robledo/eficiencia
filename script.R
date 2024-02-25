
file <-"D:/yo/dataset_paneles_solares.csv"
#install.packages("corrplot")
install.packages("car")
library(car)
library(corrplot)
if (!require(MASS)) install.packages("MASS")
library(MASS)

tabla <- read.csv(file, head=TRUE ,sep = ",")
print("head:")
print(head(tabla))

print("\n Summary:")
print(summary(tabla))
pairs(tabla)

M = cor(tabla)
corrplot.mixed(M,order="AOE")

print("Sumary modelo completo")
modelo <-lm(Eficiencia ~ ., data = tabla)
print(summary(modelo))
plot(modelo)


vif(modelo)
vifvalues <- vif(modelo) 

barplot(vifvalues , main = "VIF Values", horiz = TRUE, col = "steelblue") 
abline(v = 5, lwd = 3, lty = 2) 


print("Sumary modelo 2")
modelo2 <- lm(Eficiencia ~ Temperatura + Intensidad_Solar + Angulo_Inclinacion + Velocidad_Viento, data = tabla)
print(summary(modelo2))
vif(modelo2)
vifvalues <- vif(modelo2) 
barplot(vifvalues , main = "VIF Values", horiz = TRUE, col = "steelblue") 
abline(v = 5, lwd = 3, lty = 2) 
plot(modelo2, which=4)





residuals <- residuals(modelo2)
fitted_values <- fitted(modelo2)

plot(fitted_values, residuals, xlab = "Valores Ajustados", ylab = "Residuos", main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red")
lines(lowess(fitted_values, residuals), col = "blue")


print("b-p test")
if (!require("lmtest")) install.packages("lmtest")
library(lmtest)

bptest_result <- bptest(modelo2)

print(bptest_result)




print("q-q")
residuos <- rstandard(modelo2)

qqnorm(residuos)
qqline(residuos, col = "red")



print("comprobacion")



predicciones <- predict(modelo2, newdata = tabla)

diferencia <- predicciones - tabla$Eficiencia

mse <- mean(diferencia^2) 
rmse <- sqrt(mse)        
mae <- mean(abs(diferencia)) 

print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))



media_eficiencia <- mean(tabla$Eficiencia, na.rm = TRUE)

tabla$Eficiencia_Alta <- ifelse(tabla$Eficiencia > media_eficiencia, 1, 0)

print(head(tabla))

print(shapiro.test(modelo2$residuals))

print("modelolog")
modeloLog<-glm(Eficiencia_Alta ~ Temperatura + Intensidad_Solar + Angulo_Inclinacion + Velocidad_Viento , data = tabla, family=binomial)
print(summary(modeloLog))


all(c("Temperatura", "Intensidad_Solar", "Angulo_Inclinacion", "Velocidad_Viento") %in% names(tabla))


if("Temperatura" %in% names(tabla)) {
  print("La variable Temperatura existe en el dataframe.")
} else {
  stop("La variable Temperatura no se encuentra en el dataframe.")
}


 if("Intensidad_Solar" %in% names(tabla)) {
  print("La variable Intensidad_Solar existe en el dataframe.")
} else {
  stop("La variable Intensidad_Solar no se encuentra en el dataframe.")
}
print("llllllllllllllllllllllllllllllllllllllllllll")


print("sumary modeloLog")
modeloLog <- glm(Eficiencia_Alta ~ Temperatura + Intensidad_Solar + Angulo_Inclinacion, 
                 data = tabla, family = binomial)

print(summary(modeloLog))
#install.packages("caret")
#install.packages("pROC")

library(caret)       
library(pROC)        

predicted_probs <- predict(modeloLog, newdata = tabla, type = "response")

predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

confusionMatrix(data = factor(predicted_classes), reference = factor(tabla$Eficiencia_Alta))

roc_obj <- roc(response = tabla$Eficiencia_Alta, predictor = predicted_probs)
plot(roc_obj)
print(auc(roc_obj))

plot(modeloLog, which=4)


residuals <- residuals(modeloLog)
fitted_values <- fitted(modeloLog)

plot(fitted_values, residuals, xlab = "Valores Ajustados", ylab = "Residuos", main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red") 

lines(lowess(fitted_values, residuals), col = "blue")

print("------------------")


deviacion_residual_modelo_logistico <- residuals(modeloLog, type = "deviance")
residuos_pearson_modelo_logistico <- residuals(modeloLog, type = "pearson")

plot(fitted(modeloLog), deviacion_residual_modelo_logistico, xlab = "Fitted values", ylab = "Deviance residuals", main = "Deviance Residuals vs. Fitted")
plot(fitted(modeloLog), residuos_pearson_modelo_logistico, xlab = "Fitted values", ylab = "Pearson residuals", main = "Pearson Residuals vs. Fitted")