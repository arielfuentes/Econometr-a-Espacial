# https://www.kaggle.com/mirichoi0218/insurance

# Columns
# 
# age: age of primary beneficiary
# 
# sex: insurance contractor gender, female, male
# 
# bmi: Body mass index, providing an understanding of body, weights that are relatively high or low relative to height,
# objective index of body weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5 to 24.9
# 
# children: Number of children covered by health insurance / Number of dependents
# 
# smoker: Smoking
# 
# region: the beneficiary's residential area in the US, northeast, southeast, southwest, northwest.
# 
# charges: Individual medical costs billed by health insurance
# 
# Acknowledgements
# The dataset is available on GitHub here.
# 
# Inspiration
# Can you accurately predict insurance costs?

library(readr)
insurance <- read_csv("C:/Users/claud/Desktop/cursos R/insurance.csv")

summary(insurance)

#transformar campos de texto
insurance$sex_num = ""
index = which(insurance$sex == "female")
insurance$sex_num[index] = 1
insurance$sex_num[-index] = 0
insurance$sex_num = as.numeric(insurance$sex_num)

insurance$smoker_num = ""
index = which(insurance$smoker == "yes")
insurance$smoker_num[index] = 1
insurance$smoker_num[-index] = 0
insurance$smoker_num = as.numeric(insurance$smoker_num)

#subselect
data = insurance[,c(1,3,4,7,8,9)]

library("PerformanceAnalytics")
chart.Correlation(data[data$bmi>18,], histogram=TRUE, pch=19)

data = insurance[,c(1,3,4,9,7)]

# transformación de datos
log_data = data
for (i in names(log_data)){
  log_data[,i] = log1p(log_data[,i])
}

chart.Correlation(log_data, histogram=TRUE, pch=19)

# definir formula

formula = "charges~age+bmi+children+smoker_num"

# calibrar modelo
model = lm(formula, data)
summary(model)

log_model = lm(formula, log_data)
summary(log_model)

plot(model)
plot(log_model)

# ¿Qué pasa si no considero el intercepto?
formula = "charges~age+bmi+children+smoker_num-1"
model_si = lm(formula, log_data)
summary(model_si)
plot(model_si)

# ¿Qué pasa con las variables que son categóricas?
data_cat = insurance[,c(1,3,4,5,7)]
formula = "charges~age+bmi+children+smoker"
model_cat = lm(formula, data_cat)
summary(model_cat)
plot(model_cat)

# ¿Si agregamos espacialidad?
summary(as.factor(insurance$region))
data_esp = insurance[,c(1,3,4,5,6,7)]
formula = "charges~age+bmi+children+smoker+region"
model_esp = lm(formula, data_esp)
summary(model_esp)
plot(model_esp)

# capacidad de prediccion
log_data$predict = predict.lm(model_si,log_data)
# Calcular el error empírico
log_data$error = abs(log_data$charges - log_data$predict)/log_data$charges * 100
# Ver el promedio del error
mean(log_data$error)
#plot
plot(log_data$charges, log_data$predict, xlab="Valor Teórico", ylab="Valor Predicho")

#partición de datos
set.seed(1)
#Solo como muestra

test.rows = sample(1:nrow(pivot_1), nrow(pivot_1)*0.6)
test.set.1 = pivot_1[test.rows, ]
train.set.1 = pivot_1[-test.rows, ]