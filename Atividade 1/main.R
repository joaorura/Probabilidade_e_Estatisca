library(dplyr)
require(dplyr)

imc = function (weight, higher, amount_digits) {
  imc = weight / (higher^2)
  imc = round(imc, digits=amount_digits)
  return(imc)
}
  

data = read.table("./data/data.csv", header=T, sep=",", dec=".",fileEncoding="UTF-8")
data = data[2:5]
data["IMC"] = imc(data$Peso, data$Altura, 0)

freq_imc = table(data$IMC)

mean_imc = mean(data$IMC)
mean_imc

median_imc = median(data$IMC)
median_imc

four_decil = quantile(data$IMC, 0.4)
four_decil

imc_division = data

# f = c("Muito Abaixo do Peso", "Abaixo do Peso", "Peso Normal", 
#             "Acima do Peso", "Obesidade 1", "Obesidade 2", "Obesidade 3")

# group_by(data, f) %>% data$IMC