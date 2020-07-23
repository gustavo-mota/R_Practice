#23 questão
#a
data <- c(350, 350, 350, 358, 370, 370, 370, 371, 371, 372, 372, 384, 391, 391, 392)

media <- mean(data)  # 370.8

variancia <- var(data)  # 207.6

#24 questão
#a
# O grupo Y pois apesar da média menor, possui alunos com notas acima da média
#b
# 75%
#c
#O grupo Y pois o boxplot tem um máximo maior
#d
# O grupo X tem assimetria inferior, determinando ter mais alunos com notas abaixo da média que acima da média,
# e o grupo Y tem assimetria superior, determinando ter mais alunos com notas acima da média
#e
# O grupo X tem 2.5% pois está um acima de 2%, e o grupo Y tem 1.8% pois está muito próximo de 2%
#f
# Não, não se vê ponto fora do boxplot
#g
# O grupo X pois tem notas mais baixas

#26 questão
data <- c(56, 60, 62, 65, 69, 70, 70, 72, 72, 72, 73, 74, 76, 76, 76, 78, 78, 80, 81, 82, 83, 83, 84, 84, 85, 86, 86, 87, 88, 88, 89, 89, 90, 91, 92, 93, 94, 96, 97, 98)

#a

moda <- which.max(table(data))  # 72

media <- mean(data)  # 80.625

mediana <- median(data)  # 82.5 

variancia <- var(data)  # 109.06

quartis <- quantile(data)  
#  0%   25%   50%   75%  100% 
#56.00 72.75 82.50 88.25 98.00 

#b
#Sim, de que tem assimetria a esquerda
ca <- 3*(media-mediana)/sqrt(variancia)  # -0.538

#c
#Sim, a mediana está muito próxima da direita a assimetria é a mesma
boxplot(data, horizontal=TRUE)

#d
#Não, não há pontos fora do boxplot

#28 questão
#a
#O tipo2 pois o limite inferior é maior que a de tipo 1
tip1 <- c(350, 350, 350, 358, 370, 370, 370, 371, 371, 372, 372, 384, 391, 391, 392)
tip2 <- c(350, 354, 359, 363, 365, 368, 369, 371, 373, 374, 376, 380, 383, 388, 392)

boxplot(tip1, tip2)

#b
#Sim
3*(mean(tip1)-median(tip1))/sqrt(var(tip1))  # -0.041

3*(mean(tip2-median(tip2))/sqrt(var(tip2))  # 0

#c
# O tipo 1

#38 questão

data <- c(79,80,81,83,84,84,84,84,85,86,91,93,93,93,95,95,95,96,96,98,98,100,101,103,104,105,109,109,110,111,112,113,113,117,118,118,120,120,122,123,124,125,125,126,127,127,129,129,130,132,132,134)

#a
quantile(data)
#0%    25%    50%    75%   100% 
#79.00  93.00 107.00 122.25 134.00 

quantile(data, seq(0.01, 0.99,0.01))["80%"]  # 124.8

#b
# Não
boxplot(data)

#c
# Sim, a mediana está bem acima do peso recomendado e existem muitos acima desse valor pela assimetria

