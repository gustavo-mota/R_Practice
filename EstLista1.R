#7 questão

x <- c(0.56,  -0.23, 1.56, 0.07, 0.13, 1.72, 0.46, -1.27,    -0.69,    -0.45)

media <- mean(x)
mediana <- median(x)

y <- length(x)
variancia <- var(x)
variancia_amostral <- var(x)*(y-1)/y

dv <- sqrt(variancia)
dv_amos <- sqrt(variancia_amostral)

cv <- dv/media
cv_amos <- dv_amos/media

#  0.25,  0.5,  0.75,  0.9
#quantile(x, seq(0.01, 0.1,  0.25,  0.5,  0.75,  0.9,0.99))
qnt <- quantile(x,seq(0.01,0.99,0.01))
#quantile(x,seq(0.01,0.99,0.1))

#boxplot(x, horizontal=TRUE)

ca <- 3*(media - mediana)/variancia

#8 questão
variancia <- sum(
    ((152-162)**2)*4,
    ((156-162)**2)*9,
    ((160-162)**2)*11,
    ((164-162)**2)*8,
    ((168-162)**2)*5,
    ((172-162)**2)*3
    )/40

dv <- sqrt(variancia)

#9 questão
x <- c(-0.96,    -0.29,    0.26,    -1.15,    0.20, 0.03, 0.09, 1.12,    -1.22,    1.27)

y <- length(x)

mediana <- median(x)

media.arit <- mean(x)

media.harm <- 1/mean(1/x)

#media.geo <- exp(sum(log(x[x > 0])) / length(x))

media.geo <- (prod(x))**(1/length(x))

variancia <- var(x)
variancia_amostral <- var(x)*(y-1)/y

dv <- sqrt(variancia)
dv_amos <- sqrt(variancia_amostral)

cv <- dv/media.arit
cv_amos <- dv_amos/media.arit

ca <- 3*(media.arit - mediana)/variancia

#10 quetão
data <- c(1,2,1,6,6,2,4,4,3,5,3,3,4,3,3,4,4,5,5,5,4,4,4,4,6,5,5,6,4,6)

n <- length(data)

x <- table(data)

Fi <- as.vector(x)

Fci <- cumsum(Fi)

suf <- paste("/",n, sep="")

Fri <- as.factor(paste(Fi,suf,sep=""))

Fcri <- as.factor(paste(Fci,suf,sep=""))

df <- data.frame(value = as.factor(names(x)),Fi, Fci, Fri, Fcri)

media.arit <- mean(data)

media.harm <- 1/mean(1/data)

media.geo <- (prod(data))**(1/n)

moda <- mode(data)

mediana <- median(data)

variancia <- var(data)
variancia_amostral <- var(data)*(n-1)/n

dv <- sqrt(variancia)
dv_amos <- sqrt(variancia_amostral)

cv <- dv/media.arit
cv_amos <- dv_amos/media.arit

ca <- 3*(media.arit - mediana)/variancia

#14 questão
media <- (154+162+168+178+186)/70

moda <- 174 + ((27 - 18)/((27 - 18)+(27 - 8))) * 8

Fci <- cumsum(c(5,12,18,27,8))

mediana <- 166 + (((70/2) - 17)/18) * 8

pos_q1 <- (70*0.25) # 3 classe

pos_q3 <- (70*0.75) # 4 classe

q1 <- (pos_q1 - 17)/18 * (174-166) + 166

q3 <- (pos_q3 - 35)/27 * (182 - 174) + 174

por_01 <- (70*0.01)

por_10 <- (70*0.1)

por_15 <- (70*0.15)

por_23 <- (70*0.23)

por_90 <- (70*0.9)

p01 <- (por_01 - 0)/5 * (158 - 150) + 150

p10 <- (por_10 - 5)/12 * (166 - 158) + 158

p15 <- (por_15 - 5)/12 * (166 - 158) + 158

p23 <- (por_23 - 5)/12 * (166 - 158) + 158

p90 <- (por_90 - 62)/8 * (190 - 182) + 182

variancia <- sum(
    ((154-media)**2)*5,
    ((162-media)**2)*12,
    ((168-media)**2)*18,
    ((178-media)**2)*27,
    ((186-media)**2)*8
    )/70

dv <- sqrt(variancia)

#15 questão
data <- c(6, 8, 5,12, 11, 7, 4, 15)

media <- mean(data)

desvios <- data - media

soma <- sum(desvios)

#16 questão
c.v.est <- 0.8/7.8

c.v.comp <- 0.76/7.3 # foi maior

#17 questão
#a
data <- c(3, 5, 2, 6, 5, 9, 5, 2, 8, 6)
media <- mean(data)

moda <- table(data) # a tabela mostra

mediana <- median(data)

#b
data <- c(20, 9, 7, 2, 12, 7, 20, 15, 7)
media <- mean(data)

moda <- table(data) # a tabela mostra

mediana <- median(data)

#18 questão
#a
notas <- c(2,3,4,5,6,7,8 ,9,10)
alunos <- c(1,3,6,10,13 ,8,5,3,1)

sum(notas*alunos)/sum(alunos)

#b
sum(alunos) # revela o cálculo da mediana
cumsum(alunos) # mostra a posição

#c está na tabela

#19 questão
c.v.altura <- 8.01/162.2 #maior

c.v.peso <- 2.3/52

