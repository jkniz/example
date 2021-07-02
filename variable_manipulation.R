df <- read.csv("C:/Users/Morten Dreher/Desktop/DSM/DSM5/R/Project/Introduction-to-R-Project/cancer_data.csv")
read.csv("cancer_data.csv")
library(mc2d)
library(dplyr)
setwd("C:/Users/Morten Dreher/Desktop/DSM/DSM5/R/Project/Introduction-to-R-Project/")
getwd()
chol <- round(as.numeric(unlist(df[, 2])), digits = 2)
height <- round(as.numeric(unlist(df[, 5])), digits = 0)
weight <- round(as.numeric(unlist(df[, 8])), digits = 1)
size <- as.numeric(unlist(df[, 7]))
age <- as.numeric(unlist(df[, 1]))
smoker <- as.numeric(unlist(df[, 6]))
diet <- as.numeric(unlist(df[, 3]))
sex <- as.numeric(unlist(df[, 4]))
id <- as.numeric(unlist(df[, 9]))



hist(chol, 20)
max(chol)
mean(chol, na.rm = TRUE)
median(chol, na.rm = TRUE)


sum(is.na(chol))
chol

alc <- round(rchisq(n = 300, df = 4) * 25, digits = 1)
hist(alc, 20)
cigs_per_day <- floor(runif(n = 300, min = 1, max = 6)) * 10 * smoker
cigs_per_day

packyears <- (cigs_per_day / 20) * (age - round(runif(n = 12, min = 16, max = min(age)), digits = 2))
packyears

hbv <- rbern(n = 300, prob = .4)
hcv <- rbern(n = 300, prob = .6)
dia <- rbern(n = 300, prob = .5)

hbv
hcv
dia



range(height)
range(weight)
range(size)
range(age)


bmi <- round(weight / ((height / 100) * 2), digits = 1)
bmi

range(bmi)

bmi[which(is.na(chol))]

bmi_class <- case_when(
  bmi < 22 ~ 1,
  bmi >= 22 & bmi < 24 ~ 2,
  bmi >= 24 & bmi < 26 ~ 3,
  bmi >= 26 ~ 4
)
bmi_class
chol[which(is.na(chol))]
which(is.na(chol))
test <- mean(chol[bmi_class == bmi_class[which(is.na(chol))]])

test
bmi_class[which(is.na(chol))]
mean(chol[bmi_class == 1], na.rm = TRUE)
mean(chol[bmi_class == 2], na.rm = TRUE)
mean(chol[bmi_class == 3], na.rm = TRUE)
bmi_class[50]

chol[50] <- round(mean(chol[bmi_class == bmi_class[50]], na.rm = TRUE), digits = 2)
chol[50]
chol[72] <- round(mean(chol[bmi_class == bmi_class[72]], na.rm = TRUE), digits = 2)
chol[72]
chol[182] <- round(mean(chol[bmi_class == bmi_class[182]], na.rm = TRUE), digits = 2)
chol[182]


df_comp <- cbind(sex, age, height, weight, bmi, diet, chol, smoker, cigs_per_day, packyears, alc, size, bili, hbv, hcv, dia)
View(df_comp)
sum(is.na(df_comp))

write.csv(df_comp, file = "liver_cancer.csv")

bili <- round(1.8 + chol / 30 + size / 5 + alc / 100 + packyears / 100, digits = 2)
bili
range(bili)

hist(chol[diet == 1])
hist(chol[diet == 2])
hist(chol[diet == 3])

cor(size, chol)
plot(chol, size)

cor(bmi, size)
plot(bmi, size)

cor(packyears, size)
cor(alc, size)

plot(packyears, size)
plot(alc, size)

plot(age, size)
cor(age, size)

plot(bili, size)
cor(bili, size)

model <- lm(size ~ age + chol + bmi)
summary(model)

model_2 <- lm(size ~ hbv + hcv + dia)
summary(model_2)
