# ANOVA coefficients table video script

# Simple one-way

set.seed(210)

Treatment <- as.factor(rep(c("Control", "A", "B", "C"), each = 30))

Systolic_pressure <- c(rnorm(30, 150, sd = 4),
                       rnorm(30, 148, sd = 4),
                       rnorm(30, 145, sd = 5),
                       rnorm(30, 150, sd = 4))

boxplot(Systolic_pressure ~ Treatment,
        ylab = "Systolic blood pressure (mmHg)",
        col = "aquamarine4")

summary(lm(Systolic_pressure ~ Treatment))

Treatment <- relevel(Treatment, ref = "Control")

summary(lm(Systolic_pressure ~ Treatment))


# Two factor without interaction

set.seed(213)

Treatment <- as.factor(rep(c("Control", "A", "B", "C"), each = 40))

Age_class <- as.factor(rep(c("40-50", "70-80"), times = 80))

Systolic_pressure <- c(rnorm(40, 150, sd = 4),
                       rnorm(40, 149, sd = 4),
                       rnorm(40, 145, sd = 5),
                       rnorm(40, 153, sd = 4))

Systolic_pressure <- ifelse(Age_class == "70-80", Systolic_pressure + runif(1, min = 3, max = 5), Systolic_pressure)

boxplot(Systolic_pressure ~ Age_class*Treatment,
        ylab = "Systolic blood pressure (mmHg)",
        col = c("aquamarine4", "darkorange"))

summary(lm(Systolic_pressure ~ Treatment+Age_class))

Treatment <- relevel(Treatment, ref = "Control")

summary(lm(Systolic_pressure ~ Treatment + Age_class))

tapply(Systolic_pressure, 
       INDEX = list(Treatment, Age_class), 
       FUN = mean)



# Two factor with interaction

set.seed(20)

Treatment <- as.factor(rep(c("Control", "A", "B", "C"), each = 40))

Age_class <- as.factor(rep(c("40-50", "70-80"), times = 80))

Systolic_pressure <- c(rnorm(40, 150, sd = 4),
                       rnorm(40, 148, sd = 4),
                       rnorm(40, 145, sd = 5),
                       rnorm(40, 150, sd = 4))

Systolic_pressure <- ifelse(Age_class == "70-80" & Treatment != "B", Systolic_pressure + runif(1, min = 0, max = 10), Systolic_pressure)
Systolic_pressure <- ifelse(Age_class == "70-80" & Treatment == "B", Systolic_pressure + runif(1, min = 0, max = 2), Systolic_pressure)

boxplot(Systolic_pressure ~ Age_class*Treatment,
        ylab = "Systolic blood pressure (mmHg)",
        col = c("aquamarine4", "darkorange"))

summary(lm(Systolic_pressure ~ Treatment*Age_class))

Treatment <- relevel(Treatment, ref = "Control")

summary(lm(Systolic_pressure ~ Treatment * Age_class))


tapply(Systolic_pressure, 
       INDEX = list(Treatment, Age_class), 
       FUN = mean)
