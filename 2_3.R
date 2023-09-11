#---DEPENDENCIES----------------------------------------------------------------

library("readxl") # to handle .xlsx files
library("ggpubr") # plotting
library("ggplot2")
library("glue")   # formatted strings

#---2.3-------------------------------------------------------------------------

# data set
d <- readxl::read_excel("Glucose_BP_levels.xlsx")


# drawing a correlation scatter plot b/w the variables with a regression line
# and confidence interval
scatter_corr = ggscatter(data = d, 
                         x = "Glucose levels [mg/dL]", 
                         y = "BP [mmHg]", 
                         add = "reg.line", 
                         conf.int = TRUE, 
                         cor.coef = TRUE,
                         ggtheme = theme_gray(),
                         color = "black",
                         title = "Correlation plot b/w Glucose levels and BP",
                         cor.method = "pearson",
                         xlab = "Glucose levels [mg/dL]", 
                         ylab = "BP [mmHg]")
plot(scatter_corr) # plot

# finding pearson correlation coefficient
glucose <- as.numeric(d$`Glucose levels [mg/dL]`)
bp <- as.numeric(d$`BP [mmHg]`)
p <- cor.test(x = glucose, y = bp, method = "pearson")
print("Pearson correlation coefficient and other statistics :")
print("")
print(p)

#---confidence intervals--------------------------------------------------------

# confidence interval Glucose Levels
sample.mean <- mean(glucose) # mean
sample.n <- length(glucose)  # number of samples
sample.sd <- sd(glucose)     # standard deviation
sample.se <- sample.sd/sqrt(sample.n) # standard error

alpha = 0.01 # alpha = 1% given
degrees.freedom = sample.n - 1 # degree of freedom
t.score = qt(p = alpha/2, df = degrees.freedom, lower.tail = F) # tscore

margin.error <- t.score * sample.se
glower <- sample.mean - margin.error # upper bound for glucose levels
gupper <- sample.mean + margin.error # lower bound for glucose levels

print(glue("lower bound of CI for glucose levels is {glower}"))
print(glue("upper bound of CI for glucose levels is {gupper}"))

# confidence interval BP
sample.mean <- mean(bp) # mean
sample.n <- length(bp)  # number of samples
sample.sd <- sd(bp)     # standard deviation
sample.se <- sample.sd/sqrt(sample.n) # standard error

degrees.freedom = sample.n - 1 # degree of freedom
t.score = qt(p = alpha/2, df = degrees.freedom, lower.tail = F) # tscore

margin.error <- t.score * sample.se
blower <- sample.mean - margin.error # lower and upper bounds of CI for BP
bupper <- sample.mean + margin.error

print(glue("lower bound of CI for BP is {blower}"))
print(glue("upper bound of CI for BP is {bupper}"))

# plotting ; histogram with curve and confidence intervals in RED according to given alpha

# BP
hist(bp, 
     prob = TRUE, 
     breaks = 25,
     xlab = "BP (mmHg)",
     ylab = "probability",
     col = "orange",
     border = "black")
lines(density(bp), col = "black")

abline(v = blower, col = "red", lwd=2)
abline(v = bupper, col = "red", lwd=2)

# glucose
hist(glucose, 
     prob = TRUE, 
     breaks = 25,
     xlab = "Glucose (mg/dL)",
     ylab = "probability",
     col = "pink",
     border = "black")
lines(density(glucose), col = "black")

abline(v = glower, col = "red", lwd=2)
abline(v = gupper, col = "red", lwd=2)

#---residuals-------------------------------------------------------------------

# defining a linear model to get the regression line
model <- lm(bp~glucose)
print(summary(model)) # model summary to find residuals

# calculating standard residuals
standard_res <- rstandard(model)
print("standard residuals :")
print(standard_res)

# residual plot
plot(fitted(model), standard_res,
     main = "Residual plot",
     col = "blue")
par(bg = "grey")

#add a horizontal line at 0
abline(0,0)

# distribution of errors
plot(hist(standard_res))



