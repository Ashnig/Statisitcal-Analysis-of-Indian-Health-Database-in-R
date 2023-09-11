#---DEPENDENCIES----------------------------------------------------------------

library("readxl") # to handle database
library("glue")   # to handle formatted strings
library("ggplot2")

#---2.2.1-----------------------------------------------------------------------

# accessing All data sheet from India Health Database
alldata <- readxl::read_excel("India_Health_Database.xlsx","All data")

# making lists of population data for further computation
pop11 <- alldata[,"Population 2011"]
pop18 <- alldata[,"Population 2018"]

# calculating difference in population b/w 2018 and 2011
pop_diff <- pop18 - pop11

# assuming a linear trend and equally spaced values of independent
# variable (year), the step change in population can be calculated
# by dividing the total difference with the distance b/w the two 
# values of independent variable (year) used
step_ <- pop_diff / 7 # 2018 -2011

# initiating results
results2_2_1 <- c(pop11) # population data 2011 already present

# adding columns corresponding to the extrapolated years to results
for (year in 2012:2017){
  n_step <- year - 2011 # number of steps to be added
  pop_year <- pop11 + n_step * step_ # calculating data for year
  results2_2_1 <- append(results2_2_1,pop_year)# appending to results
}
results2_2_1 <- append(results2_2_1,pop18) # appending population data of 2018

# converting list of list to a data frame
results2_2_1 <- data.frame(results2_2_1)
# changing column names
colnames(results2_2_1) <- c('2011','2012','2013','2014','2015','2016','2017','2018')

print("Extrapolated total population")
print(cbind(alldata$State,results2_2_1))

#---2.2.2-----------------------------------------------------------------------

# accessing vector borne diseases sheet from India Health Database (modified)
dengue_data <- readxl::read_excel("India_Health_Database.xlsx","Vector borne diseases")

# choosing alternate rows to get the Death columns
dd_deaths <- dengue_data[,seq_len(ncol(dengue_data)) %% 2 == 1]
# removing the states column
dd_deaths["States"] <- NULL
# choosing alternate rows to get the Death columns
dd_cases <- dengue_data[,seq_len(ncol(dengue_data)) %% 2 == 0]

# calculating lethality year - wise
dd_lethality <- dd_deaths / dd_cases

# when Deaths and Cases are 0 lethality is undefined, we will consider it as 0
dd_lethality[is.na(dd_lethality)] = 0

# calculating average lethality of each state over the years 
avg_lethality <- rowMeans(dd_lethality)
# appending lethality column
dd_lethality["avg lethality"] <- avg_lethality

print("average lethality of all states")
print(cbind(alldata$State,avg_lethality))

# adding states column to the front
dd_lethality <- cbind(dengue_data["States"],dd_lethality)
# correcting column names
colnames(dd_lethality) <- c("States",
                            "lethality 2013",
                            "lethality 2014",
                            "lethality 2015",
                            "lethality 2016",
                            "lethality 2017",
                            "lethality 2018(P)",
                            "avg. lethality")

# confidence intervals
print("Average lethality each year")
avg_lethality_yr <- colMeans(dd_lethality[2:7])
print(avg_lethality_yr)

# plotting avg lethality
plot(avg_lethality_yr,type = "o",xlab = "year",ylab = "average lethality",main = "lethalities across years")
axis(1, at = 1:6, labels=2013:2018)

# function to find confidence interval
ci <- function(col){
  mean_value <- mean(col)
  
  # Compute the size
  n <- length(col)
  
  # Find the standard deviation
  standard_deviation <- sd(col)
  
  # Find the standard error
  standard_error <- standard_deviation / sqrt(n)
  alpha = 0.317 # alpha threshold for CI +- SI since CI = 68.3 %
  degrees_of_freedom = n - 1
  t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)
  margin_error <- t_score * standard_error
  
  # Calculating lower bound and upper bound
  lower_bound <- mean_value - margin_error
  upper_bound <- mean_value + margin_error
  
  return(c(lower_bound,upper_bound)) # return CI
}

print("Confidence Intervals for each year : ")
ci_13 <- ci(dd_lethality$'lethality 2013')
ci_14 <- ci(dd_lethality$'lethality 2014')
ci_15 <- ci(dd_lethality$'lethality 2015')
ci_16 <- ci(dd_lethality$'lethality 2016')
ci_17 <- ci(dd_lethality$'lethality 2017')
ci_18 <- ci(dd_lethality$'lethality 2018(P)')
print("lethality 2013")
print(ci_13)

print("lethality 2014")
print(ci_14)

print("lethality 2015")
print(ci_15)

print("lethality 2016")
print(ci_16)

print("lethality 2017")
print(ci_17)

print("lethality 2018(P)")
print(ci_18)

print("Yes confidence intervals include avg values")

# in t-tests we can compare means of 2 but in AOV we can
# compare more than 2 variables

df <- data.frame(
  Year <- c(2013:2018),
  avg_leth <- c(avg_lethality_yr),
  stringsAsFactors = FALSE
)

onewaytest <- aov(Year~avg_leth,data = df)
print(summary(onewaytest))

# correlation b/w urbanization and lethality
urbanisation <- alldata$`Urban Population (%)` # urban population
leth <- dd_lethality$`avg. lethality`       # avg lethality across years
states <- dd_lethality$States

df <- data.frame(states,leth,urbanisation)

# state vs urbanization
barplot(df$urbanisation,
        names.arg = df$states,
        xlab = "States",
        ylab = "urbanization %",
        main = "States vs Urbanization %",
        cex.names = 0.4,
        col = c("pink")
)

# state vs avg lethality
barplot(df$leth,
        names.arg = df$states,
        xlab = "States",
        ylab = "avg lethality",
        main = "States vs avg lethality",
        cex.names = 0.4,
        col = c("darkolivegreen3")
)
print("correlatipon b/w urbanisation and avg. lethality : ")
print(cor(urbanisation,leth))

































