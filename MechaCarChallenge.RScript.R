#Part 1: Linear Regression to Predict MPG

## Library() function to load dplyr package
library(dplyr)

## Import and read in the MechaCar_mpg.csv file as a dataframe
MechaCar_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
View(MechaCar_table)

## Perform linear regression using the lm() function.
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_table)

## Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_table))

# Part 2: Summary Statistics on Suspension Coils

## Import and read in the Suspension_Coil.csv file as a table
Suspensioncoil_table <- read.csv(file='Suspension_Coil',check.names=F,stringsAsFactors = F)
View(Suspensioncoil_table)

## Create a total_summary dataframe using the summarize() function to get the mean, median, variance, and standard deviation of the suspension coil’s PSI column.
### Ref https://www.geeksforgeeks.org/calculate-the-average-variance-and-standard-deviation-in-r-programming/
Total_Summary <- Suspensioncoil_table %>% summarize(Mean_PSI = mean(PSI),Median_PSI = mean(PSI),Variance_PSI = var(PSI),Standard_Deviation_PSI = sd(PSI))
View(Total_Summary)

## Create a lot_summary dataframe using the group_by() and the summarize() functions to group each manufacturing lot by the mean, median, variance, and standard deviation of the suspension coil’s PSI column.
Lot_Summary <- Suspensioncoil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI = mean(PSI),Median_PSI = mean(PSI),Variance_PSI = var(PSI),Standard_Deviation_PSI = sd(PSI))
View(Lot_Summary)

# Part 3: T-Test on Suspension Coils

##t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
t.test(Suspensioncoil_table$PSI,mu=1500)

## t.test() function and its subset() argument to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch.
### Ref 16-R 2 act 3
Lot1 = subset(Suspensioncoil_table, Manufacturing_Lot == "Lot1")
View(Lot1)
Lot2 = subset(Suspensioncoil_table, Manufacturing_Lot == "Lot2")
View(Lot2)
Lot3 = subset(Suspensioncoil_table, Manufacturing_Lot == "Lot3")
View(Lot3)

t.test(Lot1$PSI,mu=1500)
t.test(Lot2$PSI,mu=1500)
t.test(Lot3$PSI,mu=1500)