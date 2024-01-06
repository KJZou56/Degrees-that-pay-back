## set the working directory and import data sets
setwd("C:/Users/kj_zo/Desktop/Data science course work/Exploratory data analysis and visulization/Group project")
library(readr)
degrees_that_pay_back <- read_csv("degrees-that-pay-back.csv")
salaries_by_college_type <- read_csv("salaries-by-college-type.csv")
salaries_by_region <- read_csv("salaries-by-region.csv")

summary(degrees_that_pay_back) ## clean

summary(salaries_by_college_type) ##  wrong data type: column 5 and 8
columns_to_convert <- c(5,8) 
salaries_by_college_type[, columns_to_convert] <- sapply( ## fix the data type
  salaries_by_college_type[, columns_to_convert],
  function(x) as.numeric(x)
)

# Replace NAs with mean in col 5 and col 8
salaries_by_college_type$`Mid-Career 10th Percentile Salary`[is.na(salaries_by_college_type$`Mid-Career 10th Percentile Salary`)] <- mean(salaries_by_college_type$`Mid-Career 10th Percentile Salary`, na.rm = TRUE)
salaries_by_college_type$`Mid-Career 90th Percentile Salary`[is.na(salaries_by_college_type$`Mid-Career 90th Percentile Salary`)] <- mean(salaries_by_college_type$`Mid-Career 90th Percentile Salary`, na.rm = TRUE)

summary(salaries_by_region)
salaries_by_region$Region<-as.factor(salaries_by_region$Region)
columns_to_convert <- c(5,8) 
salaries_by_region[, columns_to_convert] <- sapply( ## fix the data type
  salaries_by_region[, columns_to_convert],
  function(x) as.numeric(x)
)

# Replace NAs with mean in col 5 and col 8
salaries_by_region$`Mid-Career 10th Percentile Salary`[is.na(salaries_by_region$`Mid-Career 10th Percentile Salary`)] <- mean(salaries_by_region$`Mid-Career 10th Percentile Salary`, na.rm = TRUE)
salaries_by_region$`Mid-Career 90th Percentile Salary`[is.na(salaries_by_region$`Mid-Career 90th Percentile Salary`)] <- mean(salaries_by_region$`Mid-Career 90th Percentile Salary`, na.rm = TRUE)

#################### plot 1 
library(dplyr)
library(ggplot2)
degrees_that_pay_back <- degrees_that_pay_back %>%
  mutate(`Undergraduate Major` = reorder(`Undergraduate Major`, `Starting Median Salary`))

degrees_that_pay_back$percentage_change_trans<-degrees_that_pay_back$`Percent change from Starting to Mid-Career Salary`*1000
ggplot(degrees_that_pay_back, aes(x = `Undergraduate Major`)) +
  geom_line(aes(y = `Starting Median Salary`, group = 1, color = "Starting Median Salary")) +
  geom_line(aes(y = `Mid-Career Median Salary`, group = 1, color = "Mid-Career Median Salary")) +
  geom_point(aes(y = `Starting Median Salary`, color = "Starting Median Salary")) +
  geom_point(aes(y = `Mid-Career Median Salary`, color = "Mid-Career Median Salary")) +
  # Add another line on the second y-axis
  geom_line(aes(y = percentage_change_trans, group = 2, color = "Percent change from Starting to Mid-Career Salary")) +
  
  # Points for the new line
  geom_point(aes(y = percentage_change_trans, color = "Percent change from Starting to Mid-Career Salary")) +
  
  scale_y_continuous(
    
    name = "Salaries in thousands",labels = scales::comma_format(scale = 1e-3, suffix = ""),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1000, name="Percentage change \n %")
  ) + 
  scale_color_manual(
    name = "Legend",
    values = c("Starting Median Salary" = "blue", "Mid-Career Median Salary" = "red", "Percent change from Starting to Mid-Career Salary" = "green"),
    breaks = c("Starting Median Salary", "Mid-Career Median Salary", "Percent change from Starting to Mid-Career Salary")
  ) +
    labs(title = "Median Salary (Starting vs Mid-career vs Percentage change)",
       fill = "Legend",
       color = "Legend") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_hline(yintercept = 55000, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 90000, linetype = "dashed", color = "red")


##############################

############################# plot 2
# Assuming salaries_by_college_type is your data frame
ggplot(salaries_by_college_type, aes(x = `Starting Median Salary`, y = `Mid-Career Median Salary`, shape = `School Type`, color = `School Type`)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 18, 19,20)) +  # Set different shapes for categories
  labs(title = "Starting vs. Mid-Career Median Salary by School Type",
       x = "Starting Median Salary",
       y = "Mid-Career Median Salary")+
  theme_minimal()
########################### 

############################ plot 3
ggplot(salaries_by_region, aes(x = `Starting Median Salary`, y = `Mid-Career Median Salary`, shape = `Region`, color = `Region`)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 18, 19,20)) +  # Set different shapes for categories
  labs(title = "Starting vs. Mid-Career Median Salary by Region",
       x = "Starting Median Salary",
       y = "Mid-Career Median Salary")+
  theme_minimal()
###########################

summary(salaries_by_college_type)
summary(salaries_by_region)
merged<-merge(salaries_by_college_type,salaries_by_region,by="School Name")

my_model<-lm(`Mid-Career Median Salary.x`~merged$`School Type`+merged$Region+merged$`Starting Median Salary.x`,data = merged)

summary_data <- summary(my_model)

# Extract coefficients, p-values, and R-squared
coefficients <- coef(summary_data)
pvalues <- coefficients[, "Pr(>|t|)"]
r_squared <- summary_data$r.squared

# Round all numbers to the third decimal place
coefficients <- round(coefficients, 2)
pvalues <- round(pvalues, 2)
r_squared <- round(r_squared, 2)

# Create a data frame for coefficients and p-values
coeff_pvalue_df <- data.frame(
  Coefficients = coefficients[, 1],
  P_Values = pvalues,
  Significance = ifelse(pvalues <= 0.001, '***', ifelse(pvalues <= 0.01, '**', ifelse(pvalues <= 0.05, '*', '.')))
)

# Create a data frame for R-squared
r_squared_df <- data.frame(
  R_Squared = r_squared
)

# Display the tabulated information using knitr::kable
cat("Coefficients and P-Values:\n")
knitr::kable(coeff_pvalue_df, format = "markdown")

cat("\nR-Squared:\n")
knitr::kable(r_squared_df, format = "markdown")
