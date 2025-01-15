# Load necessary libraries
library(tidyverse) # For data manipulation and visualization
library(readr)     # For reading files with various encodings

# Step 1: Load the dataset
file_path <- "C:\\Users\\test\\Desktop\\1MD\\02_hr_data_v00.csv" #Must change on Yours
data <- read_csv2(file_path, locale = locale(encoding = "ISO-8859-1"))

# Step 2: Inspect the data structure
glimpse(data)

# Step 3: Identify and fix issues with column names and missing rows
# Use the fourth row as the header
colnames(data) <- as.character(unlist(data[4, ]))
data <- data[-(1:2), ]  # Remove metadata rows
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Step 4: Remove Column "A" if it contains no data
data <- data[, colSums(is.na(data)) != nrow(data)]  # Keep only non-empty columns

# Step 4: Rename columns for better understanding
colnames(data) <- c("Age", "LeftCompany", "TravelFrequency", "Department", 
                    "DistanceFromHome", "Education", "EducationField", 
                    "EmployeeCount", "EmployeeID", "Gender", "JobLevel", 
                    "JobRole", "MaritalStatus", "MonthlyIncome", 
                    "NumCompaniesWorked", "Over18", "PercentSalaryHike", 
                    "StandardHours", "StockOptionLevel", "TotalWorkingYears", 
                    "TrainingTimesLastYear", "YearsAtCompany", 
                    "YearsSinceLastPromotion", "YearsWithCurrManager")

# Step 5: Convert columns to appropriate data types
numeric_columns <- c("Age", "DistanceFromHome", "MonthlyIncome", 
                     "NumCompaniesWorked", "PercentSalaryHike", 
                     "StandardHours", "StockOptionLevel", "TotalWorkingYears", 
                     "TrainingTimesLastYear", "YearsAtCompany", 
                     "YearsSinceLastPromotion", "YearsWithCurrManager")

# Convert columns to numeric, ensuring non-numeric values become NA
data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
  x <- as.character(x)
  as.numeric(ifelse(grepl("^-?\\d+(\\.\\d+)?$", x), x, NA))  # Strict numeric validation
})

# Ensure 'LeftCompany' column contains only 'YES' or 'NO'. Replace other values with NA
data$LeftCompany <- ifelse(data$LeftCompany %in% c("Yes", "No", "NO"), data$LeftCompany, NA)

# Ensure 'Gender' column contains only 'Male' or 'Female'. Replace other values with NA
data$Gender <- ifelse(data$Gender %in% c("Male", "Female"), data$Gender, NA)

# Ensure 'Over18' column contains only 'Y' or 'N'. Replace other values with NA
data$Over18 <- ifelse(data$Over18 %in% c("Y", "N"), data$Over18, NA)

# Ensure 'Education' column contains only 'College' or 'MASTER'. Replace other values with NA
data$Education <- ifelse(data$Education %in% c("College", "MASTER"), data$Education, NA)

# Ensure 'MaritalStatus' column contains only 'Single', 'Married', or 'Divorced'. Replace other values with NA
data$MaritalStatus <- ifelse(data$MaritalStatus %in% c("Single", "Married", "Divorced"), data$MaritalStatus, NA)

# Step 7: Document data types
str(data)

# Step 8: Summary statistics
summary(data)

# Step 9: Save cleaned data
write_csv(data, "C:\\Users\\test\\Desktop\\1MD\\cleaned_hr_data.csv") #Must change on Yours

# Print final summary
cat("Final dataset has", nrow(data), "rows and", ncol(data), "columns.\n")