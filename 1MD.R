# Load necessary libraries
library(tidyverse) # For data manipulation and visualization
library(readr)     # For reading files with various encodings

# Step 1: Load the dataset
file_path <- "C:\\Users\\test\\Desktop\\1MD\\02_hr_data_v00.csv"
data <- read_csv2(file_path, locale = locale(encoding = "ISO-8859-1"))

# Step 2: Inspect the data structure
glimpse(data)

# Step 3: Identify and fix issues with column names and missing rows
# Use the fourth row as the header
colnames(data) <- as.character(unlist(data[4, ]))
data <- data[-(1:4), ]  # Remove metadata rows
colnames(data) <- make.names(colnames(data), unique = TRUE)

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
numeric_columns <- c("Age", "DistanceFromHome", "Education", "MonthlyIncome", 
                     "NumCompaniesWorked", "PercentSalaryHike", 
                     "StandardHours", "StockOptionLevel", "TotalWorkingYears", 
                     "TrainingTimesLastYear", "YearsAtCompany", 
                     "YearsSinceLastPromotion", "YearsWithCurrManager")

data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
  as.numeric(as.character(x))
})

# Step 6: Handle missing values (example: replace NA with median for numeric columns)
data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

# Step 7: Document data types
str(data)

# Step 8: Summary statistics
summary(data)

# Step 9: Save cleaned data
write_csv(data, "C:\\Users\\test\\Desktop\\1MD\\cleaned_hr_data.csv")

# Print final summary
cat("Final dataset has", nrow(data), "rows and", ncol(data), "columns.\n")
