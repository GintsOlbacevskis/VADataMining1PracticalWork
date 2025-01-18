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


# Step 5: Data parsing
# Replace specific values in the 'LeftCompany' column
if ("LeftCompany" %in% colnames(data)) {
  data$LeftCompany <- as.character(data$LeftCompany)  # Ensure it's a character column
  data$LeftCompany[data$LeftCompany == "NO"] <- "No"  # Replace "NO" with "No"
  data$LeftCompany[data$LeftCompany == "Joo"] <- "Yes"  # Replace "Joo" with "Yes"
  data$LeftCompany[data$LeftCompany == "yes"] <- "Yes"  # Replace "yes" with "Yes"
}
# Ensure 'LeftCompany' column contains only 'YES' or 'NO'. Replace other values with NA
data$LeftCompany <- ifelse(data$LeftCompany %in% c("Yes", "No"), data$LeftCompany, NA)


# Replace specific values in the 'TravelFrequency' column
if ("TravelFrequency" %in% colnames(data)) {
  data$TravelFrequency <- as.character(data$TravelFrequency)  # Ensure it's a character column
  data$TravelFrequency[data$TravelFrequency == "Non - Travel"] <- "Non-Travel"  # Replace "Non - Travel" with "Non-Travel"
  data$TravelFrequency[data$TravelFrequency == "Frequently"] <- "Travel_Frequently"  # Replace "Frequently" with "Travel_Frequently"
  data$TravelFrequency[data$TravelFrequency == "Rare"] <- "Travel_Rarely"  # Replace "Non - Travel" with "Travel_Rarely"
}
# Ensure 'TravelFrequency' column contains only allowed values. Replace other values with NA
data$TravelFrequency <- ifelse(data$TravelFrequency %in% c("Non-Travel", "Travel_Frequently", "Travel_Rarely"), data$TravelFrequency, NA)


# Replace specific values in the 'Department' column
if ("Department" %in% colnames(data)) {
  data$Department <- as.character(data$Department)  # Ensure it's a character column
  data$Department[data$Department == "R&D"] <- "Research & Development"  # Replace "R&D" with "Research & Development"
  data$Department[data$Department == "HR"] <- "Human Resources"  # Replace "HR" with "Human Resources"
}
# Ensure 'Department' column contains only allowed values. Replace other values with NA
data$Department <- ifelse(data$Department %in% c("Research & Development", "Human Resources", "Sales"), data$Department, NA)


# Replace specific values in the 'Education' column
if ("Education" %in% colnames(data)) {
  data$Education <- as.character(data$Education)  # Ensure it's a character column
  data$Education[data$Education == "1"] <- "1 'Below College'"  # Replace "1" with "1 'Below College'"
  data$Education[data$Education == "2"] <- "2 'College'"  # Replace "1" with "2 'College'"
  data$Education[data$Education == "3"] <- "3 'Bachelor'"  # Replace "1" with "3 'Bachelor'"
  data$Education[data$Education == "4"] <- "4 'Master'"  # Replace "1" with "4 'Master'"
  data$Education[data$Education == "5"] <- "5 'Doctor'"  # Replace "1" with "1 'Below College'"
  data$Education[data$Education == "College"] <- "2 'College'"  # Replace "1" with "1 'Below College'"
  data$Education[data$Education == "MASTER"] <- "4 'Master'"  # Replace "1" with "1 'Below College'"
}
# Ensure 'Education' column contains only allowed values. Replace other values with NA
data$Education <- ifelse(data$Education %in% c("1 'Below College'", "2 'College'", "3 'Bachelor'", "4 'Master'", "5 'Doctor'"), data$Education, NA)


# Replace specific values in the 'EducationField' column
if ("EducationField" %in% colnames(data)) {
  data$EducationField <- as.character(data$EducationField)  # Ensure it's a character column
  data$EducationField[data$EducationField == "H R"] <- "Human Resources"  # Replace "H R" with "Human Resources"
  data$EducationField[data$EducationField == "MED"] <- "Medical"  # Replace "MED" with "Medical"
  data$EducationField[data$EducationField == "LS"] <- "Life Sciences"  # Replace "LS" with "Life Sciences"
  data$EducationField[data$EducationField == "OTH"] <- "Other"  # Replace "OTH" with "Other"
}
# Ensure 'EducationField' column contains only allowed values. Replace other values with NA
data$EducationField <- ifelse(data$EducationField %in% c("Technical Degree", "Marketing", "Medical", "Life Sciences", "Other"), data$EducationField, NA)


# Replace specific values in the 'Gender' column
if ("Gender" %in% colnames(data)) {
  data$Gender <- as.character(data$Gender)  # Ensure it's a character column
  data$Gender[data$Gender == "Uros"] <- "Male"  # Replace "Uros" with "Male"
}
# Ensure 'Gender' column contains only allowed values. Replace other values with NA
data$Gender <- ifelse(data$Gender %in% c("Male", "Female"), data$Gender, NA)


# Replace specific values in the 'JobRole' column
if ("JobRole" %in% colnames(data)) {
  data$JobRole <- as.character(data$JobRole)  # Ensure it's a character column
  data$JobRole[data$JobRole == "Healthcare Rprs"] <- "Healthcare Representative"  # Replace "Healthcare Rprs" with "Healthcare Representative"
  data$JobRole[data$JobRole == "Laboratory    Technician"] <- "Laboratory Technician"  # Replace "Laboratory    Technician" with "Laboratory Technician"
  data$JobRole[data$JobRole == "Lab Tech"] <- "Laboratory Technician"  # Replace "Lab Tech" with "Laboratory Technician"
  data$JobRole[data$JobRole == "Mngr"] <- "Manager"  # Replace "Mngr" with "Manager"
  data$JobRole[data$JobRole == "Manufacturing Dir."] <- "Manufacturing Director"  # Replace "Manufacturing Dir." with "Manufacturing Director"
  data$JobRole[data$JobRole == "RS"] <- "Research Scientist"  # Replace "RS" with "Research Scientist"
  data$JobRole[data$JobRole == "RD"] <- "Research Director"  # Replace "RD" with "Research Director"
  data$JobRole[data$JobRole == "Sales E"] <- "Sales Executive"  # Replace "Sales E" with "Sales Executive"
  data$JobRole[data$JobRole == "SR"] <- "Sales Representative"  # Replace "SR" with "Sales Representative"
}
# Ensure 'JobRole' column contains only allowed values. Replace other values with NA
data$JobRole <- ifelse(data$JobRole %in% c("Human Resources", "Healthcare Representative", "Laboratory Technician", "Laboratory Technician", "Manager", "Manufacturing Director", "Research Scientist", "Research Director", "Sales Executive", "Sales Representative"), data$JobRole, NA)


# Replace specific values in the 'MaritalStatus' column
if ("MaritalStatus" %in% colnames(data)) {
  data$MaritalStatus <- as.character(data$MaritalStatus)  # Ensure it's a character column
  data$MaritalStatus <- gsub("Single\\s*-\\s*\\d+$", "Single", data$MaritalStatus) # Replace any pattern starting with "Single - [number]" followed by numbers or spaces with "Single"
}
# Ensure 'MaritalStatus' column contains only allowed values. Replace other values with NA
data$MaritalStatus <- ifelse(data$MaritalStatus %in% c("Male", "Female", "Single"), data$MaritalStatus, NA)


# Remove specific values in the 'MonthlyIncome' column
if ("MonthlyIncome" %in% colnames(data)) {
  data$MonthlyIncome <- as.character(data$MonthlyIncome)  # Ensure it's a character column
  data$MonthlyIncome <- gsub("\\?|\\s|INR", "", data$MonthlyIncome) # Use gsub to remove '?', '[space]' and 'INR' while keeping the numbers
  data$MonthlyIncome <- trimws(data$MonthlyIncome) # Trim any extra spaces
}


# Replace specific values in the 'NumCompaniesWorked' column
if ("NumCompaniesWorked" %in% colnames(data)) {
  data$NumCompaniesWorked <- as.character(data$NumCompaniesWorked)  # Ensure it's a character column
  data$NumCompaniesWorked[data$NumCompaniesWorked == "None"] <- "0"  # Contains replace "None" with "0"
  data$NumCompaniesWorked[data$NumCompaniesWorked == "two"] <- "2"  # Contains replace "two" with "2"
  data$NumCompaniesWorked[data$NumCompaniesWorked == "tämä on ensimmäinen"] <- "0"  # Contains replace "0" with "tämä on ensimmäinen"
}


# Replace specific values in the 'Over18' column
if ("Over18" %in% colnames(data)) {
  data$Over18 <- as.character(data$Over18)  # Ensure it's a character column
  data$Over18[data$Over18 == "1"] <- "Y"  # Contains replace "1" with "Y"
}
# Ensure 'Over18' column contains only allowed values. Replace other values with NA
data$Over18 <- ifelse(data$Over18 %in% c("Y", "N"), data$Over18, NA)


# Replace specific values or add % in the 'PercentSalaryHike' column
if ("PercentSalaryHike" %in% colnames(data)) {
  data$PercentSalaryHike <- gsub("pct$", "%", data$PercentSalaryHike) # Replace 'pct' with '%'
  
  # Add '%' if it does not already end with '%'
  data$PercentSalaryHike <- ifelse(grepl("%$", data$PercentSalaryHike), 
                                   data$PercentSalaryHike, 
                                   paste0(data$PercentSalaryHike, "%"))
}



# Remove specific values in the 'StandardHours' column
if ("StandardHours" %in% colnames(data)) {
  data$StandardHours <- as.character(data$StandardHours)  # Ensure it's a character column
  data$StandardHours <- gsub("hrs", "", data$StandardHours) # Use gsub to remove 'hrs' while keeping the numbers
  data$StandardHours <- trimws(data$StandardHours) # Trim any extra spaces
}


# Replace specific values in the 'StockOptionLevel' column
if ("StockOptionLevel" %in% colnames(data)) {
  data$StockOptionLevel <- as.character(data$StockOptionLevel)  # Ensure it's a character column
  data$StockOptionLevel[data$StockOptionLevel == "No"] <- "0"  # Contains replace "No" with "0"
}


# Replace specific values in the 'TotalWorkingYears' column
if ("TotalWorkingYears" %in% colnames(data)) {
  data$TotalWorkingYears <- as.character(data$TotalWorkingYears)  # Ensure it's a character column
  data$TotalWorkingYears[data$TotalWorkingYears == "nolla"] <- "0"  # Contains replace "nolla" with "0"
  data$TotalWorkingYears[data$TotalWorkingYears == "one"] <- "0"  # Contains replace "one" with "0"
}


# Replace specific values in the 'TrainingTimesLastYear' column
if ("TrainingTimesLastYear" %in% colnames(data)) {
  data$TrainingTimesLastYear <- as.character(data$TrainingTimesLastYear)  # Ensure it's a character column
  data$TrainingTimesLastYear[data$TrainingTimesLastYear == "kuusi"] <- "6"  # Contains replace "kuusi" with "6"
}


# Replace specific values in the 'YearsAtCompany' column
if ("YearsAtCompany" %in% colnames(data)) {
  data$YearsAtCompany <- as.character(data$YearsAtCompany)  # Ensure it's a character column
  data$YearsAtCompany[data$YearsAtCompany == "I"] <- "1"  # Contains replace "I" with "1"
  data$YearsAtCompany[data$YearsAtCompany == "II"] <- "2"  # Contains replace "II" with "2"
  data$YearsAtCompany[data$YearsAtCompany == "III"] <- "3"  # Contains replace "III" with "3"
}


# Replace specific values in the 'YearsSinceLastPromotion' column
if ("YearsSinceLastPromotion" %in% colnames(data)) {
  data$YearsSinceLastPromotion <- as.character(data$YearsSinceLastPromotion)  # Ensure it's a character column
  data$YearsSinceLastPromotion[data$YearsSinceLastPromotion == "No"] <- "0"  # Contains replace "No" with "0"
  data$YearsSinceLastPromotion[data$YearsSinceLastPromotion == "x"] <- "10"  # Contains replace "x" with "10"
}


# Remove specific values in the 'YearsWithCurrManager' column
if ("YearsWithCurrManager" %in% colnames(data)) {
  data$YearsWithCurrManager <- as.character(data$YearsWithCurrManager)  # Ensure it's a character column
  data$YearsWithCurrManager <- gsub("yrs|y", "", data$YearsWithCurrManager) # Use gsub to remove 'yrs' and 'y' while keeping the numbers
  data$YearsWithCurrManager <- trimws(data$YearsWithCurrManager) # Trim any extra spaces
}


#Convert columns to appropriate data types
numeric_columns <- c("Age", "DistanceFromHome",
                     "EmployeeCount", "EmployeeID", "JobLevel",
                     "MonthlyIncome", "NumCompaniesWorked", "StandardHours",
                     "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear",
                     "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager")

# Convert columns to numeric, ensuring non-numeric values become NA
data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
  x <- as.character(x)
  as.numeric(ifelse(grepl("^-?\\d+(\\.\\d+)?$", x), x, NA))  # Strict numeric validation
})

# Step 6: Document data types
str(data)

# Step 7: Summary statistics
summary(data)

# Step 8: Save cleaned data
write_csv(data, "C:\\Users\\test\\Desktop\\1MD\\cleaned_hr_data.csv") #Must change on Yours

# Print final summary
cat("Final dataset has", nrow(data), "rows and", ncol(data), "columns.\n")