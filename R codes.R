# The folder path
setwd("/Users/sandasmiwijesuriya/Desktop/DV PORTFOLIO")

# Read the datasets
fem_stem_graduates <- read.csv("Female_STEM_Graduates.csv")
gerd <- read.csv("GERD.csv")
adult_literacy <- read.csv("Adult_Literacy.csv")
fem_youth_literacy <- read.csv("Female_Youth_Literacy.csv")
gii <- read.csv("GII.csv")
fem_labour_force <- read.csv("Female_Labour_Force.csv")
fem_researchers <- read.csv("Female_Researchers.csv")

# Summary of each dataset
# Female_STEM_Graduates
head(fem_stem_graduates)
str(fem_stem_graduates)

# GERD
head(gerd)
str(gerd)

# Adult_Literacy
head(adult_literacy)
str(adult_literacy)

# Female_Youth_Literacy
head(fem_youth_literacy)
str(fem_youth_literacy)

# GII
head(gii)
str(gii)

# Female_Labour_Force
head(fem_labour_force)
str(fem_labour_force)

# Female_Researchers
head(fem_researchers)
str(fem_researchers)

# Merging datasets on the keys : Country and Year
merged_data <- merge(fem_stem_graduates, gerd, by = c("Country", "Year"), all = TRUE)

merged_data <- merge(merged_data, adult_literacy, by = c("Country", "Year"), all = TRUE)

merged_data <- merge(merged_data, fem_youth_literacy, by = c("Country", "Year"), all = TRUE)

merged_data <- merge(merged_data, gii, by = c("Country", "Year"), all = TRUE)

merged_data <- merge(merged_data, fem_labour_force, by = c("Country", "Year"), all = TRUE)

merged_data <- merge(merged_data, fem_researchers, by = c("Country", "Year"), all = TRUE)

# Handle missing values
merged_data[is.na(merged_data)] <- 0

# Save the Merged Dataset
write.csv(merged_data, "Merged_Dataset.csv", row.names = FALSE)


### Exploratory Data Analysis 

## Univariate Analysis

# Summary Statistics
summary(merged_data)

# Visualization of Individual Variables
library(ggplot2)

# 1.Distribution of Female STEM Graduates
ggplot(merged_data, aes(x = Female_STEM_Graduates)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = 'Female STEM Graduates (%)',  y = 'Count',
    title = 'Distribution of Female STEM Graduates')
  
# 2.Distribution of GERD
ggplot(merged_data, aes(x = GERD)) +
  geom_histogram(binwidth = 0.2, fill = "lightgreen", color = "black") +
  labs(x = 'GERD as % of GDP', y = 'Count',
    title = 'Distribution of GERD as % of GDP')

## Multivariate Analysis

# Install “corrplot” package
library(corrplot)

# Correlation matrix

# Exclude 'Country' and 'Year' columns for correlation analysis
numeric_data <- merged_data[, c("Female_STEM_Graduates", "GERD", "Adult_Literacy", 
                               "Female_Youth_Literacy", "GII", "Female_Labour_Force", 
                               "Female_Researchers")]

# Handle missing data
colSums(is.na(numeric_data))
valid_data <- numeric_data[, apply(numeric_data, 2, function(x) !all(is.na(x)) & var(x, na.rm = TRUE) > 0)]

# Calculate the correlation matrix
correlation_matrix <- cor(clean_data, use = "complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "white", "red"))(200))


# Scatter Plot for Female STEM Graduates vs. Adult Literacy Rates

ggplot(merged_data, aes(x = Adult_Literacy, y = Female_STEM_Graduates)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs( x = "Adult Literacy Rates (%)", y = "Female STEM Graduates (%)",
    title = "Relationship Between Female STEM Graduates and Adult Literacy Rates")


# Box Plot for GII Across Different Countries
ggplot(merged_data, aes(x = Country, y = GII)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(x = "Country", y = "GII",
    title = "Gender Inequality Index (GII) Across Countries",
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
