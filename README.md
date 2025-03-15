# Applied-Data-Science-Project-2

This Shiny app enables users to upload and load datasets in multiple formats (.csv, .xlsx, .json, .rds) dynamically. It also offers the option to load built-in sample datasets (mtcars or iris) and displays the selected dataset in a reactive data table (DT) for easy exploration.

Once a dataset is uploaded, users can perform the following preprocessing functionalities:
- Standardizing Column Names: Converts column names to lowercase and replaces spaces with underscores to maintain consistency.
- Removing Duplicates: Detects and removes duplicate rows to avoid redundancy.
- Handling Missing Values (Mean & Median): Users can opt for either mean or median imputation, replacing missing numeric values with their respective statistical measures.
- Text Cleaning: Options include trimming whitespace, converting text to lowercase, and removing special characters to ensure uniform formatting.
- Date Standardization: Converts date columns into a consistent YYYY-MM-DD format and allows for the extraction of features such as year, month, and day.
- Outlier Handling (Z-Score & Mean): Users can either remove outliers using a Z-score threshold (greater than 3) or replace them with the mean value.
- Fixing Inconsistent Categorical Labels: Standardizes text-based categorical values to ensure consistency.
- Dropping High-Missing Columns: Removes columns where a significant portion of values are missing, ensuring a cleaner dataset.

Further data preprocessing steps available to the user include scaling numerical data (either standardizing with z-scores or normalizing using min-max scaling), encoding categorical features through one-hot encoding, and handling outliers. Outlier handling options include capping extreme values at set Z-score thresholds, replacing outliers with the mean, or removing them based on the interquartile range (IQR) method.

Polynomial: Users can raise a selected numeric column to a specified power (e.g., squaring, cubing) to capture non-linear relationships. 

Interaction: Users can multiplies two numeric columns, creating a new column that may reveal interaction effects between variables.

Datetime: Users can select a date or date-like column, and if necessary, the app will attempt to parse it into a valid Date format (YYYY-MM-DD). Once successfully parsed, columns for year, month, and day are automatically generated.

Save:  Users can download the fully processed dataset as a CSV file.

EDA: In the eda part, users can view data previews, data information, categorical variable analysis, and numerical variable analysis in the display area below. And also can draw multiple graphs，visually display data characteristics and multivariate relationships.Specific chart types include scatter plots, histograms, box plots, time series plots and so on. 
