# -------------------------------------------------------------------------
# Load required packages
# -------------------------------------------------------------------------
library(dplyr)
library(openxlsx)
library(ggplot2)
library(lubridate)

# -------------------------------------------------------------------------
# Function to read and filter GTD data
# -------------------------------------------------------------------------
#' Read the Global Terrorism Database (GTD) file, filter by country, 
#' and return a cleaned data frame with proper date columns.
#' 
#' @param filepath Path to the GTD Excel file
#' @param sheet Sheet name or index in the Excel file
#' @param country Name of the country to filter
#' 
#' @return A data frame with columns "date" and "nkill"
read_gtd_data <- function(filepath, sheet, country) {
  
  # Basic error check
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  df <- read.xlsx(filepath, sheet = sheet) %>%
    # Keep only necessary columns
    select(country_txt, iyear, imonth, iday, nkill) %>%
    # Filter for the selected country
    filter(country_txt == country) %>%
    # Remove the 'country_txt' column
    select(-country_txt) %>%
    # Create a proper Date column
    mutate(
      date = as.Date(paste(iyear, imonth, iday, sep = "-"), format = "%Y-%m-%d")
    ) %>%
    # Keep only the date column and deaths, remove rows with missing values
    select(date, nkill) %>%
    na.omit()
  
  return(df)
}

# -------------------------------------------------------------------------
# Read and summarize daily data
# -------------------------------------------------------------------------
df <- read_gtd_data(
  filepath = "../../datasets/GlobalTerrorismDatabase/globalterrorismdb_0522dist.xlsx",
  sheet    = "Data",
  country  = "Israel"
)

# Summarize the total number of deaths for each day
df <- df %>%
  group_by(date) %>%
  summarise(nkill = sum(nkill, na.rm = TRUE)) %>%
  arrange(date) %>%
  filter(nkill > 0)

# -------------------------------------------------------------------------
# Summarize monthly counts
# -------------------------------------------------------------------------
df_month <- df %>%
  # Convert each date to the first day of its month
  mutate(month = floor_date(date, unit = "month")) %>%
  # Group by this monthly date
  group_by(month) %>%
  summarise(month_nkill = sum(nkill, na.rm = TRUE)) %>%
  arrange(month)

# Quick preview
head(df_month)

# -------------------------------------------------------------------------
# Plot monthly data
# -------------------------------------------------------------------------
# Store the maximum of monthly_nkill for repeated use
max_count_month <- max(df_month$month_nkill, na.rm = TRUE)

p_month <- ggplot(df_month, aes(x = month, y = month_nkill)) +  
  geom_point(color = "gray", na.rm = TRUE) +
  geom_line(color = "black", na.rm = TRUE) +
   
  # Labels and styling
  labs(
    title    = "Deaths From Terrorist Attacks per Month (Second Intifada)",
    subtitle = "Country: Israel",
    x        = "Month",
    y        = "Number of Deaths",
    caption  = "Source: Global Terrorism Database (GTD). University of Maryland"
  ) +
  ylim(0, max_count_month) +
  xlim(as.Date("2000-09-28"), as.Date("2005-02-08")) +
  theme_minimal()

print(p_month)

df %>% filter(date >= as.Date("2000-09-28"), date <= as.Date("2005-02-08")) %>% pull("nkill") %>% sum


