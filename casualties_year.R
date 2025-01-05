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
#' @export
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
# Summarize yearly counts
# -------------------------------------------------------------------------
df_year <- df %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  group_by(year) %>%
  summarise(year_nkill = sum(nkill, na.rm = TRUE)) %>%
  arrange(year)

# Quick preview
head(df_year)

# -------------------------------------------------------------------------
# Plot yearly data
# -------------------------------------------------------------------------
max_count <- max(df_year$year_nkill, na.rm = TRUE)

p <- ggplot(df_year, aes(x = year, y = year_nkill)) +
  # Highlight specific date ranges with shaded rectangles
  geom_rect(
    data = data.frame(
      xmin = as.Date("1987-12-09"),
      xmax = as.Date("1993-09-13"),
      ymin = 0,
      ymax = max_count
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "green", alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_rect(
    data = data.frame(
      xmin = as.Date("2000-09-28"),
      xmax = as.Date("2005-02-08"),
      ymin = 0,
      ymax = max_count
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "orange", alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_point(color = "gray", na.rm = TRUE) +
  geom_line(color = "black", na.rm = TRUE) +
  
  # Add vertical lines for the same date ranges
  geom_vline(xintercept = as.Date("1987-12-09"), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.Date("1993-09-13"), linetype = "dashed", color = "green") +
  geom_vline(xintercept = as.Date("2000-09-28"), linetype = "dashed", color = "orange") +
  geom_vline(xintercept = as.Date("2005-02-08"), linetype = "dashed", color = "orange") +
  
  # Labels and styling
  labs(
    title    = "Deaths From Terrorist Attacks per Year",
    subtitle = "Country: Israel",
    x        = "Year",
    y        = "Number of Deaths",
    caption  = "Source: Global Terrorism Database (GTD). University of Maryland"
  ) +
  ylim(0, max_count) +
  theme_minimal()

print(p)

