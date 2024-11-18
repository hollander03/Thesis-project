## Linearly interpolation of GDP data
library(readr)
library(zoo)
library(writexl)
gdp_data <- read_csv("data/gdp_data.csv")y

# Create a zoo object from the quarterly data
quarterly_gdp <- zoo(gdp_data$gdp, order.by = gdp_data$DATE)

# Create a sequence of monthly dates from the start to the end of your data
monthly_dates <- seq(from = min(gdp_data$DATE), to = max(gdp_data$DATE), by = "month")

# Interpolate the quarterly data to monthly data
monthly_gdp <- na.approx(quarterly_gdp, xout = monthly_dates)

# Convert the interpolated data back to a data frame (optional)
monthly_gdp_df <- data.frame(DATE = index(monthly_gdp), gdp = coredata(monthly_gdp))

write_xlsx(monthly_gdp_df, "data/monthly_gdp_df.xlsx")
