# -------------------------------------------
# Time Series Forecasting of UK Gas Consumption
# Coursework 1
# Joshua Amoako
# -------------------------------------------


# Load required packages
library(prophet)
library(zoo)


# -------------------------------------------
# Prepare the Data
# -------------------------------------------
# UKgas is a built-in time series object.
# We need to transform it into a dataframe for Prophet.
gas_data <- data.frame(
    ds = as.Date(as.yearmon(time(UKgas))), # Convert 'Time' to Date format
    y = as.numeric(UKgas)                  # Convert values to numeric
)


# 3. Linear Trend Model (The "Baseline")
# Create a simple index to represent time steps
time_index <- 1:nrow(gas_data)
trend_model <- lm(y ~ time_index, data = gas_data)

# Print summary to console to check p-values and coefficients
print(summary(trend_model))

# 4. Prophet Model (The "Advanced Forecast")
# Initialize the model and fit it to the gas data
m <- prophet(gas_data)

# Create a future dataframe for the next 5 years (20 quarters)
future <- make_future_dataframe(m, periods = 20, freq = "quarter")

# Generate the forecast
forecast <- predict(m, future)

# 5. Quick Verification Plots
# These will show up as "Plots" pane in RStudio
plot(gas_data$ds, gas_data$y, type="l", main="Raw Data Check")
abline(trend_model, col="red") # Check the linear fit

plot(m, forecast) # Check the Prophet forecast
prophet_plot_components(m, forecast) # Check the seasonal breakdown


