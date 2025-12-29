The R codes that are used for time series double exponential smoothing are as follows:

\begin{verbatim}
library(TSA)
library(tseries)
library(lmtest) 
library(forecast)
library(ggplot2)
library(FinTS) 

# attach data file
bitcoin<- attach(Bitcoin_price)


#Checking for obvious errors
which(is.na( bitcoin))

bitcoin$Price<- 
  as.numeric(as.character(gsub(",","",bitcoin$Price)))

#Converting the data set into time series object
Bitcoin_price_ts<- ts(as.vector(bitcoin$Price), 
                      start=c(2018,9,15),frequency = 365)
plot.ts(Bitcoin_price_ts,
        main="Time series plot of Bitcoin Prices")


# Decompose the data 
decomposed_data <- decompose(Bitcoin_price_ts)
plot(decomposed_data)

#check seasonality
isSeasonal(Bitcoin_price_ts,freq = 365)

# Perform Mann-Kendall test(check trend)
library(trend)
mk_test <- mk.test(Bitcoin_price_ts)

# Display the result from mk test
print(mk_test)

# Apply Holt's method (double exponential smoothing)
DE_model <- holt(Bitcoin_price_ts,h=30)
DE_model
summary(DE_model)

#fit  the model
fitteddes <- fitted(DE_model)

observed_data <- data.frame(Time = as.numeric(time(tsObj))
                            , Values = as.numeric(tsObj))
fitted_data <- data.frame(Time = as.numeric(time(tsObj)), 
                          Values = as.numeric(fitteddes))

#plot the orginal  data with fitted  value
ggplot() +
  geom_line(data = observed_data, 
            aes(x = Time, y = Values, 
                color = "Observed"), 
            linewidth = 1) +
  geom_line(data = fitted_data, 
            aes(x = Time, y = Values,
                color = "Fitted"), linewidth = 1, 
            linetype = "dashed") +
  scale_color_manual(name = "Series", 
                     values = c("Observed" = "blue", 
                                "Fitted" = "red")) +
  ggtitle("Bitcoin Data Vs Double Exponential Smoothing Curve") +
  xlab("Time") +
  ylab("AvgClose") +
  theme_minimal()

#check AIC,BIC,MSE and MAPE
aic_des <- AIC(DE_model$model)  
bic_des <- BIC(DE_model$model)  
mse_des <- mean((DE_model$residuals)^2,
                na.rm = TRUE)
mape_des <- mean(abs((tsObj - fitteddes) / tsObj),
                 na.rm = TRUE) * 100

des_comparoson <- data.frame(
  Metric = c("AIC", "BIC", "MSE", "MAPE"),
  Value = c(aic_des, bic_des, mse_des, mape_des)
)

print(des_comparoson)

#forecast  the data
forecast_data <-forecast(DE_model,h=40)
autoplot(DE_model)


#plot foretasted value
library(seastests)
par(mfrow=c(1,1))
isSeasonal(tsObj,freq =365)
plot(DE_model, 
     main = "Time Series with Double Exponential Smoothing", 
     xlab = "Time", ylab = "Values", 
     col = "blue", lwd=2)

# Add Fitted Values from Holt-Winters Model
lines(DE_model$fitted, col = "red", 
      lwd = 2, lty=2)

# Add Legend
legend("topleft", 
       legend = c("Original Data", "Fitted (Holt-Winters)"),
       col = c("blue", "red"),
       lty = c(1,2),lwd=2)


\end{verbatim}

\subsubsection{Appendix B}

\begin{verbatim}
# Load necessary libraries
library(dplyr)
library(tidyr)
library(entropy)  # For entropy calculations
library(zoo)      # For rolling calculations
library(ggplot2)
library(plotly)   # For interactive plots

# Replace 'Bitcoin_full_data' with your 
actual data frame or file path.
data <- Bitcoin_full_data  # Ensure this is loaded in your R environment

# Convert Date column to Date type and arrange by date
data <- data %>% 
  mutate(Date = as.Date(Date)) %>% 
  arrange(Date)

# Calculate daily log returns
data <- data %>% 
  mutate(Log_Returns = log(Price / lag(Price)))

# Calculate rolling volatility (e.g., 30-day rolling standard deviation)
data <- data %>% 
  mutate(Volatility = zoo::rollapply(Log_Returns, width = 30, 
FUN = sd, fill = NA, align = "right"))

# Calculate Shannon entropy for daily log returns
data <- data %>% 
  mutate(Entropy = sapply(seq_along(Log_Returns), 
function(i) {
    if (i < 30) return(NA)
    subset_returns <- Log_Returns[(i - 29):i]
    subset_returns <- subset_returns[!is.na(subset_returns)]
    freq <- table(cut(subset_returns, breaks = 10))
    entropy::entropy(freq)
  }))

# Normalize entropy (relative to max entropy)
max_entropy <- log(10)  # Max entropy for 10 bins
data <- data %>% 
  mutate(Normalized_Entropy = Entropy / max_entropy)

# Calculate Value at Risk (VaR) at 95% confidence level
confidence_level <- 0.95
z_score <- qnorm(1 - confidence_level)
data <- data %>% 
  mutate(VaR = -z_score * Volatility)

# Remove rows with NA values
data <- data %>% drop_na()

# Save the results to a CSV file
write.csv(data, "bitcoin_full_analysis.csv", row.names = FALSE)

# ----- Separate Plots -----
# Plot 1: Daily Log Returns
plot1 <- ggplot(data, aes(x = Date, y = Log_Returns)) +
  geom_line(color = "black") +
  labs(title = "Daily Log Returns", x = "Date",
 y = "Log Returns") +
  theme_minimal()

# Plot 2: Rolling Volatility
plot2 <- ggplot(data, aes(x = Date, y = Volatility)) +
  geom_line(color = "red") +
  labs(title = "Rolling Volatility (30-day)",
 x = "Date", y = "Volatility") +
  theme_minimal()

# Plot 3: Normalized Shannon Entropy
plot3 <- ggplot(data, aes(x = Date, y = Normalized_Entropy)) +
  geom_line(color = "green") +
  labs(title = "Normalized Shannon Entropy",
 x = "Date", y = "Normalized Entropy") +
  theme_minimal()

# Plot: Value at Risk (VaR)
var_plot <- ggplot(data, aes(x = Date, y = VaR)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Value at Risk (VaR) at 95% Confidence Level",
    x = "Date",
    y = "VaR"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, 
size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

# ----- Interactive Plots with Plotly -----
# Interactive Log Returns Plot
plotly::ggplotly(plot1)

# Interactive Rolling Volatility Plot
plotly::ggplotly(plot2)

# Interactive Normalized Entropy Plot
plotly::ggplotly(plot3)

# Render the plot interactively using Plotly
interactive_var_plot <- plotly::ggplotly(var_plot)

# Print the interactive plot
interactive_var_plot




# Combined Plot: Volatility and VaR
volatility_var_plot <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Volatility,
 color = "Volatility"), size = 1) +
  geom_line(aes(y = VaR, color = "VaR"), size = 1) +
  labs(
    title = "Volatility and Value at Risk (VaR)",
    x = "Date",
    y = "Metrics"
  ) +
  scale_color_manual(
    values = c("blue", "red"),
    name = "Metrics",
    labels = c("Volatility", "VaR")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Render the combined plot interactively using Plotly
plotly::ggplotly(volatility_var_plot)


# Combined plot with entropy,volatility,VaR
combined_plot <- ggplot(data, aes(x = Date)) +
  # Volatility
  geom_line(aes(y = Volatility,
 color = "Volatility"), size = 1) +
  # Normalized Entropy
  geom_line(aes(y = Normalized_Entropy, 
color = "Normalized Entropy"), size = 1) +
  # VaR
  geom_line(aes(y = VaR,
 color = "VaR"), size = 1) +
  labs(
    title = "Combined Metrics for Bitcoin (Full Dataset)",
    x = "Date",
    y = "Metrics (Raw Values)"
  ) +
  scale_color_manual(
    values = c("green", "blue", "red"),
    name = "Metrics",
    labels = c("Volatility", "Normalized Entropy", "VaR")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  # Set y-axis breaks for the primary y-axis
  scale_y_continuous(
    name = "Metrics",
    breaks = seq(0, 1, by = 0.25),  # Custom breaks at 0, 0.25, 0.5, 0.75, 1
    sec.axis = sec_axis(~ ., 
name = "Metrics (Alternative Scale)")
  )

# Render the combined plot interactively
interactive_combined_plot <- plotly::ggplotly(combined_plot)

# Print the interactive combined plot
interactive_combined_plot

