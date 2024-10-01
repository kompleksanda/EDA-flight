## ----setup, echo=FALSE, warning=FALSE, message=FALSE------------------------------------
install.packages("formatR")
library(formatR)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  tidy.opts=list(width.cutoff=60),
  tidy=TRUE,
  results='asis'
)


## ---- cache=TRUE------------------------------------------------------------------------
install.packages(c("readr","curl", "dplyr", "ggplot2","tidyverse", "summarytools","lubridate","rmarkdown","pillar","tidyr","kableExtra"))


library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
library(rmarkdown)
library(dplyr)
library(summarytools)
library(tidyr)
library(kableExtra)
library(pillar)


## ---- cache=TRUE------------------------------------------------------------------------
flight_dataset <- read.csv("flights.csv")


## ---------------------------------------------------------------------------------------
glimpse(flight_dataset)


## ----cache=TRUE-------------------------------------------------------------------------
colnames(flight_dataset) <- c("x", "Flight_Year", "MONTH_MON", "Flight_Month", "Flight_Date", "APT_ICAO", "Airport_Name", "Country_Name", "Number_of_IFR_Departures_1", "Number_of_IFR_Arrivals_1", "Total_IFR_Movements_1", "Number_of_IFR_Departures_2", "Number_of_IFR_Arrivals_2", "Total_IFR_Movements_2", "Pivot_Label")


## ---------------------------------------------------------------------------------------
drops <- c("x","APT_ICAO", "Pivot_Label")
flight_dataset <- flight_dataset[ , !(names(flight_dataset) %in% drops)]


## ---------------------------------------------------------------------------------------
#The number of rows contained in the dataset dropping variables not needed is
nrow(flight_dataset)
#The number of columns contained in the dataset dropping variables not needed is
ncol(flight_dataset)


## ---------------------------------------------------------------------------------------
kable(sapply(flight_dataset, class), "markdown") %>%
  kable_styling("striped", full_width = F)


## ---------------------------------------------------------------------------------------
kable(head(flight_dataset, n=10), "markdown") %>%
  kable_styling("striped", full_width = F)


## ---------------------------------------------------------------------------------------
missing_values <- flight_dataset %>% summarise_all(funs(sum(is.na(.))))
kable(missing_values, "markdown") %>%
  kable_styling("striped", full_width = F)


## ---------------------------------------------------------------------------------------
flight_dataset %>%
  select(Number_of_IFR_Departures_1, Number_of_IFR_Arrivals_1, Total_IFR_Movements_1, Number_of_IFR_Departures_2, Number_of_IFR_Arrivals_2, Total_IFR_Movements_2) %>%
  dfSummary(graph.col = FALSE)


## ----numerical-column-distribution, fig.cap="The distributions of the numerical column"----
flight_dataset %>%
  ggplot(aes(x = Number_of_IFR_Departures_1)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Number of IFR Departures", x = "IFR Departures", y = "Frequency")

flight_dataset %>%
  ggplot(aes(x = Number_of_IFR_Arrivals_1)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Number of IFR Arrivals", x = "IFR Arrivals", y = "Frequency")

flight_dataset %>%
  ggplot(aes(x = Total_IFR_Movements_1)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Number of Total IFR Flights", x = "Total IFR Flights", y = "Frequency")


## ---------------------------------------------------------------------------------------
flight_dataset %>%
  select(Airport_Name, Country_Name) %>%
  dfSummary()


## ----busiest-airports, fig.cap="The top 10 busiest airports"----------------------------
flight_dataset %>%
  group_by(Airport_Name) %>%
  summarize(Total_Number_of_Flight = sum(Total_IFR_Movements_1)) %>%
  top_n(10, Total_Number_of_Flight) %>%
  ggplot(aes(x = reorder(Airport_Name, Total_Number_of_Flight), y = Total_Number_of_Flight, fill = Airport_Name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Busiest Airports", x = "Airport Name", y = "Number of Flights")


## ----flight-year-plot, fig.cap="The total flights across the years"---------------------
c_dat <- flight_dataset %>% 
  group_by(Flight_Year) %>% 
  summarise(DEPARTURES = sum(Number_of_IFR_Departures_1),
            ARRIVALS = sum(Number_of_IFR_Arrivals_1)) %>% 
  arrange(Flight_Year)

c_dat2 <- c_dat %>% gather(key = "FLIGHT_TYPE", value = "FLIGHTS", -Flight_Year)
arr_depart_chart <- ggplot(c_dat2, aes(x = factor(Flight_Year), y = FLIGHTS, fill = FLIGHT_TYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Flights", fill = "Legend") +
  theme_minimal()

c_dat2 <- c_dat %>% mutate(PERCENT_INCREASE = (DEPARTURES + ARRIVALS - lag(DEPARTURES + ARRIVALS))/lag(DEPARTURES + ARRIVALS) * 100)

bar_chart <- ggplot(c_dat2, aes(x = Flight_Year, y = DEPARTURES + ARRIVALS)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Year", y = "Sum of Departures and Arrivals", title = "Total Flights by Percentage")
bar_chart <- bar_chart +
  geom_text(aes(label = sprintf("%.1f%%", PERCENT_INCREASE)), 
            vjust = -0.5, size = 3.5, color = "red", na.rm = TRUE) +
  scale_y_continuous(labels = scales::comma)
arr_depart_chart
bar_chart


## ----heat-map, fig.cap="The heatmap of flight across months"----------------------------
# Correct month order
month_order <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

data_filtered <- flight_dataset %>%
  filter(Flight_Year >= 2019 & Flight_Year <= 2021)

# Convert Flight_Month to a factor with the correct order
data_filtered$Flight_Month <- factor(data_filtered$Flight_Month, levels = month_order)


data_agg <- data_filtered %>%
  group_by(Flight_Year, Flight_Month) %>%
  summarize(
    total_departures = sum(Number_of_IFR_Departures_1),
    total_arrivals = sum(Number_of_IFR_Arrivals_1),
    total_flights = total_departures + total_arrivals)

ggplot(data_agg, aes(x = Flight_Month, y = Flight_Year, fill = total_flights)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap of Flights in Europe (2019-2021)",
    x = "Month",
    y = "Year",
    fill = "Total Flights")


## ----line-graph, fig.cap="The line graph of the trends across month for all the years"----
ggplot(data_agg, aes(x = Flight_Month, y = total_flights, group = Flight_Year, color = factor(Flight_Year))) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  labs(
    title = "Number of Flights in Europe (2019-2021)",
    x = "Month",
    y = "Total Flights")


## ----box-plot, fig.cap="The box plot of the same chart (Outliers removed)"--------------

flight_data_long <- data_filtered %>%
  pivot_longer(cols = Total_IFR_Movements_1, names_to = "Flight_Total_Pivot", values_to = "Value")


# Calculate quartiles and IQR
Q1 <- quantile(flight_data_long$Value, 0.25)
Q3 <- quantile(flight_data_long$Value, 0.75)
IQR <- Q3 - Q1

# Determine lower and upper limits
lower_limit <- Q1 - 0.25 * IQR
upper_limit <- Q3 + 4 * IQR

ggplot(flight_data_long, aes(x = factor(Flight_Month), y = Value, fill = factor(Flight_Year))) +
  geom_boxplot(outlier.shape = NA, varwidth = TRUE) +
  coord_cartesian(ylim = c(lower_limit,upper_limit)) +
  labs(title = "Boxplot of Flights between January to December (2019-2021)",
       x = "Month",
       y = "Value",
       fill = "Year")




## ----flights-per-day, fig.cap="The average flight per day for August 2021 in the UK"----
# Filter the dataset for August 2021
data_august <- flight_dataset %>%
  filter(Flight_Year == 2021, Flight_Month == "AUG", Country_Name == "United Kingdom")

# Calculate the total flights (departures+arrivals) for each day for each airport
daily_flights <- data_august %>%
  group_by(Airport_Name, Flight_Date) %>%
  summarize(total_flights = sum(Number_of_IFR_Departures_1) + sum(Number_of_IFR_Arrivals_1), .groups = 'drop')

# Calculate the average flights per day for each airport
avg_daily_flights <- daily_flights %>%
  group_by(Airport_Name) %>%
  summarize(avg_flights_per_day = mean(total_flights), .groups = 'drop')

# Create a bar graph visualization using ggplot2
ggplot(avg_daily_flights, aes(x = reorder(Airport_Name, avg_flights_per_day), y = avg_flights_per_day)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Average Flights per Day in August 2021 (United Kingdom)",
       x = "Airport",
       y = "Average Flights per Day")


## ----scatter-plot, fig.cap="The scatter plot to show relationship between data recorded by network manager and airport operator"----
data_clean <- flight_dataset %>% filter(!is.na(Total_IFR_Movements_2))

ggplot(data_clean, aes(x = Total_IFR_Movements_1, y = Total_IFR_Movements_2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Correlation between Network Manager and Airport Operator Flights Records",
       x = "Number of Flights Recorded by Network Manager",
       y = "Number of Flights Recorded by Airport Operator") +
  theme_minimal()

