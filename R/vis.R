# install.packages("tidyverse")
# library(tidyverse)

# setwd("C:/Users/Karan/repos/CryptoMetricsOfficial/R")

kyber_data_time_cpu <- read.csv("C:/Users/Karan/repos/CryptoMetricsOfficial/R/cleaned_data/kyber_benchmark_20250214_114904.csv")
ecdh_data_time_cpu <- read.csv("C:/Users/Karan/repos/CryptoMetricsOfficial/R/cleaned_data/ecdh_benchmark_20250214_114846.csv")
kyber_data_ram <- read.csv("C:/Users/Karan/repos/CryptoMetricsOfficial/R/cleaned_data/kyber_benchmark_ram_20250214_114926.csv")
ecdh_data_ram <- read.csv("C:/Users/Karan/repos/CryptoMetricsOfficial/R/cleaned_data/ecdh_benchmark_ram_20250214_114915.csv")

# Disable science notation
options(scipen = 999)  # Prevents scientific notation globally

# Check unique values in iteration_no before merging
length(unique(kyber_data_time_cpu$iteration_no)) == length(unique(kyber_data_ram$iteration_no))
length(unique(ecdh_data_time_cpu$iteration_no)) == length(unique(ecdh_data_ram$iteration_no))


# Check Data Structure
str(kyber_data_time_cpu)
str(ecdh_data_time_cpu)
str(kyber_data_ram)
str(ecdh_data_ram)


#start and end timestamps for Kyber CPU dataset conversion
kyber_data_time_cpu$start_timestamp <- as.POSIXct(kyber_data_time_cpu$start_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")
kyber_data_time_cpu$end_timestamp <- as.POSIXct(kyber_data_time_cpu$end_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")

# start and end timestamps for ECDH CPU dataset conversion
ecdh_data_time_cpu$start_timestamp <- as.POSIXct(ecdh_data_time_cpu$start_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")
ecdh_data_time_cpu$end_timestamp <- as.POSIXct(ecdh_data_time_cpu$end_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")

#  start and end timestamps for Kyber RAM  conversion
kyber_data_ram$start_timestamp <- as.POSIXct(kyber_data_ram$start_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")
kyber_data_ram$end_timestamp <- as.POSIXct(kyber_data_ram$end_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")

# start and end timestamps for ECDH RAM conversion
ecdh_data_ram$start_timestamp <- as.POSIXct(ecdh_data_ram$start_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")
ecdh_data_ram$end_timestamp <- as.POSIXct(ecdh_data_ram$end_timestamp / 1e9, origin = "1970-01-01", tz = "UTC")

#timestamp validation
kyber_data_time_cpu$duration_calculated <- as.numeric(difftime(kyber_data_time_cpu$end_timestamp, kyber_data_time_cpu$start_timestamp, units = "secs"))
summary(kyber_data_time_cpu$duration_calculated)
summary(kyber_data_time_cpu$time_ns / 1e9)


# Summary Stats
summary(kyber_data_time_cpu)
summary(ecdh_data_time_cpu)
summary(kyber_data_ram)
summary(ecdh_data_ram)

# sUMMARY stats - VIZ
skim(kyber_data_time_cpu)
skim(ecdh_data_time_cpu)
skim(kyber_data_ram)
skim(ecdh_data_ram)

# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)

median_table <- summary_table %>%
  select(Algorithm, Median_Time_ns, Median_Heap_Memory_kb, Median_Peak_Memory_kb, Median_CPU_Usage, Median_cpu_percent)

kable(median_table, caption = "Comparison of Median Values for Kyber and ECDH") %>%
  kable_styling()

cor(kyber_data_time_cpu$time_ns, kyber_data_time_cpu$cpu_percent)
cor(ecdh_data_time_cpu$time_ns, ecdh_data_time_cpu$cpu_percent)

ggplot(kyber_data_time_cpu, aes(x = time_ns, y = cpu_percent)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Kyber: Execution Time vs CPU Usage",
       x = "Execution Time (ns)",
       y = "CPU Usage (%)") +
  theme_minimal()




median_table <- summary_table %>%
  select(Algorithm, Median_Time_ns, Median_Heap_Memory_kb, Median_Peak_Memory_kb, Median_CPU_PERCENT)

kable(median_table, caption = "Comparison of Median Values for Kyber and ECDH") %>%
  kable_styling()

# Compute summary statistics
compute_summary <- function(df, algorithm) {
  df %>%
    summarise(
      Algorithm = algorithm,
      Mean_Time_ns = mean(time_ns, na.rm = TRUE),
      Median_Time_ns = median(time_ns, na.rm = TRUE),
      StdDev_Time_ns = sd(time_ns, na.rm = TRUE),
      IQR_Time_ns = IQR(time_ns, na.rm = TRUE),
      
      Mean_Heap_Memory_kb = mean(heap_memory_kb, na.rm = TRUE),
      Median_Heap_Memory_kb = median(heap_memory_kb, na.rm = TRUE),
      StdDev_Heap_Memory_kb = sd(heap_memory_kb, na.rm = TRUE),
      IQR_Heap_Memory_kb = IQR(heap_memory_kb, na.rm = TRUE),
      
      Mean_Peak_Memory_kb = mean(peak_rss_kb, na.rm = TRUE),
      Median_Peak_Memory_kb = median(peak_rss_kb, na.rm = TRUE),
      StdDev_Peak_Memory_kb = sd(peak_rss_kb, na.rm = TRUE),
      IQR_Peak_Memory_kb = IQR(peak_rss_kb, na.rm = TRUE)
    )
}

# Apply function to Kyber & ECDH datasets
kyber_summary <- compute_summary(kyber_data_time_cpu %>% 
                                   left_join(kyber_data_ram, by=c("algorithm_name", "iteration_no")), "Kyber")

ecdh_summary <- compute_summary(ecdh_data_time_cpu %>% 
                                  left_join(ecdh_data_ram, by=c("algorithm_name", "iteration_no")), "ECDH")

# Combine results into one table
summary_table <- bind_rows(kyber_summary, ecdh_summary)

# Display table in a formatted way
kable(summary_table, caption = "Summary Statistics for Kyber and ECDH") %>%
  kable_styling()


library(ggplot2)
ggplot(combined_data, aes(x = Algorithm, y = time_ns, fill = Algorithm)) +
  geom_boxplot() +
  scale_y_log10() +  # Apply log transformation
  labs(title = "Execution Time Comparison: Kyber vs ECDH (Log Scale)", 
       x = "Algorithm", y = "Execution Time (log ns)") +
  theme_minimal()


library(ggplot2)
library(dplyr)

# Combine CPU usage data from both algorithms
kyber_data_time_cpu$Algorithm <- "Kyber"
ecdh_data_time_cpu$Algorithm <- "ECDH"

combined_cpu_data <- bind_rows(
  kyber_data_time_cpu %>% select(cpu_percent, Algorithm),
  ecdh_data_time_cpu %>% select(cpu_percent, Algorithm)
)

# Boxplot for CPU Usage
ggplot(combined_cpu_data, aes(x = Algorithm, y = cpu_percent, fill = Algorithm)) +
  geom_boxplot() +
  scale_y_log10() +  # Apply log transformation
  labs(title = "CPU Usage Comparison: Kyber vs ECDH (Log Scale)", 
       x = "Algorithm", y = "CPU Usage (log scale)") +
  theme_minimal()


# Combine heap memory data from both algorithms
kyber_data_ram$Algorithm <- "Kyber"
ecdh_data_ram$Algorithm <- "ECDH"

combined_ram_data <- bind_rows(
  kyber_data_ram %>% select(heap_memory_kb, Algorithm),
  ecdh_data_ram %>% select(heap_memory_kb, Algorithm)
)

# Boxplot for Heap Memory Usage
ggplot(combined_ram_data, aes(x = Algorithm, y = heap_memory_kb, fill = Algorithm)) +
  geom_boxplot() +
  scale_y_log10() +  # Apply log transformation
  labs(title = "Heap Memory Usage Comparison: Kyber vs ECDH (Log Scale)", 
       x = "Algorithm", y = "Heap Memory (KB, log scale)") +
  theme_minimal()







ggplot() +
  geom_density(data = kyber_data_time_cpu, aes(x = time_ns, fill = "Kyber"), alpha = 0.5) +
  geom_density(data = ecdh_data_time_cpu, aes(x = time_ns, fill = "ECDH"), alpha = 0.5) +
  labs(title = "Density Plot of Execution Time (Log Scale)", 
       x = "Execution Time (log ns)", 
       y = "Density") +
  scale_x_log10() +  # Log transformation to compress large values
  scale_fill_manual(values = c("Kyber" = "blue", "ECDH" = "red")) +
  theme_minimal()


library(ggplot2)
library(dplyr)

# Combine CPU usage data from both algorithms
kyber_data_time_cpu$Algorithm <- "Kyber"
ecdh_data_time_cpu$Algorithm <- "ECDH"

combined_cpu_data <- bind_rows(
  kyber_data_time_cpu %>% select(cpu_percent, Algorithm),
  ecdh_data_time_cpu %>% select(cpu_percent, Algorithm)
)

library(ggplot2)
library(dplyr)

# Ensure no zero or negative values before applying log10
combined_cpu_data <- combined_cpu_data %>%
  filter(cpu_percent > 0)  # Remove zero or negative values

# Density plot with log scale applied
ggplot(combined_cpu_data, aes(x = cpu_percent, fill = Algorithm)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +  # Log transformation applied safely
  labs(title = "Density Plot of CPU Usage (Log Scale)", 
       x = "CPU Usage (log %)", 
       y = "Density") +
  scale_fill_manual(values = c("Kyber" = "blue", "ECDH" = "red")) +
  theme_minimal()



median_table <- summary_table %>%
  select(Algorithm, Median_Time_ns, Median_Heap_Memory_kb, Median_Peak_Memory_kb)

kable(median_table, caption = "Comparison of Median Values for Kyber and ECDH") %>%
  kable_styling()


############################################################

# SHAPIRO TEST

shapiro_results <- list(
  "Kyber Time (ns)" = shapiro.test(kyber_data_time_cpu$time_ns),
  "Kyber CPU (%)" = shapiro.test(kyber_data_time_cpu$cpu_percent),
  "Kyber Heap Memory (KB)" = shapiro.test(kyber_data_ram$heap_memory_kb),

  "ECDH Time (ns)" = shapiro.test(ecdh_data_time_cpu$time_ns),
  "ECDH CPU (%)" = shapiro.test(ecdh_data_time_cpu$cpu_percent),
  "ECDH Heap Memory (KB)" = shapiro.test(ecdh_data_ram$heap_memory_kb)
)

shapiro_results

# Manually inserting known W-Statistic and P-Values instead of computing them
shapiro_results_df <- data.frame(
  Metric = c("Kyber Time (ns)", "Kyber CPU (%)", "Kyber Heap Memory (KB)",
             "ECDH Time (ns)", "ECDH CPU (%)", "ECDH Heap Memory (KB)"),
  W_Statistic = c(0.038166, 0.11331, 0.0046888, 0.45261, 0.45362, 0.03889),
  P_Value = c("0.00000000000000022", 
              "0.00000000000000022", 
              "0.00000000000000022", 
              "0.00000000000000022", 
              "0.00000000000000022", 
              "0.00000000000000022") # Manually entered values
)

# Use kable for a cleaner table display
library(kableExtra)
kable(shapiro_results_df, caption = "Shapiro-Wilk Normality Test Results") %>%
  kable_styling(full_width = FALSE)


# Load necessary libraries
install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

# Create histograms for Execution Time, CPU Usage, and Heap Memory
hist_time <- ggplot() +
  geom_histogram(aes(x = kyber_data_time_cpu$time_ns), bins = 50, fill = "blue", alpha = 0.5) +
  geom_histogram(aes(x = ecdh_data_time_cpu$time_ns), bins = 50, fill = "red", alpha = 0.5) +
  labs(title = "Histogram of Execution Time", x = "Execution Time (ns)", y = "Count") +
  theme_minimal()

hist_cpu <- ggplot() +
  geom_histogram(aes(x = kyber_data_time_cpu$cpu_percent), bins = 50, fill = "blue", alpha = 0.5) +
  geom_histogram(aes(x = ecdh_data_time_cpu$cpu_percent), bins = 50, fill = "red", alpha = 0.5) +
  labs(title = "Histogram of CPU Usage", x = "CPU Usage (%)", y = "Count") +
  theme_minimal()

hist_memory <- ggplot() +
  geom_histogram(aes(x = kyber_data_ram$heap_memory_kb), bins = 50, fill = "blue", alpha = 0.5) +
  geom_histogram(aes(x = ecdh_data_ram$heap_memory_kb), bins = 50, fill = "red", alpha = 0.5) +
  labs(title = "Histogram of Heap Memory Usage", x = "Heap Memory (KB)", y = "Count") +
  theme_minimal()

# Create QQ-Plots for Execution Time, CPU Usage, and Heap Memory
qq_time <- ggplot() +
  stat_qq(aes(sample = kyber_data_time_cpu$time_ns), color = "blue") +
  stat_qq(aes(sample = ecdh_data_time_cpu$time_ns), color = "red") +
  labs(title = "QQ-Plot of Execution Time") +
  theme_minimal()

qq_cpu <- ggplot() +
  stat_qq(aes(sample = kyber_data_time_cpu$cpu_percent), color = "blue") +
  stat_qq(aes(sample = ecdh_data_time_cpu$cpu_percent), color = "red") +
  labs(title = "QQ-Plot of CPU Usage") +
  theme_minimal()

qq_memory <- ggplot() +
  stat_qq(aes(sample = kyber_data_ram$heap_memory_kb), color = "blue") +
  stat_qq(aes(sample = ecdh_data_ram$heap_memory_kb), color = "red") +
  labs(title = "QQ-Plot of Heap Memory Usage") +
  theme_minimal()

# Arrange plots in a grid layout
grid.arrange(hist_time, qq_time, hist_cpu, qq_cpu, hist_memory, qq_memory, ncol = 2)

# MANN Whitney U Test = CPU Usage and Execution Time###################################################################

# Wilcoxon Rank-Sum Test for Execution Time
wilcox_time <- wilcox.test(kyber_data_time_cpu$time_ns, ecdh_data_time_cpu$time_ns, 
                           alternative = "two.sided", exact = FALSE)
print(wilcox_time)


# Wilcoxon Rank-Sum Test for CPU Usage
wilcox_cpu <- wilcox.test(kyber_data_time_cpu$cpu_percent, ecdh_data_time_cpu$cpu_percent, 
                          alternative = "two.sided", exact = FALSE)
print(wilcox_cpu)

# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(ggpubr)

# Wilcoxon Rank-Sum Test (Mann-Whitney U Test) for Execution Time
wilcox_time <- wilcox.test(kyber_data_time_cpu$time_ns, ecdh_data_time_cpu$time_ns, 
                           alternative = "two.sided", exact = FALSE)

# Wilcoxon Rank-Sum Test (Mann-Whitney U Test) for CPU Usage
wilcox_cpu <- wilcox.test(kyber_data_time_cpu$cpu_percent, ecdh_data_time_cpu$cpu_percent, 
                          alternative = "two.sided", exact = FALSE)

# Convert results to a data frame
results <- data.frame(
  Test = c("Execution Time", "CPU Usage"),
  W_Statistic = c(wilcox_time$statistic, wilcox_cpu$statistic),
  P_Value = c(wilcox_time$p.value, wilcox_cpu$p.value),
  Alternative_Hypothesis = c(wilcox_time$alternative, wilcox_cpu$alternative)
)

# Convert the table into a plot
table_plot <- ggtexttable(results, rows = NULL, theme = ttheme("minimal"))

# Add title to the plot
annotated_plot <- annotate_figure(table_plot, 
                                  top = text_grob("Execution Time Comparison between Kyber and ECDH", 
                                                  face = "bold", size = 14))

# Display the table as a plot
ggarrange(annotated_plot, ncol = 1, nrow = 1)




# Based on the above, cpu usage and execution time signficiant impact kyber and ecdh

# Execution Time Boxplot
ggplot() +
  geom_boxplot(aes(x = "Kyber", y = kyber_data_time_cpu$time_ns), fill = "red") +
  geom_boxplot(aes(x = "ECDH", y = ecdh_data_time_cpu$time_ns), fill = "blue") +
  labs(title = "Execution Time Comparison", x = "Algorithm", y = "Time (ns)")

# CPU Usage Boxplot
ggplot() +
  geom_boxplot(aes(x = "Kyber", y = kyber_data_time_cpu$cpu_percent), fill = "red") +
  geom_boxplot(aes(x = "ECDH", y = ecdh_data_time_cpu$cpu_percent), fill = "blue") +
  labs(title = "CPU Usage Comparison", x = "Algorithm", y = "CPU Percent")


median(kyber_data_time_cpu$time_ns)
median(ecdh_data_time_cpu$time_ns)
median(kyber_data_time_cpu$cpu_percent)
median(ecdh_data_time_cpu$cpu_percent)


library(dplyr)

summary_table <- data.frame(
  Metric = c("Execution Time (ns)", "CPU Usage (%)"),
  Kyber_Median = c(median(kyber_data_time_cpu$time_ns), median(kyber_data_time_cpu$cpu_percent)),
  ECDH_Median = c(median(ecdh_data_time_cpu$time_ns), median(ecdh_data_time_cpu$cpu_percent))
)

print(summary_table)


# Kruskal Wallis- time and cryptooperation - kyber
kruskal_time <- kruskal.test(time_ns ~ crypto_operation, data = kyber_data_time_cpu)
print(kruskal_time)

kruskal_cpu <- kruskal.test(cpu_percent ~ crypto_operation, data = kyber_data_time_cpu)
kruskal_heap <- kruskal.test(heap_memory_kb ~ crypto_operation, data = kyber_data_ram)

# Kruskal-Wallis Test for Execution Time
kruskal_time <- kruskal.test(time_ns ~ crypto_operation, data = kyber_data_time_cpu)
print(kruskal_time)

# Kruskal-Wallis Test for CPU Usage
kruskal_cpu <- kruskal.test(cpu_percent ~ crypto_operation, data = kyber_data_time_cpu)
print(kruskal_cpu)

# Kruskal-Wallis Test for Heap Memory Usage
kruskal_heap <- kruskal.test(heap_memory_kb ~ crypto_operation, data = kyber_data_ram)
print(kruskal_heap)


# Kruskal Wallis- time and cryptooperation - ecdh
kruskal_time <- kruskal.test(time_ns ~ crypto_operation, data = ecdh_data_time_cpu)
print(kruskal_time)

kruskal_cpu <- kruskal.test(cpu_percent ~ crypto_operation, data = ecdh_data_time_cpu)
kruskal_heap <- kruskal.test(heap_memory_kb ~ crypto_operation, data = ecdh_data_ram)

# Kruskal-Wallis Test for Execution Time
kruskal_time <- kruskal.test(time_ns ~ crypto_operation, data = ecdh_data_time_cpu)
print(kruskal_time)

# Kruskal-Wallis Test for CPU Usage
kruskal_cpu <- kruskal.test(cpu_percent ~ crypto_operation, data = ecdh_data_time_cpu)
print(kruskal_cpu)

# Kruskal-Wallis Test for Heap Memory Usage
kruskal_heap <- kruskal.test(heap_memory_kb ~ crypto_operation, data = ecdh_data_ram)
print(kruskal_heap)

# Load required libraries
library(ggplot2)
library(ggpubr)

# Create a data frame for the Kruskal-Wallis test results
kruskal_results <- data.frame(
  Algorithm = c("Kyber", "Kyber", "Kyber", "ECDH", "ECDH", "ECDH"),
  Metric = c("Execution Time", "CPU Usage", "Heap Memory Usage",
             "Execution Time", "CPU Usage", "Heap Memory Usage"),
  Chi_Squared = c(1593.3, 1504.9, 2.0, 1442.9, 1459.5, 1998.0),
  df = c(2, 2, 2, 1, 1, 1),
  p_value = c("< 0.00000000000000022", "< 0.00000000000000022", "0.3679",
              "< 0.00000000000000022", "< 0.00000000000000022", "< 0.00000000000000022")
)

# Convert the table into a plot
table_plot <- ggtexttable(kruskal_results, rows = NULL, theme = ttheme("minimal"))

# Display the table as a plot
ggarrange(table_plot, ncol = 1, nrow = 1)



# Install necessary package (if not installed)
install.packages("FSA")
library(FSA)

# Dunn’s Test for Execution Time (Kyber)
dunn_time_kyber <- dunnTest(time_ns ~ crypto_operation, data = kyber_data_time_cpu, method = "bonferroni")
print(dunn_time_kyber)

# Dunn’s Test for Execution Time (ECDH)
dunn_time_ecdh <- dunnTest(time_ns ~ crypto_operation, data = ecdh_data_time_cpu, method = "bonferroni")
print(dunn_time_ecdh)

# Dunn’s Test for CPU Usage (Kyber)
dunn_cpu_kyber <- dunnTest(cpu_percent ~ crypto_operation, data = kyber_data_time_cpu, method = "bonferroni")
print(dunn_cpu_kyber)

# Dunn’s Test for CPU Usage (ECDH)
dunn_cpu_ecdh <- dunnTest(cpu_percent ~ crypto_operation, data = ecdh_data_time_cpu, method = "bonferroni")
print(dunn_cpu_ecdh)

# Dunn’s Test for Heap Memory (Only for ECDH, since Kyber was not significant)
dunn_heap_ecdh <- dunnTest(heap_memory_kb ~ crypto_operation, data = ecdh_data_ram, method = "bonferroni")
print(dunn_heap_ecdh)






# Load required libraries
library(ggplot2)
library(ggpubr)

# Create a data frame for the Dunn's Test results
dunn_results <- data.frame(
  Algorithm = c("Kyber", "Kyber", "Kyber", "ECDH", "ECDH",
                "Kyber", "Kyber", "Kyber", "ECDH", "ECDH",
                "ECDH"),
  Metric = c("Execution Time", "Execution Time", "Execution Time",
             "Execution Time", "Execution Time",
             "CPU Usage", "CPU Usage", "CPU Usage",
             "CPU Usage", "CPU Usage",
             "Heap Memory Usage"),
  Comparison = c("Decapsulation - Encapsulation", "Decapsulation - Key Generation", "Encapsulation - Key Generation",
                 "Key Exchange - Key Generation", "Key Exchange - Key Generation",
                 "Decapsulation - Encapsulation", "Decapsulation - Key Generation", "Encapsulation - Key Generation",
                 "Key Exchange - Key Generation", "Key Exchange - Key Generation",
                 "Key Exchange - Key Generation"),
  Z_Score = c(-39.48740, -24.79865, 14.68875,
              37.98571, 37.98571,
              -38.45284, -23.66199, 14.79085,
              38.20373, 38.20373,
              44.69902),
  P_Value_Adjusted = c("< 0.00000000000000022", "< 0.00000000000000022", "< 0.000000000000002283",
                       "0", "0",
                       "< 0.00000000000000022", "< 0.00000000000000022", "< 0.000000000000000503",
                       "0", "0",
                       "0")
)

# Convert the table into a plot
table_plot <- ggtexttable(dunn_results, rows = NULL, theme = ttheme("minimal"))

# Display the table as a plot
ggarrange(table_plot, ncol = 1, nrow = 1)






# Load required libraries
library(ggplot2)
library(dplyr)
library(ggpubr)  # For statistical annotations

# Combine datasets
combined_data <- bind_rows(kyber_data_time_cpu, ecdh_data_time_cpu)

# Ensure no zero values before applying log scale
combined_data <- combined_data %>%
  mutate(time_ns = time_ns + 1)

# Kruskal-Wallis Test for Execution Time
kruskal_time <- kruskal.test(time_ns ~ crypto_operation, data = kyber_data_time_cpu)

# Create Violin Plot with Kruskal-Wallis annotation
ggplot(combined_data, aes(x = Algorithm, y = time_ns, fill = Algorithm)) +
  geom_violin(alpha = 0.7, trim = TRUE) +  
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  
  facet_wrap(~crypto_operation, scales = "free_y") +  
  scale_y_log10() +  
  labs(title = "Execution Time Distribution per Cryptographic Operation (Violin Plot)",
       subtitle = paste("Kruskal-Wallis Test: p =", format(kruskal_time$p.value, digits = 3)),  # Adds p-value to the plot
       x = "Algorithm",
       y = "Execution Time (ns, log scale)",
       fill = "Algorithm") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"))









#Speakrmans Rank Correlation Test for CPU Usage and exeuction time 
## Kyber
spearman_kyber <- cor.test(kyber_data_time_cpu$time_ns, kyber_data_time_cpu$cpu_percent, method = "spearman")
print(spearman_kyber)
##ecdh
spearman_ecdh <- cor.test(ecdh_data_time_cpu$time_ns, ecdh_data_time_cpu$cpu_percent, method = "spearman")
print(spearman_ecdh)


# Load required libraries
library(ggplot2)
library(ggpubr)

# Create a data frame for Spearman correlation results
spearman_results <- data.frame(
  Algorithm = c("Kyber", "ECDH"),
  Metric_1 = c("Execution Time (ns)", "Execution Time (ns)"),
  Metric_2 = c("CPU Usage (%)", "CPU Usage (%)"),
  Correlation_Coefficient = c(0.9614664, 0.9867239),
  P_Value = c("< 0.00000000000000022", "< 0.00000000000000022")
)

# Convert the table into a plot
table_plot <- ggtexttable(spearman_results, rows = NULL, theme = ttheme("minimal"))

# Display the table as a plot
ggarrange(table_plot, ncol = 1, nrow = 1)


# Boxplots generation for ######################################################################
# Load required libraries
library(ggplot2)
library(dplyr)

# Combine both datasets
combined_data <- bind_rows(kyber_data_time_cpu, ecdh_data_time_cpu)

# Create a boxplot for execution time distribution per algorithm
ggplot(combined_data, aes(x = Algorithm, y = time_ns, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  facet_wrap(~crypto_operation, scales = "free_y") + # Separate by cryptographic operation
  scale_y_log10() +  # Use log scale for better visualization
  labs(title = "Execution Time Distribution for Cryptographic Operations per Algorithm",
       x = "Algorithm",
       y = "Execution Time (ns)",
       fill = "Algorithm") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"))


library(ggplot2)
library(dplyr)

# Combine datasets
combined_data <- bind_rows(kyber_data_time_cpu, ecdh_data_time_cpu)

# Add 1 ns to avoid log(0) issues
combined_data <- combined_data %>%
  mutate(time_ns = time_ns + 1)

# Violin Plot
ggplot(combined_data, aes(x = Algorithm, y = time_ns, fill = Algorithm)) +
  geom_violin(alpha = 0.7, trim = TRUE) + 
  facet_wrap(~crypto_operation, scales = "free_y") + 
  scale_y_log10() +  # Log scale
  labs(title = "Execution Time Distribution per Cryptographic Operation (Violin Plot)",
       x = "Algorithm",
       y = "Execution Time (ns, log scale)",
       fill = "Algorithm") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"))















# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Combine execution time data
time_data <- bind_rows(
  kyber_data_time_cpu %>% select(time_ns) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(time_ns) %>% mutate(Algorithm = "ECDH")
)

# Convert nanoseconds to milliseconds for better visualization
time_data$time_ms <- time_data$time_ns / 1e6

# Boxplot for execution time
ggplot(time_data, aes(x = Algorithm, y = time_ms, fill = Algorithm)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  labs(title = "Execution Time Comparison", x = "Algorithm", y = "Time (ms)") +
  theme_minimal()

# Combine CPU usage data
cpu_data <- bind_rows(
  kyber_data_time_cpu %>% select(cpu_percent) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(cpu_percent) %>% mutate(Algorithm = "ECDH")
)

# Boxplot for CPU usage
ggplot(cpu_data, aes(x = Algorithm, y = cpu_percent, fill = Algorithm)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  labs(title = "CPU Usage Comparison", x = "Algorithm", y = "CPU Percent") +
  theme_minimal()

# Combine Heap Memory data
heap_memory_data <- bind_rows(
  kyber_data_ram %>% select(heap_memory_kb) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_ram %>% select(heap_memory_kb) %>% mutate(Algorithm = "ECDH")
)

# Boxplot for Heap Memory Consumption
ggplot(heap_memory_data, aes(x = Algorithm, y = heap_memory_kb, fill = Algorithm)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  labs(title = "Heap Memory Consumption", x = "Algorithm", y = "Heap Memory (KB)") +
  theme_minimal()

# Combine Peak RSS Memory data
peak_rss_data <- bind_rows(
  kyber_data_ram %>% select(peak_rss_kb) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_ram %>% select(peak_rss_kb) %>% mutate(Algorithm = "ECDH")
)

# Boxplot for Peak RSS Memory Consumption
ggplot(peak_rss_data, aes(x = Algorithm, y = peak_rss_kb, fill = Algorithm)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  labs(title = "Peak RSS Memory Consumption", x = "Algorithm", y = "Peak RSS (KB)") +
  theme_minimal()


# Density Plot

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Prepare Execution Time Data (Convert to milliseconds for better visualization)
time_data <- bind_rows(
  kyber_data_time_cpu %>% select(time_ns) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(time_ns) %>% mutate(Algorithm = "ECDH")
)

time_data$time_ms <- time_data$time_ns / 1e6  # Convert nanoseconds to milliseconds

# Density Plot for Execution Time
ggplot(time_data, aes(x = time_ms, fill = Algorithm)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot: Execution Time Distribution",
       x = "Execution Time (ms)", y = "Density") +
  theme_minimal()

# Prepare CPU Usage Data
cpu_data <- bind_rows(
  kyber_data_time_cpu %>% select(cpu_percent) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(cpu_percent) %>% mutate(Algorithm = "ECDH")
)

# Density Plot for CPU Usage
ggplot(cpu_data, aes(x = cpu_percent, fill = Algorithm)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot: CPU Usage Distribution",
       x = "CPU Percent", y = "Density") +
  theme_minimal()


# Combined line Chart
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Combine Execution Time Data
time_data <- bind_rows(
  kyber_data_time_cpu %>% select(iteration_no, time_ns) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(iteration_no, time_ns) %>% mutate(Algorithm = "ECDH")
)

time_data$time_ms <- time_data$time_ns / 1e6  # Convert nanoseconds to milliseconds

# Combine CPU Usage Data
cpu_data <- bind_rows(
  kyber_data_time_cpu %>% select(iteration_no, cpu_percent) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(iteration_no, cpu_percent) %>% mutate(Algorithm = "ECDH")
)

# Combine Memory Data (Heap + Peak RSS)
memory_data <- bind_rows(
  kyber_data_ram %>% select(iteration_no, heap_memory_kb, peak_rss_kb) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_ram %>% select(iteration_no, heap_memory_kb, peak_rss_kb) %>% mutate(Algorithm = "ECDH")
)

# Transform Data to Long Format for Faceted Plot
time_long <- time_data %>% pivot_longer(cols = time_ms, names_to = "Metric", values_to = "Value")
cpu_long <- cpu_data %>% pivot_longer(cols = cpu_percent, names_to = "Metric", values_to = "Value")
memory_long <- memory_data %>% pivot_longer(cols = c(heap_memory_kb, peak_rss_kb), names_to = "Metric", values_to = "Value")

# Merge all datasets
combined_data <- bind_rows(time_long, cpu_long, memory_long)

# Plot Combined Line Chart
ggplot(combined_data, aes(x = iteration_no, y = Value, color = Algorithm, group = interaction(Algorithm, Metric))) +
  geom_line(size = 1, alpha = 0.8) +
  facet_wrap(~Metric, scales = "free_y") +  # Separate panels for each metric
  labs(title = "Performance Metrics Over Iterations",
       x = "Iteration Number",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "top")

##Bar Chart for Execution Time and Crypto Operation
# Load required libraries
library(ggplot2)
library(dplyr)

# Convert time from ns to ms
kyber_data_time_cpu$time_ms <- kyber_data_time_cpu$time_ns / 1e6
ecdh_data_time_cpu$time_ms <- ecdh_data_time_cpu$time_ns / 1e6

# Combine datasets
crypto_time_data <- bind_rows(
  kyber_data_time_cpu %>% select(crypto_operation, time_ms) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(crypto_operation, time_ms) %>% mutate(Algorithm = "ECDH")
)

# Execution Time by Crypto Operation (Bar Chart)
ggplot(crypto_time_data, aes(x = crypto_operation, y = time_ms, fill = Algorithm)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), width = 0.7) +
  labs(title = "Average Execution Time by Crypto Operation",
       x = "Cryptographic Operation",
       y = "Execution Time (ms)",
       fill = "Algorithm") +
  theme_minimal()

# Key Size vs Execution Time (Bar Chart)
key_time_data <- bind_rows(
  kyber_data_time_cpu %>% select(key_size_bytes, time_ms) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(key_size_bytes, time_ms) %>% mutate(Algorithm = "ECDH")
)

ggplot(key_time_data, aes(x = factor(key_size_bytes), y = time_ms, fill = Algorithm)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), width = 0.7) +
  labs(title = "Average Execution Time by Key Size",
       x = "Key Size (Bytes)",
       y = "Execution Time (ms)",
       fill = "Algorithm") +
  theme_minimal()

# Scatter Plot Generation 

# Load required libraries
library(ggplot2)
library(dplyr)

# Convert time from ns to ms for better visualization
kyber_data_time_cpu$time_ms <- kyber_data_time_cpu$time_ns / 1e6
ecdh_data_time_cpu$time_ms <- ecdh_data_time_cpu$time_ns / 1e6

# Combine datasets for plotting
scatter_time_cpu <- bind_rows(
  kyber_data_time_cpu %>% select(time_ms, cpu_percent) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(time_ms, cpu_percent) %>% mutate(Algorithm = "ECDH")
)

# Execution Time vs CPU Usage (Scatter Plot)
ggplot(scatter_time_cpu, aes(x = cpu_percent, y = time_ms, color = Algorithm)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  labs(title = "Execution Time vs CPU Usage",
       x = "CPU Usage (%)",
       y = "Execution Time (ms)",
       color = "Algorithm") +
  theme_minimal()

# Combine datasets for Key Size vs Execution Time
scatter_key_time <- bind_rows(
  kyber_data_time_cpu %>% select(key_size_bytes, time_ms) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(key_size_bytes, time_ms) %>% mutate(Algorithm = "ECDH")
)

# Key Size vs Execution Time (Scatter Plot)
ggplot(scatter_key_time, aes(x = key_size_bytes, y = time_ms, color = Algorithm)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  labs(title = "Key Size vs Execution Time",
       x = "Key Size (Bytes)",
       y = "Execution Time (ms)",
       color = "Algorithm") +
  theme_minimal()

# Correlation matrix for Execution Time, CPU usage, and memory usage

# Load required libraries
library(ggcorrplot)
library(corrplot)
library(dplyr)

# Convert time from ns to ms for better readability
kyber_data_time_cpu$time_ms <- kyber_data_time_cpu$time_ns / 1e6
ecdh_data_time_cpu$time_ms <- ecdh_data_time_cpu$time_ns / 1e6

# Ensure there's a common key (iteration_no) for merging
kyber_corr_data <- kyber_data_time_cpu %>%
  select(iteration_no, time_ms, cpu_percent) %>%
  left_join(kyber_data_ram %>% select(iteration_no, heap_memory_kb, peak_rss_kb), by = "iteration_no")

ecdh_corr_data <- ecdh_data_time_cpu %>%
  select(iteration_no, time_ms, cpu_percent) %>%
  left_join(ecdh_data_ram %>% select(iteration_no, heap_memory_kb, peak_rss_kb), by = "iteration_no")

# Add Algorithm Labels
kyber_corr_data$Algorithm <- "Kyber"
ecdh_corr_data$Algorithm <- "ECDH"

# Combine Kyber and ECDH into one dataset
correlation_data <- bind_rows(kyber_corr_data, ecdh_corr_data)

# Remove NA values
correlation_data <- correlation_data %>% na.omit()

# Compute the Correlation Matrix using Spearman's Rank Correlation
correlation_matrix <- cor(correlation_data %>% select(time_ms, cpu_percent, heap_memory_kb, peak_rss_kb), method = "spearman")

# Visualize the Correlation Matrix
ggcorrplot(correlation_matrix, 
           method = "square", 
           type = "lower",
           lab = TRUE,
           title = "Correlation Matrix (Execution Time, CPU Usage, Memory)")


# Load required libraries
library(ggcorrplot)
library(dplyr)
library(gridExtra)  # For arranging plots side by side

# Ensure 'iteration_no' exists in both datasets and merge correctly
kyber_corr_data <- kyber_data_time_cpu %>%
  select(iteration_no, time_ns, cpu_percent) %>%
  left_join(kyber_data_ram %>% select(iteration_no, heap_memory_kb, peak_rss_kb), by = "iteration_no")

ecdh_corr_data <- ecdh_data_time_cpu %>%
  select(iteration_no, time_ns, cpu_percent) %>%
  left_join(ecdh_data_ram %>% select(iteration_no, heap_memory_kb, peak_rss_kb), by = "iteration_no")

# Replace NA values in 'peak_rss_kb' with the median to avoid missing correlations
kyber_corr_data$peak_rss_kb[is.na(kyber_corr_data$peak_rss_kb)] <- median(kyber_corr_data$peak_rss_kb, na.rm = TRUE)
ecdh_corr_data$peak_rss_kb[is.na(ecdh_corr_data$peak_rss_kb)] <- median(ecdh_corr_data$peak_rss_kb, na.rm = TRUE)

# Remove remaining NA values
kyber_corr_data <- kyber_corr_data %>% drop_na(time_ns, cpu_percent, heap_memory_kb, peak_rss_kb)
ecdh_corr_data <- ecdh_corr_data %>% drop_na(time_ns, cpu_percent, heap_memory_kb, peak_rss_kb)

# Compute Spearman Correlation Matrices for Kyber and ECDH
kyber_correlation_matrix <- cor(kyber_corr_data %>% select(iteration_no, time_ns, cpu_percent, heap_memory_kb, peak_rss_kb), 
                                method = "spearman")

ecdh_correlation_matrix <- cor(ecdh_corr_data %>% select(iteration_no, time_ns, cpu_percent, heap_memory_kb, peak_rss_kb), 
                               method = "spearman")

# Create Correlation Matrix Plots for Kyber and ECDH
kyber_plot <- ggcorrplot(kyber_correlation_matrix, 
                         method = "square", 
                         type = "lower",
                         lab = TRUE,
                         title = "Kyber Correlation Matrix",
                         colors = c("blue", "white", "red"))

ecdh_plot <- ggcorrplot(ecdh_correlation_matrix, 
                        method = "square", 
                        type = "lower",
                        lab = TRUE,
                        title = "ECDH Correlation Matrix",
                        colors = c("blue", "white", "red"))

# Display both correlation matrices side by side
grid.arrange(kyber_plot, ecdh_plot, ncol = 2)

# R CODE FOR LINE CHARTS ##########################################################################

# Load required libraries
library(ggplot2)
library(dplyr)

# Ensure merging of iteration_no with RAM data
kyber_data <- kyber_data_time_cpu %>%
  select(iteration_no, time_ns, cpu_percent) %>%
  left_join(kyber_data_ram %>% select(iteration_no, heap_memory_kb), by = "iteration_no")

ecdh_data <- ecdh_data_time_cpu %>%
  select(iteration_no, time_ns, cpu_percent) %>%
  left_join(ecdh_data_ram %>% select(iteration_no, heap_memory_kb), by = "iteration_no")

# Add Algorithm Labels
kyber_data$Algorithm <- "Kyber"
ecdh_data$Algorithm <- "ECDH"

# Combine the datasets
combined_data <- bind_rows(kyber_data, ecdh_data)

# Execution Time over Iterations
execution_time_plot <- ggplot(combined_data, aes(x = iteration_no, y = time_ns, color = Algorithm)) +
  geom_line(alpha = 0.7) +
  labs(title = "Execution Time Over Iterations", x = "Iteration Number", y = "Execution Time (ns)") +
  theme_minimal()

# CPU Usage over Iterations
cpu_usage_plot <- ggplot(combined_data, aes(x = iteration_no, y = cpu_percent, color = Algorithm)) +
  geom_line(alpha = 0.7) +
  labs(title = "CPU Usage Over Iterations", x = "Iteration Number", y = "CPU Usage (%)") +
  theme_minimal()

# Heap Memory over Iterations
heap_memory_plot <- ggplot(combined_data, aes(x = iteration_no, y = heap_memory_kb, color = Algorithm)) +
  geom_line(alpha = 0.7) +
  labs(title = "Heap Memory Over Iterations", x = "Iteration Number", y = "Heap Memory (KB)") +
  theme_minimal()

# Display the plots
library(gridExtra)
grid.arrange(execution_time_plot, cpu_usage_plot, heap_memory_plot, ncol = 1)




# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Merge Kyber and ECDH data
kyber_data <- kyber_data_time_cpu %>%
  select(iteration_no, time_ns, cpu_percent) %>%
  left_join(kyber_data_ram %>% select(iteration_no, heap_memory_kb), by = "iteration_no")

ecdh_data <- ecdh_data_time_cpu %>%
  select(iteration_no, time_ns, cpu_percent) %>%
  left_join(ecdh_data_ram %>% select(iteration_no, heap_memory_kb), by = "iteration_no")

# Add Algorithm Labels
kyber_data$Algorithm <- "Kyber"
ecdh_data$Algorithm <- "ECDH"

# Combine datasets
combined_data <- bind_rows(kyber_data, ecdh_data)

# Execution Time over Iterations with Smoother Trendline
execution_time_plot <- ggplot(combined_data, aes(x = iteration_no, y = time_ns, color = Algorithm)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +  # Trendline
  scale_y_continuous(limits = c(0, quantile(combined_data$time_ns, 0.99))) +  # Remove extreme outliers
  scale_x_continuous(breaks = seq(0, 1000, 100)) +  # Better X-axis spacing
  facet_wrap(~Algorithm) +  # Separate Kyber & ECDH
  labs(title = "Execution Time Over Iterations", x = "Iteration Number", y = "Execution Time (ns)") +
  theme_minimal()

# CPU Usage over Iterations with Scaling
cpu_usage_plot <- ggplot(combined_data, aes(x = iteration_no, y = cpu_percent, color = Algorithm)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +
  scale_y_continuous(limits = c(0, quantile(combined_data$cpu_percent, 0.99))) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  facet_wrap(~Algorithm) +
  labs(title = "CPU Usage Over Iterations", x = "Iteration Number", y = "CPU Usage (%)") +
  theme_minimal()

# Heap Memory over Iterations with Scaling
heap_memory_plot <- ggplot(combined_data, aes(x = iteration_no, y = heap_memory_kb, color = Algorithm)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +
  scale_y_continuous(limits = c(0, quantile(combined_data$heap_memory_kb, 0.99))) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  facet_wrap(~Algorithm) +
  labs(title = "Heap Memory Over Iterations", x = "Iteration Number", y = "Heap Memory (KB)") +
  theme_minimal()

# Display plots side by side
grid.arrange(execution_time_plot, cpu_usage_plot, heap_memory_plot, ncol = 1)





# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Merge Kyber and ECDH data, including key_size_bytes
kyber_data <- kyber_data_time_cpu %>%
  select(iteration_no, time_ns, key_size_bytes) %>%
  mutate(Algorithm = "Kyber")

ecdh_data <- ecdh_data_time_cpu %>%
  select(iteration_no, time_ns, key_size_bytes) %>%
  mutate(Algorithm = "ECDH")

# Combine both datasets
combined_data <- bind_rows(kyber_data, ecdh_data)

# ✅ Bar Chart: Average Execution Time per Key Size
bar_chart <- ggplot(combined_data, aes(x = factor(key_size_bytes), y = time_ns, fill = Algorithm)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +  # Bar chart using mean execution time
  labs(title = "Bar Chart: Key Size vs. Execution Time", x = "Key Size (Bytes)", y = "Average Execution Time (ns)") +
  theme_minimal()

# ✅ Improved Scatter Plot: Key Size vs. Execution Time
scatter_plot <- ggplot(combined_data, aes(x = key_size_bytes, y = time_ns, color = Algorithm)) +
  geom_jitter(alpha = 0.4, width = 10) +  # Add jitter to prevent overlapping points
  geom_smooth(method = "loess", se = FALSE, span = 0.2) +  # Add trendline
  scale_y_log10() +  # Use log scale for better visibility
  labs(title = "Scatter Plot: Key Size vs. Execution Time", x = "Key Size (Bytes)", y = "Execution Time (ns, log scale)") +
  theme_minimal()

# ✅ Display both plots side by side
grid.arrange(bar_chart, scatter_plot, ncol = 2)



library(ggplot2)
library(dplyr)

# Convert time from ns to ms
kyber_data_time_cpu$time_ms <- kyber_data_time_cpu$time_ns / 1e6
ecdh_data_time_cpu$time_ms <- ecdh_data_time_cpu$time_ns / 1e6

# Fix key sizes: Use shared_secret_key_size_bytes for Key Exchange
ecdh_data_time_cpu <- ecdh_data_time_cpu %>%
  mutate(corrected_key_size = ifelse(crypto_operation == "Key Exchange", 
                                     shared_secret_key_size_bytes, key_size_bytes))

kyber_data_time_cpu <- kyber_data_time_cpu %>%
  mutate(corrected_key_size = ifelse(crypto_operation %in% c("Encapsulation", "Decapsulation"), 
                                     shared_secret_key_size_bytes, key_size_bytes))

# Combine datasets with corrected key size
key_time_data <- bind_rows(
  kyber_data_time_cpu %>% select(corrected_key_size, time_ms) %>% mutate(Algorithm = "Kyber"),
  ecdh_data_time_cpu %>% select(corrected_key_size, time_ms) %>% mutate(Algorithm = "ECDH")
)

# Create the updated bar chart
ggplot(key_time_data, aes(x = factor(corrected_key_size), y = time_ms, fill = Algorithm)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), width = 0.7) +
  labs(title = "Average Execution Time by Key / Shared Secret Size",
       x = "Key Size (Bytes) / Shared Secret Size (Bytes)",
       y = "Execution Time (ms)",
       fill = "Algorithm") +
  theme_minimal()



#########################################################################################################


library(ggplot2)
library(dplyr)
library(gridExtra)

ecdh_data_time_cpu <- ecdh_data_time_cpu %>%
  mutate(corrected_key_size = ifelse(crypto_operation == "Key Exchange", 
                                     shared_secret_key_size_bytes, key_size_bytes))

kyber_data_time_cpu <- kyber_data_time_cpu %>%
  mutate(corrected_key_size = ifelse(crypto_operation %in% c("Encapsulation", "Decapsulation"), 
                                     shared_secret_key_size_bytes, key_size_bytes))

kyber_data <- kyber_data_time_cpu %>%
  select(iteration_no, time_ns, corrected_key_size) %>%
  rename(key_size_bytes = corrected_key_size) %>%
  mutate(Algorithm = "Kyber")

ecdh_data <- ecdh_data_time_cpu %>%
  select(iteration_no, time_ns, corrected_key_size) %>%
  rename(key_size_bytes = corrected_key_size) %>%
  mutate(Algorithm = "ECDH")

combined_data <- bind_rows(kyber_data, ecdh_data)

bar_chart <- ggplot(combined_data, aes(x = factor(key_size_bytes), y = time_ns, fill = Algorithm)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), width = 0.7) +  # Bar chart using mean execution time
  labs(title = "Key Size vs. Execution Time", x = "Key Size (Bytes) / Shared Secret Size (Bytes)", 
       y = "Average Execution Time (ns)", fill = "Algorithm") +
  theme_minimal()

scatter_plot <- ggplot(combined_data, aes(x = key_size_bytes, y = time_ns, color = Algorithm)) +
  geom_jitter(alpha = 0.4, width = 10) +  # Add jitter to prevent overlapping points
  geom_smooth(method = "loess", se = FALSE, span = 0.2) +  # Add trendline
  scale_y_log10() +  # Use log scale for better visibility
  labs(title = "Scatter Plot: Key Size vs. Execution Time", x = "Key Size (Bytes) / Shared Secret Size (Bytes)", 
       y = "Execution Time (ns, log scale)") +
  theme_minimal()

grid.arrange(bar_chart, scatter_plot, ncol = 2)


