install.packages('readxl')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('naniar')
install.packages('ROSE')
install.packages("modeest")

library(readxl)
library(dplyr)
library(ggplot2)
library(naniar)
library(ROSE)
library(modeest) 

data<-read.csv("D:/AIUB/Summer/DS/CaesarianSectionClassificationDataset.csv",header = TRUE,sep = ",")
print(data)


summary(data)
str(data)
head(data)














missing_values <- colSums(is.na(data))
print(missing_values) 

empty_strings <- colSums(data == "", na.rm = TRUE)
print(empty_strings) 






data_temp <- data
data_temp[data_temp == ""] <- NA

gg_miss_var(data_temp) +
  labs(title = "Missing Values per Variable", y = "Number of Missing Values")







data <- data %>%
  mutate(Age = ifelse(is.na(Age) | Age == "" | Age == "NA" | Age == "N/A" | Age == "-", median(Age, na.rm = TRUE), Age),
         Gender = ifelse(is.na(Gender) | Gender == "" | Gender == "NA" | Gender == "N/A" | Gender == "-", names(sort(table(Gender), decreasing = TRUE))[1], Gender),
         `weight.kg.` = ifelse(is.na(`weight.kg.`) | `weight.kg.` == "" | `weight.kg.` == "NA" | `weight.kg.` == "N/A" | `weight.kg.` == "-", median(`weight.kg.`, na.rm = TRUE), `weight.kg.`),
         Delivery_number = ifelse(is.na(Delivery_number) | Delivery_number == "" | Delivery_number == "NA" | Delivery_number == "N/A" | Delivery_number == "-", median(Delivery_number, na.rm = TRUE), Delivery_number),
         Delivery_time = ifelse(is.na(Delivery_time) | Delivery_time == "" | Delivery_time == "NA" | Delivery_time == "N/A" | Delivery_time == "-", names(sort(table(Delivery_time), decreasing = TRUE))[1], Delivery_time),
         Blood = ifelse(is.na(Blood) | Blood == "" | Blood == "NA" | Blood == "N/A" | Blood == "-", names(sort(table(Blood), decreasing = TRUE))[1], Blood),
         Caesarian = ifelse(is.na(Caesarian) | Caesarian == "" | Caesarian == "NA" | Caesarian == "N/A" | Caesarian == "-", names(sort(table(Caesarian), decreasing = TRUE))[1], Caesarian))

print(data)





data <- data %>% distinct()
print(data)















original_rows <- nrow(data_temp)
new_rows <- nrow(data)
cat("Original number of rows:", original_rows, "\n")
cat("Number of rows after removing duplicates:", new_rows, "\n")







str(data)










data <- data %>%
  mutate(Delivery_number = as.integer(Delivery_number),
         Delivery_time = as.integer(Delivery_time),
         Caesarian = as.integer(Caesarian),
         Gender = as.factor(Gender),
         Blood = as.factor(Blood))


str(data)




data <- data %>%
  mutate(Age = ifelse(Age < 0, NA, Age),
         `weight.kg.` = ifelse(`weight.kg.` <= 0, NA, `weight.kg.`),
         Delivery_number =as.integer( ifelse(is.na(as.integer(Delivery_number)), NA, as.integer(Delivery_number))),
         Delivery_time = ifelse(!Delivery_time %in% c(0, 1, 2), NA, Delivery_time),
         Heart = ifelse(!Heart %in% c(0, 1), NA, Heart),
         Caesarian = ifelse(!Caesarian %in% c(0, 1), NA, Caesarian))

data <- data %>%
  mutate(Gender = ifelse(!Gender %in% c("male", "female"), NA, as.character(Gender)),
         Blood = ifelse(!Blood %in% c("high", "low", "normal"), NA, as.character(Blood)))
print(data)





data <- data %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
         `weight.kg.` = ifelse(is.na(`weight.kg.`), median(`weight.kg.`, na.rm = TRUE), `weight.kg.`),
         Delivery_number = as.integer(ifelse(is.na(Delivery_number), median(as.integer(Delivery_number), na.rm = TRUE), as.integer(Delivery_number))),
         Delivery_time = ifelse(is.na(Delivery_time), median(Delivery_time, na.rm = TRUE), Delivery_time),
         Heart = ifelse(is.na(Heart), median(Heart, na.rm = TRUE), Heart),
         Caesarian = ifelse(is.na(Caesarian), median(Caesarian, na.rm = TRUE), Caesarian))


data <- data %>%
  mutate(Gender = ifelse(is.na(Gender), names(sort(table(data$Gender), decreasing = TRUE))[1], Gender),
         Blood = ifelse(is.na(Blood), names(sort(table(data$Blood), decreasing = TRUE))[1], Blood))
print(data)




identify_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
}

data <- data %>%
  filter(!identify_outliers_iqr(Age)) %>%
  filter(!identify_outliers_iqr(`weight.kg.`)) %>%
  filter(!identify_outliers_iqr(Delivery_number))

print(data)






plot_balance_categorical <- function(data, column_name) {
  counts <- table(data[[column_name]])
  max_count <- max(counts)
  min_count <- min(counts)
  
  plot <- ggplot(data, aes_string(x = column_name)) +
    geom_bar(fill = "skyblue") +
    labs(title = paste(column_name, "- Categorical Variable Distribution"),
         x = column_name, y = "Count") +
    theme_minimal()
  

  print(plot)
  

  if (max_count / min_count <= 2) {
    return(paste(column_name, "is balanced"))
  } else {
    return(paste(column_name, "is unbalanced"))
  }
}


plot_balance_continuous <- function(data, column_name) {
  std_dev <- sd(data[[column_name]], na.rm = TRUE)
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  

  plot <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
    labs(title = paste(column_name, "- Continuous Variable Distribution"),
         x = column_name, y = "Frequency") +
    theme_minimal()
  

  print(plot)
  

  if (std_dev / mean_value <= 0.5) {
    return(paste(column_name, "is balanced"))
  } else {
    return(paste(column_name, "is unbalanced"))
  }
}

print(plot_balance_categorical(data, "Gender"))

print(plot_balance_categorical(data, "Blood"))

print(plot_balance_continuous(data, "Age"))


print(plot_balance_continuous(data, "weight.kg."))


print(plot_balance_categorical(data, "Delivery_number"))


print(plot_balance_categorical(data, "Delivery_time"))


print(plot_balance_categorical(data, "Heart"))

print(plot_balance_categorical(data, "Caesarian"))






balance_categorical <- function(data, column_name) {
 
  level_counts <- table(data[[column_name]])
  max_count <- max(level_counts)
  
  
  balanced_data <- data %>% 
    group_by_at(column_name) %>% 
    sample_n(size = max_count, replace = TRUE) %>% 
    ungroup()
  
  return(balanced_data)
}


data <- balance_categorical(data, "Gender")
data <- balance_categorical(data, "Blood")
data <- balance_categorical(data, "Delivery_time")
data <- balance_categorical(data, "Delivery_number")


plot_balance_categorical <- function(data, column_name) {
  counts <- table(data[[column_name]])
  max_count <- max(counts)
  min_count <- min(counts)
  

  plot <- ggplot(data, aes_string(x = column_name)) +
    geom_bar(fill = "skyblue") +
    labs(title = paste(column_name, "- Categorical Variable Distribution"),
         x = column_name, y = "Count") +
    theme_minimal()
  

  print(plot)
  

  if (max_count / min_count <= 2) {
    return(paste(column_name, "is balanced"))
  } else {
    return(paste(column_name, "is unbalanced"))
  }
}


print(plot_balance_categorical(data, "Gender"))
print(plot_balance_categorical(data, "Blood"))
print(plot_balance_categorical(data, "Delivery_time"))
print(plot_balance_categorical(data, "Delivery_number"))





normalize_min_max <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}


data <- data %>%
  mutate(
    `weight.kg.` = normalize_min_max(`weight.kg.`)
  )
print(data,n=100)






data$Age_category <- cut(data$Age, 
                         breaks = c(-Inf, 25, 35, Inf), 
                         labels = c("Young", "Middle-aged", "Old"))


data$Weight_category <- cut(data$weight.kg., 
                            breaks = c(-Inf, 0.3, 0.6, Inf), 
                            labels = c("Underweight", "Normal", "Overweight"))


head(data)






female_data <- subset(data, Gender == "female")
print(female_data)

age_filtered_data <- subset(data, Age >= 25 & Age <= 30)
print(age_filtered_data)

multi_filter_data <- subset(data, Gender == "female" & Age >= 20 & Age <= 30 & Delivery_time == 0)
print(multi_filter_data)






plot_variable_with_stats <- function(data, var_name) {
  
  column_data <- data[[var_name]]
  
  
  if (is.numeric(column_data)) {
   
    mean_value <- mean(column_data)
    median_value <- median(column_data)
    mode_value <- mfv(column_data)
    

    ggplot(data, aes_string(x = var_name)) +
      geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "skyblue", alpha = 0.6) +
      geom_density(color = "blue", size = 1) +
      
      
      geom_vline(aes(xintercept = mean_value), color = "red", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = median_value), color = "green", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = mode_value), color = "purple", linetype = "dashed", size = 1) +
      
    
      annotate("text", x = mean_value, y = max(density(column_data)$y), 
               label = paste("Mean:", round(mean_value, 2)), color = "red", angle = 90, vjust = -0.5, hjust = -0.5) +
      annotate("text", x = median_value, y = max(density(column_data)$y) * 0.8, 
               label = paste("Median:", round(median_value, 2)), color = "black", angle = 90, vjust = -0.5, hjust = -0.5) +
      annotate("text", x = mode_value, y = max(density(column_data)$y) * 0.6, 
               label = paste("Mode:", round(mode_value, 2)), color = "purple", angle = 90, vjust = -0.5, hjust = -0.5) +
      

      labs(title = paste("Distribution of", var_name, "with Mean, Median, and Mode"),
           x = var_name, y = "Density")
    
  } else if (is.factor(column_data) || is.character(column_data)) {
   
    data[[var_name]] <- as.factor(column_data)
    
   
    mode_value <- mfv(data[[var_name]])
    
  
    ggplot(data, aes_string(x = var_name)) +
      geom_bar(fill = "skyblue", alpha = 0.7) +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
      annotate("text", x = mode_value, y = max(table(data[[var_name]])) + 2, 
               label = paste("Mode:", mode_value), color = "purple", size = 5) +
      labs(title = paste("Distribution of", var_name, "with Mode"), 
           x = var_name, y = "Count")
  } else {
    stop("The variable must be either numeric or categorical.")
  }
}

plot_variable_with_stats(data, "Age")

plot_variable_with_stats(data, "Gender")

