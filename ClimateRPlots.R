# Load Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(scales)



skygrid_data <- read.csv("C:/Users/caitg/OneDrive/Documents/Work/Honours_Stellies/BAT_sequences/Cytb new/est17_datatable.csv")
skydata <- read.table("C:/Users/caitg/OneDrive/Documents/Work/Honours_Stellies/BAT_sequences/Cytb new/est17_datatable.csv", sep = "\t", header = TRUE, fill = TRUE, quote = "", comment.char = "", skip = 1)
head(skydata)

# Check the column names
colnames(skydata)

# Inspect the structure
str(skydata)
print(head(skydata))
skydata$X.time <- gsub('\"', '', skydata$X.time)

# Step 2: Convert the cleaned values into numeric format
skydata$X.time <- as.numeric(skydata$X.time)
# Check if the columns are being correctly passed
print(skydata$X.time)  # Should print the values for the x-axis column
skydata$mean <- gsub('"', '', skydata$mean)

# Step 2: Convert the cleaned values to numeric
skydata$mean <- as.numeric(skydata$mean)

# Step 3: Check the structure of the cleaned column
str(skydata$mean)
skydata$lower. <- gsub('"', '', skydata$lower.)

# Step 2: Convert the cleaned values to numeric
skydata$lower. <- as.numeric(data$lower.)

# Step 3: Check the structure of the cleaned column
str(skydata$lower.)
print(skydata$lower.)  # Sho
# Create a basic plot

ggplot(data = skydata, aes(x =X.time, y = mean)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower., ymax = upper), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 2000)) +
  #coord_cartesian(ylim = c(0, 4000)) +
  
  labs(title = "Mean with 95% HPD Interval", x = "Time", y = "Effective Population Size (Ne)")+
scale_x_continuous("Time (years)") 
  scale_y_continuous(trans="log10","Effective Population Size(Ne)") +
  theme_minimal()
  
  
  # Load Beta Variant Data
  data <- read.csv("C:/Users/caitg/Downloads/land-use-over-the-long-term.csv")
  
  
  
  data1 <- subset(data, Entity == "South Africa")
  
  
  data2 <- subset(data, Entity == "Angola")
  
  
  data3 <- subset(data, Entity == "Tanzania")
  
  
  data4 <- subset(data, Entity == "Africa")
  
  data5<-subset(data, Entity == "Zambia")
  
  
  
  # Update the color palette to include new regions
  cols <- c("Africa" = "#1B9E77",
            "Angola" = "pink",
            "South Africa" = "#7570B3",
            "Tanzania" = "#B7E902",
            "Zambia"="#E3930B",
            "Skygrid Plot"="red")
  
  # Plot Beta Variants with Sum of Interactions
 
 sheknows <- ggplot() + 
    theme_minimal() + 
    #geom_area(data = southafrica_to_africa_table_summarize, aes(x = date4, y = sum, fill = 'Within South Africa'), alpha = 0.2) + 
    geom_line(data = data1, aes(x = Year, y = Land.use..Built.up.area, color = 'South Africa'),linetype = 'dashed',size=0.8) + 
    #geom_area(data = europe_to_southafrica_table_summarize, aes(x = date4, y = sum, fill = 'From Europe'), alpha = 0.2) + 
    geom_line(data = data2, aes(x = Year, y = Land.use..Built.up.area, color = 'Angola'),linetype = 'dashed',size=0.8) +
    #geom_area(data = europe_to_southafrica_table_summarize, aes(x = date4, y = sum, fill = 'From Europe'), alpha = 0.2) +
    geom_line(data = data3, aes(x = Year, y = Land.use..Built.up.area, color = 'Tanzania'),linetype = 'dashed',size=0.8) + 
    #geom_area(data = southafrica_to_europe_exports_table_summarize, aes(x = date4, y = sum, fill = 'To Europe'), alpha = 0.2) + 
    geom_line(data = data4, aes(x = Year, y = Land.use..Built.up.area, color = 'Africa'), linetype = 'dashed',size=0.8) + 
    #geom_area(data = Namerica_to_southafrica_table_summarize, aes(x = date4, y = sum, fill = 'From North America'), alpha = 0.2) +
    geom_line(data = data5, aes(x = Year, y = Land.use..Built.up.area, color = 'Zambia'),linetype = 'dashed' ,size=0.8) + 
    geom_line(data = skydata, aes(x = X.time, y = mean*10, color = 'Skygrid Plot'),size=1.5)+
    scale_colour_manual(values = cols, name = '') + 
    scale_fill_manual(values = cols, name = '') + 
    theme(legend.position = 'top', legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + 
   # xlab('Year') + 
   # ylab('Land Use Built.up.area') +
    scale_y_continuous(trans="log10",
        name = "Land.use Built.up.area",  # Primary y-axis label
        sec.axis = sec_axis(~ . / 100, name = "skygrid mean")  # Secondary y-axis (scaled version of predictor2)
      ) +
   scale_x_continuous(limits=c(500,2030))+
   theme_minimal()
    
  print(sheknows)
 #   scale_y_continuous(trans="log10")
  combined_dt1 <- rbind(data1, data2, data3, data4, data5)
  small_table1 <- subset(combined_dt, select = c(Year, Land.use..Grazingland))
  combined_dt2 <- rbind(data1, data2, data3, data4, data5)
  small_table2 <- subset(combined_dt, select = c(Year, Land.use..Cropland))
  combined_dt3 <- rbind(data1, data2, data3, data4, data5)
  small_table3 <- subset(combined_dt, select = c(Year, Land.use..Built.up.area))
  # Inspect the smaller table
  head(small_table1) 
  head(small_table2) 
  head(small_table3) 
  
  
  # Display the Plot
 
  # Load Beta Variant Data
  temp <- read.csv("C:/Users/caitg/Downloads/av_temp.csv")
  
  # Update the color palette to include new regions
  cols <- c("temperature" = "#1B9E77","Skygrid Plot"="red")
  # Plot Beta Variants with Sum of Interactions
  sheknows2 <- ggplot() + 
    theme_minimal() + 
    geom_line(data = temp, aes(x = Category, y = Annual.Mean, color = 'temperature'),size=1)+
    geom_line(data = skydata, aes(x = X.time, y = mean/100, color = 'Skygrid Plot'),size=1.5)+
    scale_color_manual(values = cols) + 
    theme(legend.position = 'top', legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + 
    xlab('Year') + 
    ylab('Mean Temperature') +
 scale_y_continuous(
                   name = "Mean Temperature",  # Primary y-axis label
                   sec.axis = sec_axis(~ .*40 , name = "skygrid mean")  # Secondary y-axis (scaled version of predictor2)
  ) +
 scale_x_continuous(limits=c(1880,2025))
  
  
  # Display the plot
  print(sheknows2)
sky_table<- subset(skydata, select = c(X.time, mean))
# Load and combine CSV files into one dataframe
head(sky_table)

temp_table<- subset(temp, select = c(Category, Annual.Mean))
# Load and combine CSV files into one dataframe
head(temp_table)
colnames(sky_table) <- c("year", "mean")  # Adjust based on your actual column names
colnames(small_table1) <- c("year", "mean")
colnames(small_table2) <- c("year", "mean")
colnames(small_table3) <- c("year", "mean")
colnames(temp_table) <- c("year", "mean")

min_rows <- min(nrow(sky_table), nrow(small_table1), nrow(small_table2), nrow(small_table3))
combined_data <- data.frame(
  response = sky_table$mean[1:min_rows],
  predictor1 = small_table1$mean[1:min_rows],
  predictor2 = small_table2$mean[1:min_rows],
  predictor3 = small_table3$mean[1:min_rows])
mean2<-skydata$mean
combined_data <- rbind(sky_table,small_table1, small_table2, small_table3, temp_table, mean2)

model <- glm(response ~predictor1 + predictor2 +predictor3 , data = combined_data, family = poisson())

# Inspect the combined data
head(combined_data)

# Rename columns if necessary
colnames(combined_data) <- c("year", "mean")

# Fit a GLM where mean is the response and year is the predictor
glm_model <- glm(mean ~ year, data = combined_data, family = gaussian)
"Sky Data"<- sky_table$combined_data
# View model summary
summary(glm_model)

# Predict the mean values
predicted_means <- predict(glm_model)
combined_data$predicted_mean <- predicted_means
filtered_data <- combined_data %>% 
  filter(mean > 0)
print(predictor1)

predictions <- predict(model, newdata = combined_data)
print(predictions)
print(mean2)

model <- lm(mean ~ year, data = combined_data)  # Fit a linear model
combined_data$predicted_mean <- predict(model)


ggplot(combined_data, aes(x = year)) +
geom_point(aes(y = mean), color = "blue", size = 2) +  # Points for the response variable
  geom_smooth(aes(y = mean), method = "lm", se = FALSE, color = "blue") + 
  geom_line(aes(y = predicted_mean), color = "green", size = 1) +# GLM line for mean
  geom_point(aes(y = sky_table$mean), color = "red", size = 2) +  # Points for predictor2, rescaled
  geom_smooth(aes(y = sky_table$mean), method = "lm", se = FALSE, color = "red") +  # Line for predictor2
  scale_y_continuous( 
    name = "Mean (Blue)",  # Primary y-axis label
    sec.axis = sec_axis(~ . * 10, name = "Skygrid (Red)")  # Secondary y-axis label
  ) +
  scale_x_continuous(limits = c(1900, 2050))+
  labs(title = "GLM with Two Y-Axes",
       x = "Year") +
  theme_minimal()
 
colnames(combined_data)
head(combined_data)
nrow(combined_data)  # Number of rows in the dataset
length(combined_data$mean)  # Number of rows in 'mean'
length(combined_data$mean2)  # Number of rows in 'mean2'
length(combined_data$predicted_mean)
