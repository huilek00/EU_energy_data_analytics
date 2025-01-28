#change the directory according to your own directory
setwd("C:/Users/huilek/DataspellProjects/EU_energy_data_analytics_project")
ori_electricity <- read.csv("Monthly_Electricity_Data.csv", header = TRUE)

library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)


electricity <- ori_electricity 

# Change to appropriate data type
electricity$Area <- as.factor(electricity$Area)
electricity$Country.code <- as.factor(electricity$Country.code)
electricity$Date <- as.Date(electricity$Date)
electricity$Area.type <- as.factor(electricity$Area.type)
electricity$Continent <- as.factor(electricity$Continent)
electricity$Ember.region <- as.factor(electricity$Ember.region)
electricity$EU <- as.factor(electricity$EU)
electricity$OECD <- as.factor(electricity$OECD)
electricity$G20 <- as.factor(electricity$G20)
electricity$G7 <- as.factor(electricity$G7)
electricity$Category <- as.factor(electricity$Category)
electricity$Subcategory <- as.factor(electricity$Subcategory)
electricity$Variable <- as.factor(electricity$Variable)
electricity$Unit <- as.factor(electricity$Unit)

# Delete the unnecessary columns
electricity <- electricity %>% select(-c(Category, Subcategory, 
                                             YoY.absolute.change, YoY...change))

str(electricity)
summary(electricity)

# Save the distinct values of 'Variable' columns
var_names <- unique(electricity$Variable)

# Reshape the dataframe
# Change all the distinct values of 'Variable' columns into columns, 
# each column represent a distinct values
# the 'Variable' column is used as the key to spread the values across new columns.
data <- electricity %>% spread(Variable, Value)
str(data)
summary(data)

# creating new columns from the 'Unit' column and filling them 
# with the values of the 'var_names' variable
data2 <- data %>% pivot_wider(names_from = Unit, 
                              values_from = var_names, names_sep = "_")
str(data2)
summary(data2)

# checks for any columns that are entirely filled with missing values and save the column names
col <- names(which(colSums(is.na(data2)) == nrow(data2)))
print(col)

# Delete the columns that do not have any value
data3 <- data2 %>% select (-all_of(col))
str(data3)
summary(data3)

#rename the column for data3
# replace spaces in column names with underscores
colnames(data3) <- gsub(" ", "_", colnames(data3))
colnames(data3) <- gsub(",", "_", colnames(data3))
colnames(data3) <- gsub("__", "_", colnames(data3))
# replace dot to underscore
colnames(data3) <- gsub("\\.", "_", colnames(data3))
# replace % to percent
colnames(data3) <- gsub("%", "percent", colnames(data3))
# replace / to underscore
colnames(data3) <- gsub("/", "_", colnames(data3))
# replace - to underscore
colnames(data3) <- gsub("-", "_", colnames(data3))


###############################################################################
# Data visualization
# Remove the row that has N/A for 'Demand_TWh' column
no_NA <- data3[!is.na(data3$Demand_TWh),]

# Scatterplot to visualize the relationship between Demand and Total Generation
ggplot(no_NA, aes(x = Demand_TWh, y = Total_Generation_TWh, color = Area)) + 
  geom_point() + scale_x_continuous(limits = range(no_NA$Demand_TWh, na.rm = TRUE)) +
  scale_y_continuous(limits = range(no_NA$`Total_Generation_TWh`, na.rm = TRUE)) +
  ggtitle("Demand vs Total Generation")+
  xlab("Demand (TWh)")+
  ylab("Total Generation (TWh)")

# Total electricity generation
total_generation <- data3[!is.na(data3$Total_Generation_TWh),]
total_generation <- total_generation %>% filter(Date >= '2010-01-01')
total_generate <- sum(total_generation$Total_Generation_TWh)

# Remove the row which has value Region in Area_type
# Calculate the total electricity generation for each area
electricity_generation <- total_generation %>%
  filter(Area_type == 'Country') %>%
  group_by(Area) %>%
  summarize(Total_Generation_TWh = sum(Total_Generation_TWh))

# Arrange in descending order for the column Total_Generation_TWh
electricity_generation <- electricity_generation %>%
  arrange(desc(Total_Generation_TWh))

# Find the top 10 area with highest electricity generation
top10_area <- electricity_generation %>% top_n(10)
top10_area <- top10_area %>% arrange(desc(Total_Generation_TWh))

# Create a bar chart of the top 10 areas with highest electricity generation   
ggplot(top10_area, aes(reorder(Area, -Total_Generation_TWh), Total_Generation_TWh, fill = Area)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Total_Generation_TWh/total_generate * 100,2),"%")), 
            hjust = 0.5, vjust=-0.2, size=4, color='black') +
  ggtitle("Top 10 Areas with Highest Electricity Generation (TWh)") +
  xlab("Area") +
  ylab("Electricity Generation (TWh)") 

# Total electricity generation
total_emissions <- data3[!is.na(data3$Total_emissions_mtCO2),] 
total_emissions <- total_emissions %>% filter(Date >= '2010-01-01')
total_emission <- sum(total_emissions$Total_emissions_mtCO2)

# Remove the row which has value Region in Area_type
# Calculate the total power sector emissions for each area
emissions <- total_emissions %>%
  filter(Area_type == 'Country') %>%
  group_by(Area) %>%
  summarize(Total_emissions_mtCO2 = sum(Total_emissions_mtCO2))

# Arrange in descending order for the column Total_Generation_TWh
emissions <- emissions %>%
  arrange(desc(Total_emissions_mtCO2))

# Find the top 10 area with highest power sector emissions
top10_area_emission <- emissions %>% top_n(10)
top10_area_emission <- top10_area_emission %>% arrange(desc(Total_emissions_mtCO2))

# Create a bar chart of the top 10 areas with highest power sector emissions   
ggplot(top10_area_emission, aes(reorder(Area, -Total_emissions_mtCO2), Total_emissions_mtCO2, fill = Area)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Total_emissions_mtCO2/total_emission * 100,2),"%")), 
            hjust = 0.5, vjust=-0.2, size=4, color='black') +
  ggtitle("Top 10 Areas with Highest Power Sector Emissions (mtCO2)") +
  xlab("Area") +
  ylab("Power Sector Emissions (mtCO2)")


fossil_trend <- data3 %>%
  filter(Date >= '2010-01-01') %>%
  select(Date, Fossil_TWh, Fossil_mtCO2) %>%
  group_by(Date) %>%
  summarise(Fossil_TWh = sum(Fossil_TWh), Fossil_mtCO2 = sum(Fossil_mtCO2))

ggplot(fossil_trend, aes(x = Date, y = Fossil_TWh)) +
  geom_line() +
  scale_x_date(date_breaks = "2 years",
               limits = c(min(fossil_trend$Date), max(fossil_trend$Date)), expand = c(0, 0)) +
  ggtitle("Trend of Fossil  Use over Time") +
  xlab("Year") +
  ylab("Fossil  Usage (TWh)")

#################################################################################
# Data for clustering
source_name <- c('Fossil', 'Hydro_Bioenergy_and_Other_Renewables', 'Wind_and_Solar',
                 'Coal', 'Hydro', 'Other_Fossil', 'Wind', 'Clean', 'Gas_and_Other_Fossil',
                 'Renewables', 'Bioenergy', 'Gas', 'Nuclear', 'Solar', 'Other_Renewables')

source_TWh <- c('Fossil_TWh', 'Hydro_Bioenergy_and_Other_Renewables_TWh', 'Wind_and_Solar_TWh',
                'Coal_TWh', 'Hydro_TWh', 'Other_Fossil_TWh', 'Wind_TWh', 'Clean_TWh', 'Gas_and_Other_Fossil_TWh',
                'Renewables_TWh', 'Bioenergy_TWh', 'Gas_TWh', 'Nuclear_TWh', 'Solar_TWh', 'Other_Renewables_TWh')

source_mtCO2 <- c('Fossil_mtCO2', 'Hydro_Bioenergy_and_Other_Renewables_mtCO2', 'Wind_and_Solar_mtCO2',
                  'Coal_mtCO2', 'Hydro_mtCO2', 'Other_Fossil_mtCO2', 'Wind_mtCO2', 'Clean_mtCO2',
                  'Gas_and_Other_Fossil_mtCO2', 'Renewables_mtCO2', 'Bioenergy_mtCO2', 'Gas_mtCO2',
                  'Nuclear_mtCO2', 'Solar_mtCO2', 'Other_Renewables_mtCO2')

dataCluster <- cbind(Energy = source_name[1], data3[c(source_TWh[1], source_mtCO2[1])])
colnames(dataCluster) <- c("Energy", "Electricity_Generation", "Power_Sector_Emissions")

for (i in 2:15) {
  append <- cbind(Energy = source_name[i], data3[c(source_TWh[i], source_mtCO2[i])])
  colnames(append) <- c("Energy", "Electricity_Generation", "Power_Sector_Emissions")
  dataCluster <- rbind(dataCluster, append)
}

#################################################################################
# Data visualization
# Scatterplot for Electricity Generation and Power Sector Emissions
ggplot(dataCluster, aes(x = Electricity_Generation, y = Power_Sector_Emissions, color = Energy)) + 
  geom_point() + scale_x_continuous(limits = range(dataCluster$Electricity_Generation, na.rm = TRUE)) +
  scale_y_continuous(limits = range(dataCluster$Power_Sector_Emissions, na.rm = TRUE)) +
  ggtitle("Electricity Generation vs Power Sector Emissions")+
  xlab("Electricity Generation (TWh)")+
  ylab("Power Sector Emissions (mtCO2)") 

summary(dataCluster)


# Normalize the data
data3[11:61] <- scale(data3[11:61], center = TRUE, scale = TRUE)
write.csv(data3, "Electricity_Data_Cleaned.csv", row.names = FALSE)

data4 <- data3
#remove the row Total_emissions_mtCO2 is NA
data4 <- data4[!is.na(data4$Total_emissions_mtCO2),]
#remove the row Total_Generation_TWh is NA
data4 <- data4[!is.na(data4$Total_Generation_TWh),]
# replace the row Net_Imports_TWh is NA to mean value
data4$Net_Imports_TWh[is.na(data4$Net_Imports_TWh)] <- mean(data4$Net_Imports_TWh, na.rm = TRUE)
write.csv(data4, "Test_Electricity_Data_Cleaned.csv", row.names = FALSE)

str(data4)
summary(data4)
#filter the data to get the data where the EU is 1
data4_EU <- data4 %>% filter(EU == 1)

#plot the scatterplot for Coal_TWh and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Coal_TWh, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Coal_TWh, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Gas_TWh and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Gas_TWh, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Gas_TWh, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Nuclear_TWh and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Nuclear_TWh, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Nuclear_TWh, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Other_Fossil_TWh and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Other_Fossil_TWh, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Other_Fossil_TWh, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Renewables_TWh and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Renewables_TWh, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Renewables_TWh, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for CoalmtCO2 and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Coal_mtCO2, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Coal_mtCO2, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Gas_mtCO2 and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Gas_mtCO2, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Gas_mtCO2, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Nuclear_mtCO2 and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Nuclear_mtCO2, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Nuclear_mtCO2, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Other_Fossil_mtCO2 and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Other_Fossil_mtCO2, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Other_Fossil_mtCO2, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

#plot the scatterplot for Renewables_mtCO2 and CO2_intensity_gCO2_kWh
ggplot(data4_EU, aes(x = Renewables_mtCO2, y = CO2_intensity_gCO2_kWh)) +
  geom_point() + scale_x_continuous(limits = range(data4_EU$Renewables_mtCO2, na.rm = TRUE)) +
  scale_y_continuous(limits = range(data4_EU$CO2_intensity_gCO2_kWh, na.rm = TRUE))

# create the linear regression model
linear_regression_model <- lm(CO2_intensity_gCO2_kWh ~

                                  + Coal_TWh + Coal_mtCO2
                                  +Gas_TWh + Gas_mtCO2

                                  +Nuclear_TWh + Nuclear_mtCO2
                                  +Other_Fossil_TWh + Other_Fossil_mtCO2
                                  +Renewables_TWh + Renewables_mtCO2
  , data = data4_EU)

# print the summary of the model
summary(linear_regression_model)

# predict the values of the model
test_data <- data.frame(Coal_TWh = 3, Coal_mtCO2 = 2,
                        Gas_TWh = 3.00, Gas_mtCO2 = 2.50,
                        Nuclear_TWh = 0.25, Nuclear_mtCO2 =0.09,
                        Other_Fossil_TWh = 0.20, Other_Fossil_mtCO2 = 0.57,
                        Renewables_TWh = 4, Renewables_mtCO2 = 0.2)

# predict the values of the model
predicted_values <- predict(linear_regression_model, test_data)

print(predicted_values)


# remove the Electricity_Generation and Power_Sector_Emissions is NA
dataCluster <- dataCluster[!is.na(dataCluster$Electricity_Generation),]
dataCluster <- dataCluster[!is.na(dataCluster$Power_Sector_Emissions),]
# save dataCluster to csv file
write.csv(dataCluster, "Electricity_Data_Cluster.csv", row.names = FALSE)


wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(dataCluster[,2:3], centers=k, nstart=25)$withinss)
# Using the basic R plot function, each WSS is plotted against
# the respective number of centroids, 1 through 15
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")


# create the clustering model with k = 3
clustering_model <- kmeans(dataCluster[,2:3], centers = 3, nstart = 20)
print(clustering_model)
# plot the clustering model
plot(dataCluster[,2:3], col = clustering_model$cluster, pch = 19, cex = 2)
#plot the centers
points(clustering_model$centers, col = 1:3, pch = 19, cex = 3)

test_data_cluster <- dataCluster
#create the new column cluster
test_data_cluster$cluster <- clustering_model$cluster
test_data_cluster$Efficiency_label <- ifelse(test_data_cluster$cluster == 1, "low",
                              ifelse(test_data_cluster$cluster == 2, "average", "high"))

# View the number of data points in each cluster
table(test_data_cluster$cluster)

ggplot(test_data_cluster, aes(x = Electricity_Generation, y = Power_Sector_Emissions, color = Efficiency_label)) +
  geom_point() +
  labs(title = "Electricity Generation and Power Sector Emissions",
       x = "Electricity Generation (TWh)",
       y = "Power Sector Emissions (mtCO2)") +
  theme(plot.title = element_text(hjust = 0.5))
