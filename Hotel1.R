


####### DATA LOADING#######
# Load data_hotelset .csv as data_hotel
data_hotel <- read.csv("hotel_bookings.csv")
head(data_hotel)

View(data_hotel)

str(data_hotel)

summary(data_hotel)

# there are only 4 missing values ie. they are random and negligible 
# so we can safely ignore them as insignificant
sum(is.na(data_hotel))

#####DATASET SIZE IS TOO LARGE == 119390 rows ######################

# Calculate the size of the dataset
dataset_size <- nrow(data_hotel)
dataset_size

# Select a random subset of 10% of the size of the original dataset
# set the random seed to ensure reproducibility
set.seed(123) 
subset_size <- round(0.1 * dataset_size)
subset_size
hotel_subset <- data_hotel[sample(dataset_size, subset_size), ]
# Now size == 11939
nrow(hotel_subset)

# 
# # 0-50 days lead time has the highest frequency = 50000
# hist(hotel_subset$lead_time)
# 
# #density function is right skewed i.e lead time is not normally distributed
# plot(density(hotel_subset$lead_time))

#hotel_subset LACKS GEOLOCATION DATA, SO WE FOUND A DATASET WITH THE GEO DATA
country_loc <- read.csv("countries_codes_and_coordinates.csv")
View(country_loc)


#################################################
#############DATA CLEANING###################
#############################################

#COMBINE YEAR, MONTH,DAY COLUMNS AS A SINGLE DATE

hotel_subset$arrival_date<-as.Date(with(hotel_subset,
                                        paste(arrival_date_year,arrival_date_month,arrival_date_day_of_month,sep="-")),format = "%Y-%B-%d")

View(hotel_subset)


# BOTH COUNTRY CODE COLUMNS ARE OF DIFFERENT LENGTH THESE 2 DATASETS
length(unique(hotel_subset$country))
length(unique(country_loc$Alpha.3.code))

#to check if column names match
intersect(x = hotel_subset$country,y = country_loc$Alpha.3.code)

#intersect = 0 thus, column values dont match

#CHECKING THE VALUES OF ALPHA3 COLUMN WE FOUND THAT IT HAS LEADING SPACES
head(country_loc$Alpha.3.code)

# GET RID OF LEADING SPACES IN COUNTRY CODES COLUMN
library(stringr)
country_loc$Alpha.3.code <- str_remove(string= country_loc$Alpha.3.code,pattern = "^ +")  
head(country_loc$Alpha.3.code)

#to check if column names match
intersect(x = hotel_subset$country,y = country_loc$Alpha.3.code)

# SINCE COLUMN VALUES ARE INTERSECTING, WE CAN JOIN THE DATASETS 
#USING THE COUNTRY AND ALPHACODE3 


merged_loc <- merge(hotel_subset, country_loc, 
                    by.x = "country", by.y = "Alpha.3.code")


head(merged_loc)
View(merged_loc)

#THE RESULTING DATASET- MERGEDLOC HAS GEOLOCATION PER COUNTRY 
#WITHOUT ANY EXTRA MISSING VALUES GENERATED

table(is.na(merged_loc$Country))
sum(is.na(merged_loc))


######################################################################################
##################PLOTTING PREP ########################################################

#This code creates a data frame df with two columns: "index" containing indices 
#of columns in the data frame merged_loc and "colnames" containing names of 
#columns in merged_loc.

df <- data.frame(index = 1:length(colnames(merged_loc)),
                 colnames = colnames(merged_loc))
df

numeric_columns <- names(merged_loc)[sapply(merged_loc, is.numeric)]
print(numeric_columns)


###########################################################
###############PLOTTING
####################################################################
# Filter out non-numeric variables
cor_hotel <- merged_loc[c(3,4,9,10,11,12,13,17,18,19,22,26,28,29,30)]

# Calculate the correlation matrix
corr_hotel <- cor(cor_hotel)
#View((corr_hotel))

# Create a heatmap with column names on both axes
library(ggplot2)
library(reshape2)

# Melt the correlation matrix into long format
corr_melted <- melt(corr_hotel)
#View((corr_melted))
#The resulting heatmap provides insights into the relationship between different variables in the dataset. The color gradient helps visualize the strength and direction of the correlation, with red indicating a positive correlation and blue indicating a negative correlation. The diagonal line shows the correlation of each variable with itself, which is always 1.
# Set column names for melted data frame
colnames(corr_melted) <- c("Var1", "Var2", "Corr")

# Plot the heatmap with ggplot2
ggplot(data = corr_melted, aes(x = Var1, y = Var2, fill = Corr)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation")

#################################################################



######################
######### BARCHART - count of (Cancelled, Not-Cancelled)###############
library(ggplot2)
merged_loc$cancelled <- ifelse(merged_loc$is_canceled == 1, "cancelled", "not cancelled")
ggplot(merged_loc, 
       aes(x=cancelled)) + geom_bar()


#Histogram - (for frequency distribution of lead time data)

# Extract lead time column
lead_time <- merged_loc$lead_time

# Create histogram of lead times
hist(lead_time, 
     main = "Histogram of Lead Times", 
     xlab = "Lead Time (days)", 
     col = "blue")

# Hist Analysis - The analysis of this histogram provides information about the distribution of the lead times in the hotel booking data.
# Lead time refers to the amount of time between making a reservation and the actual arrival date. The histogram shows the 
# frequency of lead times in intervals or bins, and the shape of the histogram can provide insights into the central tendency
# and spread of the data. For example, if the histogram is unimodal and symmetrical, it indicates that the lead times are 
# normally distributed with a clear average and limited variability. On the other hand, if the histogram is skewed or has 
# multiple modes, it suggests that the lead times are not normally distributed and there may be outliers or multiple groups 
# within the data. The histogram can be used to identify patterns and make informed decisions about the lead times in the hotel 
# booking data.


# ######Line chart - (To check seasonality of bookings)#########
library(dplyr)
library(lubridate)


hotel_data_agg <- merged_loc %>% 
  group_by(year = year(merged_loc$arrival_date), month = month(merged_loc$arrival_date)) %>% 
  summarize(bookings = n())

hotel_data_agg

# Plot the line graph
ggplot(hotel_data_agg, aes(x = month, y = bookings)) +
  geom_line() +
  geom_point(color = "red") +
  xlab("Months \nYear- 2015 to 2016") +
  ylab("Number of Bookings") +
  ggtitle("Number of Bookings over Time")



######################################################
## BARPLOT to compare adr for city hotel and resort hotel


#######compare mean adr for hotel type#########

# Calculate the mean adr for each hotel type
mean_adr_by_hotel_type <- aggregate(adr ~ hotel, data = merged_loc, mean)

# Plot a bar plot to compare adr between city and resort hotels
barplot(mean_adr_by_hotel_type$adr, names.arg = mean_adr_by_hotel_type$hotel, 
        xlab = "Hotel Type", ylab = "Average Daily Rate (ADR)", 
        main = "Comparison of Average Daily Rate (ADR) between City and Resort Hotels")




########################################################
#########PLOT LEAD TIME hotel_subset BY COUNTRY ON WORLD MAP
#########################################################
  # Load required libraries
  #install.packages("ggiraph")
  library(ggplot2)
  library(ggmap)
  library(ggiraph)
  
  # create data for world coordinates using map_data() function
  world_coordinates <- map_data("world")
  
  # create world map using ggplot() function
  p <- ggplot() +
    
    # geom_map() function takes world coordinates as input to plot world map
    geom_map(
      data = world_coordinates, map = world_coordinates,
      aes(long, lat, map_id = region), 
      color = "white", fill = "lightblue", size = 0.2
    ) +
    # geom_point() adds points for each hotel subset by country with lead time
    geom_point_interactive(
      data = merged_loc,
      aes(y = Latitude..average.,x = Longitude..average., color = "red",
          size=lead_time, tooltip=paste("Country: ", Country,
                                        "<br>Lead Time: ", lead_time,
                                        " days", sep="")),
      alpha = .5
    ) + 
    # remove the legend
    theme(legend.position="none")
  
  # Create an interactive plot with ggiraph() function
  ggiraph(code = print(p))
  

  
  
  
##############################################################  
##########MODELLING#########################################   
##################################################################  
  ##### RELATIONSHIP BETWEEN STAY LENGTH AND ARRIVAL DATE OR WEEK
  
cor(x = merged_loc$arrival_date_week_number, 
    y = merged_loc$stays_in_week_nights)

cor(x = merged_loc$lead_time, y = merged_loc$stays_in_week_nights)

cor(x = merged_loc$adr, y = merged_loc$stays_in_week_nights)


#####################################
########T Test adr- city and resort#########

# Subset ADR data for City and Resort hotels
city_adr <- subset(merged_loc, hotel == "City Hotel")$adr
resort_adr <- subset(merged_loc, hotel == "Resort Hotel")$adr

# Perform two-sample t-test
t_test_result <- t.test(city_adr, resort_adr)

# Print results
t_test_result


######################################################
####LINEAR REGRESSION - ADR AND CHILDREN +
#####################################################
# Fit a linear regression model
model <- lm(adr ~ children + stays_in_week_nights, data = merged_loc)

# Print model summary
summary(model)

#####################
library(ggplot2)
library(ggpubr)

# Fit linear regression model
lm_model <- lm(adr ~ children, data = hotel_subset)

# Plot linear regression model
ggscatter(hotel_subset, x = "children", y = "adr", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Children", ylab = "ADR",
          title = "Linear Regression Model: ADR vs. Children")

################################################################

# Plot a scatter plot to visualize the relationship between ADR and Lead Time
ggplot(hotel_subset, aes(x = lead_time, y = adr)) +
  geom_point() +
  labs(x = "Lead Time", y = "ADR",
       title = "Scatter Plot of ADR vs Lead Time")

# Fit a linear regression model between ADR and Lead Time
lm_adr_lead <- lm(adr ~ lead_time, data = hotel_subset)
summary(lm_adr_lead)
# Plot the linear regression line on top of the scatter plot
ggplot(hotel_subset, aes(x = lead_time, y = adr)) +
  geom_point() +
  labs(x = "Lead Time", y = "ADR",
       title = "Scatter Plot of ADR vs Lead Time with Regression Line") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")




##########################################################################
#########################################################################
############CHAPTER OF CHOICE###############




























######################################################################
# Automated advanced hotel_subset exploration using DATAEXPLORER PACKAGE
install.packages("DataExplorer")
library(DataExplorer)

create_report(hotel_subset)







