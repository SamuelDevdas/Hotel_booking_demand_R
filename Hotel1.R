


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

# 0-50 days lead time has the highest frequency = 50000
hist(data_hotel$lead_time)

#density function is right skewed i.e lead time is not normally distributed
plot(density(data_hotel$lead_time))

#DATA_HOTEL LACKS GEOLOCATION DATA, SO WE FOUND A DATASET WITH THE GEO DATA
country_loc <- read.csv("countries_codes_and_coordinates.csv")
View(country_loc)



#############DATA CLEANING###################

#COMBINE YEAR, MONTH,DAY COLUMNS AS A SINGLE DATE

data_hotel$arrival_date<-as.Date(with(data_hotel,paste(arrival_date_year,arrival_date_month,arrival_date_day_of_month,sep="-")),format = "%Y-%B-%d")

View(data_hotel)


# BOTH COUNTRY CODE COLUMNS ARE OF DIFFERENT LENGTH THESE 2 DATASETS
length(unique(data_hotel$country))
length(unique(country_loc$Alpha.3.code))

#to check if column names match
intersect(x = data_hotel$country,y = country_loc$Alpha.3.code)

#intersect = 0 thus, column values dont match

#CHECKING THE VALUES OF ALPHA3 WE FOUND THAT IT HAS LEADING SPACES
head(country_loc$Alpha.3.code)

# GET RID OF LEADING SPACES IN COUNTRY CODES COLUMN
library(stringr)
country_loc$Alpha.3.code <- str_remove(string= country_loc$Alpha.3.code,pattern = "^ +")  
head(country_loc$Alpha.3.code)

#to check if column names match
intersect(x = data_hotel$country,y = country_loc$Alpha.3.code)

# SINCE COLUMN VALUES ARE INTERSECTING, WE CAN JOIN THE DATASETS 
#USING THE COUNTRY AND ALPHACODE3 


merged_loc <- merge(data_hotel, country_loc, 
                    by.x = "country", by.y = "Alpha.3.code")


head(merged_loc)
View(merged_loc)

#THE RESULTING DATASET- MERGEDLOC HAS GEOLOCATION PER COUNTRY 
#WITHOUT ANY EXTRA MISSING VALUES GENERATED

table(is.na(merged_loc$Country))
sum(is.na(merged_loc))

######################################################################################


##################PLOTTING########################################################

df <- data.frame(index = 1:length(colnames(merged_loc)),
                 colnames = colnames(merged_loc))
df

numeric_columns <- names(merged_loc)[sapply(merged_loc, is.numeric)]
print(numeric_columns)

# To get rid of non-numeric variables for using corelation heat map function
cor_hotel <- merged_loc[c(3,4,9,10,11,12,13,17,18,19,22,26,28,29,30)]
cor_hotel

#Now all the variables are numeric
str(cor_hotel)


# Calculate the correlation matrix
#corr_hotel <- cor(cor_hotel)
corr_hotel <- data.matrix(cor_hotel, rownames.force = NA)



# Create the heat map
#corrplot(cor_hotel, method = "color", type = "lower")
heatmap(corr_hotel, scale = "row")

#Error in hclustfun(distfun(x)) : size cannot be NA nor exceed 65536

# Due to the size of the dataset function fails


######################
######### barchart - CoUNT OF (Cancelled, Not-Cancelled)###############
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


##########################################################
########



########################################################
# PLOT LEAD TIME data_hotel BY COUNTRY ON WORLD MAP
library(ggplot2)
library(ggmap)

# BACKGROUND MAP AND PLOTS

# create data for world coordinates using 
# map_data() function
world_coordinates <- map_data("world")

# create world map using ggplot() function
ggplot() +
  
  # geom_map() function takes world coordinates 
  # as input to plot world map
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region), 
    color = "white", fill = "lightblue", size = 0.2
  ) +
  geom_point(
    data = merged_loc,
    aes(y = Latitude..average.,x = Longitude..average., color = "red",
        size=lead_time),
    alpha = .5
  ) 
  
  # legend.position as none removes the legend
  theme(legend.position="none")
  

  
  
  
  
  
  
  
  
  
  
  
##################################################################  
  ##### RELATIONSHIP BETWEEN STAY LENGTH AND ARRIVAL DATE OR WEEK
  
cor(x = merged_loc$arrival_date_week_number, 
    y = merged_loc$stays_in_week_nights)

cor(x = merged_loc$lead_time, y = merged_loc$stays_in_week_nights)

cor(x = merged_loc$adr, y = merged_loc$stays_in_week_nights)












######################################################################
# Automated advanced data_hotel exploration using data_hotelEXPLORER PACKAGE
install.packages("DataExplorer")
library(DataExplorer)

create_report(data_hotel)



##################################
###converting data to numeric for correlation
date_num <- as.numeric(unique(merged_loc$arrival_date))
date_num

plot(date_num,merged_loc$stays_in_week_nights)

cor(x = date_num,y = merged_loc$stays_in_week_nights )

# adr== average daily rate == estimate of average price 
# charged per room per night
plot(data_hotel$adr, data_hotel$is_canceled)
fit <- lm(data_hotel$is_canceled~ data_hotel$adr)
abline(fit)
summary(fit)

boxplot(data_hotel$adr)

outliers <- subset(data_hotel, data_hotel$adr >400)

outliers

