# Load dependencies
library(ggplot2)
library(rms)
library(stats)
library(lme4)
library(sjPlot)
library(car)
library(ggthemes)

ggplot(trains, aes(x = Year, y = log_response, color = City)) + geom_smooth()


#read in data
trains <- read.csv("../25_final_data/public_transportation.csv", 
                   colClasses=c("City"="factor", "Month" = "factor", 
                                "Uber" = "factor", 
                                "Season" = "factor"))
trains$Labor_perc <- trains$Labor.Force/trains$Population * 100
trains <- na.omit(trains)

# Initial Investigation of Data
str(trains)
head(trains)
dim(trains)

# Investigating Response Variable
hist(trains$Average.Daily.Ridership)
trains$Month.Numeric <- factor(trains$Month.Numeric, ordered = TRUE, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))

# Create seperate dataset for each city to investigate.
trains$log_response <- log(trains$Average.Daily.Ridership)
alt <- trains[trains$City != "New York City",]
new_york <- trains[trains$City == "New York City",]
chicago <- trains[trains$City == "Chicago",]
atlanta <- trains[trains$City == "Atlanta",]
boston <- trains[trains$City == "Boston",]
los_angeles <- final[final$City == "Los Angeles",]
philadelphia <- trains[trains$City == "Philadelphia",]
portland <- trains[trains$City == "Portland",]
san_francisco <- trains[trains$City == "San Francisco",]
washington <- trains[trains$City == "Washington, D.C.",]

# Again, investigating response variables. Because of multiple cities, it looks funky.
hist(trains$log_response)
ggplot(trains, aes(x = Average.Daily.Ridership)) + geom_histogram(bins = 20) + facet_wrap(~City)
ggplot(trains, aes(x = Average.Daily.Ridership, 
                   fill = cut(Average.Daily.Ridership, 100))) + geom_histogram(bins = 40, show.legend = FALSE) + scale_fill_discrete(h = c(240, 10))
hist(alt$Average.Daily.Ridership)
ggplot(new_york, aes(x = Average.Daily.Ridership)) + geom_histogram(bins = 15)
ggplot(alt, aes(x = Average.Daily.Ridership)) + geom_histogram(bins = 25)
ggplot(alt, aes(x = log_response)) + geom_histogram(bins = 25)
ggplot(trains, aes(x = log_response)) + geom_histogram(bins = 40)



# Investigating relationship between response variables
# Population
ggplot(trains, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Population, y = log_response)) + geom_point() + geom_smooth()



# Population Density
ggplot(trains, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Population.Density, y = log_response)) + geom_point() + geom_smooth()

# Month
ggplot(trains, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(chicago, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(new_york, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(atlanta, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(boston, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(los_angeles, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(philadelphia, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(portland, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(san_francisco, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()
ggplot(washington, aes(x = Season, y = log_response)) + geom_boxplot() + geom_smooth()

# Labor Force %
ggplot(trains, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Labor_perc, y = log_response)) + geom_point() + geom_smooth()

# Gas Prices
ggplot(trains, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Gas.Price, y = log_response)) + geom_point() + geom_smooth()

# Vehicles
ggplot(trains, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Vehicles, y = log_response)) + geom_point() + geom_smooth()

# Route Miles
ggplot(trains, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Route.Miles, y = log_response)) + geom_point() + geom_smooth()

# Unemployment Rate
ggplot(trains, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Unemployment.Rate, y = log_response)) + geom_point() + geom_smooth()


  # Average Fare
ggplot(trains, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Average.Fare, y = log_response)) + geom_point() + geom_smooth()

# Max Temp
ggplot(trains, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Maximum.Temperature, y = log_response)) + geom_point() + geom_smooth()

# Min Temp
ggplot(trains, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Minimum.Temperature, y = log_response)) + geom_point() + geom_smooth()

# Precipitation
ggplot(trains, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Precipitation, y = log_response)) + geom_point() + geom_smooth()

# Snow
ggplot(trains, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Snowfall, y = log_response)) + geom_point() + geom_smooth()

# Snow Depth
ggplot(trains, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(chicago, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(new_york, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(atlanta, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(san_francisco, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()
ggplot(alt, aes(x = Snow.Depth, y = log_response)) + geom_point() + geom_smooth()

# Uber
ggplot(trains, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(chicago, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(new_york, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(atlanta, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(boston, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(los_angeles, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(philadelphia, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(portland, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(san_francisco, aes(x = Uber, y = log_response)) + geom_boxplot() 
ggplot(washington, aes(x = Uber, y = log_response)) + geom_boxplot() 

# Season
ggplot(trains, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(chicago, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(new_york, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(atlanta, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(boston, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(los_angeles, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(philadelphia, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(portland, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(san_francisco, aes(x = Season, y = log_response)) + geom_boxplot() 
ggplot(washington, aes(x = Season, y = log_response)) + geom_boxplot() 

# Year
ggplot(trains, aes(x = Year, y = log_response)) + geom_point() +geom_smooth()
ggplot(chicago, aes(x = Year, y = log_response)) + geom_point() + geom_smooth() 
ggplot(new_york, aes(x = Year, y = log_response)) + geom_point() + geom_smooth() 
ggplot(atlanta, aes(x = Year, y = log_response)) + geom_point() + geom_smooth()
ggplot(boston, aes(x = Year, y = log_response)) + geom_point() + geom_smooth()
ggplot(los_angeles, aes(x = Year, y = log_response)) + geom_point() + geom_smooth()
ggplot(philadelphia, aes(x = Year, y = log_response)) + geom_point() + geom_smooth()
ggplot(portland, aes(x = Year, y = log_response)) + geom_point()  + geom_smooth()
ggplot(san_francisco, aes(x = Year, y = log_response)) + geom_point() + geom_smooth()
ggplot(washington, aes(x = Year, y = log_response)) + geom_point()  + geom_smooth()

## INTERACTIONS
# Year and Uber
ggplot(trains, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(chicago, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(new_york, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(atlanta, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(boston, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(los_angeles, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(philadelphia, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(portland, aes(x = Year, y = Average.Daily.Ridership)) + geom_point()  + geom_smooth() + facet_wrap(~Uber)
ggplot(san_francisco, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(washington, aes(x = Year, y = Average.Daily.Ridership)) + geom_point()  + geom_smooth() + facet_wrap(~Uber)

# Uber and Population
ggplot(trains, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(chicago, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(new_york, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(atlanta, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(boston, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(los_angeles, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(philadelphia, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(portland, aes(x = Population, y = Average.Daily.Ridership)) + geom_point()  + geom_smooth() + facet_wrap(~Uber)
ggplot(san_francisco, aes(x = Population, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(washington, aes(x = Population, y = Average.Daily.Ridership)) + geom_point()  + geom_smooth() + facet_wrap(~Uber)


# Uber and Unemployment Rate
ggplot(trains, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(chicago, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(new_york, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(atlanta, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(boston, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(los_angeles, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(philadelphia, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(portland, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point()  + geom_smooth() + facet_wrap(~Uber)
ggplot(san_francisco, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + facet_wrap(~Uber)
ggplot(washington, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point()  + geom_smooth() + facet_wrap(~Uber)
ggplot(alt, aes(x = Unemployment.Rate, y = Average.Daily.Ridership)) + geom_point()  + geom_smooth() + facet_wrap(~Uber)




# Center Predictor Variables
trains$Population <- (trains$Population/100000) - mean(trains$Population/100000)
trains$Population.Density <- trains$Population.Density - mean(trains$Population.Density)
trains$Unemployment.Rate <- trains$Unemployment.Rate - mean(trains$Unemployment.Rate)
trains$Gas.Price <- trains$Gas.Price - mean(trains$Gas.Price)
trains$Vehicles <- (trains$Vehicles/10) - mean(trains$Vehicles/10)
trains$Route.Miles <- trains$Route.Miles/10 - mean(trains$Route.Mile/10)
trains$Average.Fare <- trains$Average.Fare - mean(trains$Average.Fare)
trains$Minimum.Temperature <- trains$Minimum.Temperature - mean(trains$Minimum.Temperature)
trains$Maximum.Temperature <- trains$Maximum.Temperature - mean(trains$Maximum.Temperature)
trains$Precipitation <- trains$Precipitation - mean(trains$Precipitation)
trains$Snowfall <- trains$Snowfall - mean(trains$Snowfall)
trains$Snow.Depth <- trains$Snow.Depth - mean(trains$Snow.Depth)
trains$Labor_perc <- trains$Labor_perc - mean(trains$Labor_perc)
trains$Year <- trains$Year - 2010

# 





# Modeling
model1 <- lm(Average.Daily.Ridership ~ City + Population + 
               Unemployment.Rate + Gas.Price + Vehicles + Route.Miles + 
               Average.Fare + Minimum.Temperature + Precipitation + Snowfall + 
               Snow.Depth + Uber + Season + Uber*Year + Labor_perc, data = trains)
plot(model1)
# Diagnosits obviously problematic, let's try logging response variable

model2 <- lm(log_response ~ City + Population + 
               Unemployment.Rate + Gas.Price + Vehicles + Route.Miles + 
               Average.Fare + Minimum.Temperature + Precipitation + Snowfall + 
               Snow.Depth + Uber + Season + Uber*Year + Labor_perc, data = trains)
# Much better! Now let's try interactions.
model3 <- lm(log_response ~ (City + Population + 
                Unemployment.Rate + Gas.Price + Vehicles + Route.Miles + 
                Average.Fare + Minimum.Temperature + Precipitation + Snowfall + 
                Snow.Depth + Season + Year + Labor_perc) * Uber + Uber*City*Year + (City + Population + 
                                                                     Unemployment.Rate + Gas.Price + Vehicles + Route.Miles + 
                                                                     Average.Fare + Minimum.Temperature + Precipitation + 
                                                                     Year + Labor_perc)* Season, data = trains)
  
  
# Let's try refiting model3 without the outliers (los angeles metro strike).
final <- trains[-c(118, 119),]
# Let's add uber*city*year combination

model4 <- lm(log_response ~ (City + Population + 
                               Unemployment.Rate + Gas.Price + Vehicles + Route.Miles + 
                               Average.Fare + Minimum.Temperature + Precipitation + Snowfall + 
                               Snow.Depth + Season + Year + Labor_perc) * Uber + Uber*City*Year + (City + Population + 
                                                                                                     Unemployment.Rate + Gas.Price + Vehicles + Route.Miles + 
                                                                                                     Average.Fare + Minimum.Temperature + Precipitation + 
                                                                                                     Year + Labor_perc)* Season, data = final)
model5 <- lm(log_response ~ City*poly(Year,3)*Uber + Population + Unemployment.Rate + 
               Gas.Price + Vehicles + Route.Miles + Average.Fare*City + 
               Minimum.Temperature + Precipitation + Snowfall + 
               Snow.Depth + Season + Labor_perc + Season*City, data = final)
## It does appear to change the model!
## AIC selection time.
null_model <- lm(log_response ~ Uber, data = final)
aic <- step(null_model, scope = formula(model4), direction = "both", trace = 0)


# Model diagnostics
plot(aic)
ggplot(final, aes(x = Gas.Price, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red") + geom_smooth(method= "loess")
ggplot(final, aes(x = Year, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")
ggplot(final, aes(x = Average.Fare, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")
ggplot(final, aes(x = Labor_perc, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")
ggplot(final, aes(x = Population, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")
ggplot(final, aes(x = Minimum.Temperature, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")
ggplot(final, aes(x = Route.Miles, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")
ggplot(final, aes(x = Vehicles, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")
ggplot(final, aes(x = Precipitation, y = final_model$residuals)) + geom_point() + geom_hline(yintercept  = 0,color = "red")  + geom_smooth(method= "loess")



newyear <- rep(-8:9, each = 9)
cities <- rep(c("Atlanta", "Boston", "Chicago","Los Angeles", "New York City", 'Philadelphia', "Portland", "San Francisco", "Washington, D.C."),18)
newdata <- data.frame(matrix(0, nrow=length(newyear), ncol = 12))
names(newdata) <- c("Uber","City", "Year", "Season", "Average.Fare", 
                    "Labor_perc", "Population", "Minimum.Temperature", "Gas.Price", 
                    "Route.Miles", "Vehicles", "Precipitation")
newdata$Year <- newyear; newdata$Season = "Summer"; newdata$Uber <- 0; newdata$Snowfall <- 0; newdata$City <- cities

newdata[newdata$City == 'Atlanta',"Average.Fare"] <- mean(final[final$City == 'Atlanta', 'Average.Fare'])
newdata[newdata$City == 'Boston',"Average.Fare"] <- mean(final[final$City == 'Boston', 'Average.Fare'])
newdata[newdata$City == 'Chicago',"Average.Fare"] <- mean(final[final$City == 'Chicago', 'Average.Fare'])
newdata[newdata$City == 'Los Angeles',"Average.Fare"] <- mean(final[final$City == 'Los Angeles', 'Average.Fare'])
newdata[newdata$City == 'New York City',"Average.Fare"] <- mean(final[final$City == 'New York City', 'Average.Fare'])
newdata[newdata$City == 'Philadelphia',"Average.Fare"] <- mean(final[final$City == 'Philadelphia', 'Average.Fare'])
newdata[newdata$City == 'Portland',"Average.Fare"] <- mean(final[final$City == 'Portland', 'Average.Fare'])
newdata[newdata$City == 'San Francisco',"Average.Fare"] <- mean(final[final$City == 'San Francisco', 'Average.Fare'])
newdata[newdata$City == 'Washington, D.C.',"Average.Fare"] <- mean(final[final$City == 'Washington, D.C.', 'Average.Fare'])

newdata[newdata$City == 'Atlanta',"Labor_perc"] <- mean(final[final$City == 'Atlanta', 'Labor_perc'])
newdata[newdata$City == 'Boston',"Labor_perc"] <- mean(final[final$City == 'Boston', 'Labor_perc'])
newdata[newdata$City == 'Chicago',"Labor_perc"] <- mean(final[final$City == 'Chicago', 'Labor_perc'])
newdata[newdata$City == 'Los Angeles',"Labor_perc"] <- mean(final[final$City == 'Los Angeles', 'Labor_perc'])
newdata[newdata$City == 'New York City',"Labor_perc"] <- mean(final[final$City == 'New York City', 'Labor_perc'])
newdata[newdata$City == 'Philadelphia',"Labor_perc"] <- mean(final[final$City == 'Philadelphia', 'Labor_perc'])
newdata[newdata$City == 'Portland',"Labor_perc"] <- mean(final[final$City == 'Portland', 'Labor_perc'])
newdata[newdata$City == 'San Francisco',"Labor_perc"] <- mean(final[final$City == 'San Francisco', 'Labor_perc'])
newdata[newdata$City == 'Washington, D.C.',"Labor_perc"] <- mean(final[final$City == 'Washington, D.C.', 'Labor_perc'])

newdata[newdata$City == 'Atlanta',"Population"] <- mean(final[final$City == 'Atlanta', 'Population'])
newdata[newdata$City == 'Boston',"Population"] <- mean(final[final$City == 'Boston', 'Population'])
newdata[newdata$City == 'Chicago',"Population"] <- mean(final[final$City == 'Chicago', 'Population'])
newdata[newdata$City == 'Los Angeles',"Population"] <- mean(final[final$City == 'Los Angeles', 'Population'])
newdata[newdata$City == 'New York City',"Population"] <- mean(final[final$City == 'New York City', 'Population'])
newdata[newdata$City == 'Philadelphia',"Population"] <- mean(final[final$City == 'Philadelphia', 'Population'])
newdata[newdata$City == 'Portland',"Population"] <- mean(final[final$City == 'Portland', 'Population'])
newdata[newdata$City == 'San Francisco',"Population"] <- mean(final[final$City == 'San Francisco', 'Population'])
newdata[newdata$City == 'Washington, D.C.',"Population"] <- mean(final[final$City == 'Washington, D.C.', 'Population'])

newdata[newdata$City == 'Atlanta',"Minimum.Temperature"] <- mean(final[final$City == 'Atlanta', 'Minimum.Temperature'])
newdata[newdata$City == 'Boston',"Minimum.Temperature"] <- mean(final[final$City == 'Boston', 'Minimum.Temperature'])
newdata[newdata$City == 'Chicago',"Minimum.Temperature"] <- mean(final[final$City == 'Chicago', 'Minimum.Temperature'])
newdata[newdata$City == 'Los Angeles',"Minimum.Temperature"] <- mean(final[final$City == 'Los Angeles', 'Minimum.Temperature'])
newdata[newdata$City == 'New York City',"Minimum.Temperature"] <- mean(final[final$City == 'New York City', 'Minimum.Temperature'])
newdata[newdata$City == 'Philadelphia',"Minimum.Temperature"] <- mean(final[final$City == 'Philadelphia', 'Minimum.Temperature'])
newdata[newdata$City == 'Portland',"Minimum.Temperature"] <- mean(final[final$City == 'Portland', 'Minimum.Temperature'])
newdata[newdata$City == 'San Francisco',"Minimum.Temperature"] <- mean(final[final$City == 'San Francisco', 'Minimum.Temperature'])
newdata[newdata$City == 'Washington, D.C.',"Minimum.Temperature"] <- mean(final[final$City == 'Washington, D.C.', 'Minimum.Temperature'])

newdata[newdata$City == 'Atlanta',"Gas.Price"] <- mean(final[final$City == 'Atlanta', 'Gas.Price'])
newdata[newdata$City == 'Boston',"Gas.Price"] <- mean(final[final$City == 'Boston', 'Gas.Price'])
newdata[newdata$City == 'Chicago',"Gas.Price"] <- mean(final[final$City == 'Chicago', 'Gas.Price'])
newdata[newdata$City == 'Los Angeles',"Gas.Price"] <- mean(final[final$City == 'Los Angeles', 'Gas.Price'])
newdata[newdata$City == 'New York City',"Gas.Price"] <- mean(final[final$City == 'New York City', 'Gas.Price'])
newdata[newdata$City == 'Philadelphia',"Gas.Price"] <- mean(final[final$City == 'Philadelphia', 'Gas.Price'])
newdata[newdata$City == 'Portland',"Gas.Price"] <- mean(final[final$City == 'Portland', 'Gas.Price'])
newdata[newdata$City == 'San Francisco',"Gas.Price"] <- mean(final[final$City == 'San Francisco', 'Gas.Price'])
newdata[newdata$City == 'Washington, D.C.',"Gas.Price"] <- mean(final[final$City == 'Washington, D.C.', 'Gas.Price'])

newdata[newdata$City == 'Atlanta',"Vehicles"] <- mean(final[final$City == 'Atlanta', 'Vehicles'])
newdata[newdata$City == 'Boston',"Vehicles"] <- mean(final[final$City == 'Boston', 'Vehicles'])
newdata[newdata$City == 'Chicago',"Vehicles"] <- mean(final[final$City == 'Chicago', 'Vehicles'])
newdata[newdata$City == 'Los Angeles',"Vehicles"] <- mean(final[final$City == 'Los Angeles', 'Vehicles'])
newdata[newdata$City == 'New York City',"Vehicles"] <- mean(final[final$City == 'New York City', 'Vehicles'])
newdata[newdata$City == 'Philadelphia',"Vehicles"] <- mean(final[final$City == 'Philadelphia', 'Vehicles'])
newdata[newdata$City == 'Portland',"Vehicles"] <- mean(final[final$City == 'Portland', 'Vehicles'])
newdata[newdata$City == 'San Francisco',"Vehicles"] <- mean(final[final$City == 'San Francisco', 'Vehicles'])
newdata[newdata$City == 'Washington, D.C.',"Vehicles"] <- mean(final[final$City == 'Washington, D.C.', 'Vehicles'])

newdata[newdata$City == 'Atlanta',"Route.Miles"] <- mean(final[final$City == 'Atlanta', 'Route.Miles'])
newdata[newdata$City == 'Boston',"Route.Miles"] <- mean(final[final$City == 'Boston', 'Route.Miles'])
newdata[newdata$City == 'Chicago',"Route.Miles"] <- mean(final[final$City == 'Chicago', 'Route.Miles'])
newdata[newdata$City == 'Los Angeles',"Route.Miles"] <- mean(final[final$City == 'Los Angeles', 'Route.Miles'])
newdata[newdata$City == 'New York City',"Route.Miles"] <- mean(final[final$City == 'New York City', 'Route.Miles'])
newdata[newdata$City == 'Philadelphia',"Route.Miles"] <- mean(final[final$City == 'Philadelphia', 'Route.Miles'])
newdata[newdata$City == 'Portland',"Route.Miles"] <- mean(final[final$City == 'Portland', 'Route.Miles'])
newdata[newdata$City == 'San Francisco',"Route.Miles"] <- mean(final[final$City == 'San Francisco', 'Route.Miles'])
newdata[newdata$City == 'Washington, D.C.',"Route.Miles"] <- mean(final[final$City == 'Washington, D.C.', 'Route.Miles'])




newdata$Precipitation <- 0
newdata[newdata$City == "New York City" & newdata$Year > 0,'Uber'] <- 1
newdata[newdata$City == "Chicago" & newdata$Year > 0,'Uber'] <- 1
newdata[newdata$City == "Washington, D.C." & newdata$Year > 1,'Uber'] <- 1
newdata[newdata$City == "Portland" & newdata$Year > 1,'Uber'] <- 1
newdata[newdata$City == "Atlanta" & newdata$Year > 0,'Uber'] <- 1
newdata[newdata$City == "Boston" & newdata$Year > 0,'Uber'] <- 1
newdata[newdata$City == "Los Angeles" & newdata$Year > 0,'Uber'] <- 1
newdata[newdata$City == "Philadelphia" & newdata$Year > 0,'Uber'] <- 1
newdata[newdata$City == "San Francisco" & newdata$Year > 0,'Uber'] <- 1


newdata$Uber <- factor(newdata$Uber)
newyork <- newdata[newdata$City == "New York City",]
newdata <-newdata[newdata$City != "New York City",]

preds_no_train <- predict(final_model, newdata, type = "response", se.fit = TRUE)

fit <- exp(preds_no_train$fit)
newdata$Year <- newdata$Year + 2010

ggplot(newdata, aes(x = Year, y = fit, color = City)) + geom_line() + theme_economist() + geom_vline(xintercept=2011, 
                                                 linetype="dashed", 
                                                 color = "red", 
                                                 size = 1.5) + 
  ggtitle("Predicted Train Ridership in New York") + ylab("Average Daily Ridership") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
  ) + 
  scale_y_continuous(labels = comma)


newdata$Average.Daily.Ridership <- fit
newdata <- newdata %>% arrange(City, Year)
test <-change(newdata, "Average.Daily.Ridership", "City", "Year", "pct_change", 
              slideBy = -1, type = "percent")
test <- test[test$Year == 2003 | test$Year == 2015,]
test$Ridesharing <- "Not Available"
test[test$Year == 2015,"Ridesharing"] <- "Available"
test[test$City == 'Atlanta',"City"] <- "Atlanta"
test[test$City == 'Boston',"City"] <- "Boston"
test[test$City == 'Chicago',"City"] <- "Chicago"
test[test$City == 'Los Angeles',"City"] <- "LA"
test[test$City == 'New York City',"City"] <- "NYC"
test[test$City == 'Philadelphia',"City"] <- "Philadelphia"
test[test$City == 'Portland',"City"] <- "Portland"
test[test$City == 'San Francisco',"City"] <- "SF"
test[test$City == 'Washington, D.C.',"City"] <- "D.C."



ggplot(test, aes(fill = forcats::fct_rev(Ridesharing), y = pct_change, 
                 x = City)) +  geom_bar(position="dodge", stat="identity", width = 0.8) +
  theme_bw() + guides(fill=guide_legend(title="Ridesharing")) + 
  labs(y = "Percent Change in Year over Year Ridership", title = 
         "Ridership Change by Year by Ridersharing Status") + theme(plot.title = element_text(hjust = 0.5))


specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")
