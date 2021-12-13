library(ggplot2)
library(gridExtra)
require(scales)


trains <- read.csv("../25_final_data/public_transportation.csv", 
                   colClasses=c("City"="factor", "Month" = "factor", 
                                "Uber" = "factor", 
                                "Season" = "factor"))
trains$Labor_perc <- trains$Labor.Force/trains$Population * 100
trains <- na.omit(trains)
trains$log_response <- log(trains$Average.Daily.Ridership)
alt <- trains[trains$City != "New York City",]
new_york <- trains[trains$City == "New York City",]
chicago <- trains[trains$City == "Chicago",]
atlanta <- trains[trains$City == "Atlanta",]
boston <- trains[trains$City == "Boston",]
los_angeles <- trains[trains$City == "Los Angeles",]
philadelphia <- trains[trains$City == "Philadelphia",]
portland <- trains[trains$City == "Portland",]
san_francisco <- trains[trains$City == "San Francisco",]
washington <- trains[trains$City == "Washington, D.C.",]

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


## I like including: Chicago, New York, Washington, Philadelphia

chicago_pre <- chicago[chicago$Uber == "0",]
chicago_post <- chicago[chicago$Uber == "1",]
plot1 <- ggplot(chicago_pre, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + theme_hc() + ylim(450000,750000)
plot2 <- ggplot(chicago_post, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + theme_hc() + ylim(450000,750000) + xlim(2011,2020)
grid.arrange(plot1, plot2, ncol=2)

ggplot(chicago, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + 
  geom_smooth() + theme_economist() + geom_vline(xintercept=2010.83, 
                                                 linetype="dashed", 
                                                 color = "red", 
                                                 size = 1.5) + 
  ggtitle("Average Daily Train Ridership in Chicago\n") + ylab("Average Daily Ridership") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        ) + 
  scale_y_continuous(labels = comma)


new_york_pre <- new_york[new_york$Uber == "0",]
new_york_post <- new_york[new_york$Uber == "1",]
plot1 <- ggplot(new_york_pre, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + theme_hc() + ylim(450000,750000)
plot2 <- ggplot(new_york_post, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + theme_hc() + ylim(450000,750000) + xlim(2011,2020)
grid.arrange(plot1, plot2, ncol=2)

ggplot(new_york, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + 
  geom_smooth() + theme_economist() + geom_vline(xintercept=2011.5, 
                                                 linetype="dashed", 
                                                 color = "red", 
                                                 size = 1.5) + 
  ggtitle("Average Daily Train Ridership in New York\n") + ylab("Average Daily Ridership") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
  ) + 
  scale_y_continuous(labels = comma)

san_francisco_pre <- san_francisco[san_francisco$Uber == "0",]
san_francisco_post <- san_francisco[san_francisco$Uber == "1",]
plot1 <- ggplot(san_francisco_pre, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + theme_hc() + ylim(450000,750000)
plot2 <- ggplot(san_francisco_post, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + geom_smooth() + theme_hc() + ylim(450000,750000) + xlim(2011,2020)
grid.arrange(plot1, plot2, ncol=2)

ggplot(san_francisco, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + 
  geom_smooth() + theme_economist() + geom_vline(xintercept=2011.5, 
                                                 linetype="dashed", 
                                                 color = "red", 
                                                 size = 1.5) + 
  ggtitle("Average Daily Train Ridership in San Francisco") + ylab("Average Daily Ridership") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
  ) + 
  scale_y_continuous(labels = comma)

ggplot(portland, aes(x = Year, y = Average.Daily.Ridership)) + geom_point() + 
  geom_smooth() + theme_economist() + geom_vline(xintercept=2012, 
                                                 linetype="dashed", 
                                                 color = "red", 
                                                 size = 1.5) + 
  ggtitle("Average Daily Train Ridership in Portland") + ylab("Average Daily Ridership") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
  ) + 
  scale_y_continuous(labels = comma)



