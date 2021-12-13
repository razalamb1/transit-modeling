library(ggplot2)
library(rms)
library(stats)
library(lme4)
library(sjPlot)
library(car)

trains <- read.csv("../25_final_data/public_transportation.csv", 
                   colClasses=c("City"="factor", "Month" = "factor", 
                                "Uber" = "factor", 
                                "Season" = "factor"))
trains$Labor_perc <- trains$Labor.Force/trains$Population * 100
trains <- na.omit(trains)

chicago <- trains[trains$City == "Chicago",]
chicago$Population <- (chicago$Population/100000) - mean(chicago$Population/100000)
chicago$Population.Density <- chicago$Population.Density - mean(chicago$Population.Density)
chicago$Unemployment.Rate <- chicago$Unemployment.Rate - mean(chicago$Unemployment.Rate)
chicago$Gas.Price <- chicago$Gas.Price - mean(chicago$Gas.Price)
chicago$Vehicles <- (chicago$Vehicles/10) - mean(chicago$Vehicles/10)
chicago$Route.Miles <- chicago$Route.Miles/10 - mean(chicago$Route.Mile/10)
chicago$Average.Fare <- chicago$Average.Fare - mean(chicago$Average.Fare)
chicago$Minimum.Temperature <- chicago$Minimum.Temperature - mean(chicago$Minimum.Temperature)
chicago$Maximum.Temperature <- chicago$Maximum.Temperature - mean(chicago$Maximum.Temperature)
chicago$Precipitation <- chicago$Precipitation - mean(chicago$Precipitation)
chicago$Snowfall <- chicago$Snowfall - mean(chicago$Snowfall)
chicago$Snow.Depth <- chicago$Snow.Depth - mean(chicago$Snow.Depth)
chicago$Labor_perc <- chicago$Labor_perc - mean(chicago$Labor_perc)
chicago$Year <- chicago$Year - 2010


model1 <- lm(Average.Daily.Ridership ~ Uber* (poly(Population,3) + Unemployment.Rate + 
                                                Gas.Price + Vehicles + Average.Fare + 
                                                poly(Minimum.Temperature,3) + Precipitation + 
                                                Snowfall + Snow.Depth + Season +
                                                Labor_perc + Year) + 
               Season * (Unemployment.Rate + Gas.Price + Average.Fare + 
                           Precipitation + Season + Labor_perc), data = chicago)

# Model selection
null_model <- lm(Average.Daily.Ridership ~ Uber, data = chicago)
aic <- step(null_model, scope = formula(model1), direction = "both", trace = 0)

# Model diagnostics
plot(aic)
ggplot(chicago, aes(x = Population, y = aic$residuals)) + geom_point() + geom_hline(yintercept = 0, color ="red") + geom_smooth()
ggplot(chicago, aes(x = Labor_perc, y = aic$residuals)) + geom_point() + geom_hline(yintercept = 0, color ="red") + geom_smooth()
ggplot(chicago, aes(x = Minimum.Temperature, y = aic$residuals)) + geom_point() + geom_hline(yintercept = 0, color ="red") + geom_smooth()
ggplot(chicago, aes(x = Year, y = aic$residuals)) + geom_point() + geom_hline(yintercept = 0, color ="red") + geom_smooth()
ggplot(chicago, aes(x = Average.Fare, y = aic$residuals)) + geom_point() + geom_hline(yintercept = 0, color ="red") + geom_smooth()

final <- lm(formula = Average.Daily.Ridership ~ Uber + Season + Labor_perc + 
              poly(Minimum.Temperature,2) + Year + poly(Population,3) + Average.Fare + 
              Season:Minimum.Temperature + Season:Labor_perc + Uber:Year + 
              Uber:Population, data = chicago)

ggplot(chicago, aes(x = Minimum.Temperature, y = final$residuals)) + geom_point() + geom_hline(yintercept = 0, color ="red") + geom_smooth()
ggplot(chicago, aes(x = Population, y = final$residuals)) + geom_point() + geom_hline(yintercept = 0, color ="red") + geom_smooth()


## Predicting Uber results

newage <- seq(from=-,to=27,by=.5)
newdata <- data.frame(matrix(0, nrow=length(newage), ncol = 7))
names(newdata) <- c("treat","agec", "educc", "black", "re74c", "zero", "newed")
newdata$agec <- newage; newdata$treat = 'no training'; newdata$re74c = 0; newdata$black = 'not black'; newdata$newed = 'less than 9'
newdata$educc <- 0
newdata$age2 <- newdata$agec^2
newdata$age3 <- newdata$agec^3
newdata$zero <- 'not zero'
newdata$agec <- as.numeric(newdata$agec)

newdata$educc <- as.numeric(newdata$educc)

preds_no_train <- predict(final_model, newdata, type = "response", se.fit = TRUE)
newdata$zero <- 'zero'
preds_no_train_zero <- predict(final_model, newdata, type = "response", se.fit = TRUE)
newdata$zero <- 'not zero'
newdata$treat <- 'training' 
pred_train_pos <- predict(final_model, newdata, type = "response", se.fit = TRUE)
newdata$zero <- 'zero'
pred_train_zero <- predict(final_model, newdata, type = "response", se.fit = TRUE)

plot(y=preds_no_train$fit,x=newage,xlab="Age (Centered)",ylab="Positive Income?",
     main="Expected Change in Probability of Positive Income with Age",col="darkblue",ylim=c(0,1), type = "l", lwd = 2)
points(y=pred_train_pos$fit, x=newage,col="orange", pch = 19, type = "l", lwd = 2)
points(y=preds_no_train_zero$fit, x=newage, col="green", pch = 19, type = "l", lwd = 2)
points(y=pred_train_zero$fit, x=newage, col="red", pch = 19, type = "l", lwd = 2)
legend("bottomleft",c("No Training (positive in 74)", "No Training (zero in 74)", "Training (positive in 74)","Training (zero in 74)"),col=c("darkblue","green", "orange", "red"),lty=c(2,2))

invisible(roc(jobsl$positive,fitted(no_hisp),plot=T,legacy.axes=T, print.thres = 'best', col="red3"))
invisible(roc(jobsl$positive,fitted(final_model),plot=T,legacy.axes=T, col="blue3",add=T))
legend('bottomright', c('No Hispanic', 'Polynomial'),lty=c(1,1),
       lwd=c(2,2),col=c('red3', 'blue3'))
