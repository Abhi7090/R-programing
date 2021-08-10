

 ### Importing  Dataset 
covid_data_kerala <- read.csv("D:/Abhi reddy/NMIT M.tech/covid_data_kerala.csv")
View(covid_data_kerala)
head(covid_data_kerala)
nrow(covid_data_kerala)
ncol(covid_data_kerala)
sum(is.na(covid_data_kerala))
summary(covid_data_kerala)

library(DataExplorer)
covid_data_kerala$Date = as.character(covid_data_kerala$Date)
plot_missing(covid_data_kerala)
plot_str(covid_data_kerala)
plot_histogram(covid_data_kerala)
plot_density(covid_data_kerala)
plot_correlation(covid_data_kerala, type = 'continuous','Date')
boxplot(covid_data_kerala$Confirmed,
        main = "Covid-19 Cases in Kerala",
        xlab = "Boxplot",
        ylab = "Confirmed cases",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
boxplot(covid_data_kerala$Recovered,
        main = "Covid-19 Cases in Kerala",
        xlab = "Boxplot",
        ylab = "Recovered cases",
        col = "Blue",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
boxplot(covid_data_kerala$Deceased,
        main = "Covid-19 Cases in Kerala",
        xlab = "Boxplot",
        ylab = "Deceased cases",
        col = "black",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
covid_data_kerala[is.na(covid_data_kerala)] = 0
View(covid_data_kerala)
plot_missing(covid_data_kerala)

plot_str(covid_data_kerala)
plot_histogram(covid_data_kerala)
plot_density(covid_data_kerala)
plot_correlation(covid_data_kerala, type = 'continuous','Date')



library(ggplot2)
ggplot(covid_data_kerala, aes(x = Confirmed, y = Recovered)) +
  geom_point()

ggplot(covid_data_kerala, aes(x = Confirmed, y =Deceased)) +
  geom_point()
ggplot(covid_data_kerala, aes(x = log(Confirmed), y = log(Recovered))) +
  geom_line()
my_graph <- ggplot(covid_data_kerala, aes(x = log(Confirmed), y = log(Recovered))) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)

my_graph

ggplot(covid_data_kerala, aes(x = Confirmed, y =Deceased)) +
  geom_qq() +
  geom_qq_line()



      ### After missing values 


is.na(covid_data_kerala)
sum(is.na(covid_data_kerala))
mean(is.na(covid_data_kerala))
a <- na.omit(covid_data_kerala)
a
plot_missing(a)
plot_str(a)
plot_missing(a)
plot_histogram(a)
plot_density(a)
plot_correlation(a, type = 'continuous','Date')

ggplot(a, aes(x = Confirmed, y = Recovered)) +
  geom_point()
ggplot(a, aes(x = Confirmed, y =Deceased)) +
  geom_point()
ggplot(a, aes(x = log(Confirmed), y = log(Recovered))) +
  geom_line()
my_graph <- ggplot(a, aes(x = log(Confirmed), y = log(Recovered))) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)

my_graph
my_graph <- ggplot(a, aes(x = log(Confirmed), y = log(Deceased))) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)

my_graph

install.packages("GGally")
library(GGally)
vars <- c("Confirmed","Recovered","Deceased")

ggpairs(covid_data_kerala[vars])

