library(ggplot2)

myData = read.csv("region-revenue.csv")

# data frame set up
# repeats titles 4 times
title = rep(c(myData[, 1]), 4)

# lines up regions to match with movie titles into a variable
# called region
Region = c(rep(c("Europe"), 10),
           rep(c("Latin America"), 10),
           rep(c("Asia Pacific"), 10),
           rep(c("China"), 10))

# combines the regions' revenue into 1 column called revenue
# also divides everything by 1,000,000 to help format graph
europe = c(myData[, 3])
latin = c(myData[, 4])
asia = c(myData[, 5])
china = c(myData[, 6])
revenue = c(europe, latin, asia, china)
revenue = revenue / 1000000

# create new data frame to load into graph using
# the vectors we just created
finalData = data.frame(title, Region, revenue)

# call this variable to populate the graph
revenueGraph = ggplot(finalData, aes(fill = Region, y = revenue, x = title)) +
  geom_bar(width = 0.5, position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Revenue by region", x = "Movie titles",
       y = "Revenue (in millions)")

# -------------------------------------------------
# Section for genre pie charts
# Function to reuse generic pie chart set up
pieSetup = function(dataFrame, titles) {
  genre = dataFrame[, 1]
  val = dataFrame[, 2]
  return(ggplot(dataFrame, aes(x="", y=val, fill = genre)) +
          geom_bar(stat="identity", width = 1) +
          geom_text(aes(x = 1.3,label=sprintf("%1.1f%%", val*100)),
                    position = position_stack(vjust = 0.5)) +
           theme(plot.title = element_text(hjust=0.5),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank()) +
           labs(fill = "Genres",
                x = NULL,
                y = NULL,
                title = titles) +
           coord_polar("y", start = 0))
}

createFrame = function(path) {
  rawData = read.csv(path)
  genres = rawData[, 1]
  val = rawData[, 2]
  return(data.frame(genres, val))
}

europeFrame = createFrame(path = "genres-europe.csv")
europePie = pieSetup(dataFrame = europeFrame, title = "European region")

latinFrame = createFrame(path = "genres-latin.csv")
latinPie = pieSetup(dataFrame = latinFrame, title = "Latin America")

asiaFrame = createFrame(path = "genres-asia.csv")
asiaPie = pieSetup(dataFrame = asiaFrame, title = "Asia")

chinaFrame = createFrame(path = "genres-china.csv")
chinaPie = pieSetup(dataFrame = chinaFrame, title = "China")

# -------------------------------------------------
# Section for regression
install.packages("corrplot")
library(corrplot)

removeTitles = function(dataFrame) {
  dataFrame = dataFrame[,-1]
  dataFrame = dataFrame[, -3]
  return(dataFrame)
}

makeRegression = function(dataFrame, title) {
  reg <- lm(best_movie_ratings ~ best_revenue, data=dataFrame)
  plot(dataFrame$best_revenue, dataFrame$best_movie_ratings, pch=19,
       xlab = "Revenue (in millions)", ylab = "Rating (out of 10)",
       main = title)
  grid()
  abline(reg, col="red", lwd=1.5)
}

# Scales column down by value
scaleDown = function(dataFrame, value) {
  dataFrame[,2] = dataFrame[, 2] / value
  return(dataFrame)
}

myCor = function(dataFrame, title) {
  corrplot(cor(dataFrame),
           method="number",
           type = "upper",
           title = title)
}

europe_reg = read.csv("europe_revenue.csv")
europe_reg = removeTitles(dataFrame = europe_reg)
europe_reg = scaleDown(dataFrame = europe_reg, value = 1000000)

latin_reg = read.csv("latin_revenue.csv")
latin_reg = removeTitles(dataFrame = latin_reg)
latin_reg = scaleDown(dataFrame = latin_reg, value = 1000000)

asia_reg = read.csv("asia_revenue.csv")
asia_reg = removeTitles(dataFrame = asia_reg)
asia_reg = scaleDown(dataFrame = asia_reg, value = 1000000)

china_reg = read.csv("china_revenue.csv")
china_reg = removeTitles(dataFrame = china_reg)
china_reg = scaleDown(dataFrame = china_reg, value = 1000000)

#myCor(europe_reg, "Europe")
#myCor(asia_reg, "Asia Pacific")
#myCor(china_reg, "China")
#myCor(latin_reg, "Latin America")

print("Welcome! If you'd like to generate the graphs, please call the following variable names: revenueGraph, europePie, latinPie, asiaPie, or chinaPie")
print("If you want to see the regression of a dataset, call the function makeRegression and pass it a data frame and title")
