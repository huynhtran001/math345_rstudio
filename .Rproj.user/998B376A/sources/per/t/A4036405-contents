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

print("Welcome! If you'd like to generate the graphs, please call the following variable names: revenueGraph, europePie, latinPie, asiaPie, or chinaPie")
