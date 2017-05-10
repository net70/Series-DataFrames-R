setwd("C:\\Users\\nathanielma\\Downloads")

# Q1
x  = 1:10
sqrtOfSum = sqrt(sum(1:(2*length(x)-1)))

# Q2
n = 2**c(0:as.integer(log2(1024)))
y = sum((3*(1:20) + (2*(1:20))**2))

# Q3
da = sqrt(sum((c(4,3) - c(1.25,1.75))**2))
db = sqrt(sum((c(1.5,5,2,3,0)-c(2,1.5,4,3,5))**2))

# Q4
surveydata = data.frame("id" = 1:6, 
                        "age" = c(30,21,50,48,25,NA),
                        "gender" = c('M','F','M','M','M','F'),
                        "marital" = c("single", "married", "married", "single", "divorced", "single"),
                        "bodys" = c("fat", "thin", "medium", "medium", "medium", "medium"),
                        "height" = c(170,164,168,180,195,177))
surveydata[3,]
subset(surveydata, gender == 'F')
subset(surveydata, (id != 2) & (id != 5))
subset(surveydata, select = c("gender", "marital"))
subset(surveydata, height > 175)
mean(surveydata$age, na.rm = TRUE)

# Q5
beerPref = read.csv("Beer.csv")
beerTable = prop.table(table(beerPref$Beer_Preference))*100
par(mfrow = c(1,2))

barplot(beerTable, 
        main = "Beer Preferences 1",
        xlab = "Beer Type",
        ylab = "Percentile",
        names.arg=c("Dark", "Light", "Regular"),
        col = "blue"
        )
                    
barplot(beerTable, 
        main = "Beer Preferences 2",
        horiz = TRUE,
        xlab = "Beer Type",
        ylab = "Percentile",
        names.arg=c("Dark", "Light", "Regular"),
        col = "purple"
        )

tab1 = table(beerPref$gender, beerPref$Beer_Preference)
tab1[2,1]/sum(tab1[,1])*100 # percentage of male students who perfer dark beer.

bTable = prop.table(table(beerPref$Beer_Preference, beerPref$gender), margin = 2)
barplot(bTable,
        main = "Beer Pref By Gender",
        xlab = "Gender",
        ylab = "Beer Type Preference % ",
        col = c("red", "purple", "blue"), 
        legend = rownames(bTable),
        args.legend= list(x = "topright"))

# Q6
cars = read.csv("cars.csv")

nrow(cars) # Number of cars in the DF
colnames(cars) # Fields of DF cars

par(mfrow = c(1,2))

hist(cars$mpg, main = "MPG Histogram", col = "orange", ylim = c(0,70))

boxplot(cars$mpg, 
        main = "MPG Boxplot", 
        col = "red",
        xlab = "Counts",
        ylab = "cars$mpg", 
        ylim = c(10,50))

boxplot(cars$mpg~cars$brand,
        main = "MPG by Brand Vertical",
        ylab = "MPG",
        xlab = "Brand",
        names = c("US", "Europe", "Japan"),
        col = c("red", "purple", "blue"))

boxplot(cars$mpg~cars$brand,
        main = "MPG by Brand Horizontal",
        ylab = "Brand",
        xlab = "MPG",
        names = c("US", "Europe", "Japan"),
        col = c("red", "purple", "blue"),
        horizontal=TRUE)

par(mfrow = c(2,2))
colnames(cars)
plot(cars$mpg, cars$hp, main = "MPG & HP", col = "red")
plot(cars$mpg, cars$timeto60, main = "MPG & Acceleration", col = "purple")
plot(cars$mpg, cars$weightlbs, main = "MPG & Weight", col = "blue")

pairs(~mpg+hp+timeto60+weightlbs, cars, 
      main = "MPG Scatterplot Matrix", 
      col="darkslategray4")

# Q7
passen = read.csv("passengers.csv")
plot(pass~time, data = passen, 
     type = "b", 
     main = "Passengers Line Graph", 
     col = "darkslategray4")
