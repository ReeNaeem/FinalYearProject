Olympics <- read.csv("Olympics.csv",  header = TRUE, sep = ",")
View(Olympics) #in the data set, 0 = no medal, 1 = bronze, 2 = silver, 3 = gold

test1 <- subset(Olympics, Year==2016)
View(test1)
