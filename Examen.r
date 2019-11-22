library(tidyverse)

# 1.- Create a numeric vector that contains the eight planets of our solar system. Then, create another vector 
# containing the name for each planet (Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, and Neptune).
# After that, you should assign the number of the planet and its respective name.   
# Finally, you should select and print the planets related to the names Earth, Mars, and Jupiter.

planetsNum <- c(1, 2, 3, 4, 5, 6, 7, 8)
planetsNames <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
names(planetsNum) <- planetsNames
#planetsNum[3:5]
threeplanets <- planetsNum[3:5]
threeplanets



# 2.- Construct a matrix with 10 rows containing the numbers 1 up to 50, filled row-wise.

sequenceMatrix <- matrix(1:50, nrow = 10, byrow = TRUE)
sequenceMatrix



# 3.- Create a factor with these observations "Breakfast", "Breakfast", "Breakfast", "Luch", "Luch", 
#"Dinner", "Dinner", "Dinner", "Collation", "Collation", "Collation" and print a summary.

foodTime <- c("Breakfast", "Breakfast", "Breakfast", "Luch", "Luch", "Dinner", "Dinner", "Dinner", "Collation", "Collation", "Collation")
foodTimeFactor <- factor(foodTime)
foodTimeFactor
summary(foodTimeFactor)



# 4.- Create and print a data frame that should contain:
#5 of your courses (e.g. Fundamentals of Programming, Object-Oriented Programming, Databases, etc ).
#5 of your marks
#5 of your professors' names

Courses <- c("Databases", "Technological Innovation", "Object-Oriented Programming", "Software Architecture", "Information Security I")
Marks <- c(9.4, 8, 8.9, 10, 9.7)
Professors <- c("Rogelio Florencia", "Abraham Lopez", "Gilberto Rivera", "Lucero Zamora", "Maritza Varela")
classesDataFrame <- data.frame(Courses, Marks, Professors)
classesDataFrame



# 5.- Load the dataset labeled as odb.csv that is located in the data folder and storage this dataset in a variable 
# called od. Then, you should describe the composition of the dataset. This description must contain the number of 
# observations, variables and the type of each variable.

od <- read_csv("data/odb.csv")
str(od)
# Description: This dataset contains 548 observations and 35 variables, of which 29 a numeric variables and 
# 6 character variables. The next list groups all the variables by their type:
#   NUMERIC VARIABLES                                                   CHARACTER VARIABLES 
#     1. number                                                           1. Economy
#     2. year                                                             2. Code
#     3. IG_High_income                                                   3. Region
#     4. IG_Upper_middle_income                                           4. ISO2
#     5. IG_Lower_middle_income                                           5. ISO3
#     6. IG_Low_income                                                    6. Income group
#     7. GEDIRank                                           
#     8. GEDIScore                                          
#     9. OGP                                                
#     10. ODB_Rank                                           
#     11. GODIRank                                           
#     12. GODIScore                                          
#     13. EFScore                                            
#     14. EF_Property Rights                                 
#     15. EF_Freedom from Corruption                         
#     16. GDP_PPP                                            
#     17. Technological_adoption                             
#     18. Individuals_using_Internet                         
#     19. GCI_Individuals using Internet                     
#     20. GCI_Fixed broadband Internet subscriptions/100 pop 
#     21. GCI_Fixed broadband Internet subscriptions/100 pop.
#     22. GCI_Internet bandwidth, kb/s per user_1            
#     23. GCI_Internet bandwidth, kb/s per user              
#     24. GCI_Mobile broadband subscriptions/100 pop         
#     25. GCI_Mobile broadband subscriptions/100 pop_2       
#     26. GCI_12th pillar: Innovation.x                      
#     27. GCI_12th pillar: Innovation                        
#     28. GCI_Innovation and sophistication factors          
#     29. GCI_Innovation and sophistication factor



# 6.- Filter the od dataset to retrieve only the observation from Mexico, grouped by year and in descending 
# order selecting the "ODB_Rank column

od %>% 
  filter(Economy == "Mexico") %>% 
  group_by(year) %>%
  arrange(desc(ODB_Rank))



# 7.- Create a new variable called od_year grouping per "year" and "Income group" variables. Then, remove 
#NAs observations. Later, using the function summarize() you should estimate the median of the GDP_PPP variable 
#and store it in a variable called "medianValue"

od_year <- od %>%
  group_by(year, `Income group`) %>%
  na.omit() %>%
  summarize(medianValue = median(GDP_PPP))



# 8.-Create a line plot graph to visualize trends over time. You should use the variables created in the od_year data 
#frame using Income group as color

ggplot(od_year, aes(x = year, y = medianValue, color = `Income group`)) + geom_line()



# 9.- Create a new variable called od_latin filtering per Region, selecting Latin America & Caribbean and grouping 
# per Income group.
#Then, using this data frame, create a plot visualizing the relationship between Individuals_using_Internet and 
#GDP_PPP colored per Income group and faceting per Economy

od_latin <- od %>%
  filter(Region == "Latin America & Caribbean") %>%
  group_by(`Income group`)
ggplot(od_latin, aes(x = GDP_PPP, y = Individuals_using_Internet, color = `Income group`)) + geom_point() + 
  facet_wrap(~Economy)



# 10.- Create a new variable called od_2016 filtering the year 2016 and removing observations that contain NAs.
#Then, using this data frame, create a plot visualizing the relationship between Technological adoption and GDP_PPP,
#colored per Income group. Furthermore, you should use a log scale in both axes ("x" and "y"). Finally, you should use 
#the facet function in order to wrap by Region.

od_2016 <- od %>%
  filter(year == 2016) %>%
  na.omit() 
ggplot(od_2016, aes(x = GDP_PPP, y = Technological_adoption, color = `Income group`)) + geom_point() + 
  scale_x_log10() + scale_y_log10() + facet_wrap(~Region)
