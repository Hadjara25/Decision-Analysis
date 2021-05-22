library(dplyr)
install.packages("ggplot2")
library(ggplot2)
participants_data <- read.csv("C:/Users/hp/Dropbox/My PC (DESKTOP-0VMGK23)/Documents/R project/teaching_R-master/teaching_R-master/participants_data.csv")
participants_barplot <- table(participants_data$gender)
barplot(participants_barplot)

#academic parents####

participants_barplot <- table(participants_data$academic_parents)
barplot(participants_barplot)

library(ggplot2)
ggplot(data=participants_data,
       aes(x = age,
           y=number_of_siblings)) +
  geom_point()

#days and email####

ggplot(data=participants_data,
       aes(x=letters_in_first_name,
           y=days_to_email_response))+
  geom_point()

#add color and size####
ggplot(data=participants_data,
       aes(x=age,
           y=batch,
           color=gender,
           size=number_of_siblings)) +
  geom_point()


ggplot(data = participants_data,
       aes(x=letters_in_first_name,
           y=days_to_email_response,
           color=academic_parents,
           size=working_hours_per_day))+
  geom_point()
#irisdata####

ggplot(data = iris,
       aes(x = Sepal.Length,
           y = Petal.Length,
           color = Species,
           size =  Petal.Width))+
  geom_point()


ggplot(data = iris,
       aes(x =Sepal.Width,
           y = Sepal.Length,
           color = Species,
           size = Petal.Length)) +
  geom_point()

#diamond_data####
dsmall <- top_n(diamonds, n = 100)

ggplot(data = diamonds,
       aes(x = carat,
           y = price,
           color = color)) +
  geom_point()


plot1<- ggplot(data = diamonds,
               aes(x= carat,
                   y = price,
                   alpha = 0.2))+
  geom_point()

library(tidyverse)

# log###
ggplot(data = diamonds,
       aes(x =log(depth),
           y = log(table),
           alpha = 0.2))+
  geom_point()

#colors and shape####

dsmall <- top_n(diamonds, n=100)
ggplot(data=dsmall,
       aes(x= carat,
           y= cost,
           color= color)) +
  geom_point()


#40 rows####

dsmall<- top_n(diamonds, n=40)
ggplot(data=dsmall,
       aes(x= carat,
           y= cost,
           shape=cut))+
  geom_point()


#set_parameters####
ggplot(data=diamonds,
       aes(x=carat,
           y=cost,
           alpha= I(0.1),
           color = I("blue")))+
  geom_point()


#geom_option####
dsmall <- top_n(diamonds, n= 50)
ggplot(data=dsmall,
       aes(x=carat,
           y=cost))+
  geom_point()+
  geom_smooth()

#boxplot####
ggplot(data=diamonds,
       aes(x=cut,
           y=cost))+
  geom_boxplot()


#jitterpoints####
ggplot(data=diamonds,
       aes(x=cut,
           y=cost/carat))+
  geom_boxplot()+
  geom_jitter()

#alpha+boxplot
ggplot(data=diamonds,
       aes(x=cut,
           y=cost/carat,
           alpha= I(0.1)))+
  geom_boxplot()+
  geom_jitter()

#geom_histograms
ggplot(data=diamonds,
       aes(x=carat,
           color=color,
           alpha= I(0.3)))+
  geom_density()

#subset
ggplot(data= mpg,
       aes(x= displ,
           y= hwy,
           color= manufacturer))+
  geom_point()+
  geom_smooth(method = "lm")

