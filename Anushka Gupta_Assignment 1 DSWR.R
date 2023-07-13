install.packages("imager")
install.packages("stringr")

library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)






 
 
 ######################question 3
 tennis<- function(p)
 {
   A<-0
   B<-0
   for (i in 1:5)
   {
     other_game <- sample(0:1, size=1, prob=c(p,1-p))
     
     if (other_game ==1){ 
       A <- A+1}
   
   if (other_game ==0){
     B<-B+1
   }
   
   if( B==3 || A==3)
   {
     x <- i
     break
   }
   }
return (x)}
 
 
 
 
 matches <- numeric(length=1000)
 for(i in 1:1000)
 {
   matches[i] <- tennis(0.7)
 }
 ans <- mean(matches)
 ans
 
 
 ##################question 4
 
 doors <- c(1,2,3)
 corr_door <- sample(d=doors,size=1)
 first_door <-1
 if (corr_door== first_door)
     {
        
    open_door <- sample (d=doors[-c(first_door)],size=1)
   
     }
 else {
   open_door<- doors[-c(first_door,coor_door)]
 }
 last_door<- doors[-c(first_door, open_door)]
 last_door<- last_door
 if (last_door== corr_door)
    {
       print("1")
     }
 else
   {print("0")
   }
 
 
 
 
 win<-0
 for(i in 1:1000){
   corr_door<- sample(doors,1)
   first_door<-1
   if (corr_door== first_door)
   {
      open_door <- sample (d=doors[-c(first_door)],size=1)
     
   }
   else {
     open_door<- doors[-c(first_door,coor_door)]
   }
   last_door<- doors[-c(first_door, open_door)]
   last_door<- last_door
   if (last_door== corr_door)
   {
      win<-win+1
   }
 }
 print (round(win/1000, 2))
 
 
 
 
 
 
 
 ########################question 5
 library(rvest)
 
 html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-rightnow/")
 
all_data <- html %>% html_table()
 
length(all_data)

all_data[[1]]
all_data[[2]]
all_data[[3]]
all_data[[4]]

   

 
 
 
 
 
 
 
   

