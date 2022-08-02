# Project: MOVIE THEATER Simulation Project.

# this script simulates a movie theater by randomizing the number of adults and children visitors to watch five movies, 
# with five regular screens available and 50 seats per theater.
# there are five main features of this movie theater: 
# 1. It accounts for the weekend days where more visitors are expected.
# 2. It provides four types of snacks for visitors 
# 3. It can give a 10% discount.
# 4. It offers 3 VIP screens for visitors.
# 5. It offers FREE tickets to lucky visitors chosen randomly!

# this script includes five functions -------
# movie_theater_simu(): simulation for a week revenue with regular screens.
# discount_ticket(): simulation with a week with a 10% dicount.
# snacks_func(): simulation for snacks reveune in a week.
# vip_movie_theater_simu(): simulation for a week revenue with VIP screens.


# Authors: NOUF ALJOHANI, Amal Almutairi, Salha Nasser, Rawan Alsudias, & Rahaf Alzaharani
# Last Update : Sat, 23 Jul, 2022,

#---------------------------------------------------------------------------------------------------

# Install packages.
install.packages("wesanderson")
# Load
library(wesanderson)
library("RColorBrewer")
#display.brewer.all()


# Define all important variables.
# Cost for adults and children
ticket_cost <<- 14 
ticket_cost_child <<- 8 
movies <<- c('Minions 2', 'Top Gun', 'Encanto', 'Paws of Fury', 'Memory')  # List 5 of your favorite movies
screens <<- 5 # How many screens does the theater have? (assume 1 per movie)
seats <<- 50 # How many seats does each theater hold
week <<- c("Sun", "Mon", "Tue", "Wed","Thu", "Fri", "Sat")
week_days_discount <<- rep(0,7)
visitors_adults_in_week_days <<- rep(0,7)  # Store adults visitors totals for each day
visitors_children_in_week_days <<- rep(0,7) # Store children visitors totals for each day
discount <<- 0.1
snacks <- c("Popcorn"= 5.5, "Nachos"= 7, "Hotdogs"= 6, "Candy"=4)


#=======================================================================================================
#function 1: 

#this function simulates a movie theater by randomizing the number of visitor, to get the revenue of a week
movie_theater_simu <- function() { 
  
  week_days <<- rep(0, 7)  # Store totals for each day
  r_mat <<- matrix(nrow=7, ncol=5) # to keep track of all revenue (by movie and day)
  m_mat <<- rep(0,5) # matrix to keep track of total movies revenue
  adult_m <<- rep(0,5)
  child_m <<- rep(0,5)
  
  adult_mat <<- matrix(nrow=7, ncol=5) # to keep track of adults visitors (by movie and day)
  child_mat <<- matrix(nrow=7, ncol=5) # to keep track of adults visitors (by movie and day)
  
  lucky_visitor <<- lucky_visitors()
  
  ######## function 
  # iterate through the week
  for (i in 1:length(week_days)) {
    
    # Keep track of total revenue for the day
    #total_revenue <- 0
    # iterate through the amount of screens on a particular day
    for (j in 1:screens) {
      if (i == 6  || i == 7 ){#here we increase the probability of having more people in the weekends 
        # Calculate  how many adults and children are watching the movie
        visitors_adults <<- sample(1:seats, size=1,prob = c(rep(0,2,seats-5),0.9,0.9,0.9,0.9,0.9))
        
        visitors_children <<- sample(1:(seats-(visitors_adults)),size=1)
      }
      else{
        # Calculate  how many adults and children are watching the movie
        visitors_adults <<- sample(1:seats, 1)
        visitors_children <<- sample(1:(seats-(visitors_adults)),1)
      }
      
      # Calculate the revenue for adults and children
       # if there are lucky visitors -- > only TWO TICKETs are FREE
        if (i * j == lucky_visitor) {
          l_visit <- visitors_adults - 2
          
          revenue <- (ticket_cost * l_visit ) + (ticket_cost_child * visitors_children)
        }
      else { 
        # calculate revenue like usual.
        revenue <- (ticket_cost * visitors_adults ) + (ticket_cost_child * visitors_children)
        }
       
        # Calculate the revenue for adults and children
        revenue_of_adults <- visitors_adults * ticket_cost
        revenue_of_children <- visitors_children * ticket_cost_child
        
      # Calculate revenue, and add to running total for the day
      r_mat[i,j] <<- revenue # for each screen
      adult_mat[i, j] <<- visitors_adults
      child_mat[i,j] <<- visitors_children
    }
    # Save total to the corresponding day
    week_days[i] <<- sum(r_mat[i, ])
    total_visitors[i] <<- sum(visitor)
    # total_revenue <- week_days + snacks
  }
  # to get the sum of movies columns. [revenue, and attendees].
  for (i in 1:5) {
    m_mat[i] <<- sum(r_mat[ , i])
    adult_m[i] <<- sum(adult_mat[ , i])
    child_m[i] <<- sum(child_mat[ , i])
  }
  
}


#=======================================================================================
# Function 2:
#this function simulates a week in the theater with 10% discount.

discount_ticket <- function() {
  for (i in 1:length(week_days_discount)) {

    # Keep track of total revenue for the day
    total_revenue_discount <-0
    total_visitors_adults_a_day <- 0
    #total children visitors for each day
    total_visitors_children_a_day <- 0
    
    # iterate through the amount of screens on a particular day
    for (j in 1:screens) {
      if (i == 6  || i == 7 ){#here we increase the probability of having more people in the weekends
        # Calculate  how many adults and children are watching the movie
        visitors_adults_discount <<- sample(1:seats, size=1,prob = c(rep(0,2,seats-5),0.9,0.9,0.9,0.9,0.9))
        
        visitors_children_discount <<- sample(1:(seats-(visitors_adults)),size=1)
      }
      else{
        visitors_adults_discount <<- sample(1:seats, 1)
        visitors_children_discount <<- sample(1:(seats-(visitors_adults)),1)
      }
      
      # Calculate the revenue for adults and children
      revnue_adults <-((ticket_cost - (ticket_cost*discount)) * visitors_adults_discount)
      revnue_children <- ((ticket_cost_child - (ticket_cost_child*discount)) * visitors_children_discount)
      
      # Calculate revenue, and add to running total for the day
      total_revenue_discount <- total_revenue_discount+revnue_adults+revnue_children
      total_visitors_adults_a_day <- total_visitors_adults_a_day + visitors_adults_discount
      total_visitors_children_a_day <- total_visitors_children_a_day + visitors_children_discount
    }
    # Save total to the corresponding day
    week_days_discount[i] <<- total_revenue_discount
    
    visitors_adults_in_week_days[i] <<- total_visitors_adults_a_day
    visitors_children_in_week_days[i] <<- total_visitors_children_a_day
  }
  
}

#==================================================================================================
#Function 3
# this function simulates the snacks revenue of the movie theater in a week
snacks_func <- function() { 
  snack_reveune <<- matrix(nrow=7, ncol=4)# to keep track of all revenue (by snack and day)
  snacks_week <<- rep(0,7) #Store totals for each day
  s_mat <<- rep(0,4) # to keep track of total snacks revenue
  # Calculate snacks revenue 
  for (d in 1:length(week_days)) {# a loop that goes through all 7 days 
    #snacks_week <- 0
    for (s in 1:length(snacks)) {  # a loop that goes through all 4 snacks
      
      selected_snacks <- snacks[[s]] * (sample(1:(total_visitors[d]*4), 1))# this variable is for storing the snack price multiplied by a random number of people
      
      # store the snack revenue
      snack_reveune[d,s] <<- selected_snacks 
      
    }
    #to get the sum of each day. 
    snacks_week[d] <<- sum(snack_reveune[d, ])
    
  } 
  # to get the sum of each snack.
  s_mat <<- colSums(snack_reveune)
  
}


#===========================================================================================================
#function 4: 
#this function is to simulate a VIP revenue and visitors

vip_movie_theater_simu <- function() { 
  
  # Cost for VIP adults and children
  vip_ticket_cost <<- 20 
  vip_ticket_cost_child <<- 14
  movies <- movies  # List 5 of your favorite movies
  vip_screens <- 3   # How many VIP screens does the theater have? (assume 1 per movie)
  vip_seats <<- 25 # How many VIP seats does each theater hold
  vip_week_days <<- rep(0, 7)  # Store totals for each day
  
  #vars to use in the second graph (VIP)   
  vip_visitors_adults_in_week_days <<- rep(0, 7)  # Store adults visitors totals for each day
  vip_visitors_children_in_week_days <<- rep(0, 7)  # Store children visitors totals for each day
  ############### 
  
  # iterate through the week
  for (day in (1:length(vip_week_days))) {
    
    # Keep track of total revenue for the day (VIP)
    day_vip_revenue <- 0
    
    ###############
    #vars to use in the second graph
    #total VIP adults visitors for each day
    total_vip_visitors_adults_a_day <- 0
    #total VIP children visitors for each day
    total_vip_visitors_children_a_day <- 0
    ###############
    
    # iterate through the amount of VIP screens on a particular day
    for (s in (1:vip_screens)) {
      
      # Calculate how many VIP adults and VIP children are watching the movie
      
      if (day == 6 || day == 7){#here we increase the probability of having more people in the weekends 

        vip_visitors_adults <<- sample(1:vip_seats, size=1,prob = c(rep(0,2,vip_seats-5),0.9,0.9,0.9,0.9,0.9))
        vip_visitors_children <<- sample(1:(vip_seats-(vip_visitors_adults)),size=1)
      }
      else{
         
         vip_visitors_adults <- sample(vip_seats, 1)
         vip_visitors_children <- sample((vip_seats - vip_visitors_adults),1)
      }
      
      # Calculate the revenue for adults and children (VIP)
      revenue_of_vip_adults <- vip_visitors_adults * vip_ticket_cost
      revenue_of_vip_children <- vip_visitors_children * vip_ticket_cost_child
      
      # Calculate revenue, and add to running total for the day (VIP)
      
      vip_adults_and_children_revenue <- revenue_of_vip_adults + revenue_of_vip_children
      
      day_vip_revenue <- day_vip_revenue + vip_adults_and_children_revenue
      
      ###############
      #for the second graph, add VIP adult visitors to its total var, do the same to the VIP children visitors
      total_vip_visitors_adults_a_day <- total_vip_visitors_adults_a_day + vip_visitors_adults
      total_vip_visitors_children_a_day <- total_vip_visitors_children_a_day + vip_visitors_children
    }
    
    # Save total to the corresponding day
    vip_week_days[day] <<- day_vip_revenue
    
    ###############
    #for the second graph
    vip_visitors_adults_in_week_days[day] <<- total_vip_visitors_adults_a_day
    vip_visitors_children_in_week_days[day] <<- total_vip_visitors_children_a_day
    
  }
}

# function 5: 
# function for lucky visitors that generates a random number from the available seats. 
lucky_visitors <- function () {
  return (sample(seats, 1))
  
}


#==========================================================================================
#function calls 
movie_theater_simu()
discount_ticket()
snacks_func()
vip_movie_theater_simu()


### END of Simulation

#---------------------------------------------------
### Answer
# Which day had the highest revenue? 
# The day with the highest revenue is Tuesday.

sprintf('The day with the highest revenue was %s with $%d',  week[which.max(week_days)],  max(week_days))

#////////////////////////////////////////////////////////////////////////////////////////////////////////////

# It's handy to create dataframe, it's great to understand the business as well to represent data in graph
regular_data <- data.frame(total_revenue_per_day = week_days,
                           adults_visitors = visitors_adults_in_week_days,
                           children_visitors = visitors_children_in_week_days,
                           adults_revenue = visitors_adults_in_week_days * ticket_cost,
                           children_revenue = visitors_children_in_week_days * ticket_cost_child)

#dataframe for Vips
vip_data <- data.frame(vip_total_revenue_per_day = vip_week_days,
                       vip_adults_visitors = vip_visitors_adults_in_week_days,
                       vip_children_visitors = vip_visitors_children_in_week_days,
                       vip_adults_revenue = vip_visitors_adults_in_week_days * vip_ticket_cost,
                       vip_children_revenue = vip_visitors_children_in_week_days * vip_ticket_cost_child)

# datafeame for the movie theater revenue in a week.
total_revenue_df = data.frame(
  Regular_screens_revenue = week_days,
  VIP_screens_revenue = vip_week_days,
  snacks_revenue = snacks_week, 
  total_revenue_theater= week_days + vip_week_days + snacks_week)
rownames(total_revenue_df) <- week
#//////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#############################################Graphs#########################################################

bb <- barplot(week_days,xlab="Day of Week",ylab ='Total Revenue ($)',main='Total Tickets Revenue per Day', 
        names.arg= week, col=brewer.pal(n = 7, name = "Spectral"),
        cex.names= par("cex.axis"), cex.axis= par("cex.axis"), beside=TRUE,
        ylim=range(pretty(c(0, week_days))), cex.lab=1.2, font.main = 2)
text(x=bb, y=week_days,labels =paste0('$', week_days), cex=0.8,pos=3, xpd=TRUE)

pie_labels <- paste0(round(100 * m_mat/sum(m_mat), 2), "%")
colors = brewer.pal(n = 6, name = "Spectral")
pie(m_mat, labels = pie_labels, col=colors, main='Movies Revenue Distribution in a Week' )
legend("topleft", legend = movies, fill= colors)


#discount WEEK!
bb2<-barplot(week_days_discount,xlab="Day of Week",ylab ='Total Revenue ($)',main='Total Revenue per Day with Discount', 
        names.arg = week, col=brewer.pal(n = 7, name = "Spectral"),
        cex.names= par("cex.axis"), cex.axis= par("cex.axis"), beside=TRUE,
        ylim=range(pretty(c(0, week_days))), cex.lab=1.2, font.main = 2)
text(x=bb2, y=week_days_discount,labels =paste0('$', week_days_discount), cex=0.8,pos=3, xpd=TRUE)

#"#D7191C"  "#FFFFBF" "#ABDDA4" "#2B83BA
# number of visitor (adults and children )
visitors <- t(regular_data[c('adults_visitors','children_visitors')])
visitors_type_per_day_chart <- barplot(visitors,
                                       beside = TRUE,
                                       col=c("#FDAE61", "#2B83BA"),
                                          names.arg = week,
                                         main="Visitors Number per Age per Day",
                                         ylab="Number of Visitors",
                                         xlab="Day of Week",
                                       ylim=range(pretty(c(0, visitors))))
text(x=visitors_type_per_day_chart, y=visitors,labels =visitors, cex=0.8,pos=3, xpd=TRUE) 
legend(1, 250, legend =  c("Adults visitors", "Children visitors"), fill= c("#FDAE61", "#2B83BA"))

# Make a bar chart showing total revenue per day of VIP visitors
total_vip_revenue_per_day_chart <- barplot(vip_week_days,
                                           col=brewer.pal(n = 7, name = "Spectral"),
                                      names.arg = week,
                                       main="Total Revenue Earned each Day for a Movie Theater (VIP)",
                                       ylab="VIP Revenue in $",
                                       xlab="Day of Week",
                                      ylim=range(pretty(c(0, vip_week_days))))
text(x=total_vip_revenue_per_day_chart, y=vip_week_days,labels =paste0('$', vip_week_days), cex=0.8,pos=3, xpd=TRUE)

# Make a bar chart to compare between vip adults visitors and vip adults visitors in each day
vip_visitors <- t(vip_data[c('vip_adults_visitors','vip_children_visitors')])
vip_visitors_type_per_day_chart <- barplot(vip_visitors,
                                       beside = TRUE,
                                       col=c("#FDAE61", "#2B83BA"),
                                       names.arg = week,
                                       main="VIP Visitors Number per Age per Day",
                                       ylab="Number of VIP Visitors",
                                       xlab="Day of Week",
                                       legend = c("VIP adults visitors", "VIP children visitors"),
                                       ylim = c(0,75))
text(x=vip_visitors_type_per_day_chart, y=vip_visitors,labels =vip_visitors, cex=0.8,pos=3, xpd=TRUE) 


# Make a barchart to compare between vip adults revenue and vip adults revenue in each day
vip_revenue <- t(vip_data[c('vip_adults_revenue','vip_children_revenue')])
vip_revenue_per_type_per_day_chart <- barplot(vip_revenue,
                                          beside = TRUE,
                                          col=c("#FDAE61", "#2B83BA"),
                                          names.arg = week,
                                          main="Revenue per Visitors Age per Day (VIP)",
                                          ylab="VIP Revenue in $",
                                          xlab="Day of Week",
                                          ylim = c(0,1500))
legend(1, 1500, legend = c("VIP adults revenue", "VIP children revenue"), fill= c("#FDAE61", "#2B83BA"))
text(x=vip_revenue_per_type_per_day_chart, y=vip_revenue,labels =paste0('$', vip_revenue), cex=0.8,pos=3, xpd=TRUE) 
#------------------------------------------------------------------------------------------
#make a graph to compare between vip revenue and regular revenue of the total revenue

#first calculate the total revenue
total_revenue <- vip_data$vip_total_revenue_per_day + regular_data$total_revenue_per_day

#second calculate the percentage the vip revenue and the regular revenue revenue of the total revenue
revenue_vip <- (vip_data[c('vip_total_revenue_per_day')] *100 /total_revenue)
revenue_regular <- (regular_data[c('total_revenue_per_day')] *100 /total_revenue)

#var to hold them both  
revenues <- t(cbind(revenue_vip,revenue_regular))
revenues_rounded <- round(revenues,1)

vip_revenue_per_type_per_day_chart <- barplot(revenues,
                                              beside = TRUE,
                                              col=c("#FDAE61", "#2B83BA"),
                                              names.arg = week,
                                              main="Revenue Percentage per Visitors Type per Day",
                                              ylab="Revenues Percentage",
                                              xlab="Day of Week",
                                              ylim = c(0,100))
legend(1, 100, legend = c("VIP revenue", "Regular revenue"), fill= c("#FDAE61", "#2B83BA"))
text(x=vip_revenue_per_type_per_day_chart, y=revenues_rounded,labels =paste0(revenues_rounded,'%'), cex=0.8,pos=3, xpd=TRUE)

#to see the full picture, use pie chart to represent the total for each of them in that week
week_total_revenue <- sum(vip_data$vip_total_revenue_per_day) + sum(regular_data$total_revenue_per_day)
week_revenue_vip_percentage <- round(sum(vip_data[c('vip_total_revenue_per_day')])*100 / week_total_revenue,1)
week_revenue_regular_percentage <- round(sum(regular_data[c('total_revenue_per_day')])*100 / week_total_revenue,1)
                         
#var to hold them both  
week_revenues_percentage <- c(week_revenue_vip_percentage,week_revenue_regular_percentage)

pie(week_revenues_percentage,c(paste0('VIP Revenue','\n  ',week_revenue_vip_percentage,'%'),paste0('Regular Revenue','\n  ',week_revenue_regular_percentage,'%')),
    col=c("#FDAE61", "#2B83BA"),
    main="Weekly Revenue Percentage per Visitors Type")

#------------------------------------------------------------------------------------------
# make a graph to show the snacks revenue 
snacks_per_day_chart <- barplot(snacks_week,
                                beside = FALSE,
                                names.arg= week,
                                col=brewer.pal(n = 7, name = "Spectral"),
                                main="Total Snacks Revenue per Day",
                                ylab="Snacks Revenue in $",
                                xlab="Day of Week",
                                ylim=range(pretty(c(0, snacks_week))))
text(x=snacks_per_day_chart, y=snacks_week,labels =paste0('$', snacks_week), cex=0.8,pos=3, xpd=TRUE)

# make a graph to show revenue by snack

snacks_labels <- paste0(round(100 * s_mat/sum(s_mat), 2), "%")
colors = brewer.pal(n = 4, name = "Spectral")
pie(s_mat, labels = snacks_labels, col=colors, main='Snacks Revenue Distribution' )
legend("topleft", legend = c("Popcorn", "Nachos", "Hotdogs", "Candy"), fill= colors)

#-------------------------------------------------------------------------------------
# Total Revenue of the movie Theater [Regular screens + VIP screens + snacks]
revenue_stack <-  t(total_revenue_df[c('Regular_screens_revenue','VIP_screens_revenue', 'snacks_revenue')])
total_revenue_plot <- barplot(revenue_stack,col= brewer.pal(n = 3, name = "Spectral"),
        names.arg = week,
     main="Total Revenue of Movie Theater in a Week",
     ylab="Total Movie Theater Revenue in $",
     xlab="Day of Week",
     ylim=range(pretty(c(0, total_revenue_df$total_revenue_theater))))

text(x=total_revenue_plot, y=total_revenue_df$total_revenue_theater,labels =paste0('$', total_revenue_df$total_revenue_theater), cex=0.8,pos=3, xpd=TRUE)
legend("top", legend = c('Regular Screens Revenue', 'VIP Screens Revenue', 'Snacks Revenue'), fill= brewer.pal(n = 3, name = "Spectral"))


    
