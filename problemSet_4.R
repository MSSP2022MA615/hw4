library(magrittr)
library(readr) 
library(tidyr)
library(dplyr)

myName <- "Hao He"

# Warm up

# 1
# Create a function print_order that given a numeric vector x with length(3)
# it will return the elements in order from high to low. You must use if, else if, and else in the function. 
# For instance, given x1 is (1, 3.7, 6), your result should be (6, 3.7, 1).

print_order <- function(x){
  if (x[1] > x[2]){
    vmax <- x[1]
    vmin <- x[2]
  }
  else {
    vmax <- x[2]
    vmin <- x[1]
  } 
  if ( x[3] > vmax & x[3] > vmin) {
    vmid <- vmax
    vmax <- x[3]
  } 
  else if ( x[3] < vmin & x[3] < vmax ) {
    vmid <- vmin
    vmin <- x[3]
  } 
  else {
  vmid <- x[3]
  }
    x = c(vmax,vmid,vmin)
    return(x)}

x1 <- c(1, 3.7, 6)
print_order(x1)
 
# 2
# Create a function print_string to print the numbers from 1 to any number，and print “Yes” for multiples of 3,
# print “No” for multiples of 5, 
# and print “Unknown” for multiples of both. The following should be your output when number is 5

print_string <- function(x) {
  for (x in 1: x) {
    if( x %% 3 && x %% 5 ) {
      print(x);
    } 
    else {
      if( x %% 3 == 0 ) {
        print("Yes");
      }
      else if( x %% 5 == 0 ) {
        print("No");
      }
      else if ( x %% 3 == 0 &&  x %% 5 == 0) {
        print("Unknown")
      }
    }
  }
}
print_string(5) 

# 3
# Create a function calc_sum_of_factor to calculate the sum of square of the factors of a given number. 
# Must use sapply in the function. For instance the given number 12, 
# the factors of 12 are 1, 2, 3, 4, 6, 12, your return should be 210.
calc_sum_of_factor <- function(x){
  fact = c()
  for (i in 1:x){
   if((x %% i) == 0){
     fact[i]=i
     }
    }
 sum(sapply(fact, FUN = function(x){x^2}), na.rm = TRUE)
}

# 4
# Create a function find_intersect to find the intersection of three vectors. 
# You cannot use build-in function intersect()
find_intersect <- function(a,b,c){
  intersect(intersect(a,b),c)
}
# 5
# Create a function factorial_base to calculate the factorial of a number, 
# you cannot use build-in function factorial().

factorial_base <- function(n) {
  
  Factorial <- 1
  
  if((n == 0)|(n==1))
  {Factorial <- 1
  
  }else {
    for(i in 1:n) {
      Factorial = Factorial * i
    }
    return(Factorial)
  }
}
factorial_base(1)

# 6
Tn <- function(n){
  return (n*(n+1)/2)
}

perfect_sqr <- function(x){
  return(trunc(sqrt(x))^2 == x)
}

num_tri_sqr <- function(n){
  tn = Tn(1:n)
  perfect_sqr_test = sapply(tn, FUN = perfect_sqr)
  return(tn[perfect_sqr_test])
}

q6_sum <- sum(num_tri_sqr(1500000))

# 2022 H-1B Employer Data Hub Assignments
# 1. Read the data with read_csv(), and Name h1b_2022
h1b_2022 <- read_csv("https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv")

# 3. Count the number of NA name na_num, then remove all NA value and Non descriptive value(eg. State: -) from the data name h1b_2022a.
na_num <-sum(is.na(h1b_2022))

h1b_2022a <- drop_na(h1b_2022)
h1b_2022a <- h1b_2022a[h1b_2022a$City != "-",]

# 4. Using h1b_2022a to create a new dataframe that include the following contents (By order) Name by df_num
df_num <- h1b_2022a %>% 
  group_by(State) %>% 
  arrange(State) %>% 
  summarise(across(`Initial Approval`: `Continuing Denial`, sum)) %>% 
  mutate(`Init App` = `Initial Approval` + `Initial Denial`) %>% 
  mutate(`Conti App` = `Continuing Approval` + `Continuing Denial`) %>% 
  rename( `Approve` = `Initial Approval`) %>% 
  rename(`Denial` = `Initial Denial`) %>% 
  select(State, `Init App`, `Conti App`, `Approve`, `Denial`)

# check: colnames(df_num) = c("State", "Initi App","Conti App","Approve", "Denial")
df_num

# 5. Count the total number of Approval app_num and Denial den_num using df_num.
app_num <- sum(df_num$Approve)

den_num <- sum(df_num$Denial)

# 6. Find out the number of application in each city, it should generate a dataframe like below, and name city_num.
city_num <- h1b_2022a %>% 
  group_by(City) %>% 
  arrange(City) %>% 
  count(NAICS) %>% 
  transmute(Count=sum(n)) %>% 
  select(City, Count) %>% unique()

#7. Find out the number of different visa applications, by using NAICS column, and calculate the percentage of NAICS, round 3 digits and use 100 base (0.002 * 100 = 0.2), name visa_num.
visa_num <- h1b_2022a %>% 
  group_by(NAICS) %>% 
  arrange(NAICS) %>%
  count(NAICS) %>% 
  transmute(Number=sum(n))  %>% 
  mutate(Percentage = Number/46084 * 100) 

visa_num$Percentage = round(visa_num$Percentage, 3)

# Extra bonus
non_integer_factorial <- function(x){
  return(gamma(x+1))
}



