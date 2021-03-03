#' Re-ageing a simulated individual larval fish using age-length-key method   
#'
#' This requires a simulated_sample dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data.
#' @param N The number of individuals selected from each day 
#'
#' @return a datraframe of re-aged simulated data

age_data_with_age_length_key <- function(data, 
                                         bin_width_size = 0.1,
                                         rough_bin_width = 1.5,
                                         n_samples_each = 3,
                                         exclude_min_length = FALSE) {

  #rounding function
  round_by_bin <- function(x, bin_width) {  
    round(x/bin_width, 0)*bin_width
  }
  
  ageing_data <- data %>%  
    mutate(
      rough_size = round_by_bin(size_sampled, rough_bin_width),
      random = runif(length(size_sampled))
    ) %>% 
    group_by(rough_size) %>% 
    top_n(n_samples_each, wt= random) %>% 
    select(-random) #here we are selecting first 5 individuals from each day_born (note oldest ages may have less than 5 individuals left)
  
  ageing_data %>% 
    ggplot(aes(size_sampled)) + geom_histogram() + 
    geom_histogram(binwidth=1) + facet_grid(~age) 

#age-length-key

ageing_data$agekey <- ageing_data$age


#get fish with ages
with_ages <- as.data.frame(cbind(ageing_data$size_sampled, ageing_data$agekey))

colnames(with_ages) <- c("size_sampled", "agekey")

#get fish without ages

data$agekey <- NA

without_ages <- as.data.frame(cbind(data$size_sampled, data$agekey))

colnames(without_ages) <- c("size_sampled", "agekey")

without_ages <- lencat(~size_sampled, data= without_ages,startcat=0,w=1,vname="lengthCat")


#Setting length categories for alk ## note: area category in this case

Summarize(~size_sampled,data=with_ages,digits=1)  ## to find minimun age category eg; just below min = 2.0


with_ages <- lencat(~size_sampled,data= with_ages,startcat=0,w=1,vname="lengthCat")


#avoid  problem where the minimum observed length in the length sample is less than the smallest length category in the age-length key

if (exclude_min_length) { 
  min_length <- min(with_ages$lengthCat)
  
  without_ages <- subset(without_ages, without_ages$size_sampled > min_length) 
}


#construct ALK
#Note: can rename lengthCcat label using vname="new label" argument above.
#constructing the summary contingency table of numbers of fish in each combined age and length category.

age_raw <- with(with_ages,table(lengthCat,agekey))
age_key <- prop.table(age_raw,margin=1)
round(age_key,3)   ##round for disply purposes only

#Assigning Ages to Individuals with the Age-Length Key... ageKey(): Removed. Deprecated since 0.4.24. Use alkIndivAge().

age_applied <- alkIndivAge(age_key, agekey~lengthCat, data=without_ages)


#Remove negetive ages
data2 <- data

data2$estimated_age <- age_applied$agekey

min(data2$estimated_age)

data2 <- data2 %>% filter(estimated_age >= 0)

data2

}