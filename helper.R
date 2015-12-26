stateConversion <- function(x) {
  
  state_codes <- data.frame(state = as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
                                                "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                                                "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
                                                "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT",
                                                "WA", "WI", "WV", "WY")),
                            full = as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                                             "connecticut","district of columbia","delaware","florida","georgia",
                                             "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                                             "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                                             "missouri","mississippi","montana","north carolina","north dakota",
                                             "nebraska","new hampshire","new jersey","new mexico","nevada",
                                             "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                                             "rhode island","south carolina","south dakota","tennessee","texas",
                                             "utah","virginia","vermont","washington","wisconsin",
                                             "west virginia","wyoming"))
  )
  temp <- data.frame(state=x)
  res <- state_codes$full[match(temp$state, state_codes$state)]
  return(res)  
}

aggregate_by_state <- function(data, param_name, year_min, year_max, param_states) {

  replace_na <- function(x) ifelse(is.na(x), 0, x)
  
  percent_diy <- function(x, y) round(sum(x)/y*100, 0)
  
  total_states <- data.table(state=sort(unique(data$state)))
  
  temp_data <- filter(data, data$name==param_name, data$year>=year_min, data$year<=year_max)
  
  total_num <- sum(temp_data$number) #around US
  
  aggregated <- data.table(state=character(), number=numeric()) 
  
  if(total_num != 0){
      aggregated <- temp_data %>%
                    filter(state %in% param_states) %>%
                    group_by(state) %>%
                    summarise_each(funs(percent_diy(number, total_num)), number)
  }

  res <- merge(total_states, data.table(aggregated), by = "state", all=TRUE) %>%
         mutate_each(funs(replace_na), number)
  
  res$state <- stateConversion(res$state)

  return(res)

}


aggregate_by_year <- function(data, param_name, year_min, year_max, param_states) {
  
  res <- data %>%
         filter(data$name==param_name, data$year>=year_min, data$year<=year_max, state %in% param_states) %>%
         group_by(year, state) %>%
         summarise_each(funs(sum), number)

  return(data.table(res))
  
}

