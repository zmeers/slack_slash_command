# Packages ----
library(plumber)
library(magrittr)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)


primary_calendar <- tibble::tribble(
  ~election_date,                 ~state,
  "2020-02-03",                 "Iowa",
  "2020-02-11",        "New Hampshire",
  "2020-02-22",               "Nevada",
  "2020-02-29",       "South Carolina",
  "2020-03-03",              "Alabama",
  "2020-03-03",       "American Samoa",
  "2020-03-03",             "Arkansas",
  "2020-03-03",           "California",
  "2020-03-03",             "Colorado",
  "2020-03-03",                "Maine",
  "2020-03-03",        "Massachusetts",
  "2020-03-03",            "Minnesota",
  "2020-03-03",       "North Carolina",
  "2020-03-03",             "Oklahoma",
  "2020-03-03",            "Tennessee",
  "2020-03-03",                "Texas",
  "2020-03-03",                 "Utah",
  "2020-03-03",              "Vermont",
  "2020-03-03",             "Virginia",
  "2020-03-03",     "Democrats Abroad",
  "2020-03-10",                "Idaho",
  "2020-03-10",             "Michigan",
  "2020-03-10",          "Mississippi",
  "2020-03-10",             "Missouri",
  "2020-03-10",         "North Dakota",
  "2020-03-10",           "Washington",
  "2020-03-14",    "Northern Marianas",
  "2020-03-17",              "Arizona",
  "2020-03-17",              "Florida",
  "2020-03-17",             "Illinois",
  "2020-03-17",                 "Ohio",
  "2020-03-24",              "Georgia",
  "2020-03-29",          "Puerto Rico",
  "2020-04-04",               "Alaska",
  "2020-04-04",               "Hawaii",
  "2020-04-04",            "Louisiana",
  "2020-04-04",              "Wyoming",
  "2020-04-07",            "Wisconsin",
  "2020-04-28",          "Connecticut",
  "2020-04-28",             "Delaware",
  "2020-04-28",             "Maryland",
  "2020-04-28",             "New York",
  "2020-04-28",         "Pennsylvania",
  "2020-04-28",         "Rhode Island",
  "2020-05-02",                 "Guam",
  "2020-05-02",               "Kansas",
  "2020-05-05",              "Indiana",
  "2020-05-12",             "Nebraska",
  "2020-05-12",        "West Virginia",
  "2020-05-19",             "Kentucky",
  "2020-05-19",               "Oregon",
  "2020-06-02", "District of Columbia",
  "2020-06-02",              "Montana",
  "2020-06-02",           "New Jersey",
  "2020-06-02",           "New Mexico",
  "2020-06-02",         "South Dakota",
  "2020-06-06",       "Virgin Islands",
  "2020-11-03",             "National"
)

primary_polls <- read_csv("http://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv") %>%
  filter(party == "DEM") %>%
  mutate(state = replace_na(state, "National"),
         contest = "2020D",
         start_date = mdy(start_date),
         end_date = mdy(end_date),
         mid_date = start_date + floor((end_date-start_date)/2)) %>%
  left_join(primary_calendar %>% select(election_date, state), by = "state") %>%
  mutate(t = interval(mid_date, election_date),
         t = t %/% days(1),
         f = interval(start_date, end_date),
         field_time = f %/% days(1),
         t_rev = interval(election_date, mid_date),
         t_rev = t_rev %/% days(1)) %>%
  select(-f) %>%
  rename(share = pct) %>%
  mutate(candidate_name = gsub("\\s[A-Z]\\.\\s", " ", candidate_name),
         candidate_name = gsub(" Robert", "", candidate_name),
         candidate_name = gsub("Joseph Biden Jr.", "Joseph R. Biden Jr.", candidate_name),
         candidate_name = gsub("á", "a", candidate_name)) %>%
  rename(true_name = candidate_name) %>%
  rename(last_name = answer) %>%
  mutate(notes = replace_na(notes, "standard poll")) %>%
  filter(!str_detect(notes, "head|open")) %>%
  filter(question_id != 115625)


# Slack authorization
slack_auth <- function(req) {
  
  
  if (grepl("swagger", tolower(req$PATH_INFO))) return(forward())
  
  # Verify request came from Slack ----
  if (is.null(req[["HTTP_X_SLACK_REQUEST_TIMESTAMP"]])) {
    return("401")
  }

  base_string <- paste(
    "v0",
    req$HTTP_X_SLACK_REQUEST_TIMESTAMP,
    req$postBody,
    sep = ":"
  )

  # Slack Signing secret is available as environment variable
  # SLACK_SIGNING_SECRET
  computed_request_signature <- paste0(
    "v0=",
    openssl::sha256(base_string, Sys.getenv("SLACK_SIGNING_SECRET"))
  )
  cat('SlackSig: ', Sys.getenv("SLACK_SIGNING_SECRET"))
  cat('Computed:', computed_request_signature, '\n')
  cat('Provided: ',  req$HTTP_X_SLACK_SIGNATURE, '\n')

  
  # If the computed request signature doesn't match the signature provided in the
  # request, set status of response to 401
  if (!identical(req$HTTP_X_SLACK_SIGNATURE, computed_request_signature)) {
    res$status <- 401
  } else {
    res$status <- 200
  }
  
  if (res$status == 401) {
    list(
      text = "Error: Invalid request"
    )
  } else {
    forward()
  }
}


# * Log information about the incoming request
# * @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO,   "-", req$QUERY_STRING, "--", req$ARGS, "--",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
      )

  # Forward request
  plumber::forward()
}



# * @filter cors
function(req, res) {
   res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }

}

#* Parse the incoming request and route it to the appropriate endpoint
#* @filter route-endpoint
function(req, text = "") {
  # Identify endpoint
  split_text <- urltools::url_decode(text) 
  
  state.name <- c(state.name, "National")
  
  if (str_detect(split_text, "status")){
    endpoint <- "status"
    req$PATH_INFO <- paste0("/", endpoint)
  } else{
  
    endpoint <- "newpoll"
    
    # Modify request with updated endpoint
    req$PATH_INFO <- paste0("/", endpoint)
    
    # Modify request with remaining commands from text
    req$ARGS <- ifelse(str_detect(split_text, state.name), state.name, NA_character_) %>%
      .[!is.na(.)]
  }
  
  # Forward request 
  plumber::forward()
}

#* Return a message containing status details about the state
#* @serializer unboxedJSON
#* @post /status
function(req, res) {
  # Check req$ARGS and match to state - if no state match is found, return
  # an error
  # question_ids <- unique(primary_polls$question_id)
  state_names <- unique(primary_polls$state)
  
  print(ls(req$ARGS))
  
  if (!req$ARGS %in% state_names) {
    res$status <- 400
    return(
      list(
        response_type = "ephemeral",
        text = paste("Error: No state found matching", req$ARGS)
      )
    )
  }
  
  # Filter data to customer data based on provided id / name
    state_name <- req$ARGS
    state_data <- polling_data %>% 
      dplyr::filter(state == state_name) %>%
      select(state, pollster, start_date, end_date) %>% 
      slice(1)
    

  
  # Build response
  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "ephemeral",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        title = paste0("Status update for ", state_name),
        # Fields provide a way of communicating semi-tabular data in Slack
        fields = list(
          list(
            title = "Pollster",
            value = state_data$pollster,
            short = TRUE
          ),
          list(
            title = "Start date",
            value = state_data$start_date,
            short = TRUE
          ),
          list(
            title = "End date",
            value = state_data$end_date,
            short = TRUE
          )
        )
      )
    )
  )
}




#* @serializer unboxedJSON
#* @post /newpoll
new_poll <- function(res, filter_state = "National"){

  if (!filter_state %in% primary_polls$state) {
    res$status <- 400
    stop("State" , filter_state, " not found.")
  }
  
  
  state.name <- c(state.name, "National")
  

  filter_state <- ifelse(str_detect(filter_state, state.name), state.name, NA_character_) %>%
    .[!is.na(.)]

  grab_question_id <- primary_polls %>%
    filter(state == filter_state) %>%
    arrange(desc(end_date)) %>%
    slice(1) %>%
    pluck("question_id")


  if(!is.null(grab_question_id)){
    my_results <- primary_polls %>%
      filter(question_id == grab_question_id) %>%
      mutate(start_date = format(start_date, "%b. %d, %Y"),
             end_date = format(end_date, "%b. %d, %Y")) %>%
      arrange(desc(share)) %>%
      mutate(true_name = case_when(
        true_name == "Bernard Sanders" ~ "Bernie Sanders",
        true_name == "Joseph R. Biden Jr." ~ "Joe Biden",
        TRUE ~ true_name
      ))

  }




  if(is.null(grab_question_id)){
    mytext <- paste0("There are no polls coming out of ", filter_state, " for the 2020 primaries (yet!).")
  }  else{
    mytext <- paste0("The latest *", my_results[1, 4], "* poll for the 2020 Democratic :us: primaries was fielded by *", my_results[1, 6],
                     "* from *", my_results[1,18], "* to *", my_results[1,19],
                     "*. \nThe best performing candidate in this poll was :tada: *", my_results[1,32], "* :tada: with a poll share of *", my_results[1,33],
                     "%* \nThe remaining candidates received the following results: *",
                     my_results %>% slice(-1) %>%
                       glue::glue_data("{true_name} ({share}%)") %>%
                       glue::glue_collapse(sep = ", ", last = ", and ") %>%
                       glue::glue(., "*.")
    )
  }
    
  

  httr::POST(
        url = "https://hooks.slack.com/services/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",
        httr::add_headers('Content-Type'='application/json', 
                     'Charset'='utf-8',
                     'Authorization'= 'Bearer xoxb-....-...-...'),
        body = list(
          text  = mytext,
          response_type = "in_channel", 
          type = "mrkdwn"
        ), 
        encode = "json"
      )
}

#* @serializer contentType list(type="text/plain")
#* @get /newpoll
# new_poll <- function(res, filter_state = "National"){
# 
#   state.name <- c(state.name, "National")
# 
# 
#   filter_state <- ifelse(str_detect(filter_state, state.name), state.name, NA_character_) %>%
#     .[!is.na(.)]
# 
#   grab_question_id <- primary_polls %>%
#     filter(state == filter_state) %>%
#     arrange(desc(end_date)) %>%
#     slice(1) %>%
#     pluck("question_id")
# 
# 
#   if(!is.null(grab_question_id)){
#     my_results <- primary_polls %>%
#       filter(question_id == grab_question_id) %>%
#       mutate(state = gsub("National", "national", state)) %>%
#       mutate(start_date = format(start_date, "%b. %d, %Y"),
#              end_date = format(end_date, "%b. %d, %Y")) %>%
#       arrange(desc(share)) %>%
#       mutate(true_name = case_when(
#         true_name == "Bernard Sanders" ~ "Bernie Sanders",
#         true_name == "Joseph R. Biden Jr." ~ "Joe Biden",
#         TRUE ~ true_name
#       ))
# 
#   }
# 
# 
# 
# 
#   if(is.null(grab_question_id)){
#     mytext <- paste0("There are no polls coming out of ", filter_state, " for the 2020 primaries (yet!).")
#   }  else{
#     mytext <- paste0("The latest *", my_results[1, 4], "* poll for the 2020 Democratic :us: primaries was fielded by *", my_results[1, 6],
#                      "* from *", my_results[1,18], "* to *", my_results[1,19],
#                      "*. \nThe best performing candidate in this poll was :tada: *", my_results[1,32], "* :tada: with a poll share of *", my_results[1,33],
#                      "%* \nThe remaining candidates received the following results: *",
#                      my_results %>% slice(-1) %>%
#                        glue::glue_data("{true_name} ({share}%)") %>%
#                        glue::glue_collapse(sep = ", ", last = ", and ") %>%
#                        glue::glue(., "*.")
#     )
#   }
# 
# 
# 
#   httr::POST(
#     url = "https://hooks.slack.com/services/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",
#     httr::add_headers('Content-Type'='text/plain',
#                       'Charset'='utf-8',
#                       'Authorization'= 'Bearer xoxb-...-...-...'),
#     body = list(
#       text  = mytext
#     ),
#     encode = "json"
#   )
# }

