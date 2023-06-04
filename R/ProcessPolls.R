ProcessPolls <- function(poll_file)
{
  polls_raw <- read.csv(poll_file,stringsAsFactors = FALSE)
  nameVector <- as.character(polls_raw$candidate_name)
  list_names <- str_split(nameVector," ")
  for(i in 1:length(list_names))
  {
    first_name <- list_names[[i]][[1]]
    last_name <- list_names[[i]][[length(list_names[[i]])]]
    #Special character cleaning will go here as needed.
    name_candidate_cleaned <- paste0(first_name," ",last_name)
    nameVector[[i]] <- paste0(first_name," ",last_name)
  }
  polls_raw$Candidate <- nameVector
  polls_raw$start_date <- parse_date(as.character(polls_raw$start_date),format = "%m/%d/%y")
  polls_raw <- polls_raw %>%
    filter(party %in% c("DEM","REP"))
  polls_condensed <- polls_raw %>%
    spread(key = party, value = pct) %>%
    group_by(question_id) %>%
    mutate(Share.Democratic = max(DEM,na.rm = TRUE)/100,
           Share.Republican = max(REP, na.rm = TRUE)/100) %>%
    ungroup %>%
    group_by(poll_id) %>%
    mutate(Democratic.Vote = Share.Democratic - median(Share.Democratic),
           Republican.Vote = Share.Republican - median(Share.Republican),
           Year = 2024+as.numeric(start_date-parse_date("2024-11-05"))/366,
           Weight = sqrt(sample_size)*TimeDecay(Year),
           State = state,
           Type = "Poll") %>%
    #filter(Candidate %in% candidate_vector) %>%
    ungroup %>%
    select(Candidate,State,Year,Democratic.Vote,Republican.Vote,Share.Democratic,Share.Republican,Weight,Type,poll_id)
  return(polls_condensed)
}
