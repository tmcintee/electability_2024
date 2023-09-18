# Favorability polling analysis
require(tidyverse)
require(ggrepel)
convert_grades <- function(x) {
  A <- factor(x, levels=c("A+", "A", "A-",
                          "B+", "B", "B-",
                          "C+", "C", "C-",
                          "D+", "D", "D-", "F",
                          "A/B","B/C","C/D"))
  values <- c(4.3, 4, 3.7,
              3.3, 3, 2.7,
              2.3, 2, 1.7,
              1.3, 1, 0.7, 0,
              3.5,2.5,1.5)
  return(values[A])
}
#To do: Add weighting by time.
favorables <- read_csv("favorability_polls.csv")
favorables$politician[favorables$politician == "Vivek G. Ramaswamy"] <- "Vivek Ramaswamy"
favorables$grade <- convert_grades(favorables$fte_grade)
favorables$weight <- favorables$grade * sqrt(favorables$sample_size)
favorables_trimmed <- favorables %>%
  filter(politician %in% repubs_of_interest) %>%
  filter(is.na(partisan)) %>%
  filter(!internal) %>%
  group_by(poll_id) %>%
  filter(n() > 1) %>%
  select(politician,favorable,unfavorable,sample_size,fte_grade,weight) %>%
  mutate(favorable = favorable - min(favorable),
         unfavorable = unfavorable - min(unfavorable),
         net_favorable = favorable - unfavorable) %>%
  ungroup()

politicians_favorables <- favorables_trimmed %>%
  mutate(weighted_net = net_favorable * weight) %>%
  group_by(politician) %>%
  summarize(net_favorable = 0.01*sum(weighted_net,na.rm = TRUE)/sum(weight, na.rm = TRUE))

names(politicians_favorables) <- c("Politician","Adjusted net favorable")
