# Head to head polling analysis
require(tidyverse)
require(ggrepel)
polls_processed <- ProcessPolls("president_polls.csv")
#Remove two candidate pollshttp://127.0.0.1:19177/graphics/plot_zoom_png?width=1920&height=1009
#Remove polls with only two candidates
polls_filtered <- polls_processed %>%
  group_by(poll_id) %>%
  mutate(Count.Candidates = length(unique(Candidate))) %>%
  filter(Count.Candidates > 2) %>%
  ungroup() %>%
  mutate(Weight = Weight * ( 1 - (State != "") * (1 - (1 / sqrt(50)))))
other_dems <- c("Bernard Sanders","Gavin Newsom","Hillary Clinton","Kamala Harris","Pete Buttigieg","Elizabeth Warren","Philip Murphy","J.B. Pritzker")
polls_filtered$Candidate[polls_filtered$Candidate %in% other_dems] <- "Other Democrats"
polls_summarized <- polls_filtered %>%
  group_by(Candidate) %>%
  summarise(n = n(), Mean.Democratic.Vote.Bonus = sum(Democratic.Vote * Weight, na.rm = TRUE)/(sum(Weight, na.rm = TRUE)),
            Mean.Republican.Vote.Bonus = sum(Republican.Vote * Weight, na.rm = TRUE)/(sum(Weight, na.rm = TRUE)),
            DPlus = Mean.Democratic.Vote.Bonus * sqrt(sum(Weight * Weight,na.rm = TRUE)),
            RPlus = Mean.Republican.Vote.Bonus * sqrt(sum(Weight * Weight, na.rm = TRUE))) %>%
  mutate(Net.Republican = Mean.Republican.Vote.Bonus - Mean.Democratic.Vote.Bonus,
         `Polling performance` = RPlus - 1.5 * DPlus,
         Test = RPlus / Mean.Republican.Vote.Bonus - DPlus / Mean.Democratic.Vote.Bonus)
polls_summarized$Party <- "Republican"
polls_summarized$Party[polls_summarized$Candidate == "Joe Biden"| polls_summarized$Candidate == "Other Democrats"] <- "Democrat"

polls_republican <- polls_summarized %>%
  filter(Party == "Republican")
ggplot(polls_republican %>% filter(n > 1), aes(y = Mean.Democratic.Vote.Bonus, x = Mean.Republican.Vote.Bonus, size = log(n),label = Candidate, fill = Party))+
  geom_point(size = 1)+
  geom_abline(slope = 1, intercept = 0)+
  geom_label(fill="pink")+
  scale_size_continuous(range = c(5,6))+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "Democratic vote",
       x = "Republican vote")+
  expand_limits(x = c(-0.06,.09))+
  guides(size = "none", fill = "none")+
  coord_equal()

headToHead <- polls_republican
names(headToHead)[1] <- "Politician"
write_csv(headToHead,"Head2Head.csv")
