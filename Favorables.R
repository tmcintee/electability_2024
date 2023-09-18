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


favorables_example <- favorables[favorables$poll_id == 84165,]
favorables_example <- favorables_example %>%
  filter(politician %in% repubs_of_interest) %>%
  pivot_longer(cols = "favorable":"unfavorable",names_to = "opinion",values_to = "percentage") %>%
  group_by(opinion) %>%
  mutate(adjusted = percentage - min(percentage))

g1 <- ggplot(favorables_example,aes(x = politician, fill = opinion, y = percentage))+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 30))+
  scale_fill_manual(values = c("gold","dark green"))+
  labs(x = "Politician", y = "Percentage", title = "Favorability")+
  expand_limits(y = 50)

g2 <- ggplot(favorables_example,aes(x = politician, fill = opinion, y = adjusted))+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 30))+
  scale_fill_manual(values = c("gold","dark green"))+
  labs(x = "Politician", y = "Adjusted Percentage", title = "Adjusted Favorability")+
  expand_limits(y = 50)

ggpubr::ggarrange(g1,g2)
