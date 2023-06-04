require(tidyverse)
require(ggpubr)
require(readODS)
options(scipen = 6, digits = 3)
mc_files <- list.files(path="Morning Consult/")
mc_dat <- read_csv(paste0("Morning Consult/",mc_files[[1]]))
for(i in 2:length(mc_files))
{
  mc_dat <- bind_rows(mc_dat,read_csv(paste0("Morning Consult/",mc_files[[i]])))
}
#Fix oddities in MC data:
bad <- mc_dat$State == "Joe Biden"
mc_dat[bad,]$State <- mc_dat[bad,]$Politician
mc_dat[bad,]$Politician <- "Joe Biden"

greg <- mc_dat$Politician == "Gregory Abbott"
mc_dat[greg,]$Politician <- "Greg Abbott"
#Rm duplicate:
mc_dat <- mc_dat %>% distinct()
#Process Biden approval:
biden_approval <- mc_dat %>%
  filter(Politician == "Joe Biden") %>%
  group_by(State) %>%
  summarise(`Democratic net approval` = mean(`Net Approval`),
            `Sample size` = sum(`Sample size`))
#Process gubernatorial approval:
gub_approval <- mc_dat %>%
  filter(Politician != "Joe Biden") %>%
  group_by(State,Politician) %>%
  summarise(`Net approval` = mean(`Net Approval`))
#Add partisan affiliation:
gub_affil <- read_csv("gub_affil.csv")
#Create net adjusted approval:
merged_approval <- gub_approval %>%
  merge(biden_approval) %>%
  merge(gub_affil) %>%
  mutate(`Background` = "Governor",`Historical win rate` = 10/24) %>%
  add_case(State = "Florida",
           Politician = "Donald Trump",
           Affiliation = "R",
           `Democratic net approval`= - .088,
           `Net approval` = -.122,
           `Background` = "Non-incumbent president",
           `Historical win rate` = 1/4) %>%
  add_case(State = "Indiana",
           Politician = "Mike Pence",
           Affiliation = "R",
           `Democratic net approval`= -.19,
           `Net approval` = 0,
           `Background` = "Vice president",
           `Historical win rate` = 7/11) %>%
  add_case(State = "South Carolina",
           Politician = "Nikki Haley",
           Affiliation = "R",
           `Democratic net approval`= -.1527,
           `Net approval` = .28,
           `Background` = "Ambassador",
           `Historical win rate` = 2/5 ) %>%
  add_case(State = "South Carolina",
           Politician = "Tim Scott",
           Affiliation = "R",
           `Democratic net approval`= -.1527,
           `Net approval` = .11,
           `Background` = "Senator",
           `Historical win rate` = 5/20 ) %>%
  mutate(`Adjusted net approval` =
           (Affiliation == "R") * (`Net approval` + `Democratic net approval`)+
           (Affiliation == "D") * (`Net approval` - `Democratic net approval`))
repubs_of_interest <- c("Donald Trump","Mike Pence","Nikki Haley",
                        "Greg Abbott","Brian Kemp","Chris Sununu","Glenn Youngkin","Kristi Noem","Ron DeSantis",
                        "Doug Burgum", "Tim Scott")

small_frame <- merged_approval %>%
  filter(Politician %in% repubs_of_interest)

RD_elecs <- read_ods("R-DElections.ods") %>%
  filter(!(Politician == "Tim Scott" & Year < 2022))
small_frame2 <- small_frame %>%
  merge(RD_elecs) %>%
  merge(read_csv('swing.csv')) %>%
  select(c("Politician","Background","State","Historical win rate","Home state edge","Adjusted net approval","Electoral performance A","Electoral performance B")) %>%
  mutate(`Composite score` = 2 * `Home state edge`+
           100 * `Adjusted net approval`+
           100 * `Electoral performance A`+
           0.01 * `Electoral performance B` / sd(`Electoral performance B`)+
           100 * `Historical win rate`)

source("Head2Head.R")
small_frame3 <- small_frame2 %>%
  merge(headToHead) %>%
  select(c("Politician","Background","Historical win rate","State","Home state edge","Adjusted net approval","Electoral performance A","Electoral performance B", "Polling performance", "Composite score")) %>%
  mutate(`Composite score` = 10*(`Home state edge` / sd(`Home state edge`)+
           `Adjusted net approval` / sd(`Adjusted net approval`)+
           0.5*`Electoral performance A` / sd (`Electoral performance A`)+
           0.5*`Electoral performance B` / sd(`Electoral performance B`)+
           `Historical win rate` / sd(`Historical win rate`)+
           `Polling performance` / sd(`Polling performance`)))

names(small_frame3) <- c("Politician","Background","Score","State","Score","Approval","Elections","Elections", "Polling", "Overall")
write_csv(small_frame3,"Summary.csv")
