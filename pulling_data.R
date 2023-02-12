require(tidyverse)
require(ggpubr)
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
  summarise(`Biden net approval` = mean(`Net Approval`),
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
  mutate(`Adjusted net approval` =
           (Affiliation == "R") * (`Net approval` + `Biden net approval`)+
           (Affiliation == "D") * (`Net approval` - `Biden net approval`))
merged_approval <- merged_approval %>% add_case(State = "United States",
                                                Politician = "Donald Trump",
                                                Affiliation = "R",
                                                `Biden net approval`= - .093,
                                                `Net approval` = -.129,
                                                `Adjusted net approval` = - .036)


repubs_of_interest <- c("Donald Trump","Greg Abbott","Brian Kemp","Chris Sununu","Glenn Youngkin","Kristi Noem","Ron DeSantis")

small_frame <- merged_approval %>%
  filter(Politician %in% repubs_of_interest)

RD_elecs <- read_csv("R-DElections.csv")
