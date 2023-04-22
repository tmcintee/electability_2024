grover <- read_csv("grover.csv")
ggplot(grover %>% filter(Year < 1986 & Office == "Governor of New York"),aes(x = Year, y = `Democratic vote`, color = `Grover Cleveland`))+
  geom_line(color="black")+
  geom_point()+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~Office)
