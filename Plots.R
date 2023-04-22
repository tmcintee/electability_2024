require(ggrepel)
g1 <- ggplot(merged_approval,
             aes(y = `Net approval`,
                 x = `Biden net approval`,
                 color = Affiliation,
                 label = Politician))+
  geom_label()+
  expand_limits(x = -0.2)+
  scale_color_manual(values = c("blue","red"))+
  coord_equal()

ggplot(merged_approval %>% filter(Affiliation=="R"),
       aes(x = `Net approval`,
           y = `Biden net approval`,
           color = Affiliation,
           label = Politician))+
  geom_label()+
  expand_limits(x = -0.2)+
  scale_color_manual(values = c("blue","red"))+
  coord_equal()

g2 <- ggplot(small_frame,
             aes(x = `Adjusted net approval`,
                 y = `Net approval`,
                 label = Politician))+
  geom_label_repel(min.segment.length = 0, color = "red")+
  geom_point()+
  expand_limits(x = c(-0.2,0.3))+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  coord_equal()

g2 + guides(color = FALSE)

g3 <- ggplot(RD_elecs %>% filter(Contest == "Governor"),aes(x = DiffR, y = DiffD, label = Politician))+
  geom_label_repel(color = "red", min.segment.length = 0)+
  geom_point()+
  expand_limits(x = c(-50000,120000))+
  coord_equal()+
  labs(x = "Votes in favor", y = "Votes against")


g4 <- ggplot(RD_elecs %>% filter(Contest == "Governor"),aes(x = DiffRp, y = DiffDp, label = Politician))+
  geom_label_repel(color = "red", min.segment.length = 0)+
  geom_point()+
  coord_equal()+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Votes in favor", y = "Votes against")

ggarrange(g3,g4) %>% annotate_figure(top = "Electoral performance relative to median Republican")
