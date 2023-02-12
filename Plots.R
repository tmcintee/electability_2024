
g1 <- ggplot(merged_approval,
             aes(y = `Net approval`,
                 x = `Biden net approval`,
                 color = Affiliation,
                 label = Politician))+
  geom_label()+
  expand_limits(x = -0.2)+
  scale_color_manual(values = c("blue","red"))+
  coord_equal()

ggplot(merged_approval,
       aes(x = `Adjusted net approval`,
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
                 color = Affiliation,
                 label = Politician))+
  geom_label()+
  expand_limits(x = c(-0.2,0.3))+
  scale_color_manual(values = c("red","red"))+
  coord_equal()

ggarrange(g1,g2, common.legend = TRUE, )
