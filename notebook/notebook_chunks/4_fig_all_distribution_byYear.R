# Senate + house combined
g1 <- ggplot(data = my.tab,
             aes(x = YEAR, 
                 y = PERCENT.WOMEN,
                 color = PERCENT.WOMEN)) +
  geom_point(alpha = .25, size = 3) +
  geom_smooth(size = 1, color = "tomato") +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  ylim(0, 55) + 
  ylab("Percent women") + xlab("Year") + 
  annotate("text", x = min(my.tab$YEAR) + .25, y = 52, label = "Parity", 
           family = "Droid Sans Mono", hjust = 0, size = 4) +
  ggtitle(bquote(atop("Evolution of gender ratio in state legislature", 
                      atop("(each dot represents a state)")))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") + 
  guides(color = FALSE)

g1 <- addBanner(g1, font.size = 4,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g1)


# Senate alone
g2 <- ggplot(data = my.tab,
             aes(x = YEAR, 
                 y = 100 * SENATE.WOMEN / SENATE.TOTAL,
                 color = 100 * SENATE.WOMEN / SENATE.TOTAL)) +
  geom_point(alpha = .25, size = 3) +
  geom_smooth(size = 1, color = "tomato") +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  ylim(0, 55) + 
  ylab("Percent women") + xlab("Year") + 
  annotate("text", x = min(my.tab$YEAR) + .25, y = 52, label = "Parity", 
           family = "Droid Sans Mono", hjust = 0, size = 4) +
  ggtitle(bquote(atop("Evolution of gender ratio in state senates", 
                      atop("(each dot represents a state)")))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") +
  guides(color = FALSE)

g2 <- addBanner(g2, font.size = 4,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g2)


# House alone
g3 <- ggplot(data = my.tab,
             aes(x = YEAR, 
                 y = 100 * HOUSE.WOMEN / HOUSE.TOTAL,
                 color = 100 * HOUSE.WOMEN / HOUSE.TOTAL)) +
  geom_point(alpha = .25, size = 3) +
  geom_smooth(size = 1, color = "tomato") +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  ylim(0, 55) + 
  ylab("Percent women") + xlab("Year") + 
  annotate("text", x = min(my.tab$YEAR) + .25, y = 52, label = "Parity", 
           family = "Droid Sans Mono", hjust = 0, size = 4) +
  ggtitle(bquote(atop("Evolution of gender ratio in state houses", 
                      atop("(each dot represents a state)")))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") +
  guides(color = FALSE)

g3 <- addBanner(g3, font.size = 4,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g3)


