# Senate + house combined
g1 <- ggplot(data = my.tab,
             aes(x = YEAR, 
                 y = PERCENT.WOMEN, 
                 color = STATE.ABB)) +
  geom_path(size = 1, alpha = .5) +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  ylim(0, 55) + 
  ylab("Percent women") + xlab("Year") + 
  annotate("text", x = min(my.tab$YEAR) + .25, y = 52, label = "Parity", 
           family = "Droid Sans Mono", hjust = 0, size = 4) +
  ggtitle(bquote(atop("Evolution of gender ratio in state legislature", 
                      atop()))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") +
  guides(color = guide_legend(title = "State", ncol = 3))

g1 <- addBanner(g1, font.size = 4,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g1)


# Senate alone
g2 <- ggplot(data = my.tab,
             aes(x = YEAR, 
                 y = 100 * SENATE.WOMEN / SENATE.TOTAL, 
                 color = STATE.ABB)) +
  geom_path(size = 1, alpha = .5) +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  ylim(0, 55) + 
  ylab("Percent women") + xlab("Year") + 
  annotate("text", x = min(my.tab$YEAR) + .25, y = 52, label = "Parity", 
           family = "Droid Sans Mono", hjust = 0, size = 4) +
  ggtitle(bquote(atop("Evolution of gender ratio in state senates", 
                      atop()))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") +
  guides(color = guide_legend(title = "State", ncol = 3))

g2 <- addBanner(g2, font.size = 4,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g2)


# House alone
g3 <- ggplot(data = my.tab,
             aes(x = YEAR, 
                 y = 100 * HOUSE.WOMEN / HOUSE.TOTAL, 
                 color = STATE.ABB)) +
  geom_path(size = 1, alpha = .5) +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  ylim(0, 55) + 
  ylab("Percent women") + xlab("Year") + 
  annotate("text", x = min(my.tab$YEAR) + .25, y = 52, label = "Parity", 
           family = "Droid Sans Mono", hjust = 0, size = 4) +
  ggtitle(bquote(atop("Evolution of gender ratio in state houses", 
                      atop()))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") +
  guides(color = guide_legend(title = "State", ncol = 3))

g3 <- addBanner(g3, font.size = 4,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g3)





