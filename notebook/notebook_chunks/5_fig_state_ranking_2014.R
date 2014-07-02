# Compute US averages
tmp <- filter(my.tab, YEAR == 2014)
avg.all <- 100 * sum(tmp$TOTAL.WOMEN) / sum(tmp$TOTAL.LEGISLATURE)
avg.senate <- 100 * sum(tmp$SENATE.WOMEN) / sum(tmp$SENATE.TOTAL)
avg.house <- 100 * sum(tmp$HOUSE.WOMEN, na.rm = TRUE) / sum(tmp$HOUSE.TOTAL, na.rm = TRUE)
  
# Senate + house combined
g1 <- ggplot(data = tmp,
             aes(x = reorder(STATE.NAME, PERCENT.WOMEN), 
                 y = PERCENT.WOMEN)) +
  geom_hline(yintercept = avg.all, linetype = 3, size = 1) +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  geom_bar(stat = "identity", color = "white", fill = "dodgerblue4") +
  annotate("text", x = 1.25, y = avg.all + 2, label = "US average", angle = -90, 
           family = "Droid Sans Mono", hjust = 1, size = 5) +
  annotate("text", x = 1.25, y = 52, label = "Parity", angle = -90, 
           family = "Droid Sans Mono", hjust = 1, size = 5) +
  coord_flip() + ylim(0, 55) +
  guides(fill = FALSE) +
  ylab("Percent women") + 
  ggtitle(bquote(atop("Gender ratio in state legislature", 
                      atop("Senate + House, by US state - 2014")))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") + 
  theme(axis.title.y = element_blank())

g1 <- addBanner(g1, font.size = 4, heights = c(1, 0.05*7/10),
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g1)

# Senate alone
g2 <- ggplot(data = tmp,
             aes(x = reorder(STATE.NAME, 100 * SENATE.WOMEN / SENATE.TOTAL), 
                 y = 100 * SENATE.WOMEN / SENATE.TOTAL)) +
  geom_hline(yintercept = avg.senate, linetype = 3, size = 1) +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  geom_bar(stat = "identity", color = "white", fill = "dodgerblue4") +
  annotate("text", x = 1.25, y = avg.senate + 2, label = "US average", angle = -90, 
           family = "Droid Sans Mono", hjust = 1, size = 5) +
  annotate("text", x = 1.25, y = 52, label = "Parity", angle = -90, 
           family = "Droid Sans Mono", hjust = 1, size = 5) +
  coord_flip() + ylim(0, 55) +
  guides(fill = FALSE) +
  ylab("Percent women") + 
  ggtitle(bquote(atop("Gender ratio in state senates", 
                      atop("By US state - 2014")))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") + 
  theme(axis.title.y = element_blank())

g2 <- addBanner(g2, font.size = 4, heights = c(1, 0.05*7/10),
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")
print(g2)


# House alone (Nebraska is excluded because it doesn't have a house of 
# representatives)
g3 <- ggplot(data = filter(tmp, STATE.NAME != "Nebraska"),
             aes(x = reorder(STATE.NAME, 100 * HOUSE.WOMEN / HOUSE.TOTAL), 
                 y = 100 * HOUSE.WOMEN / HOUSE.TOTAL)) +
  geom_hline(yintercept = avg.house, linetype = 3, size = 1) +
  geom_hline(yintercept = 50, linetype = 2, size = 1) +
  geom_bar(stat = "identity", color = "white", fill = "dodgerblue4") +
  annotate("text", x = 1.25, y = avg.house + 2, label = "US average", angle = -90, 
           family = "Droid Sans Mono", hjust = 1, size = 5) +
  annotate("text", x = 1.25, y = 52, label = "Parity", angle = -90, 
           family = "Droid Sans Mono", hjust = 1, size = 5) +
  coord_flip() + ylim(0, 55) +
  guides(fill = FALSE) +
  ylab("Percent women") + 
  ggtitle(bquote(atop("Gender ratio in state houses", 
                      atop("By US state - 2014")))) +
  theme_graphzoo(base_size = 13, family = "Droid Sans Mono") + 
  theme(axis.title.y = element_blank())

g3 <- addBanner(g3, font.size = 4, heights = c(1, 0.05*7/10),
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")
print(g3)
