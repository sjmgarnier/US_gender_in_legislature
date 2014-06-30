# Load US map + Capitalize state names
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

us.state.map <- as.data.table(map_data('state', project="albers", par = c(39, 45))) %>%
  mutate(STATE.NAME = sapply(region, simpleCap))


# Load Alaska map + move + rescale
start.long <- -0.425 
start.lat <- -1.475
alaska.map <- as.data.table(map_data("world2Hires", "USA:Alaska", project="albers", par=c(39, 45))) %>%
  mutate(STATE.NAME = "Alaska") %>%
  mutate(long = long - (min(long) - start.long),
         lat = lat - (min(lat) - start.lat)) %>%
  mutate(long = (long - min(long)) * 0.35 + min(long),
         lat = (lat - min(lat)) * 0.35 + min(lat)) 


# Load Hawaii map + move + rescale
start.long <- -0.2
start.lat <- -1.45
hawaii.map <- as.data.table(map_data("world2Hires", "Hawaii", project="albers", par = c(39, 45))) %>%
  filter(long > 0.1) %>%
  mutate(STATE.NAME = "Hawaii") %>%
  mutate(long = long - (min(long) - start.long),
         lat = lat - (min(lat) - start.lat)) %>%
  mutate(long = (long - min(long)) * 1.5 + min(long),
         lat = (lat - min(lat)) * 1.5 + min(lat)) 


# Senate + house combined
us.state.map <- merge(us.state.map, filter(my.tab, YEAR == 2014), by = "STATE.NAME")
alaska.map <- merge(alaska.map, filter(my.tab, YEAR == 2014), by = "STATE.NAME")
hawaii.map <- merge(hawaii.map, filter(my.tab, YEAR == 2014), by = "STATE.NAME")

g1 <- ggplot() +
  geom_polygon(data = us.state.map, 
               aes(x = long, y = lat, group = group, fill = PERCENT.WOMEN),
               color = "white") +
  geom_polygon(data = alaska.map, 
               aes(x = long, y = lat, group = group, fill = PERCENT.WOMEN),
               color = "white") +
  geom_polygon(data = hawaii.map, 
               aes(x = long, y = lat, group = group, fill = PERCENT.WOMEN),
               color = "white") +
  coord_fixed() + 
  ggtitle(bquote(atop("Gender ratio in state legislature", 
                      atop("Senate + House, by US state - 2014")))) +
  theme_graphzoo(base_size = 13 * 10 / 7, family = "Droid Sans Mono") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 0, 0, -1), "lines"),
        legend.position = "bottom",
        legend.title.align = 0.5) + 
  scale_fill_gradient(guide = guide_colorbar(title = element_text("Percent women in Senate + House", hjust = 1),
                                             label.position = "bottom",
                                             title.position = "top",
                                             barwidth = 25)) 

g1 <- addBanner(g1, font.size = 4 * 10 / 7,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g1)


# Senate alone
g2 <- ggplot() +
  geom_polygon(data = us.state.map, 
               aes(x = long, y = lat, group = group, fill = 100 * SENATE.WOMEN / SENATE.TOTAL),
               color = "white") +
  geom_polygon(data = alaska.map, 
               aes(x = long, y = lat, group = group, fill = 100 * SENATE.WOMEN / SENATE.TOTAL),
               color = "white") +
  geom_polygon(data = hawaii.map, 
               aes(x = long, y = lat, group = group, fill = 100 * SENATE.WOMEN / SENATE.TOTAL),
               color = "white") +
  coord_fixed() + 
  ggtitle(bquote(atop("Gender ratio in state senates", 
                      atop("By US state - 2014")))) +
  theme_graphzoo(base_size = 13 * 10 / 7, family = "Droid Sans Mono") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 0, 0, -1), "lines"),
        legend.position = "bottom",
        legend.title.align = 0.5) + 
  scale_fill_gradient(guide = guide_colorbar(title = element_text("Percent women in Senate + House", hjust = 1),
                                             label.position = "bottom",
                                             title.position = "top",
                                             barwidth = 25)) 

g2 <- addBanner(g2, font.size = 4 * 10 / 7,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g2)


# House alone
g3 <- ggplot() +
  geom_polygon(data = us.state.map, 
               aes(x = long, y = lat, group = group, fill = 100 * HOUSE.WOMEN / HOUSE.TOTAL),
               color = "white") +
  geom_polygon(data = alaska.map, 
               aes(x = long, y = lat, group = group, fill = 100 * HOUSE.WOMEN / HOUSE.TOTAL),
               color = "white") +
  geom_polygon(data = hawaii.map, 
               aes(x = long, y = lat, group = group, fill = 100 * HOUSE.WOMEN / HOUSE.TOTAL),
               color = "white") +
  coord_fixed() + 
  ggtitle(bquote(atop("Gender ratio in state houses", 
                      atop("By US state - 2014")))) +
  theme_graphzoo(base_size = 13 * 10 / 7, family = "Droid Sans Mono") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1, 0, 0, -1), "lines"),
        legend.position = "bottom",
        legend.title.align = 0.5) + 
  scale_fill_gradient(guide = guide_colorbar(title = element_text("Percent women in Senate + House", hjust = 1),
                                             label.position = "bottom",
                                             title.position = "top",
                                             barwidth = 25)) 

g3 <- addBanner(g3, font.size = 4 * 10 / 7,
                l.txt = "GRAPHZOO.TUMBLR.COM", 
                r.txt = "SOURCE: CAWP.RUTGERS.EDU")

print(g3)

