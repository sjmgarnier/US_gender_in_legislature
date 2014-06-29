# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# If the data has already been scraped from the website of the Center for 
# American Women and Politics, load the data. Otherwise...
if(file.exists("../data.csv")) {
  my.tab <- fread("../data.csv")
} else {
  # ...scrape and clean up data tables from the website of the Center for 
  # American Women and Politics. Note that we need to process Nebraska and 
  # Vermont separately. Nebraska has a unicameral legislature and Vermont a 3rd
  # party (the Progressive Party), and both disrupts the scraping process (damn 
  #you Nebraska and Vermont!!!).
  
  
  # Locate webpages containing data for each state on the website of the Center 
  # for American Women and Politics
  idx.html <- htmlParse("http://www.cawp.rutgers.edu/fast_facts/resources/state_fact_sheet.php")
  data.loc <- data.table(
    STATE = xpathSApply(idx.html, "//a[@href[contains(.,'/fast_facts/resources/state_fact_sheets/')]]", xmlValue),
    STATE.ABB = gsub(".*\\/(.*)\\..*", "\\1", 
                     xpathSApply(idx.html, "//a/@href[contains(.,'/fast_facts/resources/state_fact_sheets/')]")),
    URL = paste0("http://www.cawp.rutgers.edu",
                 xpathSApply(idx.html, "//a/@href[contains(.,'/fast_facts/resources/state_fact_sheets/')]")))
  
  
  # Prepare data table
  tab.names <- c("YEAR", "STATE.RANK", "SENATE.D", "SENATE.R", "SENATE.I", "SENATE.WOMEN", "SENATE.TOTAL",
                 "HOUSE.D", "HOUSE.R", "HOUSE.I", "HOUSE.WOMEN", "HOUSE.TOTAL", 
                 "TOTAL.WOMEN", "TOTAL.LEGISLATURE", "PERCENT.WOMEN")
  my.tab <- data.table()
  
  
  # Scrape data
  for (i in which(data.loc$STATE != "Vermont" & data.loc$STATE != "Nebraska")) {
    html <- htmlParse(data.loc$URL[i])
    html <- xpathSApply(html, "//div[@id='StateFactsTable']")[[1]]
    state.tab <- readHTMLTable(html, skip.rows = 1)
    party.col <- state.tab[1, ]
    party.col <- party.col[!is.na(party.col)]
    state.tab <- state.tab[-1, ]
    
    if (length(party.col) == 4) {
      state.tab <- cbind(state.tab[, 1:4], SENATE.I = NA, state.tab[, 5:8], 
                         HOUSE.I = NA, state.tab[, 9:13])
    } else if (length(party.col) == 5) {
      if (party.col[3] == "I") {
        state.tab <- cbind(state.tab[, 1:9], HOUSE.I = NA, state.tab[, 10:14])
      } else {
        state.tab <- cbind(state.tab[, 1:4], SENATE.I = NA, state.tab[, 5:14])
      }
    }
    
    setnames(state.tab, names(state.tab), tab.names)
    
    state.tab <- state.tab %>%
      mutate(STATE.NAME = data.loc$STATE[i],
             STATE.ABB = data.loc$STATE.ABB[i],
             STATE.RANK = as.numeric(as.character(STATE.RANK)),
             SENATE.D = as.numeric(as.character(SENATE.D)),
             SENATE.R = as.numeric(as.character(SENATE.R)),
             SENATE.I = as.numeric(as.character(SENATE.I)),
             SENATE.WOMEN = as.numeric(as.character(gsub("\\/", "", SENATE.WOMEN))),
             SENATE.TOTAL = as.numeric(as.character(SENATE.TOTAL)),
             HOUSE.D = as.numeric(as.character(HOUSE.D)),
             HOUSE.R = as.numeric(as.character(HOUSE.R)),
             HOUSE.I = as.numeric(as.character(HOUSE.I)),
             HOUSE.WOMEN = as.numeric(as.character(gsub("\\/", "", HOUSE.WOMEN))),
             HOUSE.TOTAL = as.numeric(as.character(HOUSE.TOTAL)),
             TOTAL.WOMEN = as.numeric(as.character(gsub("\\/", "", TOTAL.WOMEN))),
             TOTAL.LEGISLATURE = as.numeric(as.character(TOTAL.LEGISLATURE)),
             PERCENT.WOMEN = as.numeric(as.character(PERCENT.WOMEN)))
    
    my.tab <- rbind(my.tab, state.tab[,c(1, 16, 17, 2:15)])
  }
  
  
  # Scrape data for Nebraska
  html <- htmlParse(data.loc$URL[data.loc$STATE == "Nebraska"])
  html <- xpathSApply(html, "//div[@id='StateFactsTable']")[[1]]
  state.tab <- readHTMLTable(html, skip.rows = 1)
  party.col <- state.tab[1, ]
  party.col <- party.col[!is.na(party.col)]
  state.tab <- state.tab[-1, ]
  
  state.tab <- cbind(state.tab[, 1:4], SENATE.I = NA, state.tab[,5:8], HOUSE.I = NA, 
                     HOUSE.WOMEN = NA, HOUSE.TOTAL = NA, state.tab[, 10:12])
  
  setnames(state.tab, names(state.tab), tab.names)
  
  state.tab <- state.tab %>%
    mutate(STATE.NAME = data.loc$STATE[data.loc$STATE == "Nebraska"],
           STATE.ABB = data.loc$STATE.ABB[data.loc$STATE == "Nebraska"],
           STATE.RANK = as.numeric(as.character(STATE.RANK)),
           SENATE.D = as.numeric(as.character(SENATE.D)),
           SENATE.R = as.numeric(as.character(SENATE.R)),
           SENATE.I = as.numeric(as.character(SENATE.I)),
           SENATE.WOMEN = as.numeric(as.character(gsub("\\/", "", SENATE.WOMEN))),
           SENATE.TOTAL = as.numeric(as.character(SENATE.TOTAL)),
           HOUSE.D = as.numeric(as.character(HOUSE.D)),
           HOUSE.R = as.numeric(as.character(HOUSE.R)),
           HOUSE.I = as.numeric(as.character(HOUSE.I)),
           HOUSE.WOMEN = as.numeric(as.character(gsub("\\/", "", HOUSE.WOMEN))),
           HOUSE.TOTAL = as.numeric(as.character(HOUSE.TOTAL)),
           TOTAL.WOMEN = as.numeric(as.character(gsub("\\/", "", TOTAL.WOMEN))),
           TOTAL.LEGISLATURE = as.numeric(as.character(TOTAL.LEGISLATURE)),
           PERCENT.WOMEN = as.numeric(as.character(PERCENT.WOMEN)))
  
  my.tab <- rbind(my.tab, state.tab[,c(1, 16, 17, 2:15)])
  
  
  # Scrape data for Vermont
  html <- htmlParse(data.loc$URL[data.loc$STATE == "Vermont"])
  html <- xpathSApply(html, "//div[@id='StateFactsTable']")[[1]]
  state.tab <- readHTMLTable(html, skip.rows = 1)
  party.col <- state.tab[1, ]
  party.col <- party.col[!is.na(party.col)]
  state.tab <- state.tab[-1, ]
  
  state.tab <- cbind(state.tab[, 1:4], SENATE.I = NA, state.tab[,5:15])
  
  setnames(state.tab, names(state.tab), c(tab.names[1:10], "HOUSE.PRG", tab.names[11:15]))
  
  state.tab <- state.tab %>%
    mutate(STATE.NAME = data.loc$STATE[data.loc$STATE == "Vermont"],
           STATE.ABB = data.loc$STATE.ABB[data.loc$STATE == "Vermont"],
           STATE.RANK = as.numeric(as.character(STATE.RANK)),
           SENATE.D = as.numeric(as.character(SENATE.D)),
           SENATE.R = as.numeric(as.character(SENATE.R)),
           SENATE.I = as.numeric(as.character(SENATE.I)),
           SENATE.WOMEN = as.numeric(as.character(gsub("\\/", "", SENATE.WOMEN))),
           SENATE.TOTAL = as.numeric(as.character(SENATE.TOTAL)),
           HOUSE.D = as.numeric(as.character(HOUSE.D)),
           HOUSE.R = as.numeric(as.character(HOUSE.R)),
           HOUSE.I = as.numeric(as.character(HOUSE.I)),
           HOUSE.PRG = as.numeric(as.character(HOUSE.PRG)),
           HOUSE.WOMEN = as.numeric(as.character(gsub("\\/", "", HOUSE.WOMEN))),
           HOUSE.TOTAL = as.numeric(as.character(HOUSE.TOTAL)),
           TOTAL.WOMEN = as.numeric(as.character(gsub("\\/", "", TOTAL.WOMEN))),
           TOTAL.LEGISLATURE = as.numeric(as.character(TOTAL.LEGISLATURE)),
           PERCENT.WOMEN = as.numeric(as.character(PERCENT.WOMEN)))
  
  my.tab <- mutate(my.tab, HOUSE.PRG = NA) %>%
    rbind(state.tab[, c(1, 17, 18, 2:10, 12:16, 11)])
  setcolorder(my.tab, c(1:12, 18, 13:17))
  
  # Do a last clean up because some of the HTML tables had a footer with 
  # asterisks (consistency please!!!)
  my.tab <- filter(my.tab, !is.na(PERCENT.WOMEN))
  my.tab$YEAR <- as.numeric(gsub("\\*", "", my.tab$YEAR))

  
  # Save in a CSV file so that we don't have to do this ever again :-) 
  write.csv(my.tab, "../data.csv", row.names = FALSE)
}

