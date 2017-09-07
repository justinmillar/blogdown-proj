df <- read.csv("data/nba-records.csv", header = T)


library(tidyverse)

# Separate record
nba <- df %>% na.omit() %>% 
  separate(Record, c("Wins", "Losses")) %>% 
  separate(Year, c("Year", "End", "ShortSeason")) %>% 
  select(Year, Team, Wins, Losses) %>% 
  mutate(Year = as.numeric(Year),
         Wins = as.numeric(Wins), 
         Losses = as.numeric(Losses)) %>% 
  mutate(Games = Wins + Losses) %>% 
  filter(Games == 82) %>% 
  arrange(Year, Wins) %>%
  mutate(rank = row_number()) %>% 
  group_by(Year) %>%
  summarise(Best = Wins[rank==max(rank)],
            Best2 = Wins[rank==max(rank)-1],
            Best3 = Wins[rank==max(rank)-2],
            Worst = Wins[rank==min(rank)],
            Worst2 = Wins[rank==min(rank)+1],
            Worst3 = Wins[rank==min(rank)+2]) %>% 
  mutate(Top3 = (Best + Best2 + Best3)/3, 
         Bottom3 = (Worst + Worst2 + Worst3)/3)
  
nba

plot_range <- ggplot(data = nba, aes(x = Year)) +
  geom_smooth(aes(y = Worst), method = "lm", color = "black") +
  geom_point(aes(y = Worst, fill = Worst), pch = 21, size = 2) +
  geom_smooth(aes(y = Best), method = "lm", , color = "black") +
  geom_point(aes(y = Best, fill = Best), pch = 21, size = 2) +
  scale_fill_distiller(type = "div", palette = "Spectral", guide = F) +
  ylab("Wins") + ggtitle("Best vs. Worst Records by Season")

plot_avg <- ggplot(data = nba, aes(x = Year)) +
  geom_smooth(aes(y = Bottom3), method = "lm", color = "black") +
  geom_point(aes(y = Bottom3, fill = Bottom3), pch = 21, size = 2) +
  geom_smooth(aes(y = Top3), method = "lm", color = "black") +
  geom_point(aes(y = Top3, fill = Top3), pch = 21, size = 2) +
  scale_fill_distiller(type = "div", palette = "Spectral", guide = F) +
  ylab("Average Wins") + ggtitle("Top 3 vs. Bottom 3 Records by Season")

library(gridExtra)
grid.arrange(plot_range, plot_avg, ncol=2)











