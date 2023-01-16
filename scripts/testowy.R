library(rvest)
library(tidyverse)
library(ggimage)
library(lubridate)
library(ggpattern)
library(countrycode)

####PLAYER STATS

url1 <- "https://lol.fandom.com/wiki/2022_Season_World_Championship/Main_Event/Player_Statistics"
page_sg <- read_html(url1)

statystyki_graczy <- html_table(page_sg)

player_stats_tibble <- statystyki_graczy[[5]]

clean_player_stats <- player_stats_tibble[-c(1:3),-c(1,21:27)]
names(clean_player_stats) <- clean_player_stats[1,]

clean_player_stats <- clean_player_stats[-1,]

champs_all_page <- page_sg %>%
  html_elements("span.sprite.champion-sprite") %>%
  html_attr("title")

champs_player_table <- champs_all_page[-c(1:10)]

players1to15 <- champs_player_table[1:45]

player16 <- champs_player_table[46]

players17to21 <- champs_player_table[47:61]

player22 <- champs_player_table[62]

players23to82 <- champs_player_table[63:242]

vec <- c(players1to15, player16, NA, NA, players17to21, player22, NA, NA, players23to82)

champs_most_picked <- as_tibble(matrix(data = vec, ncol = 3, byrow = T), .name_repair = "unique")

names(champs_most_picked) <- c("Most_picked1","Most_picked2","Most_picked3")

player_with_champs <- cbind(clean_player_stats, champs_most_picked)

names(player_with_champs)[1] <- "Player"

Team <- c(rep("100 Thieves", times = 5), rep("Cloud9", times = 5), rep("CTBC Flying Oyster", times = 6), rep("DRX", times = 6), rep("DWG KIA", times = 5), rep("EDward Gaming", times = 5), rep("Evil Geniuses.NA", times = 5), rep("Fnatic", times = 5), rep("G2 Esports", times = 5), rep("GAM Esports", times = 5), rep("Gen.G", times = 5), rep("JD Gaming", times = 5), rep("Rogue", times = 5), rep("Royal Never Give Up", times = 5), rep("T1", times = 5), rep("Top Esports", times = 5))

player_with_champs <- cbind(Team, player_with_champs)


####CHAMPION STATS
url2 <- "https://lol.fandom.com/wiki/2022_Season_World_Championship/Main_Event/Champion_Statistics"
page_cs <- read_html(url2)

get_tables <- html_table(page_cs)

champ_stats <- get_tables[[5]]
champ_stats <- champ_stats[-c(1:3),-c(23:29)]

names(champ_stats) <- champ_stats[1,]
champ_stats <- champ_stats[-1,]

champ_stats[champ_stats == "-"] <- NA

nowe_nazwy <- c("Champion_name", "Pick_ban_n", "Pick_ban_perc", "Banned", "Games_played", "By_n_players", "Win", "Lose", "Win_rate_perc", "Kills", "Deaths", "Assists", "KDA_ratio", "Creep_score", "Creep_score_min", "Gold_k", "Gold_min", "Damage_k", "Damage_min", "Kill_part_perc", "Kill_share_perc", "Gold_share_perc")

names(champ_stats) <- nowe_nazwy

champ_stats[,2:22] <- champ_stats[,2:22] %>%
  mutate(across(.fns = ~ parse_number(.x)))



#Wektor dużych img bohaterów
load_img_path <- c(paste0(getwd(),"/img/loading/Aatrox_0.jpg"), paste0(getwd(),"/img/loading/Yuumi_0.jpg"),  paste0(getwd(),"/img/loading/Sejuani_0.jpg"), paste0(getwd(),"/img/loading/Caitlyn_0.jpg"), paste0(getwd(),"/img/loading/Azir_0.jpg"), paste0(getwd(),"/img/loading/Sylas_0.jpg"), paste0(getwd(),"/img/loading/Graves_0.jpg"), paste0(getwd(),"/img/loading/Maokai_0.jpg"), paste0(getwd(),"/img/loading/Lucian_0.jpg"), paste0(getwd(),"/img/loading/Viego_0.jpg"), paste0(getwd(),"/img/loading/Akali_0.jpg"))

load_img_path <- c(load_img_path, rep(NA, times = 83))

champ_stats <- cbind(load_img_path, champ_stats)

bg_img_path <- paste0(getwd(),"/img/loading/bg_img.jpg")


plot_champ_stat1 <- champ_stats %>%
  slice_max(Pick_ban_n, n = 5) %>%
  arrange(-Pick_ban_n) %>%
  ggplot(aes(x = reorder(Champion_name, -Pick_ban_n), 
             y = Pick_ban_n,
             image = load_img_path)) +
  ylim(0,80) +
  geom_hline(yintercept = c(20,40,60,80), color = "white", alpha = 0.5) +
  geom_bar_pattern(stat = "identity",
                   alpha = 0.7,
                   width = 0.6,
                   pattern = 'image',
                   pattern_type = 'expand',
                   pattern_filename = load_img_path[1:5]) +
  geom_hline(yintercept = c(0), 
             color = "white", 
             alpha = 0.5) +
  geom_text(aes(y = Pick_ban_n, 
                label = Pick_ban_n, 
                size = 10 ,vjust = 1.5), 
            color = "white", 
            fontface = "bold") +
  labs(caption = "Źródło: https://lol.fandom.com\nOpracowanie własne, Szymon Olędzki", color = "white") +
  theme(axis.text.x = element_text(vjust = 8, color = "white", face = "bold", size = 14),
        axis.text.y = element_text(hjust = 1.5, color = "white", face = "bold", size = 11),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(color = "white", face = "italic", size = 9),
        legend.position = "none"
  )


ggbackground(gg = plot_champ_stat1, background = bg_img_path)


#Pobieranie miniatur bohaterów
#url4 <- "https://gol.gg/_img/champions_icon/"

#Popularność League of Legends
url5 <- "https://activeplayer.io/league-of-legends/"

page_activeplayer <- read_html(url5)

ap_tables <- html_table(page_activeplayer)

lol_popularity <- ap_tables[[2]]

lol_popularity <- lol_popularity[-1,]

miesiące <- month(parse_date_time(lol_popularity$Month, "BdY"))
lata <- year(parse_date_time(lol_popularity$Month, "BdY"))

lol_popularity <- lol_popularity[,-c(1,3,4)]

lol_popularity <- cbind(Year = lata, Month = miesiące, lol_popularity)

lol_popularity[,c(3,4)] <- lol_popularity[,c(3,4)] %>%
  mutate(across(.fns = ~ parse_number(.x)))

plot_pop <- lol_popularity %>%
  group_by(Year) %>%
  summarize(Avg = mean(`Average Monthly Players`)) %>%
  mutate(Avg = round(Avg/1000000, digits = 1)) %>%
  as_tibble() %>%
  ggplot(aes(x = Year, 
             y = Avg)) +
  geom_hline(yintercept = c(50, 100, 150), color = "white", alpha = 0.5) +
  geom_bar(stat = "identity", 
           alpha = 0.99, 
           fill = '#C89B3C', 
           width = 0.6) +
  geom_text(aes(y = Avg, 
                label = paste0(Avg, " mln"), 
                vjust = 1.5), 
            color = "white", 
            fontface = "bold") +
  labs(caption = "Źródło: https://activeplayer.io\nOpracowanie własne, Szymon Olędzki", color = "white") +
  ylim(0,155) +
  geom_hline(yintercept = c(0), color = "white", alpha = 0.5) +
  theme(axis.text.x = element_text(vjust = 8, color = "white", face = "bold", size = 14),
        axis.text.y = element_text(hjust = 1, color = "white", face = "bold", size = 11),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.caption = element_text(color = "white", face = "italic", size = 9)
        )

ggbackground(gg = plot_pop, background = bg_img_path)

#TEAM ROSTERS
url6 <- "https://lol.fandom.com/wiki/2022_Season_World_Championship/Main_Event/Team_Rosters"

page_tr <- read_html(url6)

tr_ids <- page_tr %>% html_elements(".multirow-highlighter .extended-rosters-id") %>% html_text()
tr_roles <- page_tr %>% html_elements("span.sprite.role-sprite") %>% html_attr("title")
tr_country <- page_tr %>% html_elements("span.sprite.country-sprite") %>% html_attr("title")
tr_residency <- page_tr %>% html_elements("div.region-icon") %>% html_text()
tr_country_iso <- countrycode(tr_country, "country.name", "iso2c")

roster_table <- tibble(id = tr_ids, role = tr_roles, country = tr_country, region = tr_residency, iso = tr_country_iso)

plot_roster <- roster_table %>%
  group_by(iso) %>%
  summarize(Players_from_country = n()) %>%
  slice_max(Players_from_country, n = 10) %>%
  ggplot(aes(x = reorder(iso, Players_from_country),
             y = Players_from_country)) +
  geom_hline(yintercept = c(10, 20, 30, 40), color = "white") +
  geom_bar(stat = "identity", fill = "#C89B3C") +
  geom_text(aes(y = Players_from_country, 
                label = Players_from_country, 
                hjust = 1.5), 
            color = "white", 
            fontface = "bold") +
  expand_limits(y = -1) +
  geom_flag(y = -2, mapping = aes(image = iso)) +
  labs(y = "Liczba zawodników") +
  geom_hline(yintercept = c(0), color = "white") +
  coord_flip() +
  labs(caption = "Źródło: https://lol.fandom.com\nOpracowanie własne, Szymon Olędzki", color = "white") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(vjust = 1, color = "white", face = "bold", size = 14),
        axis.text.y = element_text(hjust = 1, color = "white", face = "bold", size = 11),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(color = "white", face = "italic", size = 9)
        )

ggbackground(gg = plot_roster, background = bg_img_path)
  