library(rvest)
library(tidyverse)
library(ggimage)
library(lubridate)
library(ggpattern)
library(countrycode)
library(httr)
library(knitr)
library(kableExtra)

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

nowe_nazwy_player <- c("Team", "Player", "Games", "Win", "Lose", "Win_rate_perc", "Kills", "Deaths", "Assists", "KDA_ratio", "Creep_score", "Creep_score_min", "Gold_k", "Gold_min", "Damage_k", "Damage_min", "Kill_part_perc", "Kill_share_perc", "Gold_share_perc", "Champs_played", "Most_picked1","Most_picked2","Most_picked3")

names(player_with_champs) <- nowe_nazwy_player

player_with_champs[,3:20] <- player_with_champs[,3:20] %>%
  mutate(across(.fns = ~ parse_number(.x)))

player_with_champs %>%
  select(Team, Player, Creep_score, Gold_k) %>%
  filter(Creep_score >= 300) %>%
  arrange(-Creep_score)

player_with_champs %>%
  group_by(Team) %>%
  summarize(Team_gold_min = sum(Gold_min)) %>%
  arrange(Team_gold_min)

player_with_champs %>%
  slice_max(Champs_played, n = 10) %>%
  kbl() %>%
  kable_material_dark()

####CHAMPION STATS
url2 <- "https://lol.fandom.com/wiki/2022_Season_World_Championship/Main_Event/Champion_Statistics"
page_cs <- read_html(url2)

get_tables <- html_table(page_cs)

champ_stats <- get_tables[[5]]
champ_stats <- champ_stats[-c(1:3),-c(23:29)]

names(champ_stats) <- champ_stats[1,]
champ_stats <- champ_stats[-1,]

champ_stats[champ_stats == "-"] <- NA

nowe_nazwy_champ <- c("Champion_name", "Pick_ban_n", "Pick_ban_perc", "Banned", "Games_played", "By_n_players", "Win", "Lose", "Win_rate_perc", "Kills", "Deaths", "Assists", "KDA_ratio", "Creep_score", "Creep_score_min", "Gold_k", "Gold_min", "Damage_k", "Damage_min", "Kill_part_perc", "Kill_share_perc", "Gold_share_perc")

names(champ_stats) <- nowe_nazwy_champ

champ_stats[,2:22] <- champ_stats[,2:22] %>%
  mutate(across(.fns = ~ parse_number(.x)))

champ_stats %>%
  slice_max(Games_played, n = 10)

champ_stats %>%
  slice_max(Win_rate_perc, n = 10) %>%
  arrange(-Games_played)

champ_stats %>%
  slice_max(By_n_players, n = 10) %>%
  select(Champion_name, Games_played, By_n_players, Win_rate_perc)

champ_stats %>%
  mutate(Games_played_perc = Games_played/80*100) %>%
  select(`Nazwa bohatera` = Champion_name, `Udział w grach` = Games_played , `Udział w grach (%)` = Games_played_perc, `Zwycięstwa (%)` = Win_rate_perc) %>%
  slice_max(`Udział w grach (%)`, n = 10) %>%
  kbl() %>%
  kable_material_dark() %>%
  column_spec(1, image = spec_image(most_picked_ten, 100, 100))

champ_stats %>%
  mutate(Banned_perc = Banned/80*100) %>%
  select(`Nazwa bohatera` = Champion_name, `W fazie wyboru (na 80 gier)` = Pick_ban_n, `Zbanowano w grach (%)` = Banned_perc, `Zwycięstwa (%)` = Win_rate_perc) %>%
  slice_max(`Zbanowano w grach (%)`, n = 10) %>%
  kbl() %>%
  kable_material_dark()

#Wektor małych img bohaterów

icons_url <- "https://gol.gg/tournament/tournament-picksandbans/World%20Championship%202022/"

champ_icon_urls <- read_html(icons_url) %>% html_elements("img.champion_icon") %>% html_attr("src")

champ_icon_urls <- unlist(str_sub_all(champ_icon_urls, start = 3))

icon_names <- unlist(str_sub_all(champ_icon_urls, start = 20))

for (i in 1:length(champ_icon_urls)) {
  GET(paste0("gol.gg/_", champ_icon_urls[i]), write_disk(paste0(getwd(),"/img/mini/", icon_names[i]), overwrite = TRUE))
}

img_icon_path <- c()
for (i in 1:length(icon_names)) {
  img_icon_path <- c(img_icon_path, paste0(getwd(),"/img/mini/", icon_names[i]))
}

most_picked_ten <- c(paste0(getwd(),"/img/mini/Azir.png"),paste0(getwd(),"/img/mini/Sylas.png"),paste0(getwd(),"/img/mini/Aphelios.png"),paste0(getwd(),"/img/mini/Sejuani.png"),paste0(getwd(),"/img/mini/Viego.png"),paste0(getwd(),"/img/mini/Lucian.png"),paste0(getwd(),"/img/mini/Nami.png"),paste0(getwd(),"/img/mini/Graves.png"),paste0(getwd(),"/img/mini/Akali.png"),paste0(getwd(),"/img/mini/Aatrox.png"))

#Wektor dużych img bohaterów

champ_img_url <- "http://ddragon.leagueoflegends.com/cdn/img/champion/loading/"

nazwy_boh <- pull(champ_stats[,1])

for (i in c(1:22, 24:length(nazwy_boh))) {
  if (nazwy_boh[i] == "Renata Glasc") {
    bohater <- "Renata"
  }
  else if (nazwy_boh[i] == "Wukong") {
    bohater <- "MonkeyKing"
  }
  else if (nazwy_boh[i] %in% c("Kai'Sa", "Bel'Veth")) {
    bohater <- nazwy_boh[i] %>% str_remove_all(pattern = "'") %>% str_to_title()
  }
  else {
    bohater <- nazwy_boh[i]
    bohater <- bohater %>% str_remove_all(pattern = "\\s") 
  }
  GET(paste0(champ_img_url, bohater, "_0.jpg"), write_disk(paste0(getwd(),"/img/load2/", bohater,".jpg"), overwrite = TRUE))
}

loading_img_path <- c()

for (i in 1:length(nazwy_boh)) {
  if (nazwy_boh[i] == "Renata Glasc") {
    bohater <- "Renata"
  }
  else if (nazwy_boh[i] == "Wukong") {
    bohater <- "MonkeyKing"
  }
  else if (nazwy_boh[i] %in% c("Kai'Sa", "Bel'Veth")) {
    bohater <- nazwy_boh[i] %>% str_remove_all(pattern = "'") %>% str_to_title()
  }
  else {
    bohater <- nazwy_boh[i]
    bohater <- bohater %>% str_remove_all(pattern = "\\s") 
  }
  loading_img_path <- c(loading_img_path, paste0(getwd(), "/img/load2/", bohater, ".jpg"))
}

champ_stats <- cbind(loading_img_path, champ_stats)

bg_img_path <- paste0(getwd(),"/img/loading/bg_img.jpg")


plot_champ_stat1 <- champ_stats %>%
  slice_max(Pick_ban_n, n = 5) %>%
  arrange(-Pick_ban_n) %>%
  ggplot(aes(x = reorder(Champion_name, -Pick_ban_n), 
             y = Pick_ban_n,
             image = loading_img_path)) +
  ylim(0,80) +
  geom_hline(yintercept = c(20,40,60,80), color = "white", alpha = 0.5) +
  geom_bar_pattern(stat = "identity",
                   alpha = 0.7,
                   width = 0.6,
                   pattern = 'image',
                   pattern_type = 'expand',
                   pattern_filename = loading_img_path[1:5]) +
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

roster_table %>%
  filter(role != 'Coach') %>%
  filter(role != 'Assistant Coach') %>%
  View()

plot_roster <- roster_table %>%
  group_by(iso) %>%
  summarize(Players_from_country = n()) %>%
  #slice_max(Players_from_country, n = 10) %>%
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


#GROUPS IN PLAY IN PHASE
url3 <- "https://lol.fandom.com/wiki/2022_Season_World_Championship/Play-In"
page_pi <- read_html(url3)

pi_tables <- page_pi %>% html_table()

pi_grupa_a <- pi_tables[20][[1]]
pi_grupa_b <- pi_tables[23][[1]]

pi_grupa_a <- pi_grupa_a[-c(1:7),-c(5:10)]
names(pi_grupa_a) <- c("Position", "Team", "Win_Lose", "Win_rate")
pi_grupa_a[,2] <- c("Fnatic", "Evil Geniuses", "LOUD", "DetonatioN FocusMe", "Beyond Gaming", "Chiefs Esports Club")
pi_grupa_a <- pi_grupa_a %>% add_column(Logo = "", .after = 1)

pi_grupa_b <- pi_grupa_b[-c(1:7),-c(5:10)]
names(pi_grupa_b) <- c("Position", "Team", "Win_Lose", "Win_rate")
pi_grupa_b[,2] <- c("DRX", "Royal Never Give Up", "MAD Lions", "Saigon Buffalo", "Isurus", "Istanbul Wildcats")


logo_urls <- page_pi %>% html_elements("td.tournament-roster-logo-cell") %>% html_element("img") %>% html_attr("data-src")
logo_names <- page_pi %>% html_elements("td.tournament-roster-logo-cell") %>% html_element("img") %>% html_attr("data-image-key") %>% str_remove_all(pattern = "logo_square")

for (i in 1:length(logo_urls)) {
  GET(logo_urls[i], write_disk(paste0(getwd(),"/img/team_logo/",logo_names[i]), overwrite = TRUE))
}

pi_grupa_a %>%
  kbl() %>%
  kable_material_dark() %>%
  kable_styling(bootstrap_options = c("condensed"), full_width = F) %>%
  row_spec(1:2, background = "darkgreen") %>%
  column_spec(2, image = spec_image(pi_a_images, 60, 60))

pi_a_images <- c(paste0(getwd(),'/img/team_logo/Fnatic.png'),paste0(getwd(),"/img/team_logo/Evil_Geniuses_2020.png"),paste0(getwd(),"/img/team_logo/LOUD.png"),paste0(getwd(),"/img/team_logo/DetonatioN_FocusMe.png"),paste0(getwd(),"/img/team_logo/Beyond_Gaming.png"),paste0(getwd(),"/img/team_logo/The_Chiefs_eSports_Club.png"))

pi_b_images <- c(paste0(getwd(),'/img/team_logo/DRX.png'),paste0(getwd(),"/img/team_logo/Royal_Never_Give_Up.png"),paste0(getwd(),"/img/team_logo/MAD_Lions.png"),paste0(getwd(),"/img/team_logo/Saigon_Buffalo.png"),paste0(getwd(),"/img/team_logo/Isurus.png"),paste0(getwd(),"/img/team_logo/Istanbul_Wildcats.png"))


#FAZA GRUPOWA
url4 <- "https://lol.fandom.com/wiki/2022_Season_World_Championship/Main_Event"

page_me <- read_html(url4)

me_tables <- html_table(page_me)
me_grupa_a <- me_tables[24][[1]]
me_grupa_b <- me_tables[27][[1]]
me_grupa_c <- me_tables[30][[1]]
me_grupa_d <- me_tables[33][[1]]

me_grupa_a <- me_grupa_a[-c(1:5), -c(5:8)]
names(me_grupa_a) <- c("Miejsce", "Drużyna", "Zwycięstwa-Porażki", "Procent wygranych")
me_grupa_a[,2] <- c("T1", "EDward Gaming", "Fnatic", "Cloud9")

me_grupa_b <- me_grupa_b[-c(1:5), -c(5:8)]
names(me_grupa_b) <- c("Miejsce", "Drużyna", "Zwycięstwa-Porażki", "Procent wygranych")
me_grupa_b[,2] <- c("JD Gaming", "DWG KIA", "Evil Geniuses", "G2 Esports")

me_grupa_c <- me_grupa_c[-c(1:5), -c(5:8)]
names(me_grupa_c) <- c("Miejsce", "Drużyna", "Zwycięstwa-Porażki", "Procent wygranych")
me_grupa_c[,2] <- c("DRX", "Rogue", "Top Esports", "GAM Esports")

me_grupa_d <- me_grupa_d[-c(1:5), -c(5:8)]
names(me_grupa_d) <- c("Miejsce", "Drużyna", "Zwycięstwa-Porażki", "Procent wygranych")
me_grupa_d[,2] <- c("Gen.G", "Royal Never Give Up", "100 Thieves", "CTBC Flying Oyster")

logo_me_urls <- page_me %>% html_elements("td.tournament-roster-logo-cell") %>% html_element("img") %>% html_attr("data-src")
logo_me_names <- page_me %>% html_elements("td.tournament-roster-logo-cell") %>% html_element("img") %>% html_attr("data-image-key") %>% str_remove_all(pattern = "logo_square") %>% str_remove_all(pattern = "%28") %>% str_remove_all(pattern = "%29")

for (i in 1:length(logo_me_urls)) {
  GET(logo_me_urls[i], write_disk(paste0(getwd(),"/img/team_logo/",logo_me_names[i]), overwrite = TRUE))
}

me_a_images <- c(paste0(getwd(),'/img/team_logo/T1.png'),paste0(getwd(),"/img/team_logo/EDward_Gaming.png"),paste0(getwd(),"/img/team_logo/Fnatic.png"),paste0(getwd(),"/img/team_logo/Cloud9.png"))

me_b_images <- c(paste0(getwd(),'/img/team_logo/JD_Gaming.png'),paste0(getwd(),"/img/team_logo/DWG_KIA.png"),paste0(getwd(),"/img/team_logo/Evil_Geniuses_2020.png"),paste0(getwd(),"/img/team_logo/G2_Esports.png"))

me_c_images <- c(paste0(getwd(),'/img/team_logo/DRX.png'),paste0(getwd(),"/img/team_logo/Rogue_European_Team.png"),paste0(getwd(),"/img/team_logo/Top_Esports.png"),paste0(getwd(),"/img/team_logo/GAM_Esports.png"))

me_d_images <- c(paste0(getwd(),'/img/team_logo/Gen.G.png'),paste0(getwd(),"/img/team_logo/Royal_Never_Give_Up.png"),paste0(getwd(),"/img/team_logo/100_Thieves.png"),paste0(getwd(),"/img/team_logo/CTBC_Flying_Oyster.png"))

RColorBrewer::brewer.pal(10, "RdYlGn")
