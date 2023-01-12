library(rvest)
library(tidyverse)

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
