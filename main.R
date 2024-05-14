library(magrittr)

leagues = c("EPL", "La liga", "Serie A", "Bundesliga", "Ligue 1")
seasons = c(2023)
grid = expand.grid(leagues, seasons)

for (i in 1:nrow(grid)){
  shots = worldfootballR::understat_league_season_shots(grid[i, "Var1"], grid[i, "Var2"])
  
  results <- worldfootballR::understat_league_match_results(grid[i, "Var1"], grid[i, "Var2"]) %>% 
    dplyr::mutate(Date = lubridate::as_date(datetime)) %>% 
    dplyr::select(Date, Home = home_team, Away = away_team, match_id, 
                  home_prob = forecast_win, draw_prob = forecast_draw,
                  away_prob = forecast_loss) %>% 
    tidyr::pivot_longer(c(Home, Away), names_to = "Home_Away", values_to = "Squad")
  
  hej = get_data("raw_data") %>% 
    dplyr::select(-Home_Away, -Squad) %>% 
    dplyr::distinct()
  
  final_raw_data = dplyr::left_join(shots, results, by = "match_id") %>% 
    dplyr::select(-date)
  
  conn = DBI::dbConnect(RSQLite::SQLite(), "raw_data.sqlite")
  
  DBI::dbWriteTable(conn, "raw_data", final_raw_data, 
                    extended_types = TRUE, append = TRUE)
  
  DBI::dbDisconnect(conn)
}

hej = get_data("raw_data")

nrow(hej) / 2


hej2 = hej %>% 
  dplyr::arrange(match_id, minute) %>% 
  dplyr::mutate(goal_minute = dplyr::if_else(result == "Goal", minute, NA)) %>% 
  dplyr::group_by(match_id) %>% 
  dplyr::filter(any(result == "Goal")) %>% 
  dplyr::mutate(first_goal = min(goal_minute, na.rm = TRUE),
                first_goal_scorer = dplyr::first(h_a[minute == first_goal & result == "Goal"]),
                before_first_goal = dplyr::case_when(minute < first_goal ~ "before",
                                                     minute > first_goal ~ "after",
                                                     minute == first_goal ~ "goal"),
                scorer_team = dplyr::case_when(h_a == "a" & first_goal_scorer == "a" ~ "Scorer",
                                               h_a == "a" & first_goal_scorer == "h" ~ "Conceder",
                                               h_a == "h" & first_goal_scorer == "h" ~ "Scorer",
                                               h_a == "h" & first_goal_scorer == "a" ~ "Conceder"),
                scorer_prob = dplyr::case_when(first_goal_scorer == "a" ~ away_prob,
                                               first_goal_scorer == "h" ~ home_prob),
                conceder_prob = dplyr::case_when(first_goal_scorer == "a" ~ home_prob,
                                                 first_goal_scorer == "h" ~ away_prob)) %>% 
  dplyr::group_by(match_id, scorer_team, before_first_goal) %>% 
  dplyr::mutate(xG = sum(as.numeric(xG))) %>% 
  tidyr::pivot_wider(names_from = c(before_first_goal, scorer_team), values_from = xG) %>% 
  dplyr::select(Date, match_id, scorer_prob, draw_prob, conceder_prob, first_goal, first_goal_scorer,
                before_Scorer, after_Scorer, before_Conceder, after_Conceder) %>% 
  dplyr::group_by(match_id) %>% 
  dplyr::mutate(before_Scorer = mean(before_Scorer, na.rm = TRUE),
                after_Scorer = mean(after_Scorer, na.rm = TRUE),
                before_Conceder = mean(before_Conceder, na.rm = TRUE),
                after_Conceder = mean(after_Conceder, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct(Date, match_id, .keep_all = TRUE) %>% 
  tidyr::replace_na(list(before_Scorer = 0,
                         after_Scorer = 0,
                         before_Conceder = 0,
                         after_Conceder = 0))


