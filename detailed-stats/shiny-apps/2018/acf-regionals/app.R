library(shiny)
library(shinymaterial)
library(tidyverse)
library(scales)
library(viridis)

buzzes <- read_tsv("regionals18-tu.tsv") %>% 
  filter(!is.na(buzz_location)) %>% 
  mutate(answer = ifelse(answer %in% c("Australia", "California", "Finland"),
                         paste0(answer, " (", category, ")"),
                         answer),
         buzz_location_pct = factor(round(buzz_location_pct,2), levels = seq(0,1,.01)))
bonuses <- read_tsv("regionals18-b.tsv") %>% 
  unite(part1, answer1, value1, sep = " sanjuwefhnufweim ") %>% 
  unite(part2, answer2, value2, sep = " sanjuwefhnufweim ") %>% 
  unite(part3, answer3, value3, sep = " sanjuwefhnufweim ") %>% 
  gather(part, result, part1:part3) %>% 
  separate(result, c("answer", "value"), sep = " sanjuwefhnufweim ") %>% 
  mutate(packet_bonus_part = paste(packet, bonus, part, sep = ","),
         value = as.numeric(value)) %>% 
  filter(!is.na(team)) %>% 
  unique() %>% 
  group_by(packet_bonus_part) %>% 
  mutate(heard = n(),
         conv = sum(value/10)/n())
tossups <- buzzes %>% 
  select(answer, category, subcategory, packet) %>% 
  unique()

packets_heard <- buzzes %>% 
  mutate(tournament_room = paste(tournament, room, sep = " - ")) %>% 
  group_by(packet) %>% 
  summarize(heard = length(unique(tournament_room)))
packets_played <- buzzes %>% 
  group_by(team) %>% 
  summarize(games = length(unique(packet)))
packets_by_team <- buzzes %>% 
  count(team, packet) %>% 
  mutate(n = 1) %>% 
  spread(team, n, fill = 0)

buzzes <- left_join(buzzes, packets_heard)

all_ppg <- buzzes %>% 
  group_by(player, team) %>% 
  summarize(points = sum(buzz_value)) %>% 
  left_join(packets_played) %>% 
  mutate(ppg = points/games)

all_pdf <- buzzes %>%
  filter(buzz_value == 10, is.na(bounceback)) %>% 
  group_by(answer, buzz_location_pct) %>% 
  mutate(buzzed = n()) %>% 
  select(answer, category, subcategory, heard, buzz_location_pct, buzzed) %>% 
  unique()

all_cdf <- read_csv("all_cdf.csv")

tossup_conversions <- all_cdf %>% 
  group_by(answer, category, subcategory) %>% 
  summarize(tu_conv = max(conv),
            heard = max(heard)) %>% 
  mutate(lab = paste0("Conversion: ", scales::percent(tu_conv), " (", heard, " rooms)"))

category_cdf <- all_cdf %>% 
  group_by(category, buzz_location_pct) %>% 
  summarize(conv = sum(all_buzzed)/sum(heard))

init_select <- sample(unique(buzzes$team), 2)

ui <- material_page(
  title = "2018 ACF Regionals",
  nav_bar_color = "light-blue lighten-2",
  material_tabs(
    tabs = c(
      "Team Buzzpoints" = "team",
      "Player Buzz-PPG Distributions" = "player"
    )),
  # Define side-nav tab content
  material_tab_content(
    tab_id = "team",
    material_row(
      material_column(width = 3,
                      material_card(title = 'Plot Options',
                                    selectizeInput(
                                      "teams",
                                      "Teams",
                                      selected = c("Chicago A", "Penn A", "Berkeley A"),
                                      multiple = T,
                                      choices = buzzes %>%
                                        pull(tournament) %>%
                                        unique() %>% 
                                        map(~sort(unique(buzzes$team[buzzes$tournament == .]))) %>%
                                        setNames(unique(buzzes$tournament)),
                                      options = list(maxItems = 5)
                                    ),
                                    selectInput(
                                      "selected_cat",
                                      "Category",
                                      selected = "Literature",
                                      choices = buzzes %>%
                                        pull(category) %>%
                                        unique()
                                    ),
                                    actionButton("submit", "Submit")))),
      material_row(
        material_column(width = 12, 
                        material_card(
                          plotOutput("mainTeamPlot", height = "700px"))))
  ),
  material_tab_content(
    tab_id = "player",
    material_row(
      material_column(width = 3,
                      material_card(title = 'Plot Options',
                                    selectizeInput(
                                      "players",
                                      "Players",
                                      selected = c("Jakob D2", "Adam Silverman"),
                                      multiple = T,
                                      choices = buzzes %>%
                                        pull(team) %>%
                                        unique() %>% 
                                        map(~sort(unique(buzzes$player[buzzes$team == .]))) %>%
                                        setNames(unique(buzzes$team)),
                                      options = list(maxItems = 8)
                                    ),
                                    actionButton("player_submit", "Submit"))),
    material_column(width = 9, material_card(plotOutput("mainPlayerPlot", height = "700px"))))
  )
)

server <- shinyServer(function(input, output) {
  palette_maker <- eventReactive(input$submit, {
    palette <- c("dodgerblue", 
                 "maroon", 
                 "goldenrod",
                 "forestgreen",
                 "darkorchid")[1:length(input$teams)]
    palette <- c(palette, "gray") %>% 
      set_names(input$teams, "Other")
    return(palette)
  })
  
  packets_to_display <- eventReactive(input$submit, {
    packets_by_team %>% 
      select_at(vars(c("packet", input$teams))) %>% 
      filter_at(vars(-packet), all_vars(. == 1)) %>%
      pull(packet)
  })
  
  data_func <- eventReactive(input$submit, {
    dat <- buzzes %>% 
      filter(team %in% input$teams, 
             buzz_value == 10, 
             is.na(bounceback), 
             category == input$selected_cat,
             packet %in% packets_to_display()) %>% 
      select(answer, team, player, buzz_location_pct) %>% 
      separate(player, c("first_name", "last_name"), 
               sep = " ", remove = F, extra = "merge", fill = "left") %>% 
      mutate(initials = paste0(substr(first_name, 1, 1), 
                               substr(last_name, 1, 1)),
             buzz_location_pct = as.numeric(as.character(buzz_location_pct))) %>% 
      select(-first_name, -last_name) %>% 
      left_join(all_cdf, by = c("answer", "buzz_location_pct")) %>% 
      group_by(answer) %>% 
      mutate(buzz_rank = rank(buzz_location_pct, ties.method = "random"))
    return(dat)
  })
  
  plot_maker <- eventReactive(input$submit,
                              
                              ggplot() +
                                geom_area(data = filter(all_cdf, answer %in% data_func()$answer), 
                                          aes(x = buzz_location_pct, y = conv),
                                          fill = 'gray') +
                                geom_col(data = data_func(), 
                                         aes(x = buzz_location_pct, y = conv+ifelse(conv < .05, .1, 0), fill = team), 
                                         width = .01) +
                                geom_text(data = data_func(), 
                                          aes(x = buzz_location_pct, 
                                              y = isolate(buzz_rank*-(.45/isolate(length(input$teams)))),
                                              label = initials, 
                                              color = team), 
                                          size = 4.5, family = 'Lato') +
                                geom_line(data = filter(category_cdf, category == isolate(input$selected_cat)),
                                          aes(x = buzz_location_pct,
                                              y = conv),
                                          linetype = 'dashed') +
                                geom_text(data = filter(tossup_conversions, answer %in% data_func()$answer),
                                          aes(x = .25,
                                              y = .85,
                                              label = lab)) +
                                facet_wrap(~answer) +
                                scale_y_continuous(labels = scales::percent, 
                                                   limits = c(-0.5,1), 
                                                   breaks = seq(0,1,.25)) +
                                scale_x_continuous(labels = scales::percent,
                                                   limits = c(0,1.01), 
                                                   breaks = seq(0,1,.2)) +
                                labs(title = paste0("Top Teams on ", isolate(input$selected_cat)),
                                     x = "% of Question Elapsed", 
                                     y = "% of Rooms with a correct buzz") +
                                scale_fill_manual(values = palette_maker()) +
                                scale_color_manual(values = palette_maker()) +
                                guides(fill = guide_legend(title = "Team", title.hjust = 0.5), color = F) +
                                theme_bw() +
                                theme(text = element_text(size = 20, family = 'Lato'),
                                      panel.grid.minor = element_blank(),
                                      axis.text = element_text(size = 12),
                                      legend.key.height = unit(1.25, "cm"),
                                      panel.border = element_rect(size = 1.2))
  )
  
  output$mainTeamPlot <- renderPlot({
    plot_maker()
  })
  
  player_data_func <- eventReactive(input$player_submit, {
    player_buzzes <- buzzes %>% 
      filter(player %in% input$players, !is.na(buzz_location_pct), is.na(bounceback), buzz_value == 10) %>% 
      select(player, buzz_location_pct) %>% 
      mutate(buzz_location_pct = as.numeric(as.character(buzz_location_pct)))
    player_buzz_density <- density(player_buzzes$buzz_location_pct)
    
    buzz_dist <- map_dfr(unique(player_buzzes$player), function(playa){
      buzzes_play <- filter(player_buzzes, player == playa) %>% 
        pull(buzz_location_pct)
      
      play_density <- density(buzzes_play)
      tibble(player = playa,
             x = play_density$x,
             y = play_density$y)
    })%>% 
      left_join(select(all_ppg, player, team, ppg)) %>% 
      mutate(y = y*ppg,
             player = paste0(player, "\n", team))
    
    return(buzz_dist)
  })
  
  player_plot_maker <- eventReactive(input$player_submit,
                                     ggplot(player_data_func()) +
                                       geom_area(aes(x, y, fill = player), alpha = .7) +
                                       geom_line(aes(x, y, group = player), size = 1, alpha = .5) +
                                       scale_fill_viridis(name = "Player", discrete = T, option = 'D') +
                                       scale_x_continuous(limits = c(0,1), labels = percent) +
                                       labs(title = "Buzz distributions and points scored",
                                            x = "% of Question Elapsed",
                                            y = "Buzz KDE times PPG") + 
                                       theme_bw() +
                                       theme(text = element_text(size = 20, family = 'Lato'),
                                             panel.grid.minor = element_blank(),
                                             axis.text = element_text(size = 12),
                                             legend.key.height = unit(1.75, "cm"),
                                             panel.border = element_rect(size = 1.2))
  )
  
  output$mainPlayerPlot <- renderPlot({
    
    player_plot_maker()
  })
})

shinyApp(ui = ui, server = server)
