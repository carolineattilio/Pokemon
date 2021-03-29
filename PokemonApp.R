pacman::p_load('shinydashboard','shiny','ggplot2','tidyverse','DT','fresh')

setwd("C:/Users/nzn/Pokémon")


### Imagens
poketab <- data.table::fread('pokeinfo.csv', na.strings = '')

df <- read.csv2("Pokemon_Completo.csv", header = TRUE, sep = ",")
df$X. <- NULL

#dt_tab <- poketab[, .(species_id, name, height, weight, type_1, type_2)]
#dt_tab[, sprint := paste0('<img src="pokemon-imagens/', species_id, '.png", height=52></img>')] 
#dt_tab <- select(dt_tab,name, sprint) %>%
#  rename(Name = name)


##### df 
#dfcompleto <- read.csv2("Pokemon_Completo.csv", header = TRUE, sep = ",") 
#dfcompleto$X. <- NULL

## Unindo os dfs
#df<-left_join(dfcompleto,dt_tab,by="Name")


#### Imagem 
#df <- select(df, -sprint)


### Criando tema
tema_pokemon <- create_theme(
  adminlte_color(
    red = "#f01616",
    light_blue = '#233675'
  ),
  adminlte_sidebar(
    dark_bg = '#233675'
  )
)

# Totais - Caixas superiores

totalPokemon  = df %>% 
  distinct(Name) %>% 
  count()

totalTipos  = df %>% 
  distinct(Type.1) %>% 
  count()

totalGeracao  = df %>% 
  distinct(Generation) %>% 
  count()


# Totais - Caixas superiores

pokemon  = unique(as.character(df$Name))

tipo  = unique(as.character(df$Type.1))

geracao  = unique(as.character(df$Generation))

###### User interface (UI)
ui <- fluidPage(dashboardPage(
  dashboardHeader(title = "Pokémon"),
  dashboardSidebar(
    selectInput("selPokemon",
                "Nome do Pokémon",
                c("Todos",
                  pokemon)),
    selectInput("selTipo",
                "Tipo do Pokémon",
                c("Todos",
                  tipo)),
    selectInput("selGeracao",
                "Geração do Pokémon",
                c("Todos",
                  geracao)),
    downloadButton("downloadCsv", "Download", style = "color: #fff; 
                   background-color: #27ae60; border-color: #fff; 
                   display:inline-block;margin-left: 22%")
  ),
  dashboardBody(
    use_theme(tema_pokemon),
    fluidRow(
      valueBoxOutput("Pokemon"),
      valueBoxOutput("Tipos"),
      valueBoxOutput("Geracao")  
    ),
    fluidRow(
      box(title = "Tabela - Pokémon", status = "primary", 
          solidHeader = TRUE, width = 12, 
          DT::DTOutput("tabelapokemon")))
    )
  )
)

###### Server Function (server) ###########


server <-function(input, output) {
  
  ### Total de Pokemons
  output$Pokemon <- renderValueBox({
    valueBox(
      value = format(totalPokemon),
      subtitle = "Pokémons",
      color = "red" 
    )
  })
  
  #### Total de Tipos
  output$Tipos <- renderValueBox({
    valueBox(
      value = format(totalTipos),
      subtitle = "Tipos",
      color = "red" 
    )
  })
  
  #### Total de Gerações
  output$Geracao <- renderValueBox({
    valueBox(
      value = format(totalGeracao),
      subtitle = "Gerações",
      color = "red" 
    )
  })
  
  ### Tabela Pokémon
  output$tabelapokemon <- DT::renderDataTable(DT::datatable({
    data <- df
    if (input$selPokemon != "Todos") {
      data <- data[data$Name == input$selPokemon,]
    }
    if (input$selTipo != "Todos") {
      data <- data[data$Type.1 == input$selTipo,]
    }
    if (input$selGeracao != "Todos") {
      data <- data[data$Generation == input$selGeracao,]
    }
    data
  }))
  
  # Download csv
  output$downloadCsv <- downloadHandler(
    filename = "dados_pokemon.csv",
    content = function(file) {
      write.csv(df, file)
    },
    contentType = "text/csv"
  )
}  


###### Shiny App ########

shinyApp(ui, server)

