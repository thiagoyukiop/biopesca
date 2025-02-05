# Bibliotecas -------------------------------------------------------------

if(!require(pacman)) {
  install.packages("pacman", dependencies = TRUE)
  library(pacman)
} else {
  library(pacman)
}

p_load(
  shiny, shinydashboard, shinydashboardPlus, shiny.i18n,
  dplyr, tidyverse, scales, zoo, DT, tibble,
  plotly, shinyjs, rintrojs, fresh
)

options(shiny.autoreload = TRUE)

dados_base <- read.table(
  "dados_brutos/dados_2017_2024.csv", header = TRUE, sep = ",", dec = "."
)

categorias_mantidas <- c(
  "Albacora-lage", "Albacora-bandolim", "Albacora-branca", 
  "Bonito-listrado", "Cacao-azul", "Cacao-anequim", "Meca"
)

dados <- dados_base %>%
  mutate(
    CATEGORIA = ifelse(
      CATEGORIA %in% categorias_mantidas, CATEGORIA, "Outros"
    )
  )

categoria_substituicoes <- c(
  "Albacora-bandolim" = "Albacora_bandolim",
  "Albacora-branca" = "Albacora_branca",
  "Albacora-lage" = "Albacora_lage",
  "Bonito-listrado" = "Bonito_listrado",
  "Cacao-anequim" = "Cacao_anequim",
  "Cacao-azul" = "Cacao_azul"
)

dados_ajustados <- dados %>%
  mutate(CATEGORIA = recode(CATEGORIA, !!! categoria_substituicoes))

remover_outliers <- function(dados, coluna) {
  Q1 <- quantile(dados[[coluna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dados[[coluna]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  subset(dados, dados[[coluna]] >= lim_inf & dados[[coluna]] <= lim_sup)
}

i18n <- Translator$new(
  translation_json_path = "traducoes/translations_complete.json"
)

i18n$set_translation_language("pt")

i18n$use_js()

# Interface do Usuário ----------------------------------------------------

ui <- dashboardPage(
  freshTheme = create_theme(
    adminlte_color(
      light_blue = "#5E81AC"
    ),
    adminlte_sidebar(
      width = "230px",
      dark_bg = "#D8DEE9",
      dark_hover_bg = "#81A1C1",
      dark_color = "#2E3440",
      dark_hover_color = "#1B1E28",
      dark_submenu_color = "#A9A9A9",
      dark_submenu_hover_color = "#1B1E28"
    ),
    adminlte_global(
      content_bg = "#FFFFFF",
      info_box_bg = "#D8DEE9"
    )
  ),
  scrollToTop = TRUE,
  
  # Header ------------------------------------------------------------------
  
  header = dashboardHeader(
    titleWidth = 230,
    title = uiOutput("textoHeader"),
    controlbarIcon = icon("sliders")
  ),
  
  # Sidebar -----------------------------------------------------------------
  
  sidebar = dashboardSidebar(
    width = 230,
    minified = TRUE,
    collapsed = FALSE,
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(
        text = i18n$t("apresentacao"),
        icon = icon("house"),
        menuSubItem(
          text = i18n$t("projeto"),
          tabName = "projeto",
          icon = icon("r-project")
        ),
        menuSubItem(
          text = i18n$t("leiame"),
          tabName = "leia_me",
          icon = icon("readme")
        ),
        menuSubItem(
          text = i18n$t("sobre"),
          tabName = "sobre",
          icon = icon("circle-info")
        )
      ),
      menuItem(
        text = i18n$t("distribuicao"),
        icon = icon("chart-bar"),
        menuSubItem(
          # text = i18n$t("comprimento_especie_1"),
          text = "comprimento_especie_1",
          tabName = "comprimento_especie_1",
          icon = icon("chart-pie")
        ),
        menuSubItem(
          # text = i18n$t("comprimento_especie_2"),
          text = "comprimento_especie_2",
          tabName = "comprimento_especie_2",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          # text = i18n$t("comprimento_especie_3"),
          text = "comprimento_especie_3",
          tabName = "comprimento_especie_3",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "comprimento_especie_4",
          tabName = "comprimento_especie_4",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "comprimento_especie_5",
          tabName = "comprimento_especie_5",
          icon = icon("chart-simple")
        )
      )
    )
  ),
  
  # Body --------------------------------------------------------------------
  
  body = dashboardBody(
    shiny.i18n::usei18n(i18n),
    introjsUI(),
    shiny::useBusyIndicators(),
    busyIndicatorOptions(
      spinner_type = 'bars3',
      spinner_delay = 0,
      spinner_size = 100
    ),
    tags$head(
      tags$style(
        HTML('
            
            .graficos {
            display: flex;
            width: 100%;
            # height: 30vh;
            height: 40vh;
            visibility: inherit;
            position: relative;
            z-index: 100;
            }
            
            .graficosMaiores {
            display: flex;
            width: 100%;
            # height: calc(70vh - 120px);
            height: 70vh;
            visibility: inherit;
            position: relative;
            z-index: 100;
            }
        
            .direct-chat-contacts {
              z-index: 100 !important;
            }
            
            .direct-chat-contacts p {
              text-align: justify;
              margin-right: 8px;
            }
            
            .content-wrapper {
              background-color: #FFFFFF; /* cor de fundo branca */
            }
            
            .box-header {
              text-align: center;
            }
            
            .titulos-box {
              margin-left: 25px;
              margin-right: 25px;
            }
            
            .box {
              overflow-x: auto; 
            }
            
            #LogoPTA img {
              width: 50%;
              height: auto;
            }
            
            #FluxogramaTubAzul img {
              width: 70%;
              height: auto;
            }
            
            .control-sidebar-bg {
              box-shadow: 10px 10px 10px 10px;
            }
            
             ')
      )
    ),
    tabItems(
      tabItem(
        tabName = "projeto",
        fluidRow(
          column(
            offset = 1,
            width = 5,
            infoBox(
              title = tags$div(
                h6(i18n$t("tubaroes_medidos")),
                style = "display: block; text-align: center;"
              ),
              fill = TRUE,
              width = 12,
              color = "light-blue",
              value = tags$div(
                style = "display: block; text-align: center;",
                h1(strong("28954"), style = "margin: 0px;")
              ),
              icon = icon("fish")
            )
          ),
          column(
            width = 5,
            infoBox(
              title = tags$div(
                h6(i18n$t("entrevista_desembarque")),
                style = "display: block; text-align: center;"
              ),
              fill = TRUE,
              width = 12,
              color = "light-blue",
              value = tags$div(
                style = "display: block; text-align: center;",
                h1(strong("731"), style = "margin: 0px;")
              ),
              icon = icon("paste")
            )
          )
        ),
        fluidRow(
          column(
            offset = 1,
            width = 5,
            infoBox(
              title = tags$div(
                h6(i18n$t("cadernos_bordo")),
                style = "display: block; text-align: center;"
              ),
              fill = TRUE,
              width = 12,
              color = "light-blue",
              value = tags$div(
                style = "display: block; text-align: center;",
                h1(strong("465"), style = "margin: 0px;")
              ),
              icon = icon("book-open")
            )
          ),
          column(
            width = 5,
            infoBox(
              title = tags$div(
                h6(i18n$t("embarcacoes_monitoradas")),
                style = "display: block; text-align: center;"
              ),
              fill = TRUE,
              width = 12,
              color = "light-blue",
              value = tags$div(
                style = "display: block; text-align: center;",
                h1(strong("92"), style = "margin: 0px;")
              ),
              icon = icon("ship")
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            offset = 2,
            div(
              style = "text-align: center;",
              imageOutput("LogoPTA", height = "100%")
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            offset = 2,
            tags$div(
              h3(tagList(i18n$t("projeto_texto_1"))),
              style = "text-align:center;"
            ),
            br(),
            tags$div(
              style = "text-align:center;",
              h4(tagList(i18n$t("projeto_texto_2")))
            ),
            tags$div(
              style = "text-align:justify;",
              p(
                tagList(i18n$t("projeto_texto_3")),
                tags$em("Prionace glauca", .noWS = "after"),
                tagList(i18n$t("projeto_texto_4"))
              ),
              p(tagList(i18n$t("projeto_texto_5"))),
              p(tagList(i18n$t("projeto_texto_6"))),
              p(strong(tagList(i18n$t("projeto_texto_7")))),
              p(strong(tagList(i18n$t("projeto_texto_8")))),
              p(strong(tagList(i18n$t("projeto_texto_9")))),
              p(strong(tagList(i18n$t("projeto_texto_10")))),
              p(tagList(i18n$t("projeto_texto_11"))),
              br()
            ),
            tags$div(
              style = "text-align:center;",
              h4(tagList(i18n$t("projeto_texto_12")))
            ),
            tags$div(
              style = "text-align:justify;",
              p(tagList(i18n$t("projeto_texto_13")))
            ),
            tags$div(
              style = "text-align:justify;",
              p(tagList(i18n$t("projeto_texto_14"))),
              br()
            ),
            tags$div(
              style = "text-align:center;",
              h4(tagList(i18n$t("projeto_texto_15")))
            ),
            div(
              style = "text-align: center;",
              imageOutput("FluxogramaTubAzul", height = "100%")
            )
          )
        )
      ),
      tabItem(
        tabName = "leia_me",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            tags$head(
              tags$style(
                HTML("
                  #boxWithoutHeader .box-header {
                    display: none;
                  }
                ")
              )
            ),
            box(
              id = "boxWithoutHeader",
              title = NULL,
              width = 12,
              headerBorder = FALSE,
              background = "gray",
              uiOutput("leiameMarkdown")
            )
          )
        )
      ),
      tabItem(
        tabName = "sobre",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            box(
              id = "boxWithoutHeader",
              title = NULL,
              width = 12,
              headerBorder = FALSE,
              background = "gray",
              uiOutput("sobreMarkdown")
            )
          )
        )
      ),
      tabItem(
        tabName = "comprimento_especie_1",
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("grafico_dispersao")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("DispersaoComprimentoXPeso_esp_1", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_1",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_1")))
              )
            )

          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_1")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("histograma_comprimento_esp_1", height = "100%"),
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_2",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_2")))
              )
            )
          )
        ),
        fluidRow(

          column(
            width = 6,
            box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_2")
                  )
                )
              ),
              div(
                class = "graficos",
                carousel(
                  width = 12,
                  id = "carousel_esp_1",
                  carouselItem(
                    imageOutput("blueshark1", height = "100%", width = "100%")
                  ),
                  carouselItem(
                    imageOutput("blueshark2", height = "100%", width = "100%")
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("proporcao_sexo")
                  )
                )
              ),
              div(
                class = "graficos",
                DTOutput("tabela_proporcao_esp_1", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_4",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_4")))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "comprimento_especie_2",
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("grafico_dispersao")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("DispersaoComprimentoXPeso_esp_2", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_1",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_1")))
              )
            )
            
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_1")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("histograma_comprimento_esp_2", height = "100%"),
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_2",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_2")))
              )
            )
          )
        ),
        fluidRow(
          
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_2")
                  )
                )
              ),
              div(
                class = "graficos",
                carousel(
                  width = 12,
                  id = "carousel_esp_2",
                  carouselItem(
                    imageOutput("blueshark1", height = "100%", width = "100%")
                  ),
                  carouselItem(
                    imageOutput("blueshark2", height = "100%", width = "100%")
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("proporcao_sexo")
                  )
                )
              ),
              div(
                class = "graficos",
                DTOutput("tabela_proporcao_esp_2", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_4",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_4")))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "comprimento_especie_3",
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("grafico_dispersao")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("DispersaoComprimentoXPeso_esp_3", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_1",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_1")))
              )
            )
            
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_1")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("histograma_comprimento_esp_3", height = "100%"),
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_2",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_2")))
              )
            )
          )
        ),
        fluidRow(
          
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_2")
                  )
                )
              ),
              div(
                class = "graficos",
                carousel(
                  width = 12,
                  id = "carousel_esp_3",
                  carouselItem(
                    imageOutput("blueshark1", height = "100%", width = "100%")
                  ),
                  carouselItem(
                    imageOutput("blueshark2", height = "100%", width = "100%")
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("proporcao_sexo_esp_3")
                  )
                )
              ),
              div(
                class = "graficos",
                DTOutput("tabela_proporcao_esp_3", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_4",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_4")))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "comprimento_especie_4",
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("grafico_dispersao")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("DispersaoComprimentoXPeso_esp_4", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_1",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_1")))
              )
            )
            
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_1")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("histograma_comprimento_esp_4", height = "100%"),
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_2",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_2")))
              )
            )
          )
        ),
        fluidRow(
          
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_2")
                  )
                )
              ),
              div(
                class = "graficos",
                carousel(
                  width = 12,
                  id = "carousel_esp_4",
                  carouselItem(
                    imageOutput("blueshark1", height = "100%", width = "100%")
                  ),
                  carouselItem(
                    imageOutput("blueshark2", height = "100%", width = "100%")
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("proporcao_sexo_esp_4")
                  )
                )
              ),
              div(
                class = "graficos",
                DTOutput("tabela_proporcao_esp_4", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_4",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_4")))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "comprimento_especie_5",
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("grafico_dispersao")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("DispersaoComprimentoXPeso_esp_5", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_1",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_1")))
              )
            )
            
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_1")
                  )
                )
              ),
              div(
                class = "graficos",
                plotlyOutput("histograma_comprimento_esp_5", height = "100%"),
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_2",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_2")))
              )
            )
          )
        ),
        fluidRow(
          
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("comprimento_texto_2")
                  )
                )
              ),
              div(
                class = "graficos",
                carousel(
                  width = 12,
                  id = "carousel_esp_5",
                  carouselItem(
                    imageOutput("blueshark1", height = "100%", width = "100%")
                  ),
                  carouselItem(
                    imageOutput("blueshark2", height = "100%", width = "100%")
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              status = "primary",
              headerBorder = F,
              title = strong(
                div(
                  class = "titulos-box",
                  tagList(
                    i18n$t("proporcao_sexo")
                  )
                )
              ),
              div(
                class = "graficos",
                DTOutput("tabela_proporcao_esp_5", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_4",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_4")))
              )
            )
          )
        )
      ),
    )
  ),
  
  # ControlBar --------------------------------------------------------------
  
  controlbar = dashboardControlbar(
    overlay = TRUE,
    collapsed = TRUE,
    skin = "light",
    id = "controlbar",
    width = 230,
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        title = tagList(i18n$t("filtros")),
        icon = icon("filter"),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_1'",
          sliderInput(
            inputId = "anos_captura_esp_1",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_2'",
          sliderInput(
            inputId = "anos_captura_esp_2",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_3'",
          sliderInput(
            inputId = "anos_captura_esp_3",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_4'",
          sliderInput(
            inputId = "anos_captura_esp_4",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_5'",
          sliderInput(
            inputId = "anos_captura_esp_5",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Variáveis ---------------------------------------------------------------
  
  dados_completos <- dados_ajustados %>%
    complete(CATEGORIA, ANO, MES = 1:12, fill = list(VALOR = NA))%>% 
    filter(!(ANO == 2024 & MES >= 5))
  
  dados_gerais <- dados_completos %>%
    mutate(KG = replace_na(KG, 0))
  
  nomes_meses_reativo <- reactive({
    c(
      i18n$t("mes_1"), i18n$t("mes_2"), i18n$t("mes_3"), i18n$t("mes_4"), 
      i18n$t("mes_5"), i18n$t("mes_6"), i18n$t("mes_7"), i18n$t("mes_8"), 
      i18n$t("mes_9"), i18n$t("mes_10"), i18n$t("mes_11"), i18n$t("mes_12")
    )
  })
  
  cores_categoria <- c(
    "Albacora_lage" = '#67001f',
    "Meca" = '#b2182b',
    "Cacao_anequim" = '#d6604d',
    "Bonito_listrado" = '#f4a582',
    "Outros" = '#92c5de',
    "Albacora_branca" = '#4393c3',
    "Albacora_bandolim" = '#2166ac',
    "Cacao_azul" = '#053061'
  )
  
  TabsControlbarFiltro <- c(
    "comprimento_especie_1", "comprimento_especie_2", "comprimento_especie_3"
    )
  
  TabsControlbarSemFiltro <- c("projeto", "leia_me", "sobre")
  
  # Sidebar -----------------------------------------------------------------
  
  observeEvent(input$sidebarCollapsed, {
    
    if (input$sidebarCollapsed) {
      output$textoHeader <- renderUI({NULL})
    } else {
      output$textoHeader <- renderUI({
        # tags$span(i18n$t("projeto_tubarao_azul"))
        tags$span("Biopesca")
      })
    }
  })

  # Projeto -----------------------------------------------------------------
  
  output$LogoPTA <- renderImage({
    list(
      # src = "dados_brutos/logo_tuba_azul_3.png",
      # height = "auto",
      # width = "100%",
      # contentType = "image/png",
      # id = "LogoPTA"
    )
  }, deleteFile = FALSE)
  
  output$FluxogramaTubAzul <- renderImage({
    
    # idioma <- input$selected_language
    # if (is.null(idioma) || idioma == "") {
    #   idioma <- "pt"  # Idioma padrão
    # }
    # source <- switch(
    #   idioma,
    #   "pt" = "dados_brutos/FLUXOGRAMA_pt.png",
    #   "en" = "dados_brutos/FLUXOGRAMA_en.png",
    #   "es" = "dados_brutos/FLUXOGRAMA_es.png",
    #   "dados_brutos/FLUXOGRAMA_pt.png"
    # )
    # 
    # list(
    #   src = source,
    #   height = "auto",
    #   width = "100%",
    #   contentType = "image/png"
    # )
  }, deleteFile = FALSE)
  
  # Leia-me -----------------------------------------------------------------
  
  output$leiameMarkdown <- renderUI({
    idioma <- input$selected_language
    arquivo <- switch(
      idioma,
      "pt" = "dados_brutos/README_pt.md",
      "en" = "dados_brutos/README_en.md",
      "es" = "dados_brutos/README_es.md",
      "dados_brutos/README.md"
    )
    
    shiny::includeMarkdown(arquivo)
  })
  
  # Sobre -------------------------------------------------------------------
  
  output$sobreMarkdown <- renderUI({
    idioma <- input$selected_language
    arquivo <- switch(
      idioma,
      "pt" = "dados_brutos/ABOUT_pt.md",
      "en" = "dados_brutos/ABOUT_en.md",
      "es" = "dados_brutos/ABOUT_es.md",
      "dados_brutos/ABOUT_pt.md"
    )
    
    shiny::includeMarkdown(arquivo)
  })
  
  # Distribuição de Captura Espécie 1  ----------------------------------------
  
  dados_biometria_filtro_esp_1 <- reactive({
    dados_aux <- biometria_sem_outliers %>% 
      filter(sexo == )
  })
  
  output$DispersaoComprimentoXPeso_esp_1 <- renderPlotly({
    a <- 0.00013
    b <- 2.5362
    
    biometria_esp_1 <- dados_biometria_filtro_esp_1()
    
    biometria.nls_esp_1 <- nls (
      formula = y ~ a*x^b, 
      data = biometria_esp_1,
      start = list(
        a = a,
        b = b
      ),
      control = nls.control(maxiter = 200)
    )
    
    # Valores do confint
    conf <- confint(biometria.nls)
    a_lower <- conf["a", "2.5%"]
    a_upper <- conf["a", "97.5%"]
    b_lower <- conf["b", "2.5%"]
    b_upper <- conf["b", "97.5%"]
    
    new_data <- data.frame(
      x = seq(
        min(biometria$x),
        max(biometria$x),
        length.out = 100)
    )
    # Calcular as curvas ajustadas
    new_data$fit <- coef(biometria.nls)["a"]*new_data$x^coef(biometria.nls)["b"]
    new_data$lwr <- a_lower * new_data$x^b_lower  # Limite inferior
    new_data$upr <- a_upper * new_data$x^b_upper  # Limite superior
    
    biometria$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria$y, 2), " kg"
    )
    
    new_data$curva <- paste0(
      i18n$t("comprimento_legenda"), round(new_data$x, 2 ), " cm<br>",
      i18n$t("lim_sup_legenda"), round(new_data$upr, 2), " kg<br>",
      i18n$t("curva_legenda"), round(new_data$fit, 2), " kg<br>",
      i18n$t("lim_inf_legenda"), round(new_data$lwr, 2), " kg"
    )
    
    p <- ggplot() +
      geom_line(
        data = new_data,
        aes(x = x, y = fit),
        color = "blue",
        linewidth = 0.6
      ) +  # Linha ajustada
      geom_point(
        data = biometria,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      geom_ribbon(
        data = new_data,
        aes(x = x, ymin = lwr, ymax = upr),
        fill = "blue",
        alpha = 0.2
      ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()
    
    # Tornar o gráfico interativo
    ggplotly(p) %>% 
      plotly::style(
        hoverinfo = "text",
        text = biometria$XvsY, 
        traces = 2
      ) %>% 
      plotly::style(
        hoverinfo = "text",
        text = new_data$curva,
        traces = 1
      ) %>% 
      plotly::style(
        hoverinfo = "none",
        traces = 3
      ) %>% 
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro(),
      x = ~x,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      customdata = ~case_when(
        sexo == "Macho (M)" ~ i18n$t("macho"),
        sexo == "Fêmea (F)" ~ i18n$t("femea"),
        TRUE ~ sexo
      ),
      colors = cores_sexo,
      hoverinfo = "none",
      hovertemplate = paste(
        " ", i18n$t("sexo"), ": %{customdata} <br>",
        i18n$t("legenda_intervalo"), ": %{x}<br>",
        i18n$t("frequencia"), ": %{y}<extra></extra>"
      )
    ) %>%
      layout(
        title = NULL,
        yaxis = list(
          title = i18n$t("frequencia"),
          showgrid = FALSE
        ),
        xaxis = list(
          title = i18n$t("legenda_comprimento")
        ),
        barmode = "stack",
        # hovermode = "x",
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  output$tabela_proporcao <- renderDT({
    
    dados_tabela <- biometria_sem_outliers %>% 
      group_by(sexo) %>% 
      summarise(
        n = n()
      ) %>% 
      mutate(
        prop = round((n / sum(n)) * 100, 2)
      ) %>% 
      ungroup() %>% 
      mutate(
        sexo = case_when(
          sexo == "Macho (M)" ~ i18n$t("macho"),
          sexo == "Fêmea (F)" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela) <- c(i18n$t("sexo"), "Total", i18n$t("porcentagem"))
    
    datatable(
      data = dados_tabela,
      rownames = FALSE,
      filter = "none",
      options = list(
        paging = FALSE,
        searching = FALSE,
        columnDefs = list(
          list(
            className = 'dt-center',
            targets = "_all"
          )
        ),
        class = "cell-border stripe hover",
        selection = "single"
      )
    )
  })
  
  output$blueshark1 <- renderImage({
    list(
      src = "www/Blue_shark_1.png",
      contentType = "image/png",
      height = "auto",
      width = "100%" 
    )
  }, deleteFile = FALSE)
  
  output$blueshark2 <- renderImage({
    list(
      src = "www/Blue_shark_2.png",
      contentType = "image/png",
      height = "auto",
      width = "100%" 
    )
  }, deleteFile = FALSE)
  
  # ControlBar --------------------------------------------------------------
  
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })
  
  observeEvent(input$sidebarMenu, {
    if (
      input$sidebarMenu %in% TabsControlbarFiltro && isFALSE(input$controlbar)
    ) {
      updateControlbar("controlbar")
    }
    
    if (
      input$sidebarMenu %in% TabsControlbarSemFiltro && isTRUE(input$controlbar)
    ) {
      updateControlbar("controlbar")
    }
  })
  

  # Modal Dialog ------------------------------------------------------------

  output$Lema <- renderImage({
    list(
      src = "dados_brutos/LEMA.png",
      height = "auto",
      width = "150wv",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)

  output$TituloModal <- renderUI({
    div(
      style = "text-align: center;",
      imageOutput("Lema", height = "100%"),
      h2(i18n$t("intro_texto_1")),
      h2(strong(i18n$t("intro_texto_2"))),
      tags$small(i18n$t("intro_texto_3"))
    )
  })

  output$ConteudoModal <- renderUI({
    tagList(
      h5(strong(i18n$t("intro_texto_4"))),
      tags$ul(
        tags$li(i18n$t("intro_texto_5")),
        tags$li(i18n$t("intro_texto_6")),
        tags$li(i18n$t("intro_texto_7")),
        tags$li((i18n$t("intro_texto_8")))
      )
    )
  })

  showModal(
    modalDialog(
      title = uiOutput("TituloModal"),
      fade = TRUE,
      easyClose = TRUE,
      uiOutput("ConteudoModal"),
      footer = tagList(
        div(
          class = "pull-left",
          style = "text-align: left",
          selectInput(
            inputId = "selected_language",
            label = NULL,
            width = "135px",
            choices = setNames(
              i18n$get_languages()[-1],
              c("🇧🇷 - Português","🇬🇧 - English", "🇪🇸 - Español")
            ),
            selected = i18n$get_key_translation()
          )
        ),
        modalButton(
          "Fechar",
          icon = icon("times")
        )
      )
    )
  )


  # Rintrojs ----------------------------------------------------------------

  helptext <- reactive({
    data.frame(
      tab = c("tour_button", "tour_button"),
      step = c(1, 2),
      element = c(
        "#sidebarItemExpanded",
        "#controlbarMenu"
      ),
      intro = c(
        i18n$t("step_abas"),
        i18n$t("step_filtro")
      ),
      position = c("right", "left"),
      stringsAsFactors = FALSE
    )
  })

  observeEvent(input$tour_button, {
    removeModal()
    updateControlbar("controlbar")
  })

  observeEvent(input$exit_tour, {
    removeModal()
    updateControlbar("controlbar")
  })

  observeEvent(
    input$tour_button,
    introjs(
      session,
      options = list(
        "showBullets" = FALSE,
        "showProgress" = TRUE,
        "showStepNumbers" = FALSE,
        "nextLabel" = i18n$t("proximo"),
        "prevLabel" = i18n$t("anterior"),
        "skipLabel" = i18n$t("sair"),
        "doneLabel" = i18n$t("concluido"),
        "scrollToElement" = TRUE,
        steps = subset(helptext(), tab == "tour_button")
      )
    )
  )
}

shinyApp(ui, server)
