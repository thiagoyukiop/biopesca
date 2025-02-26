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

remover_outliers <- function(dados, coluna) {
  Q1 <- quantile(dados[[coluna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dados[[coluna]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  subset(dados, dados[[coluna]] >= lim_inf & dados[[coluna]] <= lim_sup)
}

osseos_modificado <- read.csv(
  "dados_brutos/osseos_modificado.csv",
  stringsAsFactors = FALSE,
  sep = ";",
  dec = ".",
  fileEncoding = "UTF-8"
)

osseos_alterado <- osseos_modificado %>% 
  mutate(
    peso_total_kg = peso_total / 1000
    )

i18n <- Translator$new(
  translation_json_path = "traducoes/translations_complete.json"
)

i18n$set_translation_language("pt")

i18n$use_js()

# Interface do Usuário ----------------------------------------------------

ui <- dashboardPage(
  freshTheme = create_theme(
    adminlte_color(
      light_blue = "#FF6961"
    ),
    adminlte_sidebar(
      width = "280px",
      dark_bg = "#D8DEE9",
      dark_hover_bg = "#FF6F61",
      dark_color = "#2E3440",
      dark_hover_color = "#FFFFFF",
      dark_submenu_color = "#A9A9A9",
      dark_submenu_hover_color = "#FF6F61"
    ),
    adminlte_global(
      content_bg = "#FFFFFF",
      info_box_bg = "#FF6F61"
    )
  ),
  scrollToTop = TRUE,
  
  # Header ------------------------------------------------------------------
  
  header = dashboardHeader(
    titleWidth = 300,
    title = uiOutput("textoHeader"),
    controlbarIcon = icon("sliders")
  ),
  
  # Sidebar -----------------------------------------------------------------
  
  sidebar = dashboardSidebar(
    width = 400,
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
        text = i18n$t("distribuicao_comprimento"),
        icon = icon("chart-bar"),
        menuSubItem(
          text = "Micropogonias furnieri",
          tabName = "comprimento_especie_1",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "Pomatomus saltatrix",
          tabName = "comprimento_especie_2",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "Mugil liza",
          tabName = "comprimento_especie_3",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "Oligoplites saliens",
          tabName = "comprimento_especie_4",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "Trichiurus lepturus",
          tabName = "comprimento_especie_5",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "Scomberomorus brasiliensis",
          tabName = "comprimento_especie_6",
          icon = icon("chart-simple")
        ),
        menuSubItem(
          text = "Oligoplites saurus",
          tabName = "comprimento_especie_7",
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
            
            .control-sidebar-bg {
              box-shadow: 10px 10px 10px 10px;
            }
            
             ')
      )
    ),
    tabItems(
      tabItem(
        tabName = "projeto",
        # fluidRow(
        #   column(
        #     width = 8,
        #     offset = 2,
        #     div(
        #       style = "text-align: center;",
        #       imageOutput("LogoPTA", height = "100%")
        #     )
        #   )
        # ),
        fluidRow(
          column(
            width = 8,
            offset = 2,
            tags$div(
              h3("Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                 Donec aliquam ac lacus dapibus malesuada. Aliquam vitae 
                 quam nisl. Class aptent taciti sociosqu ad litora torquent."),
              style = "text-align:center;"
            ),
            br(),
            tags$div(
              style = "text-align:center;",
              h4("Lorem ipsum dolor sit amet.")
            ),
            tags$div(
              style = "text-align:justify;",
              p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                Suspendisse laoreet, justo quis posuere ultricies, libero 
                justo interdum risus, eget interdum tellus quam a augue."),
              p(" Vestibulum porta, augue suscipit bibendum tincidunt, sem sem 
                ultrices mauris, vel semper nisi leo a dolor. Pellentesque 
                mattis felis ac nisi rhoncus rhoncus. Fusce congue non orci 
                sed rutrum."),
              p("Ut ultrices vehicula enim, sit amet interdum magna malesuada 
                nec. Etiam cursus cursus tincidunt. Aliquam cursus velit id 
                urna scelerisque, in scelerisque justo finibus."),
              p(strong("Suspendisse tempor ipsum ex, sed euismod ligula 
                       convallis sit amet. Vivamus interdum ultrices metus 
                       pulvinar convallis.")),
              p(strong("Aliquam lobortis magna et eros interdum, in aliquet diam
                       hendrerit. Maecenas mollis consectetur purus, ac accumsan
                       nunc rhoncus nec.")),
              p(strong("Sed viverra nunc libero, eget porta nisl elementum sit 
                       amet. Aenean porta aliquam metus non eleifend.")),
              p(strong("Praesent sit amet maximus risus. Proin hendrerit sem 
                       vitae libero pulvinar facilisis. Proin in consectetur
                       nisi.")),
              p("Nunc consectetur nunc et finibus porttitor. Nulla facilisi. In
                facilisis commodo luctus. Fusce accumsan fringilla odio, pretium
                convallis metus. In finibus mi a ante maximus porta. Nullam sed
                tellus porttitor, sagittis diam nec, imperdiet erat. Proin sed 
                justo sem. Integer ac cursus sem. Pellentesque porta imperdiet
                tellus, vel interdum turpis mollis euismod."),
              br()
            ),
            tags$div(
              style = "text-align:center;",
              h4("Maecenas nec faucibus lacus.")
            ),
            tags$div(
              style = "text-align:justify;",
              p("Duis at scelerisque metus, finibus tincidunt lorem. Phasellus 
                ac quam eu justo aliquam volutpat. Aliquam feugiat condimentum 
                lectus in consequat. Maecenas aliquet, nulla ut blandit iaculis,
                ante nisi tempor sapien, nec bibendum risus mauris id nulla.
                Donec non nunc sit amet diam tristique venenatis.")
            ),
            tags$div(
              style = "text-align:justify;",
              p("Curabitur sit amet magna sit amet quam elementum venenatis quis
                ut metus. Mauris sollicitudin viverra odio quis auctor. Integer
                dui mauris, sollicitudin porta tristique nec, cursus eu ex.
                Integer venenatis finibus vestibulum."),
              br()
            ),
            tags$div(
              style = "text-align:center;",
              h4("Aliquam convallis blandit est, id posuere leo faucibus in. 
                 Etiam malesuada mollis pulvinar. Nullam blandit volutpat ante 
                 sed faucibus. Nulla tristique blandit dapibus.")
            )#,
            # div(
            #   style = "text-align: center;",
            #   imageOutput("FluxogramaTubAzul", height = "100%")
            # )
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
        # fluidRow(
        #   div(
        #     style = "text-align: center; margin-bottom:30px;",
        #     h1("Micropogonias furnieri")
        #   )
        # ),
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
                plotlyOutput("histograma_comprimento_esp_1", height = "100%")
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
              imageOutput("image_1_esp_1", height = "100%", width = "100%")
              # carousel(
              #   width = 12,
              #   id = "carousel_esp_1",
              #   carouselItem(
              #     imageOutput("image_1_esp_1", height = "100%", width = "100%")
              #   ),
              #   carouselItem(
              #     imageOutput("image_2_esp_1", height = "100%", width = "100%")
              #   )
              # )
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
              DTOutput("tabela_proporcao_esp_1", height = "100%"),
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
                plotlyOutput("histograma_comprimento_esp_2", height = "100%")
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
              imageOutput("image_1_esp_2", height = "100%", width = "100%")
              # carousel(
              #   width = 12,
              #   id = "carousel_esp_2",
              #   carouselItem(
              #     imageOutput("image_1_esp_2", height = "100%", width = "100%")
              #   ),
              #   carouselItem(
              #     imageOutput("image_2_esp_2", height = "100%", width = "100%")
              #   )
              # )
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
              DTOutput("tabela_proporcao_esp_2", height = "100%"),
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
                plotlyOutput("histograma_comprimento_esp_3", height = "100%")
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
              imageOutput("image_1_esp_3", height = "100%", width = "100%")
              # carousel(
              #   width = 12,
              #   id = "carousel_esp_3",
              #   carouselItem(
              #     imageOutput("image_1_esp_3", height = "100%", width = "100%")
              #   ),
              #   carouselItem(
              #     imageOutput("image_2_esp_3", height = "100%", width = "100%")
              #   )
              # )
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
              DTOutput("tabela_proporcao_esp_3", height = "100%"),
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
                plotlyOutput("histograma_comprimento_esp_4", height = "100%")
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
              imageOutput("image_1_esp_4", height = "100%", width = "100%")
              # carousel(
              #   width = 12,
              #   id = "carousel_esp_4",
              #   carouselItem(
              #     imageOutput("image_1_esp_4", height = "100%", width = "100%")
              #   ),
              #   carouselItem(
              #     imageOutput("image_2_esp_4", height = "100%", width = "100%")
              #   )
              # )
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
              DTOutput("tabela_proporcao_esp_4", height = "100%"),
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
                plotlyOutput("histograma_comprimento_esp_5", height = "100%")
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
              imageOutput("image_1_esp_5", height = "100%", width = "100%")
              # carousel(
              #   width = 12,
              #   id = "carousel_esp_5",
              #   carouselItem(
              #     imageOutput("image_1_esp_5", height = "100%", width = "100%")
              #   ),
              #   carouselItem(
              #     imageOutput("image_2_esp_5", height = "100%", width = "100%")
              #   )
              # )
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
              DTOutput("tabela_proporcao_esp_5", height = "100%"),
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
        tabName = "comprimento_especie_6",
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
                plotlyOutput("DispersaoComprimentoXPeso_esp_6", height = "100%")
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
                plotlyOutput("histograma_comprimento_esp_6", height = "100%")
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
              imageOutput("image_1_esp_6", height = "100%", width = "100%")
              # carousel(
              #   width = 12,
              #   id = "carousel_esp_6",
              #   carouselItem(
              #     imageOutput("image_1_esp_6", height = "100%", width = "100%")
              #   ),
              #   carouselItem(
              #     imageOutput("image_2_esp_6", height = "100%", width = "100%")
              #   )
              # )
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
              DTOutput("tabela_proporcao_esp_6", height = "100%"),
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
        tabName = "comprimento_especie_7",
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
                plotlyOutput("DispersaoComprimentoXPeso_esp_7", height = "100%")
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
                plotlyOutput("histograma_comprimento_esp_7", height = "100%")
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
              imageOutput("image_1_esp_7", height = "100%", width = "100%")
              # carousel(
              #   width = 12,
              #   id = "carousel_esp_7",
              #   carouselItem(
              #     imageOutput("image_1_esp_7", height = "100%", width = "100%")
              #   ),
              #   carouselItem(
              #     imageOutput("image_2_esp_7", height = "100%", width = "100%")
              #   )
              # )
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
              DTOutput("tabela_proporcao_esp_7", height = "100%"),
              sidebar = boxSidebar(
                id = "boxsidebarComprimento_4",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("dis_com_texto_4")))
              )
            )
          )
        )
      )
# fim_teste ---------------------------------------------------------------
    )
  ),
  
  # ControlBar --------------------------------------------------------------
  
  controlbar = dashboardControlbar(
    overlay = TRUE,
    collapsed = TRUE,
    skin = "light",
    id = "controlbar",
    width = 280,
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        title = tagList(i18n$t("filtros")),
        icon = icon("filter"),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_1'",
          radioButtons(
            inputId = "sexo_comprimento_esp_1",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Macho", "Fêmea"),
            choiceNames = c(
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Macho"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_2'",
          radioButtons(
            inputId = "sexo_comprimento_esp_2",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Macho", "Fêmea"),
            choiceNames = c(
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Macho"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_3'",
          radioButtons(
            inputId = "sexo_comprimento_esp_3",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Macho", "Fêmea"),
            choiceNames = c(
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Macho"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_4'",
          radioButtons(
            inputId = "sexo_comprimento_esp_4",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Macho", "Fêmea"),
            choiceNames = c(
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Macho"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_5'",
          radioButtons(
            inputId = "sexo_comprimento_esp_5",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Macho", "Fêmea"),
            choiceNames = c(
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Macho"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_6'",
          radioButtons(
            inputId = "sexo_comprimento_esp_6",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Macho", "Fêmea"),
            choiceNames = c(
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Macho"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_especie_7'",
          radioButtons(
            inputId = "sexo_comprimento_esp_7",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Macho", "Fêmea"),
            choiceNames = c(
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Macho"
          )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Variáveis ---------------------------------------------------------------
  
  cores_sexo <- c(
    "Macho" = "#053061",
    "Fêmea" = "#92c5de"
  )
  
  TabsControlbarFiltro <- c(
    "comprimento_especie_1", "comprimento_especie_2", "comprimento_especie_3",
    "comprimento_especie_4", "comprimento_especie_5", "comprimento_especie_6",
    "comprimento_especie_7"
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
    dados_aux <- osseos_alterado %>% 
      filter(
        especie == "Micropogonias furnieri" &
          sexo == input$sexo_comprimento_esp_1 &
          comprimento_total != 0 & 
          peso_total_kg != 0
      )
  })
  
  output$DispersaoComprimentoXPeso_esp_1 <- renderPlotly({
    
    biometria_sem_outliers <- remover_outliers(
      dados_biometria_filtro_esp_1(),
      "comprimento_total"
      )
    biometria_sem_outliers <- remover_outliers(
      biometria_sem_outliers,
      "peso_total_kg"
      )
    
    biometria_esp_1 <- data.frame(
      sexo = biometria_sem_outliers$sexo,
      x = biometria_sem_outliers$comprimento_total,
      y = biometria_sem_outliers$peso_total_kg
    )

    # mod0 <- glm(log(y) ~ x, data = biometria_esp_1)
    # 
    # a <- coef(mod0)[[1]]
    # 
    # b <- coef(mod0)[[2]]
    # 
    # cat("a = ", a, "\n")
    # cat("b = ", b, "\n")
    # 
    # biometria.nls_esp_1 <- nls (
    #   formula = y ~ a*x^b, 
    #   data = biometria_esp_1,
    #   start = list(
    #     a = a,
    #     b = b
    #   ),
    #   control = nls.control(maxiter = 200)
    # )
    # 
    # # Valores do confint
    # conf <- confint(biometria.nls_esp_1)
    # a_lower <- conf["a", "2.5%"]
    # a_upper <- conf["a", "97.5%"]
    # b_lower <- conf["b", "2.5%"]
    # b_upper <- conf["b", "97.5%"]
    # 
    # new_data_esp_1 <- data.frame(
    #   x = seq(
    #     min(biometria_esp_1$x),
    #     max(biometria_esp_1$x),
    #     length.out = 100)
    # )
    # # Calcular as curvas ajustadas
    # new_data_esp_1$fit <- coef(biometria.nls_esp_1)["a"] * 
    # new_data_esp_1$x^coef(biometria.nls_esp_1)["b"]
    # new_data_esp_1$lwr <- a_lower * new_data_esp_1$x^b_lower
    # new_data_esp_1$upr <- a_upper * new_data_esp_1$x^b_upper
    # 
    # new_data_esp_1$curva <- paste0(
    #   i18n$t("comprimento_legenda"), round(new_data_esp_1$x, 2 ), " cm<br>",
    #   i18n$t("lim_sup_legenda"), round(new_data_esp_1$upr, 2), " kg<br>",
    #   i18n$t("curva_legenda"), round(new_data_esp_1$fit, 2), " kg<br>",
    #   i18n$t("lim_inf_legenda"), round(new_data_esp_1$lwr, 2), " kg"
    # )
    
    biometria_esp_1$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria_esp_1$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria_esp_1$y, 2), " kg"
    )
    
    p <- ggplot() +
      # geom_line(
      #   data = new_data_esp_1,
      #   aes(x = x, y = fit),
      #   color = "blue",
      #   linewidth = 0.6
      # ) +  # Linha ajustada
      geom_point(
        data = biometria_esp_1,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      # geom_ribbon(
      #   data = new_data_esp_1,
      #   aes(x = x, ymin = lwr, ymax = upr),
      #   fill = "blue",
      #   alpha = 0.2
      # ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()
    
    # Tornar o gráfico interativo
    ggplotly(p) %>% 
      plotly::style(
        hoverinfo = "text",
        text = biometria_esp_1$XvsY, 
        # traces = 2
        traces = 1
      ) %>% 
      # plotly::style(
      #   hoverinfo = "text",
      #   text = new_data_esp_1$curva,
      #   traces = 1
      # ) %>% 
      # plotly::style(
      #   hoverinfo = "none",
      #   traces = 3
      # ) %>%
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento_esp_1 <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro_esp_1(),
      x = ~comprimento_total,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      marker = list(
        line = list(
          color = alpha('black', 0.1),
          width = 0.5
        )
      ),
      customdata = ~case_when(
        sexo == "Macho" ~ i18n$t("macho"),
        sexo == "Fêmea" ~ i18n$t("femea"),
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
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$tabela_proporcao_esp_1 <- renderDT({
    
    dados_tabela_esp_1 <- osseos_alterado %>% 
      filter(especie == "Micropogonias furnieri" & sexo != "Indeterminado") %>% 
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
          sexo == "Macho" ~ i18n$t("macho"),
          sexo == "Fêmea" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela_esp_1) <- c(
      i18n$t("sexo"), 
      "Total",
      i18n$t("porcentagem")
      )
    
    datatable(
      data = dados_tabela_esp_1,
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
  
  output$image_1_esp_1 <- renderImage({
    list(
      src = "www/Micropogonias_furnieri_imagem_1.png",
      contentType = "image/png",
      height = "auto",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$image_2_esp_1 <- renderImage({}, deleteFile = FALSE)
  
  # Distribuição de Captura Espécie 2  ----------------------------------------
  
  dados_biometria_filtro_esp_2 <- reactive({
    dados_aux <- osseos_alterado %>% 
      filter(
        especie == "Pomatomus saltatrix" &
          sexo == input$sexo_comprimento_esp_2 &
          comprimento_total != 0 & 
          peso_total_kg != 0
      )
  })
  
  output$DispersaoComprimentoXPeso_esp_2 <- renderPlotly({
    
    biometria_sem_outliers <- remover_outliers(
      dados_biometria_filtro_esp_2(),
      "comprimento_total"
    )
    biometria_sem_outliers <- remover_outliers(
      biometria_sem_outliers,
      "peso_total_kg"
    )

    biometria_esp_2 <- data.frame(
      sexo = biometria_sem_outliers$sexo,
      x = biometria_sem_outliers$comprimento_total,
      y = biometria_sem_outliers$peso_total_kg
    )

    # mod0 <- glm(log(y) ~ x, data = biometria_esp_2)
    #
    # a <- coef(mod0)[[1]]
    #
    # b <- coef(mod0)[[2]]
    #
    # cat("a = ", a, "\n")
    # cat("b = ", b, "\n")
    #
    # biometria.nls_esp_2 <- nls (
    #   formula = y ~ a*x^b,
    #   data = biometria_esp_2,
    #   start = list(
    #     a = a,
    #     b = b
    #   ),
    #   control = nls.control(maxiter = 200)
    # )
    #
    # # Valores do confint
    # conf <- confint(biometria.nls_esp_2)
    # a_lower <- conf["a", "2.5%"]
    # a_upper <- conf["a", "97.5%"]
    # b_lower <- conf["b", "2.5%"]
    # b_upper <- conf["b", "97.5%"]
    #
    # new_data_esp_2 <- data.frame(
    #   x = seq(
    #     min(biometria_esp_2$x),
    #     max(biometria_esp_2$x),
    #     length.out = 100)
    # )
    # # Calcular as curvas ajustadas
    # new_data_esp_2$fit <- coef(biometria.nls_esp_2)["a"] * 
    # new_data_esp_2$x^coef(biometria.nls_esp_2)["b"]
    # new_data_esp_2$lwr <- a_lower * new_data_esp_2$x^b_lower
    # new_data_esp_2$upr <- a_upper * new_data_esp_2$x^b_upper
    #
    # new_data_esp_2$curva <- paste0(
    #   i18n$t("comprimento_legenda"), round(new_data_esp_2$x, 2 ), " cm<br>",
    #   i18n$t("lim_sup_legenda"), round(new_data_esp_2$upr, 2), " kg<br>",
    #   i18n$t("curva_legenda"), round(new_data_esp_2$fit, 2), " kg<br>",
    #   i18n$t("lim_inf_legenda"), round(new_data_esp_2$lwr, 2), " kg"
    # )

    biometria_esp_2$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria_esp_2$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria_esp_2$y, 2), " kg"
    )

    p <- ggplot() +
      # geom_line(
      #   data = new_data_esp_2,
      #   aes(x = x, y = fit),
      #   color = "blue",
      #   linewidth = 0.6
      # ) +  # Linha ajustada
      geom_point(
        data = biometria_esp_2,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      # geom_ribbon(
      #   data = new_data_esp_2,
      #   aes(x = x, ymin = lwr, ymax = upr),
      #   fill = "blue",
      #   alpha = 0.2
      # ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()

    # Tornar o gráfico interativo
    ggplotly(p) %>%
      plotly::style(
        hoverinfo = "text",
        text = biometria_esp_2$XvsY,
        # traces = 2
        traces = 1
      ) %>%
      # plotly::style(
      #   hoverinfo = "text",
      #   text = new_data_esp_2$curva,
      #   traces = 1
      # ) %>%
      # plotly::style(
      #   hoverinfo = "none",
      #   traces = 3
      # ) %>%
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento_esp_2 <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro_esp_2(),
      x = ~comprimento_total,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      marker = list(
        line = list(
          color = alpha('black', 0.1),
          width = 0.5
        )
      ),
      customdata = ~case_when(
        sexo == "Macho" ~ i18n$t("macho"),
        sexo == "Fêmea" ~ i18n$t("femea"),
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
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  output$tabela_proporcao_esp_2 <- renderDT({
    
    dados_tabela_esp_2 <- osseos_alterado %>% 
      filter(especie == "Pomatomus saltatrix" & sexo != "Indeterminado") %>% 
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
          sexo == "Macho" ~ i18n$t("macho"),
          sexo == "Fêmea" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela_esp_2) <- c(
      i18n$t("sexo"), 
      "Total",
      i18n$t("porcentagem")
      )
    
    datatable(
      data = dados_tabela_esp_2,
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
  
  output$image_1_esp_2 <- renderImage({
    list(
      src = "www/Pomatomus_saltatrix_imagem_1.jpg",
      contentType = "image/jng",
      height = "auto",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$image_2_esp_2 <- renderImage({}, deleteFile = FALSE)
  
  # Distribuição de Captura Espécie 3  ----------------------------------------
  
  dados_biometria_filtro_esp_3 <- reactive({
    dados_aux <- osseos_alterado %>% 
      filter(
        especie == "Mugil liza" &
          sexo == input$sexo_comprimento_esp_3 &
          comprimento_total != 0 & 
          peso_total_kg != 0
      )
  })
  
  output$DispersaoComprimentoXPeso_esp_3 <- renderPlotly({
    
    biometria_sem_outliers <- remover_outliers(
      dados_biometria_filtro_esp_3(),
      "comprimento_total"
    )
    biometria_sem_outliers <- remover_outliers(
      biometria_sem_outliers,
      "peso_total_kg"
    )
    
    biometria_esp_3 <- data.frame(
      sexo = biometria_sem_outliers$sexo,
      x = biometria_sem_outliers$comprimento_total,
      y = biometria_sem_outliers$peso_total_kg
    )
    
    # mod0 <- glm(log(y) ~ x, data = biometria_esp_3)
    # 
    # a <- coef(mod0)[[1]]
    # 
    # b <- coef(mod0)[[2]]
    # 
    # cat("a = ", a, "\n")
    # cat("b = ", b, "\n")
    # 
    # biometria.nls_esp_3 <- nls (
    #   formula = y ~ a*x^b, 
    #   data = biometria_esp_3,
    #   start = list(
    #     a = a,
    #     b = b
    #   ),
    #   control = nls.control(maxiter = 200)
    # )
    # 
    # # Valores do confint
    # conf <- confint(biometria.nls_esp_3)
    # a_lower <- conf["a", "2.5%"]
    # a_upper <- conf["a", "97.5%"]
    # b_lower <- conf["b", "2.5%"]
    # b_upper <- conf["b", "97.5%"]
    # 
    # new_data_esp_3 <- data.frame(
    #   x = seq(
    #     min(biometria_esp_3$x),
    #     max(biometria_esp_3$x),
    #     length.out = 100)
    # )
    # # Calcular as curvas ajustadas
    # new_data_esp_3$fit <- coef(biometria.nls_esp_3)["a"] * 
    # new_data_esp_3$x^coef(biometria.nls_esp_3)["b"]
    # new_data_esp_3$lwr <- a_lower * new_data_esp_3$x^b_lower
    # new_data_esp_3$upr <- a_upper * new_data_esp_3$x^b_upper
    # 
    # new_data_esp_3$curva <- paste0(
    #   i18n$t("comprimento_legenda"), round(new_data_esp_3$x, 2 ), " cm<br>",
    #   i18n$t("lim_sup_legenda"), round(new_data_esp_3$upr, 2), " kg<br>",
    #   i18n$t("curva_legenda"), round(new_data_esp_3$fit, 2), " kg<br>",
    #   i18n$t("lim_inf_legenda"), round(new_data_esp_3$lwr, 2), " kg"
    # )
    
    biometria_esp_3$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria_esp_3$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria_esp_3$y, 2), " kg"
    )
    
    p <- ggplot() +
      # geom_line(
      #   data = new_data_esp_3,
      #   aes(x = x, y = fit),
      #   color = "blue",
      #   linewidth = 0.6
      # ) +  # Linha ajustada
      geom_point(
        data = biometria_esp_3,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      # geom_ribbon(
      #   data = new_data_esp_3,
      #   aes(x = x, ymin = lwr, ymax = upr),
      #   fill = "blue",
      #   alpha = 0.2
      # ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()
    
    # Tornar o gráfico interativo
    ggplotly(p) %>% 
      plotly::style(
        hoverinfo = "text",
        text = biometria_esp_3$XvsY, 
        # traces = 2
        traces = 1
      ) %>% 
      # plotly::style(
      #   hoverinfo = "text",
      #   text = new_data_esp_3$curva,
      #   traces = 1
      # ) %>% 
      # plotly::style(
      #   hoverinfo = "none",
      #   traces = 3
      # ) %>%
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento_esp_3 <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro_esp_3(),
      x = ~comprimento_total,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      marker = list(
        line = list(
          color = alpha('black', 0.1),
          width = 0.5
        )
      ),
      customdata = ~case_when(
        sexo == "Macho" ~ i18n$t("macho"),
        sexo == "Fêmea" ~ i18n$t("femea"),
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
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  output$tabela_proporcao_esp_3 <- renderDT({
    
    dados_tabela_esp_3 <- osseos_alterado %>% 
      filter(especie == "Mugil liza" & sexo != "Indeterminado") %>% 
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
          sexo == "Macho" ~ i18n$t("macho"),
          sexo == "Fêmea" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela_esp_3) <- c(
      i18n$t("sexo"),
      "Total",
      i18n$t("porcentagem")
      )
    
    datatable(
      data = dados_tabela_esp_3,
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
  
  output$image_1_esp_3 <- renderImage({
    list(
      src = "www/Mugil_liza_imagem_1.png",
      contentType = "image/png",
      height = "auto",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$image_2_esp_3 <- renderImage({}, deleteFile = FALSE)
  
  # Distribuição de Captura Espécie 4  ----------------------------------------
  
  dados_biometria_filtro_esp_4 <- reactive({
    dados_aux <- osseos_alterado %>% 
      filter(
        especie == "Oligoplites saliens" & 
          sexo == input$sexo_comprimento_esp_4 &
          comprimento_total != 0 & 
          peso_total_kg != 0
      )
  })
  
  output$DispersaoComprimentoXPeso_esp_4 <- renderPlotly({
    
    biometria_sem_outliers <- remover_outliers(
      dados_biometria_filtro_esp_4(),
      "comprimento_total"
    )
    biometria_sem_outliers <- remover_outliers(
      biometria_sem_outliers,
      "peso_total_kg"
    )
    
    biometria_esp_4 <- data.frame(
      sexo = biometria_sem_outliers$sexo,
      x = biometria_sem_outliers$comprimento_total,
      y = biometria_sem_outliers$peso_total_kg
    )
    
    # mod0 <- glm(log(y) ~ x, data = biometria_esp_4)
    # 
    # a <- coef(mod0)[[1]]
    # 
    # b <- coef(mod0)[[2]]
    # 
    # cat("a = ", a, "\n")
    # cat("b = ", b, "\n")
    # 
    # biometria.nls_esp_4 <- nls (
    #   formula = y ~ a*x^b, 
    #   data = biometria_esp_4,
    #   start = list(
    #     a = a,
    #     b = b
    #   ),
    #   control = nls.control(maxiter = 200)
    # )
    # 
    # # Valores do confint
    # conf <- confint(biometria.nls_esp_4)
    # a_lower <- conf["a", "2.5%"]
    # a_upper <- conf["a", "97.5%"]
    # b_lower <- conf["b", "2.5%"]
    # b_upper <- conf["b", "97.5%"]
    # 
    # new_data_esp_4 <- data.frame(
    #   x = seq(
    #     min(biometria_esp_4$x),
    #     max(biometria_esp_4$x),
    #     length.out = 100)
    # )
    # # Calcular as curvas ajustadas
    # new_data_esp_4$fit <- coef(biometria.nls_esp_4)["a"] * 
    # new_data_esp_4$x^coef(biometria.nls_esp_4)["b"]
    # new_data_esp_4$lwr <- a_lower * new_data_esp_4$x^b_lower
    # new_data_esp_4$upr <- a_upper * new_data_esp_4$x^b_upper
    # 
    # new_data_esp_4$curva <- paste0(
    #   i18n$t("comprimento_legenda"), round(new_data_esp_4$x, 2 ), " cm<br>",
    #   i18n$t("lim_sup_legenda"), round(new_data_esp_4$upr, 2), " kg<br>",
    #   i18n$t("curva_legenda"), round(new_data_esp_4$fit, 2), " kg<br>",
    #   i18n$t("lim_inf_legenda"), round(new_data_esp_4$lwr, 2), " kg"
    # )
    
    biometria_esp_4$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria_esp_4$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria_esp_4$y, 2), " kg"
    )
    
    p <- ggplot() +
      # geom_line(
      #   data = new_data_esp_4,
      #   aes(x = x, y = fit),
      #   color = "blue",
      #   linewidth = 0.6
      # ) +  # Linha ajustada
      geom_point(
        data = biometria_esp_4,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      # geom_ribbon(
      #   data = new_data_esp_4,
      #   aes(x = x, ymin = lwr, ymax = upr),
      #   fill = "blue",
      #   alpha = 0.2
      # ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()
    
    # Tornar o gráfico interativo
    ggplotly(p) %>% 
      plotly::style(
        hoverinfo = "text",
        text = biometria_esp_4$XvsY, 
        # traces = 2
        traces = 1
      ) %>% 
      # plotly::style(
      #   hoverinfo = "text",
      #   text = new_data_esp_4$curva,
      #   traces = 1
      # ) %>% 
      # plotly::style(
      #   hoverinfo = "none",
      #   traces = 3
      # ) %>%
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento_esp_4 <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro_esp_4(),
      x = ~comprimento_total,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      marker = list(
        line = list(
          color = alpha('black', 0.1),
          width = 0.5
        )
      ),
      customdata = ~case_when(
        sexo == "Macho" ~ i18n$t("macho"),
        sexo == "Fêmea" ~ i18n$t("femea"),
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
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  output$tabela_proporcao_esp_4 <- renderDT({
    
    dados_tabela_esp_4 <- osseos_alterado %>% 
      filter(especie == "Oligoplites saliens" & sexo != "Indeterminado") %>% 
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
          sexo == "Macho" ~ i18n$t("macho"),
          sexo == "Fêmea" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela_esp_4) <- c(
      i18n$t("sexo"),
      "Total",
      i18n$t("porcentagem")
      )
    
    datatable(
      data = dados_tabela_esp_4,
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
  
  output$image_1_esp_4 <- renderImage({
    list(
      src = "www/Oligoplites_saliens_imagem_1.png",
      contentType = "image/png",
      height = "auto",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$image_2_esp_4 <- renderImage({}, deleteFile = FALSE)
  
  # Distribuição de Captura Espécie 5  ----------------------------------------
  
  dados_biometria_filtro_esp_5 <- reactive({
    dados_aux <- osseos_alterado %>% 
      filter(
        especie == "Trichiurus lepturus" & 
          sexo == input$sexo_comprimento_esp_5 &
          comprimento_total != 0 & 
          peso_total_kg != 0
          )
  })
  
  output$DispersaoComprimentoXPeso_esp_5 <- renderPlotly({
    
    biometria_sem_outliers <- remover_outliers(
      dados_biometria_filtro_esp_5(),
      "comprimento_total"
    )
    biometria_sem_outliers <- remover_outliers(
      biometria_sem_outliers,
      "peso_total_kg"
    )
    
    biometria_esp_5 <- data.frame(
      sexo = biometria_sem_outliers$sexo,
      x = biometria_sem_outliers$comprimento_total,
      y = biometria_sem_outliers$peso_total_kg
    )
    
    # mod0 <- glm(log(y) ~ x, data = biometria_esp_5)
    # 
    # a <- coef(mod0)[[1]]
    # 
    # b <- coef(mod0)[[2]]
    # 
    # cat("a = ", a, "\n")
    # cat("b = ", b, "\n")
    # 
    # biometria.nls_esp_5 <- nls (
    #   formula = y ~ a*x^b, 
    #   data = biometria_esp_5,
    #   start = list(
    #     a = a,
    #     b = b
    #   ),
    #   control = nls.control(maxiter = 200)
    # )
    # 
    # # Valores do confint
    # conf <- confint(biometria.nls_esp_5)
    # a_lower <- conf["a", "2.5%"]
    # a_upper <- conf["a", "97.5%"]
    # b_lower <- conf["b", "2.5%"]
    # b_upper <- conf["b", "97.5%"]
    # 
    # new_data_esp_5 <- data.frame(
    #   x = seq(
    #     min(biometria_esp_5$x),
    #     max(biometria_esp_5$x),
    #     length.out = 100)
    # )
    # # Calcular as curvas ajustadas
    # new_data_esp_5$fit <- coef(biometria.nls_esp_5)["a"] * 
    # new_data_esp_5$x^coef(biometria.nls_esp_5)["b"]
    # new_data_esp_5$lwr <- a_lower * new_data_esp_5$x^b_lower
    # new_data_esp_5$upr <- a_upper * new_data_esp_5$x^b_upper
    # 
    # new_data_esp_5$curva <- paste0(
    #   i18n$t("comprimento_legenda"), round(new_data_esp_5$x, 2 ), " cm<br>",
    #   i18n$t("lim_sup_legenda"), round(new_data_esp_5$upr, 2), " kg<br>",
    #   i18n$t("curva_legenda"), round(new_data_esp_5$fit, 2), " kg<br>",
    #   i18n$t("lim_inf_legenda"), round(new_data_esp_5$lwr, 2), " kg"
    # )
    
    biometria_esp_5$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria_esp_5$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria_esp_5$y, 2), " kg"
    )
    
    p <- ggplot() +
      # geom_line(
      #   data = new_data_esp_5,
      #   aes(x = x, y = fit),
      #   color = "blue",
      #   linewidth = 0.6
      # ) +  # Linha ajustada
      geom_point(
        data = biometria_esp_5,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      # geom_ribbon(
      #   data = new_data_esp_5,
      #   aes(x = x, ymin = lwr, ymax = upr),
      #   fill = "blue",
      #   alpha = 0.2
      # ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()
    
    # Tornar o gráfico interativo
    ggplotly(p) %>% 
      plotly::style(
        hoverinfo = "text",
        text = biometria_esp_5$XvsY, 
        # traces = 2
        traces = 1
      ) %>% 
      # plotly::style(
      #   hoverinfo = "text",
      #   text = new_data_esp_5$curva,
      #   traces = 1
      # ) %>% 
      # plotly::style(
      #   hoverinfo = "none",
      #   traces = 3
      # ) %>%
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento_esp_5 <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro_esp_5(),
      x = ~comprimento_total,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      marker = list(
        line = list(
          color = alpha('black', 0.1),
          width = 0.5
        )
      ),
      customdata = ~case_when(
        sexo == "Macho" ~ i18n$t("macho"),
        sexo == "Fêmea" ~ i18n$t("femea"),
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
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  output$tabela_proporcao_esp_5 <- renderDT({
    
    dados_tabela_esp_5 <- osseos_alterado %>% 
      filter(especie == "Trichiurus lepturus" & sexo != "Indeterminado") %>% 
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
          sexo == "Macho" ~ i18n$t("macho"),
          sexo == "Fêmea" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela_esp_5) <- c(
      i18n$t("sexo"),
      "Total",
      i18n$t("porcentagem")
      )
    
    datatable(
      data = dados_tabela_esp_5,
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
  
  output$image_1_esp_5 <- renderImage({
    list(
      src = "www/Trichiurus_lepturus_imagem_1.png",
      contentType = "image/png",
      height = "auto",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$image_2_esp_5 <- renderImage({
    # list(
    #   src = "www/Micropogonias_furnieri_imagem_2.jpg",
    #   contentType = "image/jpg",
    #   height = "auto",
    #   width = "70%"
    # )
  }, deleteFile = FALSE)
  
  # Distribuição de Captura Espécie 6  ----------------------------------------
  
  dados_biometria_filtro_esp_6 <- reactive({
    dados_aux <- osseos_alterado %>% 
      filter(
        especie == "Scomberomorus brasiliensis" & 
          sexo == input$sexo_comprimento_esp_6 &
          comprimento_total != 0 & 
          peso_total_kg != 0
      )
  })
  
  output$DispersaoComprimentoXPeso_esp_6 <- renderPlotly({
    
    biometria_sem_outliers <- remover_outliers(
      dados_biometria_filtro_esp_6(),
      "comprimento_total"
    )
    biometria_sem_outliers <- remover_outliers(
      biometria_sem_outliers,
      "peso_total_kg"
    )
    
    biometria_esp_6 <- data.frame(
      sexo = biometria_sem_outliers$sexo,
      x = biometria_sem_outliers$comprimento_total,
      y = biometria_sem_outliers$peso_total_kg
    )
    
    # mod0 <- glm(log(y) ~ x, data = biometria_esp_6)
    # 
    # a <- coef(mod0)[[1]]
    # 
    # b <- coef(mod0)[[2]]
    # 
    # cat("a = ", a, "\n")
    # cat("b = ", b, "\n")
    # 
    # biometria.nls_esp_6 <- nls (
    #   formula = y ~ a*x^b, 
    #   data = biometria_esp_6,
    #   start = list(
    #     a = a,
    #     b = b
    #   ),
    #   control = nls.control(maxiter = 200)
    # )
    # 
    # # Valores do confint
    # conf <- confint(biometria.nls_esp_6)
    # a_lower <- conf["a", "2.5%"]
    # a_upper <- conf["a", "97.5%"]
    # b_lower <- conf["b", "2.5%"]
    # b_upper <- conf["b", "97.5%"]
    # 
    # new_data_esp_6 <- data.frame(
    #   x = seq(
    #     min(biometria_esp_6$x),
    #     max(biometria_esp_6$x),
    #     length.out = 100)
    # )
    # # Calcular as curvas ajustadas
    # new_data_esp_6$fit <- coef(biometria.nls_esp_6)["a"] * 
    # new_data_esp_6$x^coef(biometria.nls_esp_6)["b"]
    # new_data_esp_6$lwr <- a_lower * new_data_esp_6$x^b_lower
    # new_data_esp_6$upr <- a_upper * new_data_esp_6$x^b_upper
    # 
    # new_data_esp_6$curva <- paste0(
    #   i18n$t("comprimento_legenda"), round(new_data_esp_6$x, 2 ), " cm<br>",
    #   i18n$t("lim_sup_legenda"), round(new_data_esp_6$upr, 2), " kg<br>",
    #   i18n$t("curva_legenda"), round(new_data_esp_6$fit, 2), " kg<br>",
    #   i18n$t("lim_inf_legenda"), round(new_data_esp_6$lwr, 2), " kg"
    # )
    
    biometria_esp_6$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria_esp_6$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria_esp_6$y, 2), " kg"
    )
    
    p <- ggplot() +
      # geom_line(
      #   data = new_data_esp_6,
      #   aes(x = x, y = fit),
      #   color = "blue",
      #   linewidth = 0.6
      # ) +  # Linha ajustada
      geom_point(
        data = biometria_esp_6,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      # geom_ribbon(
      #   data = new_data_esp_6,
      #   aes(x = x, ymin = lwr, ymax = upr),
      #   fill = "blue",
      #   alpha = 0.2
      # ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()
    
    # Tornar o gráfico interativo
    ggplotly(p) %>% 
      plotly::style(
        hoverinfo = "text",
        text = biometria_esp_6$XvsY, 
        # traces = 2
        traces = 1
      ) %>% 
      # plotly::style(
      #   hoverinfo = "text",
      #   text = new_data_esp_6$curva,
      #   traces = 1
      # ) %>% 
      # plotly::style(
      #   hoverinfo = "none",
      #   traces = 3
      # ) %>%
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento_esp_6 <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro_esp_6(),
      x = ~comprimento_total,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      marker = list(
        line = list(
          color = alpha('black', 0.1),
          width = 0.5
        )
      ),
      customdata = ~case_when(
        sexo == "Macho" ~ i18n$t("macho"),
        sexo == "Fêmea" ~ i18n$t("femea"),
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
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  output$tabela_proporcao_esp_6 <- renderDT({
    
    dados_tabela_esp_6 <- osseos_alterado %>% 
      filter(
        especie == "Scomberomorus brasiliensis" & sexo != "Indeterminado"
        ) %>%  
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
          sexo == "Macho" ~ i18n$t("macho"),
          sexo == "Fêmea" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela_esp_6) <- c(
      i18n$t("sexo"), 
      "Total", 
      i18n$t("porcentagem")
      )
    
    datatable(
      data = dados_tabela_esp_6,
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
  
  output$image_1_esp_6 <- renderImage({
    list(
      src = "www/Scomberomorus_brasiliensis_imagem_1.png",
      contentType = "image/png",
      height = "auto",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$image_2_esp_6 <- renderImage({}, deleteFile = FALSE)
  
  # Distribuição de Captura Espécie 7  ----------------------------------------
  
  dados_biometria_filtro_esp_7 <- reactive({
    dados_aux <- osseos_alterado %>% 
      filter(
        especie == "Oligoplites saurus" &
          sexo == input$sexo_comprimento_esp_7 &
          comprimento_total != 0 & 
          peso_total_kg != 0
      )
  })
  
  output$DispersaoComprimentoXPeso_esp_7 <- renderPlotly({
    
    biometria_sem_outliers <- remover_outliers(
      dados_biometria_filtro_esp_7(),
      "comprimento_total"
    )
    biometria_sem_outliers <- remover_outliers(
      biometria_sem_outliers,
      "peso_total_kg"
    )
    
    biometria_esp_7 <- data.frame(
      sexo = biometria_sem_outliers$sexo,
      x = biometria_sem_outliers$comprimento_total,
      y = biometria_sem_outliers$peso_total_kg
    )
    
    # mod0 <- glm(log(y) ~ x, data = biometria_esp_7)
    # 
    # a <- coef(mod0)[[1]]
    # 
    # b <- coef(mod0)[[2]]
    # 
    # cat("a = ", a, "\n")
    # cat("b = ", b, "\n")
    # 
    # biometria.nls_esp_7 <- nls (
    #   formula = y ~ a*x^b, 
    #   data = biometria_esp_7,
    #   start = list(
    #     a = a,
    #     b = b
    #   ),
    #   control = nls.control(maxiter = 200)
    # )
    # 
    # # Valores do confint
    # conf <- confint(biometria.nls_esp_7)
    # a_lower <- conf["a", "2.5%"]
    # a_upper <- conf["a", "97.5%"]
    # b_lower <- conf["b", "2.5%"]
    # b_upper <- conf["b", "97.5%"]
    # 
    # new_data_esp_7 <- data.frame(
    #   x = seq(
    #     min(biometria_esp_7$x),
    #     max(biometria_esp_7$x),
    #     length.out = 100)
    # )
    # # Calcular as curvas ajustadas
    # new_data_esp_7$fit <- coef(biometria.nls_esp_7)["a"] *
    # new_data_esp_7$x^coef(biometria.nls_esp_7)["b"]
    # new_data_esp_7$lwr <- a_lower * new_data_esp_7$x^b_lower
    # new_data_esp_7$upr <- a_upper * new_data_esp_7$x^b_upper
    # 
    # new_data_esp_7$curva <- paste0(
    #   i18n$t("comprimento_legenda"), round(new_data_esp_7$x, 2 ), " cm<br>",
    #   i18n$t("lim_sup_legenda"), round(new_data_esp_7$upr, 2), " kg<br>",
    #   i18n$t("curva_legenda"), round(new_data_esp_7$fit, 2), " kg<br>",
    #   i18n$t("lim_inf_legenda"), round(new_data_esp_7$lwr, 2), " kg"
    # )
    
    biometria_esp_7$XvsY <- paste0(
      i18n$t("comprimento_legenda"), round(biometria_esp_7$x, 2 ), " cm<br>",
      i18n$t("peso_legenda"), round(biometria_esp_7$y, 2), " kg"
    )
    
    p <- ggplot() +
      # geom_line(
      #   data = new_data_esp_7,
      #   aes(x = x, y = fit),
      #   color = "blue",
      #   linewidth = 0.6
      # ) +  # Linha ajustada
      geom_point(
        data = biometria_esp_7,
        aes(x = x, y = y),
        colour = "blue",
        fill = "lightblue",
        size = 0.5,
        shape = 21
      ) +
      # geom_ribbon(
      #   data = new_data_esp_7,
      #   aes(x = x, ymin = lwr, ymax = upr),
      #   fill = "blue",
      #   alpha = 0.2
      # ) +  # Bandas de confiança
      labs(
        x = i18n$t("legenda_comprimento"),
        y = i18n$t("legenda_peso")
      ) +
      theme_minimal()
    
    # Tornar o gráfico interativo
    ggplotly(p) %>% 
      plotly::style(
        hoverinfo = "text",
        text = biometria_esp_7$XvsY, 
        # traces = 2
        traces = 1
      ) %>% 
      # plotly::style(
      #   hoverinfo = "text",
      #   text = new_data_esp_7$curva,
      #   traces = 1
      # ) %>% 
      # plotly::style(
      #   hoverinfo = "none",
      #   traces = 3
      # ) %>%
      config(
        locale = "pt-br",
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          'hoverClosestCartesian', 'hoverCompareCartesian'
        )
      )
  })
  
  output$histograma_comprimento_esp_7 <- renderPlotly({
    plot_ly(
      data = dados_biometria_filtro_esp_7(),
      x = ~comprimento_total,
      type = 'histogram',
      color = ~sexo,
      xbins = list(
        size = 5
      ),
      marker = list(
        line = list(
          color = alpha('black', 0.1),
          width = 0.5
        )
      ),
      customdata = ~case_when(
        sexo == "Macho" ~ i18n$t("macho"),
        sexo == "Fêmea" ~ i18n$t("femea"),
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
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) 
  })
  
  output$tabela_proporcao_esp_7 <- renderDT({
    
    dados_tabela_esp_7 <- osseos_alterado %>% 
      filter(especie == "Oligoplites saurus" & sexo != "Indeterminado") %>% 
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
          sexo == "Macho" ~ i18n$t("macho"),
          sexo == "Fêmea" ~ i18n$t("femea"),
          TRUE ~ sexo
        )
      )
    
    colnames(dados_tabela_esp_7) <- c(
      i18n$t("sexo"),
      "Total",
      i18n$t("porcentagem")
      )
    
    datatable(
      data = dados_tabela_esp_7,
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
  
  output$image_1_esp_7 <- renderImage({
    list(
      src = "www/Oligoplites_saurus_imagem_1.png",
      contentType = "image/png",
      height = "auto",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$image_2_esp_7 <- renderImage({}, deleteFile = FALSE)
  
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

  output$TituloModal <- renderUI({
    div(
      style = "text-align: center;",
      # imageOutput("LABORATORIO_RESPONSAVEL", height = "100%"),
      h2(i18n$t("intro_texto_1")),
      h2(strong(i18n$t("intro_texto_2"))),
      tags$small("Lorem ipsum dolor sit amet, consectetur adipiscing elit. In 
                 dictum neque eget facilisis fringilla. Sed at.")
      # tags$small(i18n$t("intro_texto_3"))
    )
  })

  output$ConteudoModal <- renderUI({
    tagList(
      h5(strong(i18n$t("intro_texto_4"))),
      tags$ul(
        tags$li("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                In a."),
        tags$li("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                Donec tempor odio."),
        tags$li("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                Aenean dapibus nisi nec."),
        tags$li("Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                Etiam scelerisque nibh diam, vel.")
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
