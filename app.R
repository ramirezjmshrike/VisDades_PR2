library(shiny)
library(DT)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(sf)
library(treemapify)

# Càrrega de dades
agg_data <- read.csv("data/agg_comunicacions_neteja.csv")

districtes <- read.csv("data/BarcelonaCiutat_Districtes.csv")
districtes_sf <- st_as_sf(districtes, wkt = "geometria_wgs84", crs = 4326)

barris <- read.csv("data/BarcelonaCiutat_Barris.csv")
barris_sf <- st_as_sf(barris, wkt = "geometria_wgs84", crs = 4326)

# Noms llegibles per a llegendes i gràfics
noms_llegenda <- c(
  comunicacions = "Total comunicacions",
  comunicacions_100hab = "Comunicacions per 100 habitants",
  poblacio = "Població total",
  POBLACIO_TOTAL = "Població total",
  POBLACIO_HOMES = "Població (Homes)",
  POBLACIO_DONES = "Població (Dones)",
  SUPERFICIE_TERRITORI = "Superfície del territori (ha)",
  SUPERFICIE_RESIDENCIAL = "Superfície residencial (ha)",
  DENSITAT_POBLACIO_TERRITORI = "Densitat població / territori",
  DENSITAT_HOMES_TERRITORI = "Densitat homes / territori",
  DENSITAT_DONES_TERRITORI = "Densitat dones / territori",
  DENSITAT_POBLACIO_RESIDENCIAL = "Densitat població / residencial",
  DENSITAT_HOMES_RESIDENCIAL = "Densitat homes / residencial",
  DENSITAT_DONES_RESIDENCIAL = "Densitat dones / residencial",
  COMUNICACIONS_100HAB = "Comunicacions per 100 habitants"
)

trimestres <- c("1r Trimestre 2023", "2n Trimestre 2023", "3r Trimestre 2023", "4t Trimestre 2023",
                "1r Trimestre 2024", "2n Trimestre 2024", "3r Trimestre 2024", "4t Trimestre 2024")

ui <- fluidPage(
  titlePanel("Anàlisi de les comunicacions sobre neteja rebudes per l'Ajuntament de Barcelona, 2023-2024"),
  tags$h5("Autor: Joan Manel Ramírez Jávega.
          Visualització de dades - Màster de Ciència de dades (Universitat Oberta de Catalunya)", style = "margin-top: -10px; margin-bottom: 20px; color: #555;"),
  tabsetPanel(
    tabPanel("Visió general introductòria",
             sidebarLayout(
               sidebarPanel(
                 selectInput("any", "Selecciona any:",
                             choices = c("Tots els anys", sort(unique(agg_data$ANY_ALTA)))),
                 selectInput("metrica", "Mètrica a visualitzar:",
                             choices = c("Total comunicacions" = "comunicacions",
                                         "Comunicacions per 100 habitants" = "comunicacions_100hab",
                                         "Població total" = "poblacio"),
                             selected = "comunicacions_100hab"),
                 br(),
                 helpText(HTML("Com introducció al nostre anàlisi, aquesta pàgina de la visualització mostra l'evolució de les comunicacions de neteja en relació a la població total i agrupades per districtes de tot el municipi de Barcelona.<br><br>
                          Si cliqueu en un dels Districtes del mapa, la taula resum inferior s'actualitzarà amb les dades del Districte indicat amb també la proporció sobre el total.<br><br>
                               Com es pot comprovar sel·leccionant les diverses mètriques en el menú, un dels patrons observables és que en el districte d'Horta-Guinardó sempre es comptabilitzen més comunicacions a l'Ajuntament de Barcelona sobre neteja que el districte de Nou Barris.<br><br>
                               En les següents parts de la infografia cercarem exposar quines causístiques explicarien aquest patró en les dades."))
               ),
               mainPanel(
                 leafletOutput("mapa_districtes", height = 500),
                 plotOutput("grafica_districtes"),
                 br(),
                 tableOutput("taula_resum")
               )
             )
    ),
    
    tabPanel("Anàlisi comparatiu entre els districtes d'Horta-Guinardó i Nou Barris",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("trimestres", "Sel·lecciona els trimestres:",
                             min = 1, max = length(trimestres),
                             value = c(1, length(trimestres)),
                             step = 1, ticks = FALSE, animate = TRUE,
                             sep = ""),
                 uiOutput("trimestre_labels"),
                 selectInput("variable_mapa", "Variable demogràfica / territorial:",
                             choices = c("POBLACIO_TOTAL", "POBLACIO_HOMES", "POBLACIO_DONES",
                                         "SUPERFICIE_TERRITORI", "SUPERFICIE_RESIDENCIAL",
                                         "DENSITAT_POBLACIO_TERRITORI", "DENSITAT_HOMES_TERRITORI",
                                         "DENSITAT_DONES_TERRITORI",
                                         "DENSITAT_POBLACIO_RESIDENCIAL", "DENSITAT_HOMES_RESIDENCIAL",
                                         "DENSITAT_DONES_RESIDENCIAL", "COMUNICACIONS_100HAB"),
                             selected = "POBLACIO_TOTAL"),
                 br(),
                 helpText(HTML("En aquesta pàgina de la infografia presentem la comparació de les dinàmiques entre els districtes d'Horta-Guinardó i de Nou Barris en relació a les comunicacions sobre neteja.<br><br>
                          En primer lloc, visualitzem en el <i>treemap</i> el nombre les proporcions sobre el total de comunicacions de cada barri, tot diferenciant pel color a quin Districte pertanyen. Els dos mapes visualitzen el nombre total de comunicacions per barri com també les diverses variables de caràcter demogràfic o de superfície d'ús residencial, entre d'altres.
                          Finalment, també es visualitza una taula de freqüències corresponent a les temàtiques més freqüents i que siguin superiors al 2% del total en un dels dos districtes atès al marc temporal indicat en el filtre.<br><br>
                          També si es clica en qualsevol dels barris representats en ambdós mapes, apareixerà una taula que mostrarà les temàtiques més freqúents en el barri indicat amb el mateix filtratge temporal fixat en el menú.<br><br>
                          En aquest cas, es pot observar que és el barri del Carmel el que presenta el major nombre de comunicacions sobre neteja i, a la vegada, s'observa també que la segona temàtica més freqüent fa referència a la recollida de residus en els contenidors de rebuig i els d'orgànica, mentre que la tendència general són que en aquest segon lloc hi figurin les comunicacions relatives a la recollida de mobles.<br><br>
                          Aquest mateix anti-patró es pot observar en els barris de la Font d'en Fargues i Sant Genís dels Agudells, tots també en el districte d'Horta-Guinardó. Per altra banda, quan es sel·lecciona només els tercer trimestre - corresponent a l'estiu-, s'observa que en el cas de Sant Genís dels Agudells esdevé la temàtica més freqüent. En canvi, en el cas de Nou Barris mai s'observa aquest comportament, tot i que sí s'observen altres alteracions de les freqüències de les temàtiques"))
               ),
               mainPanel(
                 h4("Treemap de les comunicacions per barri", style = "margin-top: 10px;"),
                 plotOutput("treemap_districtes"),
                 h4("Mapa de les comunicacions per barri", style = "margin-top: 10px;"),
                 leafletOutput("mapa_barris", height = 400),
                 h4("Mapa dels indicadors per barri", style = "margin-top: 10px;"),
                 leafletOutput("mapa_variable", height = 400),
                 br(),
                 tableOutput("taula_element"),
                 conditionalPanel(
                   condition = "output.barriSeleccionat",
                   h4("Distribució per temàtica del barri seleccionat"),
                   textOutput("nom_barri_seleccionat"),
                   tableOutput("taula_barri")
                 )
               )
             )
    ),
    
    tabPanel("Anàlisi de l'estacionalitat",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_heatmap",
                             "Variable a visualitzar:",
                             choices = c(
                               "Relació Comunicacions / Densitat territori" = "RELACIO_TERRITORI_NORM",
                               "Relació Comunicacions / Densitat residencial" = "RELACIO_RESIDENCIAL_NORM"
                             ),
                             selected = "RELACIO_RESIDENCIAL_NORM"
                 ),
                 br(),
                 helpText(HTML("En aquesta darrera pàgina d'aquesta infografia presentem l'anàlisi de l'estacionalitat mitjançant un <i>heatmap</i> que visualitza  l'evolució en el temps de la relació normalitzada entre el nombre de comunicacions per cada 100 habitants i la densitat de població per hectàrea. També visualitzem, amb el seu respectiu <i>heatmap</i>, l'evolució mensual de la temperatura mitjana i la preciptació acumulada a tot el municipi de Barcelona.<br><br>
                          En primer lloc, és oportú indicar que el color blau marí ens informa que la relació en aquell període i barri està fora de l'escala del rang interquartils i, per tant, es tracta com un valor extrem. En el cas dels barris amb població de l'ordre inferior als 2.000 residents censats, com és seria el cas de la Clota, Can Peguera i Torre Baró, això es produiria pel fenomen que una comunicació en un barri no necessàriament ha de ser generada per un resident del mateix barri; això també explicaria per què la classe més freqüent de comunicacions faci referència a l'estat de la neteja de carrers i places. En canvi, el color blanc fa referència a l'absència de comunicacions d'aquest tipus.<br><br>
                          En segon lloc, destaquem que s'observa un manifest comportament estacional en les dades, incrementant-se la densitat de la relació anteriorment indicada durant els mesos estivals i que, també, són els més càlids i secs. també cal incidir que l'any 2023 va ser extraordinàriament sec. D'aquesta manera, s'observa com durant els mesos de Juliol i Agost és quan augmenta l'indiacdor de densitat, destacant-hi especialment els barri del Carmel, la Font d'en Fargues, Montbau i Horta en el districte d'Horta-Guinardó. En canvi, en el cas de Nou Barris no s'observa una relació intensa, excepte en el cas dels barris que presenten de forma freqüent valors extrems com Torre Baró i Can Peguera.<br><br>
                          I, finalment, resulta d'interès assenyalar que aquest comportament només s'aprecia millor quan la relació normalitzada es calcula en base a la densitat de població sobre àrea d'ús residencial i, en canvi, no quan es realitza el càlcul sobre la superfície total del barri. Això ens reforça en trobar encertat, en aquest cas concret, preferir la mètrica de la densitat de població en superfície d'ús residencial."))
               ),
               mainPanel(
                 h4("Heatmap estacionalitat per barri i mes"),
                 plotOutput("heatmap1", height = "600px"),
                 h4("Temperatura mensual mitjana"),
                 plotOutput("heatmap2", height = "150px"),
                 h4("Precipitació mensual acumulada"),
                 plotOutput("heatmap3", height = "150px")
               )
             )
    )
  )
)


server <- function(input, output, session) {
  clicked_districte <- reactiveVal(NULL)
  
  # Filtre Pestanya #1
  dades_filtrades <- reactive({
    if (input$any == "Tots els anys") {
      agg_data
    } else {
      agg_data %>% filter(ANY_ALTA == as.numeric(input$any))
    }
  })
  
  dades_resumides <- reactive({
    dades_filtrades() %>%
      group_by(DISTRICTE) %>%
      summarise(
        comunicacions = sum(RECOMPTE, na.rm = TRUE),
        poblacio = sum(POBLACIO_TOTAL[!duplicated(BARRI)], na.rm = TRUE),
        comunicacions_100hab = comunicacions / (poblacio / 100),
        .groups = "drop"
      )
  })
  
  districtes_data <- reactive({
    left_join(districtes_sf, dades_resumides(), by = c("nom_districte" = "DISTRICTE"))
  })
  
  
  # Filtre Pestanya #2
  output$trimestre_labels <- renderUI({
    HTML(paste("<b>Interval seleccionat:</b>",
               trimestres[input$trimestres[1]], "fins",
               trimestres[input$trimestres[2]]))
  })
  
  trimestres_map <- list(
    "1r Trimestre 2023" = list(mesos = c("Gener", "Febrer", "Març"), any = 2023),
    "2n Trimestre 2023" = list(mesos = c("Abril", "Maig", "Juny"), any = 2023),
    "3r Trimestre 2023" = list(mesos = c("Juliol", "Agost", "Setembre"), any = 2023),
    "4t Trimestre 2023" = list(mesos = c("Octubre", "Novembre", "Desembre"), any = 2023),
    "1r Trimestre 2024" = list(mesos = c("Gener", "Febrer", "Març"), any = 2024),
    "2n Trimestre 2024" = list(mesos = c("Abril", "Maig", "Juny"), any = 2024),
    "3r Trimestre 2024" = list(mesos = c("Juliol", "Agost", "Setembre"), any = 2024),
    "4t Trimestre 2024" = list(mesos = c("Octubre", "Novembre", "Desembre"), any = 2024)
  )
  
  output$trimestre_labels <- renderUI({
    HTML(paste("<b>Interval seleccionat:</b>",
               trimestres[input$trimestres[1]], "fins",
               trimestres[input$trimestres[2]]))
  })
  
  trimestre_data <- reactive({
    seleccionats <- trimestres[input$trimestres[1]:input$trimestres[2]]
    
    filtres <- lapply(trimestres_map[seleccionats], function(x) {
      tibble(MES_ALTA_NOM = x$mesos, ANY_ALTA = x$any)
    }) %>%
      bind_rows()
    
    dades_filtrades <- agg_data %>%
      semi_join(filtres, by = c("MES_ALTA_NOM", "ANY_ALTA")) %>%
      filter(DISTRICTE %in% c("Horta-Guinardó", "Nou Barris"))
    
    dades_agrupades <- dades_filtrades %>%
      group_by(BARRI, DISTRICTE) %>%
      summarise(
        RECOMPTE = sum(RECOMPTE, na.rm = TRUE),
        POBLACIO_TOTAL = first(POBLACIO_TOTAL),
        POBLACIO_HOMES = first(POBLACIO_HOMES),
        POBLACIO_DONES = first(POBLACIO_DONES),
        SUPERFICIE_TERRITORI = first(SUPERFICIE_TERRITORI),
        SUPERFICIE_RESIDENCIAL = first(SUPERFICIE_RESIDENCIAL),
        COMUNICACIONS_100HAB = round(RECOMPTE / (POBLACIO_TOTAL / 100), 2),
        .groups = "drop"
      ) %>%
      mutate(
        DENSITAT_POBLACIO_TERRITORI = round(POBLACIO_TOTAL / SUPERFICIE_TERRITORI, 3),
        DENSITAT_HOMES_TERRITORI = round(POBLACIO_HOMES / SUPERFICIE_TERRITORI, 3),
        DENSITAT_DONES_TERRITORI = round(POBLACIO_DONES / SUPERFICIE_TERRITORI, 3),
        DENSITAT_POBLACIO_RESIDENCIAL = round(POBLACIO_TOTAL / SUPERFICIE_RESIDENCIAL, 3),
        DENSITAT_HOMES_RESIDENCIAL = round(POBLACIO_HOMES / SUPERFICIE_RESIDENCIAL, 3),
        DENSITAT_DONES_RESIDENCIAL = round(POBLACIO_DONES / SUPERFICIE_RESIDENCIAL, 3)
      )
    
    
    dades_agrupades
  })
  
  # Filtre Pestanya #3
  dades_heatmap <- reactive({
    agg_data %>%
      filter(DISTRICTE %in% c("Horta-Guinardó", "Nou Barris")) %>%
      
      # Crear etiqueta ANY_MES
      mutate(ANY_MES = paste(ANY_ALTA, MES_ALTA_NOM)) %>%
      
      # Agregar per barri i mes
      group_by(BARRI, CODI_BARRI, DISTRICTE, ANY_MES) %>%
      summarise(
        RECOMPTE = sum(RECOMPTE, na.rm = TRUE),
        POBLACIO_TOTAL = first(POBLACIO_TOTAL),
        SUPERFICIE_TERRITORI = first(SUPERFICIE_TERRITORI),
        SUPERFICIE_RESIDENCIAL = first(SUPERFICIE_RESIDENCIAL),
        TEMPERATURA = first(TEMPERATURA),
        PRECIPITACIONS = first(PRECIPITACIONS),
        .groups = "drop"
      ) %>%
      mutate(
        DENSITAT_POBLACIO_TERRITORI = POBLACIO_TOTAL / SUPERFICIE_TERRITORI,
        DENSITAT_POBLACIO_RESIDENCIAL = POBLACIO_TOTAL / SUPERFICIE_RESIDENCIAL,
        RECOMPTE_100HAB = RECOMPTE / (POBLACIO_TOTAL / 100),
        RELACIO_COMUNICACIONS_POBLACIO_TERRITORI = RECOMPTE_100HAB / DENSITAT_POBLACIO_TERRITORI,
        RELACIO_COMUNICACIONS_POBLACIO_RESIDENCIAL = RECOMPTE_100HAB / DENSITAT_POBLACIO_RESIDENCIAL
      ) %>%
      # Normalització de les dades en Pestanya #3
      mutate(
        RELACIO_TERRITORI_NORM = scales::rescale(RELACIO_COMUNICACIONS_POBLACIO_TERRITORI, to = c(0, 1), na.rm = TRUE),
        RELACIO_RESIDENCIAL_NORM = scales::rescale(RELACIO_COMUNICACIONS_POBLACIO_RESIDENCIAL, to = c(0, 1), na.rm = TRUE)
      ) %>%
      
      arrange(CODI_BARRI)
  })
  
  # Gestió dels valors extrems en Pestanya #3
  capar_iqr <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    pmax(pmin(x, upper), lower)
  }
  
  nivells_mesos <- c(
    "2023 Gener", "2023 Febrer", "2023 Març", "2023 Abril", "2023 Maig", "2023 Juny",
    "2023 Juliol", "2023 Agost", "2023 Setembre", "2023 Octubre", "2023 Novembre", "2023 Desembre",
    "2024 Gener", "2024 Febrer", "2024 Març", "2024 Abril", "2024 Maig", "2024 Juny",
    "2024 Juliol", "2024 Agost", "2024 Setembre", "2024 Octubre", "2024 Novembre", "2024 Desembre"
  )
  
  output$mapa_districtes <- renderLeaflet({
    df <- districtes_data()
    pal <- colorNumeric("Blues", domain = df[[input$metrica]])
    
    leaflet(df) %>%
      setView(lng = 2.17, lat = 41.39, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(get(input$metrica)),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        layerId = ~nom_districte,
        label = ~paste(nom_districte, ":",
                       if (input$metrica == "comunicacions_100hab") {
                         paste0(round(get(input$metrica), 1))
                       } else {
                         format(round(get(input$metrica)), big.mark = ".", scientific = FALSE)
                       }),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~get(input$metrica),
                opacity = 0.7, title = noms_llegenda[[input$metrica]],
                position = "bottomright")
  })
  
  # Visualització de les dades en la Pestanya #1
  observeEvent(input$mapa_districtes_shape_click, {
    click <- input$mapa_districtes_shape_click
    clicked_districte(click$id)
  })
  
  output$grafica_districtes <- renderPlot({
    df <- districtes_data()
    ggplot(df, aes(x = reorder(nom_districte, get(input$metrica)),
                   y = get(input$metrica))) +
      geom_col(fill = "#0073C2FF") +
      coord_flip() +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Districte", y = noms_llegenda[[input$metrica]],
           title = paste("Ranking per", tolower(noms_llegenda[[input$metrica]]))) +
      theme_minimal()
  })
  
  output$taula_resum <- renderTable({
    df <- dades_resumides()
    any_especific <- input$any != "Tots els anys"
    
    comunicacions_totals <- sum(df$comunicacions, na.rm = TRUE)
    comunicacions_100hab <- if (any_especific) {
      poblacio_total_barcelona <- dades_filtrades() %>%
        distinct(BARRI, .keep_all = TRUE) %>%
        summarise(poblacio = sum(POBLACIO_TOTAL, na.rm = TRUE)) %>%
        pull(poblacio)
      100 * comunicacions_totals / poblacio_total_barcelona
    } else {
      NA
    }
    
    global <- data.frame(
      Indicador = if (any_especific) {
        c("Comunicacions totals", "Població total", "Comunicacions/100hab")
      } else {
        c("Comunicacions totals", "Comunicacions/100hab")
      },
      Barcelona = if (any_especific) {
        c(
          format(comunicacions_totals, big.mark = ".", scientific = FALSE),
          format(poblacio_total_barcelona, big.mark = ".", scientific = FALSE),
          format(round(comunicacions_100hab, 1), nsmall = 1)
        )
      } else {
        c(
          format(comunicacions_totals, big.mark = ".", scientific = FALSE),
          "-"
        )
      }
    )
    
    if (!is.null(clicked_districte())) {
      df_d <- df %>% filter(DISTRICTE == clicked_districte())
      nom_col_districte <- clicked_districte()
      proporcions <- data.frame(
        Districte = if (any_especific) {
          c(
            format(df_d$comunicacions, big.mark = ".", scientific = FALSE),
            format(df_d$poblacio, big.mark = ".", scientific = FALSE),
            format(round(df_d$comunicacions_100hab, 1), nsmall = 1)
          )
        } else {
          c(
            format(df_d$comunicacions, big.mark = ".", scientific = FALSE),
            "-"
          )
        },
        Proporcio = if (any_especific) {
          c(
            scales::percent(df_d$comunicacions / comunicacions_totals, accuracy = 0.1),
            scales::percent(df_d$poblacio / poblacio_total_barcelona, accuracy = 0.1),
            "-"
          )
        } else {
          c(
            scales::percent(df_d$comunicacions / comunicacions_totals, accuracy = 0.1),
            "-"
          )
        }
      )
      colnames(proporcions) <- c(nom_col_districte, paste("Proporció en", nom_col_districte))
      cbind(global, proporcions)
    } else {
      global
    }
  }, striped = TRUE, bordered = TRUE)
  
  
  # Visualització de les dades en la Pestanya #2
  barri_seleccionat <- reactiveVal(NULL)
  
  observeEvent(input$mapa_barris_shape_click, {
    barri_seleccionat(input$mapa_barris_shape_click$id)
  })
  
  observeEvent(input$mapa_variable_shape_click, {
    barri_seleccionat(input$mapa_variable_shape_click$id)
  })
  
  output$treemap_districtes <- renderPlot({
    df <- trimestre_data() %>%
      group_by(DISTRICTE, BARRI) %>%
      summarise(comptador = sum(RECOMPTE, na.rm = TRUE), .groups = "drop")
    
    ggplot(df, aes(area = comptador, fill = DISTRICTE, label = BARRI)) +
      geom_treemap(colour = "black") +
      scale_fill_manual(values = c("Nou Barris" = "steelblue", "Horta-Guinardó" = "lightblue")) +
      geom_treemap_text(colour = "white", place = "centre", grow = TRUE)
  })
  
  output$mapa_barris <- renderLeaflet({
    df <- trimestre_data() %>%
      group_by(BARRI) %>%
      summarise(comptador = sum(RECOMPTE, na.rm = TRUE), .groups = "drop")
    
    barris_sf_filtrat <- barris_sf %>%
      filter(nom_districte %in% c("Horta-Guinardó", "Nou Barris")) %>%
      left_join(df, by = c("nom_barri" = "BARRI"))
    
    pal <- colorNumeric("Blues", domain = barris_sf_filtrat$comptador)
    
    leaflet(barris_sf_filtrat) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2.17, lat = 41.44, zoom = 13) %>%
      addPolygons(
        fillColor = ~pal(comptador),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(nom_barri, ": ", format(comptador, big.mark = ".")),
        layerId = ~nom_barri,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addPolylines(data = districtes_sf %>% filter(nom_districte %in% c("Horta-Guinardó", "Nou Barris")),
                   color = "black", weight = 2, opacity = 1) %>%
      addLegend(pal = pal, values = ~comptador,
                position = "bottomright", title = "Comunicacions")
  })
  
  output$mapa_variable <- renderLeaflet({
    variable_seleccionada <- input$variable_mapa
    
    df <- trimestre_data() %>%
      group_by(BARRI) %>%
      summarise(valor = mean(get(variable_seleccionada), na.rm = TRUE), .groups = "drop")
    
    barris_sf_filtrat <- barris_sf %>%
      filter(nom_districte %in% c("Horta-Guinardó", "Nou Barris")) %>%
      left_join(df, by = c("nom_barri" = "BARRI"))
    
    pal <- colorNumeric("Purples", domain = barris_sf_filtrat$valor, na.color = "transparent")
    
    leaflet(barris_sf_filtrat) %>%
      setView(lng = 2.17, lat = 41.44, zoom = 12.5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(valor),
        weight = 1,
        color = "black",
        layerId = ~nom_barri,
        fillOpacity = 0.7,
        label = ~paste0(nom_barri, ": ",
                        if (input$variable_mapa %in% c("POBLACIO_TOTAL", "POBLACIO_HOMES", "POBLACIO_DONES")) {
                          format(round(valor), big.mark = ".", scientific = FALSE)
                        } else {
                          round(valor, 2)
                        }
        ),
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addPolylines(data = districtes_sf %>% filter(nom_districte %in% c("Horta-Guinardó", "Nou Barris")),
                   color = "black", weight = 2, opacity = 1) %>%
      addLegend(
        pal = pal,
        values = ~valor,
        opacity = 0.7,
        title = if (input$variable_mapa %in% names(noms_llegenda)) {
          noms_llegenda[[input$variable_mapa]]
        } else {
          input$variable_mapa
        },
        position = "bottomright"
      )
  })
  
  output$taula_element <- renderTable({
    seleccionats <- trimestres[input$trimestres[1]:input$trimestres[2]]
    
    filtres <- lapply(trimestres_map[seleccionats], function(x) {
      tibble(MES_ALTA_NOM = x$mesos, ANY_ALTA = x$any)
    }) %>%
      bind_rows()
    
    df <- agg_data %>%
      semi_join(filtres, by = c("MES_ALTA_NOM", "ANY_ALTA")) %>%
      filter(DISTRICTE %in% c("Horta-Guinardó", "Nou Barris"))
    
    df_proc <- df %>%
      group_by(DISTRICTE, ELEMENT) %>%
      summarise(comptador = sum(RECOMPTE, na.rm = TRUE), .groups = "drop") %>%
      group_by(DISTRICTE) %>%
      mutate(proporcio = comptador / sum(comptador)) %>%
      ungroup()
    
    elements_valids <- df_proc %>%
      filter(proporcio >= 0.02) %>%
      distinct(ELEMENT) %>%
      pull(ELEMENT)
    
    df_filtrat <- df_proc %>%
      filter(ELEMENT %in% elements_valids) %>%
      tidyr::pivot_wider(
        names_from = DISTRICTE,
        values_from = c(comptador, proporcio),
        names_glue = "{DISTRICTE}_{.value}"
      ) %>%
      arrange(desc(`Nou Barris_comptador`)) %>%
      mutate(
        `Horta-Guinardó_comptador` = format(`Horta-Guinardó_comptador`, big.mark = "."),
        `Nou Barris_comptador` = format(`Nou Barris_comptador`, big.mark = "."),
        `Horta-Guinardó_proporcio` = scales::percent(`Horta-Guinardó_proporcio`, accuracy = 0.1),
        `Nou Barris_proporcio` = scales::percent(`Nou Barris_proporcio`, accuracy = 0.1)
      )
    
    colnames(df_filtrat) <- c("Temàtiques", "Nombre total (Horta-Guinardó)",
                              "Nombre total (Nou Barris)", "Percentatge (Horta-Guinardó)",
                              "Percentatge (Nou Barris)")
    
    df_filtrat
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  
  output$barriSeleccionat <- reactive({
    !is.null(barri_seleccionat())
  })
  outputOptions(output, "barriSeleccionat", suspendWhenHidden = FALSE)
  
  output$nom_barri_seleccionat <- renderText({
    paste("Barri seleccionat:", barri_seleccionat())
  })
  
  output$taula_barri <- renderTable({
    req(barri_seleccionat())
    
    seleccionats <- trimestres[input$trimestres[1]:input$trimestres[2]]
    
    filtres <- lapply(trimestres_map[seleccionats], function(x) {
      tibble(MES_ALTA_NOM = x$mesos, ANY_ALTA = x$any)
    }) %>%
      bind_rows()
    
    df <- agg_data %>%
      semi_join(filtres, by = c("MES_ALTA_NOM", "ANY_ALTA")) %>%
      filter(DISTRICTE %in% c("Horta-Guinardó", "Nou Barris"),
             BARRI == barri_seleccionat()) %>%
      group_by(ELEMENT) %>%
      summarise(comptador = sum(RECOMPTE, na.rm = TRUE), .groups = "drop") %>%
      mutate(proporcio = comptador / sum(comptador)) %>%
      filter(proporcio >= 0.02) %>%
      mutate(
        comptador = format(comptador, big.mark = "."),
        proporcio = scales::percent(proporcio, accuracy = 0.1)
      ) %>%
      arrange(desc(as.numeric(gsub("\\.", "", comptador))))  # ordenar per freqüència
    
    colnames(df) <- c("Temàtica", "Nombre total", "Percentatge")
    df
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  
  # Visualització de les dades en la Pestanya #3
  # Heatmap #1: Estacionalitat de relació nombre de comunicacions per 100 habitants / densitat de població total
  output$heatmap1 <- renderPlot({
    req(dades_heatmap())
    
    df <- dades_heatmap() %>%
      filter(DISTRICTE %in% c("Horta-Guinardó", "Nou Barris"))
    
    vlines_pos <- seq(3.5, length(nivells_mesos) - 0.5, by = 3)
    
    var_heatmap <- input$variable_heatmap
    
    capar_iqr <- function(x) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      pmax(pmin(x, upper), lower)
    }
    
    df[[var_heatmap]] <- capar_iqr(df[[var_heatmap]])
    
    ggplot(df, aes(
      x = factor(ANY_MES, levels = nivells_mesos),
      y = reorder(BARRI, CODI_BARRI),
      fill = .data[[var_heatmap]]
    )) +
      geom_tile(color = "white", size = 0.25) +
      scale_fill_viridis_c(
        option = "G", begin = 0.05, end = 0.98,
        name = NULL
      ) +
      scale_x_discrete(name = NULL) +
      scale_y_discrete(name = NULL, position = "right") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        panel.grid = element_blank()
      ) +
      geom_vline(xintercept = vlines_pos, color = "black", linetype = "dashed", linewidth = 0.4)
  })
  
  # Heatmap 2: Temperatura mensual mitjana
  output$heatmap2 <- renderPlot({
    req(dades_heatmap())
    
    df <- dades_heatmap() %>%
      filter(DISTRICTE %in% c("Horta-Guinardó", "Nou Barris"))
    
    ggplot(df, aes(
      x = factor(ANY_MES, levels = nivells_mesos),
      y = "Temperatura",
      fill = TEMPERATURA
    )) +
      geom_tile(color = "white", size = 0.25) +
      scale_fill_viridis_c(
        option = "G", begin = 0.05, end = 0.98,
        name = "Temperatura (°C)"
      ) +
      scale_x_discrete(name = NULL) +
      scale_y_discrete(name = NULL, position = "right") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        panel.grid = element_blank()
      ) +
      geom_vline(xintercept = seq(3.5, length(nivells_mesos) - 0.5, by = 3),
                 color = "black", linetype = "dashed", linewidth = 0.4)
  })
  
  # Heatmap 3: Precipitació mensual mitjana
  output$heatmap3 <- renderPlot({
    req(dades_heatmap())
    
    df <- dades_heatmap() %>%
      filter(DISTRICTE %in% c("Horta-Guinardó", "Nou Barris"))
    
    ggplot(df, aes(
      x = factor(ANY_MES, levels = nivells_mesos),
      y = "Precipitació",
      fill = PRECIPITACIONS
    )) +
      geom_tile(color = "white", size = 0.25) +
      scale_fill_viridis_c(
        option = "G", begin = 0.05, end = 0.98,
        name = "Precipitació (mm)"
      ) +
      scale_x_discrete(name = NULL) +
      scale_y_discrete(name = NULL, position = "right") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "top",
        panel.grid = element_blank()
      ) +
      geom_vline(xintercept = seq(3.5, length(nivells_mesos) - 0.5, by = 3),
                 color = "black", linetype = "dashed", linewidth = 0.4)
  })
  
}

shinyApp(ui, server)