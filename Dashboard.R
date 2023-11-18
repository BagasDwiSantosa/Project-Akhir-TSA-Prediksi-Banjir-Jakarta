# Load required libraries
library(leaflet)
library(shiny)
library(DT)
library(readr)
library(shinydashboard)
library(plotly)
library(shinyjs)

# Function to modify time and date columns
modify_dataset <- function(dataset) {
  dataset$Waktu <- paste(dataset$Waktu, sep = ":")
  dataset$Tanggal <- as.Date(dataset$Tanggal, format = "%d/%m/%Y")
  return(dataset)
}

# Load data
Data_Tinggi_Muka_Air <- (read_delim("Data Tinggi Muka Air.csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE))

# Load predicted data (adjust the path accordingly)
Predicted_Data <- (read_delim("Prediksi.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE))

# Location data
lokasi_data <- data.frame(
  lokasi = c("Bendung Katulampa", "Manggarai BKB", "Pos Depok"),
  latitude = c(-6.400514, -6.633167, -6.207825),
  longitude = c(106.832, 106.836806, 106.798874)
)

# Location names
lokasi_names <- lokasi_data$lokasi

# UI definition
ui <- dashboardPage(
  dashboardHeader(
    title = "DASHBOARD PREDIKSI TINGGI MUKA AIR",
    titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PETA LETAK GEOGRAFIS", tabName = "map_tab", icon = icon("globe")),
      menuItem("GRAFIK TINGGI AIR", tabName = "plot_tab", icon = icon("bar-chart")),
      menuItem("TABEL KETINGGIAN AIR", tabName = "table_tab", icon = icon("table")),
      menuItem("PREDIKSI", tabName = "prediction_tab", icon = icon("bullseye"))
    )
  ),
  dashboardBody(
    tabItems(
      # First Tab: "map_tab"
      tabItem(
        tabName = "map_tab",
        fluidRow(
          column(12,
                 div(
                   HTML("<b style='font-size: 20px;'>LETAK POS PENGAMATAN</b>"),  # Make the text bold and adjust font size
                   style = "margin-bottom:10px;"  # Atur margin bottom sesuai kebutuhan
                 ),
                 leafletOutput("map", width = "100%", height = "450px"),
                 tags$br(),
                 HTML("<b style='font-size: 20px;'>SEKILAS INFO:</b>"),
                 HTML("<p>Sungai Ciliwung adalah salah satu sungai terpenting di Pulau Jawa, terutama karena melalui wilayah ibu kota, DKI Jakarta1. Aliran utama sungai ini memiliki panjang hampir 120 km dengan daerah tangkapan airnya (daerah aliran sungai) seluas 387 km².
<p>Hulu sungai ini berada di dataran tinggi yang terletak di perbatasan Kabupaten Bogor dan Kabupaten Cianjur, atau tepatnya di mata air Gunung Gede, Gunung Pangrango dan Telaga Saat yang terletak di lereng Pegunungan Jonggol sebelah utara kawasan Puncak, Bogor. Setelah melewati bagian timur Kota Bogor, sungai ini mengalir ke utara, di sisi barat Jalan Raya Jakarta-Bogor, sisi timur Depok, dan memasuki wilayah Jakarta sebagai batas alami wilayah Jakarta Selatan dan Jakarta Timur. Ci Liwung bermuara di daerah Luar Batang, di dekat Pasar Ikan sekarang.<p>
<p>Sistem pengendalian banjir sungai ini mencakup pembuatan sejumlah pintu air atau pos pengamatan banjir, yaitu di Katulampa (Bogor), Depok, Manggarai.</p>")
          )
        )
      ),
      
      # Second Tab: "plot_tab"
      tabItem(
        tabName = "plot_tab",
        fluidRow(
          column(4,
                 selectInput("nama_pos_plot", "Pilih Nama Pos Pengamatan:", choices = lokasi_names, selected = lokasi_names[1])
          ),
          column(4,
                 dateInput("start_date_plot", "Waktu Awal:", value = min(Data_Tinggi_Muka_Air$Tanggal))
          ),
          column(4,
                 dateInput("end_date_plot", "Waktu Akhir:", value = max(Data_Tinggi_Muka_Air$Tanggal))
          ),
          column(12,
                 plotlyOutput("plot", height = "500px"),
                 textOutput("plot_text"),
                 style = "margin-bottom: 5px;"  # Add margin to the bottom of the entire column
          )
        )
      ),
      
      # Third Tab: "table_tab"
      tabItem(
        tabName = "table_tab",
        fluidRow(
          column(4,
                 selectInput("nama_pos_table", "Pilih Nama Pos Pengamatan:", choices = lokasi_names, selected = lokasi_names[1])
          ),
          column(4,
                 dateInput("start_date_table", "Waktu Awal:", value = min(Data_Tinggi_Muka_Air$Tanggal))
          ),
          column(4,
                 dateInput("end_date_table", "Waktu Akhir:", value = max(Data_Tinggi_Muka_Air$Tanggal))
          ),
          column(12,
                 dataTableOutput("table"),
                 verbatimTextOutput("keterangan"),
                 tags$br()
          )
        )
      ),
      
      # Fourth Tab: "prediction_tab"
      tabItem(
        tabName = "prediction_tab",
        fluidRow(
          column(
            4,
            selectInput("nama_pos_table_prediction", "Pilih Nama Pos Pengamatan:", choices = lokasi_names, selected = lokasi_names[1])
          ),
          column(
            12,
            div(
              HTML("<b style='font-size: 20px;'>GRAFIK TINGGI AIR</b>"),  # Make the text bold and adjust font size
              style = "margin-bottom:10px;"  # Atur margin bottom sesuai kebutuhan
            ),
            div(
              plotlyOutput("prediction_plot", height = "350px"),
              style = "margin-bottom:10px;"  # Atur margin bottom sesuai kebutuhan
            ),
            div(
              HTML("<b style='font-size: 15px;'></b>"),
              uiOutput("keterangan_prediction_div"),  # Display the MSE value within the specified div
              style = "margin-bottom:10px;"  # Adjust margin bottom as needed
            ),
            div(
              HTML("<b style='font-size: 20px;'>TABEL KETINGGIAN AIR</b>"),  # Make the text bold and adjust font size
              style = "margin-bottom:10px;"  # Atur margin bottom sesuai kebutuhan
            ),
            dataTableOutput("table_prediction"),
            style = "margin-bottom:10px;",
            verbatimTextOutput("keterangan_prediction")
          )
        )
      )
    )
  )
)
#lengend for tab plot
get_legend_text <- function(input_nama_pos_plot) {
  switch(
    input_nama_pos_plot,
    "Bendung Katulampa" = HTML(paste(
      "<span style='font-size: 20px; color:red;'>&#x25A0;</span> Siaga 1: > 200",
      "                  <span style='font-size: 20px; color:orange;'>&#x25A0;</span> Siaga 2: > 150",
      "                  <span style='font-size: 20px; color:yellow;'>&#x25A0;</span> Siaga 3: > 80",
      "                  <span style='font-size: 20px; color:green;'>&#x25A0;</span> Siaga 4: < 80"
    )),
    "Manggarai BKB" = HTML(paste(
      "<span style='font-size: 20px; color:red;'>&#x25A0;</span> Siaga 1: > 950",
      "                 <span style='font-size: 20px; color:orange;'>&#x25A0;</span> Siaga 2: > 850",
      "                 <span style='font-size: 20px; color:yellow;'>&#x25A0;</span> Siaga 3: > 750",
      "                 <span style='font-size: 20px; color:green;'>&#x25A0;</span> Siaga 4: < 750"
    )),
    "Pos Depok" = HTML(paste(
      "<span style='font-size: 20px; color:red;'>&#x25A0;</span> Siaga 1: > 350",
      "                 <span style='font-size: 20px; color:orange;'>&#x25A0;</span> Siaga 2: > 270",
      "                 <span style='font-size: 20px; color:yellow;'>&#x25A0;</span> Siaga 3: > 200",
      "                 <span style='font-size: 20px; color:green;'>&#x25A0;</span> Siaga 4: < 200"
    )),
    "Pos Default" = HTML("Data tidak tersedia")
  )
}

# Server function
server <- function(input, output) {
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet(data = lokasi_data) %>%
      addTiles() %>%
      addMarkers(lng = lokasi_data$longitude, lat = lokasi_data$latitude, label = lokasi_data$lokasi) 
  })
  
  # Filtered data based on date range
  filtered_data <- reactive({
    start_date <- input$start_date_table
    end_date <- input$end_date_table
    data <- rbind(Data_Tinggi_Muka_Air)  # Combine historical and predicted data
    data <- subset(data, Tanggal >= start_date & Tanggal <= end_date)
    return(data)
  })
  
  # Get selected location data for plot
  selected_location_data_plot <- reactive({
    nama_pos_plot <- input$nama_pos_plot
    selected_data_plot <- switch(
      nama_pos_plot,
      "Bendung Katulampa" = {
        selected_data_plot <- filtered_data() %>%
          select(Tanggal, Waktu, Katulampa)
        colnames(selected_data_plot)[3] <- "TMA"
        selected_data_plot
      },
      "Manggarai BKB" = {
        selected_data_plot <- filtered_data() %>%
          select(Tanggal, Waktu, Manggarai)
        colnames(selected_data_plot)[3] <- "TMA"
        selected_data_plot
      },
      "Pos Depok" = {
        selected_data_plot <- filtered_data() %>%
          select(Tanggal, Waktu, Depok)
        colnames(selected_data_plot)[3] <- "TMA"
        selected_data_plot
      },
      # Handle if the selected location is not valid
      NULL
    )
  })

  # render plot tab
  output$plot <- renderPlotly({
    if (!is.null(selected_location_data_plot())) {
      selected_data_plot <- selected_location_data_plot()
      
      # Lakukan logika indikator status banjir sesuai pos pengamatan yang dipilih
      status_siaga <- switch(
        input$nama_pos_plot,
        "Bendung Katulampa" = {
          ifelse(any(selected_data_plot$TMA > 200), "Siaga 1",
                 ifelse(any(selected_data_plot$TMA > 150), "Siaga 2",
                        ifelse(any(selected_data_plot$TMA > 80), "Siaga 3", "Normal")))
        },
        "Manggarai BKB" = {
          ifelse(any(selected_data_plot$TMA > 950), "Siaga 1",
                 ifelse(any(selected_data_plot$TMA > 850), "Siaga 2",
                        ifelse(any(selected_data_plot$TMA > 750), "Siaga 3", "Normal")))
        },
        "Pos Depok" = {
          ifelse(any(selected_data_plot$TMA > 350), "Siaga 1",
                 ifelse(any(selected_data_plot$TMA > 270), "Siaga 2",
                        ifelse(any(selected_data_plot$TMA > 200), "Siaga 3", "Normal")))
        },
        # Handle if the selected location is not valid
        "Pos Default" = "Data tidak tersedia"
      )
      
      # Menghasilkan teks untuk ditampilkan di bawah plot
      status_text <- paste("")
      
      # Get legend text based on the selected location
      legend_text <- get_legend_text(input$nama_pos_plot)
      
      # Set judul plot sesuai nama pos yang dipilih
      plot_title <- paste("Grafik Tinggi Air -", input$nama_pos_plot)
      
      # Define line color based on status_siaga
      line_color <- switch(
        status_siaga,
        "Siaga 1" = "red",
        "Siaga 2" = "orange",
        "Siaga 3" = "yellow",
        "Normal" = "green",
        "Data tidak tersedia" = "gray"  # Adjust the color for this case
      )
      
      # Lakukan plotly
      plot <- plot_ly(data = selected_data_plot, x = ~Tanggal, y = ~TMA, type = "scatter", mode = "lines", name = input$nama_pos_plot, line = list(color = line_color, width = 2)) %>%
        layout(
          paper_bgcolor = 'rgba(0,0,0,0)',  # Mengatur latar belakang kanvas (background) menjadi transparan
          plot_bgcolor = 'rgba(0,0,0,0)',   # Mengatur latar belakang plot menjadi transparan
          xaxis = list(range = c(min(selected_data_plot$Tanggal), max(selected_data_plot$Tanggal))),  # Menentukan rentang sumbu x
          annotations = list(
            text = legend_text,  # Menambahkan teks legenda
            x = 0.5,  # Set x to 0.5 to center the legend horizontally
            y = -0.2,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE
          ),
          legend = list(x = 0.5, y = 1),  # Center the legend horizontally at the top
          title = plot_title  # Set judul grafik sesuai nama pos yang dipilih
        ) %>%
        # Tambahkan teks status sebagai annotation di bawah plot
        add_annotations(
          text = status_text,
          x = min(selected_data_plot$Tanggal),
          y = min(selected_data_plot$TMA) - 1,  # Adjust the position as needed
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          yanchor = "bottom",
          align = "center"
        )
      
      # Adjust the margin to make space for the legend
      plot <- plot %>% layout(margin = list(b = 80))
      
      return(plot)
    }
  })
  
  
  # Get selected location data for table
  selected_location_data_table <- reactive({
    nama_pos_table <- input$nama_pos_table
    selected_data_table <- switch(
      nama_pos_table,
      "Bendung Katulampa" = {
        selected_data_table <- filtered_data() %>%
          select(Tanggal, Waktu, Katulampa)
        colnames(selected_data_table)[3] <- "TMA"
        selected_data_table$`Status Siaga` <- ifelse(selected_data_table$TMA > 200, "1",
                                                     ifelse(selected_data_table$TMA > 150, "2",
                                                            ifelse(selected_data_table$TMA > 80, "3", "4")))
        selected_data_table
      },
      "Manggarai BKB" = {
        selected_data_table <- filtered_data() %>%
          select(Tanggal, Waktu, Manggarai)
        colnames(selected_data_table)[3] <- "TMA"
        selected_data_table$`Status Siaga` <- ifelse(selected_data_table$TMA > 950, "1",
                                                     ifelse(selected_data_table$TMA > 850, "2",
                                                            ifelse(selected_data_table$TMA > 750, "3", "4")))
        selected_data_table
      },
      "Pos Depok" = {
        selected_data_table <- filtered_data() %>%
          select(Tanggal, Waktu, Depok)
        colnames(selected_data_table)[3] <- "TMA"
        selected_data_table$`Status Siaga` <- ifelse(selected_data_table$TMA > 350, "1",
                                                     ifelse(selected_data_table$TMA > 270, "2",
                                                            ifelse(selected_data_table$TMA > 200, "3", "4")))
        selected_data_table
      },
      # Handle if the selected location is not valid
      NULL
    )
  })
  
  # Render data table for table
  output$table <- renderDataTable({
    if (!is.null(selected_location_data_table())) {
      datatable(selected_location_data_table(), options = list(pageLength = 10,
                                                               rowCallback = JS(
                                                                 "function(row, data, index) {",
                                                                 "var status = data[3];",
                                                                 "if (status === 'Bahaya') {",
                                                                 "$('td', row).eq(3).css('background-color', 'red').css('color', 'white');",
                                                                 "} else if (status === 'Siaga') {",
                                                                 "$('td', row).eq(3).css('background-color', 'orange');",
                                                                 "} else if (status === 'Waspada') {",
                                                                 "$('td', row).eq(3).css('background-color', 'yellow');",
                                                                 "} else {",
                                                                 "$('td', row).eq(3).css('background-color', 'green').css('color', 'white');",
                                                                 "}",
                                                                 "}")
      ))
    }
  })
  
  # Render text output for keterangan
  output$keterangan <- renderText({
    "Keterangan:\nTMA: Tinggi Muka Air (dalam satuan cm)\nStatus Siaga:\n1 = Bahaya\n2 = Siaga\n3 = Waspada\n4 = Normal"
  })
  
  # Get selected location data for prediction tab
  selected_location_data_prediction <- reactive({
    nama_pos_table_prediction <- input$nama_pos_table_prediction
    selected_data_prediction <- switch(
      nama_pos_table_prediction,
      "Bendung Katulampa" = {
        selected_data_prediction <- Predicted_Data %>%
          select(Tanggal, Waktu, Katulampa)
        colnames(selected_data_prediction)[3] <- "TMA_Prediction"
        selected_data_prediction$`Status Siaga` <- ifelse(selected_data_prediction$TMA_Prediction > 200, "1",
                                                          ifelse(selected_data_prediction$TMA_Prediction > 150, "2",
                                                                 ifelse(selected_data_prediction$TMA_Prediction > 80, "3", "4")))
        selected_data_prediction
      },
      "Manggarai BKB" = {
        selected_data_prediction <- Predicted_Data %>%
          select(Tanggal, Waktu, Manggarai)
        colnames(selected_data_prediction)[3] <- "TMA_Prediction"
        selected_data_prediction$`Status Siaga` <- ifelse(selected_data_prediction$TMA_Prediction > 950, "1",
                                                          ifelse(selected_data_prediction$TMA_Prediction > 850, "2",
                                                                 ifelse(selected_data_prediction$TMA_Prediction > 750, "3", "4")))
        selected_data_prediction
      },
      "Pos Depok" = {
        selected_data_prediction <- Predicted_Data %>%
          select(Tanggal, Waktu, Depok)
        colnames(selected_data_prediction)[3] <- "TMA_Prediction"
        selected_data_prediction$`Status Siaga` <- ifelse(selected_data_prediction$TMA_Prediction > 350, "1",
                                                          ifelse(selected_data_prediction$TMA_Prediction > 270, "2",
                                                                 ifelse(selected_data_prediction$TMA_Prediction > 200, "3", "4")))
        selected_data_prediction
      },
      # Handle if the selected location is not valid
      NULL
    )
  })
  
  #legend for prediction plot
  get_legend_text_prediction <- function(input_nama_pos_plot) {
    switch(
      input_nama_pos_plot,
      "Bendung Katulampa" = HTML(paste(
        "<span style='font-size: 20px; color:red;'>&#x25A0;</span> Siaga 1: > 200",
        "                  <span style='font-size: 20px; color:orange;'>&#x25A0;</span> Siaga 2: > 150",
        "                  <span style='font-size: 20px; color:yellow;'>&#x25A0;</span> Siaga 3: > 80",
        "                  <span style='font-size: 20px; color:green;'>&#x25A0;</span> Siaga 4: < 80"
      )),
      "Manggarai BKB" = HTML(paste(
        "<span style='font-size: 20px; color:red;'>&#x25A0;</span> Siaga 1: > 950",
        "                 <span style='font-size: 20px; color:orange;'>&#x25A0;</span> Siaga 2: > 850",
        "                 <span style='font-size: 20px; color:yellow;'>&#x25A0;</span> Siaga 3: > 750",
        "                 <span style='font-size: 20px; color:green;'>&#x25A0;</span> Siaga 4: < 750"
      )),
      "Pos Depok" = HTML(paste(
        "<span style='font-size: 20px; color:red;'>&#x25A0;</span> Siaga 1: > 350",
        "                 <span style='font-size: 20px; color:orange;'>&#x25A0;</span> Siaga 2: > 270",
        "                 <span style='font-size: 20px; color:yellow;'>&#x25A0;</span> Siaga 3: > 200",
        "                 <span style='font-size: 20px; color:green;'>&#x25A0;</span> Siaga 4: < 200"
      )),
      "Pos Default" = HTML("Data tidak tersedia")
    )
  }
  
  # Render plot for prediction tab
  output$prediction_plot <- renderPlotly({
    if (!is.null(selected_location_data_prediction())) {
      selected_data_pred <- selected_location_data_prediction()
      
      # Convert Tanggal to Date format
      selected_data_pred$Tanggal <- as.Date(selected_data_pred$Tanggal)
      
      # Menentukan warna berdasarkan Status Siaga
      status_color_vector <- ifelse(selected_data_pred$`Status Siaga` == "1", "red",
                                    ifelse(selected_data_pred$`Status Siaga` == "2", "orange",
                                           ifelse(selected_data_pred$`Status Siaga` == "3", "yellow", "green")))
      
      plot_ly(
        data = selected_data_pred, 
        x = ~Tanggal, 
        y = ~TMA_Prediction, 
        type = "scatter", 
        mode = "lines", 
        name = input$nama_pos_table_prediction,
        line = list(color = status_color_vector)  # Mengatur warna garis berdasarkan Status Siaga
      ) %>%
        layout(
          paper_bgcolor = 'rgba(0,0,0,0)',  # Mengatur latar belakang kanvas (background) menjadi transparan
          plot_bgcolor = 'rgba(0,0,0,0)',    # Mengatur latar belakang plot menjadi transparan
          xaxis = list(title = "Tanggal", margin = list(t = 30), tickformat = "%Y-%m-%d"),   # Add x-axis title, adjust top margin, and set the tick format for date only
          yaxis = list(title = "TMA Prediction"),  # Add y-axis title
          legend = list(x = 0.5, y = 1.1, traceorder = "normal", font = list(size = 10)),  # Adjust legend position and font size
          annotations = list(
            text = get_legend_text_prediction(input$nama_pos_table_prediction),  # Menambahkan teks legenda
            x = 0.5,  # Set x to 0.5 to center the legend horizontally
            y = 1.05,  # Adjust the y-coordinate above the plot
            xref = "paper",
            yref = "paper",
            showarrow = FALSE
          ),
          # Adjust the margin to make space for the legend
          margin = list(b = 10),
          title = input$nama_pos_table_prediction
        )
    }
  })
  
  # Define MSE values for each location
  mse_values_manggarai <- 25.84
  mse_values_katulampa <- 1.06
  mse_values_depok <- 15.52
  
  # Render the MSE value
  output$keterangan_prediction <- renderText({
    selected_location <- input$nama_pos_table_prediction
    print(paste("Selected Location:", selected_location))  # Print the selected location for debugging
    
    # Use switch or if-else statements to determine the MSE value based on the location
    mse <- switch(selected_location,
                  'Bendungan Katulampa' = mse_values_katulampa,
                  'Manggarai BKB' = mse_values_manggarai,
                  'Pos Depok' = mse_values_depok)  
    
    paste("", mse)
  })
  
  # Display the MSE value within the specified div
  output$keterangan_prediction_div <- renderUI({
    selected_location <- input$nama_pos_table_prediction
    print(paste("Selected Location (UI):", selected_location))  # Print the selected location for debugging
    
    div(
      renderText({
        # Use switch or if-else statements to determine the MSE value based on the location
        mse <- switch(selected_location,
                      'Bendung Katulampa' = mse_values_katulampa,
                      'Manggarai BKB' = mse_values_manggarai,
                      'Pos Depok' = mse_values_depok,
                      0)  # Default value if location not found
        print(paste("MSE Prediction:", mse))  # Print the MSE value for debugging
        # Highlight the selected city with a specific style
        HTML(paste("MSE Prediction for ", selected_location, ": ", mse))  
      }),
      style = "margin-bottom:10px; text-align: right;"  # Adjust margin bottom and text alignment as needed
    )
  })
  
  
  # Render data table for prediction tab
  output$table_prediction <- renderDataTable({
    if (!is.null(selected_location_data_prediction())) {
      datatable(selected_location_data_prediction(), options = list(pageLength = 5,
                                                                    rowCallback = JS(
                                                                      "function(row, data, index) {",
                                                                      "var status = data[3];",
                                                                      "if (status === 'Bahaya') {",
                                                                      "$('td', row).eq(3).css('background-color', 'red').css('color', 'white');",
                                                                      "} else if (status === 'Siaga') {",
                                                                      "$('td', row).eq(3).css('background-color', 'orange');",
                                                                      "} else if (status === 'Waspada') {",
                                                                      "$('td', row).eq(3).css('background-color', 'yellow');",
                                                                      "} else {",
                                                                      "$('td', row).eq(3).css('background-color', 'green').css('color', 'white');",
                                                                      "}",
                                                                      "}"),
                                                                    margin = list(b = 200)  # Sesuaikan nilai margin sesuai kebutuhan
      ))
    }
  }, rownames = FALSE)
  # Render text output for keterangan
  output$keterangan_prediction <- renderText({
    "Keterangan:\nTMA: Tinggi Muka Air (dalam satuan cm)\nStatus Siaga:\n1 = Bahaya\n2 = Siaga\n3 = Waspada\n4 = Normal"
  })
}
# Run the Shiny App
shinyApp(ui, server) 