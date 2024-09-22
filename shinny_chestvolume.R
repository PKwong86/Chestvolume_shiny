library(shiny)
library(geometry)
library(tidyverse)
library(plotly)
library(readxl)

# Define the coordinates for A and B
adj_position <- function(y, center) {
  A <- unlist(center)
  B <- unlist(y)
  direction_vector <- A - B
  distance <- sqrt(sum(direction_vector ^ 2))
  unit_vector <- direction_vector / distance
  new_B <- B + 0.7 * unit_vector
  return(new_B)
}

# Define the Shiny app UI
ui <- fluidPage(
  titlePanel("Volume Plot from Excel Data"),
  tabsetPanel(
    tabPanel("Volume Plot and Calculation",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload Excel File", accept = ".xlsx"),
                 h4("Note: The file should have multiple of 3 columns of data (X, Y, Z coordinates)."),
                 checkboxInput("useDefaultSegments", "Use Default Segments", TRUE),
                 conditionalPanel(
                   condition = "input.useDefaultSegments == false",
                   numericInput("numSegments", "Number of Segments", min = 1, max = 10, value = 2),
                   uiOutput("segmentInputs"),
                   uiOutput("segmentNames")
                 ),
                 selectInput("visualizationType", "Select Visualization Type",
                             choices = c("Relative Change (2D)" = "relative",
                                         "Absolute Value (2D)" = "absolute",
                                         "Raw Data (2D)" = "raw")),
                 sliderInput("baselineFrames", "Number of Data Points for Baseline Calculation:",
                             min = 10, max = 200, value = 10, step = 10),
                 uiOutput("rangeSelect1"),  # Range for 'tidal 1'
                 uiOutput("rangeSelect2"),  # Range for 'tidal 2'
                 uiOutput("rangeSelect3"),  # Range for 'tidal 3'
                 uiOutput("rangeSelectVC"),  # Range for 'VC'
                 actionButton("calcButton", "Calculate")  # New button to trigger calculation
               ),
               mainPanel(
                 plotOutput("volumePlot", height = "400px"),  # GGplot 2D plot
                 h4("Original Calculations: Absolute and Relative Changes from Baseline"),
                 tableOutput("originalChangeTable"),
                 h4("Max-to-Min Changes"),
                 h5("'Tidal 1' Range"),
                 tableOutput("maxChangeTidal1"),
                 h5("'Tidal 2' Range"),
                 tableOutput("maxChangeTidal2"),
                 h5("'Tidal 3' Range"),
                 tableOutput("maxChangeTidal3"),
                 h5("'VC' Range"),
                 tableOutput("maxChangeVC"),
                 h4("Mean of 'Tidal 1', 'Tidal 2', and 'Tidal 3' Ranges"),
                 tableOutput("meanTidalRanges")
               )
             )
    ),
    tabPanel("3D Animation",
             fluidRow(
               column(12,
                      sliderInput("frameStep", "Frame Step for Animation:", min = 1, max = 20, value = 10),
                      plotlyOutput("animation3D", height = "600px")  # 3D animation
               )
             )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive expression to load and process the data
  data <- reactive({
    req(input$file)
    
    # Read the uploaded file
    df <- read_excel(input$file$datapath)
    
    # Sort columns to reorder the points
    reform <- sort(names(df))
    df <- df[, reform]
    
    # Number of points is determined by the number of columns divided by 3
    n_points <- ncol(df) / 3
    if (n_points != round(n_points)) {
      stop("The number of columns in the dataset must be a multiple of 3 (X, Y, Z coordinates for each point).")
    }
    
    # Segment definitions: default or user-defined
    if (input$useDefaultSegments) {
      segment <- list(
        c(1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17),
        c(16, 17, 19, 20, 22, 23, 25, 26, 28, 29, 1, 2),
        c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18),
        c(17, 18, 20, 21, 23, 24, 26, 27, 29, 30, 2, 3)
      )
      segment_names <- c("UL", "UR", "LL", "LR")
    } else {
      numSegments <- input$numSegments
      segment <- lapply(1:numSegments, function(i) {
        segment_points <- input[[paste0("segment_", i)]]
        as.numeric(strsplit(segment_points, ",")[[1]])
      })
      segment_names <- sapply(1:numSegments, function(i) input[[paste0("segment_name_", i)]])
    }
    
    # Volume calculation
    vol_l <- list()
    for (h in 1:length(segment)) {
      vol <- vector()
      for (k in 10:nrow(df)) {
        points <- matrix(unlist(df[k, ]), ncol = 3, byrow = TRUE) / 10
        points <- as.data.frame(points)
        colnames(points) <- c("X", "Y", "Z")
        
        center <- colMeans(points)
        point2 <- t(apply(points, 1, adj_position, center = center))
        point_adj <- as.data.frame(point2)
        colnames(point_adj) <- c("V1", "V2", "V3")
        
        hull <- convhulln(point_adj[segment[[h]], ], options = "FA")
        volume <- hull$vol
        vol <- c(vol, volume)
      }
      vol_l[[h]] <- vol
    }
    
    # Prepare 2D data for plotting
    v_by_seg <- matrix(unlist(vol_l), nrow = length(vol_l[[1]]))
    
    # Determine axis limits for 3D plot
    axis_limits <- list(
      xlim = range(df[, seq(1, ncol(df), by = 3)]),
      ylim = range(df[, seq(2, ncol(df), by = 3)]),
      zlim = range(df[, seq(3, ncol(df), by = 3)])
    )
    
    list(df = df, vol_l = vol_l, v_by_seg = v_by_seg, segments = segment, segment_names = segment_names, n_points = n_points, axis_limits = axis_limits)
  })
  
  # Render dynamic UI sliders for selecting frame ranges
  output$rangeSelect1 <- renderUI({
    data <- data()
    n_rows <- nrow(data$df)
    sliderInput("rangeTidal1", "Select Range for 'Tidal 1':", min = 1, max = n_rows, value = c(1, round(n_rows / 6)), step = 1)
  })
  
  output$rangeSelect2 <- renderUI({
    data <- data()
    n_rows <- nrow(data$df)
    sliderInput("rangeTidal2", "Select Range for 'Tidal 2':", min = 1, max = n_rows, value = c(round(n_rows / 6) + 1, round(n_rows / 3)), step = 1)
  })
  
  output$rangeSelect3 <- renderUI({
    data <- data()
    n_rows <- nrow(data$df)
    sliderInput("rangeTidal3", "Select Range for 'Tidal 3':", min = 1, max = n_rows, value = c(round(n_rows / 3) + 1,round(n_rows / 3) + 50), step = 1)
  })
  
  output$rangeSelectVC <- renderUI({
    data <- data()
    n_rows <- nrow(data$df)
    sliderInput("rangeVC", "Select Range for 'VC':", min = 1, max = n_rows, value = c(round(3 * n_rows / 4) + 1, round(3 * n_rows / 4) + 50), step = 1)
  })
  
  # Reactive expression triggered by the "Calculate" button for tables
  calculate_data <- eventReactive(input$calcButton, {
    data()
  })
  
  # Render the 2D plot (either relative change, absolute value, or raw data)
  output$volumePlot <- renderPlot({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    v_by_seg <- data$v_by_seg
    segment_names <- data$segment_names
    
    # Dynamically calculate the baseline using the first 'baselineFrames' from the beginning of the original data
    baselineFrames <- input$baselineFrames
    baseline <- colMeans(v_by_seg[1:baselineFrames, ])
    
    # Apply frame range selection for display/processing, but changes are always compared to the baseline
    if (input$visualizationType == "relative") {
      v_n <- as.data.frame(t(t(v_by_seg) / baseline))
    } else if (input$visualizationType == "absolute") {
      v_n <- as.data.frame(t(t(v_by_seg) - baseline))
    } else if (input$visualizationType == "raw") {
      v_n <- as.data.frame(v_by_seg)
    }
    
    colnames(v_n) <- segment_names
    v_n$frame <- 1:nrow(v_n)
    v_l <- gather(v_n, key = segment, volumn, -frame)
    
    # Plot the volumes and add vertical lines for selected range
    ggplot(v_l, aes(x = frame, y = volumn, col = segment)) +
      geom_line(size = 1) +
      labs(title = paste("Volume Plot (", input$visualizationType, ")", sep = ""),
           x = "Frame", y = ifelse(input$visualizationType == "relative", "Relative Volume", 
                                   ifelse(input$visualizationType == "absolute", "Absolute Volume", "Raw Volume"))) +
      geom_vline(xintercept = input$rangeTidal1[1], linetype = "dashed", color = "red", size = 1.5) +  # Start of tidal 1 range
      geom_vline(xintercept = input$rangeTidal1[2], linetype = "dashed", color = "red", size = 1.5) +  # End of tidal 1 range
      geom_vline(xintercept = input$rangeTidal2[1], linetype = "dashed", color = "green", size = 1.5) +  # Start of tidal 2 range
      geom_vline(xintercept = input$rangeTidal2[2], linetype = "dashed", color = "green", size = 1.5) +  # End of tidal 2 range
      geom_vline(xintercept = input$rangeTidal3[1], linetype = "dashed", color = "blue", size = 1.5) +  # Start of tidal 3 range
      geom_vline(xintercept = input$rangeTidal3[2], linetype = "dashed", color = "blue", size = 1.5) +  # End of tidal 3 range
      geom_vline(xintercept = input$rangeVC[1], linetype = "dashed", color = "purple", size = 1.5) +  # Start of VC range
      geom_vline(xintercept = input$rangeVC[2], linetype = "dashed", color = "purple", size = 1.5) +  # End of VC range
      theme_minimal()
  })
  
  # Calculation helper for max-to-min changes for a given range
  calculate_max_min_change <- function(range, v_by_seg, baseline) {
    abs_change <- t(t(v_by_seg) - baseline)
    relative_change <- t(t(v_by_seg) / baseline)
    max_min_abs_change <- apply(abs_change[range[1]:range[2], ], 2, function(x) max(x) - min(x))
    max_min_rel_change <- apply(relative_change[range[1]:range[2], ], 2, function(x) max(x) - min(x))
    list(abs_change = max_min_abs_change, rel_change = max_min_rel_change)
  }
  
  # Render the table with original absolute and relative changes
  output$originalChangeTable <- renderTable({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    v_by_seg <- data$v_by_seg
    segment_names <- data$segment_names
    
    # Baseline calculation
    baseline <- colMeans(v_by_seg[1:input$baselineFrames, ])
    
    # Calculate absolute and relative changes
    abs_change <- t(t(v_by_seg) - baseline)
    relative_change <- t(t(v_by_seg) / baseline)
    
    original_change_df <- data.frame(
      Segment = segment_names,
      Max_Pos_Absolute_Change = apply(abs_change, 2, max),
      Max_Neg_Absolute_Change = apply(abs_change, 2, min),
      Max_Pos_Relative_Change = apply(relative_change, 2, max),
      Max_Neg_Relative_Change = apply(relative_change, 2, min)
    )
    
    original_change_df
  })
  
  # Render max-to-min changes for each range
  output$maxChangeTidal1 <- renderTable({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    v_by_seg <- data$v_by_seg
    segment_names <- data$segment_names
    
    baseline <- colMeans(v_by_seg[1:input$baselineFrames, ])
    tidal1_range <- input$rangeTidal1
    max_min_change <- calculate_max_min_change(tidal1_range, v_by_seg, baseline)
    
    data.frame(Segment = segment_names, Max_To_Min_Abs_Change = max_min_change$abs_change, Max_To_Min_Rel_Change = max_min_change$rel_change)
  })
  
  output$maxChangeTidal2 <- renderTable({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    v_by_seg <- data$v_by_seg
    segment_names <- data$segment_names
    
    baseline <- colMeans(v_by_seg[1:input$baselineFrames, ])
    tidal2_range <- input$rangeTidal2
    max_min_change <- calculate_max_min_change(tidal2_range, v_by_seg, baseline)
    
    data.frame(Segment = segment_names, Max_To_Min_Abs_Change = max_min_change$abs_change, Max_To_Min_Rel_Change = max_min_change$rel_change)
  })
  
  output$maxChangeTidal3 <- renderTable({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    v_by_seg <- data$v_by_seg
    segment_names <- data$segment_names
    
    baseline <- colMeans(v_by_seg[1:input$baselineFrames, ])
    tidal3_range <- input$rangeTidal3
    max_min_change <- calculate_max_min_change(tidal3_range, v_by_seg, baseline)
    
    data.frame(Segment = segment_names, Max_To_Min_Abs_Change = max_min_change$abs_change, Max_To_Min_Rel_Change = max_min_change$rel_change)
  })
  
  output$maxChangeVC <- renderTable({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    v_by_seg <- data$v_by_seg
    segment_names <- data$segment_names
    
    baseline <- colMeans(v_by_seg[1:input$baselineFrames, ])
    vc_range <- input$rangeVC
    max_min_change <- calculate_max_min_change(vc_range, v_by_seg, baseline)
    
    data.frame(Segment = segment_names, Max_To_Min_Abs_Change = max_min_change$abs_change, Max_To_Min_Rel_Change = max_min_change$rel_change)
  })
  
  # Calculate and display the mean of Tidal 1, Tidal 2, and Tidal 3 ranges
  output$meanTidalRanges <- renderTable({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    v_by_seg <- data$v_by_seg
    segment_names <- data$segment_names
    
    baseline <- colMeans(v_by_seg[1:input$baselineFrames, ])
    
    tidal1_range <- input$rangeTidal1
    tidal2_range <- input$rangeTidal2
    tidal3_range <- input$rangeTidal3
    
    tidal1_change <- calculate_max_min_change(tidal1_range, v_by_seg, baseline)
    tidal2_change <- calculate_max_min_change(tidal2_range, v_by_seg, baseline)
    tidal3_change <- calculate_max_min_change(tidal3_range, v_by_seg, baseline)
    
    mean_abs_change <- rowMeans(cbind(tidal1_change$abs_change, tidal2_change$abs_change, tidal3_change$abs_change))
    mean_rel_change <- rowMeans(cbind(tidal1_change$rel_change, tidal2_change$rel_change, tidal3_change$rel_change))
    
    data.frame(Segment = segment_names, Mean_Abs_Change = mean_abs_change, Mean_Rel_Change = mean_rel_change)
  })
  
  # Render the 3D animation with frame progression
  output$animation3D <- renderPlotly({
    req(input$file)
    
    data <- calculate_data()  # Reactively triggered by the button
    df <- data$df
    segments <- data$segments
    segment_names <- data$segment_names
    step_size <- input$frameStep  # Step size for animation
    
    # Get the number of rows in the uploaded data
    n_frames <- nrow(df)
    
    # Initialize plotly object for animation
    p <- plot_ly()
    
    # Generate frames for animation (e.g., every step_size frames)
    for (k in seq(10, n_frames, by = step_size)) {
      points <- matrix(unlist(df[k, ]), ncol = 3, byrow = TRUE)
      points <- as.data.frame(points)
      colnames(points) <- c("X", "Y", "Z")
      
      # Apply adj_position function and adjust points
      center <-  colMeans(points)
      point2 <- t(apply(points, 1, adj_position, center = center))
      point_adj <- as.data.frame(point2)
      colnames(point_adj) <- c("V1", "V2", "V3")  # Ensure correct column names
      
      # Add points for the current frame
      p <- p %>%
        add_trace(
          data = point_adj, x = ~V1, y = ~V2, z = ~V3,
          type = "scatter3d", mode = "markers",
          marker = list(size = 5, color = 'blue'),
          frame = k  # Add frame identifier
        )
      
      # Loop through each segment to add convex hull surfaces for this frame
      for (i in 1:length(segments)) {
        segment_points <- point_adj[segments[[i]], ]
        hull <- convhulln(segment_points, output.options = "T")  # Convex hull indices
        
        # Add mesh for this segment in the current frame
        p <- p %>%
          add_trace(
            x = segment_points$V1, y = segment_points$V2, z = segment_points$V3,
            i = hull[, 1] - 1, j = hull[, 2] - 1, k = hull[, 3] - 1,
            type = "mesh3d", opacity = 1, name = segment_names[i],
            showscale = FALSE,
            frame = k  # Add frame identifier
          )
      }
    }
    
    # Animation options and layout with fixed axis limits
    p %>%
      animation_opts(frame = 100, transition = 0, redraw = TRUE) %>%
      layout(
        scene = list(
          xaxis = list(title = 'X', range = data$axis_limits$xlim),
          yaxis = list(title = 'Y', range = data$axis_limits$ylim),
          zaxis = list(title = 'Z', range = data$axis_limits$zlim)
        )
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
