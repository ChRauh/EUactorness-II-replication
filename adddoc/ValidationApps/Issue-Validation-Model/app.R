library(shiny)

# Shiny App
ui <- fluidPage(
  titlePanel(HTML("<br><em>Coding task:</em> Issue contexts of political statements</em><br><br>")),
  
  sidebarLayout(
    sidebarPanel(
      style = "text-align: justify;",
      h3("Instructions"),
      p(HTML("You see short excerpts from international political speeches and should <b>assess to what extent they speak on issues related to three broad areas:</b>")),
      tags$ul(style = "padding-left: 0; list-style-position: outside; text-align: match-parent;list-style-type: none;",
              tags$li(HTML("<em><b style='color: #1b9e77;'>Trade & economy</b></em> - including issues like commerce, business, or markets, for example.")),
              tags$li(HTML("<em><b style='color: #d95f02;'>Liberal democracy</b></em> - including issues such as the rule of law, human rights, or political freedoms, for example.")),
              tags$li(HTML("<em><b style='color: #7570b3;'>Security</b></em> - including issues like war and peace, military, or terrorism, for example."))
      ),
      p(HTML("These broad areas may overlap in the text and will not always be literally mentioned. There are no clear right or wrong answers - <b>your interpretation matters.</b>")),
      verbatimTextOutput("status"),
      p(HTML("Your assessments are saved on every text example. <b>You can close and later reload the app whenever you need a break.</b> Many thanks for your help!")),
      p(HTML("I am happy to tell you more about the research context once the coding is done. If other questions occur, please do not hesitate to contact me via <a href=\"mailto:rauh@wzb.eu\">Email</a>."))
    ),
    
    mainPanel(
      HTML("<br>"),
      h3("The text and your personal assessment"),
      div(
        id = "text-frame",
        uiOutput("current_text")
      ),
      HTML("<br>"),
      
      # Classification radio buttons and next button placed together
      div(
        # First set of radio buttons
        radioButtons("classification1", 
                     HTML("<span style='color: #1b9e77;'>Does the text invoke or imply <em>trade & economy issues</em>?</span>"),
                     choices = c("Clearly not", "Probably not", "Probably yes", "Clearly yes"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Second set of radio buttons
        radioButtons("classification2", 
                     HTML("<span style='color: #d95f02;'>Does the text invoke or imply <em>liberal democracy issues</em>?</span>"),
                     choices = c("Clearly not", "Probably not", "Probably yes", "Clearly yes"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Third set of radio buttons
        radioButtons("classification3", 
                     HTML("<span style='color: #7570b3;'>Does the text invoke or imply <em>security issues</em>?</span>"),
                     choices = c("Clearly not", "Probably not", "Probably yes", "Clearly yes"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Continue button
        actionButton("next_btn", "Continue", style = "margin-top: 10px; background-color: #4CAF50; color: white;")
      ),
      
      # Add custom CSS for the text frame
      tags$style(HTML("  
        #text-frame {
          border: 2px solid darkred;       /* Border around the text */
          border-radius: 5px;              /* Rounded corners */
          padding: 15px;                   /* Spacing inside the frame */
          background-color: white;         /* Light background color */
          margin-top: 10px;                /* Space above the frame */
          margin-bottom: 10px;             /* Space below the frame */
          font-size: 18px;                 /* Adjust text size */
          color: #333;                     /* Text color */
        }

        /* Style the question label */
        .shiny-input-container > label {
          font-size: 16px;        /* Larger font size for the question label */
          font-weight: bold;      /* Bold text */
          margin-bottom: 10px;    /* Space below the question label */
        }

        /* Style the radio button texts */
        .shiny-options-group {
          font-size: 16px;        /* Slightly smaller font size */
          font-style: italic;     /* Make radio button texts italic */
        }
      "))
    )
  )
)

server <- function(input, output, session) {
  # Load the text data from a local file only once at the start
  coder <- 2
  textfile <- paste0("coder", coder, "texts.csv")
  text_data <- read.csv(textfile, stringsAsFactors = FALSE)
  
  # Add columns for each classification question if not already present
  if (!"classification1" %in% colnames(text_data)) text_data$classification1 <- NA
  if (!"classification2" %in% colnames(text_data)) text_data$classification2 <- NA
  if (!"classification3" %in% colnames(text_data)) text_data$classification3 <- NA
  
  # Reactive value to keep track of the current text index
  rv <- reactiveValues(index = 1, data = text_data)
  
  # Function to skip already classified rows
  skip_to_next_unclassified <- function() {
    while (rv$index <= nrow(rv$data) &&
           !is.na(rv$data$classification1[rv$index]) &&
           !is.na(rv$data$classification2[rv$index]) &&
           !is.na(rv$data$classification3[rv$index])) {
      rv$index <- rv$index + 1
    }
  }
  
  # Skip already classified rows on app initialization
  observeEvent(rv$index, {
    skip_to_next_unclassified()
  }, ignoreInit = FALSE)
  
  # Display the current text bit
  output$current_text <- renderUI({
    if (rv$index <= nrow(rv$data)) {
      HTML(rv$data$text[rv$index])
    } else {
      HTML("<b>All classifications completed. Thank you very much!</b> <br>You can close the app now.")
    }
  })
  
  # Display progress dynamically
  output$status <- renderText({
    paste0("Your task progress: ", sum(!is.na(rv$data$classification1) &
                                         !is.na(rv$data$classification2) &
                                         !is.na(rv$data$classification3)), 
           "/", nrow(rv$data), " (", 
           round((sum(!is.na(rv$data$classification1) &
                        !is.na(rv$data$classification2) &
                        !is.na(rv$data$classification3)) / nrow(rv$data)) * 100, 0), "%)")
  })
  
  # Save classification and move to the next text when 'Next' is clicked
  observeEvent(input$next_btn, {
    if (rv$index <= nrow(rv$data)) {
      if (!is.null(input$classification1) && !is.null(input$classification2) && !is.null(input$classification3)) {
        # Save the classification directly in the in-memory data object
        rv$data$classification1[rv$index] <- input$classification1
        rv$data$classification2[rv$index] <- input$classification2
        rv$data$classification3[rv$index] <- input$classification3
        
        # Write the updated data to the file immediately
        write.csv(rv$data, file = textfile, row.names = FALSE)
        
        # Reset the selection
        updateRadioButtons(session, "classification1", selected = character(0))
        updateRadioButtons(session, "classification2", selected = character(0))
        updateRadioButtons(session, "classification3", selected = character(0))
        
        # Move to the next text
        rv$index <- rv$index + 1
        
        # Skip already classified rows
        skip_to_next_unclassified()
      } else {
        showNotification("Please answer all questions before moving to the next text.", type = "error")
      }
    } else {
      showNotification("No more texts to classify. You're done! Thanks!", type = "warning")
    }
  })
}

shinyApp(ui, server)
