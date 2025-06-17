library(shiny)

# Shiny App
ui <- fluidPage(
  titlePanel(HTML("<br><em>Coding task:</em> Statements about the EU</em><br><br>")),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p(HTML("<b><i>You are asked to read the individual text in the red frame, to provide your assessment of the text, to then continue to the next example.</i></b>")),
      p("The texts are very short excerpts from international speeches of diplomatic representatives or national leaders."),
      p(HTML("We want to learn whether the particular text does or does not suggest that the <b><em>European Union</em> (incl. the EU as a whole, its predecessors such as the European Communities, or its individual institutions)</b> has some kind of <b><em>capability to act</em></b> on its own.")),
      p(HTML("In other words, <b><em>we ask you to asses whether the individual text - on its own - implies that the EU can, does, or should do something.</em></b>")),
      p(HTML("There are no right or wrong answers - only what you personally read from the individual text snippet matters here.")),
      HTML("<br>"),
      verbatimTextOutput("status"),  # Dynamic progress display
      HTML("<br>"),
      p(HTML("Once you click on the continue button, your assessment is saved. <b>You can close and later reload the app whenever you need a break.</b>")),
      p("Many thanks for your help!"),
      p(HTML("I am happy to tell you more about the research context once the coding is done. If other questions occur, please do not hesitate to contact me via <a href=\"mailto:rauh@wzb.eu\">Email</a>."))
    ),
    
    mainPanel(
      HTML("<br>"),
      h3("The text and your personal assessment"),
      div(
        id = "text-frame",
        uiOutput("current_text")
      ),
      # h3("Your assessment of the text"),
      HTML("<br>"),
      
      # Classification radio buttons and next button placed together
      div(
        radioButtons("classification", HTML("Does this particular statement <i>in itself</i> clearly imply that the EU can, does, or should act on its own in principle?"),
                     choices = c("Clearly not", "Probably not",  "Probably yes", "Clearly yes"), 
                     inline = TRUE,
                     selected = character(0)), # Ensures no pre-selected option
        HTML("<br>"),
        actionButton("next_btn", "Continue", style = "margin-top: 10px; background-color: #4CAF50; color: white;")
      ),
      
      # Add custom CSS for the text frame
      tags$style(HTML("
        #text-frame {
          border: 2px solid darkred;       /* Border around the text */
          border-radius: 5px;              /* Rounded corners */
          padding: 15px;                   /* Spacing inside the frame */
          background-color: white;         /* Light background color */
          margin-top: 20px;                /* Space above the frame */
          margin-bottom: 20px;             /* Space below the frame */
          font-size: 20px;                 /* Adjust text size */
          color: #333;                     /* Text color */
        }
        
        /* Style the question label */
        .shiny-input-container > label {
          font-size: 18px;        /* Larger font size for the question label */
          color: darkred;         /* Dark red color for the question label */
          font-weight: bold;      /* Bold text */
          margin-bottom: 10px;    /* Space below the question label */
          font-style: normal;     /* Ensure question label is not italic */
        }
        
        /* Style the radio button texts */
        .shiny-options-group {
          font-size: 18px;        /* Slightly smaller font size */
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
  
  
  # Reactive value to keep track of the current text index
  rv <- reactiveValues(index = 1, data = text_data)  # Store the data in memory
  
  # Function to skip already classified rows
  skip_to_next_unclassified <- function() {
    while (rv$index <= nrow(rv$data) && !is.na(rv$data$label[rv$index]) && rv$data$label[rv$index] != "") {
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
      # Display the current text
      HTML(rv$data$text[rv$index])
    } else {
      # Completion message
      HTML("<b>All classifications completed. Thank you very much!</b> <br>You can close the app now.")
    }
  })
  
  # Display progress dynamically
  output$status <- renderText({
    paste0("Your task progress: ", sum(!is.na(rv$data$label) & rv$data$label != ""), 
           "/", nrow(rv$data), " (", 
           round((sum(!is.na(rv$data$label) & rv$data$label != "") / nrow(rv$data)) * 100, 0), "%)")
  })
  
  # Save classification and move to the next text when 'Next' is clicked
  observeEvent(input$next_btn, {
    if (rv$index <= nrow(rv$data)) {
      if (!is.null(input$classification) && input$classification != "") {
        # Save the classification directly in the in-memory data object
        rv$data$label[rv$index] <- input$classification
        
        # Write the updated data to the file immediately
        write.csv(rv$data, file = textfile, row.names = FALSE)
        
        # Reset the selection
        updateRadioButtons(session, "classification", selected = character(0))
        
        # Move to the next text
        rv$index <- rv$index + 1
        
        # Skip already classified rows
        skip_to_next_unclassified()
      } else {
        showNotification("Please choose your assessment before moving to the next text.", type = "error")
      }
    } else {
      showNotification("No more texts to classify. You're done! Thanks!", type = "warning")
    }
  })
}

shinyApp(ui, server)
