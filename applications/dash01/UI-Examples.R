# A box with label, sidebar, dropdown menu 

library(shiny) 
library(shinydashboard)
library(shinydashboardPlus)

shinyApp(
  ui = dashboardPage(dashboardHeader(), dashboardSidebar(), 
   dashboardBody(
    box(
      title = "Closable Box with dropdown",
      closable = TRUE,
      width = 12,
      
      status = "warning",
      
      solidHeader = FALSE,
      collapsible = TRUE,
      label = boxLabel(
        text = 1,
        
        status = "danger",
        
        style = "circle"
        
      ),
      dropdownMenu = boxDropdown(
        boxDropdownItem("Link to google", href = "http://www.google.com"),
        boxDropdownItem("item 2", href = "#"),
        dropdownDivider(),
        boxDropdownItem("item 3", href = "#", icon = icon("th"))
      ),
      sidebar = boxSidebar(
        startOpen = TRUE,
        
        id = "mycardsidebar",
        
        sliderInput(
          "obs",
          "Number of observations:",
          
          min = 0,
          
          max = 1000,
          
          value = 500
          
        )
      ),
      plotOutput("distPlot")
    )
    
  )),
  server = function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }
  
) 


# Box with descriptionBlock 


shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      box(
        solidHeader = FALSE,
        
        title = "Status summary",
        
        background = NULL,
        width = 4,
        
        status = "danger",
        
        footer = fluidRow(
          column(
            width = 6,
            descriptionBlock(
              number = "17%",
              
              numberColor = "green",
              numberIcon = icon("caret-up"),
              
              header = "$35,210.43",
              
              text = "TOTAL REVENUE",
              
              rightBorder = TRUE,
              marginBottom = FALSE
              
            )
          ),
          column(
            width = 6,
            descriptionBlock(
              number = "18%",
              
              numberColor = "red",
              numberIcon = icon("caret-down"),
              
              header = "1200",
              text = "GOAL COMPLETION",
              
              rightBorder = FALSE,
              marginBottom = FALSE
              
            )
            
          )
          
        ) ) ),
    title = "Description Blocks" ),
  server = function(input, output) {} ) 

if (interactive()) {
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(fresh)
  
  shinyApp(
    ui = dashboardPage(
      freshTheme = create_theme(
        adminlte_color(
          light_blue = "#55e7ff",
          blue = "#2011a2",
          navy = "#201148",
          red = "#ff34b3"
        ),
        adminlte_sidebar(
          dark_bg = "#D8DEE9",
          dark_hover_bg = "#81A1C1",
          dark_color = "#2E3440"
        ),
        adminlte_global(
          content_bg = "#FFF",
          box_bg = "#D8DEE9", 
          info_box_bg = "#D8DEE9"
        )
      ),
      options = list(sidebarExpandOnHover = TRUE),
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      body = dashboardBody(
        box(background = "red"),
        box(background = "blue"),
        box(background = "navy")
      ),
      controlbar = dashboardControlbar(),
      title = "DashboardPage"
    ),
    server = function(input, output) { }
  )
}



# Box with attachmentBlock
if (interactive()) {
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        box(
          title = "Attachment example",
          attachmentBlock(
            image = "https://adminlte.io/themes/AdminLTE/dist/img/photo1.png",
            title = "Test",
            href = "https://google.com",
            "This is the content"
          )
        )
      ),
      title = "AttachmentBlock"
    ),
    server = function(input, output) { }
  )
}

shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      box(
        title = "Product List",
        status = "primary",
        productList(
          productListItem(
            image = "https://www.pngmart.com/files/1/Haier-TV-PNG.png", 
            title = "Samsung TV", 
            subtitle = "$1800", 
            color = "yellow",
            href = "https://google.com",
            "This is an amazing TV, but I don't like TV!"
          ),
          productListItem(
            image = "https://upload.wikimedia.org/wikipedia/commons/7/77/IMac_Pro.svg", 
            title = "Imac 27", 
            subtitle = "$4999", 
            color = "red",
            href = "https://google.com",
            "This is were I spend most of my time!"
          )
        )
      )
    ),
    title = "Product List"
  ),
  server = function(input, output) { }
)



  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        accordion(
          id = "accordion1",
          accordionItem(
            title = "Accordion 1 Item 1",
            status = "danger",
            collapsed = TRUE,
            "This is some text!"
          ),
          accordionItem(
            title = "Accordion 1 Item 2",
            status = "warning",
            collapsed = FALSE,
            "This is some text!"
          )
        ),
        accordion(
          id = "accordion2",
          accordionItem(
            title = "Accordion 2 Item 1",
            status = "info",
            collapsed = TRUE,
            "This is some text!"
          ),
          accordionItem(
            title = "Accordion 2 Item 2",
            status = "success",
            collapsed = FALSE,
            "This is some text!"
          )
        )
      ),
      title = "Accordion"
    ),
    server = function(input, output) { }
  )



# Update accordion

  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        radioButtons("controller", "Controller", choices = c(1, 2)),
        br(),
        accordion(
          id = "accordion1",
          accordionItem(
            title = "Accordion 1 Item 1",
            status = "danger",
            collapsed = TRUE,
            "This is some text!"
          ),
          accordionItem(
            title = "Accordion 1 Item 2",
            status = "warning",
            collapsed = TRUE,
            "This is some text!"
          )
        )
      ),
      title = "Update Accordion"
    ),
    server = function(input, output, session) {
      observeEvent(input$controller, {
        updateAccordion(id = "accordion1", selected = input$controller)
      })
      observe(print(input$accordion1))
      observeEvent(input$accordion1, {
        showNotification(sprintf("You selected accordion N° %s", input$accordion1), type = "message")
      })
    }
  )

