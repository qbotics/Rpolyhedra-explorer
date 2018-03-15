#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyRGL)
library(Rpolyhedra)
library(rgl)

open3d(useNULL = TRUE)
scene <- scene3d()
rgl.close()
current.library <- ""
current.polyhedron <- ""
polyhedra <- getAvailablePolyhedra()
palette_choices <- list("rainbow")
source.selected <- "netlib"


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  theme = "polyhedra.css",
   # Application title
  titlePanel("Rpolyhedra explorer"), 
   # Sidebar with a slider input for number of bins 
   sidebarLayout( 
     sidebarPanel(
       shiny::selectInput("polyhedron_source", label = "Source", choices = sort(unique(polyhedra$source))),
       shiny::selectInput("polyhedron_name", label = "Polyhedron", choices = polyhedra$polyhedron.name, selected = current.polyhedron),
       shiny::checkboxInput(inputId="show_axes", label = "Show Axes")
      ),
      # Show a plot of the generated distribution
      mainPanel(
          rglwidgetOutput("wdg")
      )
   )
))

renderPolyhedron <- function(source, polyhedron.name, show.axes){
  open3d(useNULL = TRUE)
  rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
  rgl.viewpoint(theta = 45,phi=10,zoom=0.8,fov=1)
  
  polyhedron <- getPolyhedron(source=source, polyhedron.name = polyhedron.name)
  pos3D <- rep(0,3)
  shape.rgl <- polyhedron$getRGLModel(size = 1, origin = pos3D)
  colors <- rainbow(length(shape.rgl))
  if(show.axes == TRUE) {
    axes3d()
  }
  shade3d(shape.rgl,color=rainbow(ncol(shape.rgl$it)))

}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  options(rgl.useNULL = TRUE)
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  # set source.selected 
  polyhedra.source <- getAvailablePolyhedra(source = source.selected)
  
  output$wdg <- renderRglwidget({
    print(paste("renderer polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "show_axis", input$show_axis))
    if(!is.null(input$polyhedron_source) && !is.null(input$polyhedron_name)){
      withProgress(message = 'Processing...', value = 0, {
        renderPolyhedron(source= input$polyhedron_source, polyhedron.name = input$polyhedron_name, show.axes = input$show_axes)
        rglwidget()
      })
    }
  })
  
  observe({
    print(paste("observer polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "show_axis", input$show_axis))
    if(current.library != input$polyhedron_source) {
      shiny::updateSelectInput(session=session, inputId = "polyhedron_name", choices=getAvailablePolyhedra(input$polyhedron_source)$polyhedron.name)
      current.library <<- input$polyhedron_source
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

