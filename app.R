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

available.sources <- sort(unique(getAvailablePolyhedra()$source))
source.selected <- "netlib"
polyhedron.selected <- list()
polyhedron.color.selected <- list()


buildPolyhedraCatalog <- function(){
  available.polyhedra <- getAvailablePolyhedra(sources = source.selected)
  available.polyhedra <- available.polyhedra[available.polyhedra$status=="scraped",]
  available.polyhedra$color <- rainbow(nrow(available.polyhedra))
  available.polyhedra$text <- paste(available.polyhedra$name,
                                    "V:",available.polyhedra$vertices, 
                                    "F:",available.polyhedra$faces)
  #assign("available.polyhedra",available.polyhedra,envir = .GlobalEnv)
  available.polyhedra <<- available.polyhedra
  
  polyhedra.list <- available.polyhedra$name
  names(polyhedra.list) <- available.polyhedra$text
  
  
  #assign("polyhedra.list", polyhedra.list, envir = .GlobalEnv)
  polyhedra.list <<- polyhedra.list
  
  #Selecting first polyhedra.element
  if (is.null(polyhedron.selected[[source.selected]])){
    polyhedron.selected[[source.selected]] <- polyhedra.list[1]
    polyhedron.color.selected[[source.selected]]<- available.polyhedra[1,]$color
    #assign("polyhedron.selected", polyhedron.selected, envir = .GlobalEnv)
    #assign("polyhedron.color.selected", polyhedron.color.selected, envir = .GlobalEnv)
    polyhedron.selected <<- polyhedron.selected
    polyhedron.color.selected <<- polyhedron.color.selected
  }
  
  available.polyhedra
}

buildPolyhedraCatalog()

updateSelection <- function(source, polyhedron, color){
  #debug
  print(paste("updateSelection",source,polyhedron,color))
  
  polyhedron.color.selected
  #assign("source.selected",source,envir = .GlobalEnv)
  source.selected <<- source
  polyhedron.selected[[source]] <- polyhedron
  #assign("polyhedron.selected",polyhedron.selected,envir = .GlobalEnv)
  #debug
  print(polyhedron.selected)
  
  polyhedron.color.selected[[source]] <- color
  #assign("polyhedron.color.selected",polyhedron.color.selected,envir = .GlobalEnv)
  polyhedron.selected <<- polyhedron.selected
  polyhedron.color.selected <<- polyhedron.color.selected
  
  polyhedron.selected 
}


updateSelection(source     = source.selected, 
                polyhedron = polyhedron.selected[[source.selected]],
                color      = polyhedron.color.selected[[source.selected]])

# Define UI for application that explore polyhedra database
ui <- shinyUI(fluidPage(
  theme = "polyhedra.css",
   # Application title
  titlePanel("Rpolyhedra explorer"), 
   # Sidebar with a slider input for number of bins 
   sidebarLayout( 
     sidebarPanel(
       shiny::selectInput("polyhedron_source", label = "Source", choices = sort(available.sources),selected = source.selected),
       shiny::selectInput("polyhedron_name", label = "Polyhedron", choices = polyhedra.list, selected = polyhedron.selected[[source.selected]]),
       shiny::selectInput("polyhedron_color", label = "color", choices = available.polyhedra$color, selected = polyhedron.color.selected[[source.selected]]),
       shiny::checkboxInput(inputId="show_axes", label = "Show Axes")
      ),
      # Show a plot of the generated distribution
      mainPanel(
          rglwidgetOutput("wdg")
      )
   )
))

renderPolyhedron <- function(source, polyhedron.name, polyhedron.colors, show.axes = FALSE){
  open3d(useNULL = TRUE)
  rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
  rgl.viewpoint(theta = 45,phi=10,zoom=0.8,fov=1)
  
  polyhedron <- getPolyhedron(source=source, polyhedron.name = polyhedron.name)
  if (!is.null(polyhedron)){
    pos3D <- rep(0,3)
    shape.rgl <- polyhedron$getRGLModel(size = 1, origin = pos3D)
    colors <- rainbow(length(shape.rgl))
    if(show.axes == TRUE) {
      title3d(main=polyhedron.name,"", "x", "y", "z",color="white",family = "bitmap")
      axes3d(color = "white", family = "bitmap")
    }
    shade3d(shape.rgl,color=polyhedron.colors)
  }

}


# Define server logic required to draw a polyhedron
server <- function(input, output, session) {
  
  options(rgl.useNULL = TRUE)
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  output$wdg <- renderRglwidget({
    print(paste("renderer polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "show_axis", input$show_axis))
    if(!is.null(input$polyhedron_source) && !is.null(input$polyhedron_name)){
      withProgress(message = 'Processing...', value = 0, {
        renderPolyhedron(source= input$polyhedron_source, 
                         polyhedron.name = input$polyhedron_name, 
                         polyhedron.colors = input$polyhedron_color,
                         show.axes = input$show_axes)
        rglwidget()
      })
    }
  })
  
  observe({
    #debug
    print(paste("observer polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "show_axis", input$show_axis))
    
    new.source <- input$polyhedron_source
    init <- FALSE
    if (is.null(polyhedron.selected[[new.source]])){
      print("initializing selection in server")
      updateSelection(source = input$polyhedron_source, 
                      polyhedron = input$polyhedron_name, 
                      color = input$polyhedron_color)
      init <- TRUE
      #debug
      print("polyhedron selected in server")
      print(polyhedron.selected)
    }
      
    if(source.selected != new.source | init) {
      buildPolyhedraCatalog()
      shiny::updateSelectInput(session=session, inputId = "polyhedron_name", choices = polyhedra.list, selected = polyhedron.selected[[new.source]])
    }
    #debug
    print (paste("polyhedron selected",polyhedron.selected[[new.source]]))
    print(paste(polyhedron.selected[[new.source]], input$polyhedron_name))
    
    if(polyhedron.selected[[new.source]] != input$polyhedron_name | init) {
      #debug
      print("changing color")
      print(polyhedron.selected[[new.source]])
      print(available.polyhedra[available.polyhedra$name == polyhedron.selected[[new.source]],])
      print(available.polyhedra$name[1:5])
      
      current.color <- available.polyhedra[available.polyhedra$name == polyhedron.selected[[new.source]],"color"]
      print(current.color)
      shiny::updateSelectInput(session=session, inputId = "polyhedron_color", choices = available.polyhedra$color, selected = current.color)
    }
    updateSelection(source = input$polyhedron_source, 
                    polyhedron = input$polyhedron_name, 
                    color = input$polyhedron_color)
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

