#
# This is a Shiny web application. You can run the application by clicking
# the "Run App" button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyRGL)
library(Rpolyhedra)
library(rgl)
library(futile.logger)

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
  
  futile.logger::flog.debug(paste("building polyhedra catalog for source",source.selected))
  futile.logger::flog.debug(polyhedron.selected[[source.selected]])
  #Selecting first polyhedra.element
  if (is.null(polyhedron.selected[[source.selected]])){
    polyhedron.selected[[source.selected]] <- polyhedra.list[1]
    
    #debug
    futile.logger::flog.debug(paste("selecting first polyhedron of source",source.selected,polyhedron.selected[[source.selected]]))
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
  futile.logger::flog.debug(paste("updateSelection",source,polyhedron,color))
  
  polyhedron.color.selected
  #assign("source.selected",source,envir = .GlobalEnv)
  source.selected <<- source
  polyhedron.selected[[source]] <- polyhedron
  #assign("polyhedron.selected",polyhedron.selected,envir = .GlobalEnv)
  #debug
  futile.logger::flog.debug(polyhedron.selected)
  
  polyhedron.color.selected[[source]] <- color
  #assign("polyhedron.color.selected",polyhedron.color.selected,envir = .GlobalEnv)
  polyhedron.selected <<- polyhedron.selected
  polyhedron.color.selected <<- polyhedron.color.selected
  
  polyhedron.selected 
}


updateSelection(source     = source.selected, 
                polyhedron = polyhedron.selected[[source.selected]],
                color      = polyhedron.color.selected[[source.selected]])

renderPolyhedron <- function(source, polyhedron.name, polyhedron.colors, show.axes = FALSE, file.name=FALSE){
  open3d(useNULL = TRUE)
  rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
  rgl.viewpoint(theta = 45,phi=10,zoom=0.8,fov=1)
  
  polyhedron <- getPolyhedron(source=source, polyhedron.name = polyhedron.name)
  if (!is.null(polyhedron)){
    shape.rgl <- polyhedron$getRGLModel()
    colors <- rainbow(length(shape.rgl))
    if(show.axes == TRUE) {
      title3d(main=polyhedron.name,"", "x", "y", "z",color="white",family = "bitmap")
      axes3d(color = "white", family = "bitmap")
    }
    shade3d(shape.rgl,color=polyhedron.colors)
    if(file.name!=FALSE) {
      rgl::writeSTL(file.name, ascii = TRUE)
    }
  }
}

updateInputs<-function(session, controls,values){
  i<-1
  if ("polyhedron_name" %in% controls){
    futile.logger::flog.debug("setting polyhedron")
    ret <- shiny::updateSelectInput("polyhedron_name",  
                                    choices = polyhedra.list, selected = values[i], session = session)
    i <- i +1
  }
  if ("polyhedron_color" %in% controls){
    futile.logger::flog.debug("setting color")
    futile.logger::flog.debug(values[i])
    futile.logger::flog.debug(available.polyhedra$color)
    ret <- shiny::updateSelectInput("polyhedron_color", 
                                    choices = available.polyhedra$color, selected = values[i], session = session)
    i <- i +1
  }
  ret
}


# Define UI for application that explore polyhedra database
ui <- shinyUI(fluidPage(
  theme = shinytheme("slate"),
   # Application title
  navbarPage("Rpolyhedra explorer"), 
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       shiny::selectInput("polyhedron_source", label = "Source", choices = sort(available.sources),selected = source.selected),
       #Evaluate encapsulate in a function after changing source
       shiny::selectInput("polyhedron_name", label = "Polyhedron", choices = polyhedra.list, selected = polyhedron.selected[[source.selected]]),
       shiny::selectInput("polyhedron_color", label = "Color", choices = available.polyhedra$color, selected = polyhedron.color.selected[[source.selected]]),
       shiny::checkboxInput(inputId="show_axes", label = "Show Axes"),
       shiny::downloadButton(outputId = "export_STL_btn", label = "STL"),
       img(src = "by-nc-sa.png", width="36%"),
       shiny::actionButton(inputId = "cc-by-nc-sa",
                           label = "License",
                           onclick = 'window.open(location.href="https://creativecommons.org/licenses/by-nc-sa/4.0/");',
                           ),
       shiny::actionLink(inputId = "Rpolyhedra",
                         label = "Rpolyhedra@github",
                         onclick = 'window.open(location.href="https://github.com/qbotics/Rpolyhedra");')
     ),
      mainPanel(
          rglwidgetOutput("wdg")
      )
   )
))

# Define server logic required to draw a polyhedron
server <- function(input, output, session) {
  
  options(rgl.useNULL = TRUE)
  
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  output$wdg <- renderRglwidget({
    futile.logger::flog.debug(paste("renderer polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "show_axis", input$show_axis))
    if(!is.null(input$polyhedron_source) && !is.null(input$polyhedron_name)){
      withProgress(message = "Processing...", value = 0, {
        renderPolyhedron(source= input$polyhedron_source, 
                         polyhedron.name = input$polyhedron_name, 
                         polyhedron.colors = input$polyhedron_color,
                         show.axes = input$show_axes)
        rglwidget()
      })
    }
  })
  
  ## general update of the page
  observe({
    #debug
    futile.logger::flog.debug(paste("observer polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "show_axis", input$show_axis))
    
    new.source     <- input$polyhedron_source
    new.polyhedron <- input$polyhedron_name
    new.color      <- input$polyhedron_color
    
    futile.logger::flog.debug(paste("new.polyhedron",new.polyhedron))
    futile.logger::flog.debug(polyhedron.selected)
    futile.logger::flog.debug(input$polyhedron.name)
    
    init <- FALSE
    
    if(source.selected != new.source | init) {
      source.selected <<- new.source
      buildPolyhedraCatalog()
      futile.logger::flog.debug(polyhedra.list[1:3])
      #Evaluate encapsulate in a function
      updateInputs(session, c("polyhedron_name","polyhedron_color"),
                   c(polyhedron.selected[[source.selected]],
                   polyhedron.color.selected[[source.selected]]
                   ))
      new.polyhedron <- polyhedron.selected[[new.source]]
    }
    
    futile.logger::flog.debug(paste("new.polyhedron after source change",new.polyhedron))
    
    #debug
    futile.logger::flog.debug (paste("polyhedron selected",polyhedron.selected[[new.source]]))
    futile.logger::flog.debug(paste(polyhedron.selected[[new.source]], new.polyhedron))
    
    if(polyhedron.selected[[new.source]] != new.polyhedron | init) {
      new.color <- available.polyhedra[available.polyhedra$name == polyhedron.selected[[new.source]],"color"]
      updateInputs(session, "polyhedron_color",
                   new.color)
    }
    updateSelection(source = new.source, 
                    polyhedron = new.polyhedron, 
                    color = new.color)
    
    })
  
  # Downloadable csv of selected dataset ----
  
  output$export_STL_btn <- downloadHandler(
    filename = function() {
      paste("Rpolyhedra", "_", input$polyhedron_source, "_", input$polyhedron_name, ".STL", sep = "")
    },
    content = function(file) {
      futile.logger::flog.info(paste("the STL file is ", file))
      renderPolyhedron(source= input$polyhedron_source, 
                       polyhedron.name = input$polyhedron_name, 
                       polyhedron.colors = input$polyhedron_color,
                       show.axes = input$show_axes, 
                       file.name = file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server, options=c("display.mode"))



