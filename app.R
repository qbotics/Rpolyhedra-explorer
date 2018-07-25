#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinythemes)
library(shinyRGL)
library(Rpolyhedra)
library(rgl)
library(futile.logger)

#this has to remain in the global session. 
Rpolyhedra:::setDataDirEnvironment("HOME")
data.dir <- Rpolyhedra:::getUserSpace()
if(!dir.exists(data.dir)) {
  dir.create(data.dir, recursive=TRUE, showWarnings = FALSE)
}
Rpolyhedra::downloadRPolyhedraSupportingFiles()
Rpolyhedra:::updatePolyhedraDatabase()


# Define UI for application that explore polyhedra database
ui <- function(request) {
  shiny::bootstrapPage(
    theme = shinytheme("slate"),
    # Application title
    shiny::navbarPage("Rpolyhedra explorer"), 
    # Sidebar with a slider input for number of bins 
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("polyhedron_source", label = "Source", ""),
        #shiny::uiOutput("ui.polyhedron.source"),
        #Evaluate encapsulate in a function after changing source
        shiny::selectInput("polyhedron_name", label = "Polyhedron", ""),
        shiny::selectInput("polyhedron_color", label = "Color", ""),
        shiny::checkboxInput(inputId="show_axes", label = "Show Axes"),
        shiny::downloadButton(outputId = "export_STL_btn", label = "STL"),
        #shinyURL.ui(display=FALSE),
        shiny::img(src = "by-nc-sa.png", width="36%"),
        shiny::actionButton(inputId = "cc-by-nc-sa",
                            label = "LICENSE",
                            onclick = 'window.open(location.href="https://creativecommons.org/licenses/by-nc-sa/4.0/");',
        ),
        shiny::actionLink(inputId = "Rpolyhedra",
                          label = "Rpolyhedra@github",
                          onclick = 'window.open(location.href="https://github.com/qbotics/Rpolyhedra");')
        
      ),
      shiny::mainPanel(
        rglwidgetOutput("wdg")
      )
    )
  )
}

# Define server logic required to draw a polyhedron
server <- function(input, output, session) {
  setBookmarkExclude(c("cc-by-nc-sa", "Rpolyhedra"))
  options(rgl.useNULL = TRUE)
  options(rgl.inShiny = TRUE)
  #open3d(useNULL = TRUE)
  #scene <- scene3d()
  bookmarked <- FALSE
  available.sources <- sort(unique(getAvailablePolyhedra()$source))
  
  
  
  #callback for app exit
  onStop(function() {
    options(rgl.inShiny = FALSE)
    rgl.close()
  })
  
  
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  showParameters <- function() {
    modalDialog(
      span(paste("polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "polyhedron_color", 
                 input$polyhedron_color, "show_axis", input$show_axis)),
      
      footer = tagList(
        modalButton("Cancel")
      )
    )
  }
  
  shiny::observeEvent(c(input$polyhedron_source, input$polyhedron_name,  input$polyhedron_color), {
    futile.logger::flog.debug(paste("@@@@@ We are in Observe: polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name,
                                    "polyhedron_color", input$polyhedron_color))
    query.string <- getQueryString()
    if(input$polyhedron_source != "" && !is.null(input$polyhedron_source)) {
      polyhedron.source <- input$polyhedron_source 
    } else if(!is.null(query.string$polyhedron_source)){ 
      polyhedron.source <- substring(substring(query.string$polyhedron_source, 2), 1, nchar(query.string$polyhedron_source) - 2)
    }else {
      polyhedron.source = available.sources[2]
    }
    
    
    if(input$polyhedron_name != "" && !is.null(input$polyhedron_name)) {
      polyhedron.name <- input$polyhedron_name
    } else if(!is.null(query.string$polyhedron_name)){ 
      polyhedron.name <- substring(substring(query.string$polyhedron_name, 2), 1, nchar(query.string$polyhedron_name) - 2) 
    } else{
      polyhedron.name <- NULL
    }
    
    if(input$polyhedron_color != "" && !is.null(input$polyhedron_color)) {
      polyhedron.color <- input$polyhedron_color 
    } else  if(!is.null(query.string$polyhedron_color)) { 
      polyhedron.color <- substring(substring(query.string$polyhedron_color, 2), 1, nchar(query.string$polyhedron_color) - 2)
    } else {
      polyhedron.color <- NULL
    }
    
    updateSelectInput(session, "polyhedron_source",
                      choices = available.sources, selected=polyhedron.source)
    
    available.polyhedra <- getAvailablePolyhedra(sources = polyhedron.source)
    available.polyhedra <- available.polyhedra[available.polyhedra$status=="scraped",]
    available.polyhedra$color <- rainbow(nrow(available.polyhedra))
    available.polyhedra$text <- paste(available.polyhedra$name,
                                      "V:",available.polyhedra$vertices,
                                      "F:",available.polyhedra$faces)
    polyhedra.list <- available.polyhedra$name
    names(polyhedra.list) <- available.polyhedra$text
    
    if(is.null(polyhedron.name) || !polyhedron.name %in% polyhedra.list){
      polyhedron.name <- polyhedra.list[1]
    }
    updateSelectInput(session, "polyhedron_name",
                      choices = polyhedra.list, selected=polyhedron.name)
    
    if(is.null(polyhedron.color) || !polyhedron.color %in% available.polyhedra$color ) {
      polyhedron.color <- available.polyhedra[available.polyhedra$name==polyhedron.name,]$color
    }
    updateSelectInput(session, "polyhedron_color",
                      choices = available.polyhedra$color, selected=polyhedron.color)
    
  })
  
  renderPolyhedron <- function(polyhedron.source, polyhedron.name, polyhedron.colors, show.axes = FALSE, file.name=FALSE){
    futile.logger::flog.debug(paste("%%%%% We are in renderer polyhedron.source", polyhedron.source, "polyhedron.name", polyhedron.name, "polyhedron.colors", polyhedron.colors))
    open3d(useNULL = TRUE)
    rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
    rgl.viewpoint(theta = 45,phi=10,zoom=0.8,fov=1)
    ###browser()
    polyhedron <- getPolyhedron(source=polyhedron.source, polyhedron.name = polyhedron.name)
    if (!is.null(polyhedron)){
      shape.rgl <- polyhedron$getRGLModel()
      colors <- rainbow(length(shape.rgl))
      if(show.axes == TRUE) {
        title3d(main=polyhedron.name,"", "x", "y", "z",color="white",family = "bitmap")
        axes3d(color = "white", family = "bitmap")
      }
      #debug
      futile.logger::flog.debug(paste("polyhedron.colors", polyhedron.colors))
      if(is.null(polyhedron.colors) || polyhedron.colors == "")
      {
        available.polyhedra <- getAvailablePolyhedra(sources = polyhedron.source)
        available.polyhedra <- available.polyhedra[available.polyhedra$status=="scraped",]
        available.polyhedra$color <- rainbow(nrow(available.polyhedra))
        polyhedron.colors <- available.polyhedra[1,]$color
      }
      shade3d(shape.rgl,color=polyhedron.colors)
      if(file.name!=FALSE) {
        rgl::writeSTL(file.name, ascii = TRUE)
      }
    }
    reactiveValuesToList(input)
    session$doBookmark()
    gc(full=TRUE, reset=TRUE)
  }
  
  output$wdg <- renderRglwidget({
    futile.logger::flog.debug(paste("we are in renderRglwidget polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "polyhedron_color", 
                                    input$polyhedron_color, "show_axis", input$show_axis))
    if(!is.null(input$polyhedron_source) && !is.null(input$polyhedron_name)){
      withProgress(message = "Processing...", value = 0, {
        renderPolyhedron(polyhedron.source = input$polyhedron_source, 
                         polyhedron.name = input$polyhedron_name, 
                         polyhedron.colors = input$polyhedron_color,
                         show.axes = input$show_axes)
        gcc <- gc(full=TRUE, reset=TRUE)
        futile.logger::flog.info(paste("Memory used right now", gcc[13]+gcc[14]))
        rglwidget()
      })
    }
  })
  
  
  
}

enableBookmarking(store = "url")
# Run the application 
shinyApp(ui = ui, server = server, options=c("display.mode"))

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinythemes)
library(shinyRGL)
library(Rpolyhedra)
library(rgl)
library(futile.logger)

#this has to remain in the global session. 
Rpolyhedra:::setDataDirEnvironment("HOME")
data.dir <- Rpolyhedra:::getUserSpace()
if(!dir.exists(data.dir)) {
  dir.create(data.dir, recursive=TRUE, showWarnings = FALSE)
}
Rpolyhedra::downloadRPolyhedraSupportingFiles()
Rpolyhedra:::updatePolyhedraDatabase()


# Define UI for application that explore polyhedra database
ui <- function(request) {
  shiny::bootstrapPage(
    theme = shinytheme("slate"),
    # Application title
    shiny::navbarPage("Rpolyhedra explorer"), 
    # Sidebar with a slider input for number of bins 
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("polyhedron_source", label = "Source", ""),
        #shiny::uiOutput("ui.polyhedron.source"),
        #Evaluate encapsulate in a function after changing source
        shiny::selectInput("polyhedron_name", label = "Polyhedron", ""),
        shiny::selectInput("polyhedron_color", label = "Color", ""),
        shiny::checkboxInput(inputId="show_axes", label = "Show Axes"),
        shiny::downloadButton(outputId = "export_STL_btn", label = "STL"),
        #shinyURL.ui(display=FALSE),
        shiny::img(src = "by-nc-sa.png", width="36%"),
        shiny::actionButton(inputId = "cc-by-nc-sa",
                            label = "LICENSE",
                            onclick = 'window.open(location.href="https://creativecommons.org/licenses/by-nc-sa/4.0/");',
        ),
        shiny::actionLink(inputId = "Rpolyhedra",
                          label = "Rpolyhedra@github",
                          onclick = 'window.open(location.href="https://github.com/qbotics/Rpolyhedra");')
        
      ),
      shiny::mainPanel(
        rglwidgetOutput("wdg")
      )
    )
  )
}

# Define server logic required to draw a polyhedron
server <- function(input, output, session) {
  setBookmarkExclude(c("cc-by-nc-sa", "Rpolyhedra"))
  options(rgl.useNULL = TRUE)
  options(rgl.inShiny = TRUE)
  #open3d(useNULL = TRUE)
  #scene <- scene3d()
  bookmarked <- FALSE
  available.sources <- sort(unique(getAvailablePolyhedra()$source))
  
  
  
  #callback for app exit
  onStop(function() {
    options(rgl.inShiny = FALSE)
    rgl.close()
  })
  
  
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  showParameters <- function() {
    modalDialog(
      span(paste("polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "polyhedron_color", 
                 input$polyhedron_color, "show_axis", input$show_axis)),
      
      footer = tagList(
        modalButton("Cancel")
      )
    )
  }
  
  shiny::observeEvent(c(input$polyhedron_source, input$polyhedron_name,  input$polyhedron_color), {
    futile.logger::flog.debug(paste("@@@@@ We are in Observe: polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name,
                                    "polyhedron_color", input$polyhedron_color))
    query.string <- getQueryString()
    if(input$polyhedron_source != "" && !is.null(input$polyhedron_source)) {
      polyhedron.source <- input$polyhedron_source 
    } else if(!is.null(query.string$polyhedron_source)){ 
      polyhedron.source <- substring(substring(query.string$polyhedron_source, 2), 1, nchar(query.string$polyhedron_source) - 2)
    }else {
      polyhedron.source = available.sources[2]
    }
    
    
    if(input$polyhedron_name != "" && !is.null(input$polyhedron_name)) {
      polyhedron.name <- input$polyhedron_name
    } else if(!is.null(query.string$polyhedron_name)){ 
      polyhedron.name <- substring(substring(query.string$polyhedron_name, 2), 1, nchar(query.string$polyhedron_name) - 2) 
    } else{
      polyhedron.name <- NULL
    }
    
    if(input$polyhedron_color != "" && !is.null(input$polyhedron_color)) {
      polyhedron.color <- input$polyhedron_color 
    } else  if(!is.null(query.string$polyhedron_color)) { 
      polyhedron.color <- substring(substring(query.string$polyhedron_color, 2), 1, nchar(query.string$polyhedron_color) - 2)
    } else {
      polyhedron.color <- NULL
    }
    
    updateSelectInput(session, "polyhedron_source",
                      choices = available.sources, selected=polyhedron.source)
    
    available.polyhedra <- getAvailablePolyhedra(sources = polyhedron.source)
    available.polyhedra <- available.polyhedra[available.polyhedra$status=="scraped",]
    available.polyhedra$color <- rainbow(nrow(available.polyhedra))
    available.polyhedra$text <- paste(available.polyhedra$name,
                                      "V:",available.polyhedra$vertices,
                                      "F:",available.polyhedra$faces)
    polyhedra.list <- available.polyhedra$name
    names(polyhedra.list) <- available.polyhedra$text
    
    if(is.null(polyhedron.name) || !polyhedron.name %in% polyhedra.list){
      polyhedron.name <- polyhedra.list[1]
    }
    updateSelectInput(session, "polyhedron_name",
                      choices = polyhedra.list, selected=polyhedron.name)
    
    if(is.null(polyhedron.color) || !polyhedron.color %in% available.polyhedra$color ) {
      polyhedron.color <- available.polyhedra[available.polyhedra$name==polyhedron.name,]$color
    }
    updateSelectInput(session, "polyhedron_color",
                      choices = available.polyhedra$color, selected=polyhedron.color)
    
  })
  
  renderPolyhedron <- function(polyhedron.source, polyhedron.name, polyhedron.colors, show.axes = FALSE, file.name=FALSE){
    futile.logger::flog.debug(paste("%%%%% We are in renderer polyhedron.source", polyhedron.source, "polyhedron.name", polyhedron.name, "polyhedron.colors", polyhedron.colors))
    open3d(useNULL = TRUE)
    rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
    rgl.viewpoint(theta = 45,phi=10,zoom=0.8,fov=1)
    ###browser()
    polyhedron <- getPolyhedron(source=polyhedron.source, polyhedron.name = polyhedron.name)
    if (!is.null(polyhedron)){
      shape.rgl <- polyhedron$getRGLModel()
      colors <- rainbow(length(shape.rgl))
      if(show.axes == TRUE) {
        title3d(main=polyhedron.name,"", "x", "y", "z",color="white",family = "bitmap")
        axes3d(color = "white", family = "bitmap")
      }
      #debug
      futile.logger::flog.debug(paste("polyhedron.colors", polyhedron.colors))
      if(is.null(polyhedron.colors) || polyhedron.colors == "")
      {
        available.polyhedra <- getAvailablePolyhedra(sources = polyhedron.source)
        available.polyhedra <- available.polyhedra[available.polyhedra$status=="scraped",]
        available.polyhedra$color <- rainbow(nrow(available.polyhedra))
        polyhedron.colors <- available.polyhedra[1,]$color
      }
      shade3d(shape.rgl,color=polyhedron.colors)
      if(file.name!=FALSE) {
        rgl::writeSTL(file.name, ascii = TRUE)
      }
    }
    reactiveValuesToList(input)
    session$doBookmark()
    gc(full=TRUE, reset=TRUE)
  }
  
  output$wdg <- renderRglwidget({
    futile.logger::flog.debug(paste("we are in renderRglwidget polyhedron_source", input$polyhedron_source, "polyhedron_name", input$polyhedron_name, "polyhedron_color", 
                                    input$polyhedron_color, "show_axis", input$show_axis))
    if(!is.null(input$polyhedron_source) && !is.null(input$polyhedron_name)){
      withProgress(message = "Processing...", value = 0, {
        renderPolyhedron(polyhedron.source = input$polyhedron_source, 
                         polyhedron.name = input$polyhedron_name, 
                         polyhedron.colors = input$polyhedron_color,
                         show.axes = input$show_axes)
        gcc <- gc(full=TRUE, reset=TRUE)
        futile.logger::flog.info(paste("Memory used right now", gcc[13]+gcc[14]))
        rglwidget()
      })
    }
  })
  
  
  
}

enableBookmarking(store = "url")
# Run the application 
shinyApp(ui = ui, server = server, options=c("display.mode"))

