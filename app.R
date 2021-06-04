#
# Audit-explorer  Copyright 2017 Steve Grubb <sgrubb@redhat.com>
#
# Version 0.8
#
# This program is released under the GNU Affero General Public License v3.0
#

library(shiny)
library(shinyjs)
library(plyr)
library(dplyr)
library(networkD3)
library(rpivotTable)
library(data.tree)
library(ggplot2)


# Do common housework here
mainDir <- "~/R"
subDir  <- "audit-data"
logFile <- "audit.log"
csvFile <- "audit.csv"
txtFile <- "audit.txt"
rptFile <- "audit.rpt"
fnames  <- as.character(" ")
fnames2 <- as.character(" ")
fnames3 <- as.character(" ")
fnames4 <- as.character(" ")
fnames5 <- as.character(" ")
fnames6 <- as.character(" ")
soperands <- as.character(c(" ", "==", "!=", ">", ">=", "<", "<="))
operand1  <- as.character("")
operation <- as.character("")
operand2  <- as.character("")
ourColors <<- c("red", "blue", "green", "cyan", "yellow", "orange", "black", "gray", "purple" )
sevents <<- reactive({ 0 })

# Create our directory if it doesn't exist
if (!dir.exists(file.path(mainDir, subDir))) {
  dir.create(file.path(mainDir, subDir))
}


# Define UI for application that draws a histogram
ui <- tagList(
    useShinyjs(),
    navbarPage("Audit Explorer", id = "navbar",
        tabPanel(title = "Source", value = "tab1",
                 radioButtons("sourcerb", "Events Source:", c("Logs" = "logs", "Standard CSV" = "csv", "Upload CSV File" = "file")),
                 conditionalPanel(condition = "input.sourcerb == 'file'",
                                  fileInput("file1", "Choose Audit CSV File", accept = c("text/csv",
                                                                                         "text/comma-separated-values",
                                                                                         ".csv")
                                  )
                 ),
                 conditionalPanel(condition = "input.sourcerb == 'logs'",
                                  selectInput("start", "Start time", c("Recent" = "recent",
                                                                       "Since Boot" = "boot",
                                                                       "Today" = "today",
                                                                       "Yesterday" = "yesterday",
                                                                       "This Week" = "this-week",
                                                                       "Week Ago" = "week-ago",
                                                                       "This Month" = "this-month",
                                                                       "This Year" = "this-year",
                                                                       "Custom" = "custom"),
                                              selected = "today"
                                  ),
                                  conditionalPanel(condition = "input.start == 'custom'",
                                                   dateInput("startDate", "Start Date"),
                                                   textInput("startText", "Start Clock Time")
                                  ),
                                  selectInput("end", "End time", c("Now" = "now",
                                                                  "Recent" = "recent",
                                                                  "Before Boot" = "boot",
                                                                  "Today" = "today",
                                                                  "Yesterday" = "yesterday",
                                                                  "This Week" = "this-week",
                                                                  "Week Ago" = "week-ago",
                                                                  "This Month" = "this-month",
                                                                  "This Year" = "this-year",
                                                                  "Custom" = "custom"),
                                              selected = "now"
                                  ),
                                  conditionalPanel(condition = "input.end == 'custom'",
                                                   dateInput("endDate", "End Date"),
                                                   textInput("endText", "End Clock Time")
                                  ),
                                  checkboxGroupInput("extra", "Extra CSV info to include:",
                                                     c(#"Time" = "--extra-time", This messes up things because of overlapping names
                                                       "Keys" = "--extra-keys",
                                                       "SELinux labels" = "--extra-labels"))
                 ),
                 actionButton("load", "Load Events"),
                 wellPanel(
                   textOutput("p1text1")
                 )
        ),
        tabPanel(title = "Filter", value = "tab2",
                 wellPanel(
                   textOutput("p2text1"),
                   textOutput("p2text2")
                 ),
                 wellPanel(
                    tags$h4("Filter Expression:"),
                    uiOutput("operand1"),
                    selectInput("operator", "Operator", soperands),
                    uiOutput("operand2")
                 ),
                 wellPanel(
                   tags$h4("Create a filter expression and click Filter Events or click Use all events."),
                   actionButton("filterSkip", "Use all events"),
                   actionButton("filter", "Filter Events")

                 )
        ),
        tabPanel(title = "Summary", value = "tab3",
                 wellPanel(
                   htmlOutput("summaryText")
                 )
        ),
        tabPanel(title = "Text", value = "tab4",
                 wellPanel(
                   textOutput("text-text")
                 )
        ),
        navbarMenu(title = "Time Series",
          tabPanel("Heatmap", value = "tab5hm",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("hlowColor", "Low Color", ourColors, selected = "blue"),
                       selectInput("hhighColor", "High Color", ourColors, selected = "red"),
                       width = 3
                     ),
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("heatMap", width = "auto", height = "600px"),
                       width = 9
                     )
                   )
          )
        ),
        navbarMenu(title = "Quantitative",
          tabPanel("Charts", value = "tab6bar",
                   sidebarLayout(
                     sidebarPanel(
                       uiOutput("bar"),
                       selectInput("blowColor", "Low Color", ourColors, selected = "blue"),
                       selectInput("bhighColor", "High Color", ourColors, selected = "red"),
                       width = 3
                     ),
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("barPlot", width = "auto", height = "600px"),
                       width = 9
                     )
                   )
          ),
          #tabPanel("Graphs", value = "tab7"),
          #tabPanel("Heatmap", value = "tab8"),
          tabPanel("Pivot Table", value = "tab9",
                   mainPanel(
                     rpivotTableOutput("pivot", width = "100%", height = "50%")
                   ))
        ),
        navbarMenu(title = "Qualitative", 
          tabPanel("2 Layer Sankey", value = "tab2sankey",
                  sidebarLayout(
                    sidebarPanel(
                      uiOutput("left2"),
                      uiOutput("right2"),
                      width = 3
                    ),
                    # Show a plot of the generated distribution
                    mainPanel(
                      sankeyNetworkOutput("sankeyPlot2", width = "100%", height = "50%")
                    )
                  )
          ),
          tabPanel("3 Layer Sankey", value = "tab3sankey",
                  sidebarLayout(
                    sidebarPanel(
                      uiOutput("left3"),
                      uiOutput("middle3"),
                      uiOutput("right3"),
                      width = 3
                    ),
                    # Show a plot of the generated distribution
                    mainPanel(
                      sankeyNetworkOutput("sankeyPlot3", width = "100%", height = "50%")
                    )
                  )
          ),
          tabPanel("Tree", value = "tab12",
                   # Sidebar with a slider input for number of bins 
                   sidebarLayout(
                     sidebarPanel(
                       uiOutput("one"),
                       uiOutput("two"),
                       uiOutput("three"),
                       uiOutput("four"),
                       width = 3
                     ),
                     # Show a plot of the generated distribution
                     mainPanel(
                       diagonalNetworkOutput("diagPlot", width = "100%", height = "50%")
                     )
                   )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  # Hide all the tabs until we have data
  observe({
    hide(selector = "#navbar li a[data-value=tab2]")
    hide(selector = "#navbar li a[data-value=tab3]")
    hide(selector = "#navbar li a[data-value=tab4]")
    hide(selector = "#navbar li a[data-value=tab5hm]")
    hide(selector = "#navbar li a[data-value=tab6bar]")
    #hide(selector = "#navbar li a[data-value=tab7]")
    hide(selector = "#navbar li a[data-value=tab8]")
    hide(selector = "#navbar li a[data-value=tab9]")
    hide(selector = "#navbar li a[data-value=tab2sankey]")
    hide(selector = "#navbar li a[data-value=tab3sankey]")
    hide(selector = "#navbar li a[data-value=tab12]")
    output$p1text1 <- renderText({ paste("Select time range and click Load Events") })
  })
  
  # Rewrite help text based on current radio button
  observeEvent(input$sourcerb, {
    if (input$sourcerb == "logs")
      output$p1text1 <- renderText({ paste("Select time range and click Load Events") })
    else if (input$sourcerb == "csv")
      output$p1text1 <- renderText({ paste("Click on Load Events") })
    else if (input$sourcerb == "file")
    output$p1text1 <- renderText({ paste("Browse to the file and click on Load Events") })
  })
  
  # Trigger on loading file
  observeEvent(input$load, {
    rptname <- path.expand(file.path(mainDir, subDir, rptFile))
    # Check if file and use its path, otherwise we build the file
    if (input$sourcerb == "file") {
        finame <- file.path(input$file1[datapath])
    } else if (input$sourcerb == "logs") { 
      finame <- path.expand(file.path(mainDir, subDir, csvFile))
      loname <- path.expand(file.path(mainDir, subDir, logFile))
      if (file.exists(file.path(mainDir, subDir, logFile))) {
        unlink(file.path(mainDir, subDir, logFile))
      }
      if (file.exists(file.path(mainDir, subDir, csvFile))) {
        unlink(file.path(mainDir, subDir, csvFile))
      }
      if (file.exists(file.path(mainDir, subDir, rptFile))) {
        unlink(file.path(mainDir, subDir, rptFile))
      }
      
      if ( input$start == "custom") {
        posixDate <- as.POSIXct(input$startDate, format="%Y-%m-%d %H:%M", tz="GMT")
        sd <- paste("--start", strftime(posixDate, format="%x", tz="GMT"), input$startText)
      } else {
          sd <- paste("--start", input$start)
      }
      if ( input$end == "custom") {
        posixDate <- as.POSIXct(input$endDate, format="%Y-%m-%d %H:%M", tz="GMT")
        se <- paste("--end", strftime(posixDate, format="%x", tz="GMT"), input$endText)
      } else {
          se <- paste("--end", input$end)
      }
    
      # check if time is custom, if so use it else use label
      cmd <- paste("/usr/sbin/ausearch --input-logs --raw", sd, se, ">", loname)
      cat(cmd)
      system(cmd, wait = TRUE)
      # Now run aureport to make summary
      cmd <- paste("/usr/sbin/aureport -if", loname, "| /usr/bin/sed ':a;N;$!ba;s/\\n/<br>\\n/g' >", rptname)
      cat(cmd)
      system(cmd, wait = TRUE)
      # And we make the csv file now but wait for this one to complete
      extra <- paste(input$extra, collapse = " ")
      cmd <- paste("/usr/sbin/ausearch -if", loname, extra, "--format csv >", finame)
      cat(cmd)
      system(cmd, wait = TRUE)

    }
    audit <<- read.csv("~/R/audit-data/audit.csv", header=TRUE, stringsAsFactors = FALSE)
    fnames <- reactive({ colnames(audit) }) # All field names
    fnames1 <- colnames(audit)
    fnames1 <- setdiff(fnames1, c("TIME", "SERIAL_NUM")) # Remove time and serial number
    fnames2 <<- reactive({ fnames1 }) # For use with sankey drop boxes
    fnames3 <<- c("", colnames(audit))
    fnames3 <<- setdiff(fnames3, "SERIAL_NUM") # Remove serial number
    fnames4 <<- reactive({ fnames3 }) # For use with the tree diagrams
    fnames5 <<- colnames(audit)
    fnames5[5] <<- "HOUR" # Change serial number to HOUR
    fnames6 <<- reactive({ fnames5 })
    audit$one <<- rep(1,nrow(audit))
    events <<- reactive({ nrow(audit) })
    subaudit <<- audit                            # Consider making this reactiveValues(subaudit <- audit)
    sevents <<- reactive({ 0 })
    show(selector = "#navbar li a[data-value=tab2]")
    
    # Create all the controls based on the logs
    output$p1text1 <- renderText({ paste("Success - now click on Filter tab") })
    output$p2text1 <- renderText({ paste("Total Events:", events()) })
    output$p2text2 <- renderText({ paste("Available Events:", sevents()) })
    output$p2text3 <- renderText({ paste("Create a filter expression and click Filter Events or use all events by clicking Skip Filtering.") })
    output$operand1  <-renderUI({ selectInput("operand1", "Operand1", fnames()) })
    output$left2  <-renderUI({ selectInput("left2", "Left Side", fnames2()) })
    output$right2 <-renderUI({ selectInput("right2", "Right Side", fnames2()) })
    output$left3  <-renderUI({ selectInput("left3", "Left Side", fnames2()) })
    output$middle3 <-renderUI({ selectInput("middle3", "Middle", fnames2()) })
    output$right3 <-renderUI({ selectInput("right3", "Right Side", fnames2()) })
    output$left2  <-renderUI({ selectInput("left2", "Left Side", fnames2()) })
    output$right2 <-renderUI({ selectInput("right2", "Right Side", fnames2()) })
    output$one <-renderUI({ selectInput("one", "Level 1", fnames4()) })
    output$two <-renderUI({ selectInput("two", "Level 2", fnames4()) })
    output$three <-renderUI({ selectInput("three", "Level 3", fnames4()) })
    output$four <-renderUI({ selectInput("four", "Level 4", fnames4()) })
    output$bar <-renderUI({ selectInput("groupBy", "Group By", fnames6(), selected = "HOUR") })
    if (file.exists(file.path(mainDir, subDir, rptFile))) {
      rpt <- readChar(rptname, file.info(rptname)$size)
      output$summaryText <- renderUI({ HTML( paste("<h4>", rpt, "</h4>")) })
    } else {
      output$summaryText <- renderUI({ HTML( paste("<h4>No Report Available.</h4>")) })
    }
  }) # End observeEvent
  
  # This builds operand2 based on which field was selected.
  observeEvent(input$operand1, {
    t <- unique(subaudit[input$operand1])
    t <- t[t[input$operand1] != ""]
    if (!is.na(t))
      t <- sort(t)
    l <- c(t, fnames2())
    output$operand2  <-renderUI({ selectInput("operand2", "Operand2", l) })
  })
  
  observeEvent(input$filter, {
    # Create Subset expression
    if ( input$operand1 != " " ) {
             operand1 <- as.character(input$operand1)
    }
    if( input$operator != " " ) {
      operation <- as.character(input$operator)
    } else {
      output$p2text2 <- renderText({ paste("Error: No operator is selected.") })
      return()
    }
    if ( input$operand1 != " " ) {
      operand2 <- as.character(input$operand2)
    }
    
    # The expression needs quoting if exact value but not when comparing columns
    if (operand2 %in% fnames2()) {
      expr <- paste(operand1, operation, operand2)
    } else {
      expr <- paste(operand1, " ", operation, " '", operand2, "'", sep="")
    }
  
    # Subset the audit information depending on the expression
    if (expr != "  ") {
      subaudit <<- filter_(subaudit, expr)
      # FIXME: How do we test for error?
    } else {
      # How do we ever get here? This 'else' should probably be deleted.
      browser()
      subaudit <<- audit
    }
    
    sevents <<- reactive({ nrow(subaudit) })
    output$p2text2 <- renderText({ paste("Available Events:", sevents()) })
    
    # Unhide everything
    show(selector = "#navbar li a[data-value=tab3]")
    show(selector = "#navbar li a[data-value=tab4]")
    show(selector = "#navbar li a[data-value=tab5hm]")
    show(selector = "#navbar li a[data-value=tab6bar]")
    #show(selector = "#navbar li a[data-value=tab7]")
    show(selector = "#navbar li a[data-value=tab8]")
    show(selector = "#navbar li a[data-value=tab9]")
    show(selector = "#navbar li a[data-value=tab2sankey]")
    show(selector = "#navbar li a[data-value=tab3sankey]")
    show(selector = "#navbar li a[data-value=tab12]")
    output$pivot <- renderRpivotTable({
      rpivotTable(data =   subaudit)
    })

  }) # End observeEvent
  
  observeEvent(input$filterSkip, {
    subaudit <<- audit
    sevents <<- reactive({ nrow(subaudit) })
    output$p2text2 <- renderText({ paste("Available Events:", sevents()) })
    
    # Unhide everything
    show(selector = "#navbar li a[data-value=tab3]")
    show(selector = "#navbar li a[data-value=tab4]")
    show(selector = "#navbar li a[data-value=tab5hm]")
    show(selector = "#navbar li a[data-value=tab6bar]")
    #show(selector = "#navbar li a[data-value=tab7]")
    show(selector = "#navbar li a[data-value=tab8]")
    show(selector = "#navbar li a[data-value=tab9]")
    show(selector = "#navbar li a[data-value=tab2sankey]")
    show(selector = "#navbar li a[data-value=tab3sankey]")
    show(selector = "#navbar li a[data-value=tab12]")
    output$pivot <- renderRpivotTable({
      rpivotTable(data =   subaudit)
    })
    
  }) # End observeEvent

  # Heatmap
  observeEvent(c(input$hlowColor, input$hhighColor, input$filter, input$filterSkip), {
    if (exists("subaudit")) {
      # Create time series data frame for aggregating
      subaudit$posixDate=as.POSIXct(paste(subaudit$DATE, subaudit$TIME), format="%m/%d/%Y %H:%M:%S")
      # Create a column of hour and date to aggregate an hourly total.
      subaudit$TMONTH <- format(subaudit$posixDate, format = '%Y-%m-%d')
      # Collapse events down by summing into time intervals
      month <- aggregate(subaudit$one, by = subaudit["TMONTH"], FUN = length)
      month$t <- as.character(month[,"TMONTH"])
      if (nrow(data.frame(unique(substr(month$t, start=1, stop=7)))) > 1){
        # x = months, y = days
        final = data.frame(date=as.POSIXct(month$t, format="%Y-%m-%d", tz="GMT"))
        final$num <- month$x
        final$j <- months(as.Date(final$date)) # Intermediate value
        final$x <- factor(final$j, levels = unique(final$j))
        final$y <- as.numeric(format(final$date, "%d"))
        bag <- list()
        bag$yscale <- c(1, 31)
        bag$ybrk <- seq(1, 31, 3)
        bag$xlab <- "Month of the Year"
        bag$ylab <- "Day of the Month"
        bag$title <- "Events per Day"
      } else {
        month <- NULL
        subaudit$TMONTH <- NULL
        subaudit$TDAY <- format(subaudit$posixDate, format = '%Y-%m-%d %H')
        day <- aggregate(subaudit$one, by = subaudit["TDAY"], FUN = length)
        day$t <- as.character(day[,"TDAY"])
        if (nrow(data.frame(unique(substr(day$t, start=1, stop=10)))) > 7) {
          # x = days, y = hours
          final = data.frame(date=as.POSIXct(day$t, format="%Y-%m-%d %H", tz="GMT"))
          final$num <- day$x
          final$j <- format(final$date, format="%d") # Intermediate value
          final$x <- factor(final$j, levels = unique(final$j))
          final$y <- as.numeric(format(final$date, "%H"))
          bag <- list()
          bag$yscale <- c(0, 23)
          bag$ybrk <- seq(0, 23, 4)
          bag$xlab <- "Day of the Month"
          bag$ylab <- "Hour of the Day"
          bag$title <- "Events per Hour"
        } else if (nrow(data.frame(unique(substr(day$t, start=1, stop=10)))) > 1) {
          # x = dow, y = hours
          final = data.frame(date=as.POSIXct(day$t, format="%Y-%m-%d %H", tz="GMT"))
          final$num <- day$x
          final$j <- weekdays(as.Date(final$date)) # Intermediate value
          final$x <- factor(final$j, levels = unique(final$j))
          final$y <- as.numeric(format(final$date, "%H"))
          bag <- list()
          bag$yscale <- c(0, 23)
          bag$ybrk <- seq(0, 23, 4)
          bag$xlab <- "Day of Week"
          bag$ylab <- "Hour of Day"
          bag$title <- "Events per Hour"
        } else {
          day <- NULL
          subaudit$TDAY <- NULL
          subaudit$THOUR <- format(subaudit$posixDate, format = '%Y-%m-%d %H:%M')
          hour <- aggregate(subaudit$one, by = subaudit["THOUR"], FUN = length)
          hour$t <- as.character(hour[,"THOUR"])
          if (nrow(data.frame(unique(substr(hour$t, start=1, stop=13)))) > 1) {
            # x = hours, y = minutes
            final = data.frame(date=as.POSIXct(hour$t, format="%Y-%m-%d %H:%M", tz="GMT"))
            final$num <- hour$x
            final$j <- format(final$date, format="%H") # Intermediate value
            final$x <- factor(final$j, levels = unique(final$j))
            final$y <- as.numeric(format(final$date, "%M"))
            bag <- list()
            bag$yscale <- c(0, 59)
            bag$ybrk <- seq(0, 59, 5)
            bag$xlab <- "Hour of Day"
            bag$ylab <- "Minute of Hour"
            bag$title <- "Events per Minute"
          } else {
            hour <- NULL
            subaudit$THOUR <- NULL
            subaudit$TMINUTE <- format(subaudit$posixDate, format = '%Y-%m-%d %H:%M:%S')
            minute <- aggregate(subaudit$one, by = subaudit["TMINUTE"], FUN = length)
            minute$t <- as.character(minute[,"TMINUTE"])
            if (nrow(data.frame(unique(substr(minute$t, start=1, stop=16)))) > 1) {
              # x = minutes, y = sec
              final = data.frame(date=as.POSIXct(minute$t, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
              final$num <- minute$x
              final$j <- format(final$date, format="%M") # Intermediate value
              final$x <- factor(final$j, levels = unique(final$j))
              final$y <- as.numeric(format(final$date, "%S"))
              bag <- list()
              bag$yscale <- c(0, 59)
              bag$ybrk <- seq(0, 59, 5)
              bag$xlab <- "Minute of the Hour"
              bag$ylab <- "Second of the Minute"
              bag$title <- "Events per Second"
            } else {
              final = data.frame(date=as.POSIXct(minute$t, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
              final$num <- minute$x
              final$j <- format(final$date, format="%S") # Intermediate value
              final$x <- factor(final$j, levels = unique(final$j))
              final$y <- as.numeric(format(final$date, "%S"))
              bag <- list()
              bag$yscale <- c(0, 59)
              bag$ybrk <- seq(0, 59, 5)
              bag$xlab <- "Second of the Minute"
              bag$ylab <- "Second of the Minute"
              bag$title <- "Events Within Same Second"
            }
          }
        }
      }
      
      # Cleanup
      minute <- NULL
      subaudit$TMINUTE <- NULL
      subaudit$posixDate <- NULL
      
      output$heatMap<-renderPlot({
        pl <- ggplot(final, aes(x=final$x, y=final$y, fill=final$num)) + geom_tile() + 
          theme_bw() + scale_fill_continuous(low=input$hlowColor, high=input$hhighColor, name="Events") +
          # scale_fill_continuous(low="#99CCFF", high="#FF5533", name="Events") + #3399FF is good
          # scale_fill_continuous(low="#F7FBFF", high="#2171B5", name="Events") +
          ylim(bag$yscale) +  scale_y_continuous(breaks=bag$ybrk) +
          ggtitle(bag$title) + labs(x=bag$xlab, y=bag$ylab) +
          theme(axis.text = element_text(size = 14),
                axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),
                legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                axis.title=element_text(size = 20, face="bold"), plot.title = element_text(size = 20))
        
        print(pl)
      })
    } # End subaudit exists
  }) # End observe
  
  
  # This is the barchart
  observeEvent(c(input$groupBy, input$blowColor, input$bhighColor, input$filter, input$filterSkip), {
    #browser()
    # For some reason this triggers when the program starts up and before files are loaded.
    # So, we use this to avoid doing anything with undefined variables.
    if (exists("subaudit") && !is.null(input$groupBy)) {
      if  (! "HOUR" %in% colnames(subaudit)) {
        subaudit$posixDate=as.POSIXct(paste(subaudit$DATE, subaudit$TIME), format="%m/%d/%Y %H:%M:%S")
        # Create a column of hour and date to aggregate an hourly total.
        subaudit$HOUR = format(subaudit$posixDate, format = '%Y-%m-%d %H')
        subaudit$posixDate <- NULL
      }

    # Now summarize it
    grp <- input$groupBy
    temp <- aggregate(subaudit$one, by = subaudit[grp], FUN = length)
    temp$t <- as.character(temp[,grp])
    
    if (grp == "HOUR") {
      # Time based needs special handling
      final = data.frame(date=as.POSIXct(temp$t, format="%Y-%m-%d %H", tz="GMT"))
      final$num <- temp$x
      final$day <- weekdays(as.Date(final$date))
      final$oday <- factor(final$day, levels = unique(final$day))
      final$hour <- as.numeric(format(final$date, "%H"))
      
      output$barPlot<-renderPlot({
        pl <- ggplot(final, aes(x=final[,1], y=final$num, fill=final$num)) +
          geom_bar(stat="identity") + ggtitle(paste("Events by", grp)) +
          scale_x_datetime() + xlab("") + labs(x=grp, y="Number of Events") +
          scale_fill_gradient(low=input$blowColor, high = input$bhighColor, name=paste("Events/", grp, sep=""))
        print(pl)
      })
    } else {
      # non-time conversion branch
      final <- temp[,1:2]
      colnames(final) = c("factors", "num")
      final$factors <- abbreviate(final$factors, minlength = 20, strict = TRUE)
      
      # We will rotate based on how dense the labels are
      rot <- 90
      if (nrow(final) < 20)
        rot <- 60
      if (nrow(final) < 10)
        rot <- 45
      
      # Plot it
      output$barPlot<-renderPlot({
        pl <- ggplot(final, aes(x=final[,1], y=final$num, fill=final$num)) +
          geom_bar(stat="identity") + ggtitle(paste("Events by", grp)) +
          scale_x_discrete() + xlab("") + labs(x=grp, y="Number of Events") +
          scale_fill_gradient(low=input$blowColor, high = input$bhighColor, name=paste("Events/", grp, sep="")) +
          theme(axis.text.x = element_text(angle = rot, hjust = 1, size = 18))
        print(pl)
      })
    }
    }
  }) # End observeEvent
  
    
  # This is the 2 level sankey diagram
  observeEvent(c(input$left2, input$right2, input$filter, input$filterSkip), {
    output$sankeyPlot2 <- renderSankeyNetwork({
      left_field <- input$left2
      right_field <- input$right2
      
      # Make a dataframe for a 2 level Sankey diagram
      left = data.frame(subaudit[left_field], subaudit[right_field], subaudit$one)
      colnames(left) = c("Source", "Target", "Num")
      remove(subaudit)
      
      # Now summarize and collapse to unique values to calculate edges
      l <- ddply(left, .(Source,Target), summarize, Value=sum(Num))
      
      # Calculate Nodes lookup table
      nodes <- c(as.character(l$Source), as.character(l$Target))
      nodes <- data.frame(unique(as.factor(nodes)))
      colnames(nodes) = c("Source")
      nodes$ID <- seq.int(from = 0, to = nrow(nodes) - 1)
      nodes <- nodes[,c("ID","Source")]
      
      # Now map Node lookup table numbers to source and target
      # Merge index onto Source
      edges <- merge(l,nodes,by.x = "Source")
      edges$Source <- NULL
      
      # Merge index onto Target
      names(edges) = c("Target","Value","Sindx")
      names(nodes) = c("ID", "Target")
      edges <- merge(edges,nodes,by.x = "Target")
      edges$Target <- NULL
      
      # rename everything so its nice and neat
      names(edges) <- c("value","source","target")
      names(nodes) = c("ID", "name")
      
      sankeyNetwork(Links = edges, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize = 16, nodeWidth = 30,
                    height = 2500, width = 2500)
    })
  }) # End 2 layer sankey
  
  # This is the 3 key sankey diagram
  observeEvent(c(input$left3, input$middle3, input$right3, input$filter, input$filterSkip), {
    output$sankeyPlot3 <- renderSankeyNetwork({
      
      # Copy the inputs to simpler names
      left_field <- input$left3
      middle_field <- input$middle3
      right_field <- input$right3
    
      # Make 2 dataframes for a 3 level Sankey
      left = data.frame(subaudit[left_field], subaudit[middle_field], subaudit$one)
      colnames(left) = c("Source", "Target", "Num")
      right = data.frame(subaudit[middle_field], subaudit[right_field], subaudit$one)
      colnames(right) = c("Source", "Target", "Num")
    
      # Now summarize and collapse to unique values to calculate edges
      l <- ddply(left, .(Source,Target), summarize, Value=sum(Num))
      r <- ddply(right, .(Source,Target), summarize, Value=sum(Num))
    
      # Free up some memory
      remove(left)
      remove(right)
    
      # Calculate Nodes lookup table
      nodes <- c(as.character(l$Source), as.character(l$Target), as.character(r$Target))
      nodes <- data.frame(unique(as.factor(nodes)))
      colnames(nodes) = c("Source")
      nodes$ID <- seq.int(from = 0, to = nrow(nodes) - 1)
      nodes <- nodes[,c("ID","Source")]
    
      # Now map Node lookup table numbers to source and target
      # Merge index onto Source
      l_edges <- merge(l, nodes, by.x = "Source")
      l_edges$source = l_edges$ID
      r_edges <- merge(r, nodes, by.x = "Source")
      r_edges$source = r_edges$ID
    
      # Merge index onto Target
      names(nodes) = c("ID", "Target")
      l_edges2 <- l_edges[,c("Source","Target","Value","source")]
      r_edges2 <- r_edges[,c("Source","Target","Value","source")]
      l_edges <- merge(l_edges2, nodes, by.x = "Target")
      r_edges <- merge(r_edges2, nodes, by.x = "Target")
    
      # rename everything so its nice and neat
      names(l_edges) <- c("osrc", "otgt", "value", "source", "target")
      names(r_edges) <- c("osrc", "otgt", "value", "source", "target")
      names(nodes) = c("ID", "name")
    
      # Combine into one big final data frame
      edges <- rbind(l_edges, r_edges)
    
      sankeyNetwork(Links = edges, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 16, nodeWidth = 30,
                  height = 2500, width = 2500)
    })
  }) # End observeEvent

  # Tree chart
  observeEvent(c(input$one, input$two, input$three, input$four, input$filter, input$filterSkip), {
    if (is.null(input$one)) {
      return()
    }
    # Make some shorthand variables
    first <- input$one
    second <- input$two
    third <- input$three
    fourth <- input$four
    d <- c("")
  
    if (first != "")  { d <- c(d, first)  }
    if (second != "") { d <- c(d, second) }
    if (third != "")  { d <- c(d, third)  }
    if (fourth != "") { d <- c(d, fourth) }
    if (length(d) > 1) {
      d <- d[2:length(d)]
      a <- data.frame(subaudit[, d]) # This is not transferring the column name
      colnames(a) <- d
    } else {
      return()
    }

    # Convert from data frame to tree structure by making a map of the data.
    # The pathString is the columns glued together with a '/' separator.
    # The inner paste glues columns together, the outter adds report as an anchor
    j <- ncol(a)
    k <- nrow(a)
    for (i in 1:k) {
      a$pathString[i] <- paste("report", paste(a[i,1:j], collapse = "\\"), sep = "\\")
    }
      
    # Now convert to tree structure
    l <- as.Node(a, pathDelimiter = "\\")
      
    # And now as a hierarchial list
    b <- ToListExplicit(l, unname = TRUE)
      
    # And visualize it
    output$diagPlot <- renderDiagonalNetwork({      
      diagonalNetwork(List = b, fontSize = 10)
    })
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
