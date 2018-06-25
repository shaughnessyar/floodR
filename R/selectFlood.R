#' Interactively Select Floods with Shiny Gadget
#'
#' @description This function opens a shiny gadget to allow users to interactively select floods.
#'
#' @usage selectFloods(.data, dateVar = NULL, timeVar = NULL, dateTimeVar = NULL, QVar)
#'
#' @param .data The working dataframe
#' @param dateVar The variable holding date data
#' @param timeVar The variable holding time data
#' @param dateTimeVar The variable holding time data
#' @param QVar The variable holding discharge/stage data
#'
#' @return A list of start and end date times for selected floods
#'
#' @examples
#' \dontrun{
#' selectFloods(df, dateVar = Date, timeVar = Time, QVar = Q)
#' selectFloods(df, dateTimeVar = DateTime, QVar = discharge)
#'}
#'
#' @export
selectFloods <- function(.data, dateVar = NULL, timeVar = NULL, dateTimeVar = NULL, QVar){
  paramList <- as.list(match.call())

    ui <- miniPage(
      gadgetTitleBar("Flood Selector"),
      miniContentPanel(
         tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }"
         ),
         fixedRow(
           column(8,
                  withSpinner(plotOutput(outputId = "Plot", brush = brushOpts(id = "plot_brush", resetOnNew = TRUE),
                               dblclick = "Plot_dblclick"))),
           column(4,
                  DT::dataTableOutput(outputId = "table"),
                  actionButton(inputId = "remove", label = "Remove Flood"))
         ),

        textOutput("brush_info")),
        actionButton(inputId = "select", label = "Select Flood",width = '200px')
      )

    server <- function(input, output, session) {

      df <- reactive({
        return(.data)
      })

      graph_data <- reactive({
        data <- df()
        if(!is.null(filter_dat$lower)){
          graph_df <- data %>% filter(row_number() >= filter_dat$lower & row_number() <= filter_dat$upper)
          return(graph_df)
        } else {
          return(data)
        }
      })

      floods <- reactiveValues(
        select_flood = c()
      )

      filter_dat <- reactiveValues(lower = NULL, upper = NULL)

      output$Plot <- renderPlot({
        if("QVar" %in% names(paramList)){
          Q_inpt <- rlang::quo(!! rlang::sym(paramList$QVar))
        }
        x_var <- c(1:nrow(graph_data()))
        y_var <- graph_data() %>% select(!!Q_inpt)
        plotData <- as.data.frame(cbind(x_var, y_var))
        p <- ggplot(data = plotData, mapping = aes(x = x_var, y = y_var))+
          geom_line(color = "blue")+
          geom_point(color = "blue")+
          xlab("Date")+
          ylab(input$chems)+
          theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())
        print(p)
      })

      output$brush_info <- renderText({
        if("dateTimeVar" %in% names(paramList)){
          dateTime_inpt <- rlang::quo(!! rlang::sym(paramList$dateTimeVar))
        }
        if("dateVar" %in% names(paramList)){
          date_inpt <- rlang::quo(!! rlang::sym(paramList$dateVar))
        }
        if("timeVar" %in% names(paramList)){
          time_inpt <- rlang::quo(!! rlang::sym(paramList$timeVar))
        }
        if("dateVar" %in% names(paramList) & "timeVar" %in% names(paramList)){
          dates <- graph_data() %>% select(!!date_inpt)
          times <- graph_data() %>% select(!!time_inpt)
          dttm <- as.data.frame(cbind(dates, times))
          dttm[,3] <- paste(dttm[,1], dttm[,2])
          lower <- round(as.numeric(input$plot_brush$xmin) * nrow(graph_data()))
          upper <- round(as.numeric(input$plot_brush$xmax) * nrow(graph_data()))
          LL <- dttm[lower,3]
          UL <- dttm[upper,3]
          paste0("Date Range: from = ",  LL, " to = ", UL)
        }
       else if("dateTimeVar" %in% names(paramList)){
          dttm <- graph_data() %>% select(!!dateTime_inpt)
          lower <- round(as.numeric(input$plot_brush$xmin) * nrow(graph_data()))
          upper <- round(as.numeric(input$plot_brush$xmax) * nrow(graph_data()))
          LL <- dttm[lower,1]
          UL <- dttm[upper,1]
          paste0("Date Range: from = ",  LL, " to = ", UL)

       } else{
         warning("Incorrect numer of date/time/datetime variables")
       }
    })

       observeEvent(input$Plot_dblclick, {
         if (!is.null(input$plot_brush)) {
           filter_dat$lower <- round(as.numeric(input$plot_brush$xmin) * nrow(df()))
           filter_dat$upper <- round(as.numeric(input$plot_brush$xmax) * nrow(df()))
           } else {
             filter_dat$lower <- NULL
             filter_dat$upper <- NULL
             }
         })

      observeEvent(input$select, {
        if("dateTimeVar" %in% names(paramList)){
          dateTime_inpt <- rlang::quo(!! rlang::sym(paramList$dateTimeVar))
        }
        if("dateVar" %in% names(paramList)){
          date_inpt <- rlang::quo(!! rlang::sym(paramList$dateVar))
        }
        if("timeVar" %in% names(paramList)){
          time_inpt <- rlang::quo(!! rlang::sym(paramList$timeVar))
        }

        if("dateVar" %in% names(paramList) & "timeVar" %in% names(paramList)){
          dates <- graph_data() %>% select(!!date_inpt)
          times <- graph_data() %>% select(!!time_inpt)
          dttm <- as.data.frame(cbind(dates, times))
          dttm[,3] <- paste(dttm[,1], dttm[,2])
          lower <- round(as.numeric(input$plot_brush$xmin) * nrow(graph_data()))
          upper <- round(as.numeric(input$plot_brush$xmax) * nrow(graph_data()))
          LL <- dttm[lower,3]
          UL <- dttm[upper,3]
          floods$select_flood <- c(floods$select_flood, as.character(LL), as.character(UL))
        }
       else if("dateTimeVar" %in% names(paramList)){
          dttm <- graph_data() %>% select(!!dateTime_inpt)
          lower <- round(as.numeric(input$plot_brush$xmin) * nrow(graph_data()))
          upper <- round(as.numeric(input$plot_brush$xmax) * nrow(graph_data()))
          LL <- dttm[lower,1]
          UL <- dttm[upper,1]
          floods$select_flood <- c(floods$select_flood, as.character(LL), as.character(UL))
       }
      })

      output$table <-  DT::renderDataTable({
        if(is.null(floods$select_flood)){
          Start <- c(NA)
          End <- c(NA)
          dt <- cbind(Start, End)
          table_ouput <- datatable(dt,
                                   extensions = 'Scroller',
                                   options = list(
                                     deferRender = TRUE,
                                     scrollY = 300,
                                     scroller = TRUE
                                   )
          )
          return(table_ouput)
        }
        else{
          Start <- floods$select_flood[c(TRUE, FALSE)]
          End <-  floods$select_flood[c(FALSE, TRUE)]
          dt <- cbind(Start, End)
          table_ouput <- datatable(dt,
                                   extensions = 'Scroller',
                                   options = list(
                                    deferRender = TRUE,
                                    scrollY = 300,
                                    scroller = TRUE
                                  )
        )
        return(table_ouput)
        }

      })

      observeEvent(input$remove, {
        if(is.null(input$table_rows_selected)){
          return(NULL)
        } else{
          starts <- (2 * input$table_rows_selected)-1
          ends <- (2 * input$table_rows_selected)
          drops <- c(starts, ends)
          floods$select_flood <- floods$select_flood[-drops]
        }

      })

      observeEvent(input$done, {

        selectedFloods <- list(
          start =floods$select_flood[c(TRUE, FALSE)],
          end = floods$select_flood[c(FALSE, TRUE)]
        )

        returnValue <- selectedFloods
        stopApp(returnValue)
      })
    }

    runGadget(ui, server, viewer = dialogViewer("Flood Selector", width = c(1200)))
}
