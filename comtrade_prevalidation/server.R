library(shiny)
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

data <- readRDS(file = "data.rds")
hsdataorig <- readRDS(file = "noquant_commodity.rds")

shinyServer(function(input, output, session) {
  
  # Round numbers
  tbl <- reactive({
    
    
    yearMin <- input$years[1]
    yearMax <- input$years[2]
    
    
    if(input$uvcompare == "asis") {
      measure.vars <- c("quantity", "value", "uv", 
                        "uv_global", "uv_reporter", "uv_mirr", "uv_roll3", 
                        "influence", "residual")
      
      tbl <- data %>%
        filter_(~year >= yearMin,
                ~year <= yearMax) %>%
        # select_(~-no_quantity, ~-no_value, ~-magnit_ord, ~-item) %>%
        reshape2::melt(measure.vars = measure.vars,
                       value.name = "v", variable.name = "variable")  %>% 
        mutate_(v = ~round(v, digits = input$roundn)) %>% 
        reshape2::dcast(... ~ variable, value.var = "v") %>% 
        select_(~reporter, ~partner, ~year, ~hs, ~item, ~flow, ~quantity,
                ~value, ~uv, ~uv_reporter, ~uv_global, ~uv_mirr, ~uv_roll3,
                ~influence, ~residual, ~no_quantity, ~no_value, ~magnit_ord,
                ~selftrade, ~outofrange)
      return(tbl)
      
    }
    
    
    
    if(input$uvcompare == "abso") {
      measure.vars <- c("quantity", "value", "uv", 
                        "uv_global", "uv_reporter", "uv_mirr", "uv_roll3", 
                        "influence", "residual")
      
      tbl <- data %>%
        filter_(~year >= yearMin,
                ~year <= yearMax) %>%
        reshape2::melt(measure.vars = c("uv_global", "uv_reporter", "uv_mirr", "uv_roll3"), 
                       value.name = "v", variable.name = "variable")  %>% 
        mutate_(v = ~abs(v - uv)) %>% 
        reshape2::dcast(... ~ variable, value.var = "v") %>% 
        reshape2::melt(measure.vars = measure.vars,
                       value.name = "v", variable.name = "variable")  %>% 
        mutate_(v = ~round(v, digits = input$roundn)) %>% 
        reshape2::dcast(... ~ variable, value.var = "v") %>% 
        select_(~reporter, ~partner, ~year, ~hs, ~item, ~flow, ~quantity,
                ~value, ~uv, ~uv_reporter, ~uv_global, ~uv_mirr, ~uv_roll3,
                ~influence, ~residual, ~no_quantity, ~no_value, ~magnit_ord,
                ~selftrade, ~outofrange)
      return(tbl)
      
    }
    
    
    if(input$uvcompare == "rela") {
      measure.vars <- c("quantity", "value", "uv", 
                        "uv_global", "uv_reporter", "uv_mirr", "uv_roll3", 
                        "influence", "residual", "uv_totaldiff")
      
      tbl <- data %>%
        filter_(~year >= yearMin,
                ~year <= yearMax) %>%
        reshape2::melt(measure.vars = c("uv_global", "uv_reporter", "uv_mirr", "uv_roll3"), 
                       value.name = "v", variable.name = "variable")  %>% 
        mutate_(v = ~abs(v - uv) / uv * 100) %>% 
        reshape2::dcast(... ~ variable, value.var = "v") %>% 
        mutate_(uv_totaldiff = ~uv_global + uv_reporter + uv_mirr + uv_roll3) %>%
        reshape2::melt(measure.vars = measure.vars,
                       value.name = "v", variable.name = "variable")  %>% 
        mutate_(v = ~round(v, digits = input$roundn)) %>% 
        reshape2::dcast(... ~ variable, value.var = "v") %>% 
        select_(~reporter, ~partner, ~year, ~hs, ~item, ~flow, ~quantity,
                ~value, ~uv, ~uv_totaldiff, ~uv_reporter, ~uv_global, ~uv_mirr, ~uv_roll3,
                ~influence, ~residual, ~no_quantity, ~no_value, ~magnit_ord,
                ~selftrade, ~outofrange)
      return(tbl)
      
    }
  })
  
  userChoice <- reactive({
    userRowNumb <- input$tbl_row_last_clicked
    
    # Eventually should replace with conditional tables/graphs
    # if(is.null(userRowNumb)) return(data.frame())
    if(is.null(userRowNumb)) userRowNumb <- 1
    userRow <- tbl()[userRowNumb, c("reporter", "partner", "year", "flow", "hs")]
    userRow$flow <- as.character(userRow$flow)
    userRow
  })
  
  tsdata <- reactive({
    
    if(nrow(userChoice()) == 0) return(data.frame()) 
    
    userhs <- userChoice()$hs
    countries <- c(userChoice()$reporter, userChoice()$partner)
    yearMin <- input$years[1]
    yearMax <- input$years[2]
    
    data <- data %>% 
      filter_(~reporter %in% countries,
              ~partner  %in% countries,
              ~hs == userhs,
              ~year >= yearMin,
              ~year <= yearMax)
    data
  })
  
  output$tsdata <- DT::renderDataTable(DT::datatable(tsdata()))
  
  
  hsdata <- reactive({
   hsdataorig %>% 
      mutate_(noquantprop = ~round(noquantprop, input$roundn),
              reporter = ~stringr::str_replace(reporter, "\\(.*\\)", ""),
              reporter = ~as.factor(reporter)) %>% 
      select_(~reporter, ~hs, ~n, ~noquant, ~noquantprop, ~item) %>% 
      arrange_(~desc(noquantprop)) 
  })
  
  output$hsdata <- DT::renderDataTable(DT::datatable(hsdata(),
                                                     filter = "top",
                                       options = list(pageLength = 25),
                                       colnames = c("Flows total" = "n", 
                                                    "No quantity" = "noquant",
                                                    "No quantity %" = "noquantprop")))
  
  
  output$tsplot <- renderPlot({
    
    measure.vars <- c("uv", 
                      "uv_global", "uv_reporter", "uv_mirr", "uv_roll3") 
    
    
    data <- tsdata() %>% 
      select_(~reporter, ~partner, ~year, ~flow, ~hs, ~item, ~uv, ~uv_global, 
              ~uv_reporter, ~uv_mirr, ~uv_roll3) %>%
      filter_(.dots = list(lazyeval::interp(~flow == userChoice()[['flow']]))) %>%
      mutate_(year = ~as.Date(paste0(year, "-01-01"))) %>%
      reshape2::melt(measure.vars = measure.vars,
                     value.name = "value", variable.name = "uv_type")   
    
    ggplot(data, aes_string("year", "value", color = "uv_type")) + 
      geom_path() + geom_point() +
      scale_x_date("", labels = scales::date_format("%Y"), 
                   breaks = scales::date_breaks("year")) +
      scale_y_continuous("Unit value, $1000/t") +
      ggtitle(paste("Reporter", userChoice()$reporter,
                    "partner", userChoice()$partner,
                    userChoice()$flow, "of", userChoice()$hs))
    
  })
  
  output$origvaluests <- renderPlot({
    tsdata() %>% 
      select_(~reporter, ~partner, ~year, ~flow, ~hs, ~item, ~value, ~quantity) %>%
      reshape2::melt(measure.vars = c("value", "quantity")) %>%
      mutate_(year = ~as.Date(paste0(year, "-01-01"))) %>%
      ggplot(aes_string("year", "value", color = "reporter")) +
      geom_path() + geom_point() + 
      facet_wrap(~variable, scales = "free_y", nrow = 2) +
      scale_y_continuous("") +
      scale_x_date("", labels = scales::date_format("%Y"), 
                   breaks = scales::date_breaks("year"))
    
  })
  
  
  action <- reactive({DT::dataTableAjax(session, tbl(), rownames = T) })
  output$tbl <- DT::renderDataTable({
    DT::datatable(tbl(), rownames = T, # server = T,
                  selection = "single",
                  filter = "top",
                  options = list(
                    ajax = list(url = action())))
  }, server = T)
  
  
  ###### Tariffline no quantity
  
  hs2_miss <- readRDS("tariffline_hs2_missing_quant.Rds")
  
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- hs2_miss[hs2_miss$id == x$id, ]
    paste0(row$name, "<br />",
           "HS: ", row$hs2, "<br />",
           "No quant: ", paste0(round(100 * row$n_of_miss, 2), "%"),
           collapse = "")
  }
  
  hs2_miss %>% 
    filter(max_miss_reporter > 0) %>% 
    ggvis(x = ~reorder(name, me_miss_reporter),
          y = ~n_of_miss,
          key := ~id) %>% 
    layer_points(size := 40,
                 opacity := .2) %>% 
    add_tooltip(all_values)%>% 
 set_options(height = 600, width = 1400) %>% 
  add_axis("y",
           title = "Share of trade flows with missing quantity", 
           format = "%",
           title_offset = 50) %>% 
  add_axis("x", title = "",
           properties = axis_props(
               labels = list(angle = 270,
                             align = "right",
                             baseline = "left"))) %>% 
    bind_shiny("tariffline_hs2_noquant")
})

