library(tidyverse)
library(magrittr)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(shinyjqui)
library(furrr)
library(scales)
library(fdapace)

# Initialize Parallel Computing Capability for Increased Loop Speed ####
plan(multiprocess,workers = 3) 

# Function to Remove Shiny Inputs When They are Removed
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

fly_plot_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("plot_select_input") 
  )
}

# Server Namespace Outline for Log Hazard Functions Tab ####
fly_plot_module <- function(input,output,session){
  plot_reactive <- reactive({
    selected_cohort <- input$cohort
    selected_cohort_range <- seq(length(selected_cohort))
    filter_pastes <- selected_cohort_range %>% map_chr(~paste0("Cohort == ",selected_cohort[.x]))
    if(length(selected_cohort) != 0){
      #: Change the Data Filtering based on Select Input Value ####
      if(input$gender == "Male"){
        if(selected_cohort == "All Cohorts"){
          gg <- ggplot()
          gg <- gg + df_nested_hazard %>% extract2("data") %>% 
            future_imap(~geom_line(mapping = aes(x = age, y = hazard_male,color = "Male",text = paste0("Cage: ",df_nested_hazard %>% 
                                                                                                         extract2("Cage") %>% extract2(.y),"<br>",
                                                                                                       "Cohort: ",df_nested_hazard %>% 
                                                                                                         extract2("Cohort") %>% extract2(.y))),data = .x),.progress = TRUE) 
          gg <- gg + scale_color_manual(values = rep("lightblue",df_nested_hazard %>% extract2("data") %>%  length))+
            scale_y_continuous(trans = log_trans(),breaks = log_breaks(n = 7,base = exp(1) ))+xlab("Age")+ylab("Hazard Function")+
             geom_line(aes(x  = 1:51,y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_male"))), 
                                                     Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
            labs(color = "")
           
          gg %<>% ggplotly(tooltip = "text") %>% config(displayModeBar = FALSE) %>%  layout(annotations = 
                                                                                              list(x = 1, y = -0.1, text = "(Hazard Function is Log Transformed)", 
                                                                                                   showarrow = F, xref='paper', yref='paper', 
                                                                                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                                                                   font=list(size=9, color="#e75480")))                        
        }
        
        else{
          print(filter_pastes)
          gg <- ggplot()
          filtered_data <- df_nested_hazard %>% filter(eval(parse(text = paste0(filter_pastes,collapse = " | ")))) 
          gg <- gg + filtered_data %>% extract2("data") %>%
            future_imap(~geom_line(mapping = aes(x = age, y = hazard_male,color = "Male",text = paste0("Cage: ",filtered_data %>% 
                                                                                                         extract2("Cage") %>% extract2(.y),"<br>",
                                                                                                       "Cohort: ",filtered_data %>% 
                                                                                                         extract2("Cohort") %>% extract2(.y))),data = .x),.progress = TRUE) 
          gg <- gg + scale_color_manual(values = rep("lightblue",filtered_data %>% extract2("data") %>%  length))+
            scale_y_continuous(trans = log_trans(),breaks = log_breaks(n = 7,base = exp(1) ))+xlab("Age")+ylab("Hazard Function")+
            geom_line(aes(x  = 1:51,y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_male"))), Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
            labs(color = "")
          
          gg %<>% ggplotly(tooltip = "text")%>% config(displayModeBar = FALSE) %>%  layout(annotations = 
                                                                                             list(x = 1, y = -0.1, text = "(Hazard Function is Log Transformed)", 
                                                                                                  showarrow = F, xref='paper', yref='paper', 
                                                                                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                                                                  font=list(size=9, color="#e75480")))
        }
      }
      
      else{
        if(selected_cohort == "All Cohorts"){
          gg <- ggplot()
          gg <- gg + df_nested_hazard %>% extract2("data") %>% 
            future_imap(~geom_line(mapping = aes(x = age, y = hazard_female,color = "Female",text = paste0("Cage: ",df_nested_hazard %>% 
                                                                                                             extract2("Cage") %>% extract2(.y),"<br>",
                                                                                                           "Cohort: ",df_nested_hazard %>% 
                                                                                                             extract2("Cohort") %>% extract2(.y))),data = .x),.progress = TRUE) 
          gg <- gg + scale_color_manual(values = rep("pink",df_nested_hazard %>% extract2("data") %>%  length))+
            scale_y_continuous(trans = log_trans(),breaks = log_breaks(n = 7,base = exp(1) ))+xlab("Age")+ylab("Hazard Function")+
            geom_line(aes(x  = 1:51,y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_female"))), Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
            labs(color = "") 
          
          gg %<>% ggplotly(tooltip = "text") %>% config(displayModeBar = FALSE) %>%  layout(annotations = 
                                                                                              list(x = 1, y = -0.1, text = "(Hazard Function is Log Transformed)", 
                                                                                                   showarrow = F, xref='paper', yref='paper', 
                                                                                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                                                                   font=list(size=9, color="#e75480")))                        
        }
        
        
        
        else{ 
          gg <- ggplot()
          filtered_data <- df_nested_hazard %>%  filter(eval(parse(text = paste0(filter_pastes,collapse = " | ")))) 
          gg <- gg + filtered_data %>% magrittr::extract2("data") %>% future_imap(~geom_line(mapping = aes(x = age, y = hazard_female,color = "Female",text = paste0("Cage: ",filtered_data %>% 
                                                                                                                                                                       extract2("Cage") %>% extract2(.y),"<br>",
                                                                                                                                                                     "Cohort: ",filtered_data %>% 
                                                                                                                                                                       extract2("Cohort") %>% extract2(.y))),data = .x),.progress = TRUE)
          gg <- gg + scale_color_manual(values = c(rep("pink",filtered_data %>% extract2("data") %>% length)))+
            scale_y_continuous(trans = log_trans(),breaks = log_breaks(n = 7,base = exp(1) ))+xlab("Age")+ylab("Hazard Function")+
            geom_line(aes(x  = 1:51,y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_female"))), Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
            labs(color = "") 
          
          gg %<>% ggplotly(tooltip = "text") %>% config(displayModeBar = FALSE) %>%  layout(annotations = 
                                                                                              list(x = 1, y = -0.1, text = "(Hazard Function is Log Transformed)", 
                                                                                                   showarrow = F, xref='paper', yref='paper', 
                                                                                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                                                                   font=list(size=9, color="#e75480")))                                        
        } 
      }
    }
  }) 
  
  #: Render Plotly Reactive Object ####
  output$plot <- renderPlotly({
    if(input$gender != ""){
      plot_reactive()
    }
  })
  
  # Establish General UI Output for Each Namespace ####
  output[["plot_select_input"]] <- renderUI({
    ns <- session$ns
    tags$div(id = environment(ns)[["namespace"]],
             tagList(
               box(height = "700px",width = "700px",
                   plotlyOutput(ns("plot"),height = "480px",width = "700px"),       
                   selectInput(ns("gender"),label = "Gender",choices = c("Male","Female")),
                   selectInput(ns("cohort"),label = "Cohort",choices = c(df_nested_hazard$Cohort,"All Cohorts"),multiple = TRUE),
                   actionButton(ns('delete_button'),"Remove Plot",icon = icon('times'),style = 'float: right')
               )
               
             )
    )
  })
}

# UI ####
ui <- dashboardPage(
  dashboardHeader(title = "Medfly Research Vizualization"),
  dashboardSidebar(
    #: Sidebar ####
    sidebarMenu(
      menuItem("Box Plot Visualizations",tabName = "box_plot",icon = icon("box-open")),
      menuItem("Log Hazard Functions",tabName = "main_plots",icon = icon("skull-crossbones")),
      menuItem("Mean Curve",tabName = "mean_curve",icon = icon("sad-tear")),
      menuItem("Eigenfunctions",tabName = "eigenfunctions",icon = icon("ghost"))
    )),
  #: Body ####
  dashboardBody(
    tabItems(
      tabItem(tabName = "box_plot",
              plotlyOutput("male_boxplot"),
              plotlyOutput("female_boxplot")),
      tabItem(tabName = "mean_curve",
              plotlyOutput("male_mean_curve"),
              plotlyOutput("female_mean_curve")),
      tabItem(tabName = "eigenfunctions",
              plotlyOutput("male_eigenfunctions"),
              plotlyOutput("female_eigenfunctions")),
      tabItem(tabName = "main_plots", 
              actionButton("add_plot",label = "Add Plot",icon = icon('plus'))
      )
    )
  )
)
# Server ####
server <- function(input, output,session) {

  ### Male Log Hazard Function Boxplot and Mean Curve ####
  output$male_boxplot <- renderPlotly({
    boxplot_male <- ggplot()
    boxplot_male <- boxplot_male + box_plot_data_male %>%
      future_map2(multiples_of_five,~geom_boxplot(data = box_plot_data_male %>% as.data.frame(),aes(x = .y,y = .x,color = "Male")))+
      scale_color_manual(values = "lightblue")+
      geom_line(aes(x  = 1:51,y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_male") %>% log)),
                                               Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
      xlab("Age")+ylab("Log Transformed Hazard Function")+labs(color = "")

    boxplot_male %<>% ggplotly %>% config(displayModeBar = FALSE)
    boxplot_male
  })

  ### Female Log Hazard Function Boxplot and Mean Curve  ####
  output$female_boxplot <- renderPlotly({
    boxplot_female <- ggplot()
    boxplot_female <- boxplot_female + box_plot_data_female %>%
      future_map2(multiples_of_five,~geom_boxplot(data = box_plot_data_female %>% as.data.frame(),aes(x = .y,y = .x,color = "Female")))+
      scale_color_manual(values = "pink")+
      geom_line(aes(x  = 1:51,y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_female") %>% log)),
                                               Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
      xlab("Age")+ylab("Log Transformed Hazard Function")+labs(color = "")

    boxplot_female %<>% ggplotly %>% config(displayModeBar = FALSE)
    boxplot_female
  })

 # Male Mean Curve and Ribbon #### 
  output$male_mean_curve <- renderPlotly({
    mean_curve_male_plot <- ggplot() +  
      geom_line(aes(x  = 1:51,
                    y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_male"))), 
                                     Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
      geom_ribbon(aes(x = 1:51,ymin = MeanCI_male$CI$lower, 
                      ymax =MeanCI_male$CI$upper,fill = "Male"),
                      alpha = 0.2)+
      xlab("Age")+ylab("Log Hazard Function")+scale_fill_manual(values = c("#77C3EC"))+labs(fill = "")
    mean_curve_male_plot %<>% ggplotly %>% config(displayModeBar = FALSE)
  })
 # Female Mean Curve and Ribbon #### 
  output$female_mean_curve <- renderPlotly({
    mean_curve_female_plot <- ggplot() +  
      geom_line(aes(x  = 1:51,
                    y = GetMeanCurve(Ly = (df_nested_hazard$data %>% map(~.x %>% extract2("hazard_female"))), 
                                     Lt = (df_nested_hazard$data %>% map(~.x %>% extract2("age"))))$mu))+
      geom_ribbon(aes(x = 1:51,ymin = MeanCI_female$CI$lower, 
                      ymax =MeanCI_female$CI$upper,fill = "Female"),
                      alpha = 0.2)+
      xlab("Age")+ylab("Log Hazard Function")+scale_fill_manual(values = c("#FF64B8"))+labs(fill = "")
    mean_curve_female_plot %<>% ggplotly %>% config(displayModeBar = FALSE)  
  })
  
  # Male Eigenfunctions Plot #### 
  output$male_eigenfunctions <- renderPlotly({
    eigenfunctions_male_plot <- ggplot()+geom_line(aes(x = 1:51,y = fpca_male$phi[,1],color = "Male",linetype = "1"))+
      geom_line(aes(x = 1:51,y = fpca_male$phi[,2],color = "Male",linetype = "2"))+
      geom_line(aes(x = 1:51,y = fpca_male$phi[,3],color = "Male",linetype = "3"))+
      scale_color_manual(values = c("#77C3EC"))+xlab("Age")+ylab("Male Eigenfunctions")+labs(color = "",linetype = "")
    eigenfunctions_male_plot %<>%  ggplotly %>% config(displayModeBar = FALSE)
  })
  # Female Eigenfunctions Plot #### 
  output$female_eigenfunctions <- renderPlotly({
    eigenfunctions_female_plot <- ggplot()+geom_line(aes(x = 1:51,y = fpca_female$phi[,1],color = "Female",linetype = "1"))+
      geom_line(aes(x = 1:51,y = fpca_female$phi[,2],color = "Female",linetype = "2"))+
      geom_line(aes(x = 1:51,y = fpca_female$phi[,3],color = "Female",linetype = "3"))+
      scale_color_manual(values = c("#FF64B8"))+xlab("Age")+ylab("Female EigenFunctions")+labs(color = "",linetype = "")
    eigenfunctions_female_plot %<>%  ggplotly %>% config(displayModeBar = FALSE)
  })
  
  
 #### Add Plot Button #### 
  observeEvent(input$add_plot,{
    #: Keep Track of Namespaces ####
    i <- sprintf('%04d',input$add_plot)
    id <-sprintf('plot_select_input%s',i) 
    #: Call Namespace Ui #### 
    insertUI(
      selector = '#add_plot',
      where = "beforeBegin",
      ui = 
        column(6,
               fly_plot_ui(id)
        )
      
      
    )
    #: Call Namespace Server ####
    callModule(fly_plot_module,id)
    #: Change Plot Functions Based on Select Input Value #### 
    observeEvent(input[[paste0(id,'-gender')]],{
      if(length(input[[paste0(id,'-gender')]]) != 0){
        if(input[[paste0(id,'-gender')]] == "Male"){
          plot_reactive <- reactive({
            gg <- ggplot()
            gg <- gg + df_nested_hazard$data %>% future_imap(~geom_line(mapping = aes(x = age, y = hazard_male),
                                                                        data = df_nested_hazard$data[[.y]]))
          })
        } 
        else{  
          plot_reactive <- reactive({
            gg <- ggplot()
            gg <- gg + df_nested_hazard$data %>% future_imap(~geom_line(mapping = aes(x = age, y = hazard_female),
                                                                        data = df_nested_hazard$data[[.y]]))
          })
        }
      }
    })
    #: Remove UI and Inputs ####
    observeEvent(input[[paste0(id,'-delete_button')]],{
      removeUI(selector = sprintf('#%s',id))
      remove_shiny_inputs(id,input)
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
