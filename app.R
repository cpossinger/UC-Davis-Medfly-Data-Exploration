library(tidyverse)
library(magrittr)
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(fdapace)
library(ggpubr)

#load("medfly_data.RData")

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
        
        chosen_function <- case_when(
          input$function_type == "Log-Hazard"~ "log_hazard_male",
          input$function_type == "Log-Hazard Derivative"~ "deriv_male",
          input$function_type == "Hazard"~ "hazard_male",
          input$function_type == "Survival"~ "survival_male"
        )
        chosen_function <- sym(chosen_function) 
        
        chosen_function_mean <- case_when(
          input$function_type == "Log-Hazard"~ log_hazard_mean_male$mu, 
          input$function_type == "Log-Hazard Derivative"~ log_hazard_male_deriv_mean$mu, 
          input$function_type == "Hazard"~ hazard_mean_male$mu, 
          input$function_type == "Survival"~ surv_mean_male$mu
        )
        
        
        if(selected_cohort == "All Cohorts"){
          gg <- ggplot()
          gg <- gg + df_nested_hazard %>% extract2("data") %>% 
            imap(~geom_line(mapping = aes(x = age, 
                                          y = !! chosen_function,
                                          color = "Male",
                                          text = paste0("Cage: ",df_nested_hazard %>% 
                                                          extract2("Cage") %>% 
                                                          extract2(.y),"<br>", "Cohort: ",
                                                          df_nested_hazard %>%
                                                          extract2("Cohort") %>% 
                                                          extract2(.y))),data = .x),
                 .progress = TRUE) 
          
          gg <- gg + scale_color_manual(values = rep("lightblue", df_nested_hazard %>%  extract2("data") %>% length))+ 
                     xlab("Age")+ylab(input$function_type)+
                     geom_line(aes(x  = 1:(df_nested_hazard$data[[1]]$age %>% length),
                                   y = chosen_function_mean))+labs(color = "")
          
          gg %<>% ggplotly(tooltip = "text") %>% config(displayModeBar = FALSE) 
        }
        
        else{
          gg <- ggplot()
          filtered_data <- df_nested_hazard %>% filter(eval(parse(text = paste0(filter_pastes,collapse = " | ")))) 
          
          gg <- gg + filtered_data %>% extract2("data") %>%
            imap(~geom_line(mapping = aes(x = age, 
                                          y = !! chosen_function,
                                          color = "Male",
                                          text = paste0("Cage: ",
                                                        filtered_data %>% 
                                                          extract2("Cage") %>%  
                                                          extract2(.y),"<br>",
                                                          "Cohort: ",filtered_data %>% 
                                                          extract2("Cohort") %>% 
                                                          extract2(.y))),data = .x),
                 .progress = TRUE) 
          
          gg <- gg + scale_color_manual(values = rep("lightblue",
                                                     filtered_data %>% 
                                                       extract2("data") %>% length))+
            xlab("Age")+ylab(input$function_type)+
            geom_line(aes(x  = 1:(df_nested_hazard$data[[1]]$age %>% length),
                          y = chosen_function_mean))+labs(color = "")
          
          gg %<>% ggplotly(tooltip = "text")%>% 
            config(displayModeBar = FALSE)
        }
      }
      
      else{
        
        chosen_function <- case_when(
          input$function_type == "Log-Hazard"~ "log_hazard_female",
          input$function_type == "Log-Hazard Derivative"~ "deriv_female",
          input$function_type == "Hazard"~ "hazard_female",
          input$function_type == "Survival"~ "survival_female"
        )
        chosen_function <- sym(chosen_function) 
        
        chosen_function_mean <- case_when(
          input$function_type == "Log-Hazard"~ log_hazard_mean_female$mu, 
          input$function_type == "Log-Hazard Derivative"~ log_hazard_female_deriv_mean$mu, 
          input$function_type == "Hazard"~ hazard_mean_female$mu, 
          input$function_type == "Survival"~ surv_mean_female$mu
        )
        
        if(selected_cohort == "All Cohorts"){
          gg <- ggplot()
          gg <- gg + df_nested_hazard %>% extract2("data") %>% 
            imap(~geom_line(mapping = aes(x = age, 
                                          y = !! chosen_function,
                                          color = "Female",
                                          text = paste0("Cage: ",df_nested_hazard %>% 
                                                          extract2("Cage") %>% 
                                                          extract2(.y),"<br>",
                                                        "Cohort: ",df_nested_hazard %>% 
                                                          extract2("Cohort") %>% 
                                                          extract2(.y))),
                            data = .x),
                 .progress = TRUE) 
          
          gg <- gg + 
            scale_color_manual(values = rep("pink", df_nested_hazard %>%  extract2("data") %>% length))+ 
            xlab("Age")+
            ylab(input$function_type)+
            geom_line(aes(x  = 1:(df_nested_hazard$data[[1]]$age %>% length),
                          y = chosen_function_mean))+
            labs(color = "") 
          
          gg %<>% ggplotly(tooltip = "text") %>% config(displayModeBar = FALSE) 
        }
        else{ 
          gg <- ggplot()
          filtered_data <- df_nested_hazard %>%  filter(eval(parse(text = paste0(filter_pastes,collapse = " | ")))) 
          
          gg <- gg + filtered_data %>% extract2("data") %>% 
            imap(~geom_line(mapping = aes(x = age, 
                                          y =  !! chosen_function,
                                          color = "Female",
                                          text = paste0("Cage: ",filtered_data %>%
                                                          extract2("Cage") %>% 
                                                          extract2(.y),"<br>", 
                                                        "Cohort: ",filtered_data %>%  
                                                          extract2("Cohort") %>% 
                                                          extract2(.y))),
                            data = .x),
                 .progress = TRUE)
          
          gg <- gg + scale_color_manual(values = c(rep("pink",
                                                       filtered_data %>% 
                                                         extract2("data") %>% 
                                                         length)))+ 
            xlab("Age")+
            ylab(input$function_type)+
            geom_line(aes(x  = 1:(df_nested_hazard$data[[1]]$age %>% length),
                          y = chosen_function_mean)) 
          
          gg %<>% ggplotly(tooltip = "text") %>% config(displayModeBar = FALSE) 
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
               fluidPage(
                 box(height = "100%",width = "100%",
                     plotlyOutput(ns("plot"),height = "100%",width = "100%"),       
                     radioButtons(ns("function_type"), "Choose Function Type", 
                                  choices = c("Log-Hazard","Survival","Log-Hazard Derivative","Hazard"),inline = TRUE),
                     selectInput(ns("gender"),label = "Gender",choices = c("Male","Female")),
                     selectInput(ns("cohort"),label = "Cohort",choices = c("All Cohorts",df_nested_hazard$Cohort),multiple = TRUE,selected = "All Cohorts"),
                     actionButton(ns('delete_button'),"Remove Plot",icon = icon('times'),style = 'float: right')
                 )
               )
             )
           )
         }
       )
     }





# UI ####
ui <- dashboardPage(
  dashboardHeader(title = "Medfly Analysis"),
  dashboardSidebar(
    #: Sidebar ####
    sidebarMenu(
      menuItem("Functions",tabName = "main_plots",icon = icon("skull-crossbones")),
      menuItem("FPCA",tabName = "eigenfunction",icon = icon("hat-wizard")),
      menuItem("FCReg",tabName = "fcreg",icon = icon("ghost"))
      
    )),
  #: Body ####
  dashboardBody(
    tabItems(
      tabItem(tabName = "eigenfunction",
              fluidRow(
                tabBox(width = 12, height = "700px", 
                       selected = "Eigenfunctions",
                       tabPanel("Eigenfunctions",
                                fluidRow(
                                  column(4,offset = 2,
                                         selectInput("select_eigenfunction","Select Eigenfunction",c(1,2,3), multiple = TRUE,selected = 1)),
                                  column(4,offset = 2,
                                         radioButtons("eigen_deriv_button", "Select Function Type", c("Log-Hazard", "Log-Hazard Derivative")))),
                                plotlyOutput("eigenfunctions", height = "550px")),
                       tabPanel("Mean Curves", 
                                fluidRow(
                                  column(4, offset = 2,
                                         selectInput("select_mean","Select Mean Curve",c("Male","Female"), multiple = TRUE,selected = "Female")),
                                  column(4, offset = 2, 
                                         radioButtons("mean_deriv_button", "Select Function Type", c("Log-Hazard", "Log-Hazard Derivative")))),
                                plotlyOutput("mean_curves", height = "550px")),
                       tabPanel("Box Plots", 
                      plotlyOutput("boxplots", height = "550px")),
                       tabPanel("Modes of Variation",
                                column(2, offset = 3, 
                                       radioButtons("mov_male_female", "Choose a Gender: ", c("Male" = "male", "Female" = "female"),width = "100%")),
                                column(3, 
                                       numericInput("mov_k_modes", "Select the k-th Mode of Variation", 1,1,8,1,width = '75%')),
                                
                                column(2,
                                       radioButtons("mov_hazard_deriv", "Choose Type of Function: ", 
                                                    c("Log-Hazard" = "log_hazard", "Log-Hazard-Derivative" = "log_hazard_deriv"),width = "100%")),
                                
                                plotOutput("ModesofVariation", height = "550px")
                       ),
                       tabPanel("Fraction of Variance",
                                column(4,offset = 2,
                                       radioButtons("fve_male_female", "Choose a Gender for the Y-axis Scores: ", c("Male" = "male", "Female" = "female"))),
                                column(4,offset = 2,
                                       radioButtons("fve_hazard_deriv", "Choose Type of Function: ", c("Log-Hazard" = "log_hazard",
                                                                                                       "Log-Hazard-Derivative" = "log_hazard_deriv"))),
                                dataTableOutput("variation_table")       
                       ),
                       tabPanel("Scores",
                                fluidRow(
                                  column(2, offset = 2,
                                         tags$h1("X-Axis")),
                                  column(2,offset = 4,
                                         tags$h1("Y-Axis"))),
                                fluidRow(
                                  column(1, offset = 1,
                                         radioButtons("scores_male_female_1", "Choose a Gender: ", c("Male" = "male", "Female" = "female"))),
                                  column(2,
                                         numericInput("scores_modes_1", "Select Eigenfunction 1-8", 1,1,8,1)),
                                  
                                  column(2,
                                         radioButtons("scores_hazard_deriv_1", "Choose Type of Function: ", 
                                                      c("Log-Hazard" = "log_hazard", "Log-Hazard-Derivative" = "log_hazard_deriv"))),
                                  
                                  
                                  column(1, offset = 1, 
                                         radioButtons("scores_male_female_2", "Choose a Gender: ", c("Male" = "male", "Female" = "female"))),
                                  column(2,
                                         numericInput("scores_modes_2", "Select Eigenfunction 1-8", 1,1,8,1)),
                                  
                                  column(2, 
                                         radioButtons("scores_hazard_deriv_2", "Choose Type of Function: ",
                                                      c("Log-Hazard" = "log_hazard", "Log-Hazard-Derivative" = "log_hazard_deriv"))),
                                ),
                                  plotlyOutput("scores", width = "100%", height = "100%")     
                       )
                ),
              )
      ),
      tabItem(tabName = "main_plots", 
              actionButton("add_plot",label = "Add Plot",icon = icon('plus'))),
      tabItem(tabName = "main_derivatives", 
              actionButton("add_plot_deriv",label = "Add Plot",icon = icon('plus'))),
      tabItem(tabName = "fcreg", 
              selectInput("model_select","Select Model",c("Log Hazard Male(t) ~ Beta 0(t) + Beta 1(t)(Survival Male) + Beta 2(t)(Survival Female) + Z(t)" = 
                                                            "fcReg_male_full",
                                                          
                                                          "Log Hazard Female(t) ~ Beta 0(t) + Beta 1(t)(Survival Male) + Beta 2(t)(Survival Female) + Z(t)" = 
                                                            "fcReg_female_full",
                                                          
                                                          "Derivative Log Hazard Male(t) - Mean Derivative Log Hazard Male(t) ~ 
                                                          Beta 0(t) + Beta 1(t)(Log Hazard Male - Mean Log Hazard Male) + 
                                                          Beta 2(t)(Log Hazard Female - Mean Log Hazard Female) + Z(t)" = 
                                                            "fcReg_male_deriv_full",
                                                          
                                                          #"Log Hazard Male(t) ~ Beta 0(t) + Beta 1(t)(Survival Male) + Z(t)",
                                                          
                                                          #"Log Hazard Female(t) ~ Beta 0(t) + Beta 1(t)(Survival Male) + Z(t)",
                                                          
                                                          "Derivative Log Hazard Male(t) - Mean Derivative Log Hazard Male(t) ~
                                                          Beta 0(t) + Beta 1(t)(Log Hazard Female - Mean Log Hazard Female) + Z(t)" = 
                                                            "fcReg_male_deriv",
                                                          
                                                          "Derivative Log Hazard Female(t) - Mean Derivative Log Hazard Female(t) ~
                                                          Beta 0(t) + Beta 1(t)(Log Hazard Male - Mean Log Hazard Male) + Z(t)" = 
                                                            "fcReg_female_deriv",
                                                          
                                                          "Derivative Log Hazard Female(t) - Mean Derivative Log Hazard Female(t) ~ 
                                                          Beta 0(t) + Beta 1(t)(Log Hazard Male - Mean Log Hazard Male) + 
                                                          Beta 2(t)(Log Hazard Female - Mean Log Hazard Female) + Z(t)" = 
                                                            "fcReg_female_deriv_full"
                                                          
              ), width = "100%"),
              tabBox(width = "100%", height = "650px",
                tabPanel("Model Effects",
              plotOutput("beta_plot", height = "600px")),
              tabPanel("Y(t) & X(t)",
              plotOutput("Yt_plot", height = "300px"),
              plotOutput("Xt_plot",height = "300px"),
                       )
              )
              
              )
      
    )
  )
)
# Server ####
server <- function(input, output,session) {
  
  ### Male Log Hazard Function Boxplot and Mean Curve ####
  output$boxplots <- renderPlotly({
    
    boxplots <- ggplot()
    boxplots <- boxplots + 
      box_plot_data_male %>% 
      map2(multiples_of_five,~geom_boxplot(data = box_plot_data_male %>% 
                                             as.data.frame(),aes(x = .y,y = .x,color = "Male")))+
      
      
      box_plot_data_female %>% 
      map2(multiples_of_five,~geom_boxplot(data = box_plot_data_female %>% 
                                             as.data.frame(),aes(x = .y,y = .x,color = "Female")))+
      scale_color_manual(values = c("pink","lightblue"))+
     
      xlab("Age")+ylab("Log Transformed Hazard Function")+labs(color = "")
    
    boxplots %<>% ggplotly %>% config(displayModeBar = FALSE)
    boxplots
  })
  
  # Male Mean Curve and Ribbon #### 
  output$mean_curves <- renderPlotly({
    
    mean_curves <- ggplot() 
    if(input$mean_deriv_button == "Log-Hazard"){
      male_selected_mean <- log_hazard_mean_male
      female_selected_mean <- log_hazard_mean_female
      
      male_selected_CI <- MeanCI_male 
      female_selected_CI <- MeanCI_female 
    }else{
      male_selected_mean <- log_hazard_male_deriv_mean 
      female_selected_mean <- log_hazard_female_deriv_mean
      
      male_selected_CI <- MeanCI_male_deriv 
      female_selected_CI <- MeanCI_female_deriv 
    }
    
    print(input$mean_deriv_button)
    
    
    if("Male" %in% input$select_mean){
      mean_curves <- mean_curves + geom_line(aes(x  = 1:(df_nested_hazard$data[[1]]$age %>% length),y = male_selected_mean$mu))+
        geom_ribbon(aes(x = 1:(df_nested_hazard$data[[1]]$age %>% length),ymin = male_selected_CI$CI$lower, 
                        ymax =male_selected_CI$CI$upper,fill = "Male"),
                    alpha = 0.2)
    }
    if("Female" %in% input$select_mean){
      mean_curves <- mean_curves + geom_line(aes(x  = 1:(df_nested_hazard$data[[1]]$age %>% length),
                                                 y = female_selected_mean$mu))+
        geom_ribbon(aes(x = 1:(df_nested_hazard$data[[1]]$age%>% length),ymin = female_selected_CI$CI$lower, 
                        ymax =female_selected_CI$CI$upper,fill = "Female"),
                    alpha = 0.2) 
    }
    
    
    named_colors <- c("Male" = "#77C3EC", "Female" =  "#FF64B8")
    mean_curves <- mean_curves + xlab("Age")+ylab(paste0(input$mean_deriv_button," ", "Mean"))+labs(fill = "")+scale_fill_manual(values = named_colors )
    mean_curves %<>% ggplotly %>% config(displayModeBar = FALSE)
    mean_curves
  })
  
  # Male Eigenfunctions Plot #### 
  output$eigenfunctions <- renderPlotly({
    if(input$eigen_deriv_button == "Log-Hazard"){
      fpca_male_obj <- fpca_male
      fpca_female_obj <- fpca_female
    }
    else{
      fpca_male_obj <- fpca_male_deriv
      fpca_female_obj <- fpca_female_deriv
    }
    eigenfunctions <- ggplot()
    if("1" %in% input$select_eigenfunction) {
      eigenfunctions <- eigenfunctions + geom_line(aes(x = 1: (df_nested_hazard$data[[1]]$age %>% length),y = fpca_male_obj$phi[,1],color = "Male",linetype = "1"))+
        geom_line(aes(x = 1:(df_nested_hazard$data[[1]]$age %>% length),y = fpca_female_obj$phi[,1],color = "Female",linetype = "1"))
    }
    if("2" %in% input$select_eigenfunction){
      eigenfunctions <- eigenfunctions + geom_line(aes(x = 1:(df_nested_hazard$data[[1]]$age %>% length),y = fpca_male_obj$phi[,2],color = "Male",linetype = "2"))+
        geom_line(aes(x = 1:(df_nested_hazard$data[[1]]$age %>% length),y = fpca_female_obj$phi[,2],color = "Female",linetype = "2"))
    }
    if("3" %in% input$select_eigenfunction){
      eigenfunctions <- eigenfunctions + geom_line(aes(x = 1:(df_nested_hazard$data[[1]]$age %>% length),y = fpca_male_obj$phi[,3],color = "Male",linetype = "3"))+
        geom_line(aes(x = 1:(df_nested_hazard$data[[1]]$age %>% length),y = fpca_female_obj$phi[,3],color = "Female",linetype = "3"))
    }
    eigenfunctions <- eigenfunctions + scale_color_manual(values = c("#FF64B8","#77C3EC"))+xlab("Age")+ylab("Eigenfunctions")+
      labs(color = "",linetype = "")
    eigenfunctions %<>%  ggplotly %>% config(displayModeBar = FALSE)
    eigenfunctions
  })
  
  # Modes of Variation Plot ####
  output$ModesofVariation <- renderPlot({
    if(input$mov_male_female == "male"){
      print("male")
      if(input$mov_hazard_deriv == "log_hazard") {
        print("log_hazard")
        fpca_obj <- fpca_male
      }else{
       print("deriv") 
        fpca_obj <- fpca_male_deriv
      }
    }
    
    else{
      print("female")
      if(input$mov_hazard_deriv == "log_hazard"){
        print("log_hazard")
        fpca_obj <- fpca_female
      }else{
       print("deriv") 
        fpca_obj <- fpca_female_deriv
      }
    }
    
    print(input$mov_k_modes)
    
    title <- paste0(input$mov_male_female %>% str_to_title," ",input$mov_hazard_deriv %>% str_replace_all("_"," ")
                    %>% str_replace("deriv","Derivative") %>% str_to_title," ",input$mov_k_modes,case_when(input$mov_k_modes == 1 ~ "st ",
                                                                                                           input$mov_k_modes == 2 ~ "nd ",
                                                                                                           input$mov_k_modes == 3 ~ "rd ",
                                                                                                           TRUE ~ "th "), "Mode of Variation")
    CreateModeOfVarPlot(fpca_obj, input$mov_k_modes,main=title, xlab='Days', ylab='') 
  })
  
  output$scores <- renderPlotly({
    
    if(input$scores_male_female_1 == "male"){
      print("male")
      if(input$scores_hazard_deriv_1 == "log_hazard"){
        print("log-hazard")
        scores_1 <- fpca_male_scores
      }else{
        print("deriv")
        scores_1 <- fpca_male_deriv_scores
      }
    }
    else{
      print("female")
      if(input$scores_hazard_deriv_1 == "log_hazard"){
        print("log-hazard")
        scores_1 <- fpca_female_scores
      }else{
        print("deriv")
        scores_1 <- fpca_female_deriv_scores
      } 
    }
    
    
    if(input$scores_male_female_2 == "male"){
      if(input$scores_hazard_deriv_2 == "log_hazard"){
        scores_2 <- fpca_male_scores
      }else{
        scores_2 <- fpca_male_deriv_scores
        
      }
    }
    else{
      if(input$scores_hazard_deriv_2 == "log_hazard"){
        scores_2 <- fpca_female_scores
      }else{
        scores_2 <- fpca_female_deriv_scores
      } 
    }
    
    
    scatter_list <- data.frame(x = scores_1$scores[,input$scores_modes_1],y = scores_2$scores[,input$scores_modes_2])
    scatterplot <- ggplot(scatter_list,aes(x,y))+geom_point()+xlab(paste0(input$scores_male_female_1 %>% str_to_title," ", 
                                                                          input$scores_hazard_deriv_1 %>% str_replace_all("_"," ") %>% 
                                                                            str_replace("deriv","derivative") %>% str_to_title(), " ",
                                                                          "Scores for the ", input$scores_modes_1, 
                                                                          case_when(input$scores_modes_1 == 1 ~ "st ",
                                                                                    input$scores_modes_1 == 2 ~ "nd ",
                                                                                    input$scores_modes_1 == 3 ~ "rd ",
                                                                                    TRUE ~ "th "),
                                                                          "Eigenfunction"))+
      ylab(paste0(input$scores_male_female_2 %>% str_to_title," ", input$scores_hazard_deriv_2 %>% str_replace_all("_"," ") %>% 
                    str_replace("deriv","derivative") %>% str_to_title(), " ",
                  "Scores for the ", input$scores_modes_2, 
                  case_when(input$scores_modes_2 == 1 ~ "st ",
                            input$scores_modes_2 == 2 ~ "nd ",
                            input$scores_modes_2 == 3 ~ "rd ",
                            TRUE ~ "th "),
                  "Eigenfunction"))
    scatterplot %<>% ggplotly %>% config(displayModeBar = FALSE)
    scatterplot
    
  })
  
  output$variation_table <- renderDataTable({
    if(input$fve_male_female == "male"){
      if(input$fve_hazard_deriv == "log_hazard"){
        fpca_obj <- fpca_male
      }else{
        fpca_obj <- fpca_male_deriv
      }
    }else{
      if(input$fve_hazard_deriv == "log_hazard"){
        fpca_obj <- fpca_female
      }else{
        fpca_obj <- fpca_female_deriv
      }
      
    }
    
    FVE <- sapply(1:(fpca_obj$cumFVE %>% length),function(x){if(x != 1){fpca_obj$cumFVE[x] - fpca_obj$cumFVE[x-1]}else{fpca_obj$cumFVE[x]}})
    
    data.frame(Eigenfunction = fpca_obj$cumFVE %>% length %>% seq, cumFVE = fpca_obj$cumFVE %>% round(3),FVE = FVE %>% round(3))
  },options = list(dom = "t"))
  
  
  
  # FCR Plot #### 
  
  fcReg_male_full_plot <- reactive({
    beta_0 <- ggplot()+
      geom_line(aes(x = time_days[[1]][15:30],y = fcReg_male_full$beta0))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")
    
    
    beta_1 <- ggplot()+
      geom_line(aes(x = time_days[[1]][15:30],y = fcReg_male_full$beta["beta_1",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Survival Male")
    
    
    
    beta_2 <- ggplot()+
      geom_line(aes(x = time_days[[1]][15:30],y = fcReg_male_full$beta["beta_2",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Survival Female")
    
    
    
    
    return(ggarrange(beta_0,beta_1,beta_2,labels = c("Beta 0(t)", "Beta 1(t)","Beta 2(t)"),hjust = -5.5,vjust = 1.80)) 
  })
  
  # fcReg_male_plot <- reactive({
  #   beta_0 <- ggplot()+geom_line(aes(x = time_days[[1]][15:30],y = fcReg_male$beta0))+
  #     geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")
  #   
  #   beta_1 <- ggplot()+geom_line(aes(x = time_days[[1]][15:30],y = fcReg_male$beta["beta_1",]))+
  #     geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Survival Male")
  #   
  #   
  #   
  #   return(ggarrange(beta_0,beta_1,labels = c("Beta 0(t)", "Beta 1(t)"),hjust = -5.5,vjust = 1.80)) 
  # })
  
  fcReg_female_full_plot <- reactive({
    beta_0 <- ggplot()+geom_line(aes(x = time_days[[1]][15:30],y = fcReg_female_full$beta0))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")
    
    beta_1 <- ggplot()+geom_line(aes(x = time_days[[1]][15:30],y = fcReg_female_full$beta["beta_1",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Survival Male")
    
    beta_2 <- ggplot()+geom_line(aes(x = time_days[[1]][15:30],y = fcReg_female_full$beta["beta_2",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Survival Female")
    
    
    return(ggarrange(beta_0,beta_1,beta_2,labels = c("Beta 0(t)", "Beta 1(t)","Beta 2(t)"),hjust = -5.5,vjust = 1.80)) 
  })
  
  # fcReg_female_plot <- reactive({
  #   beta_0 <- ggplot()+geom_line(aes(x = time_days[[1]][15:30],y = fcReg_female$beta0))+
  #     geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")
  #   
  #   beta_1 <- ggplot()+geom_line(aes(x = time_days[[1]][15:30],y = fcReg_female$beta["beta_1",]))+
  #     geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Survival Male")
  #   
  #   
  #   return(ggarrange(beta_0,beta_1,labels = c("Beta 0(t)", "Beta 1(t)"),hjust = -5.5,vjust = 1.80)) 
  # })
  
  fcReg_male_deriv_full_plot <- reactive({
    beta_0 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_male_deriv_full$beta0))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")
    
    
    beta_1 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_male_deriv_full$beta["beta_1",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Log Hazard Male - Mean Log Hazard Male")
    
    beta_2 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_male_deriv_full$beta["beta_2",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Log Hazard Female - Mean Log Hazard Female")
    
    
    return(ggarrange(beta_0,beta_1,beta_2,labels = c("Beta 0(t)", "Beta 1(t)","Beta 2(t)"),hjust = -5.5,vjust = 1.80)) 
  })
  
  fcReg_female_deriv_full_plot <- reactive({
    beta_0 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_female_deriv_full$beta0))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")
    
    beta_1 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_female_deriv_full$beta["beta_1",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Log Hazard Male - Mean Log Hazard Male")
    
    beta_2 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_female_deriv_full$beta["beta_2",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Log Hazard Female - Mean Log Hazard Female")
    
    
    return(ggarrange(beta_0,beta_1,beta_2,labels = c("Beta 0(t)", "Beta 1(t)","Beta 2(t)"),hjust = -5.5,vjust = 1.80)) 
  }) 
  
  fcReg_male_deriv_plot <- reactive({
    beta_0 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_male_deriv$beta0))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")

    beta_1 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_male_deriv$beta["beta_1",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Log Hazard Female - Mean Log Hazard Female")


    return(ggarrange(beta_0,beta_1,labels = c("Beta 0(t)", "Beta 1(t)"),hjust = -5.5,vjust = 1.80))
  })
  
  fcReg_female_deriv_plot <- reactive({
    beta_0 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_female_deriv$beta0))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Beta 0(t)")

    beta_1 <- ggplot()+geom_line(aes(x = time_days[[1]][5:50],y = fcReg_female_deriv$beta["beta_1",]))+
      geom_hline(yintercept = 0,linetype = "dashed",color = "red")+xlab("Days")+ylab("Log Hazard Male - Mean Log Hazard Male")


    return(ggarrange(beta_0,beta_1,labels = c("Beta 0(t)", "Beta 1(t)"),hjust = -5.5,vjust = 1.80))
  })
  output$beta_plot <- renderPlot({
    if(input$model_select == "fcReg_male_full"){
      fcReg_male_full_plot() 
    }
    else if(input$model_select == "fcReg_female_full"){
      fcReg_female_full_plot() 
    }
    else if(input$model_select == "fcReg_male_deriv_full"){ 
      fcReg_male_deriv_full_plot() 
    }
    # else if(input$model_select ==  "Log Hazard Male(t) ~ Beta 0(t) + Beta 1(t)(Survival Male) + Z(t)"){
    #   fcReg_male_plot()   
    # }
    # else if(input$model_select ==  "Log Hazard Female(t) ~ Beta 0(t) + Beta 1(t)(Survival Male) + Z(t)"){
    #   fcReg_female_plot()    
    # }
    else if(input$model_select == "fcReg_male_deriv"){
      fcReg_male_deriv_plot()
    }
    else if(input$model_select == "fcReg_female_deriv"){
      fcReg_female_deriv_plot()
    }
    else if(input$model_select == "fcReg_female_deriv_full"){
      fcReg_female_deriv_full_plot() 
    }
  }) 
  
  
  output$Yt_plot <- renderPlot({
    if(input$model_select == "fcReg_male_full"){
      Yt <- Yt_male_full
      days <- time_days_surv[[1]]
    }
    else if(input$model_select == "fcReg_female_full"){
      Yt <- Yt_female_full 
      days <- time_days_surv[[1]]
    }
    else if(input$model_select == "fcReg_male_deriv_full"){ 
      Yt <- Yt_male_deriv_full 
      days <- time_days_deriv[[1]]
    }
    else if(input$model_select == "fcReg_male_deriv"){ 
      Yt <- Yt_male_deriv 
      days <- time_days_deriv[[1]]
    }
    else if(input$model_select == "fcReg_female_deriv"){ 
      Yt <- Yt_female_deriv 
      days <- time_days_deriv[[1]]
    }
    else if(input$model_select == "fcReg_female_deriv_full"){
      Yt <- Yt_female_deriv_full
      days <- time_days_deriv[[1]]
    }
    
    
    Yt_plot <- ggplot()
    Yt_plot <- Yt_plot + Yt %>% map(~geom_line(aes(x = days, y = .x, color = days)))+
      scale_color_gradient(low = "blue",high = "orange",name = "Days")+xlab("Days")+ylab("Y(t)")+labs(title = "Y(t)")+theme(
        plot.title = element_text(hjust = 0.5,size = 14,face = "bold")
      )
    
return(Yt_plot)
    
  })
  
  output$Xt_plot <- renderPlot({
    if(input$model_select == "fcReg_male_full"){
      Xt <- Xt_male_full
      days <- time_days_surv[[1]]
    }
    else if(input$model_select == "fcReg_female_full"){
      Xt <- Xt_female_full 
      days <- time_days_surv[[1]]
    }
    else if(input$model_select == "fcReg_male_deriv_full"){ 
      Xt <- Xt_male_deriv_full 
      days <- time_days_deriv[[1]]
    }
    
    else if(input$model_select == "fcReg_male_deriv"){ 
      Xt <- Xt_male_deriv 
      days <- time_days_deriv[[1]]
    }
    else if(input$model_select == "fcReg_female_deriv"){ 
      Xt <- Xt_female_deriv
      days <- time_days_deriv[[1]]
    }
    else if(input$model_select == "fcReg_female_deriv_full"){
      
      Xt <- Xt_female_deriv_full
      days <- time_days_deriv[[1]]
    }
    
    
    Xt_plot <- ggplot()
    Xt_plot <- Xt_plot + Xt %>% map(~geom_line(aes(x = days, y = .x, color = days)))+
      scale_color_gradient(low = "blue",high = "orange",name = "Days")+ xlab("Days")+ ylab("X(t)")+ labs(title = "X(t)")+theme(
        plot.title = element_text(hjust = 0.5,size = 14,face = "bold")
      )
    
return(Xt_plot)
    
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
            gg <- gg + df_nested_hazard$data %>% imap(~geom_line(mapping = aes(x = age, y = hazard_male),
                                                                 data = df_nested_hazard$data[[.y]]))
          })
        } 
        else{  
          plot_reactive <- reactive({
            gg <- ggplot()
            gg <- gg + df_nested_hazard$data %>% imap(~geom_line(mapping = aes(x = age, y = hazard_female),
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
