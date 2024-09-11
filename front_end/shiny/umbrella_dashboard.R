
library(shiny)
library(shinydashboard)
library(data.table)
library(odbc)
library(stats)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(fmsb)
library(tidyr)
library(DT)
library(DBI)

# load config file
source('/root/umbrella/config.R')

obtain_data <- function() {

  con <- dbConnect(odbc(), 
                  Driver = umbrella_db_driver, 
                  Server = umbrella_db_server,  
                  Database = umbrella_db_database, 
                  UID = umbrella_db_uid, 
                  PWD = umbrella_db_password, 
                  Port = umbrella_db_port)

  data <- dbGetQuery(con, "SELECT * FROM dac_usr.UMBRELLA_SCORES")
  data$UMBRELLA_ID <- gsub("UMBRELLA_000", "", data$UMBRELLA_ID)   # removes prefix so that only the code is used
  colnames(data) <- c("patientcode", "group", "period", "V1")
  data$V1 <- round(data$"V1")

  dbDisconnect(con)


  return(data)
}

final_df_pat <- obtain_data()


get_db_connection <- function() {
  # -- gets database connection -- #

  con <- dbConnect(odbc(), 
                  Driver = patient_db_driver, 
                  Server = patient_db_server,  
                  Database = patient_db_database, 
                  UID = patient_db_uid,
                  PWD = patient_db_password, 
                  Port = patient_db_port)
  return(con)

}

obtain_worklist_arts <- function(con, filter_type, filter_value, date) {
  # -- fetches the patient appointment of the chosen arts on the given dates from the db -- #
  # select column to apply the filtering on
  if (filter_type == "ARTS"){
    filter_type <- "A.[ARTSCODE]"
  } else {
    filter_type <- "AFS.[SUBAGENDA]"
  }

  query <-  sprintf("SELECT PP.PATIENTNR, PP.ACHTERNAAM, PP.VOORLETTER, PP.GEBDAT, PP.GESLACHT, AFS.DATUM, AFS.TIJD, RPP.STARTDATEPLAN, DL.OMSCHRIJV, DATEDIFF(year, PP.GEBDAT, GETDATE()) AS LEEFTIJD FROM [AGENDA_AFSPRAAK] AS AFS LEFT JOIN [RP_PLANOBJECT] AS RPP ON afs.AFSPRAAKNR = RPP.LINKEDOBJECTID AND RPP.LINKEDOBJECTCLASSID = 'AGENDA_AFSPRAAK' LEFT JOIN [CSZISLIB_ARTS] AS A ON A.[ARTSCODE] = AFS.[UITVOERDER] LEFT JOIN [AGENDA_AGENDA] AS AG ON AFS.[AGENDA] = AG.[AGENDA] LEFT JOIN [AGENDA_SUBAGEND] AS SA ON AFS.[AGENDA] = SA.[AGENDA] AND AFS.[SUBAGENDA] = SA.[SUBAGENDA] LEFT JOIN [DOSSIER_LIJSTOBJ] AS DL ON AFS.[PATIENTNR] =DL.PATIENTNR LEFT JOIN [PATIENT_PATIENT] AS PP ON AFS.[PATIENTNR] = PP.PATIENTNR WHERE %s = '%s' AND RPP.STARTDATEPLAN = '%s' AND DL.OMSCHRIJV  LIKE '%%UMBRELLA%%'", filter_type, filter_value, date)
  worklist <- as.data.table(dbGetQuery(con, query)) 
  selected_data <<- worklist
  worklist$OMSCHRIJV <- str_sub(worklist$OMSCHRIJV,-4,-1) 
  colnames(worklist)[colnames(worklist) == "OMSCHRIJV"] = "patientcode"

  return(worklist)

}

get_patients_by_arts_code_and_date <- function(data, filter_type, filter_value, date='2021-12-08') {
  # -- gets db connection and obtains patient appointments from db of the given arts and date -- #

  # get database connection
  con <- get_db_connection()

  # obtain worklist/patients of the given arts and date
  worklist <- obtain_worklist_arts(con, filter_type, filter_value, date)

  df_arts <- merge(data, worklist, by = "patientcode", all.y=TRUE)
  code_and_name <- unique(paste(df_arts$patientcode, df_arts$ACHTERNAAM))

  # close database connection
  dbDisconnect(con)

  return(code_and_name)
}

get_periods_of_patients <- function(patient_number) {
  # -- Gets all of the availble periods of the selected patient-- #

  periods <- final_df_pat[final_df_pat$patientcode == patient_number,]
  periods <- unique(periods)

  return (periods)
} 

pivot_df <- function(df) {
  # -- changes dataframe into wide format -- #

  df <- as.data.frame(df %>% pivot_wider(id_cols='period', names_from='group', values_from='V1'))
  rownames(df) <- df$period  # set row names to period
  df <- df[,-1]  # remove the period column (those are now the row names)

  return(df)

}

## Functions to create output tables/graphs of UI ##

annotate_growth_loss <- function(row, period, ref_period) {
  # -- transforms value to the value together with a red and green arrow indicating loss or growth -- #
  
  value <- row[[period]]
  ref_value <- row[[ref_period]]

  # if na, return value
  if (is.na(value) || is.na(ref_value)) {
    return(value)
  }
  # if value > ref_value color text green and add a green up-arrow
  else if (value > ref_value) {
    return(paste0("<span style='color:green'>", value, "  &#129093;</span>"))
  # if value < ref_value color text red and add a red down-arrow
  } else if (value < ref_value) {
    return(paste0("<span style='color:red'>", value, "  &#129095</span>"))
  } 
  # if the same, return value
  return(paste0("<span'>", value, "</span>"))
}

create_comparision_table <- function(patient_data, period, ref_period) {
  # -- creates table where current period is being compared to a preveous/reference period -- #

  if (length(patient_data) > 0) {
    non_breaking_space <- "\u00A0"  # whitespace which does not cause a linebreak 

    # select data of the two periods
    df_subset <- t(patient_data[rownames(patient_data) %in% c(period, ref_period),])
    df_subset <- as.data.frame(df_subset[,c(ref_period, period)])

    # if selected and reference period are the same, only show one column of data
    if (period == ref_period) {
      colnames(df_subset)[colnames(df_subset) == period] = paste0(period, non_breaking_space, "maanden")
      rownames(df_subset) <- str_replace(rownames(df_subset), " ", non_breaking_space)
      return(df_subset[,c(1), drop=FALSE])
    }

    # format period column to being colored (growth=green, loss=red, missing=grey) and adding indicating arrows
    df_subset[[period]] <- apply(df_subset, 1, annotate_growth_loss, period, ref_period)
    df_subset[[ref_period]] <- ifelse(is.na(df_subset[[ref_period]]), df_subset[[ref_period]], paste0("<span'>", df_subset[[ref_period]], "</span>"))

    # renaming columns and rows
    colnames(df_subset)[colnames(df_subset) == period] = paste0("huidige", non_breaking_space, "periode\n(", period, " maanden)")
    colnames(df_subset)[colnames(df_subset) == ref_period] = paste0("referentie", non_breaking_space, "periode\n(", ref_period, " maanden)")
    rownames(df_subset) <- str_replace(rownames(df_subset), " ", non_breaking_space)

    return(df_subset)
  }

  return(NULL)
  
}

create_domain_table <- function(data, domain) {
  # -- creates overview table of the selected domain -- #
  
  if (length(data) > 0) {
    data <- data[domain]  
    data$Maand <- as.integer(row.names(data))
    data <- data[order(data$Maand),c('Maand', domain)]  # order based on index
  }
  
  return(data)
}

create_radar_chart <- function(patient_data, period, ref_period, show_norm=TRUE) {
  # -- creates a radar chart of the data of the given patient -- #

  # set margins
  op <- par(mar=c(3,1,3,3))
  # only select data of  period and reference period
  df_subset <- patient_data[rownames(patient_data) %in% c(ref_period, period),]
  df_subset <- df_subset[order(as.numeric(row.names(df_subset))),]   # order based on index
  df_subset <- df_subset[,colSums(df_subset, na.rm=TRUE)!=0]

  
  if (length(df_subset) > 0) {
    # calculate the min and max values of each domain (CHANGE THIS TO A BETTER SOLUTION!)
    max_values <- sapply(df_subset, max, na.rm=TRUE)
    min_values <- max_values * 0
    max_values <- min_values + 100

    max_min <- rbind(max_values, min_values)
    df_subset <- rbind(max_min, df_subset)

    if (show_norm == TRUE){
      # to wide format
      norm_values <- norm_data %>% filter(sapply(groep, is_within_range, value=age_selected_patient))
      norm_df <- data.frame(matrix(nrow=1, ncol=length(norm_values$domein)))
      colnames(norm_df) <- norm_values$domein
      norm_df[1,] <- norm_values$score

      row_names <- append(rownames(df_subset), "Normaal waarden")
      df_subset <- rbind.fill(df_subset, norm_df)
      rownames(df_subset) <- row_names
    }

    colnames(df_subset) <- str_replace(colnames(df_subset), " ", "\n")

    cols_to_del <- which(is.na(df_subset[1, ]))
    if (length(cols_to_del)> 0){
       df_subset <- df_subset[, -which(is.na(df_subset[1, ]))]  # removes columns that have NA's at max_values row  
    }

    radarchart(df_subset, 
              pcol = c("#959494", scales::alpha("#00a8bb", 0.45), scales::alpha("#6d9c38", 0.7)), 
              pfcol = c(scales::alpha(c("#959494"), 0.1), scales::alpha(c("#00AFBB"), 0.12), scales::alpha("#6d9c38", 0)), 
              plwd = 2, plty = c(1,1,2), pty = 16,  axislabcol = "grey")

    # create legend
    if (show_norm == TRUE){
      legend(
          x='right', legend = append( paste0(rownames(df_subset[-c(1,2, 5),]), " maanden"), "Normaal waarden"),
          bty = "n", pch = 20 , col = c( "#959494", "#00AFBB", "#6d9c38"),
          text.col = "black", cex = 1, pt.cex = 2.9
        )
    } else {
      legend(
        x='right', legend = paste0(rownames(df_subset[-c(1,2, 5),]), " maanden"),
        bty = "n", pch = 20 , col = c( "#959494", "#00AFBB"),
        text.col = "black", cex = 1, pt.cex = 2.9
      )
    }
  
  # if no data is available, create an empty plot
  } else {
    plot(x=1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, "Geen data beschikbaar\n(geen vragenlijsten ingevuld)", cex = 1.5)
  }

  par(op)
  

}

create_bar_chart <- function(data, groep, show_norm) {

  # ggplot is very tedious in plotting barcharts. This steps is to sort the barcharts on period and not having gaps between periods
  data_plot <- data.frame("period"= as.numeric(row.names(data)), "height"=data[[groep]])
  data_plot <- data_plot[order(data_plot$period),]

  # if there is data to plot 
  if (length((data_plot)) > 0) {
      data_plot$period <- as.character(data_plot$period)
      data_plot$period <- factor(data_plot$period, levels = unique(data_plot$period))

      norm_values <- norm_data %>% filter(sapply(groep, is_within_range, value=age_selected_patient))
      norm_values <- norm_values[norm_values$domein == groep, 'score']
      
      # if not want to show normal values 
      if (!show_norm) {
        norm_values <- NULL
      }
      ggplot(data_plot, aes(x=period,y=height)) + 
        geom_bar(stat='identity',color='#00AFBB',linewidth=2,fill=scales::alpha('#00AFBB', 0.1)) + 
        scale_y_continuous(name = "Score",breaks = seq(0,100,5), limits=c(0, 100)) + 
        labs(x = "Periode (maanden)") +
        geom_hline(yintercept=norm_values, color="#6d9c38", linetype="dashed") +
        theme(panel.background = element_blank())
  } 
  # esle return NULL (no plot)
  else {
    NULL
  }

}

is_within_range <- function(value, range) {
  numbers <- as.numeric(unlist(strsplit(gsub("[\\[\\(\\)]", "", range), ",")))
  number1 <- numbers[1]
  number2 <- numbers[2]

  return(value >= number1 & value < number2)
}

# intialize variables
code_and_name <- get_patients_by_arts_code_and_date(final_df_pat, "ARTS", 190085, Sys.Date())
v_patientcode <- sort(unique(code_and_name))
v_period <- sort(unique(final_df_pat$period))
v_domain <- sort(unique(final_df_pat$group))



# DASHBOARD COMPONENTS #

header <- dashboardHeader(disable=TRUE)

sidebar <-  dashboardSidebar(
  width=250,
  tags$head(tags$style(".control-label {color: #00AFBB !important} .main-sidebar { min-width: 250px; max-width: 250px; font-family: Source Sans Pro; background-color: #e5f6f8 !important; color: #00AFBB !important;  border: 4px solid #00AFBB; border-top-right-radius: 20px; border-bottom-right-radius:20px} .skin-blue .left-side,.skin-blue .main-sidebar,.skin-blue .wrapper {background-color: #ecf0f5}")),  # removes the collapsing of sidebar
  div(h2(style="margin: 20px; color: #00AFBB;", "Umbrella PROMS ")),
  div(
  sidebarMenu(
    selectInput("arts", "Arts", names(name_code_dict)),
    dateInput("date", "Datum Spreekuur:", value = Sys.Date(), format = "dd-mm-yyyy", weekstart=1),
    selectInput('patientcode', paste0("Patiënten op ", Sys.Date()), v_patientcode)
  ))
)

mainbody <- fluidPage(
      fluidRow(
      # some css
      tags$head(tags$style(".h1 {color: grey !important} .content-wrapper {justiy: center; font-family: Source Sans Pro;} .row {diplay: flex; justify-content: center;}")),

      tags$h1(uiOutput('patient'),  style = "color: gray; margin-left:30px;"),
      column(11, style="margin-left: 30px;",

        # Radar chart row
        fluidRow(style="background-color: white; border-radius:  10px;",
          tags$h2(style="margin-left: 15px; color:  #00AFBB", "Radarchart"),
          column(2, br(), selectInput('period','Huidige Periode', v_period), selectInput('ref_period','Ref. Periode',v_period), br(), checkboxInput('show_norm_radar', 'Normaal waarden tonen*', value=TRUE), tags$p("*Normaal waarden voor Sexueel functioneren, Arm symptomen en Borst symptomen zijn niet beschikbaar", style="font-family: 'Calibri';font-size: 10px;"), br(), actionButton('button_comp_table', 'Data in tabel vorm')),
          column(10, plotOutput('radarchart', height=650, width=900)),
          br(),
        ),

        br(),br(),br(),

        # Bar chart row
        fluidRow(style="background-color: white; border-radius:  10px;",
          tags$h2(style="margin-left: 15px; color: #00AFBB", "Barchart"),
          column(12, br(),
          column(3, br(), selectInput('domain','Domein',v_domain), checkboxInput('show_norm_bar', 'Normaal waarden tonen*', value=TRUE),  tags$p("*Normaal waarden voor Sexueel functioneren, Arm symptomen en Borst symptomen zijn niet beschikbaar", style="font-family: 'Calibri';font-size: 10px;"), br(), actionButton('button_domain_table', 'Data in tabel vorm')),
          column(8, plotOutput('barchart'))
          ),
          br(), br()),
        )
    ),
    br(), br(), br()  # white space at bottom of page
)


# empty body when no patient is selected/available
no_patient_body <- fluidPage(
  fluidRow(
    style = "display: flex; align-items: center; justify-content: center; height: 100vh",
    div(
      fluidRow(
        column(12, fluidRow(tags$h2("Geen patiënt geselecteerd")))
      )
    )
  )
)

body <- dashboardBody(uiOutput("body"))
ui <- dashboardPage(header, sidebar, body)
norm_data <- read.csv('./normdata_scores.csv')


server <- function(input,output,session){

    # the following three functions are used to update parts of the dashboard (patients, periods and body)

    update_patient_choices <- function() {
      arts_info <- name_code_dict[[input$arts]]
      filter_type <- arts_info[1]
      filter_value <- arts_info[2]
      choices <-  get_patients_by_arts_code_and_date(final_df_pat, filter_type, filter_value, input$date)
      if (length(choices) == 0) {
          choices <- "geen patiënten"
        }
        updateSelectInput(session, "patientcode", label=paste0("Patiënten op ", input$date), choices = choices)
    }

    update_period_choices <- function() {
      periods <- sort(as.numeric(rownames(data_selected_patient())))
      if (length(periods) != 0 ){
        updateSelectInput(session, "period", choices = periods, selected=max(periods))
        updateSelectInput(session, "ref_period", choices = periods, selected=ifelse(length(periods) == 1, max(periods), max(periods[periods!=max(periods)])))
      } else {
        updateSelectInput(session, "period", choices = '-')
        updateSelectInput(session, "ref_period", choices = '-')
      }
    }

    update_body <- function() {
      if (input$patientcode != "geen patiënten" && input$patientcode != "geen valide datum" && !is.na(input$patientcode) && input$patientcode != "") {
        output$body <- renderUI({mainbody})
      } else {
        output$body <- renderUI({no_patient_body})
      }
    }
    
  valid_date <- reactive({

    # if there is no date input, return false
    if (length(input$date) == 0) {
      return(FALSE)
    }

    date_obj <- as.Date(input$date)
    # Check if conversion successful
    if (is.na(date_obj) || date_obj < '2000-01-01') {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })

  

  # when arts gets changed, change patient input select box
  observeEvent(input$arts, update_patient_choices())
  
  # when date gets changed, change patient input select box
  observeEvent(input$date, {
    # only update if there is a valid 
    if (valid_date() == TRUE) {
      update_patient_choices()
    } else {
      updateSelectInput(session, "patientcode", label=paste0("Patiënten op ", input$date), choices = c("geen valide datum"))
    }
  })

  # when patient gets changed, update period select boxes and update main body of dashbaord
  observeEvent(input$patientcode, {
    update_period_choices() 
    update_body()
  })

  observeEvent(input$button_comp_table, {
    showModal(modalDialog(div(tableOutput('comp_table'), width=900, height=850), size="l", fade=FALSE, style="display: flex; justify-content: center"))
  })

  observeEvent(input$button_domain_table, {
    showModal(modalDialog(div(tableOutput('domain_table')), size="m", fade=FALSE, style="display: flex; justify-content: center"))
  })

  # reactive data of selected patient
  data_selected_patient <- reactive({
  
    if (!is.null(input$patientcode)) {

      selected_patient <- str_split(input$patientcode, " ")[[1]][1]
      age_selected_patient <<- selected_data[selected_data$OMSCHRIJV == paste0('UMBRELLA_000', selected_patient), 'LEEFTIJD'][[1]]  # extract age for selected patient (needed for norm values)
      data_patient <- final_df_pat[final_df_pat$patientcode == selected_patient,]
      data_patient <- pivot_df(data_patient)
      
      return(data_patient)
    }
    return(NULL)
    }) 


  # create outputs :
  output$patient <- renderUI({str_split(input$patientcode, " ")[[1]][2]})
  output$radarchart <- renderPlot(create_radar_chart(data_selected_patient(), input$period, input$ref_period, input$show_norm_radar))
  output$comp_table <- renderTable(create_comparision_table(data_selected_patient(), input$period, input$ref_period),sanitize.text.function = function(x) x, include.rownames=TRUE)
  output$barchart <- renderPlot(create_bar_chart(data_selected_patient(), input$domain, input$show_norm_bar))
  output$domain_table <- renderTable(create_domain_table(data_selected_patient(), input$domain), include.rownames=FALSE)
}

shinyApp(ui = ui, server = server)








