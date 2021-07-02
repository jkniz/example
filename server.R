library(shiny)
library(tidyverse)
library(ggplot2)
library(survminer)
library(gmodels)
library(knitr)
source("global.R")

server <- function(input, output, session) {
  df <- read.csv("liver_cancer.csv", header = TRUE)
  df <- rename(df, ID = X)

  dataset <- reactive({
    select(df, c(ID, input$c_vars))
  })
  myChoices <- c("age", "sex", "height", "weight", "bmi", "diet", "smoker", "cigs_per_day", "packyears", "alc", "size", "bili", "hbv", "hcv", "dia", "chol")
  # observe({
  #   updateCheckboxGroupInput(session, "c_vars",
  #     choices = c_vars_choices,
  #     selected = if (input$c_all) c_vars_choices
  #   )
  # })

  observeEvent(input$c_all, {
    updateCheckboxGroupInput(session, inputId = "c_vars", selected = c_vars_choices)
  })

  observeEvent(input$c_vars, {

    if(length(input$c_vars) != length(c_vars_choices)){
      updateCheckboxInput(session, "c_all", value = FALSE)
    }
  })
  output$out_dataset <- renderDataTable(
    dataset()
  )
  scatter <- reactive(
    if (input$scat_var_group == "None") {
      ggplot(data = df, mapping = aes(x = as.numeric(unlist(df[colnames(df) == input$scat_var_x])), y = as.numeric(unlist(df[colnames(df) == input$scat_var_y])))) +
        geom_point(colour = "steelblue", size = 2) +
        # stat_smooth(method = "lm",
        #           col = "#D3D3D3",
        #          se = FALSE,
        #         size = 0.8)+
        labs(
          title = paste0(
            "Variables ", colnames(df[colnames(df) == input$scat_var_x]), " and ",
            colnames(df[colnames(df) == input$scat_var_y])
          ),
          x = colnames(df[colnames(df) == input$scat_var_x]),
          y = colnames(df[colnames(df) == input$scat_var_y])
        ) +
        theme_gray()
    } else {
      ggplot(data = df, mapping = aes(x = as.numeric(unlist(df[colnames(df) == input$scat_var_x])), y = as.numeric(unlist(df[colnames(df) == input$scat_var_y])))) +
        geom_point(mapping = aes(colour = as.character(unlist(df[colnames(df) == input$scat_var_group]))), size = 2) +
        labs(
          title = paste0(
            "Variables ", colnames(df[colnames(df) == input$scat_var_x]), " and ",
            colnames(df[colnames(df) == input$scat_var_y]), " grouped by ",
            colnames(df[colnames(df) == input$scat_var_group])
          ),
          x = colnames(df[colnames(df) == input$scat_var_x]),
          y = colnames(df[colnames(df) == input$scat_var_y]), colour = colnames(df[colnames(df) == input$scat_var_group])
        ) +
        theme_gray()
    }
  )
  box <- reactive(
    if (input$box_var_group == "None") {
      ggplot(data = df) +
        geom_boxplot(mapping = aes(x = as.numeric(unlist(df[colnames(df) == input$box_var_x]))), fill = "steelblue") +
        labs(
          title = paste0("Variable ", colnames(df[colnames(df) == input$box_var_x])),
          x = colnames(df[colnames(df) == input$box_var_x])
        ) +
        theme_minimal()
    } else {
      ggplot(data = df) +
        geom_boxplot(mapping = aes(
          x = get(input$box_var_x), # as.numeric(unlist(df[colnames(df) == input$box_var_x])),
          y = as.character(unlist(df[colnames(df) == input$box_var_group])),
          group = as.character(unlist(df[colnames(df) == input$box_var_group])),
          fill = get(input$box_var_group)
        )) + # fill=colnames(df[colnames(df) == input$scat_var_group]) didn't work
        labs(
          title = paste0(
            "Variables ", colnames(df[colnames(df) == input$box_var_x]), " and ",
            colnames(df[colnames(df) == input$box_var_group])
          ),
          x = colnames(df[colnames(df) == input$box_var_x]),
          y = colnames(df[colnames(df) == input$box_var_group])
        ) +
        theme_minimal()
    }
  )
  histo <- reactive(
    ggplot(data = df) +
      geom_histogram(mapping = aes(x = as.numeric(unlist(df[colnames(df) == input$histo_var_x]))), colour = "black", fill = "steelblue") +
      labs(
        title = paste0("Histogram of variable ", colnames(df[colnames(df) == input$histo_var_x])),
        x = colnames(df[colnames(df) == input$histo_var_x]),
        y = "Frequency"
      ) +
      theme_minimal()
  )
  # sum <- reactive({
  #   summary_table <- t(as.numeric(summary(as.numeric(unlist(df[colnames(df) == input$tab_sum])))))
  #   knitr::kable(summary_table, col.names = c("Min", "Q1", "Median", "Mean", "Q3", "Max"))
  # })
  freq <- reactive(
    if (input$tab_freq_2 == "None") {
      select(df, input$tab_freq_1)
    }
    else {
      select(df, input$tab_freq_1, input$tab_freq_2)
    }
  )
  risk <- reactive(
    if (input$tab_risk_1 != input$tab_risk_2) {
      select(df, input$tab_risk_1, input$tab_risk_2)
    }
    else {
      select(df, input$tab_risk_1)
    }
  )
  ci_out <- reactive({
    select(df, input$tab_ci)
  })
  # get_alpha <- reactive(
  #   (1 - input$slider_ci)
  # )
  output$scatter <- renderPlot({
    scatter()
  })
  output$box <- renderPlot({
    box()
  })
  output$histo <- renderPlot({
    histo()
  })
  output$sum <- renderTable({
    expr <- rbind(round(t(as.numeric(summary(as.numeric(unlist(df[colnames(df) == input$tab_sum]))))), digits = 2))
    colnames(expr) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")
    expr
  })
  output$freq <- renderTable({
    x <- as.data.frame(table(freq()))
    if (length(x) < 3) {
      names(x) <- c(input$tab_freq_1, "Freq")
    }
    else {
      names(x) <- c(input$tab_freq_1, input$tab_freq_2, "Freq")
    }
    expr <- (x)
  })
  output$risk <- renderTable({

    risk_df <- risk() %>%
      count(.data[[input$tab_risk_1]], .data[[input$tab_risk_2]])



    unique1 <- 0
    unique2 <- 0
    if (length(risk_df) > 1) {
      unique1 <- sort((unique(risk_df[, 1])))
      unique2 <- sort((unique(risk_df[, 2])))
      m <- matrix(0, nrow = length(unique1), ncol = length(unique2) + 3)
      m <- format.data.frame(data.frame(m, row.names = unique1), digits = 0)
      m[, 1] <- (unique1)
      for (i in (1:length(unique1))) {
        for (j in (2:(length(unique2) + 1))) {
          m[i, j] <- (sum(risk_df[, 1] == unique1[i] & risk_df[, 2] == unique2[j - 1]))
        }
      }
      m[, 4] <- as.numeric(m[, 2]) / (as.numeric(m[, 2]) + as.numeric(m[, 3]))
      m[, 5] <- as.numeric(m[, 2]) / (as.numeric(m[, 3]))
      colnames(m) <- c(" ", unique2, "Risk", "Odds")
    }
    else {
      m <- as.data.frame(table(risk()))
      names(m) <- c(input$tab_risk_1, "Freq")
    }

    m

  })
  output$ci <- renderTable({
    ci_var <- as.numeric(unlist(ci_out()))
    alpha <- 1 - input$slider_ci
    error <- qt(p = 1 - (alpha / 2), df = length(ci_var) - 1) * (sd(ci_var)) / sqrt(length(ci_var))
    left <- mean(ci_var) - error
    right <- mean(ci_var) + error
    tab <- rbind(round(c(alpha, left, mean(ci_var), right, error),
      digits = 4
    ))
    colnames(tab) <- c("Alpha", "Lower limit", "Mean", "Upper limit", "Standard error")
    tab
  })

  output$plot_jk <- plotly::renderPlotly({
    p <- ggplot(df, aes(x = bili, y = size)) +
      geom_point()

    plotly::ggplotly(p)
  })
}
