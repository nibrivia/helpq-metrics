library(highcharter)
library(hrbrthemes)
library(jsonlite)
library(lubridate)
library(pool)
library(shiny)
library(tidyverse)

pool <- dbPool(
  drv = odbc::odbc(),
  dsn = "helpq"
)
onStop(function() {
  poolClose(pool)
})

q_add_series <- function(dta) {
  function(hc, dim, ...) {
    hc %>%
      hc_add_series_times_values(dates  = dta[["time"]],
                                 values = dta[[dim]],
                                 step   = TRUE,
                                 ...    = ...)
  }
}

shinyServer(function(input, output) {
  ## Data ======================================================================
  date_range <- reactive({
    c(start = input$date_range[1] - days(1) + hours(3),
      end   = input$date_range[2] + days(1) + hours(3))
  })

  student_q <- reactive({
    input$refresh
    pool %>%
      tbl("StudentQ") %>%
      filter(time >= date_range()[1] & time <= date_range()[2]) %>%
      collect() %>%
      mutate(time = as_datetime(time),
             being_helped = being_helped > 0)
  })

  staff_q <- reactive({
    input$refresh
    pool %>%
      tbl("StaffQ") %>%
      filter(time >= date_range()[1] & time <= date_range()[2]) %>%
      collect() %>%
      mutate(time = as_datetime(time),
             is_helping = is_helping > 0)
  })

  wait_data <- reactive({
    wait_data <- student_q() %>%
      mutate(t_id = dense_rank(time)) %>%
      filter(!is.na(username)) %>%
      group_by(username) %>%
        arrange(time) %>%
        mutate(req_id = (t_id - lag(t_id, default = -Inf) > 1) |
                 (!being_helped & lag(being_helped, default = !first(being_helped))),
               req_id = cumsum(req_id)) %>%
      group_by(username, req_id) %>%
        mutate(start_wait = min(time[!being_helped]),
               end_wait   = max(time[!being_helped]),
               start_help = min(time[ being_helped]),
               end_help   = max(time[ being_helped]),
               t_wait     = time - start_wait,
               wait       = end_wait - start_wait) %>%
        filter(time %in% c(start_wait, end_wait, start_help, end_help)) %>%
        ungroup()

    units(wait_data$wait) <- "mins"
    units(wait_data$t_wait) <- "mins"

    return(wait_data)
  })

  current_q <- reactive({
    student_q() %>% top_n(1, time)
  })

  ## Top row ===================================================================
  output$n_staff <- renderInfoBox({
    infoBox(value = current_q()[["n_staff"]][[1]],
            title = "staff in lab",
            icon = icon("users"),
            color = "yellow",
            width = 3)
  })

  output$n_helping <- renderInfoBox({
    infoBox(value = sum(current_q()[["being_helped"]]),
            title = "students being helped",
            icon = icon("comments"),
            color = "yellow",
            width = 3)
  })

  output$n_students <- renderInfoBox({
    infoBox(value = sum(!current_q()[["being_helped"]]),
            title = "students waiting",
            icon = icon("question"),
            color = "yellow",
            width = 3)
  })

  output$wait <- renderInfoBox({
    wait_last <- wait_data() %>%
      top_n(1, time) %>%
      filter(!being_helped)

    if (nrow(wait_last) == 0) {
      return(ib <- infoBox(value = NA,
                           title = "wait time",
                           icon  = icon("clock-o"),
                           color = "red",
                           fill  = T,
                           width = 3))
    }

    wait_time <- round(max(wait_last[["t_wait"]]), digits = 2)
    color <- "green"
    if (wait_time > 15) {
      color <- "orange"
    }
    if (wait_time > 30) {
      color <- "red"
    }

    infoBox(value = wait_time,
            title = "wait time",
            icon  = icon("clock-o"),
            color = color,
            fill  = T,
            width = 3)
  })

  ## Plots =====================================================================
  output$plot_all <- renderHighchart({
    q_data <- student_q() %>%
      group_by(time) %>%
        mutate(staff_username = ifelse(is.na(position), NA, staff_username),
               n_helping = n_distinct(staff_username, na.rm = TRUE),
               n_waiting = n_helping - n_distinct(username, na.rm = TRUE)) %>%
        ungroup() %>%
      mutate(n_helping_d = n_helping != lag(n_helping),
             n_waiting_d = n_waiting != lag(n_waiting),
             n_staff_d   = n_staff   != lag(n_staff)) %>%
      filter(n_waiting_d | n_helping_d | n_staff_d)

    q_add <- q_add_series(q_data)

    highchart() %>%
      q_add("n_helping",
            name  = "Helped",
            color = "green",
            type  = "area") %>%
      q_add("n_waiting",
            name = "Waiting",
            color = "red",
            type  = "area") %>%
      q_add("n_staff",
            name  = "Staff",
            color = "black")
  })

  output$plot_staff_utilization <- renderHighchart({
    q_data <- student_q() %>%
      arrange(time) %>%
      group_by(time) %>%
        mutate(staff_utilization = 100*sum(being_helped)/n_staff) %>%
        ungroup() %>%
      filter(staff_utilization != lag(staff_utilization))

    q_add <- q_add_series(q_data)

    highchart() %>%
      q_add("staff_utilization",
            name = "Percent staff utilized",
            type = "area") %>%
      hc_yAxis(min = 0, max = 100)
  })

  output$plot_staff_view <- renderPlot({
    staff_q() %>%
      ggplot(aes(x = time,
                 y = factor(staff, levels = unique(staff)),
                 color = is_helping)) +
      geom_point() +
      theme_ipsum_rc() +
      labs(x = NULL, y = NULL)
  })

  output$plot_wait_time <- renderPlot({
    ylims <- c(0, max(wait_data()$wait[wait_data()$wait < 120]))

    wait_data() %>%
      ggplot(aes(x = time,
                 y = t_wait,
                 color = being_helped,
                 alpha = !being_helped,
                 group = paste(username, req_id))) +

      geom_line(size = .5,
                alpha = .85) +

      geom_point(data = wait_data() %>%
                  group_by(username) %>%
                  filter(time %in% start_help),
                aes(x = time,
                    y = t_wait,
                    alpha = T,
                    group = "1"),
                color = "black",
                alpha = 1,
                size = 1.2) +

      scale_y_continuous(breaks = 0:100*10,
                         minor_breaks = 0:100*5) +
      scale_color_discrete(guide = F) +
      scale_alpha_discrete(guide = F) +

      coord_cartesian(ylim = ylims) +
      theme_ipsum_rc() +
      labs(x = NULL, y = NULL)
  })

  output$q_char <- renderPlot({
    wd <- student_q() %>%
      mutate(t_id = dense_rank(time)) %>%
      filter(!is.na(username)) %>%
      group_by(username) %>%
      arrange(time) %>%
      mutate(req_id = (t_id - lag(t_id, default = -Inf) > 1) |
               (!being_helped & lag(being_helped, default = !first(being_helped))),
             req_id = cumsum(req_id)) %>%
      group_by(username, req_id) %>%
      mutate(start_wait = min(time[!being_helped]),
             end_wait   = max(time[!being_helped]),
             start_help = min(time[ being_helped]),
             end_help   = max(time[ being_helped]),
             t_wait     = time - start_wait,
             wait       = end_wait - start_wait) %>%
      ungroup()

    units(wd$wait) <- "mins"
    units(wd$t_wait) <- "mins"

    wd %>%
      group_by(username, req_id) %>%
        arrange(time) %>%
        mutate(w = as.numeric(lead(time, default = max(time)) - time,
                              units = "mins") *
                 !being_helped) %>%
        summarize(succ     = any(being_helped),
                  n_staff  = weighted.mean(n_staff,w),
                  wait     = first(wait),
                  position = max(position_adj)) %>%
        ungroup() %>%
      filter(succ,
             wait < 180,
             n_staff > 0.2,
             position > 0) %>%

      ggplot(aes(x = position/n_staff,
                 y = as.numeric(wait),
                 color = n_staff,
                 fill = n_staff)) +
      geom_point(position = position_jitter(width = 0, height = 0)) +
      theme_ipsum_rc()
  })

  output$expect <- renderPlot({
    wd <- wait_data() %>%
      filter(time > as.POSIXct("2017-10-26 03:00:00"))

    wd_rate <- wd %>%
      group_by(username, req_id) %>%
        top_n(1, time) %>%
        ungroup() %>%
      # group_by(staff_username) %>%
      filter(!is.na(end_help),
             as.numeric(end_help) > -Inf) %>%
      mutate(help_t = end_help - start_help) %>%
      summarize(rate = mean(as.numeric(end_help - start_help, units = "mins"))) %>%
      .[["rate"]]

    wd %>%
      ggplot(aes(x = time,
                 y = t_wait,
                 color = being_helped,
                 alpha = !being_helped,
                 group = paste(username, req_id))) +
      geom_line(size = .5,
                alpha = .85) +
      geom_point(data = wd %>%
                  filter(n_staff != 0) %>%
                  arrange(time) %>%
                  mutate(t_now = max(time)) %>%
                  group_by(username, req_id) %>%
                    filter(any(being_helped) | any(t_now == time)) %>%
                    summarize(n_staff = first(n_staff),
                              help_t  = first(start_wait),
                              q_len   = first(position_adj),
                              wait_time = first(wait)) %>%
                    ungroup(),
                aes(x = help_t,
                    y = q_len/n_staff*wd_rate),
                inherit.aes = F,
                color = "green", alpha = 1) +
      geom_point(data = wd %>%
                   group_by(username) %>%
                   filter(time %in% start_help),
                 aes(x = time,
                     y = t_wait,
                     alpha = T,
                     group = "1"),
                 color = "black",
                 alpha = 1,
                 size = 1.2) +
      scale_y_continuous(breaks = 0:100*10,
                         minor_breaks = 0:100*5) +
      scale_color_discrete(guide = F) +
      scale_alpha_discrete(guide = F) +
      # coord_cartesian(ylim = c(0, max(wd$wait[wd$wait < 50]))) +
      theme_ipsum_rc() +
      labs(x = NULL, y = NULL)
  })
})
