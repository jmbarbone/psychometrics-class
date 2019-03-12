#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

ui <- fluidPage(
    selectInput(inputId = "scale",
                label = "Select Scale:",
                choices = sort(unique(csv$name)),
                selected = "AAAS_AD",
                multiple = F),
    # tableOutput("data")
    plotOutput("plot")
)

server <- function(input, output) {
    csv <- read_csv("~/GitHub/cornucopia/psychometrics-class/sncss.csv")
    processed <- csv %>%
        filter(scale == "CON",
               item %in% keep_items) %>%
        mutate(name = "CON_Short",
               domain = "Short") %>%
        bind_rows(csv)
    con_short <- processed %>%
        filter(name == "CON_Short") %>%
        select(-scale, -domain)
    con_short_totals <- con_short %>%
        group_by(id) %>%
        summarise(CON_Short = sum(response))
    other_scale_totals <- tots %>%
        unite(name, scale, domain) %>%
        select(-item)
    combined_totals <- other_scale_totals %>%
        left_join(con_short_totals) %>%
        mutate(full_name = recode(name,
                                  "AAAS_AD" = "Adaptive and Aggressive Assertiveness Scale (AAA-S)",
                                  "AAAS_AG" = "Adaptive and Aggressive Assertiveness Scale (AAA-S)",
                                  "BES_AF" = "Basic Empathy Scale in Adults (BES-A)",
                                  "BES_CE" = "Basic Empathy Scale in Adults (BES-A)",
                                  "LEAD_LF" = "Leadership Self-Report Scale (LSRS)",
                                  "LEAD_TA" = "Leadership Self-Report Scale (LSRS)",
                                  "LEAD_TF" = "Leadership Self-Report Scale (LSRS)",
                                  "CON_full" = "Control",
                                  "NPI_full" = "Narcissism Personality Inventory (NPI)"),
               subscale = recode(name,
                                 "AAAS_AD" = "Adaptive Assertiveness Sub-scale",
                                 "AAAS_AG" = "Aggressive Assertiveness Sub-scale",
                                 "BES_AF" = "Effective Empathy",
                                 "BES_CE" = "Cognitive Empathy",
                                 "LEAD_LF" = "Laissez-faire",
                                 "LEAD_TA" = "Transactional Leadership",
                                 "LEAD_TF" = "Transformational Leadership",
                                 "CON_full" = "",
                                 "NPI_full" = ""))


    output$plot <- renderPlot({
        plot_data <- filter(combined_totals, name == input$scale)
        scale_title <- unique(plot_data$full_name)
        scale_subtitle <- unique(plot_data$subscale)
        cor_results <- filter(combined_totals, name == "CON_full") %>%
            cor.test(~ CON_Short + total, data = .) %>%
            papaja::apa_print.htest() %>%
            pluck("full_result")

        ggplot(plot_data,
               aes(x = CON_Short, y = total)) +
            geom_point() +
            geom_smooth(method = "lm") +
            # bbplot::bbc_style() +
            theme_light() +
            theme(plot.title = element_text(size = 20),
                  plot.subtitle = element_text(size =),
                  plot.caption = element_text(size = 15)) +
            labs(title = scale_title,
                 subtitle = scale_subtitle,
                 x = NULL,
                 y = NULL,
                 caption = latex2exp::TeX(cor_results))
    })
}

shinyApp(ui, server)
