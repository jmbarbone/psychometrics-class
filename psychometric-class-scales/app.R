library(shiny)
library(tidyverse)
csv <- read_csv("all_totals.csv")
con_short_totals <- csv %>%
    filter(name == "CON_Short") %>%
    select(id, "CON_Short" = total)
combined_totals <- csv %>%
    filter(name != "CON_Short") %>%
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

ui <- fluidPage(
    selectInput(inputId = "scale",
                label = "Select Scale:",
                choices = sort(unique(csv$name)),
                selected = "AAAS_AD",
                multiple = F),
    plotOutput("plot")
)

server <- function(input, output) {
    output$plot <- renderPlot({
        plot_data <- filter(combined_totals, name == input$scale)
        scale_title <- unique(plot_data$full_name)
        scale_subtitle <- unique(plot_data$subscale)
        cor_results <- plot_data %>%
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
