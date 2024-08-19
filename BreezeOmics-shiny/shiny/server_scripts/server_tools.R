output$ui_tools <- renderUI({
    fluidPage(style = "margin-top:10px;margin-bottom:30px;",

        bs_accordion_sidebar(spec_side = c(width = 3, offset = 0), spec_main = c(width = 9, offset = 0), id = "number_letter_equation") %>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>Shiny-VennDiagram</div>"),
            content_side = uiOutput("veendiagram_option"),
            content_main = uiOutput("veendiagram_main_panel"),
            ) %>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>Shiny-ComplexHeatmap</div>"),
            content_side = uiOutput("complexheatmap_option"),
            content_main = uiOutput("complexheatmap_main_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>Shiny-Scatter plot</div>"),
            content_side = uiOutput("Scatter_plot_option"),
            content_main = uiOutput("Scatter_plot_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>KEGG pathway get</div>"),
            content_side = uiOutput("getkegg_option"),
            content_main = uiOutput("getkegg_main_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>GO function get</div>"),
            content_side = uiOutput("getgo_option"),
            content_main = uiOutput("getgo_main_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>Rice Symbol annotation</div>"),
            content_side = uiOutput("symbol_anno_option"),
            content_main = uiOutput("symbol_anno_main_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>Rice locus conversion</div>"),
            content_side = uiOutput("locus_conv_option"),
            content_main = uiOutput("locus_conv_main_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>TFs family search</div>"),
            content_side = uiOutput("tfs_search_option"),
            content_main = uiOutput("tfs_search_main_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>Files merge by column</div>"),
            content_side = uiOutput("merge_option"),
            content_main = uiOutput("merge_main_panel"),
            )%>%
            bs_append(
            title_side = HTML("<div style='font-size:20px;font-weight:1000;line-height: 2em;'>Column statistics</div>"),
            content_side = uiOutput("col_stat_option"),
            content_main = uiOutput("col_stat_main_panel"),
            ),
        # activate tooltips, popovers, accordion-sidebar and MathJax
        use_bs_tooltip(),
        use_bs_popover(),
        use_bs_accordion_sidebar(), # needs to be at end, for some reason
        withMathJax()
    )
})

source("server_scripts/server_tools/server_venndiagram.R", local = TRUE)
source("server_scripts/server_tools/server_complexheatmap.R", local = TRUE)
source("server_scripts/server_tools/server_getkegg.R", local = TRUE)
source("server_scripts/server_tools/server_getgo.R", local = TRUE)
source("server_scripts/server_tools/server_rice_symbol_anno.R", local = TRUE)
source("server_scripts/server_tools/server_tfs_search.R", local = TRUE)
source("server_scripts/server_tools/server_id_conversion.R", local = TRUE)
source("server_scripts/server_tools/server_merge.R", local = TRUE)
source("server_scripts/server_tools/server_column_stat.R", local = TRUE)
source("server_scripts/server_tools/server_scatter_plot.R", local = TRUE)