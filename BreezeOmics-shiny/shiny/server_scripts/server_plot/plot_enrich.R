#kegg bar plot
kegg_bar_plot <- reactive({
    if(!is.null(kegg_table())){
        if(nrow(kegg_table()$rich_up)<=2){return(NULL)}
            if(input$enrich_log2fc_kegg == "Contain"){

                #UP
                rich_up <- kegg_table()$rich_up
                rich_down <- kegg_table()$rich_down
                rich_up <- data.frame(classa = rich_up$classa, classb = rich_up$classb, classc = rich_up$classc)
                rich_down <- data.frame(classa = rich_down$classa, classb = rich_down$classb, classc = rich_down$classc)
                rich <- rbind(rich_up, rich_down)
                rich <- rich[!duplicated(rich, fromLast=TRUE),]
                rich$Count <- 1
                print(nrow(rich))

                data1 = aggregate(rich$Count,by=list(rich$classb),FUN=sum)
                data1<-data1[order(-data1$x),]
                colnames(data1) <- c("classb", 'count_classb')
                data1 <-dplyr::inner_join(data1, data.frame(classa = rich$classa, classb = rich$classb), by = "classb")
                data1 <- data1[!duplicated(data1, fromLast=TRUE),]
                data1 <- data1[order(data1$classa),]
                data1$CB<-ifelse(stringr::str_count(data1$classb) > 60, paste0(stringr::str_sub(data1$classb,end=60),' ...'), data1$classb)
                names(data1)[names(data1) == "classa"] <- "KEGG_classA"
                # row.names(data1) <- 1: nrow(data1)
                data1$value <- 1: nrow(data1) # nolint

                #plot
                CPCOLS <- c("#FFC0CB", "#BA55D3", "#4169E1","#3CB371","#FFFF00","#FFFF00","#FFFF00","#FFFF00")
                s=ggplot(data=data1, aes(x=count_classb, y=reorder(CB,-value), fill=KEGG_classA))+
                geom_bar(stat="identity", width=0.5)+
                labs(colour = "KEGG_classA", x = 'Number of Genes', y = 'KEGG_classB\n', title = 'KEGG Pathway Annotation statistics\n')+
                theme_classic()+theme(legend.title = element_text(size = 12,color = 'black',face = 'bold'))+
                theme(axis.title.x = element_text(size = 12,colour = 'black',),axis.title.y = element_text(size = 16,colour = 'black',face = 'bold',),plot.title = element_text(size=18,colour = 'black',face = 'bold',hjust = 0.4 ),
                        axis.text.x=element_text(vjust=1,size=10,colour = 'black'),axis.text.y=element_text(vjust=0.3,size=12,colour = 'black'), legend.text=element_text(size=11,colour = 'black'))

            }else {
                rich <- kegg_table()$rich_up
                rich <- data.frame(classa = rich$classa, classb = rich$classb, classc = rich$classc)
                rich <- rich[!duplicated(rich, fromLast=TRUE),]
                rich$Count <- 1
                data1 <- aggregate(rich$Count,by=list(rich$classb),FUN=sum)
                data1 <- data1[order(-data1$x),]
                colnames(data1) <- c("classb", 'count_classb')
                data1 <-dplyr::inner_join(data1, data.frame(classa = rich$classa, classb = rich$classb), by = "classb")
                data1 <- data1[!duplicated(data1, fromLast=TRUE),]
                data1 <- data1[order(data1$classa),]
                data1$CB<-ifelse(stringr::str_count(data1$classb) > 60, paste0(stringr::str_sub(data1$classb,end=60),' ...'), data1$classb)
                names(data1)[names(data1) == "classa"] <- "KEGG_classA"
                # row.names(data1) <- 1: nrow(data1)
                data1$value <- 1: nrow(data1) # nolint

                #plot
                CPCOLS <- c("#FFC0CB", "#BA55D3", "#4169E1","#3CB371","#FFFF00","#FFFF00","#FFFF00","#FFFF00")
                s=ggplot(data=data1, aes(x=count_classb, y=reorder(CB,-value), fill=KEGG_classA))+
                geom_bar(stat="identity", width=0.5)+
                labs(colour = "KEGG_classA", x = 'Number of Genes', y = 'KEGG_classB\n', title = 'KEGG Pathway Annotation\n')+
                theme_classic()+theme(legend.title = element_text(size = 12,color = 'black',face = 'bold'))+
                theme(axis.title.x = element_text(size = 12,colour = 'black',),axis.title.y = element_text(size = 16,colour = 'black',face = 'bold',),plot.title = element_text(size=18,colour = 'black',face = 'bold',hjust = 0.4 ),
                        axis.text.x=element_text(vjust=1,size=10,colour = 'black'),axis.text.y=element_text(vjust=0.3,size=12,colour = 'black'), legend.text=element_text(size=11,colour = 'black'))
            }

        return(s)
    }else {
       return(NULL)
    }
})
output$kegg_plot_bar <- renderPlot(kegg_bar_plot())

#kegg dot plot
kegg_dot_plot <- reactive({
    if(!is.null(kegg_table())){
        if(input$enrich_log2fc_kegg=="Contain"){
            #UP
            rich <- kegg_table()$rich_up
            dot = rich
            dot <- dot[order(dot$Pvalue),]

            if (nrow(dot) <= 10) {dot = dot}else 
                {if(nrow(dot)<=input$kegg_num_of_item){dot = nrow(dot)}else 
                    {dot = dot[1:input$kegg_num_of_item,]}
                }

            
            if(input$kegg_show_pathway == "NO") {
                dot$Description <- sub(" \\[.+\\]","", dot$Description)
            }

            dot$DES <- ifelse(stringr::str_count(dot$Description) > input$kegg_y_text_length, paste0(stringr::str_sub(dot$Description,end=input$kegg_y_text_length),' ...'), dot$Description)
            dot$Count <- as.numeric(sub("/\\d+", "", dot$GeneRatio))

            if(input$kegg_rank_by == "RichFactor"){
                dot$rank_by <- dot$RichFactor
            }else if (input$kegg_rank_by == "Pvalue") {
            dot$rank_by <- -dot$Pvalue
            }else if (input$kegg_rank_by == "P.adjust") {
            dot$rank_by <- -dot$P.adjust
            }else {
            dot$rank_by <- dot$Count
            }
            dot_up <- dot

            #DOWN
            rich <- kegg_table()$rich_down
            dot = rich
            dot <- dot[order(dot$Pvalue),]

            if (nrow(dot) <= 10) {dot = dot}else 
                {if(nrow(dot)<=input$kegg_num_of_item){dot = nrow(dot)}else 
                    {dot = dot[1:input$kegg_num_of_item,]}
                }

            if(input$kegg_show_pathway == "NO") {
                dot$Description <- sub(" \\[.+\\]","", dot$Description)
            }

            dot$DES <- ifelse(stringr::str_count(dot$Description) > input$kegg_y_text_length, paste0(stringr::str_sub(dot$Description,end=input$kegg_y_text_length),' ...'), dot$Description)
            dot$Count <- as.numeric(sub("/\\d+", "", dot$GeneRatio))

            if(input$kegg_rank_by == "RichFactor"){
                dot$rank_by <- dot$RichFactor
            }else if (input$kegg_rank_by == "Pvalue") {
            dot$rank_by <- -dot$Pvalue
            }else if (input$kegg_rank_by == "P.adjust") {
            dot$rank_by <- -dot$P.adjust
            }else {
            dot$rank_by <- dot$Count
            }
            dot_down <- dot

            if(input$kegg_plot == "Dot plot"){
                dot_up$group <- "Up regulation" 
                dot_down$group <- "Down regulation" 
                dot <- rbind(dot_up, dot_down)
                dot$group <- factor(dot$group, levels=c('Up regulation', "Down regulation"))

                d = ggplot(dot, aes(RichFactor, reorder(DES, rank_by)))+
                    geom_point() +
                    geom_point(aes(size=Count,color=Pvalue))+
                    scale_color_gradient(low=input$color_kegg_plot_hight,high=input$color_kegg_plot_low)+
                    labs(color=expression(Pvalue),size='Count',x='RichFactor',y='KEGG pathway enrichment',title=NULL)+
                    theme_bw()+
                    theme(axis.title.x = element_text(size = input$kegg_x_title_size, color = 'black'),axis.title.y = element_text(size = input$kegg_y_title_size,face = 'bold'),
                        plot.title = element_text(size=input$kegg_y_title_size,face = 'bold',hjust = 1.1),
                        axis.text.x=element_text(vjust=1,size=input$kegg_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$kegg_y_text_size,colour = 'black'))+
                    theme(panel.grid.minor = element_blank())+guides(size = guide_legend(order = 1))
                d <- d + facet_grid(group~., scale = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(size = input$kegg_x_title_size))


            }else if (input$kegg_plot == "Bar plot") {
                dot_up$group <- "Up regulation" 
                if(input$kegg_rank_by == "RichFactor"){
                    dot_up$rank_by <- dot_up$RichFactor
                    # dot <- dot[order(dot$rank_by, decreasing = T),]
                    # rownames(dot) <- 1: nrow(dot)
                    }else if (input$kegg_rank_by == "Pvalue") {
                    dot_up$rank_by <- -log10(dot_up$Pvalue)
                    #    dot <- dot[order(dot$rank_by, decreasing = T),]
                    #    rownames(dot) <- 1: nrow(dot)
                    }else if (input$kegg_rank_by == "P.adjust") {
                    dot_up$rank_by <- -log10(dot_up$P.adjust)
                    #    dot <- dot[order(dot$rank_by, decreasing = T),]
                    #    rownames(dot) <- 1: nrow(dot)
                    }else {
                    dot_up$rank_by <- dot_up$Count
                    #    dot <- dot[order(dot$rank_by, decreasing = T),]
                    #    rownames(dot) <- 1: nrow(dot)
                    }
                dot_up <- dot_up[order(dot_up$rank_by, decreasing = F),]

                dot_down$group <- "Down regulation" 
                if(input$kegg_rank_by == "RichFactor"){
                    dot_down$rank_by <- dot_down$RichFactor
                    # dot <- dot[order(dot$rank_by, decreasing = T),]
                    # rownames(dot) <- 1: nrow(dot)
                    }else if (input$kegg_rank_by == "Pvalue") {
                    dot_down$rank_by <- -log10(dot_down$Pvalue)
                    #    dot <- dot[order(dot$rank_by, decreasing = T),]
                    #    rownames(dot) <- 1: nrow(dot)
                    }else if (input$kegg_rank_by == "P.adjust") {
                    dot_down$rank_by <- -log10(dot_down$P.adjust)
                    #    dot <- dot[order(dot$rank_by, decreasing = T),]
                    #    rownames(dot) <- 1: nrow(dot)
                    }else {
                    dot_down$rank_by <- dot_down$Count
                    #    dot <- dot[order(dot$rank_by, decreasing = T),]
                    #    rownames(dot) <- 1: nrow(dot)
                    }

                dot_down$rank_by <- -dot_down$rank_by
                dot_down <- dot_down[order(dot_down$rank_by, decreasing = T),]

                dot <- rbind(dot_down, dot_up)
                dot$plot_rank <- 1:nrow(dot) # nolint
                
                ytitle <- ifelse(input$kegg_rank_by == "RichFactor", "RichFactor",
                            ifelse(input$kegg_rank_by == "Pvalue", "-log10(Pvalue)",
                                ifelse(input$kegg_rank_by == "P.adjust", "-log10(P.adjust)",
                                    ifelse(input$kegg_rank_by == "Count", "Count"))))

                # d = ggplot(dot, aes(x = rank_by, y = reorder(plot_rank, plot_rank), fill = group)) +
                #     geom_col(width = 0.5) +
                #     scale_fill_manual(values = c("Down regulation"="blue", "Up regulation"="red"))+
                #     # labs(y = '', x = ytitle, title = "KEGG pathway enrichment\n", fill = expression("Pvalue")) +
                #     # theme(axis.title.x = element_text(size = input$kegg_x_title_size, color = 'black'),axis.title.y = element_text(size = input$kegg_y_title_size,face = 'bold'),
                #     #     plot.title = element_text(size=input$kegg_y_title_size,face = 'bold',hjust = 1.1),
                #     #     axis.text.x=element_text(vjust=1,size=input$kegg_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$kegg_y_text_size,colour = 'black'))+
                #     # theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
                #     geom_text(data = subset(dot, rank_by > 0),aes(y=plot_rank, x= -0.01, label= paste0("", DES)), color = "black",size = 3, hjust = 1)+
                #     geom_text(data = subset(dot, rank_by < 0),aes(y=plot_rank, x= 0.01, label= paste0("", DES)), color = "black",size = 3,hjust = 0) +
                #     theme_bw()+theme(panel.grid =element_blank())+
                #     theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_text(size=15), legend.title = element_blank(), axis.title.y = element_text(size=15), axis.text.y = element_blank())+
                #     theme(legend.key = element_rect(fill = 'transparent'),
                #             legend.position = c(0.01, 0.01),
                #             legend.justification = c(0.01, 0.01)) + 
                #     theme(panel.border = element_rect(fill=NA,color="black", size=0.5)) + xlim(-1,1) + 
                #     xlab(ytitle) + ylab("KEGG pathway enrichment\n")



                d = ggplot(dot, aes(x = rank_by, y = reorder(plot_rank, plot_rank), fill = group)) + 
                    geom_bar(stat = "identity", width = 0.8,position="dodge") + 
                    scale_fill_manual(values = c("Down regulation"=input$color_kegg_plot_low, "Up regulation"=input$color_kegg_plot_hight))+
                    geom_text(data = subset(dot, rank_by > 0),aes(y=plot_rank, x= -0.01, label= paste0("", DES)), color = "black",size = (input$kegg_y_text_size)/3, hjust = 1)+
                    geom_text(data = subset(dot, rank_by < 0),aes(y=plot_rank, x= 0.01, label= paste0("", DES)), color = "black",size = (input$kegg_y_text_size)/3,hjust = 0)+
                    theme_bw()+theme(panel.grid =element_blank())+
                    theme(axis.line.y = element_blank(), 
                            axis.ticks.y = element_blank(), 
                            axis.title.x = element_text(size=input$kegg_x_title_size), 
                            legend.title = element_blank(), 
                            axis.title.y = element_text(size=input$kegg_y_title_size), 
                            axis.text.y = element_blank(),
                            axis.text.x = element_text(size=input$kegg_x_text_size))+
                    theme(legend.key = element_rect(fill = 'transparent'),
                            legend.position = c(0.01, 0.01),
                            legend.justification = c(0.01, 0.01),
                            legend.text = element_text(size = input$kegg_x_title_size)) + 
                    theme(panel.border = element_rect(fill=NA,color="black", size=0.5)) + 
                    labs(x=ytitle,y="KEGG pathway enrichment",title=NULL)
            }



        }else {
            rich <- kegg_table()$rich_up
            dot = rich
            dot <- dot[order(dot$Pvalue),]

            if (nrow(dot) <= 10) {dot = dot}else 
                {if(nrow(dot)<=input$kegg_num_of_item){dot = nrow(dot)}else 
                    {dot = dot[1:input$kegg_num_of_item,]}
                }

            if(input$kegg_show_pathway == "NO") {
                dot$Description <- sub(" \\[.+\\d+\\]","", dot$Description)
            }

            dot$DES <- ifelse(stringr::str_count(dot$Description) > input$kegg_y_text_length, paste0(stringr::str_sub(dot$Description,end=input$kegg_y_text_length),' ...'), dot$Description)
            dot$Count <- as.numeric(sub("/\\d+", "", dot$GeneRatio))

            if(input$kegg_rank_by == "RichFactor"){
                dot$rank_by <- dot$RichFactor
                # dot <- dot[order(dot$rank_by, decreasing = T),]
                # rownames(dot) <- 1: nrow(dot)
            }else if (input$kegg_rank_by == "Pvalue") {
            dot$rank_by <- -dot$Pvalue
            #    dot <- dot[order(dot$rank_by, decreasing = T),]
            #    rownames(dot) <- 1: nrow(dot)
            }else if (input$kegg_rank_by == "P.adjust") {
            dot$rank_by <- -dot$P.adjust
            #    dot <- dot[order(dot$rank_by, decreasing = T),]
            #    rownames(dot) <- 1: nrow(dot)
            }else {
            dot$rank_by <- dot$Count
            #    dot <- dot[order(dot$rank_by, decreasing = T),]
            #    rownames(dot) <- 1: nrow(dot)
            }
            if(input$kegg_plot == "Dot plot"){
                d = ggplot(dot, aes(RichFactor, reorder(DES, rank_by)))+
                    geom_point() +
                    geom_point(aes(size=Count,color=Pvalue))+
                    scale_color_gradient(low=input$color_kegg_plot_hight,high=input$color_kegg_plot_low)+
                    labs(color=expression(Pvalue),size='Count',x='RichFactor',y='PathWay\n',title='KEGG pathway enrichment')+
                    theme_bw()+
                    theme(axis.title.x = element_text(size = input$kegg_x_title_size, color = 'black'),axis.title.y = element_text(size = input$kegg_y_title_size,face = 'bold'),
                        plot.title = element_text(size=input$kegg_y_title_size,face = 'bold',hjust = 1.1),
                        axis.text.x=element_text(vjust=1,size=input$kegg_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$kegg_y_text_size,colour = 'black'))+
                    theme(panel.grid.minor = element_blank())+guides(size = guide_legend(order = 1))
            }else if (input$kegg_plot == "Bar plot") {
                d = ggplot(dot, aes(reorder(DES, rank_by), RichFactor)) +
                    geom_col(aes(fill = Pvalue), width = 0.5) +
                    scale_fill_gradient(low=input$color_kegg_plot_hight,high=input$color_kegg_plot_low) +
                    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
                    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
                    coord_flip() +
                    labs(x = '', y = 'RichFactor', title = "KEGG pathway enrichment", fill = expression("Pvalue")) +
                    theme(axis.title.x = element_text(size = input$kegg_x_title_size, color = 'black'),axis.title.y = element_text(size = input$kegg_y_title_size,face = 'bold'),
                        plot.title = element_text(size=input$kegg_y_title_size,face = 'bold',hjust = 1.1),
                        axis.text.x=element_text(vjust=1,size=input$kegg_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$kegg_y_text_size,colour = 'black'))
            }
        }

        return(d)
    }else {
       return(NULL)
    }
})
output$kegg_plot_dot <- renderPlot(kegg_dot_plot())
output$download_kegg_bar_plot <- downloadHandler(#下载
    filename = function() { paste("KEGG_dot_plot", input$kegg_plot_format, sep = ".")},
    content = function(file) {
    p <- kegg_dot_plot()
    ggsave(file, p, width = input$kegg_width, height = input$kegg_height)
})



#go dot plot
go_dot_plot <- reactive({
    if(!is.null(go_table())){
        if(input$enrich_log2fc_go=="Contain"){
            # return(NULL)

            #UP
            rich <- go_table()$rich_up
            dot <- rich
            dot <- dot[order(dot$Pvalue),]

            bp <- dot[dot$ontology == "biological_process",]
            bp_num <- ifelse(nrow(bp) <= 10, nrow(bp), ifelse((nrow(bp) <= input$go_num_of_item), nrow(bp), input$go_num_of_item))
            bp <- bp[order(bp$Pvalue),][1:bp_num,]
            bp$group <- "Biological process"

            cc <- dot[dot$ontology == "cellular_component",]
            cc_num <- ifelse(nrow(cc) <= 10, nrow(cc), ifelse((nrow(cc) <= input$go_num_of_item), nrow(cc), input$go_num_of_item))
            cc <- cc[order(cc$Pvalue),][1:cc_num,]
            cc$group <- "Cellular component"

            mf <- dot[dot$ontology == "molecular_function",]
            mf_num <- ifelse(nrow(mf) <= 10, nrow(mf), ifelse((nrow(mf) <= input$go_num_of_item), nrow(mf), input$go_num_of_item))
            mf <- mf[order(mf$Pvalue),][1:mf_num,]
            mf$group <- "Molecular function"

            dot <- rbind(bp, cc)
            dot <- rbind(dot, mf)
            
            dot$DES <- ifelse(stringr::str_count(dot$Description) > input$go_y_text_length, paste0(stringr::str_sub(dot$Description,end=input$go_y_text_length),' ...'), dot$Description)
            dot$Count <- as.numeric(sub("/\\d+", "", dot$GeneRatio))

            if(input$go_rank_by == "RichFactor"){
                dot$rank_by <- dot$RichFactor
            }else if (input$go_rank_by == "Pvalue") {
            dot$rank_by <- -dot$Pvalue
            }else if (input$go_rank_by == "P.adjust") {
            dot$rank_by <- -dot$P.adjust
            }else {
            dot$rank_by <- dot$Count
            }
            dot_up <- dot

            #DOWN
            rich <- go_table()$rich_down
            dot <- rich
            dot <- dot[order(dot$Pvalue),]

            bp <- dot[dot$ontology == "biological_process",]
            bp_num <- ifelse(nrow(bp) <= 10, nrow(bp), ifelse((nrow(bp) <= input$go_num_of_item), nrow(bp), input$go_num_of_item))
            bp_num <- ifelse(bp_num >= input$go_num_of_item, input$go_num_of_item, bp_num)
            bp <- bp[order(bp$Pvalue),][1:bp_num,]
            bp$group <- "Biological process"

            cc <- dot[dot$ontology == "cellular_component",]
            cc_num <- ifelse(nrow(cc) <= 10, nrow(cc), ifelse((nrow(cc) <= input$go_num_of_item), nrow(cc), input$go_num_of_item))
            cc_num <- ifelse(cc_num >= input$go_num_of_item, input$go_num_of_item, cc_num)
            cc <- cc[order(cc$Pvalue),][1:cc_num,]
            cc$group <- "Cellular component"

            mf <- dot[dot$ontology == "molecular_function",]
            mf_num <- ifelse(nrow(mf) <= 10, nrow(mf), ifelse((nrow(mf) <= input$go_num_of_item), nrow(mf), input$go_num_of_item))
            mf_num <- ifelse(mf_num >= input$go_num_of_item, input$go_num_of_item, mf_num)
            mf <- mf[order(mf$Pvalue),][1:mf_num,]
            mf$group <- "Molecular function"

            dot <- rbind(bp, cc)
            dot <- rbind(dot, mf)
            
            dot$DES <- ifelse(stringr::str_count(dot$Description) > input$go_y_text_length, paste0(stringr::str_sub(dot$Description,end=input$go_y_text_length),' ...'), dot$Description)
            dot$Count <- as.numeric(sub("/\\d+", "", dot$GeneRatio))

            if(input$go_rank_by == "RichFactor"){
                dot$rank_by <- dot$RichFactor
            }else if (input$go_rank_by == "Pvalue") {
            dot$rank_by <- -dot$Pvalue
            }else if (input$go_rank_by == "P.adjust") {
            dot$rank_by <- -dot$P.adjust
            }else {
            dot$rank_by <- dot$Count
            }
            dot_down <- dot



            #plot
            if(input$go_plot == "Dot plot"){
                dot_up$group2 <- "Up regulation" 
                dot_down$group2 <- "Down regulation" 
                dot <- rbind(dot_up, dot_down)
                dot$group2 <- factor(dot$group2, levels=c('Up regulation', "Down regulation"))

                d_up = ggplot(dot_up, aes(RichFactor, reorder(DES, rank_by)))+
                    geom_point() +
                    geom_point(aes(size=Count,color=Pvalue))+
                    scale_color_gradient(low=input$color_go_plot_hight,high=input$color_go_plot_low)+
                    labs(color=expression(Pvalue),size='Count',x='RichFactor',y='GO pathway enrichment\n',title="Up regulation ")+
                    theme_bw()+
                    theme(axis.title.x = element_text(size = input$go_x_title_size, color = 'black'),axis.title.y = element_text(size = input$go_y_title_size,face = 'bold'),
                        plot.title = element_text(size=input$go_y_title_size,face = 'bold',hjust = 1.1),
                        axis.text.x=element_text(vjust=1,size=input$go_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$go_y_text_size,colour = 'black'))+
                    theme(panel.grid.minor = element_blank())+guides(size = guide_legend(order = 1))
                d_up <- d_up + facet_grid(group~., scale = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(size = input$go_x_title_size))

                d_down = ggplot(dot_down, aes(RichFactor, reorder(DES, rank_by)))+
                    geom_point() +
                    geom_point(aes(size=Count,color=Pvalue))+
                    scale_color_gradient(low=input$color_go_plot_hight,high=input$color_go_plot_low)+
                    labs(color=expression(Pvalue),size='Count',x='RichFactor',y='',title="Down regulation ")+
                    theme_bw()+
                    theme(axis.title.x = element_text(size = input$go_x_title_size, color = 'black'),axis.title.y = element_text(size = input$go_y_title_size,face = 'bold'),
                        plot.title = element_text(size=input$go_y_title_size,face = 'bold',hjust = 1.1),
                        axis.text.x=element_text(vjust=1,size=input$go_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$go_y_text_size,colour = 'black'))+
                    theme(panel.grid.minor = element_blank())+guides(size = guide_legend(order = 1))
                d_down <- d_down + facet_grid(group~., scale = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(size = input$go_x_title_size))

                d <- d_up + d_down




            }else if (input$go_plot == "Bar plot") {
                dot_up$group2 <- "Up regulation" 
                if(input$go_rank_by == "RichFactor"){
                    dot_up$rank_by <- dot_up$RichFactor
                    }else if (input$go_rank_by == "Pvalue") {
                    dot_up$rank_by <- -log10(dot_up$Pvalue)
                    }else if (input$go_rank_by == "P.adjust") {
                    dot_up$rank_by <- -log10(dot_up$P.adjust)
                    }else {
                    dot_up$rank_by <- dot_up$Count
                    }
                dot_up <- dot_up[order(dot_up$rank_by, decreasing = F),]

                dot_down$group2 <- "Down regulation" 
                if(input$go_rank_by == "RichFactor"){
                    dot_down$rank_by <- dot_down$RichFactor
                    }else if (input$go_rank_by == "Pvalue") {
                    dot_down$rank_by <- -log10(dot_down$Pvalue)
                    }else if (input$go_rank_by == "P.adjust") {
                    dot_down$rank_by <- -log10(dot_down$P.adjust)
                    }else {
                    dot_down$rank_by <- dot_down$Count
                    }

                dot_down$rank_by <- -dot_down$rank_by
                dot_down <- dot_down[order(dot_down$rank_by, decreasing = T),]

                dot <- rbind(dot_down, dot_up)
                dot$plot_rank <- 1:nrow(dot) # nolint


                #grid rank
                dot_bp <- dot[dot$group == "Biological process",]
                dot_bp$grid_rank <-  1:nrow(dot_bp) # nolint
                dot_cc <- dot[dot$group == "Cellular component",]
                dot_cc$grid_rank <-  1:nrow(dot_cc) # nolint # nolint
                dot_mf <- dot[dot$group == "Molecular function",]
                dot_mf$grid_rank <-  1:nrow(dot_mf) # nolint

                dot <- rbind(dot_bp, dot_cc)
                dot <- rbind(dot, dot_mf)


                ytitle <- ifelse(input$go_rank_by == "RichFactor", "RichFactor",
                            ifelse(input$go_rank_by == "Pvalue", "-log10(Pvalue)",
                                ifelse(input$go_rank_by == "P.adjust", "-log10(P.adjust)",
                                    ifelse(input$go_rank_by == "Count", "Count"))))

                d = ggplot(dot, aes(x = rank_by, y = reorder(grid_rank, grid_rank), fill = group2)) + 
                    geom_bar(stat = "identity", width = 0.8,position="dodge") + 
                    scale_fill_manual(values = c("Down regulation"=input$color_go_plot_low, "Up regulation"=input$color_go_plot_hight))+
                    theme_bw()+theme(panel.grid =element_blank())+
                    theme(axis.line.y = element_blank(), 
                            axis.ticks.y = element_blank(), 
                            axis.title.x = element_text(size=input$go_x_title_size), 
                            legend.title = element_blank(), 
                            axis.title.y = element_text(size=input$go_y_title_size), 
                            axis.text.y = element_blank(),
                            axis.text.x = element_text(size=input$go_x_text_size))+
                    theme(legend.key = element_rect(fill = 'transparent'),
                            legend.position = c(0.01, 0.01),
                            legend.justification = c(0.01, 0.01),
                            legend.text = element_text(size = input$go_x_title_size)) + 
                    theme(panel.border = element_rect(fill=NA,color="black", size=0.5)) + 
                    labs(x=ytitle,y="GO pathway enrichment\n",title=NULL)
                d <- d + facet_grid(group~., scale = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(size = input$go_x_title_size))
                d <- d + 
                    geom_text(data = subset(dot, rank_by > 0),aes(y=grid_rank, x= -0.01, label = DES), color = "black",size = (input$go_y_text_size)/3, hjust = 1)+
                    geom_text(data = subset(dot, rank_by < 0),aes(y=grid_rank, x= 0.01, label = DES), color = "black",size = (input$go_y_text_size)/3,hjust = 0)
                    
            }

        }else {
            rich <- go_table()$rich_up
            dot <- rich
            dot <- dot[order(dot$Pvalue),]

            bp <- dot[dot$ontology == "biological_process",]
            bp_num <- ifelse(nrow(bp) <= 10, nrow(bp), ifelse((nrow(bp) <= input$go_num_of_item), nrow(bp), input$go_num_of_item))
            bp_num <- ifelse(bp_num >= input$go_num_of_item, input$go_num_of_item, bp_num)
            # print(bp_num)
            bp <- bp[order(bp$Pvalue),][1:bp_num,]
            bp$group <- "Biological process"

            cc <- dot[dot$ontology == "cellular_component",]
            cc_num <- ifelse(nrow(cc) <= 10, nrow(cc), ifelse((nrow(cc) <= input$go_num_of_item), nrow(cc), input$go_num_of_item))
            cc_num <- ifelse(cc_num >= input$go_num_of_item, input$go_num_of_item, cc_num)
            # print(cc_num)
            cc <- cc[order(cc$Pvalue),][1:cc_num,]
            cc$group <- "Cellular component"

            mf <- dot[dot$ontology == "molecular_function",]
            mf_num <- ifelse(nrow(mf) <= 10, nrow(mf), ifelse((nrow(mf) <= input$go_num_of_item), nrow(mf), input$go_num_of_item))
            mf_num <- ifelse(mf_num >= input$go_num_of_item, input$go_num_of_item, mf_num)
            # print(mf_num)
            mf <- mf[order(mf$Pvalue),][1:mf_num,]
            mf$group <- "Molecular function"

            dot <- rbind(bp, cc)
            dot <- rbind(dot, mf)
            
            dot$DES <- ifelse(stringr::str_count(dot$Description) > input$go_y_text_length, paste0(stringr::str_sub(dot$Description,end=input$go_y_text_length),' ...'), dot$Description)
            dot$Count <- as.numeric(sub("/\\d+", "", dot$GeneRatio))

            if(input$go_rank_by == "RichFactor"){
                dot$rank_by <- dot$RichFactor
            }else if (input$go_rank_by == "Pvalue") {
            dot$rank_by <- -dot$Pvalue
            }else if (input$go_rank_by == "P.adjust") {
            dot$rank_by <- -dot$P.adjust
            }else {
            dot$rank_by <- dot$Count
            }
            
            if(input$go_plot == "Dot plot"){
                d <- ggplot(dot, aes(RichFactor, reorder(DES, rank_by)))+
                    geom_point() +
                    geom_point(aes(size=Count,color=Pvalue))+
                    scale_color_gradient(low=input$color_go_plot_hight,high=input$color_go_plot_low)+
                    labs(color=expression(Pvalue),size='Count',x='RichFactor',y='',title='Gene Ontology enrichment')+
                    theme_bw()+
                    theme(axis.title.x = element_text(size = input$go_x_title_size, color = 'black'),axis.title.y = element_text(size = input$go_y_title_size,face = 'bold'),
                        plot.title = element_text(size=input$go_y_title_size,face = 'bold',hjust = 1.1),
                        axis.text.x=element_text(vjust=1,size=input$go_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$go_y_text_size,colour = 'black'))+
                    theme(panel.grid.minor = element_blank())+guides(size = guide_legend(order = 1))
                d <- d + facet_grid(group~., scale = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(size = 12))

            }else if (input$go_plot == "Bar plot") {
                d <- ggplot(dot, aes(reorder(DES, rank_by), RichFactor)) +
                    geom_col(aes(fill = Pvalue), width = 0.5) +
                    scale_fill_gradient(low=input$color_go_plot_hight,high=input$color_go_plot_low) +
                    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
                    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
                    coord_flip() +
                    labs(x = '', y = 'RichFactor', title = "Gene Ontology enrichment") +
                    theme(axis.title.x = element_text(size = input$go_x_title_size, color = 'black'),axis.title.y = element_text(size = input$go_y_title_size,face = 'bold'),
                        plot.title = element_text(size=input$go_y_title_size,face = 'bold',hjust = 1.1),
                        axis.text.x=element_text(vjust=1,size=input$go_x_text_size),axis.text.y=element_text(vjust=0.3,size=input$go_y_text_size,colour = 'black'))
                d <- d + facet_grid(group~., scale = 'free_y', space = 'free_y') + theme(strip.text.y = element_text(size = 12))
            }
        }

        return(d)
    }else {
       return(NULL)
    }
})
output$go_plot_dot <- renderPlot(go_dot_plot())
output$download_go_bar_plot <- downloadHandler(#下载
    filename = function() { paste("GO_dot_plot", input$go_plot_format, sep = ".")},
    content = function(file) {
    p <- go_dot_plot()
    ggsave(file, p, width = input$go_width, height = input$go_height)
})
