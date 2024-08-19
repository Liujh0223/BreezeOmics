# summary bar plot
summary_DEGs <- reactive({
    if(is.null(deg())){return(NULL)}
    req(deg())

    files <- list.files(path = deg(), full.names = TRUE)

    result_df <- data.frame(file_name = character(), count = integer(), stringsAsFactors = FALSE)

    for (file in files) {
    # 读取文件
    data <- read.csv(file)
    data[,3] <- as.numeric(data[,3])
    data[,4] <- as.numeric(data[,4])
    data[,5] <- as.numeric(data[,5])
    data$Change <- ifelse(data[,4] < input$pvaluecutoff1 & abs(data[,3]) >= input$log2fccutoff1, 
                        ifelse(data[,3] >= input$log2fccutoff1, paste0("Up-regulation ( Log2FC ≥ ", input$log2fccutoff1, " & P ≤ ", input$pvaluecutoff1 , " )"), 
                               paste0("Down-regulation ( Log2FC ≤ ", -input$log2fccutoff1, " & P ≤ ", input$pvaluecutoff1 , " )"))
                        ,"No-change")
    # 计算第二列每个元素的数量
    counts <- table(data[[2]])
    
    # 将结果添加到数据框
    result_df <- rbind(result_df, data.frame(file_name = basename(file), count = counts, all = nrow(data), stringsAsFactors = FALSE))
    }


    # 计算百分比
    data <- result_df %>%
    group_by(file_name) %>%
    mutate(Percent = count.Freq / all * 100)

    ###groupinfo
    groupinfo <- result_degfile()
    groupinfo$file_name <- paste0(groupinfo$GroupName, "(TR=", groupinfo$TR, ", CO=", groupinfo$CO, ")")
    data$file_name <- factor(data$file_name, levels = groupinfo$file_name)

    # 绘制百分比堆叠柱状图
    a <- ggplot(data, aes(x = file_name, y = Percent, fill = count.Var1)) +
    geom_bar(stat = "identity", position = "fill") +  # position = "fill" 保证每根柱子高度为100%
    scale_y_continuous(labels = scales::percent) +  # y轴显示为百分比
    scale_fill_manual(values = c(input$color_down_summary_DEGs, input$color_not_summary_DEGs, input$color_up_summary_DEGs)) +
    labs(title = "Percentage Sales by summary of DEGs",
        x = NULL,
        y = "Percentage",
        fill = "Product") +
    theme_bw() +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title = element_text(size = input$title_size_summary_DEGs),
            axis.text.x = element_text(size = input$text_x_summary_DEGs, color = "black"),
            axis.text.y = element_text(size = input$text_y_summary_DEGs, color = "black"),
            legend.title = element_blank(),
            legend.text = element_text(size = input$legend_text_size_summary_DEGs),
            legend.position = "top",
            legend.direction = "vertical") + 
    geom_text(aes(label = paste0(count.Freq), y = Percent), 
                position = position_fill(vjust = 0.5), color = input$color_text_summary_DEGs, size = input$genes_size_summary_DEGs)
    return(a)
})
output$summary_DEGs <- renderPlot(summary_DEGs())
output$download_summary_DEGs <- downloadHandler(#下载
    filename = function() { paste("DEGs_summary", input$summary_DEGs_format, sep = ".")},
    content = function(file) {
    p <- summary_DEGs()
    ggsave(file, p, width = input$width_summary_DEGs, height = input$height_summary_DEGs)
})

# Volcano map
vol_plot <- reactive({
    if(!is.null(deg())){
        log2fccutoff1 <- input$log2fccutoff1
        pvaluecutoff1 <- input$pvaluecutoff1
        a <- getdegtable()# Gene, CGSNL_symbol, Change, DESeq2_log2FC, DESeq2_pvalue, DESeq2_padj, X1600.1_CPM, X1600.2_CPM, X1600.3_CPM, ZH11.1_CPM, ZH11.2_CPM, ZH11.3_CPM

        if(colnames(a)[2]=="Symbol"){
           colnames(a)[1:6] <- c("gene", "symbol", "change", "log2fc", "pvalue", "padj")
        }else {
           colnames(a)[1:5] <- c("gene", "change", "log2fc", "pvalue", "padj")
        }

        a$up <- NA
        a$down <- NA
        a$not <- NA

        if(!is.null(input$degresulttable_rows_selected)){
            if(length(a[input$degresulttable_rows_selected, 1]) >=6 ) {
                for(i in a[input$degresulttable_rows_selected, 1][1:6]){
                    if(a[a$gene == i, "change"] == "UP"){a[a$gene == i, "up"] <- i }
                    if(a[a$gene == i, "change"] == "DOWN"){a[a$gene == i, "down"] <- i }
                    if(a[a$gene == i, "change"] == "NOT"){a[a$gene == i, "not"] <- i }
                    }
            }else {
                for(i in a[input$degresulttable_rows_selected, 1]){
                    if(a[a$gene == i, "change"] == "UP"){a[a$gene == i, "up"] <- i }
                    if(a[a$gene == i, "change"] == "DOWN"){a[a$gene == i, "down"] <- i }
                    if(a[a$gene == i, "change"] == "NOT"){a[a$gene == i, "not"] <- i }
                    }
            }
        }

        xmin <- min(a$log2fc)
        xmax <- max(a$log2fc)
        if(abs(xmin) > abs(xmax)) {xlim = abs(xmin)} else {xlim = abs(xmax)}
        vol_title <- paste0("Analyze by ",input$differenceanalysis_software, " :  ","|log2FC| >= ", log2fccutoff1," & pvalue <= ", pvaluecutoff1,
                        "\n", nrow(a[a$change == "UP", ]), "  genes up regulation ",
                        "\n", nrow(a[a$change == "DOWN", ]),"  genes down regulation ")

        if(input$vol_plot_type == "vol"){
            vol <- ggplot(data = a, aes(x = log2fc, y = -log10(pvalue), color = change)) +
                geom_point(size = 1) + scale_color_manual(values = c(input$color_vol_plot_up, input$color_vol_plot_not, input$color_vol_plot_down), limits = c('UP', 'NOT', 'DOWN')) +
                theme_bw()+
                geom_vline(xintercept = c(-log2fccutoff1, log2fccutoff1), lty = 2, color = "black") + 
                labs(title = vol_title, color = '') + xlab(expression("log"["2"]*" (Fold Change)")) + ylab(expression("-log"["10"]*"(P Value)")) +
                geom_hline(yintercept = -log10(pvaluecutoff1), lty = 2, color = "black", size = 0.5) + xlim(-xlim,xlim) +
                theme(plot.title = element_text(size = 12, family = "sans")) +
                theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
                theme(legend.position = c(input$vol_plot_legend_x, input$vol_plot_legend_y), legend.title = element_blank(), 
                    legend.background = element_rect(fill = alpha('white', 1), color = "black", size = 0.5),
                    legend.key = element_rect(fill = alpha('white', 0)),
                    panel.background = element_rect(size = 0.5))
        }else if (input$vol_plot_type == "rank") {
            data <- a
            sorted_index <- order(data$log2fc)
            data <- data[sorted_index, ]
            data$rank <- 1:nrow(data) # nolint

            vol <- ggplot(data,aes(x=rank,y=log2fc,colour=change))+
                    geom_point(size=2)+ #这里的size是控制图里的所有点的大小
                    scale_color_manual(values = c(input$color_vol_plot_down, input$color_vol_plot_not, input$color_vol_plot_up))+
                    geom_hline(yintercept=c(log2fccutoff1,-log2fccutoff1),linetype=2,size=0.5)+
                    geom_hline(yintercept = c(0),linetype=1,size=0.5)+
                    labs(title = vol_title, color = '') + xlab('Rank of differentially expressed genes') + ylab(expression("log"["2"]*" (Fold Change)"))+
                    theme_bw() +
                    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
                    theme(legend.position = c(input$vol_plot_legend_x, input$vol_plot_legend_y), legend.title = element_blank(), 
                        legend.background = element_rect(fill = alpha('white', 1), color = "black", size = 0.5),
                        legend.key = element_rect(fill = alpha('white', 0)),
                        panel.background = element_rect(size = 0.5))
        }



        vol_label <- vol + ggrepel::geom_text_repel(aes(label = up),size = 4, fontface = "italic", segment.size = 0.5, direction = "x", hjust = 0, color = input$color_vol_plot_up)+
                     ggrepel::geom_text_repel(aes(label = down),size = 4, fontface = "italic", segment.size = 0.5, direction = "x", hjust = 0, color = input$color_vol_plot_down)+
                     ggrepel::geom_text_repel(aes(label = not),size = 4, fontface = "italic", segment.size = 0.5, direction = "x", hjust = 0, color = input$color_vol_plot_not)       

        ifelse(input$label_vol_plot, vol <- vol_label, vol)
            

    }else {
       return(NULL)
    }
    vol
})
output$download_vol_plot <- downloadHandler(#下载
    filename = function() { paste(input$selectgroupname, "Volcano", input$vol_plot_format, sep = ".")},
    content = function(file) {
    p <- vol_plot()
    ggsave(file, p, width = input$vol_plot_width, height = input$vol_plot_height)
})

# deg bar plot
deg_bar_1 <- reactive({
    gene <- input$degresulttable_rows_selected[1]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    data <- getdegtable()
    gene_id <- data[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- data[gene, ][, 6:ncol(data)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_1 <- renderPlot(deg_bar_1())

deg_bar_2 <- reactive({
    gene <- input$degresulttable_rows_selected[2]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    data <- getdegtable()
    gene_id <- data[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- data[gene, ][, 6:ncol(data)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_2 <- renderPlot(deg_bar_2())

deg_bar_3 <- reactive({
    gene <- input$degresulttable_rows_selected[3]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    
    data <- getdegtable()
    gene_id <- data[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- data[gene, ][, 6:ncol(data)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_3 <- renderPlot(deg_bar_3())

deg_bar_4 <- reactive({
    gene <- input$degresulttable_rows_selected[4]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    data <- getdegtable()
    gene_id <- data[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- data[gene, ][, 6:ncol(data)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_4 <- renderPlot(deg_bar_4())

deg_bar_5 <- reactive({
    gene <- input$degresulttable_rows_selected[5]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    data <- getdegtable()
    gene_id <- data[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- data[gene, ][, 6:ncol(data)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_5 <- renderPlot(deg_bar_5())

deg_bar_6 <- reactive({
    gene <- input$degresulttable_rows_selected[6]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    data <- getdegtable()
    gene_id <- data[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- data[gene, ][, 6:ncol(data)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_6 <- renderPlot(deg_bar_6())

output$download_deg_bar <- downloadHandler(#下载
    filename = function() { paste("DEGs_bar_plot", input$deg_bar_format, sep = ".")},
    content = function(file) {

    if(length(input$degresulttable_rows_selected)>=1){
        width = input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=2){
        width = 2 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=3){
        width = 3 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=4){
        width = 4 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), deg_bar_4(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=5){
        width = 5 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), deg_bar_4(), deg_bar_5(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=6){
        width = 6 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), deg_bar_4(), deg_bar_5(), deg_bar_6(),nrow=1)
        ggsave(file, p, width = width, height = height)
    }
})