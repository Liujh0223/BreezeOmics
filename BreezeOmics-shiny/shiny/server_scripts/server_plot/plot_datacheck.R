#corheatmap
plotcorheat <- reactive({
    if (is.null(countdata())) {
     return(NULL)
    }else {
        cpm <- cpm()
        cpm <- data.frame(cpm)
        data <- cpm[, apply(cpm, 2, var) != 0]
        newdata <- data[order(names(data))]
        matrix <-  cor(newdata)

        get_lower_tri<-function(cormat){
            cormat[lower.tri(cormat)] <- NA
            return(cormat)
        }
        matrix <- get_lower_tri(matrix)
        # print(matrix)
        melt_cor <- reshape2::melt(matrix, na.rm = TRUE)
        midpoint_cor <- (as.numeric(apply(melt_cor, 2, max)[3])+as.numeric(apply(melt_cor, 2, min)[3]))/2

        ggheatmap <- ggplot(data = melt_cor, aes(Var2, Var1, fill = value))+
                        geom_raster()+
                        scale_fill_gradient2(low = input$color_down_corheatmap, high = input$color_up_corheatmap, mid = input$color_mid_corheatmap, 
                                            midpoint = midpoint_cor, space = "Lab", 
                                            name="Pearson Correlation Coefficient") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = input$corheatmapangel, vjust = 1, size = 10, hjust = 0, colour = 'black'),
                            axis.text.y = element_text(vjust = 1, size = 10, hjust = 1, colour = 'black'),
                            legend.text = element_text(size = 10),
                            legend.title = element_text(size = 10, face = "bold"),
                            plot.background = element_rect(fill = "white", linewidth = 0, colour = "white", color = "white"),
                            plot.margin = unit(c(rep(input$corheatmapmargin, 4)), "cm")
                            )+
                        coord_fixed()
        
        if(input$corheatcor == T){
            ggheatmap + 
                geom_text(aes(Var2, Var1, label = sprintf("%.1f", value)), color = "black", size = input$corheatpcccex) +
                guides(fill = guide_colorbar(barwidth = 20, barheight = 1, title.position = "top", title.hjust = 0.5)) + 
                theme(
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.border = element_blank(),
                    axis.ticks = element_blank(),
                    legend.position = c(0.4, 0.95),
                    legend.direction = "horizontal"
                )
        }else {
            ggheatmap + 
                guides(fill = guide_colorbar(barwidth = 20, barheight = 1, title.position = "top", title.hjust = 0.5)) + 
                theme(
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.border = element_blank(),
                    axis.ticks = element_blank(),
                    legend.position = c(0.4, 0.95),
                    legend.direction = "horizontal"
                )
        }
    }
})
output$downloadcorheatmap <- downloadHandler(#下载
    filename = function() { paste("cor_heatmap", input$corheatmapformat, sep = ".")},
    content = function(file) {
    p <- plotcorheat()
    ggsave(file, p, width = input$corheatmapwidth, height = input$corheatmapheight)
})

#PCA
pcaprcomp <- reactive({#pca prcomp 返回ord
    if (is.null(cpm())) {
        return(NULL)
    } else {
        cpm <- cpm()
        cpm1 <- cpm[apply(cpm, 1, sum) != 0, ]
        cpm1 <- na.omit(cpm1)
        cpm1  <- t(cpm1)
        
        ord <- prcomp(cpm1, scale = input$pcascale)
    }
    ord
})
screeplot <- reactive({#pca碎石图
    if (is.null(pcaprcomp())) {
    return(NULL)
    } else {
    ord <- pcaprcomp()
    ord <- summary(ord)
    summ1 <- data.frame(ord$importance)

    if (ncol(summ1) >= 10) {
    summ2 <- as.data.frame(t(summ1[, 1:10]))
    } else {
    summ2 <- as.data.frame(t(summ1))
    }

    summ3 <- data.frame(label = rownames(summ2), label2 = summ2$`Proportion of Variance`)
    #print(summ3)
    # print(summ1)
    a <- ggplot(summ3, aes(reorder(label, -label2), label2, group = 1)) +
        geom_bar(stat="identity",fill="#4878B6") +
        geom_line(stat="identity",color="black") +
        theme(  panel.background = element_rect(fill = "white"), 
                panel.grid = element_line(colour = "#DDE5F0"), 
                legend.position = "none", 
                axis.title = element_text(size = 15, color = "black"),
                axis.text = element_text(size = 12, color = "black"),
                plot.title = element_text(size = 15, color = "black"),
            ) +
        geom_text(data = summ3,aes(y=label2, x= label, label= paste0(round(label2,2)*100, "%")), color = "black",size = 4, hjust = 0.4, vjust=-0.8) +
        labs(title= "Scree Plot", x="Dimensions", y="Perecentage of Variance")+
        coord_cartesian(ylim = c(0, max(summ3$label2) * 1.1))

    ## table
    table_tbl <- round(pcaproportiontable(), 2)
    tbl <- gridExtra::tableGrob(table_tbl, rows=row.names(table_tbl), theme= gridExtra::ttheme_minimal())
    tbl <- gtable::gtable_add_grob(tbl, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(tbl), l = 1, r = ncol(tbl))
    tbl <- gtable::gtable_add_grob(tbl, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(tbl))
    b <- gridExtra::grid.arrange(tbl, a, nrow = 2, as.table=TRUE, heights=c(2,5))


    return(b)
    }

})
pcaproportiontable <- reactive({#pcatable
    if (is.null(pcaprcomp())) {
    return(NULL)
    } else {
        ord <- pcaprcomp()
        summ <- summary(ord)
        summ1 <- data.frame(summ$importance)
        if (ncol(summ1) >= 5) {
        summ2 <- summ1[, 1:5]
        } else {
        summ2 <- summ1
        }
    }
    summ2
})
pcaplot <- reactive({
    if (is.null(groupinfo())) {
    groupinfo <- groupinfotemplate()
    # print(groupinfo)
    }else {
        groupinfo <- groupinfo()
        # print(groupinfo)
        groupinfo <- data.frame(groupinfo$group, row.names = groupinfo$X1)
        colnames(groupinfo) <- "group"
    }
    pcaprcomp <- pcaprcomp()


    if (ncol(pcaprcomp$x) >= 6) {
    pccount <- 6
    } else {
        pccount <- ncol(pcaprcomp$x)
    }
    #print(pcaprcomp$x[, 1: pccount]) #for debug
    dt <- pcaprcomp$x[, 1: pccount]

    xx <- input$pcapx#输入x的dimension
    yy <- input$pcapy#输入y的dimension
    summ <- summary(pcaprcomp)
    xlab <- paste0("PC", xx, "(", round(summ$importance[2, xx] * 100, 2), "%)")#xdimension百分比
    ylab <- paste0("PC", yy, "(", round(summ$importance[2, yy] * 100, 2), "%)")#ydimension百分比

    dtt <- cbind(dt[, xx], dt[, yy])
    dtt <- cbind(dtt, groupinfo)
    colnames(dtt) <- c("x", "y", "group")
    # print(dtt)
    xmin <- min(dtt[, 1:2]$x) - (input$pcapaxis * 100)
    xmax <- max(dtt[, 1:2]$x) + (input$pcapaxis * 100)
    ymin <- min(dtt[, 1:2]$y) - (input$pcapaxis * 100)
    ymax <- max(dtt[, 1:2]$y) + (input$pcapaxis * 100)

    p1 <- ggplot2::ggplot(data = dtt[, 1:2],aes(x = x, y = y, color = dtt$group)) +
        geom_point(shape = 17, size = 2.5) + labs(x = xlab, y = ylab, color = "group") +
        theme_bw() + theme(legend.key = element_rect(fill = "transparent"), panel.background = element_blank()) + 
        coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + guides(color = guide_legend(override.aes = list(size = 3)))

    voal_label <- ggforce::geom_mark_ellipse(aes(fill = dtt$group,color = dtt$group, label = dtt$group), alpha = 0.1, expand = 0.01, show.legend = F)
    voal <- ggforce::geom_mark_ellipse(aes(fill = dtt$group,color = dtt$group), alpha = 0.1, expand = 0.01, show.legend = F)
    sample_name <- geom_text(label = rownames(dtt), size=3.5, vjust = -1, color='black')
    grid_blank <- theme(panel.grid = element_blank())
    borders_blank <- theme(panel.border = element_blank())
    hline_blank <- geom_hline(yintercept = 0, lty = 2, color = "black")
    vline_blank <- geom_vline(xintercept = 0, lty = 2, color = "black")
    ifelse(input$pcaguidelines, p1 <- p1 + hline_blank + vline_blank, p1)
    ifelse(input$pcaborders, p1, p1 <- p1 + borders_blank)
    ifelse(input$pcagrid, p1, p1 <- p1 + grid_blank)
    ifelse(input$pcaellipse & input$pcaellipselable, p1 <- p1 + voal_label,
        ifelse(input$pcaellipse & input$pcaellipselable == FALSE, p1 <- p1 + voal, p1))
    ifelse(input$pcasamplename, p1 <- p1 + sample_name, p1)
    p1
})
output$downloadpca <- downloadHandler(
    filename = function() { paste("pca", input$pcaformat, sep = ".")},
    content = function(file) {
    p <- pcaplot()
    ggsave(file, p, width = input$pcawidth, height = input$pcaheight)}
)
output$downloadscreeplot <- downloadHandler(
    filename = function() { paste("PCA_Scree_plot", input$pcaformat, sep = ".")},
    content = function(file) {
    p <- screeplot()
    ggsave(file, p, width = input$pcawidth, height = input$pcaheight)}
)

#top 500
plottop500 <- reactive({
    if (is.null(cpm())) {
    return(NULL)
    }else {
        cpm <- cpm()
        samplecount <- ncol(cpm)
        cpm$sd <- apply(cpm, 1, var)
        # for bug
        cpm <- cpm[cpm$sd > 0,]
        cpm_nrow <- ifelse(nrow(cpm) < 500, nrow(cpm), 500)

        cpm_sd <- cpm[order(cpm$sd, decreasing = TRUE), ][1:cpm_nrow, ]
        cpm500 <- cpm_sd[, 1:samplecount]
        cpm500 <- scale(t(cpm500))
        cpm500 <- data.frame(cpm500)
        if (is.null(groupinfo())) {
        if (samplecount >= 18) {
        cpm500 <- t(cpm500)
        gg <- hclust(dist(cpm500))
        cpm500 <- cpm500[gg$order,]
        cpm500 <- as.matrix(cpm500)
        melt_cor <- reshape2::melt(cpm500, na.rm = TRUE)
        p <- ggplot(data = melt_cor, aes(x=Var1, y=Var2, fill=value)) + 
                geom_raster()+
                scale_fill_gradient2(low=input$color_down_top500, high=input$color_up_top500, mid=input$color_mid_top500,name="Expression")+
                theme_minimal()+
                guides(fill = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5))+
                theme(
                panel.grid.major=element_blank(),
                axis.text.x = element_blank(),
                axis.title = element_blank(),
                axis.text.y = element_text(vjust = 1, size = 10, hjust = 1,colour = 'black'),
                legend.title = element_text(size = 9, face = "bold")
                )
            # cpm500 <- cpm500
            # p <- pheatmap::pheatmap(cpm500, show_rownames = T, cluster_rows = F, cluster_cols = T, show_colnames = F, treeheight_col = 0, treeheight_row = 0)
            # grid::grid.draw(p)
        }else {
        zz <- hclust(dist(t(cpm500)))
        cpm500 <- cpm500[,zz$order]
        cpm500 <- as.matrix(cpm500)
        melt_cor <- reshape2::melt(cpm500, na.rm = TRUE)
        p <- ggplot(data = melt_cor, aes(x=Var1, y=Var2, fill=value)) + 
                geom_raster()+
                scale_fill_gradient2(low=input$color_down_top500, high=input$color_up_top500, mid=input$color_mid_top500,name="Expression")+
                theme_minimal()+
                guides(fill = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5))+
                theme(
                panel.grid.major=element_blank(),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_text(angle = -45, vjust = 1, size = 10, hjust = 0,colour = 'black'),
                legend.title = element_text(size = 9, face = "bold")
                )
            # cpm500 <- t(cpm500)
            # p <- pheatmap::pheatmap(cpm500, show_rownames = F, cluster_rows = T, cluster_cols = F, show_colnames = T, treeheight_col = 0, treeheight_row = 0, angle_col = 315)
            # grid::grid.draw(p)
        }
        }else {
        groupinfo <- groupinfo()
        colnames(groupinfo) <- c("variable", "group")
        # annotation_row <- data.frame(groupinfo$group)
        # rownames(annotation_row) <- groupinfo$variable
        
        group <- mutate(groupinfo, Y = "group")
        set.seed(input$top500groupcolor)
        sample_color <- sample(colors(), length(unique(group$group)))
        sample_color <- data.frame(group = unique(group$group), sample_color = sample_color)
        group <- dplyr::inner_join(group, sample_color, by = "group")
        
        if (samplecount >= 18) {
        anno <- ggplot(group,aes(x=Y,y=variable,fill=group)) + geom_raster() + theme_void() + guides(fill = "none") + scale_fill_manual(values = sample_color$sample_color)
        cpm500 <- t(cpm500)
        gg <- hclust(dist(cpm500))
        cpm500 <- cpm500[gg$order,]
        cpm500 <- as.matrix(cpm500)
        melt_cor <- reshape2::melt(cpm500, na.rm = TRUE)
        p <- ggplot(data = melt_cor, aes(x=Var1, y=Var2, fill=value)) + 
                geom_raster()+
                scale_fill_gradient2(low=input$color_down_top500, high=input$color_up_top500, mid=input$color_mid_top500,name="Expression")+
                theme_minimal()+
                guides(fill = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5))+
                theme(
                panel.grid.major=element_blank(),
                axis.text.x = element_blank(),
                axis.title = element_blank(),
                axis.text.y = element_text( vjust = 1, size = 10, hjust = 1,colour = 'black'),
                legend.title = element_text(size = 9, face = "bold")
                )
            p <- p %>% aplot::insert_right(anno, width = .05)
            # cpm500 <- cpm500
            # p <- pheatmap::pheatmap(cpm500, show_rownames = T, cluster_rows = F, cluster_cols = T, show_colnames = F, treeheight_col = 0, treeheight_row = 0,
            #                    annotation_row = annotation_row, annotation_legend = F, annotation_names_row = F)
            # grid::grid.draw(p)
        }else {
            anno <- ggplot(group,aes(x=variable,y=Y,fill=group)) + geom_raster() + theme_void() + guides(fill = "none")+ scale_fill_manual(values = sample_color$sample_color)
            zz <- hclust(dist(t(cpm500)))
            cpm500 <- cpm500[,zz$order]
            cpm500 <- as.matrix(cpm500)
            melt_cor <- reshape2::melt(cpm500, na.rm = TRUE)
            p <- ggplot(data = melt_cor, aes(x=Var1, y=Var2, fill=value)) + 
            geom_raster()+
            scale_fill_gradient2(low=input$color_down_top500, high=input$color_up_top500, mid=input$color_mid_top500,name="Expression")+
            theme_minimal()+
            guides(fill = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5))+
            theme(
                panel.grid.major=element_blank(),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_text(angle = -45, vjust = 1, size = 10, hjust = 0,colour = 'black'),
                legend.title = element_text(size = 9, face = "bold")
                )
            p <- p %>% aplot::insert_top(anno, height = .05)
            # cpm500 <- t(cpm500)
            # p <- pheatmap::pheatmap(cpm500, show_rownames = F, cluster_rows = T, cluster_cols = F, show_colnames = T, treeheight_col = 0, treeheight_row = 0, angle_col = 315,
            #                    annotation_col = annotation_row, annotation_legend = FALSE, annotation_names_col = F)
            # grid::grid.draw(p)
        }

        }


    }
    p
})
output$downloadtoop500 <- downloadHandler(#下载
    filename = function() { paste("variance top 500", input$top500format, sep = ".")},
    content = function(file) {
    p <- plottop500()
    ggsave(file, p, width = input$top500width, height = input$top500height)
})

#Hierarchical clustering
datacheck_hcluster <- reactive({
    if (is.null(countdata())) {
     return(NULL)
    }else {
        cpm <- cpm()
        data_Expr <- log2(t(cpm)+1)
        datExpr_tree <- hclust(dist(data_Expr), method = "average")

        if (is.null(groupinfo())) {
        groupinfo <- groupinfotemplate()
        # print(groupinfo)
        }else {
            groupinfo <- groupinfo()
            # print(groupinfo)
            groupinfo <- data.frame(groupinfo$group, row.names = groupinfo$X1)
            colnames(groupinfo) <- "group"
        }
        group <- data.frame(groupinfo$group, row.names = groupinfo$X1)
        colnames(groupinfo) <- "group"
        set.seed(input$datacheck_hc_seed)
        sample_colors1 <- WGCNA::numbers2colors(as.numeric(factor(group$group)), 
                                 colors = sample(colors(), length(unique(group$group))),signed = FALSE)

        col=as.matrix(data.frame(Group=sample_colors1))

        WGCNA::plotDendroAndColors(datExpr_tree, col,
                            groupLabels = colnames(sample),
                            cex.dendroLabels = input$datacheck_hc_cex,
                            cex.colorLabels = input$datacheck_hc_cex,
                            marAll = c(input$datacheck_hc_bottom, input$datacheck_hc_left, input$datacheck_hc_top, input$datacheck_hc_right),
                            cex.rowText = input$datacheck_hc_cex,
                            main = NA)
        return(list(datExpr_tree = datExpr_tree, col = col, sample = sample))

    }
})
output$downloaddatacheck_hc <- downloadHandler(
    filename = function() { paste("Hierarchical clustering", input$datacheck_hc_format, sep = ".")},
    content = function(file) {
    if(input$datacheck_hc_format == "png"){
        width = input$datacheck_hc_width *100
        height = input$datacheck_hc_height *100
        png(file, width = width, height = height)
            WGCNA::plotDendroAndColors(datacheck_hcluster()$datExpr_tree, datacheck_hcluster()$col,
                                groupLabels = colnames(datacheck_hcluster()$sample),
                                cex.dendroLabels = input$datacheck_hc_cex,
                                cex.colorLabels = input$datacheck_hc_cex,
                                marAll = c(input$datacheck_hc_bottom, input$datacheck_hc_left, input$datacheck_hc_top, input$datacheck_hc_right),
                                cex.rowText = input$datacheck_hc_cex,
                                main = NA)
        dev.off()
    }else {
        pdf(file, width = input$datacheck_hc_width, height = input$datacheck_hc_height)
            WGCNA::plotDendroAndColors(datacheck_hcluster()$datExpr_tree, datacheck_hcluster()$col,
                                groupLabels = colnames(datacheck_hcluster()$sample),
                                cex.dendroLabels = input$datacheck_hc_cex,
                                cex.colorLabels = input$datacheck_hc_cex,
                                marAll = c(input$datacheck_hc_bottom, input$datacheck_hc_left, input$datacheck_hc_top, input$datacheck_hc_right),
                                cex.rowText = input$datacheck_hc_cex,
                                main = NA)
        dev.off()
    }

})