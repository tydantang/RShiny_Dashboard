#Load packages
library(shiny)
library(DT)
library(tidyr)

#Define input parameters and contents of plots and tables 
shinyServer(function(input, output, session) {
    
    #Create a reactive expression that loads user-uploaded csv data
    df_o <- reactive({
        req(input$file1)
        inFile <- input$file1
        read.csv(inFile$datapath, header = TRUE, fileEncoding = "UTF-8-BOM")
    })

    #Update input parameters based on the user-uploaded data
    observe({
        ndf_o <- df_o()[,sapply(df_o(), is.numeric) & colnames(df_o()) != "ID"]
        cdf_o <- df_o()[,sapply(df_o(), is.factor) & colnames(df_o()) != "ID"]
        updateCheckboxGroupInput(session, "SelIn1",
                                label = "Variables",
                                choices = names(ndf_o),
                                selected = names(ndf_o)[1])
        updateSelectInput(session, "SelIn2",
                          label = "Group by",
                          choices = c(names(cdf_o), "none"),
                          selected = "none")
        updateSelectInput(session, "SelIn3",
                          label = "Filter Categories",
                          choices = c(names(cdf_o), "none"),
                          selected = "none")
    })
    
    #Update other input parameters on click of some specific input parameters
    observeEvent(input$SelIn2,{
        cdf_o <- df_o()[,sapply(df_o(), is.factor) & colnames(df_o()) != "ID"]
        updateSelectInput(session, "SelIn3",
                          label = "Filter Categories",
                          choices = c(names(cdf_o)[!(names(cdf_o) == input$SelIn2)], "none"),
                          selected = "none")
    })
    
    #Define the content of a UI that only appears when triggered by other inputs 
    output$reactUI1 <- renderUI({
        req(input$SelIn3!="none")
        cdf_o <- df_o()[,sapply(df_o(), is.factor) & colnames(df_o()) != "ID"]
        selectInput("subUI1",
                    "Filter Subcategories",
                    choices = unique(cdf_o[,input$SelIn3]),
                    selected = unique(cdf_o[,input$SelIn3])[1])
    })
    
    #Create a download button for downloading the plot in the corresponding tab 
    output$download1 <- renderUI({
        req(input$SelIn1)
        downloadButton("down1", "Download the plot")
    })
    
    #Another download button
    output$download2 <- renderUI({
        req(input$SelIn1)
        downloadButton("down2", "Download the plot")
    })
    
    #Update data frame by the filter
    df <- reactive({
        if (input$SelIn3 == "none"){
            df_o()
        } else{
            req(input$subUI1)
            df_o()[df_o()[, input$SelIn3] == input$subUI1,]
        }
    })

    #Define the boxplot
    output$boxchart <- renderPlot({
        req(input$SelIn1)
        if (input$SelIn3 != "none"){
            req(input$subUI1)
        }
        gdf <- gather(df(), Var, Quantity, input$SelIn1)
        V_len <- length(input$SelIn1)
        par(mar=c(11,5,5,1))
        if (input$SelIn2 == "none"){
            x <- boxplot(df()[, input$SelIn1], data=df(), notch=FALSE,
                         col = c("gold","lightgreen", "lightblue", "lightcoral", "lightsteelblue", "cyan"),
                         ylab = "Value",
                         show.names=TRUE,
                         names=input$SelIn1,
                         medcol = "white",
                         main = "Boxplot (median value)",
                         las = 2)
            # points(1:length(input$SelIn1), x$stats[3,], col = "red")
            text(1:length(input$SelIn1), x$stats[3,], labels = x$stats[3,])
        } else{
            x <- boxplot(gdf[, "Quantity"] ~ Var + get(input$SelIn2), data=gdf, notch=FALSE,
                         col = c(rep("gold", V_len),
                               rep("lightgreen", V_len),
                               rep("lightblue", V_len),
                               rep("lightcoral", V_len)),
                         ylab = "Value",
                         medcol = "white",
                         main = "Boxplot (median value)",
                         las = 2)
            # points(1:(length(input$SelIn1)*length(unique(df()[, input$SelIn2]))), x$stats[3,], col = "red")
            text(1:(length(input$SelIn1)*length(unique(df()[, input$SelIn2]))), x$stats[3,], labels = x$stats[3,])
        }
        if (input$SelIn3 != "none"){
            mtext(text=paste("Filtered by", input$SelIn3, "=", input$subUI1), side=3)
        }
    })
    
    #Define the trend plot
    output$linechart <- renderPlot({
        par(mar=c(11,5,5,1))
        x <- grepl("day", tolower(colnames(df())))
        stripchart(df()[, x], main = "Trend (mean value)", vertical = TRUE, method = "stack", pch = 16, ylab = " Value (%)", col="lightblue", las = 2)
        y <- round(colMeans(df()[, x]), 2)
        points(1:sum(x), y, col = "red", lwd = 3)
        text(1:sum(x), y, labels = y)
        lines(1:sum(x), y, col="blue", lwd=3)
        
        if (input$SelIn3 != "none"){
            mtext(text=paste("Filtered by", input$SelIn3, "=", input$subUI1), side=3)
        }
    })
    
    #Define the data table
    output$tablechart1 <- DT::renderDataTable({
        df()[, sapply(df(), is.factor) | colnames(df()) == "ID"| colnames(df()) %in% c(input$SelIn1)]
    })
    
    #Define the summary table
    output$tablechart2 <- DT::renderDataTable({
        if (input$SelIn2 == "none"){
            round(data.frame(do.call(cbind, lapply(df()[, input$SelIn1, drop=FALSE], summary))), 2)
        } else{
            x <- do.call(data.frame, aggregate(df()[, c(input$SelIn1), drop=FALSE], by=list(df()[, input$SelIn2]), FUN = summary))
            x[,-1] <- round(x[,-1], 2)
            setNames(data.frame(t(x[,-1])), x[,1])
        }
    })
    
    #Define the boxplot to download on click of download button
    output$down1 <-  downloadHandler(
        filename = function() {
            paste(input$SelIn1, "pdf", sep = ".")
        },
        content = function(file) {
            pdf(file)
            
            gdf <- gather(df(), Var, Quantity, input$SelIn1)
            V_len <- length(input$SelIn1)
            par(mar=c(11,5,5,1))
            if (input$SelIn2 == "none"){
                x <- boxplot(df()[, input$SelIn1], data=df(), notch=FALSE,
                             col = c("gold","lightgreen", "lightblue", "lightcoral", "lightsteelblue", "cyan"),
                             ylab = "Value",
                             show.names=TRUE,
                             names=input$SelIn1,
                             medcol = "white",
                             main = "Boxplot (median value)",
                             las = 2)
                # points(1:length(input$SelIn1), x$stats[3,], col = "red")
                text(1:length(input$SelIn1), x$stats[3,], labels = x$stats[3,])
            } else{
                x <- boxplot(gdf[, "Quantity"] ~ Var + get(input$SelIn2), data=gdf, notch=FALSE,
                             col = c(rep("gold", V_len),
                                     rep("lightgreen", V_len),
                                     rep("lightblue", V_len),
                                     rep("lightcoral", V_len)),
                             ylab = "Value",
                             medcol = "white",
                             main = "Boxplot (median value)",
                             las = 2)
                # points(1:(length(input$SelIn1)*length(unique(df()[, input$SelIn2]))), x$stats[3,], col = "red")
                text(1:(length(input$SelIn1)*length(unique(df()[, input$SelIn2]))), x$stats[3,], labels = x$stats[3,])
            }
            if (input$SelIn3 != "none"){
                mtext(text=paste("Filtered by", input$SelIn3, "=", input$subUI1), side=3)
            }
            
            dev.off()
        }
    )
    
    #Define the trend plot to download on click of download button
    output$down2 <-  downloadHandler(
        filename = function() {
            paste(input$SelIn1, "pdf", sep = ".")
        },
        content = function(file) {
            pdf(file)
            
            par(mar=c(11,5,5,1))
            x <- grepl("day", tolower(colnames(df())))
            stripchart(df()[, x], main = "Trend (mean value)", vertical = TRUE, method = "stack", pch = 16, ylab = " Value (%)", col="lightblue", las = 2)
            y <- round(colMeans(df()[, x]), 2)
            points(1:sum(x), y, col = "red", lwd = 3)
            text(1:sum(x), y, labels = y)
            lines(1:sum(x), y, col="blue", lwd=3)
            
            if (input$SelIn3 != "none"){
                mtext(text=paste("Filtered by", input$SelIn3, "=", input$subUI1), side=3)
            }
            
            dev.off()
        }
    )
})
