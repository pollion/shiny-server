working_dir<-getwd()
source(paste(working_dir, "/functions.r",sep="",collapse=NULL))
source(paste(working_dir, "/data_preparation.r",sep="",collapse=NULL))

#########

shinyServer(
        
        function(input, output) {
                
                output$single_barplot_var1 <- renderPlot({
                        bar_plot <- ggplot(data, aes(x=data[,1]))
                        bar_plot <- bar_plot + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                        bar_plot <- bar_plot + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot <- bar_plot + xlab("")
                        bar_plot <- bar_plot + ylab("Relative Häufigkeit")
                        bar_plot <- bar_plot + theme(legend.position="none")
                        print(bar_plot)
                        
                        if (input$labelxvertical) {
                                bar_plot <- ggplot(data, aes(x=data[,1]))
                                bar_plot <- bar_plot + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot <- bar_plot + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot <- bar_plot + xlab("")
                                bar_plot <- bar_plot + ylab("Relative Häufigkeit")
                                bar_plot <- bar_plot + theme(legend.position="none")
                                print(bar_plot)
                        }
                        
                        if (input$Anzahl_Beobachtungen) {
                                bar_plot <- bar_plot + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot)
                        }
                        
                })
                
                output$summary1 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,1]))
                        sumtablepercent <- table(data[,1])/sum(table(data[,1]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        #oldnames<-c("var",colnames(sumtable))
                        #newnames<-c("Variable","Anzahl","Prozent")
                        #for(i in 1:length(colnames(sumtable))) {
                        #        names(sumtable)[names(sumtable)==oldnames[i]]=newnames[i]
                        #}
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                        
                })
                
                output$single_hist_var2 <- renderPlot({
                        histog <- ggplot(data, aes(x=data[,2]))
                        histog <- histog + geom_histogram(fill="#3AAA35") 
                        histog <- histog + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        histog <- histog + xlab("")
                        histog <- histog + ylab("Relative Häufigkeit")
                        print(histog)
                })
                
                
                
                output$summary2 <- renderTable({
                        #sumtable <- summary(as.matrix(data[,2]))
                        sumtable<-as.matrix(describe(data[,2]))
                        sumtable<-sumtable[,c(2:5,8:9)]
                        names(sumtable)[names(sumtable)=="x"] <- "Wert"
                        xtable(as.matrix(sumtable),type="html")
                })
                
                output$single_barplot_var3 <- renderPlot({
                        bar_plot3 <- ggplot(data, aes(x=data[,3]))
                        bar_plot3 <- bar_plot3 + geom_bar(aes(y = (..count..)/sum(..count..),fill="#3AAA35")) 
                        bar_plot3 <- bar_plot3 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot3 <- bar_plot3 + xlab("")
                        bar_plot3 <- bar_plot3 + ylab("Relative Häufigkeit")
                        bar_plot3 <- bar_plot3 + theme(legend.position="none")
                        print(bar_plot3)
                        
                        if (input$labelxvertical3) {
                                bar_plot3 <- ggplot(data, aes(x=data[,3]))
                                bar_plot3 <- bar_plot3 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot3 <- bar_plot3 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot3 <- bar_plot3 + xlab("")
                                bar_plot3 <- bar_plot3 + ylab("Relative Häufigkeit")
                                bar_plot3 <- bar_plot3 + theme(legend.position="none")
                                print(bar_plot3)
                        }
                        
                        if (input$Anzahl_Beobachtungen3) {
                                bar_plot3 <- bar_plot3 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot3)
                        }
                })
                
                output$summary3 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,3]))
                        sumtablepercent <- table(data[,3])/sum(table(data[,3]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$single_barplot_var4 <- renderPlot({
                        bar_plot4 <- ggplot(data, aes(x=data[,4]))
                        bar_plot4 <- bar_plot4 + geom_bar(fill="#3AAA35") 
                        bar_plot4 <- bar_plot4 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot4 <- bar_plot4 + xlab("")
                        bar_plot4 <- bar_plot4 + ylab("Relative Häufigkeit")
                        print(bar_plot4)
                        
                        if (input$labelxvertical4) {
                                bar_plot4 <- ggplot(data, aes(x=data[,4]))
                                bar_plot4 <- bar_plot4 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot4 <- bar_plot4 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot4 <- bar_plot4 + xlab("")
                                bar_plot4 <- bar_plot4 + ylab("Relative Häufigkeit")
                                bar_plot4 <- bar_plot4 + theme(legend.position="none")
                                print(bar_plot4)
                        }
                        
                        if (input$Anzahl_Beobachtungen4) {
                                bar_plot4 <- bar_plot4 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot4)
                        }
                })
                
                output$summary4 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,4]))
                        sumtablepercent <- table(data[,4])/sum(table(data[,4]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$single_barplot_var5 <- renderPlot({
                        bar_plot5 <- ggplot(data, aes(x=data[,5]))
                        bar_plot5 <- bar_plot5 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                        bar_plot5 <- bar_plot5 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot5 <- bar_plot5 + xlab("")
                        bar_plot5 <- bar_plot5 + ylab("Relative Häufigkeit")
                        bar_plot5 <- bar_plot5 + theme(legend.position="none")
                        print(bar_plot5)
                        
                        if (input$labelxvertical5) {
                                bar_plot5 <- ggplot(data, aes(x=data[,5]))
                                bar_plot5 <- bar_plot5 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot5 <- bar_plot5 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot5 <- bar_plot5 + xlab("")
                                bar_plot5 <- bar_plot5 + ylab("Relative Häufigkeit")
                                bar_plot5 <- bar_plot5 + theme(legend.position="none")
                                print(bar_plot5)
                        }
                        
                        if (input$Anzahl_Beobachtungen5) {
                                bar_plot5 <- bar_plot5 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot5)
                        }
                })
            
                output$summary5 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,5]))
                        sumtablepercent <- table(data[,5])/sum(table(data[,5]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$single_barplot_var6 <- renderPlot({
                        bar_plot6 <- ggplot(data, aes(x=data[,6]))
                        bar_plot6 <- bar_plot6 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                        bar_plot6 <- bar_plot6 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot6 <- bar_plot6 + xlab("")
                        bar_plot6 <- bar_plot6 + ylab("Relative Häufigkeit")
                        bar_plot6 <- bar_plot6 + theme(legend.position="none")
                        print(bar_plot6)
                        
                        if (input$labelxvertical6) {
                                bar_plot6 <- ggplot(data, aes(x=data[,6]))
                                bar_plot6 <- bar_plot6 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot6 <- bar_plot6 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot6 <- bar_plot6 + xlab("")
                                bar_plot6 <- bar_plot6 + ylab("Relative Häufigkeit")
                                bar_plot6 <- bar_plot6 + theme(legend.position="none")
                                print(bar_plot6)
                        }
                        
                        if (input$Anzahl_Beobachtungen6) {
                                bar_plot6 <- bar_plot6 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot6)
                        }
                })
                
                output$summary6 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,6]))
                        sumtablepercent <- table(data[,6])/sum(table(data[,6]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$single_barplot_var7 <- renderPlot({
                        bar_plot7 <- ggplot(data, aes(x=data[,7]))
                        bar_plot7 <- bar_plot7 + geom_bar(aes(y = (..count..)/sum(..count..),fill="#3AAA35")) 
                        bar_plot7 <- bar_plot7 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot7 <- bar_plot7 + xlab("")
                        bar_plot7 <- bar_plot7 + ylab("Relative Häufigkeit")
                        bar_plot7 <- bar_plot7 + theme(legend.position="none")
                        print(bar_plot7)
                        
                        if (input$labelxvertical7) {
                                bar_plot7 <- ggplot(data, aes(x=data[,7]))
                                bar_plot7 <- bar_plot7 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot7 <- bar_plot7 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot7 <- bar_plot7 + xlab("")
                                bar_plot7 <- bar_plot7 + ylab("Relative Häufigkeit")
                                bar_plot7 <- bar_plot7 + theme(legend.position="none")
                                print(bar_plot7)
                        }
                        
                        if (input$Anzahl_Beobachtungen7) {
                                bar_plot7 <- bar_plot7 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot7)
                        }
                })
                
                output$summary7 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,7]))
                        sumtablepercent <- table(data[,7])/sum(table(data[,7]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$single_barplot_var8 <- renderPlot({
                        bar_plot8 <- ggplot(data, aes(x=data[,8]))
                        bar_plot8 <- bar_plot8 + geom_bar(aes(y = (..count..)/sum(..count..),fill="#3AAA35")) 
                        bar_plot8 <- bar_plot8 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot8 <- bar_plot8 + xlab("")
                        bar_plot8 <- bar_plot8 + ylab("Relative Häufigkeit")
                        bar_plot8 <- bar_plot8 + theme(legend.position="none")
                        print(bar_plot8)
                        
                        if (input$labelxvertical8) {
                                bar_plot8 <- ggplot(data, aes(x=data[,8]))
                                bar_plot8 <- bar_plot8 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot8 <- bar_plot8 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot8 <- bar_plot8 + xlab("")
                                bar_plot8 <- bar_plot8 + ylab("Relative Häufigkeit")
                                bar_plot8 <- bar_plot8 + theme(legend.position="none")
                                print(bar_plot8)
                        }
                        
                        if (input$Anzahl_Beobachtungen8) {
                                bar_plot8 <- bar_plot8 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot8)
                        }
                })
                
                output$summary8 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,8]))
                        sumtablepercent <- table(data[,8])/sum(table(data[,8]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$single_barplot_var9 <- renderPlot({
                        bar_plot9 <- ggplot(data, aes(x=data[,9]))
                        bar_plot9 <- bar_plot9 + geom_bar(aes(y = (..count..)/sum(..count..),fill="#3AAA35")) 
                        bar_plot9 <- bar_plot9 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot9 <- bar_plot9 + xlab("")
                        bar_plot9 <- bar_plot9 + ylab("Relative Häufigkeit")
                        bar_plot9 <- bar_plot9 + theme(legend.position="none")
                        print(bar_plot9)
                        
                        if (input$labelxvertical9) {
                                bar_plot9 <- ggplot(data, aes(x=data[,9]))
                                bar_plot9 <- bar_plot9 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot9 <- bar_plot9 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot9 <- bar_plot9 + xlab("")
                                bar_plot9 <- bar_plot9 + ylab("Relative Häufigkeit")
                                bar_plot9 <- bar_plot9 + theme(legend.position="none")
                                print(bar_plot9)
                        }
                        
                        if (input$Anzahl_Beobachtungen9) {
                                bar_plot9 <- bar_plot9 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot9)
                        }
                })
                
                output$summary9 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,9]))
                        sumtablepercent <- table(data[,9])/sum(table(data[,9]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$single_barplot_var10 <- renderPlot({
                        bar_plot10 <- ggplot(data, aes(x=data[,10]))
                        bar_plot10 <- bar_plot10 + geom_bar(aes(y = (..count..)/sum(..count..),fill="#3AAA35")) 
                        bar_plot10 <- bar_plot10 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                theme(panel.background = element_rect(fill = 'transparent'),
                                      panel.grid.minor= element_blank(),
                                      panel.grid.major.x= element_blank(),
                                      panel.grid.major.y= element_line(color="#696969",size=.7),
                                      axis.line = element_line(color = "#696969",size=.7))
                        bar_plot10 <- bar_plot10 + xlab("")
                        bar_plot10 <- bar_plot10 + ylab("Relative Häufigkeit")
                        bar_plot10 <- bar_plot10 + theme(legend.position="none")
                        print(bar_plot10)
                        
                        if (input$labelxvertical10) {
                                bar_plot10 <- ggplot(data, aes(x=data[,10]))
                                bar_plot10 <- bar_plot10 + geom_bar(aes(y = (..count..)/sum(..count..), fill="#3AAA35"))
                                bar_plot10 <- bar_plot10 + theme_bw()+guides(fill = guide_legend(reverse = TRUE),guide=FALSE)+
                                        scale_fill_manual(values=c("#3AAA35","#EBEBEB"))+
                                        theme(panel.background = element_rect(fill = 'transparent'),
                                              panel.grid.minor= element_blank(),
                                              panel.grid.major.x= element_blank(),
                                              panel.grid.major.y= element_line(color="#696969",size=.7),
                                              axis.line = element_line(color = "#696969",size=.7),
                                              axis.text.x = element_text(angle = 90, hjust = 1))
                                bar_plot10 <- bar_plot10 + xlab("")
                                bar_plot10 <- bar_plot10 + ylab("Relative Häufigkeit")
                                bar_plot10 <- bar_plot10 + theme(legend.position="none")
                                print(bar_plot10)
                        }
                        
                        if (input$Anzahl_Beobachtungen10) {
                                bar_plot10 <- bar_plot10 + geom_text(stat="bin", color="black", vjust=-0.5, size=5,aes(y=(..count..)/sum(..count..), label=..count..), position = position_dodge(width=.5))
                                print(bar_plot10)
                        }
                })
                
                output$summary10 <- renderTable({ 
                        sumtablecount <- as.matrix(table(data[,10]))
                        sumtablepercent <- table(data[,10])/sum(table(data[,10]))*100
                        sumtable<-as.data.frame(cbind(sumtablecount,sumtablepercent))
                        names(sumtable)[names(sumtable)=="V1"] <- "Anzahl"
                        names(sumtable)[names(sumtable)=="sumtablepercent"] <- "Prozent"
                        xtable(sumtable,type="html")
                })
                
                output$wordcloud11 <- renderPlot({
                        wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
                })
                
                output$summary11 <- renderTable({ 
                        sumtablecount <- as.matrix(v)[1:10,]
                        xtable(as.data.frame(sumtablecount),type="html")
                })
                
                output$wordcloud12 <- renderPlot({
                        wordcloud(d2$word,d2$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
                })
                
                output$summary12 <- renderTable({ 
                        sumtablecount <- as.matrix(v2)[1:10,]
                        xtable(as.data.frame(sumtablecount),type="html")
                })
                
        }
)