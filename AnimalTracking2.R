
#Install these pacakges
library(EBImage)
library(shiny)
library(dplyr)
library(ggpubr)
library(av)
library(RColorBrewer)
library(shinythemes)

#If EBImage package download causes issues due to version, do this:
install.packages("BiocManager") 
BiocManager::install("EBImage")
library(EBImage)


##### START #####

#Set working directory to file containing avi videos
VideosWD<-"C:/Users/anton/OneDrive/Desktop/CPA_Test/Videos"
setwd(VideosWD)

#Run the rest of the code at once
Videos<-Sys.glob("*.avi")
for (i in 1:length(Videos)){
  file.rename(Videos[i], paste0(i,".avi"))}
unlink(paste0(getwd(),"/*.jpg"))
First<-Sys.glob("*.avi")
Convert<-av_video_images(First[1], 
                         destdir = VideosWD,
                         format = "jpg", fps = 1)

Images<-Sys.glob("*.jpg")
imgdim <-readImage(Images[1])
dim<-dim(imgdim)

ui <- fluidPage(theme = shinytheme("darkly"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("GroupNames", "Group names", value = "Control,Experimental", width = NULL, placeholder = NULL),
                    textInput("Group1", "Members of GROUP 1", value = "1,3", width = NULL, placeholder = NULL),
                    textInput("Group2", "Members of GROUP 2", value = "2,4", width = NULL, placeholder = NULL),
                    textInput("SessionNames", "Session names", value = "B1", width = NULL, placeholder = NULL),
                    textInput("ChamberNames", "Chamber names", value = "Horizontal/Cherry,Vertical/Honey", width = NULL, placeholder = NULL),
                    sliderInput("Select",
                                label = "Select Image:",
                                min = 1,
                                max = length(Images),
                                value = 1),
                    sliderInput("x",
                                label = "Cropping X Axis:",
                                min = 0,
                                max = dim[1],
                                value = c(280,430)),
                    sliderInput("y",
                                label = "Cropping Y Axis:",
                                min = 0,
                                max = dim[2],
                                value = c(90,435)),
                    sliderInput("Blur",
                                label = "Blurring:",
                                min = 1,
                                max = 10,
                                value = 3),
                    sliderInput("Threshold",
                                label = "Threshold:",
                                min = 0,
                                max = 0.1,
                                value = 0.07),
                    checkboxInput("box_checked", "Apply Threshold", value = FALSE),
                    sliderInput("SamplingRate",
                                label = "Sampling rate (FPS):",
                                min = 0.25,
                                max = 20,
                                value = 1),
                    actionButton('Track', 'Track Animals'),
                    
                  ),
                  mainPanel(mainPanel( tabsetPanel(type = "tabs",
                                                   tabPanel("Animal Segmentation", plotOutput("img1",brush="plot_brush"),
                                                            "TABLE",tableOutput("data")),
                                                   tabPanel("Heatmap", plotOutput("img2")))))))


server <- function(input, output){

  observe(
    if (input$box_checked==0){
      output$img1 <-   renderPlot({
        plot(gblur((readImage(Images[input$Select]))[input$x[1]:input$x[2],input$y[1]:input$y[2],],sigma=input$Blur))
      },height=1000,width=1300)}
    else { 
      output$img1<-renderPlot({
        plot(gblur((readImage(Images[input$Select]))[input$x[1]:input$x[2],input$y[1]:input$y[2],],sigma=input$Blur)<input$Threshold)
      },height=1000,width=1300)})

  
  observeEvent(input$Track,{
    # Cat<-cat("input$image_brush:\n")
    # Str<-str(input$image_click)
    
    GroupNames <<- as.character(unlist(strsplit(input$GroupNames,",")))
    Group1<<-as.numeric(unlist(strsplit(input$Group1,",")))
    Group2<<-as.numeric(unlist(strsplit(input$Group2,",")))
    SessionNames<<-as.character(unlist(strsplit(input$SessionNames,",")))
    Output<-    brushedPoints(mtcars, input$plot_brush)
    Blurring <<- input$Blur
    CroppingX <<- input$x
    CroppingY <<- input$y
    Threshold <<- input$Threshold
    unlink(paste0(getwd(),"/*.jpg"))
    Videos<-Sys.glob("*.avi")
    Videos2<-rep(paste0(1:length(Videos),".avi"),each=1)
    NumberOfAnimals<-(length(Group1)+length(Group2))
    AnimalNumbers<-(c(1:NumberOfAnimals))
    Group <- rep(GroupNames[1], max(AnimalNumbers))
    Group[Group2] <- c(GroupNames[2])
    Session<-rep(SessionNames,each=max(AnimalNumbers))
    VideoName<-c()
    for (i in 1:max(NumberOfAnimals)){
      VideoName[i]<-paste0("Animal_",AnimalNumbers[i],"_",Group[i],"_",Session[i])}
    for (i in 1:length(Videos)){
      file.rename(Videos2[i], paste0(VideoName[i],".avi"))}
    Video<-Sys.glob("*.avi")
    X_Coordinates<-data.frame(matrix(ncol=4,nrow=50000))
    Y_Coordinates<-data.frame(matrix(ncol=4,nrow=50000))
    colnames(X_Coordinates)<-VideoName
    colnames(Y_Coordinates)<-VideoName
    for (i in 1:length(Video)){
      unlink(paste0(getwd(),"/*.jpg"))
      Image<-av_video_images(Video[i], 
                             destdir = VideosWD,
                             format = "jpg", fps = input$SamplingRate)
      Images<-Sys.glob("*.jpg")
      for (j in 1:length(Images)){
        Image1<-readImage(Images[j])
        Image2<- bwlabel((gblur(channel(Image1,"gray"),sigma = Blurring))<Threshold)
        Features<-as.data.frame(computeFeatures.moment(Image2))
        Area1<-c(which((Features[,c("m.cx")]<CroppingX[1]|
                          Features[,c("m.cx")]>CroppingX[2]|
                          Features[,c("m.cy")]<CroppingY[1]|
                          Features[,c("m.cy")]>CroppingY[2])==1))
        Image3<-rmObjects(Image2,c(Area1))
        Image4<-colorLabels(watershed(distmap(Image3),1 ))
        
        
        Area2<-c(which.max((as.data.frame(computeFeatures.shape(Image2))$s.area)[Area1]))
        X_Coordinates[j,i] <- computeFeatures.moment(Image2)[Area1[Area2], c("m.cx")]
        Y_Coordinates[j,i] <- computeFeatures.moment(Image2)[Area1[Area2], c("m.cy")]}}
    unlink(paste0(getwd(),"/*.jpg"))
    X_Coordinates<-na.omit(X_Coordinates)
    Y_Coordinates<-na.omit(Y_Coordinates)
    Data <- list()
    for (i in 1:length(Video)) {
      Data[[i]] <- data.frame("X"=subset(X_Coordinates,select=c(VideoName[i])),
                              "Y"=subset(Y_Coordinates,select=c(VideoName[i])))
      colnames(Data[[i]])<-c("X","Y")
      write.csv(Data[[i]],paste0("Coordinates_",VideoName[i],".csv"))}
    
    ExcelFiles<-c(Sys.glob("*.csv"))
    nb.cols <- 15
    mycolors <- c("white",colorRampPalette(brewer.pal(8, "YlOrRd"))(nb.cols))
    
    plot_list = list()
    for (i in 1:length(ExcelFiles)) {
      Data<-read.csv(ExcelFiles[i])
      p = ggplot(data = Data, aes(x = X, y = Y*-1)) +
        geom_density_2d_filled(alpha = 1,bins=nb.cols,h=100)+
        scale_fill_manual(values=mycolors)+
        geom_point(size=1.5,colour="green")+
        ylab("")+
        xlab("")+
        xlim(c(CroppingX[1]-((CroppingX[2]-CroppingX[1])/4),CroppingX[2]+((CroppingX[2]-CroppingX[1])/4)))+
        ylim(c(dim[2]*-1,0))+
        theme_void()+
        theme(legend.position = "none")+
        # geom_rect(xmin=CroppingX[1]-(CroppingX[2]-CroppingX[1]),
        #           xmax=CroppingX[2]+(CroppingX[2]-CroppingX[1]),
        #           ymin=dim[2]*-1,
        #           ymax=CroppingY[1]*-1,
        #           colour="white")+
        geom_segment(aes(x = CroppingX[1], y = CroppingY[1]*-1, xend = CroppingX[2], yend = CroppingY[1]*-1),linewidth=3)+
        geom_segment(aes(x = CroppingX[1], y = CroppingY[2]*-1, xend = CroppingX[2], yend = CroppingY[2]*-1),linewidth=3)+
        geom_segment(aes(x = CroppingX[1], y = CroppingY[1]*-1, xend = CroppingX[1], 
                         yend = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x = CroppingX[2], y = CroppingY[1]*-1, xend = CroppingX[2], 
                         yend = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x = CroppingX[1], y = CroppingY[2]*-1, xend = CroppingX[1], 
                         yend = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x = CroppingX[2], y = CroppingY[2]*-1, xend = CroppingX[2], 
                         yend = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x = CroppingX[1], y = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38)), 
                         xend = CroppingX[1]+((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         yend = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x = CroppingX[2], y = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38)), 
                         xend = CroppingX[2]-((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         yend = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x = CroppingX[1], y = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38)), 
                         xend = CroppingX[1]+((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         yend = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x = CroppingX[2], y = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38)), 
                         xend = CroppingX[2]-((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         yend = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x =CroppingX[1]+((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         y = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38)), 
                         xend = CroppingX[1]+((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         yend = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        geom_segment(aes(x =CroppingX[2]-((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         y = (CroppingY[2]*-1)+(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38)), 
                         xend = CroppingX[2]-((CroppingX[2]-CroppingX[1])*((15-8)/2)/15), 
                         yend = (CroppingY[1]*-1)-(((CroppingY[1]*-1)-(CroppingY[2]*-1))*(15/38))),linewidth=3)+
        ggtitle(VideoName[i])+
        theme(plot.title = element_text(hjust = 0.5,vjust = -15,size=15,face="bold"))
      plot_list[[i]] = p
    }
    
    for (i in 1:length(VideoName)) {
      file_name = paste0(VideoName[i],".tiff")
      tiff(file_name,width = 700,height = 1000,units = "px")
      print(plot_list[[i]])
      dev.off()
    }
    
    output$img2 <-renderPlot({
      
      ggarrange(plotlist=plot_list,ncol=2,nrow=2)
    },height=1000,width=500)
    
  })}

shinyApp(ui = ui, server = server)

