#############################################################################################################################
# Title: Macromorphoscopic Databank Analytical Program
# Purpose: Provide end-user an analytical option for MMS trait data
# Version: 0.2 (beta)
# Author: Joseph T. Hefner, PhD, D-ABFA
# Last updated: 19 Sep 2019 (jth)
# Updates: 
# 1. 11 Oct 2018: jth initiated code
# 2. 12 Oct 2018: identifying major issues...I am an idiot.
# 3. 15 Oct 2018: cleaning code and polishing page design
# 4. 16 Oct 2018: Cleaning code and polishing; changed theme and added in images of the traits
# 5. 17 Oct 2018: Tuned aNN, added code to maintain training sample, added link for book
# 6. 30 Oct 2018: updated buttons and input style
# 7. 06 Nov 2018: updated user inputs
# 8. 07 Nov 2018: changed input buttons for 'groups' and increased size of MMSCS boxes
# 9. 13 Nov 2018: added in additional traits and updated a tuned neural network; updated to v. 0.2
#10. 14 Nov 2018: changed order of variables to alphabetical
#11. 03 Dec 2018: reduced reference data (considerably) to exclude problematic African and Guatamalen samples
#12. 03 Dec 2018: updated aNN model
#13. 19 Sep 2019: removed "reset" button to address crashing issue identified by jjlynch2
#14. 13 Dec 2019: Addressing a number of issues
# Notes:
# 
#
#
#
#############################################################################################################################
# Install libraries and dependencies
#############################################################################################################################
mamd<-read.csv("data/mamd.csv", sep=',', header = T)         
geo.origin<-read.csv("data/Geo.Origin.csv", sep=',', header = T) 
#############################################################################################################################
library(shinythemes)
library(shiny)
library(ModelMetrics)
library(nnet)
library(dplyr)
library(knitr)
library(reshape2)
library(httr)
library(e1071)
library(caret)
library(stats)
library(ggplot2)
library(shinyjs)
library(shinyBS)
library(shinyLP)
library(shinycssloaders)
library(png)
library(shinyWidgets)
set.seed(1234)
digits=4
options(scipen = 999)
#############################################################################################################################
ui <-shinyUI(
  
  
  fluidPage(
    
    
    list(tags$head(HTML('<link rel="icon", href="mamd_logo1.png",
                        type="image/png" />'))),
    div(style="padding: 10px 10px; width: '100%'",
        titlePanel(
          title="", windowTitle="MaMD Analytical 0.2 - Beta"
        )
    ),
    
    navbarPage(title=div(img(src="mamd_logo1.png"), "Macromorphoscopic (MaMD) Analytical 0.2 - Beta"),
               inverse = T, # for diff color view
               theme = shinytheme("superhero"),
               
               tabPanel("The MaMD", icon = icon("home"),
                        
                        jumbotron("Macromorphoscopic Analysis", "MaMD - Analytical Program v. 0.2",
                                  buttonLabel = "Check out the new photographic atlas detailing MMS traits"),
                        fluidRow(
                          column(6, panel_div(class_type = "primary", panel_title = "The MaMD",
                                              content = "MaMDAnalytical uses an artificial neural network (aNN) to classify an unknown cranium into a reference group. The reference data are housed in the Macromorphoscopic Databank, established in 2018 by Hefner (2018). To analyze your case, click on the Analysis tab above.")),
                          column(6, panel_div("success", "Databank Questions?",
                                              HTML("Email: <a href='mailto:hefnerj1@msu.edu?Subject=MaMDAnalytical%20Help' target='_top'>Joseph T. Hefner, PhD, D-ABFA</a>")))
                        ),  # end of fluidRow
                        
                        fluidRow(
                          column(6, panel_div("info", "MaMD Status", "Currently running Beta v.0.2. Please note, this program comes without guarantee or warranty, at the user's own risk. The current Beta version will undergo several drastic changes and should not be used for casework until the full release is made available.")),
                          column(6, panel_div("danger", "Security and License", "© Joseph T. Hefner, 2018")),
                          
                          
                          bsModal("Analysis", "", "tabBut", size = "large",
                                  p(a("Atlas of Human Cranial Macromorphoscopic Traits", href="https://www.elsevier.com/books/atlas-of-human-cranial-macromorphoscopic-traits/hefner/978-0-12-814385-8")), img(src="AHCMT_capture.PNG")
                          )
                          
                        )),
               
               tabPanel("Analysis", icon = icon("object-group"),
                        wells(content = "MaMD Analyical is freely provided. Please note, this program comes without guarantee or warranty, at the user's own risk.",
                              size = "default"),
                        h1("MaMD Analytical 0.2 - Beta", align = "center"),
                        hr(),
                        sidebarPanel(
                          
                        selectizeInput("refsamp", "Select MaMD Reference Sample", choices = c("Macromorphoscopic Databank" = "mamd"), 
                                         selected = "mamd", multiple = TRUE, width = "95%")),
                        
                        prettyCheckboxGroup("Group", "Groups (select all that apply)", choices = levels(geo.origin$Group),
                                            selected = NULL,
                                            shape = "round", status="warning",
                                            outline = TRUE, fill = FALSE, thick = TRUE,
                                            plain = FALSE, bigger = TRUE, inline = TRUE,
                                            choiceNames = NULL, choiceValues = NULL),
                        
                        
                        br(),
                        mainPanel(
                          
                          
                          useShinyjs(),
                          div(
                            id="test",
                              fluidRow(
                                column(12, tableOutput("el_table")),
                                column(12, tableOutput("el_table1"))
                            )),
                          
                          div(
                            id="mainPanel",
                            
                            
                            fluidRow(
                              column(12, textInput("case", "Case #:")),
                              column(12, textInput("tech", "Analyst:"))
                            ),
                            tags$hr(),
                            actionButton("evaluate", "Process", icon = icon("gear")), 
                            column(10, h3("Predicted Ancestry:")), 
                            column(12, h3(verbatimTextOutput("aNN_pred"))), 
                            column(10, h3("Posterior Probabilities:")),
                            column(12, h3(verbatimTextOutput("aNN_prob"))),
                            column(10, h3("Classification Matrix:")),
                            column(12, h3(verbatimTextOutput("aNN_cm")))
                          ) )
                        
                        
                        
                        
               ),
               tabPanel("MMS Traits", icon = icon("user-circle"),
                        
                        jumbotron("Macromorphoscopic Traits", "Trait and character state definitions (Hefner and Linde 2018)",
                                  button = FALSE),
                        hr(),
                        fluidRow(
                          column(6, thumbnail_label(image = 'ANS.bmp', label = 'Aneterior Nasal Spine (ANS)',
                                                    content = 'The anterior nasal spine (ANS) is a uniquely human bony feature located at the inferior border of the nasal aperture, just anterior to the floor of the nasal cavity. Known by a variety of names (e.g., anterior nasal spine, nasal spine, spina nasalis anterior), ANS has both clinical and applied value in anthropology.
                                                    This trait exhibits the following character states: slight, intermediate, and marked, with corresponding scores: 1<2<3. Slight (1) indicates minimal to no projection beyond the inferior nasal aperture. Intermediate (2) indicates a moderate projection of the anterior nasal spine beyond the inferior nasal aperture. Marked (3) indicates a pronounced projection of the spine beyond the inferior nasal aperture.',
                                                    button_link = NULL, button_label = 'ANS')),
                          column(6, thumbnail_label(image = 'INA.bmp', label = 'Inferior Nasal Aperture (INA)',
                                                    content = 'The external, fleshy nose has been the subject of extensive anthropological and clinical study, in part due to the adaptive significance pertaining to that region of the human face, but also because of the numerous pathological conditions that affect this region of the head. As early as 1850, anatomists identified morphological differences in the inferior nasal aperture (INA). Currently, the inferior nasal aperture is an assessment of the shape of the inferior border of the nasal aperture just lateral to the anterior nasal spine, which defines the transition from nasal floor to the vertical portion of the maxillae. Character states for INA include: a marked (1) slope of the nasal floor starting inside the nasal cavity and terminating on the vertical surface of the maxilla; a moderate (2) slope from the nasal floor to the vertical maxilla beginning more anteriorly than INA 1; an abrupt (3) transition from the nasal floor to the vertical maxilla; a weak (4) ridge of bone that crosses the anterior nasal floor perpendicularly, resulting in a partial nasal sill; and, a pronounced (5) ridge, or nasal sill, obstructing the nasal floor-to-maxilla transition.',
                                                    button_link = NULL, button_label = 'INA'))
                        ),
                        
                        fluidRow(
                          column(6, thumbnail_label(image = 'IOB.bmp', label = 'Interorbital Breadth (IOB)',
                                                    content = 'The interorbital breadth (IOB) was historically scored using measurement data (dacryon to dacryon). However, as early as the 1950s, these data were described using mean expressions between groups (e.g., wide, narrow). Today, IOB is considered the distance between the two orbits in the region of dacryon, relative to the overall breadth of the facial skeleton. Interorbital breadth is scored using three character states: narrow, medium, and broad. Narrow (1) indicates an approximately 1/5 ratio of interorbital space to the facial skeleton. Medium (2) indicates an approximately 1/4 ratio of the interorbital space to the facial skeleton. Broad (3) indicates an approximately 1/3 ratio of the interorbital space to the facial skeleton.',
                                                    button_link = NULL, button_label = 'IOB')),
                          column(6, thumbnail_label(image = 'MT.bmp', label = 'Malar Tubercle (MT)',
                                                    content = 'The malar tubercle (MT) has been used for over a century as an indicator of geographic origin. Malar tubercle is known by a number of different names (e.g., marginal process, tuber zygomaticum, tuberculum zygomaxillare, zygomaticomaxillary tubercle, zygomaxillary tubercle), all describing a caudally-protruding tubercle varying in size and position which may occur on the maxillae, the zygomatic bones, or at the intersection between these two bones along the inferior aspect of the zygomaticomaxillary suture. This trait exhibits the following character states: no projection, trace, medium, and pronounced with corresponding scores: 0<1<2<3. No projection (0) indicates no projection or tubercle below the ruler edge. Trace (1) specifies a trace tubercle below the ruler edge, approximately 2 mm or less. Medium (2) shows a protrusion approximately 2 to 4 mm below the ruler edge. Pronounced (3) signifies a pronounced tubercle below the ruler edge, greater than 4 mm. ',
                                                    button_link = NULL, button_label = 'MT'))
                        ),
                        fluidRow(
                          column(6, thumbnail_label(image = 'NAW.bmp', label = 'Nasal Aperture Width (NAW)',
                                                    content = 'Nasal aperture width (NAW) includes the width of the nasal aperture relative to the entire facial skeleton using a ratio of total facial width to nasal opening. The trait is scored viewing the cranium anteriorly, and dividing the midfacial region into fractions (Figure 8.1, Hefner and Linde [2018]). Nasal aperture width comprises three character states scored 1<2< 3. Narrow (1), intermediate (2), and broad (3).',
                                                    button_link = NULL, button_label = 'NAW')),
                          column(6, thumbnail_label(image = 'NBC.bmp', label = 'Nasal Bone Contour (NBC)',
                                                    content = 'Evaluating the nasal bone contour (NBC) is a consideration of the curvature of the midfacial region—specifically the nasal bones and frontal process of both maxillae—approximately one centimeter below the cranial landmark nasion. Nasal bone contour comprises five character states—scored 0<1<2<3<4. A purely visual assessment of NBC is not recommended. This trait should be recorded using a six inch, stainless steel contour gauge. NBC 0 = a score of 0 is assigned to NBC exhibiting a low and rounded contour. NBC 1 = a score of 1 is assigned to NBC exhibiting an oval contour with elongated, high, and rounded lateral walls. NBC 2 = a score of 2 is assigned to NBC exhibiting steep lateral walls and a broad (approximately 7 mm or more), flat plateau. On the contour gauge, the plateau is identified by a flat cluster of four or more needles. NBC 3 = a score of 3 is assigned to NBC exhibiting steep-sided lateral walls and a narrow surface plateau. NBC 4 = a score of is assigned to NBC exhibiting a triangular cross-section and no surface plateau.',
                                                    button_link = NULL, button_label = 'NBC'))
                          
                          
                        ),
                        
                        fluidRow(
                          column(6, thumbnail_label(image = 'NO.bmp', label = 'Nasal Overgrowth (NO)',
                                                    content = 'Nasal overgrowth (NO) is a projection of the lateral border of the nasal bones at their inferior edge beyond the maxilla at the cranial landmark nasale inferious. This trait exhibits two character states based on presence or absence of overgrowth. This trait is visualized by close inspection of the inferior lateral border of the left nasal bone where it meets the maxilla. It may be useful to gently run your finger along the border of the maxilla and nasal bones at nasale inferious to determine whether a projection is present. If the left side is damaged, the right side may be used to assess the trait. If both nasal bones are missing, fractured (ante- or peri-mortem), or damaged, do not score nasal overgrowth.',
                                                    button_link = NULL, button_label = 'NO')),
                          column(6, thumbnail_label(image = 'PBD.bmp', label = 'Postbregmatic Depression (PBD)',
                                                    content = 'Post-bregmatic depression (PBD) is a slight to broad depression along the sagittal suture, posterior to bregma, which is not the result of pathology. Observed in lateral profile, the trait is scored as either absent (no depression) or present, with corresponding scores: 0<1. This depression can range in expression from slight to marked. To score PBD, hold the cranium in a lateral profile view and look for a depression posterior to bregma.',
                                                    button_link = NULL, button_label = 'PBD'))
                        ),
                        
                        fluidRow(
                          column(6, thumbnail_label(image = 'PZT.bmp', label = 'Posterior Zygomatic Tubercle (PZT)',
                                                    content = 'The posterior zygomatic tubercle (PZT), or the marginal process, is a posterior projection of the zygomatic bone at approximately midorbit as viewed in lateral plane. To observe the various degrees of expression, a small, transparent ruler is placed on the frontal process of the zygomatic from the landmarks frontomalare posterale to jugale. The extent of bony protrusion beyond the rulers edge is then assessed as: absent, weak, moderate, or marked, with corresponding scores: 0<1<2<3.',
                                                    button_link = NULL, button_label = 'PZT')),
                          column(6, thumbnail_label(image = 'ZS.bmp', label = 'Zygomaticomaxillary Suture (ZS)',
                                                    content = 'The Zygomaticomaxillary Suture (ZS) describes the course of the suture between the maxilla and the zygomatic. When asymmetry occurs, the side presenting the highest expression is recorded. The infraorbital suture should be ignored when making a determination. Assessment of ZS is based primarily on the number of major angles present and the approximate location of greatest lateral projection of the suture. Sutures having greatest lateral projection at the inferior margin, but a slight angle near the midpoint of the suture should be scored as 0. The course of the suture is best observed in the anterior view. Zygomaticomaxillary suture shape has three character states: no angles, one angle, and two or more angles; scored 0>1>2.',
                                                    button_link = NULL, button_label = 'ZS'))
                        ),
                        observeEvent(input$Create_Desktop_Icon, {
                          if(Sys.info()[['sysname']] == "Windows") {
                            target <- paste('"', file.path(R.home("bin"), "R.exe"), '"', sep="")
                            arguments <- paste('"', "-e ", "library(QA3D);QA3D()", '"', sep="")
                            icon <- paste('"', system.file("vbs/QA3D.ico", package = "QA3D"), '"', sep="")
                            pathname <- paste('"', paste(gsub("/Documents", "", file.path(path.expand("~"), "Desktop") ), "QA3D.lnk", sep = "/"), '"', sep="")
                            vbs <- paste('"', system.file("vbs/createLink.vbs", package = "QA3D"), '"', sep="")
                            system(paste("cscript", vbs, pathname, target, arguments, icon, sep=" "))
                          }
                          if(Sys.info()[['sysname']] == "Linux") {
                            icon_name <- "QA3D.desktop"
                            cat(
                              paste(
                                "[Desktop Entry]\nEncoding=UTF-8\nTerminal=true\nType=Application\nCategories=Application\nName=QA3D\n",
                                "Version=",	packageVersion("QA3D"),"\n",
                                "Icon=",		system.file("QA3D/www/fav.png", package = "QA3D"),"\n",
                                "Exec=",		paste(file.path(R.home("bin"), "R"), "-e", "library(QA3D);QA3D()", sep=" ")
                                ,sep=""),#paste
                              file = paste(file.path(path.expand("~"), "Desktop"), "QA3D.desktop", sep = "/")
                            )#cat
                            Sys.chmod(paste(file.path(path.expand("~"), "Desktop"), "QA3D.desktop", sep="/"), mode = "0777", use_umask = TRUE)
                          }
                          if(Sys.info()[['sysname']] != "Linux" && Sys.info()[['sysname']] != "Windows") { #.command for mac
                            icon_name <- "QA3D.sh"
                            cat(paste(file.path(R.home("bin"), "R"), "-e", "'library(QA3D);QA3D()'", sep=" "), file = paste(file.path(path.expand("~"), "Desktop"), "QA3D.command", sep = "/"))
                            Sys.chmod(paste(file.path(path.expand("~"), "Desktop"), "QA3D.sh", sep="/"), mode = "0777", use_umask = TRUE)
                          }
                        }),
                          )
)
  ) # end of fluid page
    )
#############################################################################################################################
#############################################################################################################################

server<-shinyServer(function(input, output, session) {    
  
    
   refdata <- reactive({  
    input$evaluate  
    isolate({
      if(length(input$refsamp) == 0) return(NULL)
      switch(input$refsamp,
             "mamd" = mamd,  
             NULL)
    })
    
  })
  
  
  getdata<-reactive({  
    input$evaluate
    filtereddata<-refdata()
    filtereddata<- filtereddata %>% filter(Group %in% c(input$Group, input$Group1)) %>% droplevels()
    filtereddata1<-filtereddata[,-1]
    return(filtereddata1)  
    
  })
  
    elements <- reactive({  
    input$evaluate
    isolate({
      elements <- c()
      if(!is.na(input$ANS)) elements <- c(elements, "ANS" = input$ANS)
      if(!is.na(input$INA)) elements <- c(elements, "INA" = input$INA)
      if(!is.na(input$IOB)) elements <- c(elements, "IOB" = input$IOB)
      if(!is.na(input$MT)) elements <- c(elements, "MT" = input$MT)
      if(!is.na(input$NAW)) elements <- c(elements, "NAW" = input$NAW)
      if(!is.na(input$NBC)) elements <- c(elements, "NBC" = input$IOB)
      if(!is.na(input$NO)) elements <- c(elements, "NO" = input$NO)
      if(!is.na(input$PBD)) elements <- c(elements, "PBD" = input$PBD)
      if(!is.na(input$PZT)) elements <- c(elements, "PZT" = input$PZT)
      if(!is.na(input$ZS)) elements <- c(elements, "ZS" = input$ZS)
      if(length(elements) == 0)  return(NULL)
      return(data.frame(as.list(elements)))
      
    })
  })
  
  
    el_names <- c("<h5>MMS Trait</h5>", "<h5>Unknown Scores</h5>")                                       
    
    ANS <- c("ANS",
             "<input id='ANS' class='btn btn-primary btn-sm' type='number' value='NA' min='1' max='3' outline=TRUE width = 150%>"           
    )
    
    INA <- c("INA",
             "<input id='INA' class='btn btn-primary btn-sm' type='number' value='NA' min='1' max='5' outline=TRUE width = 150%>"           
    )
    
    IOB <- c("IOB",
             "<input id='IOB' class='btn btn-primary btn-sm' type='number' value='NA' min='1' max='3' width = 150%>"
             
    )
    
    MT <- c("MT",
            "<input id='MT' class='btn btn-primary btn-sm' type='number' value='NA' min='0' max='3' width = 150%>"
            
    )
    
    NAW <- c("NAW",
             "<input id='NAW' class='btn btn-primary btn-sm' type='number' value='NA' min='1' max='3' width = 150%>"
             
    )
    
    NBC <- c("NBC",
             "<input id='NBC' class='btn btn-primary btn-sm' type='number' value='NA' min='0' max='4' width = 150%>"
             
    )
    
    NO <- c("NO",
            "<input id='NO' class='btn btn-primary btn-sm' type='number' value='NA' min='0' max='1' width = 150%>"
            
    )
    
    PBD <- c("PBD",
             "<input id='PBD' class='btn btn-primary btn-sm' type='number' value='NA' min='0' max='1' width = 150%>"
             
    )
    
    
    PZT <- c("PZT",
             "<input id='PZT' class='btn btn-primary btn-sm' type='number' value='NA' min='0' max='3' width = 150%>"
             
    )
    
    ZS <- c("ZS",
            "<input id='ZS' class='btn btn-primary btn-sm' type='number' value='NA' min='0' max='2' width = 150%>"
            
    )
 
  output$plot1 <- renderImage({
    # A temp file to save the output, deleted after renderImage
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    png(outfile, width=400, height=400)
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "The Macromorphoscopic Databank")
  }, deleteFile = TRUE)
  
  
  
  
  output$el_table <- renderTable({                                                                            
    
    data.frame(el_names, ANS, INA, IOB, MT, NAW)
  }, sanitize.text.function = function(x) x, sanitize.rownames.function = function(x) x, 
  sanitize.colnames.function = function(x) x, include.rownames = FALSE, include.colnames = FALSE, spacing="xs",align = "c")
  
  output$el_table1<-renderTable({                                                                            
    
    data.frame(el_names, NBC, NO, PBD, PZT, ZS)
  }, sanitize.text.function = function(x) x, sanitize.rownames.function = function(x) x, 
  sanitize.colnames.function = function(x) x, include.rownames = FALSE, include.colnames = FALSE, spacing="xs",align = "c")
  
  refsamp <- reactive({
    if (is.null(getdata()) | is.null(elements())) return()
    ref <- dplyr::select_(getdata(), .dots = c("Group.1", names(elements()))) %>% droplevels()
    return(ref)
  })
  
  aNN_mod <- eventReactive(input$evaluate, {                                                                   
    set.seed(1234)
    aNN_data<-na.omit(refsamp()) %>% droplevels()
    aNN_formula<-as.formula(Group.1 ~ .)                                                                      
    fit<-nnet(aNN_formula, data = aNN_data, size = 10, rang = 0.1,
              decay = 5e-4, maxit = 2000, trace=FALSE)                                                                     
    f<-fitted(fit)                                                                                            
    fr<-round(f,digits=3)
    mod <- predict(fit, type="class")
    mod<-as.factor(mod)
    ctab<-confusionMatrix(aNN_data$Group.1, mod)
    pred<-predict(fit, newdata = elements(),type=c("raw"))
    pred.post<-cbind(fit$xlevels,pred) 
    pred.post<-as.data.frame(pred.post,row.names = "Posterior Prob")
    pred.post$V1<-NULL
    pred.post<-format(round(pred, 3), nsmall = 3)
    pred.post
    
    aNNpred<-colnames(pred)[apply(pred,1,which.max)]
    
    return(list(aNN_data, fit, ctab,pred.post,aNNpred)) 
  })
  
output$aNN_prob <- renderPrint({ 
    if(is.null(aNN_mod())) return()
    aNN_mod()[[4]]
    
    })
   
 output$aNN_pred <- renderText({ 
    if(is.null(aNN_mod())) return()
    preds<-(aNN_mod()[[5]])   
    print(preds)
    })
  
  
  output$aNN_cm <- renderPrint({ 
    if(is.null(aNN_mod())) return()
    aNN_mod()[[3]]
  })
  
 
  
  })


#############################################################################################################################
shinyApp(ui = ui, server = server)
