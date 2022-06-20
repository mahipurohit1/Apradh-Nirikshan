#libraries----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(leaflet)


#DATASETS----
data1<-read.csv("C:\\Users\\Sakshi\\Desktop\\Apradh Nirikshan\\CSV_Data\\CRIME AGAINST CHILDREN.csv")
data2<-read.csv("C:\\Users\\Sakshi\\Desktop\\Apradh Nirikshan\\CSV_Data\\CRIME AGAINST WOMEN.csv")
data3<-read.csv("C:\\Users\\Sakshi\\Desktop\\Apradh Nirikshan\\CSV_Data\\CRIME AGAINST SC.csv")
data4<-read.csv("C:\\Users\\Sakshi\\Desktop\\Apradh Nirikshan\\CSV_Data\\CRIME AGAINST SENIOR CITIZENS.csv")
smartcities <- read.csv("C:\\Users\\Sakshi\\Desktop\\Apradh Nirikshan\\CSV_Data\\smart1.csv")
smartcities$State.UT <- as.character(smartcities$State.UT)

#Dataset_Manipulations
data2['Ratio']<-data2['Persons.Arrested...Male']/data2['Persons.Arrested...Female']


#बच्चों के खिलाफ अपराध----
#Plot 1 : Murder Cases of Children

cp1<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Murder..Section.302.IPC....V..Col.4.,data=data1,type="bar") %>%
  layout(title = "बच्चों की हत्या के मामले",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "हत्या के मामलों की संख्या ",
                      zeroline = FALSE))
#Plot 2 : Abetment of Suicide of Children
cp2<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Abetment.of.suicide.of.child..Section.305.IPC....V..Col.7.,data=data1,type="bar") %>%
  layout(title = "बच्चों को आत्महत्या के लिए उकसाना",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "अवप्रेरण की संख्या",
                      zeroline = FALSE))
#Plot 3 : Attempts to commit murder of Children
cp3<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Attempt.to.commit.Murder..Section.307.IPC....V..Col.10.,data=data1,type="bar") %>%
  layout(title = "बच्चों की हत्या का प्रयास",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "प्रयासों की संख्या ",
                      zeroline = FALSE))
#Plot 4: Infanticides
cp4<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Infanticide..Section.315.IPC....V..Col.13.,data=data1,type="bar") %>%
  layout(title = "शिशुनाशक",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "शिशु हत्याओं की संख्या",
                      zeroline = FALSE))
#Plot 5:Foeticides
cp5<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Foeticide..Section.316.IPC....V..Col.16.,data=data1,type="bar") %>%
  layout(title = "भ्रूण हत्याएँ",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "भ्रूण हत्याओं की संख्या ",
                      zeroline = FALSE))
#Plot 6: Kidnapping Abductions
cp6<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Kidnapping...Abduction.of.Children...V..Col.22.,data=data1,type="bar") %>%
  layout(title = "अपहरण",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "अपहरण की संख्या ",
                      zeroline = FALSE))
#Plot 7: Human Trafficking
cp7<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Human.Trafficking..Sections.370...370A.IPC....V..Col.25.,data=data1,type="bar") %>%
  layout(title = "मानव तस्करी",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या ",
                      zeroline = FALSE))
#Plot 8: Selling of Minors for Prostitution
cp8<-plot_ly(x=data1$State.UT..Col.2.,y=data1$Selling.of.minors.for.prostitution..Sec.372.IPC....V..Col.28.,data=data1,type="bar") %>%
  layout(title = "वेश्यावृत्ति के लिए नाबालिगों को बेचना",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = " ",
                      zeroline = FALSE))

#महिलाओं के खिलाफ अपराध----
#Plot 1: Males:Females being arrested
wp1<-plot_ly(x=data2$State.UT,y=data2$Ratio,data=data2,type="bar") %>%
  layout(title = "महिलाओं के खिलाफ अपराधों के लिए गिरफ्तार किए जा रहे पुरुषों और महिलाओं का अनुपात",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "पुरुषों और महिलाओं का अनुपात ",
                      zeroline = FALSE)) 
#Plot 2: Acquitted Males
wp2<-plot_ly(x=data2$State.UT,y=data2$Persons.Acquitted...Male,data=data2,type="bar") %>%
  layout(title = "महिलाओं के खिलाफ अपराधों के लिए पुरुषों को बरी किया गया",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "बरी किए गए पुरुषों की संख्या",
                      zeroline = FALSE))

#Plot 3: Acquitted Females
wp3<-plot_ly(x=data2$State.UT,y=data2$Persons.Acquitted...Female,data=data2,type="bar") %>%
  layout(title = "महिलाओं के खिलाफ अपराधों के लिए बरी महिलाएं",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "बरी की गई महिलाओं की संख्या",
                      zeroline = FALSE))

#Plot 4: Lack of Evidence
wp4<-plot_ly(x=data2$State.UT,y=data2$Final.Report...True.but.Insufficient.Evidence..Col.10.,data=data2,type="bar") %>%
  layout(title = "उन मामलों के लिए साक्ष्य की कमी जो सच हैं",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या",
                      zeroline = FALSE))
#Plot 5: Cases Not Concluded
# wp5<-plot_ly(x=data2$State.UT,y=data2$Cases_Not_Concluded,data=data2,type="bar") %>%
#   layout(title = "मामले समाप्त नहीं हुए",
#          xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
#                       zeroline = FALSE),
#          yaxis = list(title = "मामलों की संख्या",
#                       zeroline = FALSE))
#Plot 6: Mistake of fact( to be done)
wp6<-plot_ly(x=data2$State.UT,y=data2$Final.Report...Mistake.of.Fact..Col.12.,data=data2,type="bar") %>%
  layout(title = "तथ्य की गलती के मामले",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या",
                      zeroline = FALSE))
#Plot 7: Non-Cognizable ( to be done)
wp7<-plot_ly(x=data2$State.UT,y=data2$Final.Report...Non.Cognizable..Col.13.,data=data2,type="bar") %>%
  layout(title = "गैर-संज्ञेय अपराधों के मामले",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "",
                      zeroline = FALSE))
#Plot *: Cases Compounded ( to be done)
wp8<-plot_ly(x=data2$State.UT,y=data2$Cases.Compounded..Col.9.,data=data2,type="bar") %>%
  layout(title = "कंपाउंडिंग अपराध",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "",
                      zeroline = FALSE))
#वरिष्ठ नागरिकों के खिलाफ अपराध----
#Plot 1: Murder of Senior Citizens
ep1<-plot_ly(x=data4$State.UT..Col.2.,y=data4$Murder..Sec.302.IPC...Col.4....V,data=data4,type="bar") %>%
  layout(title ="वरिष्ठ नागरिकों की हत्या",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "",
                      zeroline = FALSE)) 
#Plot 2: Grevious Hurt of Senior Citizens
ep2<-plot_ly(x=data4$State.UT..Col.2.,y=data4$Grievous.Hurt..Sec.325..326..326A...326B.IPC...Col.16....V,data=data4,type="bar") %>%
  layout(title ="वरिष्ठ नागरिकों की गंभीर चोट",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या",
                      zeroline = FALSE)) 
#Plot 3: Rape of Senior Citizens
ep3<-plot_ly(x=data4$State.UT..Col.2.,y=data4$Rape..Sec..376.IPC...Col.19....V,data=data4,type="bar") %>%
  layout(title = "वरिष्ठ नागरिकों के साथ बलात्कार",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "",
                      zeroline = FALSE)) 
#Plot 4: Robbery of Senior Citizens
ep4<-plot_ly(x=data4$State.UT..Col.2.,y=data4$Robbery..Sec.392.to.394.IPC...Col.25....V,data=data4,type="bar") %>%
  layout(title = "वरिष्ठ नागरिकों की डकैती",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या",
                      zeroline = FALSE)) 
#Plot 5: Lack of Evidence
ep5<-plot_ly(x=data4$State.UT..Col.2.,y=data4$Final.Report...True.but.Insufficient.Evidence..Col.9.,data=data4,type="bar") %>%
  layout(title = "उन मामलों के लिए साक्ष्य की कमी जो सच हैं",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "",
                      zeroline = FALSE))
#अनुसूचित जाति के खिलाफ अपराध----
#Plot 1: Murder of SC
sp1<-plot_ly(x=data3$State.UT..Col.2.,y=data3$SC.ST.Prevention.of.Atrocities..Act.r.w.IPC...Murder..Section.302.IPC....V..Col.4.,data=data3,type="bar") %>%
  layout(title = "अनुसूचित जाति की हत्या",
         xaxis = list(title ="राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या ",
                      zeroline = FALSE))
#Plot 2: Sexual Harrasement of SC Women
sp2<-plot_ly(x=data3$State.UT..Col.2.,y=data3$SC.ST.Prevention.of.Atrocities..Act.r.w.IPC...Assault.on.SC.Women.to.Outrage.Her.Modesty...Sexual.Harassment..Section.354A.IPC....V..Col.13B.,data=data3,type="bar") %>%
  layout(title = "अनुसूचित जाति की महिलाओं के यौन उत्पीड़न",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या ",
                      zeroline = FALSE))

#Plot 3: Rape of SC women
sp3<-plot_ly(x=data3$State.UT..Col.2.,y=data3$SC.ST.Prevention.of.Atrocities..Act.r.w.IPC...Rape..Section.376.IPC....V..Col.22.,data=data3,type="bar") %>%
  layout(title ="अनुसूचित जाति की महिलाओं के साथ बलात्कार",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या ",
                      zeroline = FALSE))
#Plot 4: Rioting 
sp4<-plot_ly(x=data3$State.UT..Col.2.,y=data3$
               SC.ST.Prevention.of.Atrocities..Act.r.w.IPC...Rioting..Sections.147.to.151.IPC....V..Col.28.,data=data3,type="bar") %>%
  layout(title = "दंगा",
         xaxis = list(title ="राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या ",
                      zeroline = FALSE))
#plot 5: Robbery
sp5<-plot_ly(x=data3$State.UT..Col.2.,y=data3$SC.ST.Prevention.of.Atrocities..Act.r.w.IPC...Robbery..Section.392.to.394.IPC....V..Col.31.,data=data3,type="bar") %>%
  layout(title ="अनुसूचित जाति की लूट",
         xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                      zeroline = FALSE),
         yaxis = list(title = "मामलों की संख्या ",
                      zeroline = FALSE))

ui<-dashboardPage(
  #tags$img(src="https://cdn.pixabay.com/photo/2021/03/16/20/54/graph-6100978_1280.png"),
  dashboardHeader(title="अपराध निरीक्षण"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("वर्णन",tabName = "home", icon = icon("arrow-circle-right")),
      menuItem("डेटा विश्लेषण",tabName = "data",icon = icon("envelope")),
      menuItem("अपराध दर",tabName = "plot1",icon=icon("bar-chart-o")),
      menuItem("अपराधों के बारे में",tabName="about", icon = icon("folder-open"))
      
    )),
  dashboardBody(
    
    
    
    tabItems(
      tabItem(
        tabName ="home",h1("अपराध डेटा की जांच"),
        tags$img(src="https://exigotech.com.au/wordpress/wp-content/uploads/2020/08/analyticsinr.png"),
        h3("राष्ट्रीय अपराध विश्लेषण क्यों महत्वपूर्ण हैं?"),
        h3("1) प्रेडिक्टिव पोलिसिंग"),
        helpText("  भविष्य कहनेवाला पुलिसिंग डेटा एक विशिष्ट क्षेत्र पर ध्यान केंद्रित करने में मदद कर सकता है"),
        helpText("  और पुलिस संसाधनों को अधिक प्रभावी ढंग से उपयोग करने की अनुमति देता है।"),
        h3("2) सामुदायिक संबंधों में सुधार"),
        helpText("किसी भी अन्य सरकारी एजेंसी की तरह, जनता को इस बारे में सूचित किया जाना चाहिए कि पुलिस समुदाय की कितनी अच्छी तरह"),
        helpText(" रक्षा कर रही है।"),
        helpText("अपराध के आंकड़े जनता के साथ साझा करने से पुलिस पर विश्वास बढ़ता है और अच्छे कामकाजी संबंध बनते हैं।"),
        h3("3) पहल आकलन"),
        helpText("कानून प्रवर्तन पहल अपराध को कम करने के लिए बनाई गई हैं।अपराध के आँकड़े यह निर्धारित करने में महत्वपूर्ण हैं कि क्या ये पहल काम कर रही हैं, या यदि परिवर्तन की आवश्यकता है|"),
        helpText("डेटा दिखा सकता है कि लक्षित क्षेत्रों में अपराध बढ़ रहा है या नीचे।यह आपराधिक न्याय पेशेवरों को यह समझने में मदद कर सकता है कि उनकी पहल सफल है या नहीं।"),
      ),
      tabItem(tabName = "data",h1("अन्वेषक डेटा विश्लेषण: प्लॉट्स"),
              fluidRow(column(9,selectInput("cr","अपराध चुनें:",choices=c("महिलाओं के खिलाफ अपराध","बच्चों के खिलाफ अपराध","वरिष्ठ नागरिकों के खिलाफ अपराध","अनुसूचित जाति के खिलाफ अपराध")))),
              fluidRow(plotlyOutput("Plot1"),br(),br(),br(),plotlyOutput("Plot2"),br(),br(),br(),plotlyOutput("Plot3"))),
      
      #plot
      tabItem(tabName = "plot1",h1("भारत के अपराध दर"),leafletOutput("my_leaf1")),
      tabItem(tabName = "about",h1("हम भारत में कितने सुरक्षित हैं?"),
      tags$img(src="https://images.hindustantimes.com/img/2021/09/15/original/Cyber_crime_1631741983309.png"),
      helpText("भारत अपने पड़ोसियों की तुलना में पाकिस्तान से अधिक सुरक्षित है और इस क्रम में श्रीलंका, बांग्लादेश, म्यांमार, नेपाल,"),
      helpText("भूटान और चीन से कम सुरक्षित है। यदि चोरी को छोड़कर सभी अपराधों को ध्यान में रखा जाता है,"),
      helpText("तो क्लेमेंट्स वर्ल्डवाइड की एक रिपोर्ट में कहा गया है कि भारत शीर्ष पांच देशों में से एक है जो असुरक्षित लगते हैं।"),
      
      
              h2("यहां कुछ लिंक दिए गए हैं जो भारत में हुए प्रमुख अपराधों को दर्शाते हैं।"),
              helpText("https://www.drishtiias.com/daily-updates/daily-news-analysis/crime-in-india-report-2020-ncrb"),
              helpText("https://www.scoopwhoop.com/news/most-horrific-rape-cases-happened-in-2019/"),
              helpText("https://www.rediff.com/news/report/pix-14-sensational-murders-that-shook-india-/20150827.htm"),
              
      )
    ),
    # tags$img(src="https://cdn.pixabay.com/photo/2021/03/16/20/54/graph-6100978_1280.png"),
    tags$head(tags$style(HTML('
   .content-wrapper{
   font-family : consolas;
   background-color:#ffffff
   }
   img {
    vertical-align: middle;
    width: 400px;
    height: 350px;
    top: 80px;
    left: 1150px;
    position: absolute;
   }

   .help-block{
   color: #000000;
    font-size: 17px;
    margin-top:10px;
   }
   h1{
   font-weight:bold;
   }
   .content{
   background-color:#ffffff
   }
    #sidebarCollapsed {
    background-color: #605ca8;
    font-size: 17px;
    color:black;
}
                        
                   '))),
    
  ),
  skin=c("purple"))


#server

server<-function(input,output,session){
  
  
  output$my_leaf1 <- renderLeaflet({
    
    leaflet(data = smartcities) %>% setView(lng = 80, lat = 20, zoom = 4)%>% addTiles() %>% addMarkers(~Longitude, ~Latitude, popup = ~paste(sep = "<br/>", paste(tags$b("State/UT : "), State.UT),paste(tags$b("अपराध दर :"), Crime_rate, (tags$b("%")))))
    
  })
  
  output$Plot1<- renderPlotly({
    
    if(input$cr=="बच्चों के खिलाफ अपराध"){
      subplot(cp4,cp5,cp6,nrows=1,widths=c(1/3,1/3,1/3),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.1 , y = 1.05, text = "बच्चों की हत्या के मामले", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.5 , y = 1.05, text = "बच्चों को आत्महत्या के लिए उकसाना", showarrow = F, xref='paper', yref='paper'),
          list(x = .9 , y = 1.05, text = "बच्चों की हत्या का प्रयास", showarrow = F, xref='paper', yref='paper'))
        )
      
    }
    else if(input$cr=="महिलाओं के खिलाफ अपराध")
    {
      subplot(wp6,wp7,wp8,nrows=1,widths=c(1/3,1/3,1/3),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.1 , y = 1.05, text = "तथ्य की गलती के मामले", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.5 , y = 1.05, text = "गैर-संज्ञेय अपराधों के मामले", showarrow = F, xref='paper', yref='paper'),
          list(x = .9 , y = 1.05, text = "कंपाउंडिंग अपराध", showarrow = F, xref='paper', yref='paper'))
        ) 
    }
    else if(input$cr=="अनुसूचित जाति के खिलाफ अपराध")
    {
      subplot(sp1,sp3,nrows=1,widths=c(1/2,1/2),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.1 , y = 1.05, text = "अनुसूचित जाति की हत्या", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.9 , y = 1.05, text = "अनुसूचित जाति की महिलाओं के साथ बलात्कार", showarrow = F, xref='paper', yref='paper'))
        )
    }
    else if(input$cr=="वरिष्ठ नागरिकों के खिलाफ अपराध")
    {
      subplot(ep2,ep3,nrows=1,widths=c(1/2,1/2),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.2 , y = 1.05, text = "वरिष्ठ नागरिकों की गंभीर चोट", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.8 , y = 1.05, text = "वरिष्ठ नागरिकों के साथ बलात्कार", showarrow = F, xref='paper', yref='paper'))
        ) 
    }
    else{
      return()
    }
  })
  
  output$Plot2<- renderPlotly({
    
    if(input$cr=="बच्चों के खिलाफ अपराध"){
      subplot(cp4,cp5,cp6,nrows=1,widths=c(1/3,1/3,1/3),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.1 , y = 1.05, text = "शिशुनाशक", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.5 , y = 1.05, text = "भ्रूण हत्याएँ", showarrow = F, xref='paper', yref='paper'),
          list(x = .9 , y = 1.05, text = "अपहरण", showarrow = F, xref='paper', yref='paper'))
        )
    }
    else if(input$cr=="महिलाओं के खिलाफ अपराध")
    {
      subplot(wp1,wp4,nrows=1,widths=c(1/2,1/2),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.001 , y = 1.05, text = "महिलाओं के खिलाफ अपराधों के लिए गिरफ्तार किए जा रहे पुरुषों और महिलाओं का अनुपात", showarrow = F, xref='paper', yref='paper'),
          list(x = .9 , y = 1.05, text = "उन मामलों के लिए साक्ष्य की कमी जो सच हैं", showarrow = F, xref='paper', yref='paper'))
          #         list(x = 1 , y = 1.05, text = "मामले समाप्त नहीं हुए", showarrow = F, xref='paper', yref='paper'))
        )
    }
    else if(input$cr=="अनुसूचित जाति के खिलाफ अपराध")
    {
      plot_ly(x=data3$State.UT..Col.2.,y=data3$SC.ST.Prevention.of.Atrocities..Act.r.w.IPC...Assault.on.SC.Women.to.Outrage.Her.Modesty...Sexual.Harassment..Section.354A.IPC....V..Col.13B.,data=data3,type="bar") %>%
        layout(title = "अनुसूचित जाति की महिलाओं के यौन उत्पीड़न",
               xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                            zeroline = FALSE),
               yaxis = list(title = "मामलों की संख्या ",
                            zeroline = FALSE))
      
    }
    
    else if(input$cr=="वरिष्ठ नागरिकों के खिलाफ अपराध")
    {
      subplot(ep4,ep5,nrows=1,widths=c(1/2,1/2),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.2 , y = 1.05, text = "वरिष्ठ नागरिकों की डकैती", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.8 , y = 1.05, text = "उन मामलों के लिए साक्ष्य की कमी जो सच हैं", showarrow = F, xref='paper', yref='paper'))
        ) 
    }
    else{
      return()
    }
  })
  
  output$Plot3<- renderPlotly({
    
    if(input$cr=="बच्चों के खिलाफ अपराध"){
      subplot(cp7,cp8,nrows=1,widths=c(1/2,1/2),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.2 , y = 1.05, text = "मानव तस्करी", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.8 , y = 1.05, text = "वेश्यावृत्ति के लिए नाबालिगों को बेचना", showarrow = F, xref='paper', yref='paper'))
        )
      
      
    }
    else if(input$cr=="महिलाओं के खिलाफ अपराध")
    {
      subplot(wp2,wp3,nrows=1,widths=c(1/2,1/2),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.05 , y = 1.05, text = "महिलाओं के खिलाफ अपराधों के लिए पुरुषों को बरी किया गया", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.8 , y = 1.05, text = "महिलाओं के खिलाफ अपराधों के लिए बरी महिलाएं", showarrow = F, xref='paper', yref='paper'))
        ) 
    }
    else if(input$cr=="अनुसूचित जाति के खिलाफ अपराध")
    {
      subplot(sp4,sp5,nrows=1,widths=c(1/2,1/2),shareX = TRUE,shareY = FALSE,titleY = TRUE)%>% 
        layout(title= "" ,annotations = list(
          list(x = 0.2 , y = 1.05, text = "दंगा", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.8 , y = 1.05, text = "अनुसूचित जाति की लूट", showarrow = F, xref='paper', yref='paper'))
        ) 
    }
    
    else if(input$cr=="वरिष्ठ नागरिकों के खिलाफ अपराध")
    {
      plot_ly(x=data4$State.UT..Col.2.,y=data4$Murder..Sec.302.IPC...Col.4....V,data=data4,type="bar") %>%
        layout(title = "वरिष्ठ नागरिकों की हत्या के मामलों की संख्या",
               xaxis = list(title = "राज्य/केन्द्र-शासित प्रदेश",
                            zeroline = FALSE),
               yaxis = list(title = "मामलों की संख्या",
                            zeroline = FALSE)) 
      
    }
    else{
      return()
    }
  })
  
}
shinyApp(ui,server)