library(shiny);library(ggplot2);library(grid)

data.frame(Status = c("Underweight","Normal","Overweight","Obese"),
           BMI=c(18.5,6.5,5,10),id=rep("",4) )-> gr



BMI <- function(weight,height,units)  { 
        if(units == "metric") {
                round(weight/(height/100)^2,2)->temp1
                temp1
        }
        else if(units == "imperial"){
                round((weight/(height*12)^2)*703,2)-> temp1
                temp1
        }
}



shinyServer(function(input, output){
        reactive({if(input$units=="metric") c("kilograms","centimeters")
                     else if(input$units=="imperial") c("pounds","feets")})-> mar
        reactive({BMI(input$weight,input$height,input$units)}) -> aa
        reactive({aa()})-> a
        b <- reactive({
                  if(a() < 18.5 ) "underweight." 
                  else if( a() < 25)  "normal."
                  else if(a() < 30) "overweight."
                  else "obese." })
        
        
                
        output$pl <- renderPlot({
                ggplot(data=gr,aes(x=id,y=BMI,fill=Status,width=0.1))+
                        geom_bar(stat="identity",position="stack",size=1)+coord_flip()+
                        scale_fill_brewer(type="div",palette="OrRd",breaks=c("Underweight","Normal","Overweight","Obese"))+
                        scale_x_discrete(name="")+theme_bw()+theme(legend.position="top")+theme(legend.position=c(0.5,0.2))+
                        guides(fill=guide_legend(title=NULL,nrow=1))+
                        annotate("segment",y= a(),x=1.2,yend= a(),xend=1.04,col="orange",size=1.2, arrow  =arrow(length= unit(0.5,"cm")))+
                        annotate("text", label= "Your BMI",x=1.23,y=a(),col="orange",size=6,fontface="bold")+
                        annotate("point",x=1,y=a(),size=4,col="orange")})  
        
        output$ht <- renderText({paste("Your height is",input$height,mar()[[2]])})
        output$wt <- renderText({paste("Your weight is",input$weight,mar()[[1]])})
        output$BMI <- renderText({paste("Your BMI is",as.character(a()),"and you are",b())})
        
})