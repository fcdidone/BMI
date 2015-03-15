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

idealWT<-function(BMI,height,units)
{
        if(units =="metric"){
                if(BMI < 18.5){
                        x <- round(((height/100)^2*c(18.5,25)),2)
                        x <- paste("Your weight should be between",as.character(x[1]),"and",as.character(x[2]),"kilograms")
                        x
                } 
                else if(BMI> 25){
                        x <- round(((height/100)^2*c(18.5,25)),2)
                        x <- paste("Your weight should be between",as.character(x[1]),"and",as.character(x[2]),"kilograms")
                        x
                } else "Your weight is fine"
                
        }
        else if (units=="imperial"){
                if(BMI<18.5){
                        x <- round(((height*0.3048)^2*c(18.5,25)*2.20462),2)
                        x <- paste("Your weight should be between",as.character(x[1]),"and",as.character(x[2]),"pounds")
                        x
                } else if(BMI> 25){
                        x <- round(((height*0.3048)^2*c(18,5,25)*2.20462),2)
                        x <- paste("Your weight should be between",as.character(x[1]),"and",as.character(x[2]),"pounds")
                        x
                }  else "Your weight is fine"
        }}

ltext<- function(height,units)
{ 
        if(units =="imperial"){
                x1 <- as.character(round(((height*0.3048)^2*c(18.5,25,30)*2.20462),1))
                paste(x1,"lbs")->x1
                x1
        }
        else if(units=="metric"){
                x1 <- as.character(round(((height/100)^2*c(18.5,25,30)),1))
                paste(x1, "kg") -> x1
                x1
        }}


shinyServer(function(input, output){
        mar <<- reactive({if(input$units=="metric") c("kilograms","centimeters")
                     else if(input$units=="imperial") c("pounds","feets")})
        aa <<- reactive({BMI(input$weight,input$height,input$units)})     
        
        b <<- reactive({
                  if(aa() < 18.5 ) "underweight." 
                  else if( aa() < 25)  "normal."
                  else if(aa() < 30) "overweight."
                  else "obese." })
        cc <<- reactive({ltext(input$height,input$units)})
        
                
        
        
        
                
        output$pl <- renderPlot({
                ggplot(data=gr,aes(x=id,y=BMI,fill=Status,width=0.1))+
                        geom_bar(stat="identity",position="stack",size=1)+coord_flip()+
                        scale_fill_brewer(type="div",palette="OrRd",breaks=c("Underweight","Normal","Overweight","Obese"))+
                        scale_x_discrete(name="")+theme_bw()+theme(legend.position="top")+theme(legend.position=c(0.5,0.2))+
                        guides(fill=guide_legend(title=NULL,nrow=1))+
                        annotate("segment",y= aa(),x=1.2,yend= aa(),xend=1.04,col="orange",size=1.2, arrow  =arrow(length= unit(0.5,"cm")))+
                        annotate("text", label= "Your BMI",x=1.23,y=aa(),col="orange",size=6,fontface="bold")+
                        annotate("point",x=1,y=aa(),size=4,col="orange")+
                        annotate("text",label=cc()[1],x=0.93,y=18.55,size=5,col="orange",fontface="bold")+
                        annotate("text",label=cc()[2],x=0.93,y=25.5,size=5,col="orange",fontface="bold")+
                        annotate("text",label=cc()[3],x=0.93,y=30.5,size=5,col="orange",fontface="bold")})  
        
        output$ht <- renderText({paste("Your height is",input$height,mar()[[2]])})
        output$wt <- renderText({paste("Your weight is",input$weight,mar()[[1]])})
        output$BMI <- renderText({paste("Your BMI is",as.character(aa()),"and you are",b())})
        output$iBMI <- renderText({idealWT(aa(),input$height,input$units)})
        
})
        