library(shiny)
library(shinydashboard)
#install.packages("shinyWidgets")
library(shinyWidgets)

todays_date <- as.Date(Sys.Date())

#read the csv files
hos <- read.csv("Res_CombiOR.hos.csv")
poc<- read.csv ("Res_CombiOR.poc.csv")
HosProb<- read.csv("Res_HosProb.csv")

#split the files into risk factors and results
hos_rows<- hos[c(1,2,3,4,5,6,7,8,9,10)]
hos_results<-hos[c(11,12,13)]

prob_rows<- HosProb[c(1,2,3,4,5,6,7,8,9,10)]
prob_results<-HosProb[c(11,12,13)]

poc_rows<- poc[c(1,2,3)]
poc_results<-poc[c(4,5,6)]

#User interface
ui <- fluidPage(
  tags$head(
    tags$img(src = "NJMU-1.png", height = "40px", width = "140px", style = "position:absolute;top:8px;left:15px;z-index:9999;"),
    tags$style(type="text/css", "label{  display: table-cell; text-align: left; vertical-align: middle;} .form-group { display: table; } #resultsheader{
                                 font-size: 16px;} #preventionheader{font-size: 16px;} #chinese{float:right;}")
  ),
  
  actionButton("chinese","English (英语)", onclick = "window.open('https://uncover-livingreview.shinyapps.io/RSV-risk/')"),
  
  fluidRow(box(width =12, title="呼吸道合胞病毒感染风险预测工具", solidHeader = TRUE, br(),
               tags$p("呼吸道合胞病毒（RSV）是导致5岁以下儿童下呼吸道感染的最常见病原体，约占所有呼吸道病原体的30%，严重危害儿童健康。不同儿童RSV下呼吸道感染住院风险及住院后发生不良结局的风险存在一定差异。本工具由南京医科大学科研团队开发，可用于预测中国5岁以下儿童发生RSV下呼吸道感染的相关风险。",br(),
                      "请在下方输入儿童信息，本工具可以根据这些信息预测: ",br(),"1. 儿童RSV下呼吸道感染住院的风险", br(), "2. 儿童未来一年内RSV下呼吸道感染住院的可能性；  ", br(), "3. 儿童RSV下呼吸道感染住院后，发生不良结局（需要延长住院时间、吸氧、使用呼吸机或转入ICU）的风险。",style = "font-size:16px;",br(),br() ))),
  fluidRow(
    
    box(width =12, tags$b("【儿童基本信息】", style = "font-size:16px;"), br(),br(),
        #This is to make the values in line with the csv file. 1 means it is positive for risk factor, 0 means negative
        prettyRadioButtons("sex", "1. 性别: ", choices= list("  男"=1,"  女"=0), selected = 0, shape = "square", inline=T), 
        dateInput("dob", "2. 出生日期:", width = "320px"), #need to figure out how to make this look better
        prettyRadioButtons("siblings", "3. 兄弟姐妹数量（共同居住，不含儿童本人）:", choices= list("0"= 0, "1"=1, "≥2"=2), selected = 0, shape="square", inline=T),
        numericInput("weight", "4. 出生体重 (公斤): ", value=0, min=0, max=20),
        prettyRadioButtons("prematurity", "5. 是否早产（指胎龄不满37周）:", choices = list("是"=1, "否"=0), selected = 0, shape="square", inline= T),
        prettyRadioButtons("BD", "6.是否患有支气管肺发育不良：", choices = list("是"=1, "否"=0), selected = 0, shape="square", inline= T),
        prettyRadioButtons("down_syndrome", "7. 是否患有唐氏综合症：", choices = list("是"=1, "否"=0),selected = 0, shape="square", inline= T),
        prettyRadioButtons("chd", "8. 是否患有先天性心脏病：", choices = list("是"=1, "否"=0), selected = 0, shape="square", inline= T),
        prettyRadioButtons("hiv", "9. 是否感染艾滋病病毒: ", choices = list("是"=1, "否"=0), selected = 0, shape="square", inline= T),
        prettyRadioButtons("smoking", "10. 母亲孕期是否吸烟：", choices = list("是"=1, "否"=0), selected = 0, shape="square", inline= T)
    )),
  
  fluidRow(box(actionButton("action", "提交"))),
  #textOutput("selected_var"),
  
  fluidRow(box(width =12, br(), tags$p( 
    htmlOutput("resultsheader"),br(),
    textOutput("res_combiOR.hos"), br(),
    textOutput("Res_hosProb"), br(),
    textOutput("Res_CombiOR.poc"), br(),
    htmlOutput("preventionheader"),br(),
    textOutput("prevention"), br(),
    tags$b("【免责声明】", style = "font-size:16px;"),br(),
    "本工具所包含和生成的所有信息仅用于健康教育目的。这些信息不应被用于任何健康问题或疾病的诊断或治疗，也不能以任何方式取代临床判断或指导个体患者的治疗。"
    
  )))
  
  
  
)

#server
server <- function(input, output, session) {
  
  #creates the 2 seperate variables from the siblings radio button
  output$res_combiOR.hos <- renderText({
    #creates the 2 seperate variables from the siblings radio button
    req(input$action)
    siblings2<-0
    siblings1<-0
    if (isolate(input$siblings) == 2){
      siblings2<-1}
    else if (isolate(input$siblings)==1){
      siblings1<-1}
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    masterlist<- isolate(c(siblings2, siblings1, input$BD, weight, input$down_syndrome,input$prematurity, input$chd, input$hiv, input$sex, input$smoking))
    #under here match the masterlist to the row of the csv. Use the code in calculations.R
    row<-which(colSums(t(hos_rows) == masterlist) == ncol(hos_rows))
    hosest<- hos_results[row, "est"]
    isolate(paste(" 1.	该儿童RSV下呼吸道感染住院的风险是一般儿童的", round(hosest, digits=2), "倍"))
    
  })
  
  output$Res_hosProb<- renderText({
    #copy the code from output$res_combiOR.hos
    req(input$action)
    siblings2<-0
    siblings1<-0
    if (isolate(input$siblings) == 2){
      siblings2<-1}
    else if (isolate(input$siblings)==1){
      siblings1<-1}
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    masterlist<- isolate(c(siblings2, siblings1, input$BD, weight, input$down_syndrome,input$prematurity, input$chd, input$hiv, input$sex, input$smoking))
    row<-which(colSums(t(prob_rows) == masterlist) == ncol(prob_rows))
    probest<- prob_results[row, "est"]
    probest<- probest*100
    isolate(paste("2.	该儿童未来一年内发生RSV下呼吸道感染住院的可能性是百分之 ", round(probest, digits = 2)))
    
  })
  
  output$Res_CombiOR.poc<-renderText({
    req(input$action)
    #returns a list of the combination of the child's metrics, in line with the csv file
    calc_dob<-isolate(as.Date(input$dob))
    #calculates the difference in weeks between the child's dob and the current date
    diff_date<-difftime(todays_date, calc_dob, units= "weeks")
    diff_date <- as.numeric(diff_date)
    #6 months is 25 days 
    if (diff_date > 26){
      age<-0
    } else {age<-1}
    masterlist<-isolate(c(input$prematurity, input$chd, age))
    row<-isolate(which(colSums(t(poc_rows) == masterlist) == ncol(poc_rows)))
    pocest<- isolate(poc_results[row, "est"])
    
    isolate(paste("3.	该儿童因RSV下呼吸道感染住院后发生不良结局的风险是一般儿童的   ", round(pocest, digits=2), "倍"))
    
  })
  
  output$resultsheader<-renderText({
    #just to check that the combination list is correct
    #return(selected_var())
    req(input$action)
    isolate(HTML(paste("<b>【RSV风险预测结果】<b>")))
  })
  output$preventionheader<-renderText({
    #just to check that the combination list is correct
    #return(selected_var())
    req(input$action)
    isolate(HTML(paste("<b>【如何预防RSV感染】<b>")))
  })
  
  output$prevention<-renderText({
    req(input$action)
    isolate(paste("目前暂无可用疫苗，国内单抗尚未上市。可通过加强个人防护等措施有效预防感染，包括在RSV好发的冬春季节，尽量避免去人群聚集场所，外出时规范佩戴口罩；注意手卫生，勤洗手，接触公共物品后不用手触碰眼、口、鼻；在儿童医院等医疗机构就诊要做好儿童个人防护，防止院内交叉感染。"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


#notes and thoughts===========================
#  #lists<- renderPrint({
#masterlist<-as.list(selected_var())
#longlist<-sapply(masterlist,head,9)
#return(longlist)
#})

#label {
#/* Other styling... */
#text-align: right;
#clear: both;
#float:left;
#margin-right:15px;
#}
