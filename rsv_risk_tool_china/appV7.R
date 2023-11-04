rm(list=ls())
library(shiny)
library(shinythemes) # 美化主题
# install.packages("shinydashboard")
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


todays_date <- as.Date(Sys.Date())

#read the csv files
# hos <- read.csv("Res_CombiOR.hos.csv") #住院风险
# poc <- read.csv ("Res_CombiOR.poc.csv")#住院后不良结局风险
# HosProb <- read.csv("Res_HosProb.csv")#住院概率（绝对值）

hos <- read.csv("D:/学习相关/RSV risk pred/02. 数据代码/RSV risk/summaryData_hosORs.csv") #住院风险  32768行
poc <- read.csv("D:/学习相关/RSV risk pred/02. 数据代码/RSV risk/summaryData_hosPoorORs.csv")#住院后不良结局风险   8行
HosProb <- read.csv("D:/学习相关/RSV risk pred/02. 数据代码/RSV risk/summaryData_hosProp.csv")#住院概率（绝对值） 32768行
RiskPercent <- read.csv("D:/学习相关/RSV risk pred/02. 数据代码/RSV risk/summaryData_forHospRiskClass.csv") # summaryData_forHospRiskClass.csv

#split the files into risk factors and results
hos_rows<- hos[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
hos_results<-hos[c(17,18,19)]

prob_rows<- HosProb[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
prob_results<-HosProb[c("Hosp_Prob")]
names(prob_results)[1] <- "est"
# names(prob_results)[2] <- "lci"
# names(prob_results)[3] <- "uci"

RiskPercent_rows <- RiskPercent[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
RiskPercent_results<-RiskPercent[c("Percent")]

poc_rows<- poc[c(2,3,4)]
poc_results<-poc[c(5,6,7)]

# UI----
#used navbarPage function instead of 2 different shiny apps for ease in server hosting 
ui<-navbarPage(
  "", 
  tags$img(src = "NJMU-1.png", height = "40px", width = "140px",
           style = "position:absolute;top:8px;left:15px;z-index:9999;"), #add logo and adjust size and position
  
  tags$head(
    tags$style(type="text/css", 
               "label{  display: table-cell; text-align: left; } .form-group { display: table; } 
                 #resultsheader{ font-size: 16px;} #preventionheader{font-size: 16px;} #resultsheader2{font-size: 16px;} #preventionheader2{font-size: 16px;}
                                 .navbar .navbar-nav {float: right}
                                 .navbar .navbar-header {float: right}
                                 .navbar-default { background-color: #FFFFFF}
                            ")
  ),
  
  tabPanel("English (英语)", 
           
           tags$style(HTML(
             '.background-container {
      background-image: url("background.png");
      background-size:  350px;
      background-size:  cover;
      background-repeat: no-repeat;
      background-position: center top;
      /*background-attachment: fixed; */
    }
    
    .page-content {
/*background-color: rgba(255, 255, 255, 0.8);  半透明背景颜色 */
        padding: 20px;
        margin: 20px; /* 用于分隔文字与背景图 */
        border-radius: 10px;
    }

.my-button {
        background-color: #FF0000; /* 设置背景颜色为红色 */
        color: #FFFFFF; /* 设置字体颜色为白色 */
        font-weight: bold; /* 设置字体加粗 */
        font-size: 16px; /* 设置字体大小为16像素 */
        padding: 10px 20px; /* 设置内边距 */
}

 .background-image1 {
      background-image: url("配色1.png");
      background-size: cover;
      background-repeat: no-repeat;
    }
    
     .background-image2 {
      background-image: url("配色2.png");
      background-size: cover;
      background-repeat: no-repeat;
     }
    
         .background-image3 {
      background-image: url("配色3.png");
      background-size: cover;
      background-repeat: no-repeat;
     }
      ')),
           
           tags$body(
             div(class = "background-container",
                 div(class = "page-content",
                     br(), br(),br(),
                     p(strong("RESPIRATORY SYNCYTIAL VIRUS-RELATED HOSPITALISATION RISK TOOL (RSV HeaRT)"),br(), br(),style="text-align:left;color:white;font-size: 35px;", 
                     ),
                 )
             )),
           hr(), # 添加水平线
           
           fluidRow( 
             box(width = 12, 
                 tags$p(br(),
                        tags$b("Instructions for use:"), style="text-align:left;color:#4169E1;font-size: 20px;"),
                 p("Respiratory syncytial virus (RSV) is the most common pathogen identified in children under five years with acute lower respiratory infections (ALRI), accounting for about 30% of all respiratory pathogens and affecting children's health.",br(),
                   "The risk for hospitalisation with RSV-ALRI and poor outcomes after hospitalisation differs by individual.",br(), 
                   "This tool, developed by a research team from Nanjing Medical University, aims to predict the risks associated with RSV-ALRI in children under five years in China.",
                   style="text-align:left;color:blcak;font-size: 16px;",
                   br()
                 ),
                 style = "background-color: light-blue;"
                 # 有效颜色：Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
             )),
           
           
           fluidRow(
             box(width = 12,
                 tags$p(
                   br(),
                   tags$b("Please enter child’s information below:"),  # Based on the provided information, the tool can predict: 
                   style="text-align:left;color:#4169E1;font-size: 20px;"
                 ))),
           # RF----
           fluidRow(
             box(width =12,
                 #This is to make the values in line with the csv file. 1 means it is positive for risk factor, 0 means negative
                 # prettyRadioButtons("sex", "1. Sex: ", choices= list("Male"=1,"  Female"=0), selected = 0, shape = "round", inline=T), # 用于创建单选按钮组
                 dateInput("dob", "1. Date of Birth: ", width = "320px", value = "2022-09-01", startview = "year"), #need to figure out how to make this look better
                 numericInput("weight", "2. Birthweight (kg): ", value=2.5, min=0, max=20,step=0.1),
                 prettyRadioButtons("prematurity", "3. Gestational age: ",# "margin-right: 20px;",  style = "primary",
                                    # "3. Prematurity (i.e., gestational age < 37 weeks):", 
                                    choices = list("<28 weeks"=1, "28 to <32 weeks "=2, "32 to <37 weeks"=3,"≥ 37weeks"=0), selected = 0, shape="round", inline= T),
                 # prettyRadioButtons("BD", "5. Bronchopulmonary dysplasia:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("down_syndrome", "4. Down syndrome: ", choices = list("Yes"=1, "No"=0),selected = 0, shape="round", inline= T),
                 prettyRadioButtons("chd", "5. Congenital heart disease: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Cystic_fibrosis", "6. Cystic fibrosis: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Previous_asthma", "7. Previous asthma: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("hiv", "8. HIV infection:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 
                 prettyRadioButtons("passive_smoking", "9. Passive smoking: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Not_exclusive_breastfeeding", "10. Not exclusive breastfeeding: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Attendance_at_daycare_center", "11. Attendance at daycare center: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("siblings", "12. Number of siblings (defined as those living together with the child): ", 
                                    choices= list("0"= 0, "≥1"=1), selected = 0, shape="round", inline=T),
                 numericInput("pregnancy_age", "13. Maternal childbearing age: ", value=25, min=0, max=60,step=1),
                 prettyRadioButtons("pregnancy_smoking", "14. Maternal smoking during pregnancy: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T)
                 
             )),
           
           fluidRow(box(actionButton("action", "Submit",class = "my-button"))),
           br(),
           
           fluidRow(
             box(width = 12,
                 tags$p(br(),
                        tags$b("Based on the provided information, the tool can predict:"), 
                        style="text-align:left;color:#4169E1;font-size: 20px;",
                        br()))),
           
           fluidRow(
             column(
               br(),
               div(class = "background-image1",
                 strong("Risk of hospitalisation:"),htmlOutput("res_combiOR.hos"),"times that of his/ her peers*;",br(),br(),
             # 
             #     strong("Probability of hospitalisation:"),
             #       # style="text-align:center;color:black;background-color:papayawhip;padding:15px;border-radius:10px;font-size: 16px;",
             #       htmlOutput("Res_hosProb"),"within the next year",br(),br(),
             # 
             #     strong("Children with an RSV hospitalisation risk higher than X%:"), # The proportion of children with higher hospitalisation risks than him/her:
             #       # style="text-align:center;color:black;background-color:rgba(117, 166, 215, 0.73);padding:15px;border-radius:10px;font-size: 16px;",
             #       htmlOutput("Res_hosRiskPercent"),# br(),br(),
             #     style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 16px;"
             #     ),
             #   width=6 #,br()
             #     ),

             # # column(
             #   br(),
             #   div(
                 strong("RSV hospitalisation risk higher than X% in children under 5 years:"), htmlOutput("Res_hosRiskPercent"), # The proportion of children with higher hospitalisation risks than him/her
                   style="text-align:center;color:black;background-color:rgba(117, 166, 215, 0.73);padding:15px;border-radius:10px;font-size: 16px;",
               ),
               width=4,
               br()),
             
             column(
               br(),
               div(class = "background-image2",
                 strong("Probability of hospitalisation:"),
                   style="text-align:center;color:black;background-color:papayawhip;padding:15px;border-radius:10px;font-size: 16px;",
                   htmlOutput("Res_hosProb"),
                   "within the next year"),
               width=4,
               br()),

             column(    
               br(),
               div(class = "background-image3",
                 strong("Risk of poor outcomes† after hospitalisation:"),
                   style="text-align:center;color:black;background-color:#BFF7BB;padding:15px;border-radius:10px;font-size: 16px;",
                   htmlOutput("Res_CombiOR.poc"),
                   "times that of his/ her peers"),
               width=4,
               br())
           ),
           
           br(),
           
           fluidRow( 
             box(width = 12, 
                 tags$p(# br(),
                   "* those children who do not have any risk factors"))),
           
           fluidRow( 
             box(width = 12, 
                 tags$p(# br(),
                   "† need for prolonged hospital stay, oxygen supplementation, mechanical ventilation, or ICU admission"))),
           
           fluidRow(
             box(width =12,
                 div(htmlOutput("preventionheader")))),
           br(),
           fluidRow(
             box(width =12,   
                 div(textOutput("prevention")))),
           
           
           hr(),
           fluidRow(box(width = 12,
                        div(
                          p("Disclaimer: All information contained and generated by this tool is only used for health education. This information should not be used in the diagnosis or treatment for any health issues or diseases, nor should it in any way replace clinical judgment or guidance for the treatment of individual patients."),
                          style = "text-align:left; font-size:12px;"
                        ))),
           
           
           hr(),
           fluidRow(box(width = 12,
                        "© The Infectious Diseases Epidemiology & Modelling group, Nanjing Medical University, Nanjing, China",
                        style = "text-align:center; font-family: times;font-size:10px;"))
  ), 
  
  tabPanel("Chinese（中文）",
           
           tags$style(HTML(
             '.background-container {
      background-image: url("background.png");
      background-size:  350px;
      background-size:  cover;
      background-repeat: no-repeat;
      background-position: center top;
      /*background-attachment: fixed; */
    }
    
    .page-content {
/*background-color: rgba(255, 255, 255, 0.8);  半透明背景颜色 */
        padding: 20px;
        margin: 20px; /* 用于分隔文字与背景图 */
        border-radius: 10px;
    }

.my-button {
        background-color: #FF0000; /* 设置背景颜色为红色 */
        color: #FFFFFF; /* 设置字体颜色为白色 */
        font-weight: bold; /* 设置字体加粗 */
        font-size: 16px; /* 设置字体大小为16像素 */
        padding: 10px 20px; /* 设置内边距 */
      }

      ')),
           
           tags$body(
             div(class = "background-container",
                 div(class = "page-content",
                     br(), br(),br(),
                     # div(class = "text-container",
                     p(strong("呼吸道合胞病毒相关住院风险预测工具 (RSV HeaRT)"),br(), br(),style="text-align:left;color:white;font-size: 35px;", 
                     ),
                 )
             )),
           hr(), # 添加水平线
           
           fluidRow( 
             box(width = 12, 
                 tags$p(br(),
                        tags$b("使用指导："), style="text-align:left;color:#4169E1;font-size: 20px;"),#  br(), # 用于显示加粗的文本
                 p("呼吸道合胞病毒（RSV）是导致5岁以下儿童下呼吸道感染的最常见病原体，约占所有呼吸道病原体的30%，严重危害儿童健康。",br(),
                   "不同儿童RSV下呼吸道感染住院风险及住院后发生不良结局的风险存在一定差异。",br(), 
                   "本工具由南京医科大学科研团队开发，可用于预测中国5岁以下儿童发生RSV下呼吸道感染住院的相关风险。",
                   style="text-align:left;color:blcak;font-size: 16px;",
                   br()  # ,br(),br(),br(),br()),
                 ),
                 style = "background-color: light-blue;"
                 # 有效颜色：Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
             )),
           # fluidRow(box(width =12, title="呼吸道合胞病毒感染风险预测工具", solidHeader = TRUE, br(),
           #              tags$p("呼吸道合胞病毒（RSV）是导致5岁以下儿童下呼吸道感染的最常见病原体，约占所有呼吸道病原体的30%，严重危害儿童健康。不同儿童RSV下呼吸道感染住院风险及住院后发生不良结局的风险存在一定差异。本工具由南京医科大学科研团队开发，可用于预测中国5岁以下儿童发生RSV下呼吸道感染的相关风险。",br(),
           #                     "请在下方输入儿童信息，本工具可以根据这些信息预测: ",br(),"1. 儿童RSV下呼吸道感染住院的风险", br(), "2. 儿童未来一年内RSV下呼吸道感染住院的可能性；  ", br(), "3. 儿童RSV下呼吸道感染住院后，发生不良结局（需要延长住院时间、吸氧、使用呼吸机或转入ICU）的风险。",style = "font-size:16px;",br(),br() ))),
           # 
           fluidRow(
             box(width = 12, 
                 tags$p( 
                   br(),
                   tags$b("请在下方输入儿童信息："),  
                   style="text-align:left;color:#4169E1;font-size: 20px;"
                 ))),
           
           # RF----
           fluidRow(
             box(width =12,
                 # prettyRadioButtons("sex", "1. Sex: ", choices= list("Male"=1,"  Female"=0), selected = 0, shape = "round", inline=T), # 用于创建单选按钮组
                 # prettyRadioButtons("sex2", "1. 性别: ", choices= list("  男"=1,"  女"=0), selected = 0, shape = "round", inline=T), 
                 dateInput("dob2", "1. 出生日期: ", width = "320px", value = "2022-09-01", startview = "year",language = "zh-CN"), #need to figure out how to make this look better
                 numericInput("weight2", "2.出生体重 (公斤): ", value=2.5, min=0, max=20,step=0.1),
                 prettyRadioButtons("prematurity2", "3. 胎龄: ", 
                                    # "3. Prematurity (i.e., gestational age < 37 weeks):", 
                                    choices = list("<28 周"=1, "28 到 <32 周"=2, "32 到 <37 周"=3,"≥ 37 周"=0), selected = 0, shape="round", inline= T),
                 # prettyRadioButtons("BD", "5. Bronchopulmonary dysplasia:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("down_syndrome2", "4. 是否患有唐氏综合征: ", choices = list("Yes"=1, "No"=0),selected = 0, shape="round", inline= T),
                 prettyRadioButtons("chd2", "5. 是否患有先天性心脏病: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Cystic_fibrosis2", "6. 是否患有囊性纤维化: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Previous_asthma2", "7. 既往是否患有哮喘: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("hiv2", "8. 是否感染艾滋病病毒:", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 
                 prettyRadioButtons("passive_smoking2", "9. 被动吸烟（家中是否有家属吸烟）: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Not_exclusive_breastfeeding2", "10. 出生后头六个月为纯母乳喂养: ", choices = list("Yes"=0, "No"=1), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("Attendance_at_daycare_center2", "11. 是否上托幼机构/幼儿园: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T),
                 prettyRadioButtons("siblings2", "12. 兄弟姐妹数量（共同居住，不含儿童本人）: ", 
                                    choices= list("0"= 0, "≥1"=1), selected = 0, shape="round", inline=T),
                 numericInput("pregnancy_age2", "13. 母亲生育时的年龄: ", value=25, min=0, max=60,step=1),
                 prettyRadioButtons("pregnancy_smoking2", "14. 母亲孕期是否吸烟: ", choices = list("Yes"=1, "No"=0), selected = 0, shape="round", inline= T)
                 
                 
                 # dateInput("dob2", "2. 出生日期:"), #need to figure out how to make this look better
                 # numericInput("weight2", "3. 出生体重 (公斤): ", value=0, min=0, max=20),
                 # prettyRadioButtons("prematurity2", "4. 是否早产（指胎龄不满37周）:", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
                 # prettyRadioButtons("BD2", "5. 是否患有支气管肺发育不良：", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
                 # prettyRadioButtons("down_syndrome2", "6. 是否患有唐氏综合征：", choices = list("是"=1, "否"=0),selected = 0, shape="round", inline= T),
                 # prettyRadioButtons("chd2", "7. 是否患有先天性心脏病：", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
                 # prettyRadioButtons("hiv2", "8. 是否感染艾滋病病毒: ", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
                 # prettyRadioButtons("siblings2", "9. 兄弟姐妹数量（共同居住，不含儿童本人）:", choices= list("0"= 0, "1"=1, "≥2"=2), selected = 0, shape="round", inline=T),
                 # 
                 # prettyRadioButtons("smoking2", "10. 母亲孕期是否吸烟：", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T)
                 
             )),
           
           # fluidRow(
           #   box(width =12, tags$b("【儿童基本信息】", style = "font-size:16px;"), br(),br(),
           #       #This is to make the values in line with the csv file. 1 means it is positive for risk factor, 0 means negative
           #       prettyRadioButtons("sex2", "1. 性别: ", choices= list("  男"=1,"  女"=0), selected = 0, shape = "round", inline=T), 
           #       dateInput("dob2", "2. 出生日期:"), #need to figure out how to make this look better
           #       prettyRadioButtons("siblings2", "3. 兄弟姐妹数量（共同居住，不含儿童本人）:", choices= list("0"= 0, "1"=1, "≥2"=2), selected = 0, shape="round", inline=T),
           #       numericInput("weight2", "4. 出生体重 (公斤): ", value=0, min=0, max=20),
           #       prettyRadioButtons("prematurity2", "5. 是否早产（指胎龄不满37周）:", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
           #       prettyRadioButtons("BD2", "6.是否患有支气管肺发育不良：", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
           #       prettyRadioButtons("down_syndrome2", "7. 是否患有唐氏综合症：", choices = list("是"=1, "否"=0),selected = 0, shape="round", inline= T),
           #       prettyRadioButtons("chd2", "8. 是否患有先天性心脏病：", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
           #       prettyRadioButtons("hiv2", "9. 是否感染艾滋病病毒: ", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T),
           #       prettyRadioButtons("smoking2", "10. 母亲孕期是否吸烟：", choices = list("是"=1, "否"=0), selected = 0, shape="round", inline= T)
           #   )),
           
           fluidRow(box(actionButton("action2", "提交",class = "my-button"))),
           br(),
           
           fluidRow(
             box(width = 12,
                 tags$p(br(),
                        tags$b("本工具可以根据这些信息预测:"),  
                        style="text-align:left;color:#4169E1;font-size: 20px;",
                        br()))),
           
           fluidRow(
             column(
               # br(),
               div(
                 class = "background-image1",
               strong("RSV下呼吸道感染住院的风险:"),br(),
               # tags$div(
               #   style = "display: flex;",
               #   "是一般儿童*的",
               #   htmlOutput("res_combiOR.hos2"),
               #   "倍"
               # ),br(), # 避免自动换行
               # "是一般儿童*的",htmlOutput("res_combiOR.hos2"),"倍；",br(), # 会换行
               htmlOutput("res_combiOR.hos2"),"倍相比于一般儿童*；",br(), # 会换行
               "且高于X%的五岁下儿童", htmlOutput("Res_hosRiskPercent2"),# 住院相对风险高于他/她的儿童在五岁下儿童的占比
                   # The proportion of children with higher hospitalisation risks than him/her"
                 style="text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 16px;"),
               width=4,br()),
             
               column(
                 br(),
                 div(class = "background-image2",
                   strong("未来的一年内RSV下呼吸道感染住院的可能性："),htmlOutput("Res_hosProb2"),
                 # The proportion of children with higher hospitalisation risks than him/her"
                 style="text-align:center;color:black;background-color:papayawhip;padding:15px;border-radius:10px;font-size: 16px;"),
                 width=4,br()),
             
              column(    
               br(),
               div(class = "background-image3",
                 strong("RSV下呼吸道感染住院后，发生不良结局†的风险："), htmlOutput("Res_CombiOR.poc2"),"倍相比于一般儿童",
                   style="text-align:center;color:black;background-color:#BFF7BB;padding:15px;border-radius:10px;font-size: 16px;"),
               width=4,br())
           ),
           
           fluidRow( 
             box(width = 12, 
                 tags$p(# br(),
                   "* 指任何危险因素都不存在的那些儿童"))),
           
           fluidRow( 
             box(width = 12, 
                 tags$p(# br(),
                   "† 需要延长住院时间，吸氧，使用呼吸机或转入重症监护室（ICU）"))),
           
           br(),
           fluidRow(
             box(width =12,
                 div(htmlOutput("preventionheader2")))),
           br(),
           fluidRow(
             box(width =12,   
                 div(textOutput("prevention2")))),
           
           # fluidRow(box(width =12, br(), tags$p( 
           #   htmlOutput("resultsheader2"),br(),
           #   textOutput("res_combiOR.hos2"), br(), # textOutput
           #   textOutput("Res_hosProb2"), br(), # textOutput
           #   textOutput("Res_CombiOR.poc2"), br(), # textOutput
           #   htmlOutput("preventionheader2"),br(),
           #   textOutput("prevention2"), br(),
           
           hr(),
           fluidRow(box(width = 12,
                        div(
                          p("免责声明：本工具所包含和生成的所有信息仅用于健康教育目的。这些信息不应被用于任何健康问题或疾病的诊断或治疗，也不能以任何方式取代临床判断或指导个体患者的治疗。"),
                          style = "text-align:left; font-size:12px;" 
                        ))),
           
           hr(),
           fluidRow(box(width = 12,
                        "版权所有：南京医科大学，传染病流行病学与建模团队",
                        style = "text-align:center; font-family: times;font-size:10px;"))
           
  ))

#The server code needs to be copied twice - once for the English version and once for the Chinese version
server <- function(input, output) {
  #English version =============================================================================================
  
  output$res_combiOR.hos <- renderUI({ # renderText
    #creates the 2 seperate variables from the siblings radio button
    req(input$action) # 作用是确保在满足特定条件时才执行后续的代码。
    
    # siblings2<-0
    # siblings1<-0
    # 
    # if (isolate(input$siblings) == 2){
    #   siblings2<-1}
    # else if (isolate(input$siblings)==1){
    #   siblings1<-1}
    
    #calculates risk factor based on birth prematurity      
    prematurity1<-0 # <28w RF12
    prematurity2<-0 # 28-32w RF13
    prematurity3<-0 # 33-36w RF11
    
    if (isolate(input$prematurity) == 1){
      prematurity1<-1}
    else if (isolate(input$prematurity)==2){
      prematurity2<-1} 
    else if (isolate(input$prematurity)==3){
      prematurity3<-1}
    
    #calculates risk factor based on birth weight
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    
    #calculates risk factor based on pregnancy age
    if (isolate(input$pregnancy_age)<25){
      pregnancy_age<-1
    } else {pregnancy_age <-0}  
    
    masterlist<- isolate(c(input$chd, input$Cystic_fibrosis, input$Attendance_at_daycare_center, input$down_syndrome,input$hiv,
                           input$Not_exclusive_breastfeeding, weight, pregnancy_age, input$pregnancy_smoking,
                           input$passive_smoking,prematurity3,prematurity1,prematurity2, input$Previous_asthma,input$siblings)) # 注意顺序一定要与原来的危险因素保持一致
    # 创建一个包含多个变量和输入值的向量，isolate() 函数用于确保这些输入值在特定条件下保持不变，以避免不必要的重新计算。这个向量可以用于后续的分析、计算或其他应用程序逻辑中。
    # 使用`isolate()`函数来确保`masterlist`的值不受其他Shiny应用程序元素的影响。
    # 用于将输入值从Shiny应用程序的响应式上下文中隔离出来。这意味着无论后续发生什么交互事件，`masterlist`的值将保持不变，不会受到Shiny应用程序中其他输入元素的影响。
    #under here match the masterlist to the row of the csv. Use the code in calculations.R
    row<-which(colSums(t(hos_rows) == masterlist) == ncol(hos_rows)) # 挑选出原始危险因素对应列 相应位置与家长选的危险因素完全一样 的行
    hosest<- hos_results[row, "est"] # 再根据行索引这个估计值
    
    # isolate(paste(" 1. The risk of hospitalisation for RSV-ALRI for the child would be ", round(hosest, digits=2), "times that of his/ her peers."))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(format(round(hosest, digits=2),nsmall=2)), "</span></b>"))  
    # 默认字体："Arial", "Helvetica", sans-serif
    # 衬线字体（有尾巴）："Times New Roman", Times, serif
    # 等宽字体（每个字符占据相同的宽度）："Courier New", Courier, monospace
    # 手写风格字体："Comic Sans MS", cursive
    # 装饰性艺术字体："Impact", fantasy
    # HTML("<b style='font-weight: bold; font-style: italic;font-size: 50px;'> paste(round(hosest, digits=2)) </p>")
  })
  
  output$Res_hosProb<- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action)
    # siblings2<-0
    # siblings1<-0
    # if (isolate(input$siblings) == 2){
    #   siblings2<-1}
    # else if (isolate(input$siblings)==1){
    #   siblings1<-1}
    
    #calculates risk factor based on birth prematurity   
    prematurity1<-0 # <28w RF12
    prematurity2<-0 # 28-32w RF13
    prematurity3<-0 # 33-36w RF11
    
    if (isolate(input$prematurity) == 1){
      prematurity1<-1}
    else if (isolate(input$prematurity)==2){
      prematurity2<-1} 
    else if (isolate(input$prematurity)==3){
      prematurity3<-1}
    
    #calculates risk factor based on birth weight
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    
    #calculates risk factor based on pregnancy age
    if (isolate(input$pregnancy_age)<25){
      pregnancy_age<-1
    } else {pregnancy_age <-0}  
    
    # masterlist<- isolate(c(siblings2, siblings1, input$BD, weight, input$down_syndrome,input$prematurity, input$chd, input$hiv, input$sex, input$smoking))
    masterlist<- isolate(c(input$chd, input$Cystic_fibrosis, input$Attendance_at_daycare_center, input$down_syndrome,input$hiv,
                           input$Not_exclusive_breastfeeding, weight, pregnancy_age, input$pregnancy_smoking,
                           input$passive_smoking,prematurity3,prematurity1,prematurity2, input$Previous_asthma,input$siblings)) # 注意顺序一定要与原来的危险因素保持一致
    
    row<-which(colSums(t(prob_rows) == masterlist) == ncol(prob_rows))
    probest<- prob_results[row, "est"]
    # probest<- probest*1000
    # isolate(paste("2. The probability of hospitalisation for RSV-ALRI within the next year for the child would be  ", round(probest, digits = 2), "%"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(format(round(as.numeric(probest),2),nsmall=2),"%",sep="") , "</span></b>"))  # round(probest, digits=2),"‰
    
  })
  # ----
  output$Res_hosRiskPercent <- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action)
    # siblings2<-0
    # siblings1<-0
    # if (isolate(input$siblings) == 2){
    #   siblings2<-1}
    # else if (isolate(input$siblings)==1){
    #   siblings1<-1}
    
    #calculates risk factor based on birth prematurity   
    prematurity1<-0 # <28w RF12
    prematurity2<-0 # 28-32w RF13
    prematurity3<-0 # 33-36w RF11
    
    if (isolate(input$prematurity) == 1){
      prematurity1<-1}
    else if (isolate(input$prematurity)==2){
      prematurity2<-1} 
    else if (isolate(input$prematurity)==3){
      prematurity3<-1}
    
    #calculates risk factor based on birth weight
    if (isolate(input$weight)<2.5){
      weight<-1
    } else {weight <-0}
    
    #calculates risk factor based on pregnancy age
    if (isolate(input$pregnancy_age)<25){
      pregnancy_age<-1
    } else {pregnancy_age <-0}  
    
    # masterlist<- isolate(c(siblings2, siblings1, input$BD, weight, input$down_syndrome,input$prematurity, input$chd, input$hiv, input$sex, input$smoking))
    masterlist<- isolate(c(input$chd, input$Cystic_fibrosis, input$Attendance_at_daycare_center, input$down_syndrome,input$hiv,
                           input$Not_exclusive_breastfeeding, weight, pregnancy_age, input$pregnancy_smoking,
                           input$passive_smoking,prematurity3,prematurity1,prematurity2, input$Previous_asthma,input$siblings)) # 注意顺序一定要与原来的危险因素保持一致
    
    row<-which(colSums(t(RiskPercent_rows) == masterlist) == ncol(RiskPercent_rows))
    RiskPercent<- RiskPercent_results[row, "Percent"]
    # RiskPercent<- RiskPercent*10000
    # isolate(paste("2. The probability of hospitalisation for RSV-ALRI within the next year for the child would be  ", round(probest, digits = 2), "%"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(RiskPercent,"%",sep="") , "</span></b>"))   #％        ‱    ‰ round(RiskPercent, digits=2),"‱",s
    
  })
  
  output$Res_CombiOR.poc<-renderUI({
    req(input$action)
    #returns a list of the combination of the child's metrics, in line with the csv file
    calc_dob<-isolate(as.Date(input$dob))
    #calculates the difference in weeks between the child's dob and the current date
    diff_date<-difftime(todays_date, calc_dob, units = "weeks")
    diff_date <- as.numeric(diff_date)
    # 6 months is 25 weeks 
    if (diff_date > 26){
      age<-0
    } else {age<-1}
    
    
    #calculates risk factor based on birth prematurity   
    # prematurity1<-0 # <28w RF12
    # prematurity2<-0 # 28-32w RF13
    # prematurity3<-0 # 33-36w RF11
    
    if (isolate(input$prematurity) %in% c(1,2,3)){
      prematurity1<-1}
    else {
      prematurity1<-0} 
    # else if (isolate(input$prematurity)==3){
    #   prematurity3<-1} 
    
    
    masterlist<-isolate(c(prematurity1, input$chd, age))
    row<-isolate(which(colSums(t(poc_rows) == masterlist) == ncol(poc_rows)))
    pocest<- isolate(poc_results[row, "est"])
    
    # isolate(paste("3. The risk of poor outcomes after being hospitalised for RSV-ALRI would be ", round(pocest, digits=2), "times that of his / her peers."))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(format(round(pocest, digits=2),nsmall=2)), "</span></b>"))  
    
  })
  
  output$resultsheader<-renderText({ # 输出文本
    #just to check that the combination list is correct
    #return(selected_var())
    req(input$action)
    isolate(HTML(paste("<b>[Results]<b>")))
  })
  
  output$preventionheader<-renderText({# 输出文本
    #just to check that the combination list is correct
    #return(selected_var())
    req(input$action)
    isolate(HTML(
      paste0("<b><span style='font-weight: bold;font-size: 20px;font-family:Helvetica;color:#4169E1;'>", 
             "How to prevent RSV infection:", "</span></b>")))
    # isolate(HTML(paste("<b>How to prevent RSV infection<b>")))
  })
  
  
  output$prevention<-renderText({# 输出文本
    req(input$action)
    isolate(paste("To date, there are no vaccines for preventing RSV infection, prophylactic monoclonal antibodies and maternal vaccines for protecting infants have not yet been marketed in China. Nonetheless, RSV infection can be effectively prevented by strengthening personal precautions, including avoiding visiting crowded places in winter and spring when RSV is most active and wearing masks at public venues; paying attention to hand hygiene, including washing hands frequently, and avoiding touching eyes, mouth, and nose with unwashed hands; and using personal protective equipment when attending hospitals to prevent hospital-acquired infections."))
  })
  
  #Chinese version ======================================================================================
  #all variables end with 2
  
  output$res_combiOR.hos2 <- renderUI({
    #creates the 2 seperate variables from the siblings radio button
    req(input$action2)
    # siblings22<-0
    # siblings12<-0
    # if (isolate(input$siblings2) == 2){
    #   siblings22<-1}
    # else if (isolate(input$siblings2)==1){
    #   siblings12<-1}
    # #calculates risk factor based on birth weight
    # if (isolate(input$weight2)<2.5){
    #   weight2<-1
    # } else {weight2 <-0}
    
    #calculates risk factor based on birth prematurity      
    prematurity11<-0 # <28w RF12
    prematurity21<-0 # 28-32w RF13
    prematurity31<-0 # 33-36w RF11
    
    if (isolate(input$prematurity2) == 1){
      prematurity11<-1}
    else if (isolate(input$prematurity2)==2){
      prematurity21<-1} 
    else if (isolate(input$prematurity2)==3){
      prematurity31<-1}
    
    #calculates risk factor based on birth weight
    if (isolate(input$weight2)<2.5){
      weight2<-1
    } else {weight2 <-0}
    
    #calculates risk factor based on pregnancy age
    if (isolate(input$pregnancy_age2)<25){
      pregnancy_age2<-1
    } else {pregnancy_age2 <-0}  
    
    # masterlist<- isolate(c(siblings22, siblings12, input$BD2, weight2, input$down_syndrome2,input$prematurity2, input$chd2, input$hiv2, input$sex2, input$smoking2))
    masterlist<- isolate(c(input$chd2, input$Cystic_fibrosis2, input$Attendance_at_daycare_center2, input$down_syndrome2,input$hiv2,
                           input$Not_exclusive_breastfeeding2, weight2, pregnancy_age2, input$pregnancy_smoking2,
                           input$passive_smoking2,prematurity31,prematurity11,prematurity21, input$Previous_asthma2,input$siblings2))
    #under here match the masterlist to the row of the csv. Use the code in calculations.R
    row<-which(colSums(t(hos_rows) == masterlist) == ncol(hos_rows))
    hosest<- hos_results[row, "est"]
    # isolate(paste(format(round(hosest, digits=2),nsmall=2)))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>",
                paste(format(round(hosest, digits=2),nsmall=2)), "</span></b>"))
    
  })
  
  output$Res_hosProb2<- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action2)
    # siblings22<-0
    # siblings12<-0
    # if (isolate(input$siblings2) == 2){
    #   siblings22<-1}
    # else if (isolate(input$siblings2)==1){
    #   siblings12<-1}
    # if (isolate(input$weight2)<2.5){
    #   weight2<-1
    # } else {weight2 <-0}
    
    #calculates risk factor based on birth prematurity      
    prematurity11<-0 # <28w RF12
    prematurity21<-0 # 28-32w RF13
    prematurity31<-0 # 33-36w RF11
    
    if (isolate(input$prematurity2) == 1){
      prematurity11<-1}
    else if (isolate(input$prematurity2)==2){
      prematurity21<-1} 
    else if (isolate(input$prematurity2)==3){
      prematurity31<-1}
    
    #calculates risk factor based on birth weight
    if (isolate(input$weight2)<2.5){
      weight2<-1
    } else {weight2 <-0}
    
    #calculates risk factor based on pregnancy age
    if (isolate(input$pregnancy_age2)<25){
      pregnancy_age2<-1
    } else {pregnancy_age2 <-0}  
    
    # masterlist<- isolate(c(siblings22, siblings12, input$BD2, weight2, input$down_syndrome2,input$prematurity2, input$chd2, input$hiv2, input$sex2, input$smoking2))
    masterlist<- isolate(c(input$chd2, input$Cystic_fibrosis2, input$Attendance_at_daycare_center2, input$down_syndrome2,input$hiv2,
                           input$Not_exclusive_breastfeeding2, weight2, pregnancy_age2, input$pregnancy_smoking2,
                           input$passive_smoking2,prematurity31,prematurity11,prematurity21, input$Previous_asthma2,input$siblings2))
    row<-which(colSums(t(prob_rows) == masterlist) == ncol(prob_rows))
    probest2<- prob_results[row, "est"]
    # probest2<- probest2*1000
    
    # isolate(paste(format(round(as.numeric(probest2),2),nsmall=2),"%",sep=""))
    
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>",
                paste(format(round(as.numeric(probest2),2),nsmall=2),"%",sep="") , "</span></b>"))    # round(probest2 digits=2),"‰",
    
  })
  # ----
  output$Res_hosRiskPercent2 <- renderUI({
    #copy the code from output$res_combiOR.hos
    req(input$action2)
    # siblings22<-0
    # siblings12<-0
    # if (isolate(input$siblings2) == 2){
    #   siblings22<-1}
    # else if (isolate(input$siblings2)==1){
    #   siblings12<-1}
    # if (isolate(input$weight2)<2.5){
    #   weight2<-1
    # } else {weight2 <-0}
    
    #calculates risk factor based on birth prematurity      
    prematurity11<-0 # <28w RF12
    prematurity21<-0 # 28-32w RF13
    prematurity31<-0 # 33-36w RF11
    
    if (isolate(input$prematurity2) == 1){
      prematurity11<-1}
    else if (isolate(input$prematurity2)==2){
      prematurity21<-1} 
    else if (isolate(input$prematurity2)==3){
      prematurity31<-1}
    
    #calculates risk factor based on birth weight
    if (isolate(input$weight2)<2.5){
      weight2<-1
    } else {weight2 <-0}
    
    #calculates risk factor based on pregnancy age
    if (isolate(input$pregnancy_age2)<25){
      pregnancy_age2<-1
    } else {pregnancy_age2 <-0}  
    
    # masterlist<- isolate(c(siblings22, siblings12, input$BD2, weight2, input$down_syndrome2,input$prematurity2, input$chd2, input$hiv2, input$sex2, input$smoking2))
    masterlist<- isolate(c(input$chd2, input$Cystic_fibrosis2, input$Attendance_at_daycare_center2, input$down_syndrome2,input$hiv2,
                           input$Not_exclusive_breastfeeding2, weight2, pregnancy_age2, input$pregnancy_smoking2,
                           input$passive_smoking2,prematurity31,prematurity11,prematurity21, input$Previous_asthma2,input$siblings2))
    row<-which(colSums(t(RiskPercent_rows) == masterlist) == ncol(RiskPercent_rows))
    RiskPercent<- RiskPercent_results[row, "Percent"]
    # RiskPercent<- RiskPercent*10000
    
    # isolate(paste("2.	该儿童未来一年内发生RSV下呼吸道感染住院的可能性是百分之 ", round(probest2, digits = 2), "%"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(RiskPercent,"%",sep="") , "</span></b>"))   #  round(RiskPercent, digits=2),"‱",
    
  })
  
  output$Res_CombiOR.poc2<-renderUI({
    req(input$action2)
    #returns a list of the combination of the child's metrics, in line with the csv file
    calc_dob<-isolate(as.Date(input$dob2))
    #calculates the difference in weeks between the child's dob and the current date
    diff_date<-difftime(todays_date, calc_dob, units= "weeks")
    diff_date <- as.numeric(diff_date)
    # 6 months is 25 days 
    if (diff_date > 26){
      age2<-0
    } else {age2<-1}
    
    #calculates risk factor based on birth prematurity   
    # prematurity1<-0 # <28w RF12
    # prematurity2<-0 # 28-32w RF13
    # prematurity3<-0 # 33-36w RF11
    
    if (isolate(input$prematurity2) %in% c(1,2,3)){
      prematurity4<-1}
    else {
      prematurity4<-0} 
    
    masterlist<-isolate(c(prematurity4, input$chd2, age2))
    row<-isolate(which(colSums(t(poc_rows) == masterlist) == ncol(poc_rows)))
    pocest<- isolate(poc_results[row, "est"])
    
    # isolate(paste("3.	该儿童因RSV下呼吸道感染住院后发生不良结局的风险是一般儿童的   ", round(pocest, digits=2), "倍"))
    HTML(paste0("<b><span style='font-weight: bold;font-size: 28px;font-family:Helvetica;color:#EE4000;'>", 
                paste(format(round(pocest, digits=2),nsmall=2)), "</span></b>"))  
    
  })
  
  output$resultsheader2<-renderText({
    #just to check that the combination list is correct
    #return(selected_var())
    req(input$action2)
    isolate(HTML(paste("<b>【RSV风险预测结果】<b>")))
  })
  
  output$preventionheader2<-renderText({
    #just to check that the combination list is correct
    #return(selected_var())
    req(input$action2)
    # isolate(HTML(paste("<b>【如何预防RSV感染】<b>")))
    
    isolate(HTML(
      paste0("<b><span style='font-weight: bold;font-size: 20px;font-family:Helvetica;color:#4169E1;'>", 
             "如何预防RSV感染：", "</span></b>")))
  })
  
  output$prevention2<-renderText({
    req(input$action2)
    isolate(paste("目前暂无可用疫苗， 国内预防性单克隆抗体和保护婴儿的孕妇疫苗尚未上市。可通过加强个人防护等措施有效预防感染，包括在RSV好发的冬春季节，尽量避免去人群聚集场所，外出时规范佩戴口罩；注意手卫生，勤洗手，接触公共物品后不用手触碰眼、口、鼻；在儿童医院等医疗机构就诊要做好儿童个人防护，防止院内交叉感染。"))
  })
}

shinyApp(ui = ui, server = server)
# getwd()
