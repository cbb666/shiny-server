library(shiny)
library(shinydashboard)
library(shinyWidgets)

todays_date <- as.Date(Sys.Date())

#read the csv files
# hos <- read.csv("Res_CombiOR.hos.csv") #住院风险
# poc <- read.csv ("Res_CombiOR.poc.csv")#住院后不良结局风险
# HosProb <- read.csv("Res_HosProb.csv")#住院概率（绝对值）

hos <- read.csv("summaryData_hosORs.csv") #住院风险  32768行
poc <- read.csv("summaryData_hosPoorORs.csv")#住院后不良结局风险   8行
HosProb <- read.csv("summaryData_hosProp.csv")#住院概率（绝对值） 32768行
RiskPercent <- read.csv("summaryData_forHospRiskClass.csv") # summaryData_forHospRiskClass.csv

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
shinyUI(navbarPage(
  tags$head(
  "", 
  tags$img(src = "NJMU-1.png", height = "40px", width = "140px",
           style = "position:absolute;top:8px;left:15px;z-index:9999;"), #add logo and adjust size and position    
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
 )
