library(shiny)
library(shinydashboard)
library(shinyWidgets)


shinyServer(function(input, output) {
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
)
