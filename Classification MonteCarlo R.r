##
#http://lib.stat.cmu.edu/datasets/colleges/
library(car)
aaup_txt<-"E:/Revo_R_Data/College_Data/aaup.csv"
usnews_txt<-"E:/Revo_R_Data/College_Data/usnews.csv"
aaup <- RxTextData(aaup_txt, delimiter = ",",missingValueString = "*")
usnews<-RxTextData(usnews_txt, delimiter = ",",missingValueString = "*")
aaup_xdf<-rxImport(aaup,outFile = "E:/Revo_R_Data/College_Data/aaup.xdf",overwrite=TRUE)
names(aaup_xdf)<-c("FICE","College Name","State","Type","average_salary_full_professor","average_salary_associate_professor","average_salary_assistant_professor","average_salary_ALL","average_comp_full_professor","average_comp_associate_professor","average_comp_assistant_professor","average_all_ranks","number_of__full_professors","number_of_associate_professors","number_of_assistant_professors","number_of_instructors","number_of_total_faculty")
usnews_xdf<-rxImport(usnews,outFile = "E:/Revo_R_Data/College_Data/usnews.xdf",overwrite=TRUE)
names(usnews_xdf)<-c("FICE","College Name","State","Public_Private_Flag","Average_Math_SAT","Average_Verbal_SAT","Average_Combined_SAT","Average_ACT","Quartile_1_MATH_SAT","Quartile_3_MATH_SAT","Quartile_1_VERBAL_SAT","Quartile_3_VERBAL_SAT","Quartile_1_ACT","Quartile_3_ACT","Number_of_applicants_received","Number_of_applicants_accepted","Number_of_applicants_enrolled","pct_new_students_in_top_10_percent_of_HS","pct_new_students_in_top_25_percent_of_HS","FT_Undergrads","number_of_parttime_undergrads","in_state_tuition","out_of_state_tuition","room_and_board","room_cost","board_costs","additional_fees","estimate_Book_Costs","estimated_personal_spending","phd_percent_of_faculty","percent_of_faculty_w_terminal_degrees","student_faculty_ratio","alumni_donation_percent","instructional_expenses","graduation_rate")
collegeData<-rxMerge(aaup_xdf,usnews_xdf,matchVars = "FICE",type="inner")

#Decision Forest Model
collegeData$Public_Private_Flag<-collegeData$Public_Private_Flag-1
collegeData_DecisionForest <- rxDTree( Public_Private_Flag ~ Average_Verbal_SAT + Average_Math_SAT + Average_ACT + graduation_rate,data = collegeData, maxDepth = 6)	
plot(createTreeView(collegeData_DecisionForest))
DC_pred<-rxPredict(collegeData_DecisionForest,data = collegeData,computeResiduals=TRUE)
#ROC PLOT
actualVpred<-data.frame(DC_pred$Public_Private_Flag_Pred,collegeData$Public_Private_Flag)
names(actualVpred)<-c("predicted","actual")
graduation_roc <- rxRoc(actualVarName = "actual", predVarNames = c("predicted"), 
                     data = actualVpred)
plot(graduation_roc)	

#Simulation
library(mc2d)
library(plyr)
collegeData_actualsandpredicted <- data.frame (collegeData[,1:3],actualVpred)
collegeData_actualsandpredicted$actual <- as.factor(collegeData_actualsandpredicted$actual)
k<-1
i<-100
simulation_storage<-data.frame(0,0,0,0)
names(simulation_storage)<-c('FF'  ,'TF' ,'FT'  ,'TT' )
for (k in 1:100){
simulation<-ifelse(collegeData_actualsandpredicted$predicted > runif(nrow(collegeData_actualsandpredicted),0,1),1,0)
simulation<-as.factor( ifelse(collegeData_actualsandpredicted$actual == 1 , ifelse(    simulation == 1    ,  'TT' , ' TF'), ifelse(    simulation == 1    ,  'FT' , ' FF')))
collegeData_actualsandpredicted <- data.frame(collegeData_actualsandpredicted, simulation)
names(collegeData_actualsandpredicted)[5+k]<-paste("simulation",k,sep="")

#TODO create ordering of Factors so the lineup is always FF TF FT TT 

temp_simulationresults<-count(collegeData_actualsandpredicted,vars= paste("simulation",k,sep="")) 
simulation_storage[k,]<-temp_simulationresults$freq




}

summary(collegeData_actualsandpredicted$simulation4)
