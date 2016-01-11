#NWEA

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)
library(RColorBrewer)
library(devtools)
require(devtools)
library(rjson)
library(rCharts)



#Load the CDF files from the CPS website
winter_data<-read_excel("NWEA_Report_Details_CDF_Winter_15_16.xlsx")
fall_data<-read_excel("NWEA_Report_Details_CDF_Fall_15_16.xlsx")
prev_spring_data<-read_excel("NWEA_Spring_14_15.xls")
prev_fall_data<-read_excel("NWEA_Fall_14_15.xls")
prev_winter_data<-read_excel("NWEA_Wint_14_15.xls")
prev_spring_data<-prev_spring_data %>% select(-GradeLevel, -StudentHomeroom, -STUDENT_SPECIAL_ED_INDICATOR, -STUDENT_ELL_INDICATOR )

#merge function
merge.all <- function(by, ...) {
  frames <- list(...)
  return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
}  # end merge.all

#get Grade, HR, Special Ed and ELL Status in a seperate table to that you can join those later with the 2-year long data
#grade, HR, Special Ed and ELL status might change over the years, so we only want to merge the most recent info
current_roster<-winter_data%>%filter(Discipline=="Reading" & GrowthMeasureYN ==TRUE & !grepl('Survey', TestName))%>%select(StudentID,GradeLevel,StudentHomeroom, STUDENT_SPECIAL_ED_INDICATOR, STUDENT_ELL_INDICATOR)


constants<-c("StudentID", "StudentLastName", "StudentFirstName", "Discipline")

#define data tables with just the student id, name, overall RIT score
prev_spring_points<- filter(prev_spring_data, !grepl('Survey', TestName) & Discipline %in% c("Reading", "Mathematics") & GrowthMeasureYN ==TRUE)%>%select(StudentID, StudentLastName, StudentFirstName, Discipline, TestDurationMinutes, TestRITScore, GrowthNeededtoAchieveProjected1)
prev_winter_points<- filter(prev_winter_data, !grepl('Survey', TestName)& Discipline %in% c("Reading", "Mathematics") & GrowthMeasureYN ==TRUE)%>%select(StudentID, StudentLastName, StudentFirstName, Discipline, TestDurationMinutes, TestRITScore, GrowthNeededtoAchieveProjected1)
prev_fall_points<- filter(prev_fall_data, !grepl('Survey', TestName)& Discipline %in% c("Reading", "Mathematics") & GrowthMeasureYN ==TRUE)%>%select(StudentID, StudentLastName, StudentFirstName, Discipline, TestDurationMinutes, TestRITScore, GrowthNeededtoAchieveProjected1)
winter_points<- filter(winter_data, !grepl('Survey', TestName)& Discipline %in% c("Reading", "Mathematics") & GrowthMeasureYN ==TRUE)%>%select(StudentID, StudentLastName, StudentFirstName, Discipline, TestDurationMinutes, TestRITScore, GrowthNeededtoAchieveProjected1)
fall_points<- filter(fall_data, !grepl('Survey', TestName)& Discipline %in% c("Reading", "Mathematics") & GrowthMeasureYN ==TRUE)%>%select(StudentID, StudentLastName, StudentFirstName, Discipline, TestDurationMinutes, TestRITScore, GrowthNeededtoAchieveProjected1)

#function called suffix_col_names
suffix_col_names<-function(your_df, start_col, end_col, your_str, your_sep){
  for (i in start_col:end_col){
    colnames(your_df)[i]<-paste(colnames(your_df)[i], sep=your_sep,your_str)
  }
  return(your_df)
}
#call function to rename columns so that all NWEA data can be merged
prev_spring_points<-suffix_col_names(prev_spring_points,5,7,"SPR1415",".")
prev_winter_points<-suffix_col_names(prev_winter_points,5,7,"WINT1415",".")
prev_fall_points<-suffix_col_names(prev_fall_points,5,7,"FALL1415",".")
fall_points<-suffix_col_names(fall_points,5,7,"FALL1516",".")
winter_points<-suffix_col_names(winter_points,5,7,"WINT1516",".")

scores_merge<-merge.all(by = constants, 
                        filter(winter_points), 
                        filter(fall_points),
                        filter(prev_spring_points),
                        filter(prev_winter_points),
                        filter(prev_fall_points))
#merge with roster data
scores_merge<-merge.all(by = "StudentID",scores_merge,current_roster)
# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  ##get the HR data using Shiny's reactivity
  hrData <- reactive({
    hr_data = select(filter(mutate(scores_merge, Short_name= paste(StudentFirstName, substr(StudentLastName,1,1), sep=" ")), 
                            StudentHomeroom==input$HR & Discipline==input$Subject), 
                     Short_name, TestRITScore.FALL1415, TestRITScore.WINT1415, TestRITScore.SPR1415,
                     TestRITScore.FALL1516, TestRITScore.WINT1516)
    return(hr_data)
    
  })
  
  # Generate an interactive table view of the data using dTable
  output$mytable <- renderChart2({
    dTable(hrData(), sPaginationType = input$pagination)
  })
  
  # Fill in the spot we created for a plot
  output$scorePlot<- renderChart2({
    line_plot_int<-function(){
      shaped_df<-melt(hrData(),
                      id.vars="Short_name", value.name="RITScore", variable.name="Session")
      linePlot <- hPlot(
        RITScore~Session,
        group="Short_name",
        data = shaped_df,  
        type = 'line')
      linePlot$plotOptions(line = list(marker = list(symbol = 'circle')))
      
      return(linePlot)
      
    }
    return(line_plot_int()) 
  })
})