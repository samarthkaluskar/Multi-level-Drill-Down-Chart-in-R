library(rio)
library(dplyr)
library(purrr)
library(highcharter)
library(scales)
library(stringr)
#Source: https://stackoverflow.com/questions/55795935/r-highcharter-multi-level-drilldown-with-multiple-series
#setwd("C:/Users/Samarth.Kaluskar/Documents/DrillDown_Chart")

Test <- data.frame(Group = c("A", "A", "A", "A", "A", "A", "A", "A", 
                             "B", "B", "B", "B", "B", "B", "B", "B"),
                   Group_Two = c("AA", "AAA", "AA", "AAA", "AAA", "AA", 
                                 "AA", "AAA", "BB", "BBB", "BB", "BBB", 
                                 "BB", "BBB", "BB", "BBB"),
                   Group_Three = c("AJX", "ABX", "AJX", "ABX", "APX", "ANX", 
                                   "ANX", "APX", "BJX", "BBX", "BJX", "BBX", 
                                   "BPX", "BNX", "BPX", "BNX"),
                   Group_Four = c("TH", "TH", "SW", "SW", "GC", "PB", "JB", 
                                  "NX", "TH", "TH", "SW", "SW", "GC", "PB", 
                                  "JB", "NX"),
                   Value = c(5293, 78225, 33235, 56022, 13056, 6160, 44067, 75529, 
                             95679, 98172, 27159, 77475, 37838, 25897, 88400, 28484))

#Test <- 

TestSum <- Test %>%
  group_by(Group) %>%
  summarize(Quantity = sum(Value)
  )

TestSum[,"Proportion"] <- round(prop.table(TestSum[,"Quantity"])*100,2)
TestSum$Proportion<-paste(TestSum$Proportion, "%")
TestSum <- arrange(TestSum,desc(Quantity))

#First level drill down
Lvl1dfStatus <- tibble(tier_1 = TestSum$Group, y = TestSum$Quantity, z = TestSum$Proportion, drilldown = tolower(tier_1))

#Second level drill down
Level_2_Drilldowns <- lapply(unique(Test$Group), function(x_level) {
  TestSum2 <- subset(Test, Test$Group %in% x_level)
  #TestSum2 <- Test[Test$Group == x_level,]
  TestSum2 <- TestSum2 %>%
    group_by(Group_Two) %>%
    summarize(Quantity = sum(Value)
    )
  TestSum2 <- arrange(TestSum2,desc(Quantity)) ###CHECK
  TestSum2[,"Proportion"] <- round(prop.table(TestSum2[,"Quantity"])*100,2)
  TestSum2$Proportion<-paste(TestSum2$Proportion, "%")
  Lvl2dfStatus <- tibble(name = TestSum2$Group_Two, y = TestSum2$Quantity, z= TestSum2$Proportion, drilldown = tolower(paste(x_level, name, sep = "_")))
  list(id = tolower(x_level), name = "tier_2", type = "column", data = list_parse(Lvl2dfStatus), dataLabels = list(enabled = TRUE, format='{point.z}'))
})

#Third level drill down
Level_3_Drilldowns <- lapply(unique(Test$Group), function(x_level) {
  TestSum2 <- subset(Test, Test$Group %in% x_level)
  #TestSum2 <- Test[Test$Group == x_level,]
  lapply(unique(TestSum2$Group_Two), function(y_level) {
    TestSum3 <- subset(TestSum2, TestSum2$Group_Two %in% y_level)
    #TestSum3 <- TestSum2[TestSum2$Group_Two == y_level,]
    TestSum3 <- TestSum3 %>%
      group_by(Group_Three) %>%
      summarize(Quantity = sum(Value)
      )
    TestSum3 <- arrange(TestSum3,desc(Quantity))
    TestSum3[,"Proportion"] <- round(prop.table(TestSum3[,"Quantity"])*100,2)
    TestSum3$Proportion<-paste(TestSum3$Proportion, "%")
    Lvl3dfStatus <- tibble(name = TestSum3$Group_Three,y = TestSum3$Quantity, z = TestSum3$Proportion , drilldown = tolower(paste(x_level, y_level, name, sep = "_")))
    list(id = tolower(paste(x_level, y_level, sep = "_")), name = "tier_3", type = "column", data = list_parse(Lvl3dfStatus), dataLabels = list(enabled = TRUE, format='{point.z}'))
  })
})%>% unlist(recursive = FALSE)

#Fourth level drill down
Level_4_Drilldowns <- lapply(unique(Test$Group), function(x_level) {
  TestSum2 <- subset(Test, Test$Group %in% x_level)
  #TestSum2 <- Test[Test$Group == x_level,]
  lapply(unique(TestSum2$Group_Two), function(y_level) {
    TestSum3 <- subset(TestSum2, TestSum2$Group_Two %in% y_level)
    #TestSum3 <- TestSum2[TestSum2$Group_Two == y_level,]
    lapply(unique(TestSum3$Group_Three), function(z_level) {
      TestSum4 <- subset(TestSum3, TestSum3$Group_Three %in% z_level)
      #TestSum4 <- TestSum3[TestSum3$Group_Three == z_level,]
      TestSum4 <- TestSum4 %>%
        group_by(Group_Four) %>%
        summarize(Quantity = sum(Value)
        )
      TestSum4 <- arrange(TestSum4,desc(Quantity))
      TestSum4[,"Proportion"] <- round(prop.table(TestSum4[,"Quantity"])*100,2)
      TestSum4$Proportion<-paste(TestSum4$Proportion, "%")
      Lvl4dfStatus <- tibble(name = TestSum4$Group_Four,y = TestSum4$Quantity, z = TestSum4$Proportion)
      list(id = tolower(paste(x_level, y_level, z_level, sep = "_")), name = "tier_4", type = "column", data = list_parse(Lvl4dfStatus), dataLabels = list(enabled = TRUE, format='{point.z}'))
    })
  })%>% unlist(recursive = FALSE)
}) %>% unlist(recursive = FALSE)

background_color <- JS("function() {
      var chart = this; chart.update({
        chart: {
          backgroundColor: '#FCFFC5'
        }
      }); console.log('Updated chart background color!');
    }")

MyFormat <- JS("function() {
          if(this.value  > 0 ){
                    return '$' + this.value / 1000 + 'K';
                }else{
                    return '($' + this.value / 1000*-1 + 'K)';
                }
        }")

Drilldown <- JS("function(e) {
                    this.xAxis[0].setTitle({ text: e.seriesOptions.name});
               }")

Drillup <- JS("function(e) {
                    this.xAxis[0].setTitle({ text: e.seriesOptions.name});
               }")

mygraph <- hchart(
  Lvl1dfStatus,
  type = "column",
  hcaes(x = tier_1, y = y, drilldown = drilldown),
  name = "tier_1",
  color = "#a666b0",
  colorByPoint = FALSE,
  dataLabels = list(enabled = TRUE, format='{point.z}')
)%>%
  hc_chart(events = list(load = background_color, drillup = Drillup, drilldown = Drilldown))%>%
  hc_yAxis(title = list(text = "Difference vs LY", 
                        style = list(fontSize = "3.5vh", color="black")), 
           opposite = FALSE,
           labels = list(formatter = MyFormat, style = list(color = "black")))%>%
  hc_xAxis(title = list( style = list(fontSize = "3.5vh", color="black")))%>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = c(Level_2_Drilldowns, Level_3_Drilldowns, Level_4_Drilldowns)
  )

mygraph



