library(tidyverse)
library(data.table)
library(plotly)

#################################################
# ANALYSIS                                      #
#################################################


data <- read.csv("Data/data.csv")

native_only <- data %>% 
  filter(AmIAKN == "Yes")

non_native <- data %>% 
  filter(AmIAKN == "No")

total_kids <- nrow(data)
  
native_kids <- nrow(native_only)

non_native_kids <- nrow(non_native)




# native placement settings in current FC setting

native_placement <- native_only %>% 
  select(CurPlSet) %>% 
  group_by(CurPlSet) %>%
  mutate(count = n()) %>% 
  distinct(CurPlSet, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(percent = signif((count * 100) / sum(count), digits = 4))

# overall  placement settings in current FC setting

overall_placement <- data %>% 
  select(CurPlSet) %>% 
  group_by(CurPlSet) %>%
  mutate(count = n()) %>% 
  distinct(CurPlSet, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(percent = signif((count * 100) / sum(count), digits = 4))

# non-native placement settings in current FC setting

non_native_placement <- non_native %>% 
  select(CurPlSet) %>% 
  group_by(CurPlSet) %>%
  mutate(count = n()) %>% 
  distinct(CurPlSet, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(percent = signif((count * 100) / sum(count), digits = 4))

# native average number of removals

native_avg_rems <- native_only %>% 
  select(TotalRem) %>% 
  mutate(avg = signif(sum(TotalRem) / native_kids, digits = 4)) %>% 
  distinct(avg) %>% 
  pull(avg)

# overall average number of removals

overall_avg_rems <- data %>% 
  select(TotalRem) %>% 
  mutate(TotalRem = ifelse(is.na(TotalRem), 0, TotalRem)) %>% 
  mutate(avg = signif(sum(TotalRem) / n(), digits = 4)) %>% 
  distinct(avg) %>% 
  pull(avg)

# non-native average number of removals

non_native_avg_rems <- non_native %>% 
  select(TotalRem) %>% 
  mutate(TotalRem = ifelse(is.na(TotalRem), 0, TotalRem)) %>% 
  mutate(avg = signif(sum(TotalRem) / n(), digits = 4)) %>% 
  distinct(avg) %>% 
  pull(avg)

avg_rems = data.table(
   native = c("Native", "Non-Native", "Overall"),
   num_rems = c(native_avg_rems, 
         non_native_avg_rems,
         overall_avg_rems
         )
 )


# native average number of placements

native_avg_num_plep <- native_only %>% 
  select(NumPlep) %>% 
  mutate(avg = signif(sum(NumPlep) / native_kids, digits = 4)) %>% 
  distinct(avg) %>% 
  pull(avg)

# overall avg number of placements

overall_avg_num_plep <- data %>% 
  select(NumPlep) %>% 
  mutate(avg = signif(sum(NumPlep) / n(), digits = 4)) %>% 
  distinct(avg) %>% 
  pull(avg)

# non-native avg number of placements

non_native_avg_num_plep <- non_native %>% 
  select(NumPlep) %>% 
  mutate(avg = signif(sum(NumPlep) / n(), digits = 4)) %>% 
  distinct(avg) %>% 
  pull(avg)


avg_num_plep = data.table(
  native = c("Native", "Non-Native", "Overall"),
  num_pleps = c(native_avg_num_plep, 
               non_native_avg_num_plep,
               overall_avg_num_plep
  )
)

# native repeaters vs non repeaters percentages

native_rep <- native_only %>% 
  select(TotalRem) %>% 
  mutate(isRep = ifelse(TotalRem > 1, TRUE, FALSE)) %>% 
  group_by(isRep) %>% 
  mutate(count = n()) %>% 
  mutate(percent = signif((count * 100) / native_kids, digits = 4)) %>% 
  distinct(percent)

# overall avg repeater percentages

overall_rep <- data %>% 
  select(TotalRem) %>% 
  mutate(isRep = ifelse(TotalRem > 1, TRUE, FALSE)) %>% 
  group_by(isRep) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  mutate(percent = signif((count * 100) / n(), digits = 4)) %>% 
  distinct(percent, .keep_all = T) %>% 
  select(isRep, percent)

# non-native avg repeater percentages

non_native_rep <- non_native %>% 
  select(TotalRem) %>% 
  mutate(isRep = ifelse(TotalRem > 1, TRUE, FALSE)) %>% 
  group_by(isRep) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  mutate(percent = signif((count * 100) / n(), digits = 4)) %>% 
  distinct(percent, .keep_all = T) %>% 
  select(isRep, percent)

avg_repeater <- data.frame(
  native = c(rep("native" , 2) , rep("non-native" , 2) , rep("overall" , 2)),
  repeater = rep(c(TRUE , FALSE) , 3),
  percent = c(native_rep$percent, non_native_rep$percent , overall_rep$percent)
  )

# native average stay in FC

native_LifeLOS <- native_only %>% 
  select(LifeLOS) %>% 
  mutate(LifeLOS = ifelse(is.na(LifeLOS), 0, LifeLOS)) %>% 
  mutate(LifeLOS = round(sum(LifeLOS) / native_kids)) %>% 
  distinct(LifeLOS) %>% 
  pull(LifeLOS) 

# overall average stay in FC

overall_LifeLOS <- data %>% 
  select(LifeLOS) %>% 
  mutate(LifeLOS = ifelse(is.na(LifeLOS), 0, LifeLOS)) %>% 
  mutate(LifeLOS = round(sum(LifeLOS) / n())) %>% 
  distinct(LifeLOS) %>% 
  pull(LifeLOS) 

# non-native average stay in FC

non_native_LifeLOS <- non_native %>% 
  select(LifeLOS) %>% 
  mutate(LifeLOS = ifelse(is.na(LifeLOS), 0, LifeLOS)) %>% 
  mutate(LifeLOS = round(sum(LifeLOS) / n())) %>% 
  distinct(LifeLOS) %>% 
  pull(LifeLOS) 

avg_LifeLOS <- data.frame(
  native = c("Native", "Non-Native"),
  LifeLOS = c(native_LifeLOS, overall_LifeLOS)
)


#  Do native children placed with native caretakers have less LifeLOS and 
#  numPleps?

by_caretaker <- native_only %>% 
  select(NumPlep, LifeLOS, RF1AMAKN, RF1ASIAN, RF1BLKAA, RF1NHOPI, RF1WHITE, RF1UTOD) %>%
  mutate(caretaker_native = ifelse(RF1AMAKN == "Yes", "Yes", "No")) %>% 
  mutate(caretaker_native = ifelse(is.na(caretaker_native), "No caretaker",
                                   caretaker_native)) %>% 
  group_by(caretaker_native) %>% 
  mutate(Frequency = n(), 
         LifeLOS = sum(LifeLOS) / n(),
         NumPlep = sum(NumPlep) / n()) %>% 
  distinct(caretaker_native, .keep_all = T) %>% 
  select(NumPlep, LifeLOS, caretaker_native, Frequency)

#################################################
# PLOTS                                         #
#################################################


# average number of removals by native status

bubblefont <- list(size = 15,
                  color = '#ffefc1',
                  family = 'alte_haas_groteskregular')
titlefont <- list(family = 'alte_haas_groteskbold',
                  color = '#ff9770')

color_palette <- c('#610453', '#1b3496', '#026E5E', '#390561', '#116DAB')

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

num_rems_viz <- plot_ly(avg_rems,
        x = ~native,
        y = "",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste("Average <br>", 
                              "Number of <br>",
                              "Removals: <br>",
                              avg_rems$num_rems),
        size = ~num_rems,
        color = ~native,
        sizes = c(117, 143),
        marker = list(opacity = 0.55,
                      sizemode = 'radius'
                      ),
        colors = color_palette,
        showlegend = F
) %>% 
  add_text(text = ~native,
           textfont = bubblefont,
           showlegend = F,
           ) %>% 
  layout(title = 'Native American Children Have a Higher Number of Removals
         Than Non-Native Children',
         font= titlefont,
         xaxis = ax,
         yaxis = ax) %>% 
  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)")

num_rems_viz

# average number of placements by native status
num_pleps_viz <- plot_ly(avg_num_plep,
        x = ~native,
        y = "",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste("Average <br>", 
                              "Number of <br>",
                              "Placements: <br>",
                              avg_num_plep$num_pleps),
        size = ~num_pleps,
        color = ~native,
        sizes = c(166, 200),
        marker = list(opacity = 0.55,
                      sizemode = 'radius'
        ),
        colors = color_palette,
        showlegend = F
) %>% 
  add_text(text = ~native,
           textfont = bubblefont,
           showlegend = F,
  ) %>% 
  layout(title = 'Native American Children Have a Higher Number of Placements
         Than Non-Native Children',
         xaxis = ax,
         yaxis = ax,
         font= titlefont) %>% 
  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)")

num_pleps_viz

# percentage of repeaters by native status

repeater_viz <- plot_ly(labels = c("Native", "Non-Native", "Overall"),
                        marker = list(colors = "Set1"))
repeater_viz  <- repeater_viz  %>% add_pie(data = native_rep,
                       labels = ~isRep,
                       values = ~percent,
                       name = "Native",
                       text = c("Repeaters", "Non-Repeaters"),
                       textfont = bubblefont,
                       marker = list(colors = color_palette),
                       title = "Native American",
                       hoverinfo = none,
                       domain = list(x = c(0, 0.4), y = c(0.4, 1)))
repeater_viz  <- repeater_viz %>% add_pie(data = non_native_rep,
                       labels = ~isRep,
                       values = ~percent,
                       text = c("Repeaters", "Non-Repeaters"),
                       textfont = bubblefont,
                       marker = list(colors = color_palette),
                       title = "Non-Native American",
                       hoverinfo = none,
                       colors = color_palette,
                       name = "Non Native",
                       domain = list(x = c(0.6, 1), y = c(0.4, 1)))
repeater_viz  <- repeater_viz %>% add_pie(data = overall_rep,
                       labels = ~isRep,
                       values = ~percent,
                       text = c("Repeaters", "Non-Repeaters"),
                       textfont = bubblefont,
                       marker = list(colors = color_palette),
                       title = "Overall",
                       colors = color_palette,
                       hoverinfo = none,
                       name = "Overall",
                       domain = list(x = c(0.25, 0.75), y = c(0, 0.6)))
repeater_viz <- repeater_viz  %>% 
                layout(
                title = "Native American Children are More Likely To Go Back 
                Into Foster Care",
                font = titlefont,
                showlegend = F,
                grid=list(rows=2, columns=2
                                ),
                xaxis = list(showgrid = FALSE,
                            zeroline = FALSE,
                            showticklabels = FALSE
                            ),
                yaxis = list(showgrid = FALSE,
                            zeroline = FALSE,
                            showticklabels = FALSE
                            )
                ) %>%
                layout(
                  plot_bgcolor  = "rgba(0, 0, 0, 0)",
                  paper_bgcolor = "rgba(0, 0, 0, 0)"
                  )
  

repeater_viz

# Total stay in FC by native status

LifeLOS_viz <- plot_ly(avg_LifeLOS,
        x = "",
        y = "",
        color = ~native,
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste("Average <br>", 
                              "Stay in <br>",
                              "Foster Care: <br>",
                              avg_LifeLOS$LifeLOS,
                              "days"),
        size = ~LifeLOS,
        sizes = c(215, 247),
        marker = list(
                      sizemode = 'radius'
        ),
        colors = color_palette,
        showlegend = T
) %>% 
  layout(title = "Native American Children Spend Longer in Foster Care\n than Non-Native Children",
         xaxis = ax,
         yaxis = ax,
         font = titlefont,
         plot_bgcolor='transparent',
         paper_bgcolor='transparent'
         ) %>% 
  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)")

LifeLOS_viz
