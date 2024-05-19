survey <- read.csv("/Users/kjx/Desktop/survey.csv", header = T)
names(survey) <- c("Jantina", "Bangsa", "PE1", "PE2", "PE3", "PE4", "EE1", "EE2", "EE3", "EE4", "SI1", "SI2", "SI3", "FC1", "FC2", "FC3", "FC4", "HC1", "HC2", "PV1", "PV2", "PV3", "H1", "H2", "H3", "BI1", "BI2", "BI3", "BI4", "BI5", "UB1", "UB2", "UB3")


#seminR

install.packages('seminr')
library(seminr)
??seminr


#measurement model
pls2_cm <- constructs(
  composite('pe', multi_items('PE', 1:4)),
  composite('ee', multi_items('EE', 1:4)),
  composite('si', multi_items('SI', 1:3)),
  composite('fc', multi_items('FC', 1:4)),
  composite('hc', multi_items('HC', 1:2)),
  composite('pv', multi_items('PV', 1:3)),
  composite('h', multi_items('H', 1:3)),
  composite('bi', multi_items('BI', 1:5)),
  composite('ub', multi_items('UB', 1:3))
)


pls2_cm <- constructs(
  composite('pe', multi_items('PE', 1:4)),
  composite('hc', multi_items('HC', 1:2)),
  composite('h', multi_items('H', 1:3)),
  composite('fc', multi_items('FC', 1:4)),
  composite('bi', multi_items('BI', 1:5)),
  composite('ub', multi_items('UB', 1:3))
)


#structural model

pls2_sm <- relationships(
  paths(from = c('pe','ee','si','fc','hc','pv','h'), to = c("bi")),
  paths(from = c('fc', 'bi', 'h'), to = c("ub"))
)

pls2_sm <- relationships(
  paths(from = c('pe','hc','h'), to = c("bi")),
  paths(from = c('fc', 'bi', 'h'), to = c("ub"))
)


pls2_model <- estimate_pls(data = survey,
                           measurement_model = pls2_cm,
                           structural_model = pls2_sm,
                           inner_weights = path_weighting,
                           missing = mean_replacement,
                           missing_value = '-99')



### Ploting SEM
library(ggplot2)

summary(pls2_model)
pls2_model


t1 <- seminr_theme_create(mm.node.label.fontsize = 15, mm.edge.label.fontsize = 15, sm.node.label.fontsize = 15,  sm.edge.label.fontsize = 15, mm.edge.minlen = 2, sm.edge.minlen = 1.5, mm.edge.boot.show_p_value = F, mm.edge.boot.show_p_stars = T)
plot(pls2_model, theme = t1)

plot_scores(pls2_model)
par(cex = 2)



#The analysis

sum_seminr <- summary(pls2_model)
sum_seminr$meta
sum_seminr$iterations
sum_seminr$paths
sum_seminr$total_effects
sum_seminr$total_indirect_effects
sum_seminr$loadings
sum_seminr$weights
sum_seminr$validity
sum_seminr$reliability
sum_seminr$composite_scores
sum_seminr$vif_antecedents
sum_seminr$fSquare
sum_seminr$descriptives
sum_seminr$it_criteria

write.csv(sum_seminr$reliability, file = "/Users/kjx/Library/CloudStorage/OneDrive-UniversitiKebangsaanMalaysia/Y4S1/FYP/Analyse2/reliability.csv")


sum_seminr$validity$htmt

write.csv(sum_seminr$validity$htmt, file = "/Users/kjx/Library/CloudStorage/OneDrive-UniversitiKebangsaanMalaysia/Y4S1/FYP/Analyse2/htmt.csv")

#CA < 0.7 for hc and ub, delete one item at the time to check the changes
#measurement model

survey_2 <- survey
survey_2$UB3 <- NULL

fit_pls()


fit_pls <- function(){
  
  pls2_cm <- constructs(
    composite('pe', multi_items('PE', 1:4)),
    composite('ee', multi_items('EE', 1:4)),
    composite('si', multi_items('SI', 1:3)),
    composite('fc', multi_items('FC', 1:4)),
    composite('hc', multi_items('HC', 2)),
    composite('pv', multi_items('PV', 1:3)),
    composite('h', multi_items('H', 1:3)),
    composite('bi', multi_items('BI', 1:5)),
    composite('ub', multi_items('UB', 1:2))
  )
  
  #structural model
  
  pls2_sm <- relationships(
    paths(from = c('pe','ee','si','fc','hc','pv','h'), to = c("bi")),
    paths(from = c('fc', 'bi', 'h'), to = c("ub"))
  )
  
  
  pls2_model <- estimate_pls(data = survey_2,
                             measurement_model = pls2_cm,
                             structural_model = pls2_sm,
                             inner_weights = path_weighting,
                             missing = mean_replacement,
                             missing_value = '-99')
  summary(pls2_model)
  
}


#prediction
p_pls <- predict_pls(model = pls2_model, technique = predict_DA, noFolds = 10, cores = NULL)

plstest <- p_pls$PLS_out_of_sample_residuals
plstrain <- p_pls$PLS_in_sample_residuals

sqrt(sum(plstrain^2)/217)
sqrt(sum(plstest^2)/217)


#two items so we use spearman cor

hc_data <- data.frame("HC1" = survey$HC1, "HC2" = survey$HC2)
factor(hc_data$HC1, ordered = F) -> hc_data$HC1
factor(hc_data$HC2) -> hc_data$HC2

cor.test(hc_data$HC1, hc_data$HC2, method = c("spearman"))
cor.test(hc_data$HC1, hc_data$HC2, method = c("kendall"))         


ub_data <- data.frame("UB1" = survey$UB1, "UB2" = survey$UB2, "UB3" = survey$UB3)

cor.test(ub_data$UB1, ub_data$UB2, method = c("kendall")) 
cor.test(ub_data$UB2, ub_data$UB3, method = c("kendall")) 
cor.test(ub_data$UB1, ub_data$UB3, method = c("kendall")) 

cor(ub_data$UB1, ub_data$UB2, method = c("kendall")) 
cor(ub_data$UB2, ub_data$UB3, method = c("kendall")) 
cor(ub_data$UB1, ub_data$UB3, method = c("kendall")) 






