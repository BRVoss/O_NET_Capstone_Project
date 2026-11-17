if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(glmnet)) install.packages("glmnet")
if(!require(GGally)) install.packages("GGally")
if(!require(ggrepel)) install.packages("ggrepel")

library(tidyverse)
library(caret)
library(readxl)
library(dplyr)
library(glmnet)
library(GGally)
library(ggrepel)

###O*NET data files from https://www.onetcenter.org/database.html#tabular

#Reformatting Data 
work_styles<-read_xlsx("Work Styles.xlsx")
work_styles<-work_styles %>% filter(`Scale ID`=="WI")
work_styles<-work_styles %>% select(c(Title, `Element Name`, `Data Value`))
work_styles<-work_styles %>% rename(`Work Style Value`=`Data Value`, `Work Style Element`=`Element Name`)
work_styles<-work_styles %>% pivot_wider(names_from = `Work Style Element`, values_from = `Work Style Value`)

abilities<-read_xlsx("Abilities.xlsx")
abilities<-abilities %>% filter(`Scale ID`=="IM")
abilities<-abilities %>% select(c(Title, `Element Name`, `Data Value`))
abilities<-abilities %>% rename(`Ability Element`=`Element Name`, `Ability Value`=`Data Value` )
abilities<-abilities %>% pivot_wider(names_from = `Ability Element`, values_from = `Ability Value`)

essential_skills<-read_xlsx("Essential Skills.xlsx")
essential_skills<-essential_skills %>% filter(`Scale ID`=="IM")
essential_skills<-essential_skills %>% select(c(Title, `Element Name`, `Data Value`))
essential_skills<-essential_skills %>% rename(`Essential Skills Element`= `Element Name`, `Essential Skills Value`=`Data Value`)
essential_skills<-essential_skills %>% pivot_wider(names_from = `Essential Skills Element`, values_from = `Essential Skills Value`)

knowledge<-read_xlsx("Knowledge.xlsx")
knowledge<-knowledge %>% filter(`Scale ID`=="IM")
knowledge<-knowledge %>%  select(c(Title, `Element Name`, `Data Value`))
knowledge<-knowledge %>% rename(`Knowledge Element`=`Element Name`, `Knowledge Value`=`Data Value`)
knowledge<-knowledge %>% pivot_wider(names_from = `Knowledge Element`, values_from = `Knowledge Value`)

transferable_skills<-read_xlsx("Transferable Skills.xlsx")
transferable_skills<-transferable_skills %>% filter(`Scale ID`=="IM")
transferable_skills<-transferable_skills %>% select(c(Title, `Element Name`, `Data Value`))
transferable_skills<-transferable_skills %>% rename(`Transferable Skills Element`=`Element Name`, `Transferable Value`=`Data Value`)
transferable_skills<-transferable_skills %>% pivot_wider(names_from = `Transferable Skills Element`, values_from = `Transferable Value`)

o_net_data<-work_styles%>%
  inner_join(abilities, by= "Title")%>%
  inner_join(essential_skills, by="Title")%>%
  inner_join(knowledge, by="Title")%>%
  inner_join(transferable_skills, by="Title")

#Create binomial with outcome means
o_net_data<-o_net_data%>%
  mutate(`Innovation Variable`= ifelse(Innovation>=mean(Innovation),1,0),
         `Attention to Detail Variable`= ifelse(`Attention to Detail`>=mean(`Attention to Detail`),1,0),
         `Stress Tolerance Variable`= ifelse(`Stress Tolerance`>=mean(`Stress Tolerance`),1,0))



###INNOVATION MODELS###
#Split Data Into Test and Train Sets
set.seed(1888)

train_index_innovation <- createDataPartition(o_net_data$`Innovation Variable`, p = 0.8, list = FALSE)

train_set_all_innovation<-o_net_data[train_index_innovation,]
test_set_all_innovation<-o_net_data[-train_index_innovation,]

train_set_all_innovation<-train_set_all_innovation%>%
  select(c(-Title,-Innovation, -`Attention to Detail Variable`, -`Stress Tolerance Variable`))
test_set_all_innovation<-test_set_all_innovation%>%
  select(c(-Title, -Innovation, -`Attention to Detail Variable`, -`Stress Tolerance Variable`))

train_set_work_styles_innovation<-train_set_all_innovation%>%
  select(c(`Achievement Orientation`, `Intellectual Curiosity`, `Tolerance for Ambiguity`, Initiative, Adaptability, `Self-Confidence`, Perseverance, `Leadership Orientation`, Humility, Sincerity, Empathy, Cooperation, Optimism, `Social Orientation`, Cautiousness, `Attention to Detail`, Dependability, Integrity, `Stress Tolerance`, `Self-Control`, `Innovation Variable`))
test_set_work_styles_innovation<-test_set_all_innovation%>%
  select(c(`Achievement Orientation`, `Intellectual Curiosity`, `Tolerance for Ambiguity`, Initiative, Adaptability, `Self-Confidence`, Perseverance, `Leadership Orientation`, Humility, Sincerity, Empathy, Cooperation, Optimism, `Social Orientation`, Cautiousness, `Attention to Detail`, Dependability, Integrity, `Stress Tolerance`, `Self-Control`, `Innovation Variable`))

train_set_all_innovation <- train_set_all_innovation %>%
  mutate(`Innovation Variable` = factor(`Innovation Variable`, labels = c("High", "Low")))
test_set_all_innovation <- test_set_all_innovation %>%
  mutate(`Innovation Variable` = factor(`Innovation Variable`, labels = c("High", "Low")))

train_set_work_styles_innovation <- train_set_work_styles_innovation %>%
  mutate(`Innovation Variable` = factor(`Innovation Variable`, labels = c("High", "Low")))
test_set_work_styles_innovation <- test_set_work_styles_innovation %>%
  mutate(`Innovation Variable` = factor(`Innovation Variable`, labels = c("High", "Low")))

#All Data glm
set.seed(1888)
innovation_glm_all_model<-train(`Innovation Variable` ~ .,
                                data= train_set_all_innovation,
                                method= "glm",
                                family= "binomial",
                                preProcess= c("center", "scale"),
                                trControl=trainControl(
                                  method= "cv",
                                  number= 10
                                ))


i_glm_all<-confusionMatrix(predict(innovation_glm_all_model, test_set_all_innovation, type="raw"),
                test_set_all_innovation$`Innovation Variable`)

i_glm_all

#Work Styles Data glm
set.seed(1888)
innovation_glm_ws_model<-train(`Innovation Variable` ~ .,
                               data= train_set_work_styles_innovation,
                               method= "glm",
                               family= "binomial",
                               preProcess= c("center", "scale"),
                               trControl=trainControl(
                                 method= "cv",
                                 number= 10
                               ))

i_glm_ws<-confusionMatrix(predict(innovation_glm_ws_model, test_set_work_styles_innovation, type="raw"),
                test_set_work_styles_innovation$`Innovation Variable`)

i_glm_ws

#All Data glmnet
set.seed(1888)
innovation_glmnet_all_model<-train(`Innovation Variable` ~ .,
                                   data= train_set_all_innovation,
                                   method= "glmnet",
                                   family= "binomial",
                                   preProcess= c("center", "scale"),
                                   tuneGrid= expand.grid(
                                     alpha=1,
                                     lambda= seq(.001,.1, length=50)
                                   ),
                                   trControl=trainControl(
                                     method= "cv",
                                     number= 10
                                   ))

i_glmnet_all<-confusionMatrix(predict(innovation_glmnet_all_model, test_set_all_innovation, type="raw"),
                test_set_all_innovation$`Innovation Variable`)

i_glmnet_all

#Work Styles Data glmnet
set.seed(1888)
innovation_glmnet_ws_model<-train(`Innovation Variable` ~ .,
                                  data= train_set_work_styles_innovation,
                                  method= "glmnet",
                                  family= "binomial",
                                  preProcess= c("center", "scale"),
                                  tuneGrid= expand.grid(
                                    alpha=1,
                                    lambda= seq(.001,.1, length=50)
                                  ),
                                  trControl=trainControl(
                                    method= "cv",
                                    number= 10
                                  ))

i_glmnet_ws<-confusionMatrix(predict(innovation_glmnet_ws_model, test_set_work_styles_innovation, type="raw"),
                test_set_work_styles_innovation$`Innovation Variable`)

i_glmnet_ws

#All Data knn
set.seed(1888)
innovation_knn_all_model <- train(`Innovation Variable` ~ .,
                                  data = train_set_all_innovation,
                                  method = "knn",
                                  preProcess = c("center", "scale"),
                                  tuneGrid = data.frame(k = seq(9, 71, 2)),
                                  trControl = trainControl(
                                    method = "cv",
                                    number = 10
                                  ))

i_knn_all<-confusionMatrix(predict(innovation_knn_all_model, test_set_all_innovation, type="raw"),
                test_set_all_innovation$`Innovation Variable`)

i_knn_all

#Work Styles Data knn
set.seed(1888)
innovation_knn_ws_model <- train(`Innovation Variable` ~ .,
                                 data = train_set_work_styles_innovation,
                                 method = "knn",
                                 preProcess = c("center", "scale"),
                                 tuneGrid = data.frame(k = seq(9, 71, 2)),
                                 trControl = trainControl(
                                   method = "cv",
                                   number = 10
                                 ))

i_knn_ws<-confusionMatrix(predict(innovation_knn_ws_model, test_set_work_styles_innovation, type="raw"),
                test_set_work_styles_innovation$`Innovation Variable`)

i_knn_ws

###ATTENTION TO DETAIL MODELS###
#Split Data Into Test and Train Sets
set.seed(1888)

train_index_attention <- createDataPartition(o_net_data$`Attention to Detail Variable`, p = 0.8, list = FALSE)

train_set_all_attention<-o_net_data[train_index_attention,]
test_set_all_attention<-o_net_data[-train_index_attention,]

train_set_all_attention<-train_set_all_attention%>%
  select(c(-Title,-`Attention to Detail`, -`Innovation Variable`, -`Stress Tolerance Variable`))
test_set_all_attention<-test_set_all_attention%>%
  select(c(-Title, -`Attention to Detail`, -`Innovation Variable`, -`Stress Tolerance Variable`))

train_set_work_styles_attention<-train_set_all_attention%>%
  select(c(`Achievement Orientation`, `Intellectual Curiosity`, `Tolerance for Ambiguity`, Initiative, Adaptability, `Self-Confidence`, Perseverance, `Leadership Orientation`, Humility, Sincerity, Empathy, Cooperation, Optimism, `Social Orientation`, Cautiousness, `Innovation`, Dependability, Integrity, `Stress Tolerance`, `Self-Control`, `Attention to Detail Variable`))
test_set_work_styles_attention<-test_set_all_attention%>%
  select(c(`Achievement Orientation`, `Intellectual Curiosity`, `Tolerance for Ambiguity`, Initiative, Adaptability, `Self-Confidence`, Perseverance, `Leadership Orientation`, Humility, Sincerity, Empathy, Cooperation, Optimism, `Social Orientation`, Cautiousness, `Innovation`, Dependability, Integrity, `Stress Tolerance`, `Self-Control`, `Attention to Detail Variable`))

train_set_all_attention <- train_set_all_attention %>%
  mutate(`Attention to Detail Variable` = factor(`Attention to Detail Variable`, labels = c("High", "Low")))
test_set_all_attention <- test_set_all_attention %>%
  mutate(`Attention to Detail Variable` = factor(`Attention to Detail Variable`, labels = c("High", "Low")))

train_set_work_styles_attention <- train_set_work_styles_attention %>%
  mutate(`Attention to Detail Variable` = factor(`Attention to Detail Variable`, labels = c("High", "Low")))
test_set_work_styles_attention <- test_set_work_styles_attention %>%
  mutate(`Attention to Detail Variable` = factor(`Attention to Detail Variable`, labels = c("High", "Low")))

#All Data glm
set.seed(1888)
attention_glm_all_model<-train(`Attention to Detail Variable` ~ .,
                               data= train_set_all_attention,
                               method= "glm",
                               family= "binomial",
                               preProcess= c("center", "scale"),
                               trControl=trainControl(
                                 method= "cv",
                                 number= 10
                               ))


atd_glm_all<-confusionMatrix(predict(attention_glm_all_model, test_set_all_attention, type="raw"),
                test_set_all_attention$`Attention to Detail Variable`)

atd_glm_all

#Work Styles Data glm
set.seed(1888)
attention_glm_ws_model<-train(`Attention to Detail Variable` ~ .,
                              data= train_set_work_styles_attention,
                              method= "glm",
                              family= "binomial",
                              preProcess= c("center", "scale"),
                              trControl=trainControl(
                                method= "cv",
                                number= 10
                              ))

atd_glm_ws<-confusionMatrix(predict(attention_glm_ws_model, test_set_work_styles_attention, type="raw"),
                test_set_work_styles_attention$`Attention to Detail Variable`)

atd_glm_ws

#All Data glmnet
set.seed(1888)
attention_glmnet_all_model<-train(`Attention to Detail Variable` ~ .,
                                  data= train_set_all_attention,
                                  method= "glmnet",
                                  family= "binomial",
                                  preProcess= c("center", "scale"),
                                  tuneGrid= expand.grid(
                                    alpha=1,
                                    lambda= seq(.001,.1, length=50)
                                  ),
                                  trControl=trainControl(
                                    method= "cv",
                                    number= 10
                                  ))

atd_glmnet_all<-confusionMatrix(predict(attention_glmnet_all_model, test_set_all_attention, type="raw"),
                test_set_all_attention$`Attention to Detail Variable`)

atd_glmnet_all

#Work Styles Data glmnet
set.seed(1888)
attention_glmnet_ws_model<-train(`Attention to Detail Variable` ~ .,
                                 data= train_set_work_styles_attention,
                                 method= "glmnet",
                                 family= "binomial",
                                 preProcess= c("center", "scale"),
                                 tuneGrid= expand.grid(
                                   alpha=1,
                                   lambda= seq(.001,.1, length=50)
                                 ),
                                 trControl=trainControl(
                                   method= "cv",
                                   number= 10
                                 ))

atd_glmnet_ws<-confusionMatrix(predict(attention_glmnet_ws_model, test_set_work_styles_attention, type="raw"),
                test_set_work_styles_attention$`Attention to Detail Variable`)

atd_glmnet_ws

#All Data knn
set.seed(1888)
attention_knn_all_model <- train(`Attention to Detail Variable` ~ .,
                                 data = train_set_all_attention,
                                 method = "knn",
                                 preProcess = c("center", "scale"),
                                 tuneGrid = data.frame(k = seq(9, 71, 2)),
                                 trControl = trainControl(
                                   method = "cv",
                                   number = 10
                                 ))

atd_knn_all<-confusionMatrix(predict(attention_knn_all_model, test_set_all_attention, type="raw"),
                test_set_all_attention$`Attention to Detail Variable`)

atd_knn_all

#Work Styles Data knn
set.seed(1888)
attention_knn_ws_model <- train(`Attention to Detail Variable` ~ .,
                                data = train_set_work_styles_attention,
                                method = "knn",
                                preProcess = c("center", "scale"),
                                tuneGrid = data.frame(k = seq(9, 71, 2)),
                                trControl = trainControl(
                                  method = "cv",
                                  number = 10
                                ))

atd_knn_ws<-confusionMatrix(predict(attention_knn_ws_model, test_set_work_styles_attention, type="raw"),
                test_set_work_styles_attention$`Attention to Detail Variable`)

atd_knn_ws

###STRESS TOLERANCE###
#Split Data Into Test and Train Sets
set.seed(1888)

train_index_stress <- createDataPartition(o_net_data$`Stress Tolerance`, p = 0.8, list = FALSE)

train_set_all_stress<-o_net_data[train_index_stress,]
test_set_all_stress<-o_net_data[-train_index_stress,]

train_set_all_stress<-train_set_all_stress%>%
  select(c(-Title, -`Stress Tolerance`, -`Innovation Variable`, -`Attention to Detail Variable`))
test_set_all_stress<-test_set_all_stress%>%
  select(c(-Title, -`Stress Tolerance`, -`Innovation Variable`, -`Attention to Detail Variable`))

train_set_work_styles_stress<-train_set_all_stress%>%
  select(c(`Achievement Orientation`, `Intellectual Curiosity`, `Tolerance for Ambiguity`, Initiative, Adaptability, `Self-Confidence`, Perseverance, `Leadership Orientation`, Humility, Sincerity, Empathy, Cooperation, Optimism, `Social Orientation`, Cautiousness, `Innovation`, Dependability, Integrity, `Attention to Detail`, `Self-Control`, `Stress Tolerance Variable`))
test_set_work_styles_stress<-test_set_all_stress%>%
  select(c(`Achievement Orientation`, `Intellectual Curiosity`, `Tolerance for Ambiguity`, Initiative, Adaptability, `Self-Confidence`, Perseverance, `Leadership Orientation`, Humility, Sincerity, Empathy, Cooperation, Optimism, `Social Orientation`, Cautiousness, `Innovation`, Dependability, Integrity, `Attention to Detail`, `Self-Control`, `Stress Tolerance Variable`))

train_set_all_stress <- train_set_all_stress %>%
  mutate(`Stress Tolerance Variable` = factor(`Stress Tolerance Variable`, labels = c("High", "Low")))
test_set_all_stress <- test_set_all_stress %>%
  mutate(`Stress Tolerance Variable` = factor(`Stress Tolerance Variable`, labels = c("High", "Low")))

train_set_work_styles_stress <- train_set_work_styles_stress %>%
  mutate(`Stress Tolerance Variable` = factor(`Stress Tolerance Variable`, labels = c("High", "Low")))
test_set_work_styles_stress <- test_set_work_styles_stress %>%
  mutate(`Stress Tolerance Variable` = factor(`Stress Tolerance Variable`, labels = c("High", "Low")))

#All Data glm
set.seed(1888)
stress_glm_all_model<-train(`Stress Tolerance Variable` ~ .,
                            data= train_set_all_stress,
                            method= "glm",
                            family= "binomial",
                            preProcess= c("center", "scale"),
                            trControl=trainControl(
                              method= "cv",
                              number= 10
                            ))


st_glm_all<-confusionMatrix(predict(stress_glm_all_model, test_set_all_stress, type="raw"),
                test_set_all_stress$`Stress Tolerance Variable`)

st_glm_all

#Work Styles Data glm
set.seed(1888)
stress_glm_ws_model<-train(`Stress Tolerance Variable` ~ .,
                           data= train_set_work_styles_stress,
                           method= "glm",
                           family= "binomial",
                           preProcess= c("center", "scale"),
                           trControl=trainControl(
                             method= "cv",
                             number= 10
                           ))

st_glm_ws<-confusionMatrix(predict(stress_glm_ws_model, test_set_work_styles_stress, type="raw"),
                test_set_work_styles_stress$`Stress Tolerance Variable`)

st_glm_ws

#All Data glmnet
set.seed(1888)
stress_glmnet_all_model<-train(`Stress Tolerance Variable` ~ .,
                               data= train_set_all_stress,
                               method= "glmnet",
                               family= "binomial",
                               preProcess= c("center", "scale"),
                               tuneGrid= expand.grid(
                                 alpha=1,
                                 lambda= seq(.001,.1, length=50)
                               ),
                               trControl=trainControl(
                                 method= "cv",
                                 number= 10
                               ))

st_glmnet_all<-confusionMatrix(predict(stress_glmnet_all_model, test_set_all_stress, type="raw"),
                test_set_all_stress$`Stress Tolerance Variable`)

st_glmnet_all

#Work Styles Data glmnet
set.seed(1888)
stress_glmnet_ws_model<-train(`Stress Tolerance Variable` ~ .,
                              data= train_set_work_styles_stress,
                              method= "glmnet",
                              family= "binomial",
                              preProcess= c("center", "scale"),
                              tuneGrid= expand.grid(
                                alpha=1,
                                lambda= seq(.001,.1, length=50)
                              ),
                              trControl=trainControl(
                                method= "cv",
                                number= 10
                              ))

st_glmnet_ws<-confusionMatrix(predict(stress_glmnet_ws_model, test_set_work_styles_stress, type="raw"),
                test_set_work_styles_stress$`Stress Tolerance Variable`)

st_glmnet_ws

#All Data knn
set.seed(1888)
stress_knn_all_model <- train(`Stress Tolerance Variable` ~ .,
                              data = train_set_all_stress,
                              method = "knn",
                              preProcess = c("center", "scale"),
                              tuneGrid = data.frame(k = seq(9, 71, 2)),
                              trControl = trainControl(
                                method = "cv",
                                number = 10
                              ))

st_knn_all<-confusionMatrix(predict(stress_knn_all_model, test_set_all_stress, type="raw"),
                test_set_all_stress$`Stress Tolerance Variable`)

st_knn_all

#Work Styles Data knn
set.seed(1888)
stress_knn_ws_model <- train(`Stress Tolerance Variable` ~ .,
                             data = train_set_work_styles_stress,
                             method = "knn",
                             preProcess = c("center", "scale"),
                             tuneGrid = data.frame(k = seq(9, 71, 2)),
                             trControl = trainControl(
                               method = "cv",
                               number = 10
                             ))

st_knn_ws<-confusionMatrix(predict(stress_knn_ws_model, test_set_work_styles_stress, type="raw"),
                test_set_work_styles_stress$`Stress Tolerance Variable`)

st_knn_ws

###DATA VIS 
#formatting
confusion_matrix_values <- rbind(
  data.frame(Model = "Innovation GLM All Data",
             Accuracy = i_glm_all$overall["Accuracy"],
             Sensitivity = i_glm_all$byClass["Sensitivity"],
             Specificity = i_glm_all$byClass["Specificity"]),
  data.frame(Model = "Innovation GLM Work Styles",
             Accuracy = i_glm_ws$overall["Accuracy"],
             Sensitivity = i_glm_ws$byClass["Sensitivity"],
             Specificity = i_glm_ws$byClass["Specificity"]),
  data.frame(Model = "Innovation GLMnet All Data",
             Accuracy = i_glmnet_all$overall["Accuracy"],
             Sensitivity = i_glmnet_all$byClass["Sensitivity"],
             Specificity = i_glmnet_all$byClass["Specificity"]),
  data.frame(Model = "Innovation GLMnet Work Styles",
             Accuracy = i_glmnet_ws$overall["Accuracy"],
             Sensitivity = i_glmnet_ws$byClass["Sensitivity"],
             Specificity = i_glmnet_ws$byClass["Specificity"]),
  data.frame(Model = "Innovation KNN All Data",
             Accuracy = i_knn_all$overall["Accuracy"],
             Sensitivity = i_knn_all$byClass["Sensitivity"],
             Specificity = i_knn_all$byClass["Specificity"]),
  data.frame(Model = "Innovation KNN Work Styles",
             Accuracy = i_knn_ws$overall["Accuracy"],
             Sensitivity = i_knn_ws$byClass["Sensitivity"],
             Specificity = i_knn_ws$byClass["Specificity"]),
             
  data.frame(Model = "Attention to Detail GLM All Data",
             Accuracy = atd_glm_all$overall["Accuracy"],
             Sensitivity = atd_glm_all$byClass["Sensitivity"],
             Specificity = atd_glm_all$byClass["Specificity"]),
  data.frame(Model = "Attention to Detail GLM Work Styles",
             Accuracy = atd_glm_ws$overall["Accuracy"],
             Sensitivity = atd_glm_ws$byClass["Sensitivity"],
             Specificity = atd_glm_ws$byClass["Specificity"]),
  data.frame(Model = "Attention to Detail GLMnet All Data",
             Accuracy = atd_glmnet_all$overall["Accuracy"],
             Sensitivity = atd_glmnet_all$byClass["Sensitivity"],
             Specificity = atd_glmnet_all$byClass["Specificity"]),
  data.frame(Model = "Attention to Detail GLMnet Work Styles",
             Accuracy = atd_glmnet_ws$overall["Accuracy"],
             Sensitivity = atd_glmnet_ws$byClass["Sensitivity"],
             Specificity = atd_glmnet_ws$byClass["Specificity"]),
  data.frame(Model = "Attention to Detail KNN All Data",
             Accuracy = atd_knn_all$overall["Accuracy"],
             Sensitivity = atd_knn_all$byClass["Sensitivity"],
             Specificity = atd_knn_all$byClass["Specificity"]),
  data.frame(Model = "Attention to Detail KNN Work Styles",
             Accuracy = atd_knn_ws$overall["Accuracy"],
             Sensitivity = atd_knn_ws$byClass["Sensitivity"],
             Specificity = atd_knn_ws$byClass["Specificity"]),
  
  data.frame(Model = "Stress Tolerance GLM All Data",
             Accuracy = st_glm_all$overall["Accuracy"],
             Sensitivity = st_glm_all$byClass["Sensitivity"],
             Specificity = st_glm_all$byClass["Specificity"]),
  data.frame(Model = "Stress Tolerance GLM Work Styles",
             Accuracy = st_glm_ws$overall["Accuracy"],
             Sensitivity = st_glm_ws$byClass["Sensitivity"],
             Specificity = st_glm_ws$byClass["Specificity"]),
  data.frame(Model = "Stress Tolerance GLMnet All Data",
             Accuracy = st_glmnet_all$overall["Accuracy"],
             Sensitivity = st_glmnet_all$byClass["Sensitivity"],
             Specificity = st_glmnet_all$byClass["Specificity"]),
  data.frame(Model = "Stress Tolerance GLMnet Work Styles",
             Accuracy = st_glmnet_ws$overall["Accuracy"],
             Sensitivity = st_glmnet_ws$byClass["Sensitivity"],
             Specificity = st_glmnet_ws$byClass["Specificity"]),
  data.frame(Model = "Stress Tolerance KNN All Data",
             Accuracy = st_knn_all$overall["Accuracy"],
             Sensitivity = st_knn_all$byClass["Sensitivity"],
             Specificity = st_knn_all$byClass["Specificity"]),
  data.frame(Model = "Stress Tolerance KNN Work Styles",
             Accuracy = st_knn_ws$overall["Accuracy"],
             Sensitivity = st_knn_ws$byClass["Sensitivity"],
             Specificity = st_knn_ws$byClass["Specificity"])
  )

rownames(confusion_matrix_values) <- confusion_matrix_values[,1]

confusion_matrix_values[,1]<-NULL

confusion_matrix_values<-confusion_matrix_values%>%
  mutate(`Work Style Element` = c("Innovation", "Innovation", "Innovation", "Innovation", "Innovation", "Innovation", "Attention to Detail", "Attention to Detail", "Attention to Detail", "Attention to Detail", "Attention to Detail", "Attention to Detail", "Stress Tolerance", "Stress Tolerance", "Stress Tolerance", "Stress Tolerance", "Stress Tolerance", "Stress Tolerance"))%>%
  mutate(`Model Type` = c("GLM", "GLM", "Lasso", "Lasso", "kNN", "kNN", "GLM", "GLM", "Lasso", "Lasso", "kNN", "kNN", "GLM", "GLM", "Lasso", "Lasso", "kNN", "kNN"))%>%
  mutate(`Data Set` = c("All Data", "Work Styles", "All Data", "Work Styles","All Data", "Work Styles","All Data", "Work Styles","All Data", "Work Styles","All Data", "Work Styles","All Data", "Work Styles","All Data", "Work Styles","All Data", "Work Styles"))

confusion_matrix_values<-confusion_matrix_values%>%
  mutate(Labels = c("Innovation Regression All Data", "Innovation Regression Work Styles", "Innovation Lasso All Data", "Innovation Lasso Work Styles", "Innovation KNN All Data", "Innovation KNN Work Styles", "Attention to Detail Regression All Data", "Attention to Detail Regression Work Styles", "Attention to Detail Lasso All Data", "Attention to Detail Lasso Work Styles", "Attention to Detail KNN All Data", "Attention to Detail KNN Work Styles", "Stress Tolerance Regression All Data", "Stress Tolerance Regression Work Styles", "Stress Tolerance Lasso All Data", "Stress Tolerance Lasso Work Styles", "Stress Tolerance KNN All Data", "Stress Tolerance KNN Work Styles"))

#heatmap
confusion_matrix_values %>%
  select(Labels, Accuracy, Sensitivity, Specificity) %>%
  pivot_longer(-Labels, names_to = "Metric", values_to = "Value") %>%
  mutate(Labels = factor(Labels, levels = rev(unique(Labels))),
         Metric = factor(Metric, levels = c("Accuracy", "Sensitivity", "Specificity"))) %>%
  ggplot(aes(x = Metric, y = Labels, fill = Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Value, 2)), size = 3) +
  scale_fill_viridis_c(direction= -1) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

#Parallel Coordinates Graph Work Style Element
onet_pallet<-c("#f97316", "#44aa99", "#aa3377")

ggparcoord(confusion_matrix_values,
           columns = 1:3, groupColumn = 4, 
           scale="globalminmax",
           showPoints= TRUE) +
  scale_color_manual(values= onet_pallet) +
  geom_text_repel(data=confusion_matrix_values,
            aes(x=1, y = Accuracy, label= Labels),
            hjust= 1.2, size=2, inherit.aes=FALSE,
            max.overlaps= Inf,
            segment.size=.2,
            direction= "y") +
  scale_x_discrete(expand= expansion(mult= c(.45, .075))) +
  coord_cartesian(clip= "off")


#Parallel Coordinates Graph Data Set Differences
ggparcoord(confusion_matrix_values,
           columns = 1:3, groupColumn = 6, 
           scale="globalminmax",
           showPoints= TRUE) +
  scale_color_manual(values= onet_pallet) +
  geom_text_repel(data=confusion_matrix_values,
                  aes(x=1, y = Accuracy, label= Labels),
                  hjust= 1.2, size=2, inherit.aes=FALSE,
                  max.overlaps= Inf,
                  segment.size=.2,
                  direction= "y") +
  scale_x_discrete(expand= expansion(mult= c(.45, .075))) +
  coord_cartesian(clip= "off")

#Parallel Coordinates Graph Model Type
ggparcoord(confusion_matrix_values,
           columns = 1:3, groupColumn = 5, 
           scale="globalminmax",
           showPoints= TRUE) +
  scale_color_manual(values= onet_pallet) +
  geom_text_repel(data=confusion_matrix_values,
                  aes(x=1, y = Accuracy, label= Labels),
                  hjust= 1.2, size=2, inherit.aes=FALSE,
                  max.overlaps= Inf,
                  segment.size=.2,
                  direction= "y") +
  scale_x_discrete(expand= expansion(mult= c(.45, .075))) +
  coord_cartesian(clip= "off")

#Parallel Coordinates Graph Model Type for only innovation models
innovation_graph <- confusion_matrix_values %>%
  filter(`Work Style Element` == "Innovation")

innovation_graph%>%
  ggparcoord(innovation_graph,
           columns = 1:3, groupColumn = 5,
           scale = "globalminmax",
           showPoints = TRUE) +
  scale_color_manual(values = onet_pallet) +
  geom_text_repel(data = innovation_graph,
                  aes(x = 1, y = Accuracy, label = Labels),
                  hjust = 1.2, size = 2, inherit.aes = FALSE,
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  direction = "y") +
  scale_x_discrete(expand = expansion(mult = c(0.45, 0.075))) +
  coord_cartesian(clip = "off")

#Parallel Coordinates Graph Model Type for only attention to detail models
attention_graph <- confusion_matrix_values %>%
  filter(`Work Style Element` == "Attention to Detail")

attention_graph%>%
  ggparcoord(attention_graph,
             columns = 1:3, groupColumn = 5,
             scale = "globalminmax",
             showPoints = TRUE) +
  scale_color_manual(values = onet_pallet) +
  geom_text_repel(data = attention_graph,
                  aes(x = 1, y = Accuracy, label = Labels),
                  hjust = 1.2, size = 2, inherit.aes = FALSE,
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  direction = "y") +
  scale_x_discrete(expand = expansion(mult = c(0.45, 0.075))) +
  coord_cartesian(clip = "off")

#Parallel Coordinates Graph Model Type for only stress tolerance models
stress_graph <- confusion_matrix_values %>%
  filter(`Work Style Element` == "Stress Tolerance")

stress_graph%>%
  ggparcoord(stress_graph,
             columns = 1:3, groupColumn = 5,
             scale = "globalminmax",
             showPoints = TRUE) +
  scale_color_manual(values = onet_pallet) +
  geom_text_repel(data = stress_graph,
                  aes(x = 1, y = Accuracy, label = Labels),
                  hjust = 1.2, size = 2, inherit.aes = FALSE,
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  direction = "y") +
  scale_x_discrete(expand = expansion(mult = c(0.45, 0.075))) +
  coord_cartesian(clip = "off")
