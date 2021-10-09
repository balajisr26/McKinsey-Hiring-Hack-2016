#0.183767
train=read.csv("train.csv",stringsAsFactors = F)
test=read.csv("test.csv",stringsAsFactors = F)
str(train)

train$Ind<-1
test$Ind<-2

test$Email_Status<--999

comb<-rbind(train,test)

comb$Total_Past_Communications<-ifelse(is.na(comb$Total_Past_Communications)
                                    ,ifelse(comb$Email_Campaign_Type==1,0,comb$Total_Past_Communications)
                                      ,comb$Total_Past_Communications)


comb$TPC_Bin<-ifelse(comb$Total_Past_Communications==0,1,
                     ifelse(comb$Total_Past_Communications >0 & comb$Total_Past_Communications<20,2
                      ,ifelse(comb$Total_Past_Communications>=20 & comb$Total_Past_Communications<21,3
                      ,ifelse(comb$Total_Past_Communications>=21 & comb$Total_Past_Communications<24,4
                      ,ifelse(comb$Total_Past_Communications>=24 & comb$Total_Past_Communications<30,5
                      ,ifelse(comb$Total_Past_Communications>=30 & comb$Total_Past_Communications<38,6
                      ,ifelse(comb$Total_Past_Communications>=38 & comb$Total_Past_Communications<40,7
                      ,ifelse(comb$Total_Past_Communications>=40 & comb$Total_Past_Communications<50,8,9))))))))

comb$WC_Bin<-ifelse(comb$Word_Count<275,1
                    ,ifelse(comb$Word_Count<392,2
                            ,ifelse(comb$Word_Count<484,3
                                    ,ifelse(comb$Word_Count<593,4
                                            ,ifelse(comb$Word_Count<661,5
                                                    ,ifelse(comb$Word_Count<1140,6
                                                            ,ifelse(comb$Word_Count<1271,7,8)))))))



comb$SHS_Bin<-ifelse(comb$Subject_Hotness_Score <.1,1
                    ,ifelse(comb$Subject_Hotness_Score<.4,2
                    ,ifelse(comb$Subject_Hotness_Score<.5,3
                    ,ifelse(comb$Subject_Hotness_Score<2.2,4
                    ,ifelse(comb$Subject_Hotness_Score<4.2,5,6)))))

str(comb)

library(sqldf)

comb<-sqldf("select comb.*,A.no_of_zero from
            comb inner join (select Email_Campaign_Type,Email_Type
            ,Email_Status,count(*) as no_of_zero
            from comb where Ind=1
            and Email_Status=0
            group by Email_Campaign_Type,Email_Type,Email_Status) A
            on comb.Email_Campaign_Type=A.Email_Campaign_Type
            and comb.Email_Type=A.Email_Type")


comb<-sqldf("select comb.*,A.no_of_one from
             comb inner join (select Email_Campaign_Type,Email_Type
             ,Email_Status,count(*) as no_of_one
              from comb where Ind=1
              and Email_Status=1
              group by Email_Campaign_Type,Email_Type,Email_Status) A
              on comb.Email_Campaign_Type=A.Email_Campaign_Type
              and comb.Email_Type=A.Email_Type")

comb<-sqldf("select comb.*,A.no_of_two from
                            comb inner join (select Email_Campaign_Type,Email_Type
            ,Email_Status,count(*) as no_of_two
            from comb where Ind=1
            and Email_Status=2
            group by Email_Campaign_Type,Email_Type,Email_Status) A
            on comb.Email_Campaign_Type=A.Email_Campaign_Type
            and comb.Email_Type=A.Email_Type")

comb<-sqldf("select comb.*,A.no_of_zero_3 from
            comb inner join (select Email_Campaign_Type,Email_Type
            ,Email_Source_Type,Email_Status,count(*) as no_of_zero_3
            from comb where Ind=1
            and Email_Status=0
            group by Email_Campaign_Type,Email_Type,Email_Source_Type
            ,Email_Status) A
            on comb.Email_Campaign_Type=A.Email_Campaign_Type
            and comb.Email_Type=A.Email_Type
            and comb.Email_Source_Type=A.Email_Source_Type")


comb<-sqldf("select comb.*,A.no_of_one_3 from
            comb inner join (select Email_Campaign_Type,Email_Type
            ,Email_Source_Type,Email_Status,count(*) as no_of_one_3
            from comb where Ind=1
            and Email_Status=1
            group by Email_Campaign_Type,Email_Type,Email_Source_Type
            ,Email_Status) A
            on comb.Email_Campaign_Type=A.Email_Campaign_Type
            and comb.Email_Type=A.Email_Type
            and comb.Email_Source_Type=A.Email_Source_Type")

comb<-sqldf("select comb.*,A.no_of_two_3 from
            comb inner join (select Email_Campaign_Type,Email_Type
            ,Email_Source_Type,Email_Status,count(*) as no_of_two_3
            from comb where Ind=1
            and Email_Status=2
            group by Email_Campaign_Type,Email_Type,Email_Source_Type
            ,Email_Status) A
            on comb.Email_Campaign_Type=A.Email_Campaign_Type
            and comb.Email_Type=A.Email_Type
            and comb.Email_Source_Type=A.Email_Source_Type")

comb$Subject_Hotness_Score<-scale(comb$Subject_Hotness_Score,scale=TRUE,center = FALSE)
comb$Word_Count<-scale(comb$Word_Count,scale = TRUE,center = FALSE)
comb$Total_Past_Communications=scale(comb$Total_Past_Communications,scale=TRUE,center=FALSE)
comb$Total_Links=scale(comb$Total_Links,scale=TRUE,center=FALSE)
comb$Total_Images=scale(comb$Total_Images,scale = TRUE,center = FALSE)

str(comb)

comb[is.na(comb)]<--1

feature.names=names(comb[,! names(comb) %in% c("Email_ID","Ind","Customer_Location","Time_Email_sent_Category")])

feature.names

comb$Email_Type=as.factor(comb$Email_Type)
comb$Email_Source_Type=as.factor(comb$Email_Source_Type)
comb$Email_Campaign_Type=as.factor(comb$Email_Campaign_Type)

train<-comb[comb$Ind==1,]
test<-comb[comb$Ind==2,]

library(Matrix)

sparse_matrix<-sparse.model.matrix(Email_Status~.-1,data=train[,feature.names])

lbl=as.factor(train[,"Email_Status"])

library(xgboost)
set.seed(23)

model_xgb_cv <- xgb.cv(data=sparse_matrix, label=as.matrix(lbl), objective="multi:softmax"
                       , num_class=3, nfold=10, nrounds=160, eta=0.1, max_depth=6
                       , subsample=0.9, colsample_bytree=0.9, min_child_weight=1, eval_metric="merror", prediction=T
                       ,print.every.n = 10,num_parallel_tree=4)

nround=which(model_xgb_cv$dt$test.merror.mean  ==min(model_xgb_cv$dt$test.merror.mean))

model_xgb <- xgboost(data=sparse_matrix, label=as.matrix(lbl), objective="multi:softmax"
                     , num_class=3, nrounds=nround, eta=0.1, max_depth=6
                     , subsample=0.9, colsample_bytree=0.9, min_child_weight=1
                     , eval_metric='merror',print.every.n = 10,num_parallel_tree=4)

sparse_matrix_test<-sparse.model.matrix(Email_Status~.-1,data=test[,feature.names])

predxg=predict(model_xgb,sparse_matrix_test)

MySubmission = data.frame(Email_ID=test$Email_ID,Email_Status = predxg)

write.csv(MySubmission, "xgb26.csv", row.names=FALSE)

min(model_xgb_cv$dt$test.merror.mean)

xgb.dump(model_xgb,"xgb26",wih.stats = TRUE)



#imp<-xgb.importance(feature_names=sparse_matrix@Dimnames[[2]],filename_dump="xgb17")
#imp

(13449+503+7)/nrow(val)




