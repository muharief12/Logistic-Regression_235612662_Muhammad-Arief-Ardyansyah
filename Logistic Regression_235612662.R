# %% [code] {"_execution_state":"idle","execution":{"iopub.status.busy":"2023-10-22T05:10:24.190130Z","iopub.execute_input":"2023-10-22T05:10:24.192539Z","iopub.status.idle":"2023-10-22T05:10:25.654935Z"}}
# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load

library(tidyverse) # metapackage of all tidyverse packages

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:11:10.922265Z","iopub.execute_input":"2023-10-22T05:11:10.962270Z","iopub.status.idle":"2023-10-22T05:11:11.054619Z"}}
library(readxl)
df <- read_excel("E:/Bismillah S2 Teknik Industri di UAJY/Semester 1/Data Mining - Business Intenlligence/Meeting - 7/Material Kit/CreditApproval (1) (1) (1).xlsx")

# %% [markdown]
# # Data Preprocessing

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:11:13.504295Z","iopub.execute_input":"2023-10-22T05:11:13.506447Z","iopub.status.idle":"2023-10-22T05:11:13.560185Z"}}
str(df)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:11:16.100699Z","iopub.execute_input":"2023-10-22T05:11:16.103158Z","iopub.status.idle":"2023-10-22T05:11:16.144037Z"}}
unique(df$Status)
unique(df$Homeowner)
unique(df$Decision)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:36.196601Z","iopub.execute_input":"2023-10-22T05:17:36.199206Z","iopub.status.idle":"2023-10-22T05:17:36.219944Z"}}
colnames(df) <- c("home_owner","credit_score","yoc_history","rev_bal","rev_util","status","decision")

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:40.179985Z","iopub.execute_input":"2023-10-22T05:17:40.182174Z","iopub.status.idle":"2023-10-22T05:17:40.235131Z"}}
head(df)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:44.046172Z","iopub.execute_input":"2023-10-22T05:17:44.048449Z","iopub.status.idle":"2023-10-22T05:17:44.078954Z"}}
str(df)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:46.756192Z","iopub.execute_input":"2023-10-22T05:17:46.758310Z","iopub.status.idle":"2023-10-22T05:17:46.805858Z"}}
cat("Total Missing Value dari feature home_owner:",sum(is.na(df$home_owner)), "\n")
cat("Total Missing Value dari feature credit_score:", sum(is.na(df$credit_score)), "\n")
cat("Total Missing Value dari feature yoc_history:", sum(is.na(df$yoc_history)), "\n")
cat("Total Missing Value dari feature rev_bal:", sum(is.na(df$rev_bal)), "\n")
cat("Total Missing Value dari feature rev_util:", sum(is.na(df$rev_util)), "\n")
cat("Total Missing Value dari feature status:", sum(is.na(df$status)), "\n")
cat("Total Missing Value dari feature decision:", sum(is.na(df$decision)), "\n")

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:49.530912Z","iopub.execute_input":"2023-10-22T05:17:49.532869Z","iopub.status.idle":"2023-10-22T05:17:49.556276Z"}}
df1 <- subset(df, select= -which(sapply(df, is.character)))
str(df1)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:51.672139Z","iopub.execute_input":"2023-10-22T05:17:51.674453Z","iopub.status.idle":"2023-10-22T05:17:51.995327Z"}}
boxplot(df1[,1])
summary(df1[,1])

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:54.908028Z","iopub.execute_input":"2023-10-22T05:17:54.910149Z","iopub.status.idle":"2023-10-22T05:17:55.021717Z"}}
boxplot(df1[,2], names=c("yoc_history"))
summary(df1[,2])

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:57.362592Z","iopub.execute_input":"2023-10-22T05:17:57.364516Z","iopub.status.idle":"2023-10-22T05:17:57.455498Z"}}
boxplot(df1[,3])
summary(df1[,3])

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:17:59.961450Z","iopub.execute_input":"2023-10-22T05:17:59.963198Z","iopub.status.idle":"2023-10-22T05:18:00.008804Z"}}
# Hitung lower fence dan upper fence untuk setiap kolom
lower_fence <- quantile(df1$rev_bal, probs = 0.25) - 1.5 * IQR(df1$rev_bal)
upper_fence <- quantile(df1$rev_bal, probs = 0.75) + 1.5 * IQR(df1$rev_bal)
# Identifikasi outlier pada setiap kolom
outliers <- df1$rev_bal < lower_fence | df1$rev_bal > upper_fence

# Tampilkan outlier untuk setiap kolom
print("Outliers:")
print(outliers)

# Tampilkan data frame setelah penanganan outlier (misalnya, menghapus outlier)
data_frame_no_outliers <- df1$rev_bal
data_frame_no_outliers[outliers] <- NA  # Mengubah outlier menjadi NA (tidak disarankan untuk data aktual)
df1$rev_bal <- data_frame_no_outliers
print("Data frame setelah penanganan outlier:")
print(df1$rev_bal)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:03.546837Z","iopub.execute_input":"2023-10-22T05:18:03.548632Z","iopub.status.idle":"2023-10-22T05:18:03.565624Z"}}
sum(is.na(df1$rev_bal))

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:05.963167Z","iopub.execute_input":"2023-10-22T05:18:05.965465Z","iopub.status.idle":"2023-10-22T05:18:05.994563Z"}}
mean_df1_rev_bal <- mean(df1$rev_bal, na.rm=TRUE)
mean_df1_rev_bal

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:10.359795Z","iopub.execute_input":"2023-10-22T05:18:10.361796Z","iopub.status.idle":"2023-10-22T05:18:10.379514Z"}}
df1$rev_bal <- ifelse(is.na(df1$rev_bal), mean_df1_rev_bal, df1$rev_bal)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:12.732320Z","iopub.execute_input":"2023-10-22T05:18:12.744662Z","iopub.status.idle":"2023-10-22T05:18:12.766700Z"}}
summary(df1[,3])

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:15.312444Z","iopub.execute_input":"2023-10-22T05:18:15.314547Z","iopub.status.idle":"2023-10-22T05:18:15.408217Z"}}
boxplot(df1[,4])
summary(df1[,4])

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:18.340807Z","iopub.execute_input":"2023-10-22T05:18:18.342761Z","iopub.status.idle":"2023-10-22T05:18:18.356757Z"}}
df2 <- df

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:20.492110Z","iopub.execute_input":"2023-10-22T05:18:20.494353Z","iopub.status.idle":"2023-10-22T05:18:20.526098Z"}}
str(df2)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:22.829063Z","iopub.execute_input":"2023-10-22T05:18:22.831108Z","iopub.status.idle":"2023-10-22T05:18:22.856442Z"}}
summary(df2$rev_bal)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:25.437027Z","iopub.execute_input":"2023-10-22T05:18:25.439102Z","iopub.status.idle":"2023-10-22T05:18:25.456002Z"}}
df2$rev_bal <- df1$rev_bal

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:27.555749Z","iopub.execute_input":"2023-10-22T05:18:27.557704Z","iopub.status.idle":"2023-10-22T05:18:27.575459Z"}}
summary(df2$rev_bal)

# %% [markdown]
# # Credit Approval Metric Study Case
# 1. Spesific
#   - Identifikasi Profil Nasabah berdasarkan kepemilikan rumah.
# 2. Measurable
#   - Sebaran data Credit History Nasabah terhadap kepemilikan rumah
# 3. Achievable
#   - Peforma Nasabah yang memiliki rumah terhadap revolving utilization
# 4. Relevant
#   - Hubungan Revolving Balance terhadap Revolving Util ditingjau dari Nasabah yang memiliki Rumah.
# 5. Time-bound
#   - Sebaran data dari Histori Kredit terhadap diterimanya pengajuan kredit.

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:31.058923Z","iopub.execute_input":"2023-10-22T05:18:31.061018Z","iopub.status.idle":"2023-10-22T05:18:31.117514Z"}}
library(ggplot2)
library(lattice)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:18:33.172987Z","iopub.execute_input":"2023-10-22T05:18:33.174899Z","iopub.status.idle":"2023-10-22T05:18:33.757684Z"}}
#Spesific
#Identifikasi Profil Nasabah berdasarkan kepemilikan rumah.
plotdata <- df2 %>%
  count(home_owner) %>%
  arrange(desc(home_owner)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5 *prop)

ggplot(plotdata,
       aes(x = "",
           y = prop,
           fill = home_owner)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  geom_text(aes(label = paste0(home_owner, ": ", round(prop, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Profil Nasabah berdasarkan Home Owner") +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title horizontally
  )

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:19:01.110666Z","iopub.execute_input":"2023-10-22T05:19:01.113857Z","iopub.status.idle":"2023-10-22T05:19:01.542191Z"}}
#Spesific
#Identifikasi Profil Nasabah berdasarkan kepemilikan rumah.
plotdata <- df2 %>% count(home_owner)

ggplot(plotdata, aes(x = home_owner,y = n)) +
  geom_bar(stat = "identity", fill="blue") +
  geom_text(aes(label = n), vjust=-0.5) +
  labs(x = "Home Owner", y = "Frequency",
       title = "Participants by home owner") +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title horizontally
  )

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:21:49.926493Z","iopub.execute_input":"2023-10-22T05:21:49.930171Z","iopub.status.idle":"2023-10-22T05:21:50.139118Z"}}
#Measurable
#Sebaran data Rata-Rata Years of Credit History Nasabah terhadap kepemilikan rumah
categories = c("N", "Y")
valueN = mean(df2$yoc_history[df2$home_owner == "N"])
valueY = mean(df2$yoc_history[df2$home_owner == "Y"])

barchart(cbind(valueN,valueY) ~ categories,
         col = c("blue", "green"),  # Warna batang
         auto.key = list(space = "right"),  # Menambahkan legenda
         main = "Rata-Rata Years of Credit History Nasabah terhadap kepemilikan rumah",
         xlab = "Home Owner",
         ylab = "yoc history"
)
cat("Rata-rata Years of Credit History Nasabah yang tidak Memiliki Rumah: ", valueN, "Years", "\n")
cat("Rata-rata Years of Credit History Nasabah yang Memiliki Rumah: ", valueY, "Years", "\n")

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:17:35.275681Z","iopub.execute_input":"2023-10-22T06:17:35.281919Z","iopub.status.idle":"2023-10-22T06:17:35.320204Z"}}
str(df2)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T05:46:17.843879Z","iopub.execute_input":"2023-10-22T05:46:17.845758Z","iopub.status.idle":"2023-10-22T05:46:17.990144Z"}}
#Achievable
#Peforma Nasabah ditinjau dari kepemilikan rumah terhadap revolving utilization
ru_by_ho = df2$rev_util[df2$home_owner == "Y"]

histogram(~ru_by_ho, data=df2, col="green",main="Peforma Nasabah ditinjau dari \n kepemilikan rumah terhadap revolving utilization")
unique(df2$rev_util)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:18:52.344628Z","iopub.execute_input":"2023-10-22T06:18:52.346840Z","iopub.status.idle":"2023-10-22T06:18:53.071853Z"}}
#Relevant
#Hubungan Revolving Balance terhadap Revolving Util ditingjau dari Nasabah yang memiliki Rumah
df2_hoy = df2[df2$home_owner == "Y",]

ggplot(data = df2_hoy, aes(x = rev_util, y = rev_bal)) +
  geom_point(color = "blue", shape = 16) +  # Menambahkan titik
  labs(title = "Hubungan antara Revolving Utilization terhadap Revolving Balance", x = "rev_util", y = "rev_bal") +
  geom_smooth(method = "lm", color = "red")

cor.test(df2_hoy$rev_bal, df2_hoy$rev_util)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:05:10.425958Z","iopub.execute_input":"2023-10-22T06:05:10.428789Z","iopub.status.idle":"2023-10-22T06:05:10.680305Z"}}
#Time-bound
#Sebaran data dari Histori Kredit terhadap diterimanya pengajuan kredit.
categories = c("Reject", "Approve")
valueN = mean(df2$yoc_history[df2$decision == "Reject"])
valueY = mean(df2$yoc_history[df2$decision == "Approve"])

barchart(cbind(valueN,valueY) ~ categories,
         col = c("blue", "green"),  # Warna batang
         auto.key = list(space = "right"),  # Menambahkan legenda
         main = "Rata-Rata Years of Credit History Berdasarkan Decision",
         xlab = "Decision",
         ylab = "Rata-rata Years of Credit History"
)
cat("Rata-rata Years of Credit History Nasabah yang memiliki Decision Reject : ", valueN, "\n")
cat("Rata-rata Years of Credit History Nasabah yang memiliki Decision Approve : ", valueY, "\n")

# %% [markdown]
# # Data Preprocessing Pengembangan Model

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:30:50.956241Z","iopub.execute_input":"2023-10-22T06:30:50.958480Z","iopub.status.idle":"2023-10-22T06:30:50.975411Z"}}
df3 <- df2

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:30:53.407499Z","iopub.execute_input":"2023-10-22T06:30:53.409320Z","iopub.status.idle":"2023-10-22T06:30:53.456991Z"}}
head(df3)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:30:56.802173Z","iopub.execute_input":"2023-10-22T06:30:56.803995Z","iopub.status.idle":"2023-10-22T06:30:56.828631Z"}}
str(df3)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:31:03.296303Z","iopub.execute_input":"2023-10-22T06:31:03.298166Z","iopub.status.idle":"2023-10-22T06:31:03.323499Z"}}
df3$home_owner <- factor(df3$home_owner, levels = c("N","Y"), labels = c(0,1))
str(df3)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:31:08.218922Z","iopub.execute_input":"2023-10-22T06:31:08.220685Z","iopub.status.idle":"2023-10-22T06:31:08.238649Z"}}
unique(df3$home_owner)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:31:12.746367Z","iopub.execute_input":"2023-10-22T06:31:12.748453Z","iopub.status.idle":"2023-10-22T06:31:12.790154Z"}}
head(df3)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:31:16.428053Z","iopub.execute_input":"2023-10-22T06:31:16.429950Z","iopub.status.idle":"2023-10-22T06:31:16.458288Z"}}
df3$status <- factor(df3$status, levels = c("Nasabah baru","Nasabah lama"), labels = c(0,1))
df3$decision <- factor(df3$decision, levels = c("Reject","Approve"), labels = c(0,1))
str(df3)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:48:56.817978Z","iopub.execute_input":"2023-10-22T06:48:56.825307Z","iopub.status.idle":"2023-10-22T06:48:56.901650Z"}}
head(df3)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:54:39.042457Z","iopub.execute_input":"2023-10-22T06:54:39.045022Z","iopub.status.idle":"2023-10-22T06:54:39.068281Z"}}
df4 <- df3
df4$home_owner <- as.numeric(df4$home_owner)
df4$status <- as.numeric(df4$status)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:54:41.362824Z","iopub.execute_input":"2023-10-22T06:54:41.365742Z","iopub.status.idle":"2023-10-22T06:54:41.389667Z"}}
unique(df4$home_owner)
unique(df4$status)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:54:48.054433Z","iopub.execute_input":"2023-10-22T06:54:48.056303Z","iopub.status.idle":"2023-10-22T06:54:48.091273Z"}}
head(df4)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:55:03.761543Z","iopub.execute_input":"2023-10-22T06:55:03.763251Z","iopub.status.idle":"2023-10-22T06:55:03.795914Z"}}
str(df4)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:55:25.296539Z","iopub.execute_input":"2023-10-22T06:55:25.300794Z","iopub.status.idle":"2023-10-22T06:55:25.331731Z"}}
glm(decision~., family="binomial", data=df4)

# %% [markdown]
# ## Training Data - Library Caret

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:40:26.848115Z","iopub.execute_input":"2023-10-22T06:40:26.851934Z","iopub.status.idle":"2023-10-22T06:40:28.505786Z"}}
library(lattice)
library(caret)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:55:44.652719Z","iopub.execute_input":"2023-10-22T06:55:44.654626Z","iopub.status.idle":"2023-10-22T06:55:44.670324Z"}}
'%in%' <- Negate('%in%')
options(scipen=999) 

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:55:52.804205Z","iopub.execute_input":"2023-10-22T06:55:52.806559Z","iopub.status.idle":"2023-10-22T06:55:52.833367Z"}}
# Prep Training and Test data. 
set.seed(100) 
trainDataIndex <- createDataPartition(df4$decision, p=0.7, list = F)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:56:01.525611Z","iopub.execute_input":"2023-10-22T06:56:01.533612Z","iopub.status.idle":"2023-10-22T06:56:01.584506Z"}}
#70% Training Data
trainData <- df4[trainDataIndex, ]
testData <- df4[-trainDataIndex, ]

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:56:11.175787Z","iopub.execute_input":"2023-10-22T06:56:11.177704Z","iopub.status.idle":"2023-10-22T06:56:11.203032Z"}}
# Down Sample 
set.seed(100) 
down_train <- downSample(x = trainData[, colnames(trainData) %in% "decison"], y = trainData$decision) 
table(down_train$decision)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:56:19.319308Z","iopub.execute_input":"2023-10-22T06:56:19.321131Z","iopub.status.idle":"2023-10-22T06:56:19.346682Z"}}
str(trainData)

# %% [markdown]
# # Membuat Logit Model

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:56:34.299766Z","iopub.execute_input":"2023-10-22T06:56:34.302074Z","iopub.status.idle":"2023-10-22T06:56:34.342697Z"}}
logitmod <- glm(decision~., family="binomial", data=trainData)
summary(logitmod)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:57:17.635198Z","iopub.execute_input":"2023-10-22T06:57:17.641647Z","iopub.status.idle":"2023-10-22T06:57:17.684838Z"}}
pred <- predict(logitmod, testData, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0) 
y_pred <- factor(y_pred_num, levels=c(0, 1)) 
y_act <- testData$decision 

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:57:31.226072Z","iopub.execute_input":"2023-10-22T06:57:31.228266Z","iopub.status.idle":"2023-10-22T06:57:31.249538Z"}}
mean(y_pred == y_act)

# %% [code] {"execution":{"iopub.status.busy":"2023-10-22T06:57:42.138139Z","iopub.execute_input":"2023-10-22T06:57:42.141901Z","iopub.status.idle":"2023-10-22T06:57:42.186769Z"}}
str(testData)