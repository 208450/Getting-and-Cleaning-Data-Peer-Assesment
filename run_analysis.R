
# 1. une os dados
nrows <- 100
train_path <- paste("UCI HAR Dataset", "train",  "X_train.txt", sep="/")
test_path  <- paste("UCI HAR Dataset", "test", "X_test.txt", sep="/")

# carregando dados de treinamento e teste 

train_read <- read.table(train_path, nrows=nrows, header = FALSE, dec = ".", fill = TRUE, comment.char = "")
train_class <- sapply(train_read, class)
rm(train_read)

train <- read.table(train_path, colClasses =train_class, header = FALSE, dec = ".", comment.char = "")


test_read <- read.table(test_path, nrows=nrows, header = FALSE, dec = ".", fill = TRUE, comment.char = "")
test_class <- sapply(test_read, class)
rm(test_read)

test <- read.table(test_path, colClasses = test_class, header = FALSE, dec = ".", fill = TRUE, comment.char = "")

#carregando recursos 

recurso_path  <- paste("UCI HAR Dataset", "features.txt",  sep="/")
atividade_path  <- paste("UCI HAR Dataset", "activity_labels.txt",  sep="/")

# carregando recursos e atividades para ter nomes
# -------------------------------------------------------
# variaveis de nome
assunto_train_path <- paste("UCI HAR Dataset", "train","subject_train.txt", sep="/")
assunto_test_path  <- paste("UCI HAR Dataset", "test", "subject_test.txt",   sep="/")

y_train  <- paste("UCI HAR Dataset", "train", "y_train.txt", sep="/")
y_test  <- paste("UCI HAR Dataset", "test", "y_test.txt", sep="/")


recursos <- read.table(recurso_path, colClasses = c("numeric","character"), col.names = c("Variable.id","Variable.Name"), header = FALSE, comment.char = "")

assunto_train <- read.table(assunto_train_path, colClasses = c("numeric"), col.names = c("Subject.id"), header = FALSE, comment.char = "")
atividade_train <- read.table(y_train, colClasses = c("numeric"), col.names = c("Activity id"), header = FALSE, comment.char = "")

assunto_test <- read.table(assunto_test_path, colClasses = c("numeric"), col.names = c("Subject.id"), header = FALSE, comment.char = "")
atividade_test <- read.table(y_test, colClasses = c("numeric"), col.names = c("Activity.id"), header = FALSE, comment.char = "")

#merge parcial - adiciona o assunto e entao a atividade 
teste <- cbind(assunto_test, atividade_test,test)
treino <- cbind(assunto_train, atividade_train,train)

#merge total
merge_all_data <- rbind(teste,treino)

#nome colunas
names(merge_all_data) <- c("Subject.id","Activity.id",recursos[,2])

#ordena dados
merge_all_data  <- merge_all_data [order(merge_all_data$Subject.id, merge_all_data$Activity.id),]

# 2. Extrai medidas das médias e desvio padrão

for (i in seq_along(recursos$Variable.Name)) {
  if (!grepl("mean",recursos$Variable.Name[i]) & !grepl("std",recursos$Variable.Name[i])) {
       merge_all_data[,recursos$Variable.Name[i]] <- NULL
  }
}

# 3. Usa nomes de atividade descritivos para citar as atividades no conjunto de dados

atividade <- read.table(atividade_path, colClasses = c("numeric","character"), col.names = c("Activity.id","Activity.Name"), header = FALSE, comment.char = "")

library(plyr)
merge_all_data <- join(merge_all_data, atividade, by = "Activity.id")
merge_all_data$Activity.id <- NULL


# 4. Apropriadamente rotula o conjunto de dados com nomes de variáveis descritivas.
rotulo <- names(merge_all_data)
rotulo <- rotulo[complete.cases(rotulo)]

#limpa o nome das variaveis
for (i in seq_along(rotulo)) {
  rotulo[i] <- gsub("mean","Mean",rotulo[i])
  rotulo[i] <- gsub("std","Std",rotulo[i])
  rotulo[i] <- gsub("\\()","",rotulo[i]) 
  rotulo[i] <- gsub("-","",rotulo[i])
}

names(merge_all_data) <- rotulo

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
final_data <- ddply(merge_all_data, .(Subject.id, Activity.Name), numcolwise(mean))


#cleaning variable names
rotulo  <- names(final_data)
for (i in 3:length(rotulo)) {
  
  rotulo[i] <- paste0("Mean.",rotulo[i])
}
names(final_data) <- rotulo

#cria arquivo com os dados formatados 
write.table(final_data,"file_final_data.txt",row.name=FALSE)

