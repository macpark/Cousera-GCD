#
r_Data = function (fname, folder) {
    path = file.path(folder, paste0("y_", fname, ".txt"))
    y_data = read.table(path, header=FALSE, col.names = c("ActiviytID"))
    
    path = file.path(folder, paste0("subject_", fname, ".txt"))
    subject_data = read.table(path, header=FALSE, col.names=c("SubjectID"))
    
    data_columns = read.table("features.txt", header=FALSE, as.is=TRUE, col.names=c("MeasureID", "MeasureName"))
    
    path = file.path(folder, paste0("X_", fname, ".txt"))
    dataset = read.table(path, header=FALSE, col.names=data_columns$MeasureName)
    
    subset_data_columns = grep(".*mean\\(\\)|.*std\\(\\)", data_columns$MeasureName)
    
    dataset = dataset[, subset_data_columns]
    
    dataset$ActivityID = y_data$ActivityID
    dataset$SubjectID = subject_data$SubjectID
    
    dataset
}

mergeDataset = function () {
    dataset = rbind(r_Data("test", "test"), r_Data("train", "train"))
    cnames = colnames(dataset)
    cnames = gsub("\\.+mean\\.+", cnames, replacement = "Mean")
    cnames = gsub("\\.+std\\.+", cnames, replacement = "Std")
    colnames(dataset) = cnames
    dataset
}

activityLabels = function (dataset) {
    activity_labels = read.table("activity_labels.txt", header = FALSE, as.is=TRUE, col.names = c("ActivityID", "ActivityName"))
    activity_labels$ActivityName = as.factor(activity_labels$ActivityName)
    data_labels = merge(dataset, activity_labels)
    data_labels
}

merge_label_data = function () {
    activityLabels(mergeDataset())
}

tidyData = function(merge_label_data) {
    library(reshape2)
    
    vars = c("ActivityID", "ActivityName", "SubjectID")
    measure_vars = setdiff(colnames(merge_label_data), vars)
    melted_data <- melt(merge_label_data, id=vars, measure.vars=measure_vars)
    
    dcast(melted_data, ActivityName + SubjectID ~ variable, mean)
}

tidy_data <- tidyData(merge_label_data())
write.table(tidy_data, "tidy.txt")

