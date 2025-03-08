check_column_names <- function(file_path) {
    # 读取数据
    fileformat <- strsplit(file_path, "\\.")[[1]][-1]
    if (fileformat == "csv") {
        column_names <- unlist(read.csv(file_path, nrows = 1, header = F))
    }else if (fileformat == "xlsx") {
        column_names <- openxlsx::read.xlsx(file_path, colNames = F, rowNames = T)
        column_names <- unlist(column_names[1,])
    }
    # 检查每个列名
    valid <- TRUE
    for (name in column_names) {
    # 检查列名是否以数字开头
    if (grepl("^[0-9]", name)) {
        valid <- FALSE
        cat(sprintf("列名 '%s' 以数字开头\n", name))
    }
    # 检查列名是否只包含允许的标点符号（_ 和 .）
    if (grepl("[^a-zA-Z0-9_\\.]", name)) {
        valid <- FALSE
        cat(sprintf("列名 '%s' 包含不允许的标点符号\n", name))
    }
    }
    if (valid) {
    cat("所有列名都符合要求\n")
    }
    return(valid)
}