tab_voronoys <- function(texto, texto2, texto3, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "width:80%;height: 130px;background-color:', cor, ';"> 
                  <span class = "name" style = "font-size:20px; color:black">', texto, '</span>
                  <span class = "name2" style = "color:black">', texto2, '</span>
                  <span class = "name3" style = "color:black">', texto3, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">',
                      icon,
                    '</div>
                  </div>
              </div></a>'))
}

myactionButton <- function(id, label , bcol, hbcol, bordercol, width, borderradius) {
  tags$div(
    id = id,
    tags$style(
      HTML(
        sprintf(paste0("#%s button { background-color: ",bcol,";width: ",width,"%;","border-color:",bordercol,";","border-radius:",borderradius,";","}",
                        "#%s button:hover { background-color: ",hbcol,";width: ",width,"%;","border-color:",bordercol,";","border-radius:",borderradius,";","}"),
          id, id
        )
      )
    ),
    tags$button(
      type = "button",
      class = "btn btn-primary",
      id = paste0(id, "-button"),
      label
    )
  )
}

check_format_1 <- function(element) {
  # 按逗号分割元素
  parts <- strsplit(element, ",")[[1]]
  # 检查长度是否为4
  if (length(parts) != 4) {
      return(FALSE)
  }
  # 检查前三个部分是否为数值，第四个部分是否为字符串
  if (all(sapply(parts[1:3], is.numeric)) && is.character(parts[4])) {
      return(TRUE)
  } else {
      return(FALSE)
  }
}

check_format_2 <- function(element) {
  # 按逗号分割元素
  parts <- strsplit(element, ",")[[1]]
  # 检查长度是否为5
  if (length(parts) != 5) {
      return(FALSE)
  }
  # 检查前四个部分是否为数值，第五个部分是否为字符串
  if (all(sapply(parts[1:4], is.numeric)) && is.character(parts[5])) {
      return(TRUE)
  } else {
      return(FALSE)
  }
}

is.numeric <- function(x) {
  !is.na(as.numeric(x))
}

format_to_datafarme <- function(element){
    input_string <- element
    # 第一步：按分号分割字符串
    split_by_semicolon <- strsplit(input_string, ";")[[1]]
    # 第二步：对每个元素按逗号分割，并转换为矩阵
    split_by_comma <- lapply(split_by_semicolon, function(x) strsplit(x, ",")[[1]])
    # 将列表转换为矩阵
    result_matrix <- do.call(rbind, split_by_comma)
    result_matrix <- as.data.frame(result_matrix)
    return(result_matrix)
}

update_colnames <- function(df) {
  # 获取新的列名
  new_colnames <- sapply(df, function(col) {
    if (startsWith(as.character(col[1]), "#")) {
      return(substring(col[1], 2))  # 去掉开头的 #
    } else {
      return(NULL)
    }
  })
  
  # 替换列名
  colnames(df) <- ifelse(!is.null(new_colnames), new_colnames, colnames(df))
  
  # 去掉第一行
  df <- df[-1, ]
  
  # 返回修改后的数据框
  return(df)
}