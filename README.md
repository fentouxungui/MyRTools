# MyRTools

Frequently used R functions in my work | 工作上经常用的小工具

## Install

```r
if(!require("MyRTools")){
  library("devtools")
  devtools::install_github("fentouxungui/MyRTools")
}
library(MyRTools)
```

## 批量修改文件

以readline模式批量读取文件，对符合模式的行内容进行文字替换！

比较常用的一个功能，比如说用shiny搭建的多个应用，当某些包升级了，我们需要批量修改多个app.R中的文件内容，那么这个小工具就非常方便！

### 批量修改文件

```r
BatchModifyFileExtended("./man/",
                         FileNameKeyWords = c("a","m"),
                         FileNamepattern = NULL,
                         FileName.ignore.case = TRUE,
                         KeyWordOldLocation = c("a","b"),
                         Location.ignore.case = FALSE,
                         keyWordOld = "roxygen2",
                         keyWordNew = "####",
                         Replace = FALSE,
                         SaveOld = TRUE)
```

### 列出所有包含关键字的文件 - 返回路径+文件名的向量

```r
FindFiles(Dir,
          FileNameKeyWords,
          ignore.case,
          pattern)
```
                     
