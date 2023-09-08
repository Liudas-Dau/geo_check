#' geo_check
#'
#' The geo_check function checks consistency of age data available in a provided
#' data frame, providing options to convert character interval ages to numeric
#' and vice verse.
#'
#' @param df
#' @param age_column
#' @param time
#' @param res
#'
#' @note Note that function assumes that commas in numerical ages seperate thousand.
time <- "Cenozoic"
res <- "Interval"

df1 <- as.data.frame(df)
age_column <- "Age"
df1$numeric_age <- as.numeric(ifelse(grepl("\\d", df$Age),df$Age, NaN ))
df1$interval_age <- ifelse(!grepl("\\d", df$Age),df$Age, NA )

df1$interval_age <- gsub("[[:punct:]]", " " , df1$interval_age)
df1$interval_age <- gsub("^[[:space:]]|[[:space:]]$", "" , df1$interval_age)

l_w <- strsplit(df1$interval_age, "\\s+")

df1$interval_age <- unlist(lapply(l_w, FUN = function(x) {
  f_letters <- toupper(substr(x, 1, 1))
  ifelse(any(is.na(f_letters)) & length(f_letters) == 1, NA,
         paste0(f_letters, "",tolower(substr(x, 2, nchar(x))), collapse = " "))
}))
int_names <- data.frame(interval_age = c(NA, rep(unique(unlist(geo_key[,1:5])),2)))
for (i in 1:nrow(df1)){
  if (!is.na(df1$interval_age[i])){
    int_names$interval_age[1] <- df1$interval_age[i]
    rez <- tax_check(int_names, name = "interval_age", dis = 0.1)
    if (any(rez$synonyms$count_lesser == 1)){
      id <- which(rez$synonyms$count_lesser == 1)
      df1$interval_age[i] <- rez$synonyms$greater[id]
    }
  }
}
time

col_id <- which(apply(geo_key,2, function(x) time %in% x ))

range_id <- range(which(geo_key[,col_id] == time))

geo_key[range_id,c(7)]
df1$numeric_age
geo_key



