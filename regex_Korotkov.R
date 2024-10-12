# Загрузите данные о землетрясениях
anss <- readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/earthquakes_2011.html", warn=FALSE)

# Выберите строки, которые содержат данные с помощью регулярных выражений и функции grep
# Ищем строки, которые начинаются с даты в формате ГГГГ/ММ/ДД
pattern <- "^\\d{4}/\\d{2}/\\d{2}"
data_lines <- grep(pattern, anss, value = TRUE)

# Напечатаем первые и последние строки для проверки
cat("\nExtracted lines with valid date pattern (HEAD): \n")
print(head(data_lines))
cat("\nExtracted lines with valid date pattern (TAIL): \n")
print(tail(data_lines))

# Проверим, что все строки в результирующем векторе подходят под шаблон
all_match <- all(grepl(pattern, data_lines))
cat(paste("\n All lines match the pattern:", all_match))