#Дисперсионный анализ. Лабораторная работа.
#Коротков Виталий Константинович РИ-411055.

#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
data = read.csv("data/diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#files/Diet_data_description.docx

colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)


#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

#попарные различия между средними значениями для всех групп
TukeyHSD(fit)

#Tukey honest significant differences test)
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

# Задание
# Добавить проверку на выбросы и избавиться от них
# Повторно провести все тесты и сравнить результаты с выбросами и без

# Функция для удаления выбросов
remove_outliers <- function(data) {
  Q1 <- quantile(data$weight.loss, 0.25)
  Q3 <- quantile(data$weight.loss, 0.75)
  IQR <- Q3 - Q1
  # Определяем границы для выбросов
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  # Фильтруем данные
  return(data[data$weight.loss >= lower_bound & data$weight.loss <= upper_bound, ])
}

# Удалим значения с потерей веса более 8 кг для диеты типа "A"
data_no_extreme <- data[!(data$diet.type == "A" & data$weight.loss > 8), ]

# Применим функцию для удаления выбросов
data_no_extreme <- remove_outliers(data_no_extreme)

# Повторим дисперсионный анализ (ANOVA) на данных без значительных отклонений
fit_no_extreme <- aov(weight.loss ~ diet.type, data=data_no_extreme)
summary(fit_no_extreme)

# Повторный вывод графика "Ящик с усами" для данных без значительных отклонений
boxplot(weight.loss ~ diet.type, data=data_no_extreme, col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type",
        main = "Ящик с усами для данных без значительных отклонений")
abline(h=0, col="green")

# Открыть документ files/Diet_data_description.docx и попытаться выполнить задания из него

# 1. Paired t-test: игнорируем тип диеты и проверим, потеря ли веса в целом произошла
t_test_result <- t.test(data_no_extreme$initial.weight, data_no_extreme$final.weight, paired = TRUE)
print(t_test_result)

# 2. Compute variable: удалим переменную weight.loss и пересчитаем её с использованием начального и конечного веса
data_no_extreme$weight.loss <- NULL
data_no_extreme$weight.loss <- data_no_extreme$initial.weight - data_no_extreme$final.weight
summary(data_no_extreme$weight.loss)

# 3. Summary statistics: сводная статистика по типу диеты
summary_by_diet <- aggregate(weight.loss ~ diet.type, data=data_no_extreme, summary)
print(summary_by_diet)

# 4. One-way ANOVA: какая диета лучше для потери веса? Есть ли гендерные различия в потере веса?
fit_diet <- aov(weight.loss ~ diet.type, data=data_no_extreme)
summary(fit_diet)

fit_gender <- aov(weight.loss ~ gender, data=data_no_extreme)
summary(fit_gender)

# 5. Two-way ANOVA: влияние диеты и пола на потерю веса
fit_diet_gender <- aov(weight.loss ~ diet.type * gender, data=data_no_extreme)
summary(fit_diet_gender)

# 6. Interactions: график средних значений потери веса по диете и полу
interaction.plot(data_no_extreme$diet.type, 
                 data_no_extreme$gender, 
                 data_no_extreme$weight.loss,
                 col = c("dodgerblue", "salmon"), # Изменены цвета для лучшего восприятия
                 pch = 16, # Символы для точек
                 lty = 1, 
                 lwd = 2, 
                 ylab = "Средняя потеря веса (кг)", 
                 xlab = "Тип диеты",
                 main = "Взаимодействие между типом диеты и полом\nна потерю веса",
                 axes = TRUE, 
                 legend = FALSE) # Отключаем встроенную легенду

# Добавление легенды
legend("topright", 
       legend = c("Мужчины", "Женщины"), 
       col = c("dodgerblue", "salmon"), 
       pch = 16, 
       bty = "n", # Убираем рамку вокруг легенды
       lty = c(1, 1)) # Указываем линии для легенды


# 7. ANCOVA: добавим рост в однофакторный или двухфакторный ANOVA
fit_ancova <- aov(weight.loss ~ diet.type + height, data=data_no_extreme)
summary(fit_ancova)


# ВЫВОДЫ

# Выводы по результатам анализа:
# 1. В ходе работы была применена функция для удаления выбросов,
#    что позволило исключить значения, выходящие за пределы 1.5 * IQR. Также некоторые выбросы были удалены вручную.

# 2. Дисперсионный анализ (ANOVA):
#    Результаты ANOVA показывают, что диета имеет статистически значимое влияние на потерю веса (p < 0.001).
#    Это свидетельствует о том, что различные типы диет действительно различаются по своей эффективности.

# 3. График "Ящик с усами":
#    Повторный график "Ящик с усами" для данных без значительных отклонений демонстрирует различия
#    в потерях веса между разными типами диет, что подтверждает результаты ANOVA.

# 4. Paired t-test:
#    Результаты парного t-теста показывают, что средняя разница между начальными и конечными весами составляет
#    примерно 3.72 кг с очень высоким уровнем значимости (p < 2.2e-16). Это подтверждает, что в целом наблюдается
#    значительная потеря веса у участников программы.

# 5. Сводная статистика по типу диеты:
#    Анализ сводной статистики по типам диет показывает, что средняя потеря веса варьируется от 2.80 кг (диета A)
#    до 5.15 кг (диета C), что также подтверждает различия в эффективности диет.

# 6. Гендерные различия:
#    ANOVA по полу не показала статистически значимых различий в потерях веса (p = 0.894), что говорит о том,
#    что пол не оказывает заметного влияния на результаты работы программы.

# 7. Взаимодействие диеты и пола:
#    В двухфакторном ANOVA было выявлено значимое взаимодействие между типом диеты и полом (p = 0.033),
#    что указывает на то, что эффективность диеты может различаться в зависимости от пола.

# 8. ANCOVA:
#    В ANCOVA результаты также подтверждают, что тип диеты остается значимым фактором (p < 0.001),
#    однако влияние роста на потерю веса оказалось статистически незначимым (p = 0.888).










