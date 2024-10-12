# Урал (Домашние матчи)
ural_home <- c(2, 0, 1, 0)

# Выездные матчи
ural_away <- c(0, 0, 1, 1)

# Напечатайте на консоль оба вектора
print(ural_home)
print(ural_away)

# Назначим имена элементам вектора (Команды Гости)
names(ural_home) <- c("Ufa", "CSKA", "Arsenal", "Anzhi")

# Проделайте то же самое для вектора ural_away, назначив имена команд гостей (away_names)
away_names <- c("Rostov", "Amkar", "Rubin", "Orenburg")
names(ural_away) <- away_names

# Напечатайте на консоль оба вектора, заметьте разницу
print(ural_home)
print(ural_away)

# Посчитайте статистику домашних и выездных матчей (общее кол-во голов, среднее количество голов)
total_home_goals <- sum(ural_home)
total_away_goals <- sum(ural_away)

mean_home_goals <- mean(ural_home)
mean_away_goals <- mean(ural_away)

print(paste("Total home goals:", total_home_goals))
print(paste("Total away goals:", total_away_goals))
print(paste("Average home goals:", mean_home_goals))
print(paste("Average away goals:", mean_away_goals))

# Сравните векторы ural_home и ural_away и сделайте вывод
comparison <- ural_home > ural_away
print(comparison)

# Вывод: В домашних матчах "Урал" забил в общей сложности больше голов (3 гола), чем в выездных (2 гола). 
# Среднее количество голов за матч дома (0.75) также выше, чем в выездных матчах (0.5). 
# В домашних играх "Урал" был результативнее против команды "Ufa", но в остальных матчах результативность была одинаково низкой.
# Выездные матчи показали лучшие результаты в играх с "Rubin" и "Orenburg", где были забиты голы, в то время как против "Rostov" и "Amkar" голов не было.


