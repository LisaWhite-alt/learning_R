"""
Дата фрейм attitude -- встроенный массив данных, содержащий рейтинг департаментов одной финансовой компании, составленный сотрудниками.
Представьте, что вы хотите устраиваться как раз в эту компанию, и дата фрейм (совершенно случайно!) оказался в вашем распоряжении. 
Вы решили, что самое главное для вас -- это возможность учиться новому (learning).
Возьмите 5 топовых департаментов по этому показателю. Из этого набора вам более всего подойдёт тот департамент,
который имеет наибольшую сумму баллов по трём показателям: реакция на жалобы работников (complaints),
надбавки в зависимости от результатов работы (raises) и возможность продвижения (advance).
Какой же департамент вам выбрать? Напишите его номер XX (номер строки в дата фрейме).
"""
number_company <- function() {
  df1 <- attitude[order(attitude$learning),]
  df2 <- df1[-(1:(nrow(df1)-5)), ]
  df3 <- df2[c("complaints", "raises", "advance")]
  df4 <- cbind(df3, data.frame("SUM" = rowSums(df3)))
  df5 <- df4[order(df4$"SUM"), ]
  result <- row.names(df5)[5]
  return(result)
}

number_company()


# Работа со встроенным дата фреймом quakes

# Минимальная сила землетрясений по шкале Рихтера:
min(quakes$mag)

# Максимальная сила землетрясений по шкале Рихтера:
max(quakes$mag)

# Средняя глубина землетрясений (км):
mean(quakes$depth)

# Медианная глубина землетрясений (км):
median(quakes$depth)

# Количество станций, зарегистрировавших землетрясение, записанное третьим:
head(quakes)$stations[3]
quakes$stations[3]

# Количество станций, зарегистрировавших землетрясение, записанное предпоследним:
tail(quakes)$stations[5]
quakes$stations[nrow(quakes) - 1]


# Помогите Арчибальду! Cкачайте файл по ссылке, добавьте новые данные в общий дата фрейм и повторите подсчёт общего покрытия, добавив переменную total_coverage. В качестве ответа пришлите величину среднего покрытия с точностью до второго знака: X.XX"

avian <- read.csv("C:/Dev/R_lessons/avianHabitat.csv")
avian2 <- read.csv("C:/Dev/R_lessons/avianHabitat2.csv")
avian3 <- rbind(avian, avian2)
coverage_variables <- names(avian3)[stri_detect(names(avian3), regex="^P")]
avian3$total_coverage <- rowSums(avian3[, coverage_variables])
mean(avian3$total_coverage)


# Растительность Аляски, как мы уже знаем, достаточно скудна. Исследователям интересно, какие выдающиеся экземпляры растений удалось обнаружить? На массиве avianHabitat найдите максимальные высоты по каждому виду растений и отсортируйте эти виды по убыванию, от самого высокого к самому низкому.

height_variables <- names(avian)[-(1:4)][c(F, T)]
avian_sub <- avian[height_variables]
colMax <- function(data) sapply(data, max)
sort(colMax(avian_sub), decreasing = T)
