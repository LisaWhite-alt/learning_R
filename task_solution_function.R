"""
Пусть функция decorate_string действует поверх функции paste, дополнительно приклеивая к результату аргумент pattern. При этом этот аргумент должен быть присоединён как в начале строки (строк), так и в конце, но перевёрнутый задом наперёд.
"""
decorate_string <- function(pattern, ...) { 
  paste(pattern, paste(...), intToUtf8(rev(utf8ToInt(pattern))), sep = "")
}

decorate_string(pattern = "123", "abc")
decorate_string(pattern = "123", "abc", "def")
decorate_string(pattern = "123", c("abc", "def"))
decorate_string(pattern = "123", "abc", "def", sep = "+")
decorate_string(pattern = "!", c("x", "x"), collapse = "_")
decorate_string(pattern = ".:", 1:2, 3:4, 5:6, sep = "&")

"""
cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")
Составьте вектор cat_catalogue, содержащий всевозможные комбинации имеющихся характеристик, и отсортируйте его.
"""
cat_catalogue <- sort(outer(outer(outer(cat_temper, cat_color, paste), cat_age, paste), cat_trait, paste))
cat_catalogue[42]
