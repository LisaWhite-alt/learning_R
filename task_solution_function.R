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
