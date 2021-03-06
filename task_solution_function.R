"""
����� ������� decorate_string ��������� ������ ������� paste, ������������� ���������� � ���������� �������� pattern. ��� ���� ���� �������� ������ ���� ����������� ��� � ������ ������ (�����), ��� � � �����, �� ����������� ����� ������.
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
cat_temper <- c("����������", "�������", "���������", "�������")
cat_color <- c("�����", "�����", "������", "�����")
cat_age <- c("���", "������")
cat_trait <- c("� ������ �������", "� ������� �������", "� �������� �����")
��������� ������ cat_catalogue, ���������� ������������ ���������� ��������� �������������, � ������������ ���.
"""
cat_catalogue <- sort(outer(outer(outer(cat_temper, cat_color, paste), cat_age, paste), cat_trait, paste))
cat_catalogue[42]


"""
��������� �� ���������
"""
# Random walk with absorption
simulate_walk <- function(lower = -6, upper = 6, n_max = 100, p = 0.01) {
  current_position_x <- (lower + upper) / 2
  current_position_y <- (lower + upper) / 2
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(list(status = "Absorbed", 
                                 position_x = current_position_x,
                                 position_y = current_position_y,
                                 steps = i))
    current_position_x <- current_position_x + rnorm(1)
    current_position_y <- current_position_y + rnorm(1)
    distance <- sqrt(current_position_x^2 + current_position_y^2)
    if (distance > 6) return(list(status = "Distance breach", 
                                  position_x = current_position_x,
                                  position_y = current_position_y,
                                  steps = i))
  }
  return(list(status = "Max steps reached", 
              position_x = current_position_x,
              position_y = current_position_y,
              steps = n_max))
}

# Simulate results
result <- replicate(100000, simulate_walk(), simplify = FALSE)
result <- data.frame(
  status = sapply(result, function(x) x$status),
  position_x = sapply(result, function(x) x$position_x),
  position_y = sapply(result, function(x) x$position_y),
  steps = sapply(result, function(x) x$steps)
)

# Inspect results
r <- tapply(result$position_x, result$status, length)
r
r_mean <- r[2] / 100000 * 100
r_mean

"""
������� ���������� �������� �� ������ �������
"""
avian <- read.csv("C:/Dev/R_lessons/avianHabitat.csv")
avian$Site <- stri_replace(avian$Site, regex = "[:digit:]+", "")
avian <- subset(aggregate(cbind(HHt = avian$HHt, DBHt = avian$DBHt, WHt = avian$WHt,
                                EHt = avian$EHt, AHt = avian$AHt, LHt = avian$LHt),
                          list(Site = avian$Site, Observer = avian$Observer),
                          function(c) sum(c > 0)))
avian
