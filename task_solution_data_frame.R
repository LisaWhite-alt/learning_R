"""
���� ����� attitude -- ���������� ������ ������, ���������� ������� ������������� ����� ���������� ��������, ������������ ������������.
�����������, ��� �� ������ ������������ ��� ��� � ��� ��������, � ���� ����� (���������� ��������!) �������� � ����� ������������. 
�� ������, ��� ����� ������� ��� ��� -- ��� ����������� ������� ������ (learning).
�������� 5 ������� ������������� �� ����� ����������. �� ����� ������ ��� ����� ����� ������� ��� �����������,
������� ����� ���������� ����� ������ �� ��� �����������: ������� �� ������ ���������� (complaints),
�������� � ����������� �� ����������� ������ (raises) � ����������� ����������� (advance).
����� �� ����������� ��� �������? �������� ��� ����� XX (����� ������ � ���� ������).
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


# ������ �� ���������� ���� ������� quakes

# ����������� ���� ������������� �� ����� �������:
min(quakes$mag)

# ������������ ���� ������������� �� ����� �������:
max(quakes$mag)

# ������� ������� ������������� (��):
mean(quakes$depth)

# ��������� ������� ������������� (��):
median(quakes$depth)

# ���������� �������, ������������������ �������������, ���������� �������:
head(quakes)$stations[3]
quakes$stations[3]

# ���������� �������, ������������������ �������������, ���������� �������������:
tail(quakes)$stations[5]
quakes$stations[nrow(quakes) - 1]


# �������� ����������! C������� ���� �� ������, �������� ����� ������ � ����� ���� ����� � ��������� ������� ������ ��������, ������� ���������� total_coverage. � �������� ������ �������� �������� �������� �������� � ��������� �� ������� �����: X.XX"

avian <- read.csv("C:/Dev/R_lessons/avianHabitat.csv")
avian2 <- read.csv("C:/Dev/R_lessons/avianHabitat2.csv")
avian3 <- rbind(avian, avian2)
coverage_variables <- names(avian3)[stri_detect(names(avian3), regex="^P")]
avian3$total_coverage <- rowSums(avian3[, coverage_variables])
mean(avian3$total_coverage)


# �������������� ������, ��� �� ��� �����, ���������� ������. �������������� ���������, ����� ���������� ���������� �������� ������� ����������? �� ������� avianHabitat ������� ������������ ������ �� ������� ���� �������� � ������������ ��� ���� �� ��������, �� ������ �������� � ������ �������.

height_variables <- names(avian)[-(1:4)][c(F, T)]
avian_sub <- avian[height_variables]
colMax <- function(data) sapply(data, max)
sort(colMax(avian_sub), decreasing = T)
