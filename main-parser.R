# ..............................................................................
#
# Определяем частотность слов в книге на английском языке и составляем случайные
#  словосочетания пригалательное + существительное из самых частых слов
#
# создан: 04.08.2025
# ..............................................................................
#




# ПАКЕТЫ -----------------------------------------------------------------------

library("readtext")
library("dplyr")
library("data.table")
library("wordcloud2")
library("RColorBrewer")




# КОНСТАНТЫ --------------------------------------------------------------------

# количество слов с наибольшей встречаемостью
MAX_WORDS <- 5

# url текста книги для загрузки
#  The Project Gutenberg eBook of The Man Who Was Thursday: A Nightmare
URL_BOOK <- "https://www.gutenberg.org/cache/epub/1695/pg1695.txt"

# url таблицы со словарём
URL_VOCABULARY <- "https://raw.githubusercontent.com/nalgeon/words/refs/heads/main/data/oxford-5k.csv"

# url файла со стоп-словами
URL_STOP <- "https://raw.githubusercontent.com/stopwords-iso/stopwords-en/refs/heads/master/stopwords-en.txt"

# ядро для генератора случайных чисел
my_seed <- 50825




# ФУНКЦИИ ----------------------------------------------------------------------

# объединяем частоты некоторых форм одного и того же слова
eval(parse('./functions/uf_combine_word_forms_frequencies.R', 
           encoding = 'UTF-8'))

# генерируем словосочетание
eval(parse('./functions/uf_random_two_words_by_book.R', 
           encoding = 'UTF-8'))



# ПАРСЕР -----------------------------------------------------------------------



# 1. Загружаем данные из всех источников =======================================


# <<< Текст книги читаем с URL <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
raw_txt <- readtext(URL_BOOK)
# str(raw_txt)
#...............................................................................


# <<< Словарь стоп-слов английского языка читаем с URL <<<<<<<<<<<<<<<<<<<<<<<<<
#
stop_txt <- readLines(URL_STOP)
# str(stop_txt)
#
#...............................................................................


# <<< Словарь Оксфордский читаем с URL <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
DT_voc <- data.table(read.csv(URL_VOCABULARY)[, 1:3])
# str(DT_voc)
#
#...............................................................................


# <<< Вручную допиленная часть словаря под эту книгу из файла <<<<<<<<<<<<<<<<<<
#
df_add <- read.csv2('./reference/additional_voc.csv')
# str(df_add)
#
#...............................................................................


# <<< Таблица с окончаниями для подсчёта словоформ <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
df_endings <- read.csv2('./reference/word_endings.csv')
# str(df_add)
#
#...............................................................................



# 2. Чистим данные =============================================================


# 1. Стартовая очистка, цель - вектор слов с частотами #########################

# Оставляем только текст книги

#  * выкидываем начало:
#    до названия первой главы
search_pattern <- '(.*\n)(CHAPTER I.\nTHE TWO POETS OF SAFFRON PARK\n.*)'
replace_pattern <- '\\2'
clean_txt <- gsub(search_pattern, replace_pattern, raw_txt$text)

#  * выкидываем описалово лицензии проекта Гуттенберга: 
#    после заданной фразы
search_pattern <- '\\\\*\\\\*\\\\* END OF THE PROJECT GUTENBERG EBOOK THE MAN WHO WAS THURSDAY: A NIGHTMARE \\\\*\\\\*\\\\*.*'
replace_pattern <- ''
clean_txt <- gsub(search_pattern, replace_pattern, clean_txt)


# Разбиваем текст построчно
clean_txt <- strsplit(clean_txt, '\n')[[1]]


# Убираем пустые строки
clean_txt <- clean_txt[clean_txt != '']


# Убираем названия глав
chapter_num <- c(grep('CHAPTER ', clean_txt), grep('CHAPTER ', clean_txt) + 1)
clean_txt <- clean_txt[!(1:length(clean_txt) %in% chapter_num)]


# Убираем знаки препинания
clean_txt <- sapply(clean_txt, function(x){
    gsub('[[:punct:]]', '', x)
    }) %>% unname %>% unlist


# Разбиваем на отдельные слова
clean_txt <- strsplit(clean_txt, ' ') %>% unlist %>% unname
tbl <- table(clean_txt)
DT <- data.table(word = names(tbl), freq = as.vector(tbl))


# Убираем пустоты
DT <- DT[!(DT$word == ''), ]


# Убираем числа
DT <- DT[!grepl('\\d', DT$word), ]


# 2. Продвинутая очистка: строчные/прописные буквы в начале слов ###############

# работаем с заглавными буквами
low_reg <- tolower(DT$word)

# эти слова начинаются с большой буквы
capital <- which(!(low_reg == DT$word))

# надо проверить, есть ли варианты написания со строчной
lower_match <- sapply(capital, function(x){
    which(DT$word == tolower(DT$word[x]))
})

# прибавляем к частотам строчных частоты заглавных
DT[lower_match[sapply(lower_match, length) == 1] %>% unlist, 'freq'] <- 
    DT[lower_match[sapply(lower_match, length) == 1] %>% unlist, freq] + 
    DT[capital[sapply(lower_match, length) == 1], freq]

# убираем заглавные
DT <- DT[!capital[sapply(lower_match, length) == 1], ]

# исправляем 'I' на 'i'
DT[DT$word == 'I', 'word'] <- 'i'


# 3. Совмещаем со словарями ####################################################

# убираем стоп-слова
DT <- DT %>% filter(!(DT$word %in% stop_txt))

# сортировка слов по алфавиту критически важна
DT <- arrange(DT, word)

# совмещаем простые словоформы
#  функция запускает цикл по всем вариантам окончаний и обновляет таблицу данных
DT <- uf_combine_word_forms_frequencies(DT, df_endings)

# сортировка по убыванию частот
DT <- arrange(DT, -freq)

# написание строчными буквами как ключ для поиска по словарям
DT$word_key <- tolower(DT$word)


# >>> Рисунок 1 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# рисуем все слова с частотой не менее 10
wordcloud2(data = DT[DT$freq >= 10, ])
#
# ..............................................................................


# совмещаем с оксфордским словарём, чтобы узнать часть речи
words_to_shuffle <- merge(DT, DT_voc[, -2], 
                          by.x = 'word_key', by.y = 'word') 

# добавляем часть словаря, составленную вручную,
#  оставляем существительные и прилагательные с частотой не менее MAX_WORDS,
#  сортируем,
#  добавляем пустой столбец под цвета по градиенту
words_to_shuffle <- rbind(words_to_shuffle, df_add) %>% 
    filter(pos %in% c('noun', 'adjective'), freq >= MAX_WORDS) %>%
    arrange(pos, freq) %>% mutate(clrs = '')

# сколько существительных и прилагательных в итоге осталось
words_to_shuffle %>% count(pos)

# кодируем цвета по градиенту в столбец clrs: 
#  прилагательные синие, существительные красные
bluecols <- brewer.pal(5, 'Blues')
clrs_adj <- colorRampPalette(bluecols)

words_to_shuffle[grepl('adjective', words_to_shuffle$pos), 'clrs'] <- 
    clrs_adj(sum(grepl('adjective', words_to_shuffle$pos)))

redcols <- brewer.pal(5, 'Reds')
clrs_noun <- colorRampPalette(redcols)

words_to_shuffle[grepl('noun', words_to_shuffle$pos), 'clrs'] <- 
    clrs_noun(sum(grepl('noun', words_to_shuffle$pos)))

# сортировка по алфавиту (по частотам облако выходит странное),
#  плюс столбец с индексом для случайной выборки
words_to_shuffle <- words_to_shuffle[, -1] %>% arrange(word) %>% 
    mutate(id = 1:dim(words_to_shuffle)[1])


# >>> Рисунок 2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
wordcloud2(data = words_to_shuffle, 
           color = words_to_shuffle$clrs)
#
# ..............................................................................


# 4. Сохраняем итоговую таблицу   

write.csv2(words_to_shuffle[, 1:3], file = './results/THE MAN WHO WAS THURSDAY.csv',
           row.names = F)



# 5. Наконец составляем собственно случайное словосочетание ####################

uf_random_two_words_by_book(words_to_shuffle, my_seed)

# ядро генератора случайных чисел
rnd_seed <- ceiling(rnorm(m = as.numeric(format(Sys.time(), "%S")), 
                          s = as.numeric(format(Sys.time(), "%M")), 
                          1:10) * 10)

data.table(seed = rnd_seed, 
           two_words = sapply(rnd_seed, function(x){ 
               uf_random_two_words_by_book(words_to_shuffle, x)
               })) %>% unique %>% arrange(two_words)

rnd_seed
# [1] 400 623 737 480 554 410 518 115 649 941
