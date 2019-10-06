
df = read.table("morse_code.csv")
df <- df[1:26,1]

alphabet<-c()
morse<-c()

for (i in 1:length(df)){
    str_df <- toString(df[[i]])
    str_list <- strsplit(str_df, ',')
    alphabet <- c(alphabet, str_list[[1]][1])
    morse <- c(morse, str_list[[1]][2])
}

alphabet
lower_alphabet<-tolower(alphabet)
print(lower_alphabet)
print(morse)



letters_to_morse <- function(letters){
    lowers<-tolower(letters)
    split <-strsplit(lowers, ' ')
    length<-lengths(split)
    morse_vector <- c()
    for (i in 1:length){
      letter_list <- strsplit(split[[1]][i], '')
      word_morse_code<-c()
      for (j in 1:lengths(letter_list)){
        morse_index <- match(letter_list[[1]][j], lower_alphabet)
        word_morse_code <- c(word_morse_code, morse[morse_index])
      }
      word_morse_vector <- paste(word_morse_code, sep='', collapse = 's')
      #print(word_morse_vector)
      morse_vector<-c(morse_vector, word_morse_vector)
      morse_vector <- paste(morse_vector, sep='', collapse = 'ss')
    }
    print(morse_vector)
}
#

let<-"Chuseok is similar to Thanksgiving"
letters_to_morse(let)



morse_to_letter <- function(letters){
  split <-strsplit(letters, 'ss')
  sentence <- c()
  for (i in 1:lengths(split)){
    morse_letters <- split[[1]][i]
    splitted <- strsplit(morse_letters, 's')
    word <- c()
    for (i in 1:lengths(splitted)){
      
      alphabet_index <- match(splitted[[1]][i], morse)
      word<-c(word, lower_alphabet[alphabet_index])
    }
    sentence <- c( sentence,paste(word, collapse = ""))
  }
  print(paste(sentence, collapse = " "))
}
#

morse_example<-"DDdsDDDsDDDsDddssdDddsddDsDdDdsDdDssDDDsDdssDdDDsDDDsddDsdDdssDDsddsDddsDsdsdDdsDDssdsDddDsdDsDDsddd"
morse_to_letter(morse_example)




