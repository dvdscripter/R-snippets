#' Try to follow KISS ph.
#' 
#' Remove all accents and turn characters to lower case.
#' 
#' @param x character vector to be sanitized
#' @return return the input character vector sanitized
#' @export
sanitize = function(x) {
	letras_com_acentos = 'ÀÁÂÃÄÅàáâãäåÒÓÔÕÕÖØòóôõöøÈÉÊËèéêëðÇçÐÌÍÎÏìíîïÙÚÛÜùúûüÑñŠšŸÿýŽž'
	letras_sem_acentos = 'AAAAAAaaaaaaOOOOOOOooooooEEEEeeeeeCcDIIIIiiiiUUUUuuuuNnSsYyyZz'
	x = as.character(x)
	x = gsub('[\'`]', '', x)
	tolower( chartr(letras_com_acentos, letras_sem_acentos, x) )
}