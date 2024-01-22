/**
  * Common Regexes for Regex Parsers
  */
trait CommonRegexes {

  def textBeforeDate = "^(.(?!(\\d{1,2}\\/\\d{1,2}\\/\\d{2,4})))+".r

  def space = "\\s+".r

  def rawNumber = "[0-9]{2}".r

  def namePattern = "^[A-Za-z]+\\b".r

  def nameWithoutBoundaryPattern = "^[A-Za-z]+".r

  def nameWithSpacesPattern = "^[A-Za-z ]+".r

  def nameAllCapsWithSpacesPattern = "^[A-Z ]+".r

  def dateTextStartBracket = "\\(".r

  def singleDate = "\\d{2}\\/\\d{2}\\/\\d{4}\\b".r

  def dateTextEndBracket = "\\)".r

  def amountValue = "\\$\\d{1,9}?(,)?\\d{0,9}\\.\\d{2}\\b".r

  def amountValueWithoutDecimal = "\\$\\d{1,9}?(,)?\\d{0,9}\\b".r

  def amountValueWithoutCurrency = "\\d{1,9}?(,)?\\d{0,9}\\.\\d{2}\\b".r

  def textBeforeAmountWithoutCurrency = "^(.(?!(\\d{1,9}?(,)?\\d{0,9}\\.\\d{2})))+".r

  def nameTextPattern = "[A-Z]+\\b".r

  def dobText = "D.O.B.".r

  def alphabeticalText = "[A-Za-z ]+\\b".r

  def textWithSpaces = "^([a-zA-Z\\s]+)".r

  def percent = "(((100)|(0*\\d{1,2}))%)".r


}
