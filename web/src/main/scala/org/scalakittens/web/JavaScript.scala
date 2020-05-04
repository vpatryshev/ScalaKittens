package org.scalakittens.web

import org.scalakittens.Library.{PropMap, readResource}

/**
 * Common JavaScript stuff for parsers
 * Store here the things that are common
 */
object JavaScript {
  val library: String = readResource("/Tools.js").getOrElse(throw new IllegalStateException("Failed to read javascript library /Tools.js; please check the build")).toString

  val foundElement = "window['HEXelem']"
  private def find(element: String, selector: String) = s"findField(${element.js}, ${selector.js})"

  private def jsFindAndKeep(element: String, selector: String) = s"$foundElement=${find(element, selector)}"

  val latestFoundText = s"return textOf($foundElement)"

  val latestFoundHtml = s"$foundElement.innerHTML"

  val latestFoundOuterHtml = s"$foundElement.outerHTML"

  def jsGetContent(element: String, selector: String) = s"${jsFindAndKeep(element, selector)}; $latestFoundText"

  def jsGetContent(selector: String) = s"${jsFindAndKeep("body", selector)}; $latestFoundText"

  def jsGetContentAll(selector: String) = s"return contentOfAll(${selector.js})"

  def jsClickLastFoundLink = s"return $foundElement ? clickLink($foundElement) : 'OK'"

  def jsClickLastFound = s"return clickOn($foundElement)"

  def jsFoundTable() = s"try { return extractTableContents($foundElement) } catch (err) { return [err.toString()] }"

  def jsGetTable(selector: String) = s"try { return extractTable(${selector.js}) } catch (err) { return [null, err.toString()] }"

  def jsGetTableAsArray(selector: String) = s"try { return extractArrayFromTable(${selector.js}) } catch (err) { return [err.toString()] }"

  def jsExtractProps(selector: String, fields: PropMap) =
      s"""
      response = []
      function addRow(name, selector) {
        var item = element.querySelector(selector)
        if (item) {
          response.push(name)
          response.push(textOf(item))
        }
      }
      var errors = "no errors"
      selector = ${selector.js}
      element = document.querySelector(selector)
      if (!element) return ("Table not found: " + selector)
      """ + fields.map(kv ⇒ s"addRow('${kv._1}', ${kv._2.js})").mkString("\n") + """
      return response
      """

  def jsExtractSimpleTable(selector: String, from: Int) =
    s"return extractContentOfSimpleTable(${selector.js}, $from)"

  def jsExtractTableFields(fields: PropMap, expectedRowSize:Int = 0) = {
    def minRowSize = if (expectedRowSize <= 0) fields.size else expectedRowSize
    def oneRow(kv: (String, String)) = s"[${kv._1.js}, '' + ${kv._2}]"
    s"""
      tables = $foundElement
      if (!tables || (tables.length == 0)) return ("Table not found.")
      extractRow = function(row) { return [${fields .map (oneRow) .mkString(",")}] }

      errors = ""
      response = []
      for (var i = 0; i < tables.length; i++) {
        table = tables[i];
        rows = table.rows;
        for (var j = 1; j < rows.length; j++) {
          try {
            row = rows[j].cells
            if (row.length >= $minRowSize) {
              responseRow = extractRow(row)
              response.push(responseRow)
            }
          } catch (err) {
            errors += "Table#" + i +" Row#" + j + ": " + err + "; "
          }
        }
      }
      return response.length == 0 ? errors : response
      """
  }

  def jsFillFormField(containerSelector:Any, fieldSelector:Any, value:Any) =
    """
         sSel = """ + (""+containerSelector).js + """
         sField = """ + (""+fieldSelector).js + """
         sVal = """ + (""+value).js + """
         elem = document.querySelector(sSel)
         if (!elem) return 'Selector not found: '+sSel

         field = elem.querySelector(sField)
         if (!field) return 'Form field <<' + sField + '>> not found'

         try {
           field.focus()
         } catch (e) {
           return "Unable to focus() input field " + field.getAttribute('name') + ": " + e
         }
         field.value = sVal
         try { field.onChange() } catch (e) { /* ignore */}
         try {
           field.blur()
         } catch (e) {
           return "Unable to blur() input field " + field.getAttribute('name') + ": " + e
         }
       """

}
