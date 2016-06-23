<%
  java.util.Map expected = new java.util.HashMap();
  /*
<form method="post" action="hi.iq/register.jsp" enctype="multipart/form-data">
  Name:  <input type="text" name="name" value="J.Doe">
  email: <input type="text" name="email" value="abuse@spamcop.com">
  file: <input type="file" name="file-upload">
  <input type="submit">
</form>
*/

  expected.put("name", "J.Doe");
  expected.put("email", "abuse@spamcop.com");
  expected.put("file-upload", "This is the test file");
  com.myjavatools.web.ServerRequest myRequest =
    new com.myjavatools.web.ServerRequest(request);
  if (myRequest.getParameterNames().hasMoreElements()) {
    boolean success = true;
    for (java.util.Iterator i = expected.entrySet().iterator();
         i.hasNext();) {
      java.util.Map.Entry entry = (java.util.Map.Entry)i.next();
      String key   = (String)entry.getKey();
      String value = (String)entry.getValue();
      String actual= myRequest.getParameter(key);
      if (!value.equals(actual)) {
        %>Error: The value of <%=key%> is "<%=actual%>", must be "<%=value%>".<%
        success = false;
      }
    }
    if (success) {%>Success<%}
  } else {%>
<html>
  <head>
    <title>
      test 3
    </title>
  </head>
  <body bgcolor="#ffffff">
    <h1>Test 3. Multipart Post With File</h1>
    <form method="Post">
<%  for (java.util.Iterator i = expected.entrySet().iterator();
         i.hasNext();) {
      java.util.Map.Entry entry = (java.util.Map.Entry)i.next();
      String key   = (String)entry.getKey();
      String value = (String)entry.getValue();
      %><%=key%>: <input name="<%=key%>" <%
      if (key.startsWith("file")) {%>
        type="file"/>
        Please create a file containing "<%=value%>" and upload it here<%
      } else {%>
        type="text"
        value="<%=value%>"/><%
      }%>
    <br /><%
    }
%>
      <input type="submit">
    </form>
  </body>
</html>

<%}%>
