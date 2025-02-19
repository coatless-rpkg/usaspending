# Set a custom redactor to shorten the API URL
httptest2::set_redactor(function(response) {
  # First apply the standard header redaction
  response <- httptest2::redact_headers(response)
  
  # Then shorten the URL path
  httptest2::gsub_response(
    response,
    "https\\://api\\.usaspending\\.gov/api/v2/", 
    "usasp/"
  )
})