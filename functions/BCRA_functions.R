

bcra_api <- function(path){
  url = modify_url(url = "https://api.estadisticasbcra.com", path = path)
  GET(url, add_headers(Authorization = "bearer eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2MzAwNzcxOTEsInR5cGUiOiJleHRlcm5hbCIsInVzZXIiOiJzdGVmYW5vYmVsdHJhbWUxMkBnbWFpbC5jb20ifQ.RmpQ--4wQ3YDo5LnbAQKx7cg9yeEH1PFwcJC5pWgbexUykrRotXH58Hu_OSPGQfZnszJrjyMnkav5c3Nqi3hGg")
  )
}