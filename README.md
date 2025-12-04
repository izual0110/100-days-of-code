# 100-days-of-code

> clj -X:repl

##### for windows
> docker build -t clj .
>
> docker run -p 5000:5000 clj

##### or
> docker-compose up -d --build
>
> docker-compose down


##### warn-on-reflection
> (set! *warn-on-reflection* true)
>

##### for windows
> Invoke-Expression (New-Object System.Net.WebClient).DownloadString('https://github.com/clojure/brew-install/releases/latest/download/win-install.ps1')
> 
> Set-ExecutionPolicy -Scope CurrentUser -ExecutionPolicy Unrestricted
>
> clj -X:repl