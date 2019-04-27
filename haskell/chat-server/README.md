# chat-server
## install dependencies
```
$ stack install stm containers async network
```

## run and access from telnet
```
$ stack run
Listening on port 7777

-- other terminal
$ telnet localhost 7777
Trying ::1...
Connected to localhost.
Escape character is '^]'.
What is your name?
kerfume
*** kerfume has connected.
```
