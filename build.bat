REM clean up before calling assembler 
del fly.p
del fly.lst
del fly.sym

call zxasm fly

REM call racing.p will auto run emulator EightyOne if installed
call fly.p