@echo off
set astyle="D:\Embedded\AStyle\bin\astyle.exe"
for /r . %%a in (*.cpp;*.c) do %astyle% -n --style=linux "%%a"
for /r . %%a in (*.hpp;*.h) do %astyle% -n --style=linux "%%a"
for /r . %%a in (*.orig) do del "%%a"
exit
