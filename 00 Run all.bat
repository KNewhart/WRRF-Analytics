"C:\Program Files\R\R-3.6.3\bin\Rscript.exe" "C:\Users\newhartk\Desktop\LIFT_2019\R output\Ammonia Forecast v2 - 1.R" >C:\Users\newhartk\Desktop\LIFT_2019\output.txt 2>&1
%windir%\SysWOW64\cscript "C:\Users\newhartk\Desktop\LIFT_2019\Raw data\Historian Data Export CSV v2.vbs" >>C:\Users\newhartk\Desktop\LIFT_2019\output.txt 2>&1
"C:\Program Files\R\R-3.6.3\bin\Rscript.exe" "C:\Users\newhartk\Desktop\LIFT_2019\R output\Ammonia Forecast v2 - 2.R" >>C:\Users\newhartk\Desktop\LIFT_2019\output.txt 2>&1
%windir%\SysWOW64\cscript "C:\Users\newhartk\Desktop\LIFT_2019\R output\Historian Data Import CSV.vbs" >>C:\Users\newhartk\Desktop\LIFT_2019\output.txt 2>&1
'%windir%\SysWOW64\cscript "C:\Users\newhartk\Desktop\LIFT_2019\R output\Historian Data Import Check.vbs"
'cmd /k