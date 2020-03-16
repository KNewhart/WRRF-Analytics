%windir%\SysWOW64\cscript "%homepath%\Desktop\LIFT_2019\Raw data\Historian Data Export CSV.vbs"
"%ProgramFiles%\R\R-3.6.3\bin\Rscript.exe" "%homepath%\Desktop\LIFT_2019\R output\Ammonia Forecast.R"
%windir%\SysWOW64\cscript "%homepath%\Desktop\LIFT_2019\R output\Historian Data Import CSV.vbs"