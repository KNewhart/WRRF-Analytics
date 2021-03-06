Call ExportDataToCSV("ww-historian", "NIA33391_AB3_PREDICTED_AMMONIA.F_CV", "4/2/2020 16:00", Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.NIA33391_AB3_AMMONIA.F_CV", "5/1/2020 00:00", Now)

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Function ExportDataToCSV(ServerName, TagName, StartTime, EndTime)
' - ServerName: Name of Historian Server.  If left NULL will use local server.
' - TagName: Name of tag to be exported
' - StartTime: Start of raw data query
' - EndTime: End of raw data query
'
'  Examples:
'  Call ExportRawToCSV("", "Simulation00001", "1/1/2017 8:00", "1/2/2017 13:00")
'  Call ExportRawToCSV("", "Simulation00001", Now-1, Now) - returns data from exactly 24 hours ago until right now.
'  Call ExportRawToCSV("SERVER1", "Simulation00001", Yesterday, Today) - returns data from yesterday on a remote server.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub ExportDataToCSV(strServerName, strTagName, dtStartTime, dtEndTime)

	Dim ihServer, ihDataRecords, strOutputFile, strOutputPath

	WScript.Echo Now() & ": Starting Raw Data Export for tag " & strTagName & ", from " & dtStartTime & " - " & dtEndTime

	' Place output file in directory where script resides.
	strOutputPath = CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName)
	'strOutputFile = strOutputPath & "\" & strTagName & "_" & DTFormat(dtStartTime) & "_" & DTFormat(dtEndTime) & ".CSV"
	strOutputFile = strOutputPath & "\" & strTagName & ".CSV"

	' Make sure we can connect to Historian
	Set ihServer = CreateObject("iHistorian_SDK.Server")
	If ihServer is Nothing Then
		WScript.Echo "ERROR: Unable to create iHistorian_SDK.Server object"
	ElseIf ihServer.Connect(CStr(strServerName)) Then
	    WScript.Echo Now() & ": Connected to Historian Server - " & strServerName
		Set ihDataRecords = ihServer.Data.NewRecordset
		With ihDataRecords
			.Criteria.Tagmask = strTagName
			.Criteria.StartTime = dtStartTime
			.Criteria.EndTime = dtEndTime
            		.Criteria.SamplingInterval = 86400000              ' 86,400,000 = 1 day, 604,800,000 = 1 week
			.Criteria.SamplingMode = 4                         ' CurVal(1), Interpolated(2), RawByTime(4), RawByNumber(5), Calc(6)
            		.Criteria.CalculationMode = 6                      ' Count(6), Max(5), Min(4), Avg(1)
			.Fields.Value = True
			.Fields.DataQuality = False
			' Execute query against historian with parameters provided above.
			If NOT .QueryRecordset Then
				WScript.Echo "ERROR: Unable to query historian tag " & strTagName & " between " & dtStartTime & " and " & dtEndTime
			Else
				WScript.Echo Now() & ": Query execution completed"
				' Export recordset to output file.
				If NOT .Export(Cstr(strOutputFile), 1) Then
					WScript.Echo "ERROR: Unable to export file " & .LastError
				Else
					WScript.Echo Now() & ": File export completed - " & strOutputFile
				End If
			End If
		End With
		ihServer.Disconnect
	Else
		WScript.Echo "ERROR: Unable to connect to Historian server"
	End If
	
End Sub

Function DTFormat(dtDateTime)

	DTFormat = year(dtDateTime) & right("0" & month(dtDateTime), 2) & right("0" & day(dtDateTime), 2) & _
        right("0" & hour(dtDateTime), 2) & right("0" & minute(dtDateTime), 2) & right("0" & second(dtDateTime), 2)

End Function