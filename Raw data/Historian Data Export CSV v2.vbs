Const n = 0.00266643328799142

Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_16_DO_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_7_DO_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.NIX33384_AB3_Z8_DO.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_8_DO_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_7_FLOW_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_8_FLOW_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_16_VALVE_POS_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_7_VALVE_POS_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.BASIN3_8_VALVE_POS_PVENG.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.NIA33391_AB3_AMMONIA.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.NIR33391_AB3_Z3_NITRATE.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.NIR33392_AB3_Z9_NITRATE.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.NR4242.F_CV", Now-n, Now)
Call ExportDataToCSV("ww-historian", "WWSCADA2.NIF31053_PE_TO_ABAS_FLOW.F_CV", Now-n, Now)

Sub ExportDataToCSV(strServerName, strTagName, dtStartTime, dtEndTime)

	Dim ihServer, ihDataRecords, strOutputFile, strOutputPath

	WScript.Echo Now() & ": Starting Raw Data Export for tag " & strTagName & ", from " & dtStartTime & " - " & dtEndTime

	' Place output file in directory where script resides.
	strOutputPath = CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName)
	' strOutputFile = strOutputPath & "\" & strTagName & "_" & DTFormat(dtStartTime) & "_" & DTFormat(dtEndTime) & ".CSV"
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
