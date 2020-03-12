Call ImportDataFromCSV("ww-historian", "HistorianDataImport.csv")

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Sub ImportDataFromCSV(strServerName, strCSVFilePath)
' - strServerName: Name of Historian Server.  If left Null will use local server.
' - strCSVFilePath: Filename containing data to import.  Assumed in local directory to script.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub ImportDataFromCSV(strServerName, strCSVFilePath)

    'On Error Resume Next

    Dim ihServerMgr, ihServer, strOutputPath

    WScript.Echo Now() & ": Starting Data Import for file " & strCSVFilePath

    ' Look for input file in directory where script resides.
    strCSVFilePath = CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName) & "\" & strCSVFilePath

    Set ihServerMgr = CreateObject("iHistorian_SDK.ServerManager")
    Set ihServer = CreateObject("iHistorian_SDK.Server")
    If (ihServer is Nothing) Then
        WScript.Echo "ERROR: Unable to create iHistorian_SDK.Server object"
    ElseIf ihServer.Connect(CStr(strServerName)) Then
        WScript.Echo Now() & ": Connected to Historian Server - " & strServerName
        Set ihDataRecords = ihServer.Data.NewRecordset
        With ihDataRecords
            If (Not .Import(Cstr(strCSVFilePath), 1)) Then
                WScript.Echo "ERROR: Unable to import file " & .LastError
            Else
                If (.WriteRecordset) Then
                    WScript.Echo Now() & ": Data import committed from file " & strCSVFilePath
                Else
                    WScript.Echo Now() & ": Data import not committed from file " & strCSVFilePath                    
                End If
            End If
        End With
        ihServer.Disconnect
    Else
        ' WScript.Echo "ERROR: Unable to connect to Historian server - " & ihServerMgr.DefaultServer.ServerName
    End If
	
End Sub