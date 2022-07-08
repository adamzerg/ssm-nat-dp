
FUNCTION SF1_GenMap_Html GLOBAL
    DISABLE DateTime.GetCurrentDateTime.Local DateTimeFormat: DateTime.DateTimeFormat.DateAndTime CurrentDateTime=> CurrentDateTime
    DISABLE DateTime.Add DateTime: CurrentDateTime TimeToAdd: lookback_minutes TimeUnit: DateTime.TimeUnit.Minutes ResultedDate=> MinFileDateTime
    DISABLE Folder.GetFiles Folder: file_path_aptom FileFilter: $'''*.xlsx''' IncludeSubfolders: False FailOnAccessDenied: True SortBy1: Folder.SortBy.CreationTime SortDescending1: True SortBy2: Folder.SortBy.NoSort SortDescending2: False SortBy3: Folder.SortBy.NoSort SortDescending3: False Files=> file_list_aptom
    DISABLE SET file_aptom_last TO file_list_aptom[0].FullName
    DISABLE Text.Replace Text: file_aptom_last TextToFind: $'''.*-''' IsRegEx: True IgnoreCase: False ReplaceWith: $'''%''%''' ActivateEscapeSequences: False Result=> Replaced
    DISABLE Text.Replace Text: Replaced TextToFind: $'''.xlsx''' IsRegEx: False IgnoreCase: False ReplaceWith: $'''%''%''' ActivateEscapeSequences: False Result=> Replaced
    DISABLE Text.ConvertTextToDateTime.ToDateTimeCustomFormat Text: Replaced CustomFormat: $'''yyyyMMddHHmmss''' DateTime=> TextAsDateTime
    DISABLE Display.ShowMessageDialog.ShowMessage Title: $'''ShowMesasge''' Message: TextAsDateTime Icon: Display.Icon.None Buttons: Display.Buttons.OK DefaultButton: Display.DefaultButton.Button1 IsTopMost: False
    DISABLE IF TextAsDateTime >= MinFileDateTime THEN
        Scripting.RunDOSCommand.RunDOSCommand DOSCommandOrApplication: $'''C:\\PROGRA~1\\R\\R-42~1.0\\bin\\Rscript.exe D:\\Documents\\GitHub\\ssm-nat-dp\\02-ssm-nat-dp-map.R''' WorkingDirectory: $'''D:\\Documents\\GitHub\\ssm-nat-dp\\''' StandardOutput=> CommandOutput StandardError=> CommandErrorOutput ExitCode=> CommandExitCode
        IF Contains(CommandOutput, $'''map generated succeeded''', False) THEN
            CALL SF2_Upload_Html
        END
    DISABLE END
END FUNCTION
