
FUNCTION Scheduler GLOBAL
    SET DelayInMinutes TO 3
    Variables.CreateNewList List=> TriggerDateTimeList
    DISABLE Variables.AddItemToList Item: $'''08:10''' List: TriggerDateTimeList NewList=> TriggerDateTimeList
    DISABLE Variables.AddItemToList Item: $'''Sunday 11:15''' List: TriggerDateTimeList NewList=> TriggerDateTimeList
    DateTime.GetCurrentDateTime.Local DateTimeFormat: DateTime.DateTimeFormat.DateAndTime CurrentDateTime=> PreviousDateTime
    LOOP LoopIndex FROM 1 TO 99999999 STEP 1
        DateTime.GetCurrentDateTime.Local DateTimeFormat: DateTime.DateTimeFormat.DateAndTime CurrentDateTime=> CurrentDateTime
        SET RunFlow TO $'''False'''
        IF IsNotEmpty(TriggerDateTimeList) THEN
            LOOP FOREACH TriggerDateTime IN TriggerDateTimeList
                Text.SplitText.Split Text: TriggerDateTime StandardDelimiter: Text.StandardDelimiter.Space DelimiterTimes: 1 Result=> DateTimeList
                IF DateTimeList.Count = 2 THEN
                    Text.ConvertTextToDateTime.ToDateTime Text: $'''0001-01-01 %DateTimeList[1]%''' DateTime=> DateTimeObject
                    IF (CurrentDateTime.DayOfWeek.ToLower = DateTimeList[0].ToLower AND CurrentDateTime.Hour = DateTimeObject.Hour AND CurrentDateTime.Minute = DateTimeObject.Minute) = $'''True''' THEN
                        SET RunFlow TO $'''True'''
                        EXIT LOOP
                    END
                ELSE
                    Text.ConvertTextToDateTime.ToDateTime Text: $'''0001-01-01 %DateTimeList[0]%''' DateTime=> DateTimeObject
                    IF (CurrentDateTime.Hour = DateTimeObject.Hour AND CurrentDateTime.Minute = DateTimeObject.Minute) = $'''True''' THEN
                        SET RunFlow TO $'''True'''
                        EXIT LOOP
                    END
                END
            END
        ELSE
            DateTime.Subtract FromDate: CurrentDateTime SubstractDate: PreviousDateTime TimeUnit: DateTime.DifferenceTimeUnit.Minutes TimeDifference=> TimeDifference
            IF (LoopIndex = 1 OR TimeDifference >= DelayInMinutes) = $'''True''' THEN
                SET RunFlow TO $'''True'''
            END
        END
        IF RunFlow = $'''True''' THEN
            SET PreviousDateTime TO CurrentDateTime
            # Trigger Your Flows Here
            CALL SF0_Scraping
        END
        WAIT DelayInMinutes * 60
    END
    # Tested with Power Automate Desktop version 2.16.00306.22020
END FUNCTION
