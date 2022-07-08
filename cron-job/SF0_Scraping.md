
FUNCTION SF0_Scraping GLOBAL
    DISABLE DateTime.GetCurrentDateTime.Local DateTimeFormat: DateTime.DateTimeFormat.DateAndTime CurrentDateTime=> CurrentDateTime
    Text.ConvertDateTimeToText.FromCustomDateTime DateTime: CurrentDateTime CustomFormat: $'''yyyyMMddHHmmss''' Result=> FormattedDateTime
    Web.DownloadFromWeb.DownloadToFile Url: $'''https://www.ssm.gov.mo/docs/stat/apt/RNA010.xlsx''' FilePath: $'''%file_path_booking%RNA010-%FormattedDateTime%.xlsx''' ConnectionTimeout: 30 FollowRedirection: True ClearCookies: False UserAgent: $'''Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.21) Gecko/20100312 Firefox/3.6''' Encoding: Web.Encoding.AutoDetect AcceptUntrustedCertificates: False DownloadedFile=> DownloadedFile
    DISABLE Display.ShowMessageDialog.ShowMessage Title: $'''PopupMesasge''' Message: DownloadedFile Icon: Display.Icon.None Buttons: Display.Buttons.OK DefaultButton: Display.DefaultButton.Button1 IsTopMost: False
    WebAutomation.LaunchEdge.LaunchEdge Url: $'''https://eservice.ssm.gov.mo/aptmon/ch''' WindowState: WebAutomation.BrowserWindowState.Normal ClearCache: False ClearCookies: False WaitForPageToLoadTimeout: 60 Timeout: 60 BrowserInstance=> BrowserAptmon
    WebAutomation.ExtractData.ExtractTable BrowserInstance: BrowserAptmon Control: $'''html > body > div:eq(1) > div > div:eq(2) > div:eq(1) > div > table > tbody > tr''' ExtractionParameters: {[$'''td:eq(0)''', $'''Own Text''', $'''''', $'''Value #1'''], [$'''td:eq(1) > div:eq(0)''', $'''Own Text''', $'''''', $'''Value #2'''], [$'''td:eq(2)''', $'''Own Text''', $'''''', $'''Value #3'''], [$'''td:eq(3)''', $'''Own Text''', $'''''', $'''Value #4'''], [$'''td:eq(4)''', $'''Own Text''', $'''''', $'''Value #5'''], [$'''td:eq(6)''', $'''Own Text''', $'''''', $'''Value #6'''] } PostProcessData: False TimeoutInSeconds: 60 ExtractedData=> DataFromWebPage
    Excel.LaunchExcel.LaunchUnderExistingProcess Visible: False Instance=> ExcelFinal
    SET ExcelHeader TO {['序號', 'Location', '口採樣點', '鼻採樣點', '輪候人數', '等候時間'] }
    Excel.WriteToExcel.Write Instance: ExcelFinal Value: ExcelHeader
    Excel.WriteToExcel.WriteCell Instance: ExcelFinal Value: DataFromWebPage Column: $'''A''' Row: 2
    Excel.SaveExcel.SaveAs Instance: ExcelFinal DocumentFormat: Excel.ExcelFormat.FromExtension DocumentPath: $'''%file_path_aptom%aptmon-%FormattedDateTime%'''
    Excel.CloseExcel.CloseAndSave Instance: ExcelFinal
    WebAutomation.CloseWebBrowser BrowserInstance: BrowserAptmon
    CALL SF1_GenMap_Html
END FUNCTION
