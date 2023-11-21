  #define MyAppName "AquaCropPlotter"
#define MyAppVersion "1.0.0"
#define MyAppExeName "AquaCropPlotter.bat"
#define RVersion "4.1.0"
#define IncludeR true
#define PandocVersion "2.11.4"
#define IncludePandoc false
#define IncludeChrome false
#define RtoolsVersion "35"
#define IncludeRtools false
#define MyAppPublisher ""
#define MyAppURL ""

[Setup]
AppName = {#MyAppName}
AppId = {{P7QEIIR8-072U-WB0W-Y8WB-VHTN0QXXJVMZ}
DefaultDirName = {userdocs}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = RInno_installer
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = aquacropplotter_icon.ico
AppVersion = 1.0.0
AppPublisher = {#MyAppPublisher}
AppPublisherURL = {#MyAppURL}
AppSupportURL = {#MyAppURL}
AppUpdatesURL = {#MyAppURL}
PrivilegesRequired = lowest
InfoBeforeFile = info_before.txt
InfoAfterFile = info_after.txt
Compression = lzma2/ultra64
SolidCompression = yes
ArchitecturesInstallIn64BitMode = x64

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\aquacropplotter_icon.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\aquacropplotter_icon.ico"

[Files]
Source: "LICENSE"; Flags: dontcopy noencryption
Source: "{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
#if IncludeR
    Source: "R-{#RVersion}-win.exe"; DestDir: "{tmp}"; Check: RNeeded
#endif
#if IncludePandoc
    Source: "pandoc-{#PandocVersion}-windows.msi"; DestDir: "{tmp}"; Check: PandocNeeded
#endif
#if IncludeChrome
    Source: "chrome_installer.exe"; DestDir: "{tmp}"; Check: ChromeNeeded
#endif
#if IncludeRtools
    Source: "Rtools{#RtoolsVersion}.exe"; DestDir: "{tmp}";
#endif
Source: "app.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "AquaCropPlotter.bat"; DestDir: "{app}"; Flags: ignoreversion;
Source: "aquacropplotter_icon.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion;
Source: "Rinno setup standalone app user browser.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "bin/askpass_1.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/backports_1.4.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/base64enc_0.1-3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/bit_4.0.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/bit64_4.0.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/blob_1.2.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/broom_1.0.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/bslib_0.4.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/cachem_1.0.7.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/callr_3.7.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/cellranger_1.1.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/cli_3.6.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/clipr_0.8.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/codetools_0.2-19.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/colorspace_2.1-0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/commonmark_1.9.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/conflicted_1.2.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/cpp11_0.4.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/crayon_1.5.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/crosstalk_1.2.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/curl_5.0.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/data.table_1.14.8.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/DBI_1.1.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/dbplyr_2.3.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/digest_0.6.31.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/dplyr_1.1.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/DT_0.27.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/dtplyr_1.3.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/ellipsis_0.3.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/evaluate_0.20.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/fansi_1.0.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/farver_2.1.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/fastmap_1.1.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/fontawesome_0.5.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/forcats_1.0.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/fs_1.5.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/furrr_0.3.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/future_1.32.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/gargle_1.4.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/generics_0.1.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/ggplot2_3.4.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/globals_0.16.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/glue_1.6.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/googledrive_2.1.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/googlesheets4_1.1.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/gtable_0.3.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/haven_2.5.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/highr_0.10.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/hms_1.1.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/htmltools_0.5.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/htmlwidgets_1.6.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/httpuv_1.6.9.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/httr_1.4.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/ids_1.0.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/installr_0.23.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/isoband_0.2.7.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/jquerylib_0.1.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/jsonlite_1.8.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/knitr_1.42.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/labeling_0.4.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/later_1.3.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/lattice_0.21-8.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/lazyeval_0.2.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/lifecycle_1.0.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/listenv_0.9.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/lubridate_1.9.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/magrittr_2.0.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/MASS_7.3-58.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/Matrix_1.5-4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/memoise_2.0.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/mgcv_1.8-42.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/mime_0.12.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/modelr_0.1.11.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/munsell_0.5.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/nlme_3.1-162.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/openssl_2.0.6.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/parallelly_1.35.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/pillar_1.9.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/pkgconfig_2.0.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/prettyunits_1.1.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/processx_3.8.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/progress_1.2.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/promises_1.2.0.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/ps_1.7.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/purrr_1.0.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/R6_2.5.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/ragg_1.2.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rappdirs_0.3.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/RColorBrewer_1.1-3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/Rcpp_1.0.10.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/readr_2.1.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/readxl_1.4.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rematch_1.0.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rematch2_2.1.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/reprex_2.0.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rlang_1.1.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rmarkdown_2.21.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rstudioapi_0.14.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/rvest_1.0.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/sass_0.4.5.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/scales_1.2.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/selectr_0.4-2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/shiny_1.7.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/shinyBS_0.61.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/shinydashboard_0.7.2.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/shinyjs_2.1.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/sourcetools_0.1.7-1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/stringi_1.7.12.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/stringr_1.5.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/sys_3.4.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/systemfonts_1.0.4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/textshaping_0.3.6.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/tibble_3.2.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/tidyr_1.3.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/tidyselect_1.2.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/tidyverse_2.0.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/timechange_0.2.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/tinytex_0.45.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/tzdb_0.3.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/utf8_1.2.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/uuid_1.1-0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/vctrs_0.6.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/viridisLite_0.4.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/vroom_1.6.1.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/withr_2.5.0.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/xfun_0.39.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/xml2_1.3.3.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/xtable_1.8-4.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "bin/yaml_2.3.7.zip"; DestDir: "{app}\bin"; Flags: ignoreversion;
Source: "utils/config.cfg"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/ensure.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/get_app_from_app_url.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/launch_app.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/package_manager.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils/wsf/js/JSON.minify.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils/wsf/js/json2.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils/wsf/js/run.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils/wsf/run.wsf"; DestDir: "{app}\utils\wsf"; Flags: ignoreversion;
Source: "www/aquacropplotter.png"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/aquacropplotter_icon.ico"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/aquacropplotter_icon.jpg"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/glossary.csv"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/naming convention.png"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www/workflow.png"; DestDir: "{app}\www"; Flags: ignoreversion;

  [Run]
  #if IncludeR
      Filename: "{tmp}\R-{#RVersion}-win.exe"; Parameters: "/SILENT"; WorkingDir: {tmp}; Check: RNeeded; Flags: skipifdoesntexist; StatusMsg: "Installing R if needed"
  #endif
  #if IncludePandoc
      Filename: "msiexec.exe"; Parameters: "/i ""{tmp}\pandoc-{#PandocVersion}-windows.msi"" /q"; WorkingDir: {tmp}; Check: PandocNeeded; Flags: skipifdoesntexist; StatusMsg: "Installing Pandoc if needed"
  #endif
  #if IncludeChrome
      Filename: "{tmp}\chrome_installer.exe"; Parameters: "/install"; WorkingDir: {tmp}; Check: ChromeNeeded; Flags: skipifdoesntexist; StatusMsg: "Installing Chrome if needed"
  #endif
  #if IncludeRtools
      Filename: "{tmp}\Rtools{#RtoolsVersion}.exe"; Parameters: "/VERYSILENT"; WorkingDir: {tmp}; Flags: skipifdoesntexist; StatusMsg: "Installing Rtools"
  #endif
  Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: shellexec postinstall skipifsilent


  [UninstallDelete]
Type: filesandordirs; Name: "{app}\library";
Type: filesandordirs; Name: "{app}\utils";
Type: filesandordirs; Name: "{app}\log";

[Code]
const
  ChromeRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe';
  IERegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE';
  FFRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\firefox.exe';
var
  RVersions: TStringList;
  RRegKey: string;
  RegPathsFile: string;
  SecondLicensePage: TOutputMsgMemoWizardPage;
  License2AcceptedRadio: TRadioButton;
  License2NotAcceptedRadio: TRadioButton;

// Is R installed?
function RDetected(): boolean;
var
    v: Integer;
    success: boolean;
begin
  success := false;
  for v := 0 to (RVersions.Count - 1) do
    begin
      if RegKeyExists(HKLM, 'Software\R-Core\R\' + RVersions[v]) or RegKeyExists(HKCU, 'Software\R-Core\R\' + RVersions[v]) then
      begin
        success := true;
        RRegKey := 'Software\R-Core\R\' + RVersions[v];
        break;
      end;
    end;
  Result := success;
end;

// If R is not detected, it is needed
function RNeeded(): boolean;
begin
  Result := not RDetected;
end;


// Is Chrome installed?
function ChromeDetected(): boolean;
var
    success: boolean;
begin
  success := RegKeyExists(HKLM, ChromeRegKey) or RegKeyExists(HKCU, ChromeRegKey);
  begin
    Result := success;
  end;
end;

// If Chrome is not detected, it is needed
function ChromeNeeded(): boolean;
begin
  Result := not ChromeDetected;
end;


// Registry path update function (adds an extra backslash for json)
function AddBackSlash(Value: string): string;
begin
  Result := Value;
  StringChangeEx(Result, '\', '\\', True);
end;


// Pandoc is stored in the System PATH
function PandocDetected(): boolean;
var
  PandocDir, Path: String;
begin
  Log('Checking for Pandoc in %PATH%');
  if RegQueryStringValue(HKEY_CURRENT_USER, 'Environment', 'Path', Path) then
  begin // Successfully read the value
    Log('HKCU\Environment\PATH = ' + Path);
    PandocDir := ExpandConstant('{localappdata}\Pandoc\');
    Log('Looking for Pandoc in %PATH%: ' + PandocDir + ' in ' + Path);
    if Pos(LowerCase(PandocDir), Lowercase(Path)) = 0 then
		begin
			Log('Did not find Pandoc in %PATH%');
			Result := False;
		end
    else
		begin
			Log('Found Pandoc in %PATH%');
			Result := True;
		end
  end
  else // The key probably doesn't exist
  begin
    Log('Could not access HKCU\Environment\PATH.');
    Result := False;
  end;
end;

// If Pandoc is not detected, it is needed
function PandocNeeded(): boolean;
begin
  Result := not PandocDetected;
end;

// Save installation paths
procedure SaveInstallationPaths();
var
  RPath, ChromePath, IEPath, FFPath, PandocPath: string;
begin
  RPath := '';
  ChromePath := '';
  IEPath := '';
  FFPath := '';
  PandocPath := ExpandConstant('{localappdata}\Pandoc\');
  RegPathsFile := ExpandConstant('{app}\utils\regpaths.json');

  if Length(RRegKey) = 0 then
    RDetected;

  // Create registry paths file
  SaveStringToFile(RegPathsFile, '{' + #13#10, True);

  // R RegPath
  if RegQueryStringValue(HKLM, RRegKey, 'InstallPath', RPath) or RegQueryStringValue(HKCU, RRegKey, 'InstallPath', RPath) then
    SaveStringToFile(RegPathsFile, '"r": "' + AddBackSlash(RPath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"r": "none",' + #13#10, True);

  // Chrome RegPath
  if RegQueryStringValue(HKLM, ChromeRegKey, 'Path', ChromePath) or RegQueryStringValue(HKCU, ChromeRegKey, 'Path', ChromePath) then
    SaveStringToFile(RegPathsFile, '"chrome": "' + AddBackSlash(ChromePath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"chrome": "none",' + #13#10, True);

  // Internet Explorer RegPath
  if RegQueryStringValue(HKLM, IERegKey, '', IEPath) then
    SaveStringToFile(RegPathsFile, '"ie": "' + AddBackSlash(IEPath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"ie": "none",' + #13#10, True);

  // Firefox RegPath
  if RegQueryStringValue(HKLM, FFRegKey, 'Path', FFPath) then
    SaveStringToFile(RegPathsFile, '"ff": "' + AddBackSlash(FFPath) + '",' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"ff": "none",' + #13#10, True);

  // Pandoc RegPath
  // ** Last Line in json file (no trailing comma) **
  if PandocDetected() then
    SaveStringToFile(RegPathsFile, '"pandoc": "' + AddBackSlash(PandocPath) + '"' + #13#10, True)
  else
    SaveStringToFile(RegPathsFile, '"pandoc": "none"' + #13#10, True);

  SaveStringToFile(RegPathsFile, '}', True);
end;

// Pre- and post-installation actions
procedure CurStepChanged(CurStep: TSetupStep);
begin
  // Pre-installation actions
  if CurStep = ssInstall then
  begin
  #if IncludeR
  #else
    // With `CurStep = ssInstall` we can still `Abort` if R not included but needed
    if RNeeded then
    begin
      SuppressibleMsgBox(Format('Error: R >= %s not found',[RVersions[RVersions.Count - 1]]), mbError, MB_OK, MB_OK);
      Abort;
    end;
  #endif
  end;
  // Post-installation actions
  if CurStep = ssPostInstall then
  begin
    SaveInstallationPaths;
  end;
end;

// Add RInno's license to the installer
procedure CheckLicense2Accepted(Sender: TObject);
begin
  { Update Next button when user (un)accepts the license }
  WizardForm.NextButton.Enabled := License2AcceptedRadio.Checked;
end;

function CloneLicenseRadioButton(Source: TRadioButton): TRadioButton;
begin
  Result := TRadioButton.Create(WizardForm);
  Result.Parent := SecondLicensePage.Surface;
  Result.Caption := Source.Caption;
  Result.Left := Source.Left;
  Result.Top := Source.Top;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.OnClick := @CheckLicense2Accepted;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  { Update Next button when user gets to second license page }
  if CurPageID = SecondLicensePage.ID then
  begin
    CheckLicense2Accepted(nil);
  end;
end;

procedure InitializeWizard();
var
  LicenseFileName: string;
  LicenseFilePath: string;
begin
  { Create second license page, with the same labels as the original license page }
  SecondLicensePage :=
    CreateOutputMsgMemoPage(
      wpLicense, SetupMessage(msgWizardLicense), SetupMessage(msgLicenseLabel),
      SetupMessage(msgLicenseLabel3), '');

  { Shrink license box to make space for radio buttons }
  SecondLicensePage.RichEditViewer.Height := WizardForm.LicenseMemo.Height;

  { Load license }
  { Loading ex-post, as Lines.LoadFromFile supports UTF-8, }
  { contrary to LoadStringFromFile. }
  LicenseFileName := 'LICENSE';
  ExtractTemporaryFile(LicenseFileName);
  LicenseFilePath := ExpandConstant('{tmp}\' + LicenseFileName);
  SecondLicensePage.RichEditViewer.Lines.LoadFromFile(LicenseFilePath);
  DeleteFile(LicenseFilePath);

  { Clone accept/do not accept radio buttons for the second license }
  License2AcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseAcceptedRadio);
  License2NotAcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseNotAcceptedRadio);

  { Initially not accepted }
  License2NotAcceptedRadio.Checked := True;

  // Initialize the values of supported versions
  RVersions := TStringList.Create; // Make a new TStringList object reference
  // Add strings to the StringList object
  RVersions.Add('4.3.2');
  RVersions.Add('4.3.0');
  RVersions.Add('4.3.2');
  RVersions.Add('4.3.1');
  RVersions.Add('4.3.0');
  RVersions.Add('4.2.3');
  RVersions.Add('4.2.2');
  RVersions.Add('4.2.1');
  RVersions.Add('4.2.0');
  RVersions.Add('4.1.3');
  RVersions.Add('4.1.2');
  RVersions.Add('4.1.1');
  RVersions.Add('4.1.0');

end;

// Procedure called by InnoSetup when it is closing
procedure DeinitializeSetup();
begin
  RVersions.Free;
end;
