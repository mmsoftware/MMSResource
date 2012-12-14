unit NxCommonReg;

interface

uses
  Classes, DesignIntf, DesignEditors, NxClasses, NxEdit, NxStdCtrls, NxThemesSupport, NxPopupControl;

type
  TNxColorSchemeEditor = class(TDefaultEditor)
  public
    function GetVerb (Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  Dialogs, NxVersions;

procedure Register;
begin
  RegisterComponents('Next Suite', [TNxColorScheme]);
  RegisterComponents('Next Editors', [TNxButtonEdit, TNxCalcEdit, TNxFolderEdit, TNxImagePathEdit, TNxCheckBox,
    TNxColorPicker, TNxComboBox, TNxDatePicker, TNxFontComboBox, TNxSpinEdit, TNxEdit, TNxMemo,
    TNxNumberEdit, TNxRadioButton, TNxTabControl, TNxTimeEdit, TNxMonthCalendar]);
  RegisterComponentEditor(TNxColorScheme, TNxColorSchemeEditor);
end;

{ TNxColorSchemeEditor }

procedure TNxColorSchemeEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  with GetComponent as TNxColorScheme do
    case Index of
      0: ColorScheme := csBlue;
      1: ColorScheme := csSilver;
      2: ColorScheme := csBlack;
      4: ShowMessage('Copyright (C) 1996-2008 Berg' + #13#10 +
                     'Version ' + strNextSuiteVer + #13#10 +
                     'http://www.bergsoft.net/');
    end;
end;

function TNxColorSchemeEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := '&Blue Color Scheme';
    1: Result := '&Silver Color Scheme';
    2: Result := 'B&lack Color Scheme';
    3: Result := '-';
    4: Result := 'Version...';
  end;
end;

function TNxColorSchemeEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

end.
