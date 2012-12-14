unit Delphi2007Frm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, IniFiles;

type
  TDelphi2007Form = class(TForm)
    btn1: TButton;
    edt1: TEdit;
    edt2: TEdit;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Delphi2007Form: TDelphi2007Form;

implementation

uses
  MMSSystems, MMSFileUnits;
const
  CMMS = 'MMS';
{$R *.dfm}

procedure TDelphi2007Form.btn1Click(Sender: TObject);
var
  sStr: string;
begin
//  ShowMessage(IntToStr(Length(High(sStr))));
end;

procedure TDelphi2007Form.FormCreate(Sender: TObject);
var
  sPath: string;
  oIni: TIniFile;
begin
  sPath := ExtractFileDir(Application.ExeName);
  sPath := ExtractFileDir(sPath);
  AddEnvironment(CMMS, sPath);
  oIni := TIniFile.Create(GetExePath + 'Delphi2007.Ini');
  try
    AddDelphi2007LibraryPath(oIni.ReadString('LibraryPath', 'Path', ''));
  finally
    FreeAndNil(oIni);
  end;
end;

end.
