program Delphi2007;

uses
  Forms,
  Delphi2007Frm in 'Delphi2007Frm.pas' {Delphi2007Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDelphi2007Form, Delphi2007Form);
  Application.Run;
end.
