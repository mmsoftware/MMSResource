unit UTF8ToGb2312;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls;
  function UtfToGb(const AUTFCode: String) : string;

implementation

function UtfToGb(const AUTFCode: String) : string;
var
  SourceLength: integer;
  DoneLength: integer;
  AscNo: integer;
  Byte1,Byte2,Byte3:integer;
begin
  Result := '';
  if Trim(AUTFCode) = '' then exit;

  SourceLength := Length(AUTFCode) + 1;
  DoneLength := 1;
  repeat
    AscNo := ord(AUTFCode[DoneLength]);
    case (AscNo and $E0) of
    $E0:
    begin
      Byte1 := (AscNo and $0f) shl 12;
      Inc(DoneLength);
      if DoneLength > SourceLength then break;
      AscNo := ord(AUTFCode[DoneLength]);
      Byte2 := (AscNo and $3f) shl 6;
      Inc(DoneLength);
      if DoneLength > SourceLength then break;
      AscNo := ord(AUTFCode[DoneLength]);
      Byte3 := AscNo and $3f;
    end;
    $C0:
    begin
      Byte1 := (AscNo and $1f) shl 6;
      Inc(DoneLength);
      if DoneLength > SourceLength then break;
      AscNo := ord(AUTFCode[DoneLength]);
      Byte2 := (AscNo and $3f);
      Byte3 := 0;
    end;
    0..$bf:
    begin
      Byte1 := AscNo;
      Byte2 := 0;
      Byte3 := 0;
    end;
    end;
    Result := Result + widechar(Byte1 + Byte2 + Byte3);
    Inc(DoneLength);
    if DoneLength > SourceLength then break;
  until DoneLength >= SourceLength;
end;

end.
