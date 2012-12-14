
{
    示例 为 : 建立以一个文档, 添加文字, 表格, 图像, 矩形.对他们进行操作

}
unit WordAppUnit;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleServer, WordXP, StdCtrls,ActiveX,ComObj,Oledb;

type
  TDrawType = (dtRang, dtEllise);
  {图像}
  TImage = class
  public
    //图像
    procedure AddImage(ARange: Range; const AFileName: string; AWidth: Integer = 60; AHeight: Integer = 60);
    procedure DelImage(ARange: Range; Index: Integer);
    procedure SetImageSize(ARange: Range; Index: Integer; AWidth: Integer = 60; AHeight: Integer = 60);
  end;

  TCommOper = class(TImage)
  public
    procedure AddText(ARange: Range; const AText: string);
    //删除文字
    procedure DelText(ARange: Range; const ADelText: string);
    //修改文字
    procedure EditText(ARange: Range; const ADelText, ANewText: string);
  end;

  {绘图}
  TDrawLine = class
  private
    FDocument: TWordDocument;
    FShap: Shape;
    procedure SetForeColor(AColor: Integer);
    procedure SetLineWeight(AWeight: Single);
    procedure SetDashStyle(AStyle: LongWord);
    procedure SetFillColor(AFillColor: Integer);
    procedure SetFillStyle(AStyle: LongWord);
  public
    constructor Create(ADocument: TWordDocument);
    procedure Draw(ADrawType: TDrawType);
  public
    property ForeColor: Integer write SetForeColor;
    property LineWeight: Single write SetLineWeight;
    property DashStyle: LongWord write SetDashStyle;
    property FillColor: Integer write SetFillColor;
    property FillStyle: LongWord write SetFillStyle;
  end;

  {表格}
  TTable = class(TCommOper)
  private
    FDocument: TWordDocument;
    FTable: Table;
  public
    constructor Create(ADocument: TWordDocument);
    destructor Destroy; override;
    procedure Add(ARow, ACol: Integer);

    procedure AddRow(ARow: Integer);
    procedure AddCol(ACol: Integer);
    procedure DelRow(ARow: Integer);
    procedure DelCol(ACol: Integer);

    function CellRange(ARow, ACol: Integer): Range;
    //添加文字
    procedure CellAddText(ARow, ACol: Integer; const AText: string);
    //删除文字
    procedure CellDelText(ARow, ACol: Integer; const ADelText: string);
    //修改文字
    procedure CellEditText(ARow, ACol: Integer; const ADelText,ANewText: string);
  end;

  {文档}
  TDoc = class(TCommOper)
  private
    F_Document: _Document;
    FWordDoc: TWordDocument;
    //标题内容
    //级别
    FTitleLev: Word;
    FTables: TList;
    FImages: TList;
    FDrawLines: TList;
    function GetImage(AIndx: Integer): TImage;
    function GetTable(AIndx: Integer): TTable;
    function GetDrawLine(AIndx: Integer): TDrawLine;
    function GetImageCount: Integer;
    function GetTableCount: Integer;
    function GetDrawLineCount: Integer;
    procedure SetTitleLev(ALeve: Word);
  public
    constructor Create;
    destructor Destroy; override;
    //返回最后一个对象
    function AddTable(): TTable;
    function AddImage(): TImage;
    function AddTDrawLine(): TDrawLine;
  public
    property Document: _Document read F_Document write F_Document;
    property WordDoc: TWordDocument read FWordDoc write FWordDoc;
    //
    property Imgaes[indx: Integer]: TImage read GetImage;
    property Tables[indx: Integer]: TTable read GetTable;
    property DrawLines[indx: Integer]: TDrawLine read GetDrawLine;

    property ImageCount: Integer read GetImageCount;
    property TableCount: Integer read GetTableCount;
    property DrawLineCount: Integer read GetDrawLineCount;

    property TitleLev: Word read FTitleLev write SetTitleLev;
  end;

  {应用}
  TWordApp = class
  private
    FWrdApp: TWordApplication;
    FConnected: Boolean;
    //断开时退出
    FQuit: Boolean;
    FDoc: Tdoc;
  public
    constructor Create(AQuit: Boolean = False);
    destructor Destroy; override;
    procedure NewDoc(ATemplatePath : string);
    procedure OpenDoc(const AFileName: string);
    procedure SaveDoc(const AFileName: string);
    procedure CloseDoc();
  public
    property Doc: TDoc read FDoc write FDoc;
  end;


implementation
{ TWordApp }

procedure TWordApp.CloseDoc;
begin
  if not FConnected then
    Exit;
end;

constructor TWordApp.Create(AQuit: Boolean = False);
begin
  FWrdApp := TWordApplication.Create(nil);
  FQuit := AQuit;
  try
    FWrdApp.Connect;
    FWrdApp.Visible := False;
    FWrdApp.Options.CheckSpellingAsYouType := False;
    FWrdApp.Options.CheckGrammarAsYouType := False;
    FConnected := True;
  except
    FConnected := False;
  end;
end;

destructor TWordApp.Destroy;
begin
  inherited;
  if FConnected then
  begin
    FWrdApp.Disconnect;
    if FQuit then
        FWrdApp.Quit;
  end;
  if FDoc <> nil then
    FreeAndNil(FDoc);
  FreeAndNil(FWrdApp);
end;

procedure TWordApp.NewDoc(ATemplatePath : string);
var
  Template, NewTemplate, DocumentType, Visible: OleVariant;
begin
  if FDoc <> nil then
      Exit;
  if not FConnected then
      Exit;
  Template := ATemplatePath;
  NewTemplate := False;
  DocumentType := wdNewBlankDocument;
  Visible := False;
  try
    FDoc := Tdoc.Create;
    FDoc.Document := FWrdApp.Documents.Add(Template, NewTemplate, DocumentType, Visible);
    FDoc.FWordDoc.ConnectTo(FDoc.Document);  //WordApplication1.Documents.Item(ItemIndex));
    //设置标题的字号
//    FDoc.FWordDoc.Range.Text := '              文档标题' + #13 + #13;

//    FDoc.FWordDoc.Paragraphs.Item(1).Range.Font.Bold := 1;
//    FDoc.FWordDoc.Paragraphs.Item(1).Range.Font.Size := 20;
  except
    Application.Terminate;
  end;
end;

procedure TWordApp.OpenDoc(const AFileName: string);
var
  encoding, ConfirmConversions, ReadOnly, PassWordDocument, PasswordTemplate,
  Revent, WritePasswordDocument, WritePassWordTemplate, Format,
  FileName, Revert, Visible: OleVariant;
begin
  if not FConnected then
    Exit;
  FileName := AFileName;
  ConfirmConversions := False;
  ReadOnly := False;
  Revent := False;
  PassWordDocument := '';
  PasswordTemplate := '';
  Revert := False;
  WritePasswordDocument := '';
  WritePassWordTemplate := '';
  Format := wdOpenFormatAuto;
  encoding := '';
  Visible := True;
  try
    FDoc.Document := FWrdApp.Documents.Open(FileName,ConfirmConversions,ReadOnly,Revent,PassWordDocument,
                                            PasswordTemplate,Revert,WritePasswordDocument,WritePassWordTemplate,Format,
                                            encoding,Visible,Revert,Revert,Revert);
    FDoc.FWordDoc.ConnectTo(FDoc.Document); //WordApplication1.Documents.Item(ItemIndex));
  except
      Application.Terminate;
  end;
end;

procedure TWordApp.SaveDoc(const AFileName: string);
var
  EParam : OleVariant;
  FileName : OleVariant;
begin
  FileName := AFileName;
  EParam := EmptyParam;

  try
    FDoc.FWordDoc.SaveAs(FileName, EParam);
  except
    showmessage('文档保存失败！');
  end;
end;

{ Tdoc }

function Tdoc.AddImage: TImage;
begin
  Result := TImage.Create;
  FImages.Add(Result);
end;

function Tdoc.AddTable: TTable;
begin
  Result := TTable.Create(FWordDoc);
  FTables.Add(Result);
end;

function Tdoc.AddTDrawLine: TDrawLine;
begin
  Result := TDrawLine.Create(FWordDoc);
  FDrawLines.Add(Result);
end;

constructor Tdoc.Create;
begin
  inherited Create;
  FWordDoc := TWordDocument.Create(nil);
  FImages := TList.Create;
  FTables := TList.Create;
  FDrawLines := TList.Create;
end;

destructor Tdoc.Destroy;
begin
  while FTables.Count > 0 do
  begin
      TTable(FTables[0]).Free;
      FTables.Delete(0);
  end;
  FTables.Free;

  while FImages.Count > 0 do
  begin
      TImage(FImages[0]).Free;
      FImages.Delete(0);
  end;
  FImages.Free;

  while FDrawLines.Count > 0 do
  begin
      TDrawLine(FDrawLines[0]).Free;
      FDrawLines.Delete(0);
  end;
  FDrawLines.Free;
  FWordDoc.Free;
  inherited;
end;

function TDoc.GetDrawLine(AIndx: Integer): TDrawLine;
begin
  Result := nil;
  if (AIndx > -1) and (AIndx < FDrawLines.Count) then
        Result := FDrawLines[AIndx];
end;

function TDoc.GetDrawLineCount: Integer;
begin
  Result := FDrawLines.Count;
end;

function TDoc.GetImage(AIndx: Integer): TImage;
begin
  Result := nil;
  if (AIndx > -1) and (AIndx < FImages.Count) then
    Result := FImages[AIndx];
end;

function TDoc.GetImageCount: Integer;
begin
  Result := FImages.Count;
end;

function TDoc.GetTable(AIndx: Integer): TTable;
begin
  Result := nil;
  if (AIndx > -1) and (AIndx < FTables.Count) then
    Result := FTables[AIndx];
end;

function TDoc.GetTableCount: Integer;
begin
  Result := FTables.Count;
end;

procedure TDoc.SetTitleLev(ALeve: Word);
begin
  FWordDoc.Paragraphs.Item(1).Range.Font.Size := ALeve * 10;
end;

{ TTable }

procedure TTable.Add(ARow, ACol: Integer);
begin
  if (ARow > 0) and (ACol >0) then
  begin
   FTable := FDocument.Tables.Add(FDocument.Words.Last, ARow, ACol, EmptyParam, EmptyParam);
  end;
end;

procedure TTable.AddCol(ACol: Integer);
begin
  FTable.Columns.Add(EmptyParam);
end;

procedure TTable.AddRow(ARow: Integer);
begin
  try
    if not Assigned(FTable) then
      Exit;
    FTable.Rows.Add(EmptyParam);
  except
  end;
end;

procedure TTable.CellAddText(ARow, ACol: Integer; const AText: string);
begin
  if not Assigned(FTable) then
    Exit;
  AddText(FTable.Cell(ARow, ACol).Range, AText);
end;

procedure TTable.CellDelText(ARow, ACol: Integer; const ADelText: string);
begin
  if not Assigned(FTable) then
    Exit;
  DelText(FTable.Cell(ARow, ACol).Range, '')
end;

procedure TTable.CellEditText(ARow, ACol: Integer; const ADelText, ANewText: string);
begin
  if not Assigned(FTable) then
    Exit;
  EditText(FTable.Cell(ARow, ACol).Range, ADelText, ANewText);
end;

function TTable.CellRange(ARow, ACol: Integer): Range;
begin
  Result := FTable.Cell(ARow, ACol).Range;
end;

constructor TTable.Create(ADocument: TWordDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

procedure TTable.DelCol(ACol: Integer);
begin
  if not Assigned(FTable) then
   Exit;
  FTable.Columns.Item(ACol).Delete;
end;

procedure TTable.DelRow(ARow: Integer);
begin
  if not Assigned(FTable) then
   Exit;
  FTable.Rows.Item(ARow).Delete;
end;

destructor TTable.Destroy;
begin
  inherited;
end;

{ TCommOper }

procedure TImage.AddImage(ARange: Range; const AFileName: string; AWidth: Integer = 60; AHeight: Integer= 60);
var
  pic, LinkToFile, SaveWithDocument, _Range: OleVariant;
begin
  LinkToFile := False;
  SaveWithDocument := True;
  _Range := ARange;
  pic := ARange.InlineShapes.AddPicture(AFileName, LinkToFile, SaveWithDocument, _Range);
  pic.Width := AWidth;
  pic.Height := AHeight;
end;

procedure TCommOper.AddText(ARange: Range; const AText: string);
begin
  ARange.InsertAfter(AText);
end;

procedure TImage.DelImage(ARange: Range; Index: Integer);
begin
  if (Index > 0) and (Index <= ARange.InlineShapes.Count) then
   ARange.InlineShapes.Item(Index).Delete;
end;

procedure TCommOper.DelText(ARange: Range; const ADelText: string);
begin
  ARange.Text := '';
end;

procedure TCommOper.EditText(ARange: Range; const ADelText, ANewText: string);
var
  FindText, MatchCase, MatchWholeWord, MatchWildcards, MatchSoundsLike,
  MatchAllWordForms, Forwd, Wrap, Format, ReplaceWith, Replace: OleVariant;
begin
  FindText := ADelText;
  MatchCase := False;
  MatchWholeWord := false;
  MatchWildcards := False;
  MatchSoundsLike := False;
  MatchAllWordForms := False;
  Forwd := True;
  Wrap := wdFindContinue;
  Format := False;
  ReplaceWith := ANewText;
  Replace := True;

  ARange.Find.Execute(FindText, MatchCase, MatchWholeWord, MatchWildcards,
    MatchSoundsLike, MatchAllWordForms, Forwd, Wrap, Format, ReplaceWith, Replace,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TImage.SetImageSize(ARange: Range; Index: Integer; AWidth, AHeight: Integer);
begin
  if ARange.InlineShapes.Count < 1 then
    Exit;
  if (AWidth > 0) and (AHeight > 0) then
  begin
    ARange.InlineShapes.Item(Index).Width := AWidth;
    ARange.InlineShapes.Item(Index).Width := AHeight;
  end;
end;

{ TDrawLine }

constructor TDrawLine.Create(ADocument: TWordDocument);
begin
  FDocument := ADocument;
end;

procedure TDrawLine.Draw(ADrawType: TDrawType);
var
  _Range: OleVariant;
begin
  _Range := FDocument.Range;
  case ADrawType of
    dtRang:                            //  默认大小
      FShap:= FDocument.Shapes.Addshape(1, 50, 50, 100, 200, _Range);
    dtEllise:
      FShap:= FDocument.Shapes.Addshape(9, 50, 50, 100, 200, _Range);
  end;
end;

procedure TDrawLine.SetDashStyle(AStyle: LongWord);
begin
  FShap.Line.DashStyle := AStyle;
end;

procedure TDrawLine.SetFillColor(AFillColor: Integer);
begin
  FShap.Fill.ForeColor.RGB := AFillColor;
end;

procedure TDrawLine.SetFillStyle(AStyle: LongWord);
begin
  FShap.Fill.TwoColorGradient(AStyle, 1);
end;

procedure TDrawLine.SetForeColor(AColor: Integer);
begin
  FShap.Line.ForeColor.RGB := AColor;
end;

procedure TDrawLine.SetLineWeight(AWeight: Single);
begin
  FShap.Line.Weight := AWeight;
end;

end.
