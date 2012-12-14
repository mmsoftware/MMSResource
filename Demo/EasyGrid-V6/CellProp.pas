// *****************************************************************************
//
// Note: This free package of source code can only be used for reference and
//       learning purpose, you can distribute it freely, but please do not use
//       it for profit sake.
//
//       Special thanks to: RICHBBS (www.delphibbs.com)
//
//                                                         Huang Qian, Feb 2002
//                                                         Wuhan University
//
// *****************************************************************************

unit CellProp;

interface             

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TFormCellProp = class(TForm)
    PageCtlCellProp: TPageControl;
    TabSheetCellType: TTabSheet;
    TabSheetAlign: TTabSheet;
    TabSheetFont: TTabSheet;
    TabSheetOthers: TTabSheet;
    BtnOk: TButton;
    BtnCancel: TButton;
    LbxCellType: TListBox;
    LabelExample1: TLabel;
    BevelExample1: TBevel;
    MemoExample1: TMemo;
    LabelAlign: TLabel;
    BevelAlign: TBevel;
    LabelFontName: TLabel;
    LabelFrame: TLabel;
    BevelFrameStyle: TBevel;
    LabelHAlign: TLabel;
    LabelVAlign: TLabel;
    CbxHAlign: TComboBox;
    CbxVAlign: TComboBox;
    LabelControl: TLabel;
    BevelControl: TBevel;
    ChkBoxAutoWordBreak: TCheckBox;
    ChkBoxMerge: TCheckBox;
    LabelCellType: TLabel;
    BevelCellType: TBevel;
    LabelFontStyle: TLabel;
    LabelFontSize: TLabel;
    LbxFontName: TListBox;
    LbxFontStyle: TListBox;
    LbxFontSize: TListBox;
    LabelFontColor: TLabel;
    GbxFontPreview: TGroupBox;
    PanelFontPreview2: TPanel;
    PanelFontPreview1: TPanel;
    ChkBoxUnderLine: TCheckBox;
    ChkBoxDrawLeft: TCheckBox;
    ChkBoxDrawRight: TCheckBox;
    ChkBoxDrawTop: TCheckBox;
    ChkBoxDrawBottom: TCheckBox;
    LabelFrameSize: TLabel;
    EditFrameSize: TEdit;
    LabelPointSize: TLabel;
    LabelBackColor: TLabel;
    BevelBackColor: TBevel;
    LabelIntro1: TLabel;
    PanelFontColor: TPanel;
    BtnSetFontColor: TButton;
    BtnSetBackColor: TButton;
    PanelBackColor: TPanel;
    EditFontSize: TEdit;
    LabelMaxLength: TLabel;
    EditMaxLength: TEdit;
    LabelIntLength: TLabel;
    EditIntLength: TEdit;
    LabelDecLength: TLabel;
    EditDecLength: TEdit;
    ChkBoxAllowNeg: TCheckBox;
    ChkBoxThousandSep: TCheckBox;
    ChkBoxTrailingZero: TCheckBox;
    ChkBoxZeroNull: TCheckBox;
    LabelPenStyle: TLabel;
    CbxPenStyle: TComboBox;
    ColorDialogCellProp: TColorDialog;
    BevelReadOnly: TBevel;
    LabelReadOnly: TLabel;
    ChkBoxReadOnly: TCheckBox;
    procedure LbxCellTypeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LbxFontStyleClick(Sender: TObject);
    procedure LbxFontSizeClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure LbxFontNameClick(Sender: TObject);
    procedure ChkBoxUnderLineClick(Sender: TObject);
    procedure EditFontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure BtnSetFontColorClick(Sender: TObject);
    procedure BtnSetBackColorClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure CbxHAlignClick(Sender: TObject);
    procedure CbxVAlignClick(Sender: TObject);
    procedure EditFontSizeChange(Sender: TObject);
    procedure EditFrameSizeChange(Sender: TObject);
    procedure EditMaxLengthChange(Sender: TObject);
    procedure EditIntLengthChange(Sender: TObject);
    procedure EditDecLengthChange(Sender: TObject);
  private
    CellRange: TRect;
    procedure SetNumControls(Value: boolean);
    procedure GetFirstCellProp;
    procedure SetControlState;
  public
    ParentGrid: Pointer;
  end;

implementation
uses EasyGrid;
{$R *.DFM}

procedure TFormCellProp.LbxCellTypeClick(Sender: TObject);
begin
  with (Sender as TListBox) do
  begin
    case ItemIndex of
      0 :
        begin
          MemoExample1.Clear;
          MemoExample1.Lines.Add('中文字符');
          MemoExample1.Lines.Add('12345678');
          MemoExample1.Lines.Add('ABCDEFG HIJKLMN');
          MemoExample1.Lines.Add(',.<>!@#$%^&*()');
          LabelIntro1.Caption := '常规类型的单元格可以输入任意字符,而不作特殊的处理。';
          SetNumControls(False);
        end;
      1 :
        begin
          MemoExample1.Clear;
          MemoExample1.Lines.Add('123');
          MemoExample1.Lines.Add('12345.6789');
          MemoExample1.Lines.Add('-98765.4321');
          MemoExample1.Lines.Add('0.0005');
          LabelIntro1.Caption := '数值类型的单元格用于一般数字的显示和输入。';
          SetNumControls(True);
        end;
      2 :
        begin
          MemoExample1.Clear;
          MemoExample1.Lines.Add('1999-01-01');
          LabelIntro1.Caption := '日期类型的单元格允许按日期格式显示和输入内容。';
          SetNumControls(False);
        end;
      3 :
        begin
          MemoExample1.Clear;
          MemoExample1.Lines.Add('8:30');
          MemoExample1.Lines.Add('00:00:01');
          MemoExample1.Lines.Add('12:00:00');
          MemoExample1.Lines.Add('23:59:59');
          LabelIntro1.Caption := '时间类型的单元格允许按时间格式显示和输入内容。';
          SetNumControls(False);
        end;
    end;
  end;
end;

procedure TFormCellProp.SetNumControls(Value: boolean);
begin
  LabelMaxLength.Visible := Value;
  EditMaxLength.Visible := Value;

  LabelIntLength.Visible := Value;
  EditIntLength.Visible := Value;

  LabelDecLength.Visible := Value;
  EditDecLength.Visible := Value;

  ChkBoxAllowNeg.Visible := Value;
  ChkBoxThousandSep.Visible := Value;
  ChkBoxTrailingZero.Visible := Value;
  ChkBoxZeroNull.Visible := Value;
end;

procedure TFormCellProp.GetFirstCellProp;
var
  ACellInfo: PCellInfo;
begin
  with TEasyGrid(ParentGrid) do
    ACellInfo := Cells[Col, Row];
  with ACellInfo^, TEasyGrid(ParentGrid) do
  begin
    // 第一页上的控件
    LbxCellType.ItemIndex := Ord(DataStyle);
    LbxCellTypeClick(LbxCellType);
    EditMaxLength.Text := IntToStr(MaxLength);
    EditIntLength.Text := IntToStr(IntLength);
    EditDecLength.Text := IntToStr(DecLength);
    ChkBoxAllowNeg.Checked := AllowNegative;
    ChkBoxThousandSep.Checked := ThousandSep;
    ChkBoxTrailingZero.Checked := TrailingZero;
    ChkBoxZeroNull.Checked := ZeroNull;
    // 第二页上的控件
    ChkBoxAutoWordBreak.Checked := AutoWordBreak;
    ChkBoxMerge.Checked :=
      ((Merges[Col, Row].Left <> Merges[Col, Row].Right) or
       (Merges[Col, Row].Top  <> Merges[Col, Row].Bottom));
    case AlignMode of
      taTopLeft, taLeft, taBottomLeft :
        CbxHAlign.ItemIndex := 0;
      taTop, taCenter, taBottom :
        CbxHAlign.ItemIndex := 1;
      taTopRight, taRight, taBottomRight :
        CbxHAlign.ItemIndex := 2;
    end;
    case AlignMode of
      taTopLeft, taTop, taTopRight :
        CbxVAlign.ItemIndex := 0;
      taLeft, taCenter, taRight :
        CbxVAlign.ItemIndex := 1;
      taBottomLeft, taBottom, taBottomRight :
        CbxVAlign.ItemIndex := 2;
    end;
    // 第三页上的控件
    with LbxFontName do
    begin
      ItemIndex := Items.IndexOf(FontName);
      if (ItemIndex >= 0) then
        LbxFontNameClick(LbxFontName);
    end;
    with LbxFontStyle do
    begin
      ItemIndex := -1;
      if FontStyle = [] then
        ItemIndex := 0
      else if FontStyle = [fsItalic] then
        ItemIndex := 1
      else if FontStyle = [fsBold] then
        ItemIndex := 2
      else if FontStyle = [fsBold,fsItalic] then
        ItemIndex := 3;
      if ItemIndex >= 0 then
        LbxFontStyleClick(LbxFontStyle);
    end;
    with LbxFontSize do
    begin
      ItemIndex := Items.IndexOf(IntToStr(FontSize));
      if ItemIndex >= 0 then
        LbxFontSizeClick(LbxFontSize);
    end;
    ChkBoxUnderLine.Checked := (fsUnderLine in FontStyle);
    PanelFontColor.Color := FontColor;
    PanelFontColor.Visible := True;
    with PanelFontPreview1.Font do
    begin
      Name :=  FontName;
      Size :=  FontSize;
      Style := FontStyle;
      Color := FontColor;
    end;
    // 第四页上的控件
    ChkBoxDrawLeft.Checked := DrawLeft;
    ChkBoxDrawTop.Checked := DrawTop;
    ChkBoxDrawRight.Checked := DrawRight;
    ChkBoxDrawBottom.Checked := DrawBottom;
    EditFrameSize.Text := IntToStr(LineWidth);
    CbxPenStyle.ItemIndex := Ord(PenStyle);
    PanelBackColor.Color := ACellInfo.Color;
    PanelBackColor.Visible := True;
    ChkBoxReadOnly.Checked := ReadOnly;
  end;
end;

procedure TFormCellProp.SetControlState;
var
  ACellInfo: PCellInfo;
  ACol, ARow: integer;
  NewCellTypeIndex, NewHAlignIndex, NewVAlignIndex: integer;
  NewFontNameIndex, NewFontStyleIndex, NewPenStyleIndex: integer;
  CellTypeVary, MaxLengthVary, IntLengthVary, DecLengthVary: boolean;
  AllowNegVary, ThousandSepVary, TrailingZeroVary, ZeroNullVary: boolean;
  HAlignVary, VAlignVary: boolean;
  AutoWordBreakVary, MergeVary, UnderLineVary: boolean;
  FontNameVary, FontStyleVary, FontSizeVary, FontColorVary: boolean;
  DrawLeftVary, DrawTopVary, DrawRightVary, DrawBottomVary: boolean;
  FrameSizeVary, PenStyleVary, BackColorVary, ReadOnlyVary: boolean;
begin
  CellTypeVary := False;
  MaxLengthVary := False;
  IntLengthVary := False;
  DecLengthVary := False;
  AllowNegVary := False;
  ThousandSepVary := False;
  TrailingZeroVary := False;
  ZeroNullVary := False;
  HAlignVary := False;
  VAlignVary := False;
  AutoWordBreakVary := False;
  MergeVary := False;
  UnderLineVary := False;
  FontNameVary := False;
  FontStyleVary := False;
  FontSizeVary := False;
  FontColorVary := False;
  DrawLeftVary := False;
  DrawTopVary := False;
  DrawRightVary := False;
  DrawBottomVary := False;
  FrameSizeVary := False;
  PenStyleVary := False;
  BackColorVary := False;
  ReadOnlyVary := False;
  for ACol:=CellRange.Left to CellRange.Right do
    for ARow:=CellRange.Top to CellRange.Bottom do
    begin
      ACellInfo := TEasyGrid(ParentGrid).Cells[ACol, ARow];
      with ACellInfo^ do
      begin
        // 第一页上的控件
        NewCellTypeIndex := Ord(DataStyle);
        if (not CellTypeVary) and (NewCellTypeIndex <> LbxCellType.ItemIndex) then
          CellTypeVary := True;
        if (not MaxLengthVary) and (MaxLength <> StrToInt(EditMaxLength.Text)) then
          MaxLengthVary := True;
        if (not IntLengthVary) and (IntLength <> StrToInt(EditIntLength.Text)) then
          IntLengthVary := True;
        if (not DecLengthVary) and (DecLength <> StrToInt(EditDecLength.Text)) then
          DecLengthVary := True;
        if (not AllowNegVary) and (AllowNegative <> ChkBoxAllowNeg.Checked) then
          AllowNegVary := True;
        if (not ThousandSepVary) and (ThousandSep <> ChkBoxThousandSep.Checked) then
          ThousandSepVary := True;
        if (not TrailingZeroVary) and (TrailingZero <> ChkBoxTrailingZero.Checked) then
           TrailingZeroVary := True;
        if (not ZeroNullVary) and (ZeroNull <> ChkBoxZeroNull.Checked) then
          ZeroNullVary := True;
        // 第二页上的控件
        if not HAlignVary then
        begin
          NewHAlignIndex := -1;
          case AlignMode of
            taTopLeft, taLeft, taBottomLeft :
              NewHAlignIndex := 0;
            taTop, taCenter, taBottom :
              NewHAlignIndex := 1;
            taTopRight, taRight, taBottomRight :
              NewHAlignIndex := 2;
          end;
          if NewHAlignIndex <> CbxHAlign.ItemIndex then
            HAlignVary := True;
        end;
        if not VAlignVary then
        begin
          NewVAlignIndex := -1;
          case AlignMode of
            taTopLeft, taTop, taTopRight :
              NewVAlignIndex := 0;
            taLeft, taCenter, taRight :
              NewVAlignIndex := 1;
            taBottomLeft, taBottom, taBottomRight :
              NewVAlignIndex := 2;
          end;
          if NewVAlignIndex <> CbxVAlign.ItemIndex then
            VAlignVary := True;
        end;
        if (not AutoWordBreakVary) and (AutoWordBreak <> ChkBoxAutoWordBreak.Checked) then
          AutoWordBreakVary := True;
        if (not MergeVary) then
        begin
          with TEasyGrid(ParentGrid) do
            if ((Merges[ACol, ARow].Left <> Merges[Col, Row].Left) or
                (Merges[ACol, ARow].Top <> Merges[Col, Row].Top) or
                (Merges[ACol, ARow].Right <> Merges[Col, Row].Right) or
                (Merges[ACol, ARow].Bottom <> Merges[Col, Row].Bottom)) then
               MergeVary := True;
        end;
        // 第三页上的控件
        if (not FontNameVary) then
        begin
          NewFontNameIndex := LbxFontName.Items.IndexOf(FontName);
          if NewFontNameIndex <> LbxFontName.ItemIndex then
            FontNameVary := True;
        end;
        if (not FontStyleVary) then
          with LbxFontStyle do
          begin
            NewFontStyleIndex := -1;
            if FontStyle = [] then
              NewFontStyleIndex := 0
            else if FontStyle = [fsItalic] then
              NewFontStyleIndex := 1
            else if FontStyle = [fsBold] then
              NewFontStyleIndex := 2
            else if FontStyle = [fsBold,fsItalic] then
              NewFontStyleIndex := 3;
            if NewFontStyleIndex <> ItemIndex then
              FontStyleVary := True;
          end;
        if (not FontSizeVary) and (FontSize <> StrToInt(EditFontSize.Text)) then
          FontSizeVary := True;
        if (not FontColorVary) and (FontColor <> PanelFontColor.Color) then
          FontColorVary := True;
        if (not UnderLineVary) and ((fsUnderLine in FontStyle) xor ChkBoxUnderLine.Checked) then
          UnderLineVary := True;
        // 第四页上的控件
        if (not DrawLeftVary) and (DrawLeft <> ChkBoxDrawLeft.Checked) then
          DrawLeftVary := True;
        if (not DrawTopVary) and (DrawTop <> ChkBoxDrawTop.Checked) then
          DrawTopVary := True;
        if (not DrawRightVary) and (DrawRight <> ChkBoxDrawRight.Checked) then
          DrawRightVary := True;
        if (not DrawBottomVary) and (DrawBottom <> ChkBoxDrawBottom.Checked) then
          DrawBottomVary := True;
        if (not FrameSizeVary) and (LineWidth <> StrToInt(EditFrameSize.Text)) then
          FrameSizeVary := True;
        NewPenStyleIndex := Ord(PenStyle);
        if (not PenStyleVary) and (NewPenStyleIndex <> CbxPenStyle.ItemIndex) then
          PenStyleVary := True;
        if (not BackColorVary) and (Color <> PanelBackColor.Color) then
          BackColorVary := True;
        if (not ReadOnlyVary) and (ReadOnly <> ChkBoxReadOnly.Checked) then
          ReadOnlyVary := True;
      end;
    end;
  // 第一页上的控件
  if CellTypeVary then
    LbxCellType.ItemIndex := -1;

  if MaxLengthVary then
    EditMaxLength.Text := '';
  if IntLengthVary then
    EditIntLength.Text := '';
  if DecLengthVary then
    EditDecLength.Text := '';
  ChkBoxAllowNeg.AllowGrayed := False;
  if AllowNegVary then
    begin
      ChkBoxAllowNeg.AllowGrayed := True;
      ChkBoxAllowNeg.State := cbGrayed;
    end;
  ChkBoxThousandSep.AllowGrayed := False;
  if ThousandSepVary then
    begin
      ChkBoxThousandSep.AllowGrayed := True;
      ChkBoxThousandSep.State := cbGrayed;
    end;
  ChkBoxTrailingZero.AllowGrayed := False;
  if TrailingZeroVary then
    begin
      ChkBoxTrailingZero.AllowGrayed := True;
      ChkBoxTrailingZero.State := cbGrayed;
    end;
  ChkBoxZeroNull.AllowGrayed := False;
  if ZeroNullVary then
    begin
      ChkBoxZeroNull.AllowGrayed := True;
      ChkBoxZeroNull.State := cbGrayed;
    end;

  // 第二页上的控件
  if HAlignVary then
    CbxHAlign.ItemIndex := -1;
  if VAlignVary then
    CbxVAlign.ItemIndex := -1;
  ChkBoxAutoWordBreak.AllowGrayed := False;
  if AutoWordBreakVary then
    begin
      ChkBoxAutoWordBreak.AllowGrayed := True;
      ChkBoxAutoWordBreak.State := cbGrayed;
    end;
  ChkBoxMerge.AllowGrayed := False;
  if MergeVary then
    begin
      ChkBoxMerge.AllowGrayed := True;
      ChkBoxMerge.State := cbGrayed;
    end;

  // 第三页上的控件
  if FontNameVary then
    LbxFontName.ItemIndex := -1;
  if FontStyleVary then
    LbxFontStyle.ItemIndex := -1;
  if FontSizeVary then
  begin
    LbxFontSize.ItemIndex := -1;
    EditFontSize.Text := '';
  end;
  if FontColorVary then
    PanelFontColor.Visible := False;
  ChkBoxUnderLine.AllowGrayed := False;
  if UnderLineVary then
    begin
      ChkBoxUnderLine.AllowGrayed := True;
      ChkBoxUnderLine.State := cbGrayed;
    end;
    
  // 第四页上的控件
  ChkBoxDrawLeft.AllowGrayed := False;
  if DrawLeftVary then
    begin
      ChkBoxDrawLeft.AllowGrayed := True;
      ChkBoxDrawLeft.State := cbGrayed;
    end;
  ChkBoxDrawTop.AllowGrayed := False;
  if DrawTopVary then
    begin
      ChkBoxDrawTop.AllowGrayed := True;
      ChkBoxDrawTop.State := cbGrayed;
    end;
  ChkBoxDrawRight.AllowGrayed := False;
  if DrawRightVary then
    begin
      ChkBoxDrawRight.AllowGrayed := True;
      ChkBoxDrawRight.State := cbGrayed;
    end;
  ChkBoxDrawBottom.AllowGrayed := False;
  if DrawBottomVary then
    begin
      ChkBoxDrawBottom.AllowGrayed := True;
      ChkBoxDrawBottom.State := cbGrayed;
    end;
  if FrameSizeVary then
    EditFrameSize.Text := '';
  if PenStyleVary then
    CbxPenStyle.ItemIndex := -1;
  if BackColorVary then
    PanelBackColor.Visible := False;
  ChkBoxReadOnly.AllowGrayed := False;
  if ReadOnlyVary then
    begin
      ChkBoxReadOnly.AllowGrayed := True;
      ChkBoxReadOnly.State := cbGrayed;
    end;
end;

procedure TFormCellProp.FormShow(Sender: TObject);
begin
  GetFirstCellProp;
  CellRange := TRect(TEasyGrid(ParentGrid).Selection);
  SetControlState;
end;

procedure TFormCellProp.FormCreate(Sender: TObject);
begin
  LbxFontName.Items.Assign(Screen.Fonts);
end;

procedure TFormCellProp.LbxFontNameClick(Sender: TObject);
begin
  with LbxFontName do
    PanelFontPreview1.Font.Name := Items[ItemIndex];
end;

procedure TFormCellProp.LbxFontStyleClick(Sender: TObject);
begin
  case LbxFontStyle.ItemIndex of
    0 : PanelFontPreview1.Font.Style := [];
    1 : PanelFontPreview1.Font.Style := [fsItalic];
    2 : PanelFontPreview1.Font.Style := [fsBold];
    3 : PanelFontPreview1.Font.Style := [fsBold, fsItalic];
  end;
end;

procedure TFormCellProp.LbxFontSizeClick(Sender: TObject);
begin
  with LbxFontSize do
  begin
    EditFontSize.Text := Items[ItemIndex];
    PanelFontPreview1.Font.Size := StrToInt(EditFontSize.Text);
  end;
end;

procedure TFormCellProp.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  TEdit((Sender as TUpDown).Associate).Enabled := True;
end;

procedure TFormCellProp.ChkBoxUnderLineClick(Sender: TObject);
begin
  if ChkBoxUnderLine.Checked then
    PanelFontPreview1.Font.Style := PanelFontPreview1.Font.Style + [fsUnderLine]
  else
    PanelFontPreview1.Font.Style := PanelFontPreview1.Font.Style - [fsUnderLine];
end;

procedure TFormCellProp.EditFontSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (integer(Key) in [$30..$39,VK_BACK,VK_INSERT,VK_END,VK_HOME]) then
    Key := #0;
end;

procedure TFormCellProp.EditFontSizeChange(Sender: TObject);
begin
  if (EditFontSize.Text <> '') and (StrToInt(EditFontSize.Text) > 409) then
    EditFontSize.Text := '409';
  if EditFontSize.Text = '' then
    PanelFontPreview1.Font.Size := 1
  else
    PanelFontPreview1.Font.Size := StrToInt(EditFontSize.Text);
end;

procedure TFormCellProp.BtnSetFontColorClick(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    PanelFontPreview1.Font.Color := ColorDialogCellProp.Color;
    PanelFontColor.Color := ColorDialogCellProp.Color;
    PanelFontColor.Visible := True;
  end;
end;

procedure TFormCellProp.BtnSetBackColorClick(Sender: TObject);
begin
  if ColorDialogCellProp.Execute then
  begin
    PanelBackColor.Color := ColorDialogCellProp.Color;
    PanelBackColor.Visible := True;
  end;
end;

procedure TFormCellProp.CbxHAlignClick(Sender: TObject);
begin
  if CbxVAlign.ItemIndex < 0 then
    CbxVAlign.ItemIndex := 0;
end;

procedure TFormCellProp.CbxVAlignClick(Sender: TObject);
begin
  if CbxHAlign.ItemIndex < 0 then
    CbxHAlign.ItemIndex := 0;
end;

procedure TFormCellProp.BtnOkClick(Sender: TObject);
var
  NewAlignMode: TAlignMode;
  NewFontStyle: TFontStyles;
  i, j: Integer;
  AText: string;
  CanSetCellProp: Boolean;
begin
  CanSetCellProp := True;
  if Assigned(TEasyGrid(ParentGrid).BeforeSetCellProp) then
    TEasyGrid(ParentGrid).BeforeSetCellProp(ParentGrid, CellRange, CanSetCellProp);
  if not CanSetCellProp then Exit;
  // 暂时禁止刷新
  TEasyGrid(ParentGrid).AutoUpdate := False;
  // 第一页上的控件
  if EditMaxLength.Text <> '' then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnMaxLength, Pointer(StrToInt(EditMaxLength.Text)));
  if EditIntLength.Text <> '' then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnIntLength, Pointer(StrToInt(EditIntLength.Text)));
  if EditDecLength.Text <> '' then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnDecLength, Pointer(StrToInt(EditDecLength.Text)));
  if not (ChkBoxAllowNeg.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnAllowNegative, Pointer(ChkBoxAllowNeg.Checked));
  if not (ChkBoxThousandSep.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnThousandSep, Pointer(ChkBoxThousandSep.Checked));
  if not (ChkBoxTrailingZero.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnTrailingZero, Pointer(ChkBoxTrailingZero.Checked));
  if not (ChkBoxZeroNull.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnZeroNull, Pointer(ChkBoxZeroNull.Checked));
  if LbxCellType.ItemIndex >= 0 then
  begin
    // 改变网格类型时要清空原来的内容,否则会引起数据类型转换错误
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnDataStyle, Pointer(LbxCellType.ItemIndex));
    with TEasyGrid(ParentGrid) do
    for i:=CellRange.Left to CellRange.Right do
      for j:=CellRange.Top to CellRange.Bottom do
      begin
        AText := Cells[i, j].ForeText;
        Cells[i, j].ForeText := '';
        ForeTexts[i, j] := AText;
      end;
  end;
  // 第二页上的控件
  if (CbxHAlign.ItemIndex >= 0) and (CbxVAlign.ItemIndex >= 0) then
  begin
    NewAlignMode := TEasyGrid(ParentGrid).CellAlignMode;
    case CbxHAlign.ItemIndex of
      0 :
        case CbxVAlign.ItemIndex of
          0 :
            NewAlignMode := taTopLeft;
          1 :
            NewAlignMode := taLeft;
          2 :
            NewAlignMode := taBottomLeft;
        end;
      1 :
        case CbxVAlign.ItemIndex of
          0 :
            NewAlignMode := taTop;
          1 :
            NewAlignMode := taCenter;
          2 :
            NewAlignMode := taBottom;
        end;
      2 :
        case CbxVAlign.ItemIndex of
          0 :
            NewAlignMode := taTopRight;
          1 :
            NewAlignMode := taRight;
          2 :
            NewAlignMode := taBottomRight;
        end;
    end;
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnAlignMode, Pointer(NewAlignMode));
  end;
  if not (ChkBoxAutoWordBreak.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnAutoWordBreak, Pointer(ChkBoxAutoWordBreak.Checked));
  if not (ChkBoxMerge.State = cbGrayed) then
    if ChkBoxMerge.Checked then
      TEasyGrid(ParentGrid).SetMerges(CellRange, True)
    else
      TEasyGrid(ParentGrid).DeleteMerges(CellRange);
  // 第三页上的控件
  with LbxFontName do
  if (ItemIndex >= 0) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnFontName, Pointer(Items[ItemIndex]));
  with LbxFontStyle do
  if (ItemIndex >= 0) then
  begin
    NewFontStyle := [];
    case ItemIndex of
      1 : NewFontStyle := [fsItalic];
      2 : NewFontStyle := [fsBold];
      3 : NewFontStyle := [fsBold, fsItalic];
    end;
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnFontStyle, @NewFontStyle);
  end;
  if (ChkBoxUnderLine.State <> cbGrayed) then
  with TEasyGrid(ParentGrid) do
  for i:=CellRange.Left to CellRange.Right do
    for j:=CellRange.Top to CellRange.Bottom do
      if ChkBoxUnderLine.Checked then
        Cells[i ,j].FontStyle := Cells[i ,j].FontStyle + [fsUnderLine]
      else
        Cells[i ,j].FontStyle := Cells[i ,j].FontStyle - [fsUnderLine];
  if EditFontSize.Text <> '' then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnFontSize, Pointer(StrToInt(EditFontSize.Text)));
  if PanelFontColor.Visible then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnFontColor, Pointer(PanelFontColor.Color));
  // 第四页上的控件
  if not (ChkBoxDrawLeft.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnDrawLeft, Pointer(ChkBoxDrawLeft.Checked));
  if not (ChkBoxDrawTop.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnDrawTop, Pointer(ChkBoxDrawTop.Checked));
  if not (ChkBoxDrawRight.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnDrawRight, Pointer(ChkBoxDrawRight.Checked));
  if not (ChkBoxDrawBottom.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnDrawBottom, Pointer(ChkBoxDrawBottom.Checked));
  if EditFrameSize.Text <> '' then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnLineWidth, Pointer(StrToInt(EditFrameSize.Text)));
  with CbxPenStyle do
  if ItemIndex >= 0 then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnPenStyle, Pointer(ItemIndex));
  if PanelBackColor.Visible then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnColor, Pointer(PanelBackColor.Color));
  if not (ChkBoxReadOnly.State = cbGrayed) then
    TEasyGrid(ParentGrid).SetRangeProperty(CellRange, pnReadOnly, Pointer(ChkBoxReadOnly.Checked));
  // 刷新网格
  TEasyGrid(ParentGrid).AutoUpdate := True;
end;

procedure TFormCellProp.EditFrameSizeChange(Sender: TObject);
begin
  if (EditFrameSize.Text <> '') and (StrToInt(EditFrameSize.Text) > 5) then
    EditFrameSize.Text := '5';
end;

procedure TFormCellProp.EditMaxLengthChange(Sender: TObject);
begin
  if (EditMaxLength.Text <> '') and (StrToInt(EditMaxLength.Text) > 255) then
    EditMaxLength.Text := '255';
end;

procedure TFormCellProp.EditIntLengthChange(Sender: TObject);
begin
  if (EditIntLength.Text <> '') and (StrToInt(EditIntLength.Text) > 255) then
    EditIntLength.Text := '255';
end;

procedure TFormCellProp.EditDecLengthChange(Sender: TObject);
begin
  if (EditDecLength.Text <> '') and (StrToInt(EditDecLength.Text) > 255) then
    EditDecLength.Text := '255';
end;

end.
