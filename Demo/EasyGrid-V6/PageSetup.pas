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

unit PageSetup;       

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Spin, Buttons, Printers, PreviewBox;

type
  TFormPageSetup = class(TForm)
    PageCtlPageSetup: TPageControl;
    TabSheetPageInfo: TTabSheet;
    LabelDirection: TLabel;
    BevelDirection: TBevel;
    LabelStartPageNo: TLabel;
    TabSheetMargin: TTabSheet;
    LabelMargin: TLabel;
    BevelMargin: TBevel;
    LabelAlign: TLabel;
    BevelAlign: TBevel;
    ChkBoxHorzCenter: TCheckBox;
    ChkBoxVertCenter: TCheckBox;
    TabSheetHF: TTabSheet;
    TabSheetTitle: TTabSheet;
    LabelTitle: TLabel;
    BevelTitle: TBevel;
    BtnOk: TButton;
    BtnCancel: TButton;
    ImageVertPrint: TImage;
    RBtnVPrint: TRadioButton;
    ImageHorzPrint: TImage;
    RBtnHPrint: TRadioButton;
    LabelScale: TLabel;
    BevelScale: TBevel;
    LabelScalePercent: TLabel;
    SEditScale: TSpinEdit;
    BevelPageNo: TBevel;
    LabelPageNo: TLabel;
    LabelTMargin: TLabel;
    SEditTMargin: TSpinEdit;
    LabelBMargin: TLabel;
    SEditBMargin: TSpinEdit;
    LabelLMargin: TLabel;
    SEditLMargin: TSpinEdit;
    LabelRMargin: TLabel;
    SEditRMargin: TSpinEdit;
    LabelHSize: TLabel;
    SEditHeaderSize: TSpinEdit;
    LabelFSize: TLabel;
    SEditFooterSize: TSpinEdit;
    LabelHF: TLabel;
    BevelHF: TBevel;
    GbxPageRange: TGroupBox;
    RBtnPrintAll: TRadioButton;
    RBtnPageRange: TRadioButton;
    LabelFromPage: TLabel;
    LabelToPage: TLabel;
    SEditFromPage: TSpinEdit;
    SEditToPage: TSpinEdit;
    TabSheetGrid: TTabSheet;
    GbxHeader: TGroupBox;
    CbxHeaderType: TComboBox;
    EditHeader1: TEdit;
    EditHeader2: TEdit;
    EditHeader3: TEdit;
    LabelHeader1: TLabel;
    LabelHeader2: TLabel;
    LabelHeader3: TLabel;
    LabelHeaderType: TLabel;
    GbxFooter: TGroupBox;
    LabelFooter1: TLabel;
    LabelFooter2: TLabel;
    LabelFooter3: TLabel;
    LabelFooterType: TLabel;
    CbxFooterType: TComboBox;
    EditFooter1: TEdit;
    EditFooter2: TEdit;
    EditFooter3: TEdit;
    LbxTitleType: TListBox;
    LabelHLineStyle: TLabel;
    CbxHLineStyle: TComboBox;
    LabelHLineWidth: TLabel;
    SEditHLineWidth: TSpinEdit;
    ChkBoxHDoubleLine: TCheckBox;
    LabelFLineStyle: TLabel;
    CbxFLineStyle: TComboBox;
    LabelFLineWidth: TLabel;
    SEditFLineWidth: TSpinEdit;
    ChkBoxFDoubleLine: TCheckBox;
    LabelTitleDistance: TLabel;
    SEditTitleDistance: TSpinEdit;
    LabelTitleContent: TLabel;
    LabelTitleFont: TLabel;
    LabelTail: TLabel;
    BevelTail: TBevel;
    LbxTailType: TListBox;
    LabelTailDistance: TLabel;
    SEditTailDistance: TSpinEdit;
    LabelTailContent: TLabel;
    LabelTailFont: TLabel;
    GbxSplit: TGroupBox;
    RBtnHorzSplit: TRadioButton;
    RBtnVertSplit: TRadioButton;
    ImageHorzSplit: TImage;
    ImageVertSplit: TImage;
    LabelGridInfo: TLabel;
    BevelGridInfo: TBevel;
    ChkBoxMonoColored: TCheckBox;
    ChkBoxPrintGridLine: TCheckBox;
    ChkBoxPrintColTitle: TCheckBox;
    ChkBoxPrintRowTitle: TCheckBox;
    ChkBoxPrintFixedCols: TCheckBox;
    ChkBoxPrintFixedRows: TCheckBox;
    GbxConjunction: TGroupBox;
    LabelTopConjunction: TLabel;
    EditTopConjunction: TEdit;
    LabelBottomConjunction: TLabel;
    EditBottomConjunction: TEdit;
    RBtnPrintConjunction: TRadioButton;
    RBtnNoConjunction: TRadioButton;
    BtnPrinter: TButton;
    FontDialogPageSetup: TFontDialog;
    SEditStartPageNo: TSpinEdit;
    BtnInsertPageNo: TSpeedButton;
    SBtnInsertPageCount: TSpeedButton;
    SBtnInsertDate: TSpeedButton;
    SBtnInsertTime: TSpeedButton;
    SEditFont: TSpeedButton;
    BtnTitleFont: TSpeedButton;
    MemoTitleContent: TMemo;
    BtnTailFont: TSpeedButton;
    MemoTailContent: TMemo;
    LabelTitleExtent: TLabel;
    SEditTitleExtent: TSpinEdit;
    BevelTitleContent: TBevel;
    BevelTailContent: TBevel;
    LabelTailExtent: TLabel;
    SEditTailExtent: TSpinEdit;
    procedure ImageHorzSplitClick(Sender: TObject);
    procedure ImageVertSplitClick(Sender: TObject);
    procedure ImageVertPrintClick(Sender: TObject);
    procedure ImageHorzPrintClick(Sender: TObject);
    procedure BtnPrinterClick(Sender: TObject);
    procedure RBtnVPrintClick(Sender: TObject);
    procedure RBtnHPrintClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbxTitleTypeClick(Sender: TObject);
    procedure LbxTailTypeClick(Sender: TObject);
    procedure CbxHeaderTypeChange(Sender: TObject);
    procedure CbxFooterTypeChange(Sender: TObject);
    procedure BtnInsertPageNoClick(Sender: TObject);
    procedure SBtnInsertPageCountClick(Sender: TObject);
    procedure SBtnInsertDateClick(Sender: TObject);
    procedure SBtnInsertTimeClick(Sender: TObject);
    procedure SEditFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoTitleContentChange(Sender: TObject);
    procedure MemoTailContentChange(Sender: TObject);
    procedure SEditTitleDistanceChange(Sender: TObject);
    procedure SEditTailDistanceChange(Sender: TObject);
    procedure BtnTitleFontClick(Sender: TObject);
    procedure BtnTailFontClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
  private
    { Private declarations }
    FMainTitle, FTitle1, FTitle2, FTitle3, FTitle4, FTitle5, FTitle6 : TTitle;
    FTail1, FTail2, FTail3, FTail4, FTail5, FTail6: TTitle;
    FTitle: TTitle;
    FTail: TTail;
    FPaperSize, FPageWidth, FPageHeight: Integer; 
    procedure InsertEditText(AText: string);
    procedure AssignEditProp(AHeader: Pointer; AEdit: TEdit);
  public
    { Public declarations }
    ParentGrid: Pointer;
  end;

implementation
uses EasyGrid;
{$R *.DFM}

procedure TFormPageSetup.FormCreate(Sender: TObject);
begin
  FMainTitle := TTitle.Create;
  FTitle1 := TTitle.Create;
  FTitle2 := TTitle.Create;
  FTitle3 := TTitle.Create;
  FTitle4 := TTitle.Create;
  FTitle5 := TTitle.Create;
  FTitle6 := TTitle.Create;
  FTail1 := TTail.Create;
  FTail2 := TTail.Create;
  FTail3 := TTail.Create;
  FTail4 := TTail.Create;
  FTail5 := TTail.Create;
  FTail6 := TTail.Create;
end;

procedure TFormPageSetup.FormDestroy(Sender: TObject);
begin
  FMainTitle.Free;
  FTitle1.Free;
  FTitle2.Free;
  FTitle3.Free;
  FTitle4.Free;
  FTitle5.Free;
  FTitle6.Free;
  FTail1.Free;
  FTail2.Free;
  FTail3.Free;
  FTail4.Free;
  FTail5.Free;
  FTail6.Free;
end;

procedure TFormPageSetup.ImageHorzSplitClick(Sender: TObject);
begin
  RBtnHorzSplit.Checked := True;
  RBtnHorzSplit.SetFocus;
end;

procedure TFormPageSetup.ImageVertSplitClick(Sender: TObject);
begin
  RBtnVertSplit.Checked := True;
  RBtnVertSplit.SetFocus;
end;

procedure TFormPageSetup.ImageVertPrintClick(Sender: TObject);
begin
  RBtnVPrint.Checked := True;
  RBtnVPrint.SetFocus;
end;

procedure TFormPageSetup.ImageHorzPrintClick(Sender: TObject);
begin
  RBtnHPrint.Checked := True;
  RBtnHPrint.SetFocus;
end;

procedure TFormPageSetup.BtnPrinterClick(Sender: TObject);
begin
  // 设置打印机纸张大小为指定的大小
  SetPaperSize(FPaperSize,
               Round(FPageHeight / ScreenToPrinterY * 10 / PrinterPixelsPerMmY),
               Round(FPageWidth / ScreenToPrinterX * 10 / PrinterPixelsPerMmX));
  if TEasyGrid(ParentGrid).PrinterSetup then
  begin
    RBtnHPrint.Checked := (Printer.Orientation = poLandscape);
    RBtnVPrint.Checked := (Printer.Orientation = poPortrait);
    GetPaperSize(FPaperSize, FPageHeight, FPageWidth);
    // 如果是自定义纸张,则取自定义尺寸
    if FPaperSize = 256 then
      begin
        FPageWidth := Round(FPageWidth / 10 * PrinterPixelsPerMmX * ScreenToPrinterX);
        FPageHeight := Round(FPageHeight / 10 * PrinterPixelsPerMmY * ScreenToPrinterY);
      end
    // 否则取预定义尺寸
    else
      begin
        FPageWidth := Round(Printer.PageWidth * ScreenToPrinterX);
        FPageHeight := Round(Printer.PageHeight * ScreenToPrinterY);
      end;
  end;
end;

procedure TFormPageSetup.RBtnVPrintClick(Sender: TObject);
begin
  Printer.Orientation := poPortrait;
  FPageWidth := Round(Printer.PageWidth * ScreenToPrinterX);
  FPageHeight := Round(Printer.PageHeight * ScreenToPrinterY);
end;

procedure TFormPageSetup.RBtnHPrintClick(Sender: TObject);
begin
  Printer.Orientation := poLandscape;
  FPageWidth := Round(Printer.PageWidth * ScreenToPrinterX);
  FPageHeight := Round(Printer.PageHeight * ScreenToPrinterY);
end;

procedure TFormPageSetup.FormShow(Sender: TObject);
var
  Grid: TEasyGrid;
begin
  Grid := TEasyGrid(ParentGrid);
  with Grid.EasyGridPageInfo.DetailPageInfo, Grid.EasyGridPageInfo.CommonPageInfo do
  begin
    // 第一页上的控件
    RBtnHPrint.Checked := (Orientation = poLandscape);
    RBtnVPrint.Checked := (Orientation = poPortrait);
    SEditScale.Value := Scale;
    SEditStartPageNo.Value := StartPageNo;
    RBtnPrintAll.Checked := PrintAllPages;
    RBtnPageRange.Checked := not PrintAllPages;
    SEditFromPage.Value := StartPage + 1;
    SEditToPage.Value := EndPage + 1;
    FPaperSize := PaperSize;
    FPageWidth := PageWidth;
    FPageHeight := PageHeight;
    // 第二页上的控件
    SEditTMargin.Value := Round(Margin.Top / ScreenPixelsPerMmY);
    SEditBMargin.Value := Round(Margin.Bottom / ScreenPixelsPerMmY);
    SEditLMargin.Value := Round(Margin.Left / ScreenPixelsPerMmX);
    SEditRMargin.Value := Round(Margin.Right / ScreenPixelsPerMmX);
    SEditHeaderSize.Value := Round(HeaderExtent / ScreenPixelsPerMmY);
    SEditFooterSize.Value := Round(FooterExtent / ScreenPixelsPerMmY);
    ChkBoxHorzCenter.Checked := HorzCenter;
    ChkBoxVertCenter.Checked := VertCenter;
    // 第三页上的控件
    CbxHeaderType.ItemIndex := 0;
    CbxHeaderTypeChange(Self);
    CbxFooterType.ItemIndex := 0;
    CbxFooterTypeChange(Self);
    // 第四页上的控件
    LbxTitleType.ItemIndex := 0;
    LbxTitleTypeClick(Self);
    LbxTailType.ItemIndex := 0;
    LbxTailTypeClick(Self);
    SEditTitleExtent.Value := Round(TitleExtent / ScreenPixelsPerMmY);
    SEditTailExtent.Value := Round(TailExtent / ScreenPixelsPerMmY);
    // 第五页上的控件
    RBtnHorzSplit.Checked := HorzSplit;
    RBtnVertSplit.Checked := not HorzSplit;
    RBtnPrintConjunction.Checked := PrintConjunction;
    RBtnNoConjunction.Checked := not PrintConjunction;
    EditTopConjunction.Text := TopConjunctionText;
    EditBottomConjunction.Text := BottomConjunctionText;
    ChkBoxMonoColored.Checked := MonoColored;
    ChkBoxPrintGridLine.Checked := PrintGridLine;
    ChkBoxPrintColTitle.Checked := PrintColTitle;
    ChkBoxPrintRowTitle.Checked := PrintRowTitle;
    ChkBoxPrintFixedCols.Checked := PrintFixedCols;
    ChkBoxPrintFixedRows.Checked := PrintFixedRows;

    FMainTitle.Assign(MainTitle);
    FTitle1.Assign(Title1);
    FTitle2.Assign(Title2);
    FTitle3.Assign(Title3);
    FTitle4.Assign(Title4);
    FTitle5.Assign(Title5);
    FTitle6.Assign(Title6);
    FTail1.Assign(Tail1);
    FTail2.Assign(Tail2);
    FTail3.Assign(Tail3);
    FTail4.Assign(Tail4);
    FTail5.Assign(Tail5);
    FTail6.Assign(Tail6);
  end;
end;

procedure TFormPageSetup.LbxTitleTypeClick(Sender: TObject);

  procedure GetTitleProp;
  begin
    with FTitle do
    begin
      MemoTitleContent.Clear;
      MemoTitleContent.Lines.Add(Content);
      MemoTitleContent.SelStart := 1;
      MemoTitleContent.SelLength := 0;
      MemoTitleContent.Font.Name := FontName;
      MemoTitleContent.Font.Size := FontSize;
      MemoTitleContent.Font.Style := FontStyle;
      MemoTitleContent.Font.Color := FontColor;
      SEditTitleDistance.Value := Round(Distance / ScreenPixelsPerMmY);
    end;
  end;

begin
  with LbxTitleType, TEasyGrid(ParentGrid).EasyGridPageInfo.CommonPageInfo do
  begin
    case ItemIndex of
      0 : FTitle := FMainTitle;
      1 : FTitle := FTitle1;
      2 : FTitle := FTitle2;
      3 : FTitle := FTitle3;
      4 : FTitle := FTitle4;
      5 : FTitle := FTitle5;
      6 : FTitle := FTitle6;
    end;
    GetTitleProp;
  end;
end;

procedure TFormPageSetup.LbxTailTypeClick(Sender: TObject);

  procedure GetTailProp;
  begin
    with FTail do
    begin
      MemoTailContent.Clear;
      MemoTailContent.Lines.Add(Content);
      MemoTailContent.SelStart := 1;
      MemoTailContent.SelLength := 0;
      MemoTailContent.Font.Name := FontName;
      MemoTailContent.Font.Size := FontSize;
      MemoTailContent.Font.Style := FontStyle;
      MemoTailContent.Font.Color := FontColor;
      SEditTailDistance.Value := Round(Distance / ScreenPixelsPerMmY);
    end;
  end;

begin
  with LbxTailType, TEasyGrid(ParentGrid).EasyGridPageInfo.CommonPageInfo do
  begin
    case ItemIndex of
      0 : FTail := FTail1;
      1 : FTail := FTail2;
      2 : FTail := FTail3;
      3 : FTail := FTail4;
      4 : FTail := FTail5;
      5 : FTail := FTail6;
    end;
    GetTailProp;
  end;
end;

procedure TFormPageSetup.CbxHeaderTypeChange(Sender: TObject);
var
  Grid: TEasyGrid;
begin
  Grid := TEasyGrid(ParentGrid);
  with CbxHeaderType, Grid.EasyGridPageInfo.CommonPageInfo do
  begin
    case ItemIndex of
      0 : // 自定义页眉
        begin
          CbxHLineStyle.ItemIndex := Ord(HeaderLineStyle);
          ChkBoxHDoubleLine.Checked := HeaderDoubleLine;
          SEditHLineWidth.Value := HeaderLineWidth;
          EditHeader1.Text := Header1.Content;
          EditHeader2.Text := Header2.Content;
          EditHeader3.Text := Header3.Content;
        end;
      1 :
        begin
          CbxHLineStyle.ItemIndex := 0;
          ChkBoxHDoubleLine.Checked := True;
          SEditHLineWidth.Value := 1;
          EditHeader1.Text := '';
          EditHeader2.Text := '第 &P 页';
          EditHeader3.Text := '';
        end;
      2 :
        begin
          CbxHLineStyle.ItemIndex := 0;
          ChkBoxHDoubleLine.Checked := True;
          SEditHLineWidth.Value := 1;
          EditHeader1.Text := '';
          EditHeader2.Text := '第 &P 页, 共 &C 页';
          EditHeader3.Text := '';
        end;
      3 :
        begin
          CbxHLineStyle.ItemIndex := 0;
          ChkBoxHDoubleLine.Checked := True;
          SEditHLineWidth.Value := 1;
          EditHeader1.Text := '报表1';
          EditHeader2.Text := '&D';
          EditHeader3.Text := '第 &P 页';
        end;
      4 :
        begin
          CbxHLineStyle.ItemIndex := 0;
          ChkBoxHDoubleLine.Checked := True;
          SEditHLineWidth.Value := 1;
          EditHeader1.Text := '报表1';
          EditHeader2.Text := '&T';
          EditHeader3.Text := '第 &P 页';
        end;
    end;
    AssignEditProp(Header1, EditHeader1);
    AssignEditProp(Header2, EditHeader2);
    AssignEditProp(Header3, EditHeader3);
  end;
end;

procedure TFormPageSetup.CbxFooterTypeChange(Sender: TObject);
var
  Grid: TEasyGrid;
begin
  Grid := TEasyGrid(ParentGrid);
  with CbxFooterType, Grid.EasyGridPageInfo.CommonPageInfo do
  begin
    case ItemIndex of
      0 : // 自定义页脚
        with Grid.EasyGridPageInfo.CommonPageInfo do
        begin
          CbxFLineStyle.ItemIndex := Ord(FooterLineStyle);
          ChkBoxFDoubleLine.Checked := FooterDoubleLine;
          SEditFLineWidth.Value := FooterLineWidth;
          EditFooter1.Text := Footer1.Content;
          EditFooter2.Text := Footer2.Content;
          EditFooter3.Text := Footer3.Content;
        end;
      1 :
        begin
          CbxFLineStyle.ItemIndex := 0;
          ChkBoxFDoubleLine.Checked := True;
          SEditFLineWidth.Value := 1;
          EditFooter1.Text := '';
          EditFooter2.Text := '第 &P 页';
          EditFooter3.Text := '';
        end;
      2 :
        begin
          CbxFLineStyle.ItemIndex := 0;
          ChkBoxFDoubleLine.Checked := True;
          SEditFLineWidth.Value := 1;
          EditFooter1.Text := '';
          EditFooter2.Text := '第 &P 页, 共 &C 页';
          EditFooter3.Text := '';
        end;
      3 :
        begin
          CbxFLineStyle.ItemIndex := 0;
          ChkBoxFDoubleLine.Checked := True;
          SEditFLineWidth.Value := 1;
          EditFooter1.Text := '报表1';
          EditFooter2.Text := '&D';
          EditFooter3.Text := '第 &P 页';
        end;
      4 :
        begin
          CbxFLineStyle.ItemIndex := 0;
          ChkBoxFDoubleLine.Checked := True;
          SEditFLineWidth.Value := 1;
          EditFooter1.Text := '报表1';
          EditFooter2.Text := '&T';
          EditFooter3.Text := '第 &P 页';
        end;
    end;
    AssignEditProp(Footer1, EditFooter1);
    AssignEditProp(Footer2, EditFooter2);
    AssignEditProp(Footer3, EditFooter3);
  end;
end;

procedure TFormPageSetup.InsertEditText(AText: string);
var
  OldSelStart: Integer;
begin
  if not (ActiveControl is TEdit) then Exit;
  with (ActiveControl as TEdit) do
  begin
    OldSelStart := SelStart;
    Text := Copy(Text, 1, SelStart) + AText + Copy(Text, SelStart + SelLength + 1, 255);
    SelStart := OldSelStart + Length(AText);
  end;
end;

procedure TFormPageSetup.BtnInsertPageNoClick(Sender: TObject);
begin
  InsertEditText('&P');
end;

procedure TFormPageSetup.SBtnInsertPageCountClick(Sender: TObject);
begin
  InsertEditText('&C');
end;

procedure TFormPageSetup.SBtnInsertDateClick(Sender: TObject);
begin
  InsertEditText('&D');
end;

procedure TFormPageSetup.SBtnInsertTimeClick(Sender: TObject);
begin
  InsertEditText('&T');
end;

procedure TFormPageSetup.AssignEditProp(AHeader: Pointer; AEdit: TEdit);
begin
  with THeader(AHeader) do
  begin
    AEdit.Font.Name := FontName;
    AEdit.Font.Size := FontSize;
    AEdit.Font.Style := FontStyle;
    AEdit.Font.Color := FontColor;
  end;
end;

procedure TFormPageSetup.SEditFontClick(Sender: TObject);
begin
  if not (ActiveControl is TEdit) then Exit;
  FontDialogPageSetup.Font.Assign(TEdit(ActiveControl).Font);
  if FontDialogPageSetup.Execute then
    TEdit(ActiveControl).Font.Assign(FontDialogPageSetup.Font);
end;

procedure TFormPageSetup.MemoTitleContentChange(Sender: TObject);
var
  i: Integer;
begin
  FTitle.Content := '';
  with MemoTitleContent do
    for i:=0 to Lines.Count-1 do
      FTitle.Content := FTitle.Content + Lines[i];
end;

procedure TFormPageSetup.MemoTailContentChange(Sender: TObject);
var
  i: Integer;
begin
  FTail.Content := '';
  with MemoTailContent do
    for i:=0 to Lines.Count-1 do
      FTail.Content := FTail.Content + Lines[i];
end;

procedure TFormPageSetup.SEditTitleDistanceChange(Sender: TObject);
begin
  FTitle.Distance := SEditTitleDistance.Value * ScreenPixelsPerMmY;
end;

procedure TFormPageSetup.SEditTailDistanceChange(Sender: TObject);
begin
  FTail.Distance := SEditTailDistance.Value * ScreenPixelsPerMmY;
end;

procedure TFormPageSetup.BtnTitleFontClick(Sender: TObject);
begin
  FontDialogPageSetup.Font.Assign(MemoTitleContent.Font);
  if FontDialogPageSetup.Execute then
    MemoTitleContent.Font.Assign(FontDialogPageSetup.Font);
  with FTitle do
  begin
    FontName := MemoTitleContent.Font.Name;
    FontSize := MemoTitleContent.Font.Size;
    FontStyle := MemoTitleContent.Font.Style;
    FontColor := MemoTitleContent.Font.Color;
  end;
end;

procedure TFormPageSetup.BtnTailFontClick(Sender: TObject);
begin
  FontDialogPageSetup.Font.Assign(MemoTailContent.Font);
  if FontDialogPageSetup.Execute then
    MemoTailContent.Font.Assign(FontDialogPageSetup.Font);
  with FTail do
  begin
    FontName := MemoTailContent.Font.Name;
    FontSize := MemoTailContent.Font.Size;
    FontStyle := MemoTailContent.Font.Style;
    FontColor := MemoTailContent.Font.Color;
  end;
end;

procedure TFormPageSetup.BtnOkClick(Sender: TObject);

  procedure GetHeaderInfo(AHeader: THeader; AEdit: TEdit);
  begin
    with AHeader, AEdit do
    begin
      Content := Text;
      FontName := Font.Name;
      FontSize := Font.Size;
      FontStyle := Font.Style;
      FontColor := Font.Color;
    end;
  end;

  procedure GetFooterInfo(AFooter: TFooter; AEdit: TEdit);
  begin
    GetHeaderInfo(THeader(AFooter), AEdit);
  end;

var
  Grid: TEasyGrid;
begin
  if (SEditFromPage.Value > SEditToPage.Value) then
  begin
    Application.MessageBox('起始页不能大于终止页。','提示',
                           MB_OK + MB_ICONINFORMATION);
    SEditToPage.SetFocus;
    Exit;
  end;
  Grid := TEasyGrid(ParentGrid);
  with Grid.EasyGridPageInfo.DetailPageInfo, Grid.EasyGridPageInfo.CommonPageInfo do
  begin
    // 第一页上的控件
    if RBtnHPrint.Checked then
      Orientation := poLandscape
    else
      Orientation := poPortrait;
    Scale := SEditScale.Value;
    StartPageNo := SEditStartPageNo.Value;
    PrintAllPages := RBtnPrintAll.Checked;
    StartPage := SEditFromPage.Value - 1;
    EndPage := SEditToPage.Value - 1;
    PaperSize := FPaperSize;
    PageWidth := FPageWidth;
    PageHeight := FPageHeight;
    // 第二页上的控件
    Margin.Top := SEditTMargin.Value * ScreenPixelsPerMmY;
    Margin.Bottom := SEditBMargin.Value * ScreenPixelsPerMmY;
    Margin.Left := SEditLMargin.Value * ScreenPixelsPerMmX;
    Margin.Right := SEditRMargin.Value * ScreenPixelsPerMmX;
    HeaderExtent := SEditHeaderSize.Value * ScreenPixelsPerMmY;
    FooterExtent := SEditFooterSize.Value * ScreenPixelsPerMmY;
    HorzCenter := ChkBoxHorzCenter.Checked;
    VertCenter := ChkBoxVertCenter.Checked;
    // 第三页上的控件
    HeaderLineStyle := TPenStyle(CbxHLineStyle.ItemIndex);
    HeaderDoubleLine := ChkBoxHDoubleLine.Checked;
    HeaderLineWidth := SEditHLineWidth.Value;
    GetHeaderInfo(Header1, EditHeader1);
    GetHeaderInfo(Header2, EditHeader2);
    GetHeaderInfo(Header3, EditHeader3);
    FooterLineStyle := TPenStyle(CbxFLineStyle.ItemIndex);
    FooterDoubleLine := ChkBoxFDoubleLine.Checked;
    FooterLineWidth := SEditFLineWidth.Value;
    GetFooterInfo(Footer1, EditFooter1);
    GetFooterInfo(Footer2, EditFooter2);
    GetFooterInfo(Footer3, EditFooter3);
    // 第四页上的控件
    MainTitle.Assign(FMainTitle);
    Title1.Assign(FTitle1);
    Title2.Assign(FTitle2);
    Title3.Assign(FTitle3);
    Title4.Assign(FTitle4);
    Title5.Assign(FTitle5);
    Title6.Assign(FTitle6);
    Tail1.Assign(FTail1);
    Tail2.Assign(FTail2);
    Tail3.Assign(FTail3);
    Tail4.Assign(FTail4);
    Tail5.Assign(FTail5);
    Tail6.Assign(FTail6);
    TitleExtent := SEditTitleExtent.Value * ScreenPixelsPerMmY;
    TailExtent := SEditTailExtent.Value * ScreenPixelsPerMmY;
    // 第五页上的控件
    HorzSplit := RBtnHorzSplit.Checked;
    PrintConjunction := RBtnPrintConjunction.Checked;
    TopConjunctionText := EditTopConjunction.Text;
    BottomConjunctionText := EditBottomConjunction.Text;
    MonoColored := ChkBoxMonoColored.Checked;
    PrintGridLine := ChkBoxPrintGridLine.Checked;
    PrintColTitle := ChkBoxPrintColTitle.Checked;
    PrintRowTitle := ChkBoxPrintRowTitle.Checked;
    PrintFixedCols := ChkBoxPrintFixedCols.Checked;
    PrintFixedRows := ChkBoxPrintFixedRows.Checked;
  end;
  ModalResult := mrOk;
end;

end.
