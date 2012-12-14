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

unit PreviewBox;               

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Printers, Forms, FlatSB, CommCtrl,
  SysUtils, Math;

const
  crExcel = 1001;
  crZoomIn = 1002;
  crZoomOut = 1003;

resourcestring
  BadCommonPageInfo = '错误的页面信息, 该文件头可能已经损坏。';
  
type
  TMm = Double;

  // 页眉页脚信息
  THeader = class(TObject)
  public
    FontSize: Integer;                                  // 字体大小
    FontStyle: TFontStyles;                             // 字体风格
    FontColor: TColor;                                  // 字体颜色
    FontName: string;                                   // 字体名
    Content: string;                                    // 页眉页脚内容
    constructor Create;
    procedure LoadFromStream(AFileStream: TFileStream);
    procedure SaveToStream(AFileStream: TFileStream);
    procedure Assign(Source: Pointer);
  end;
  TFooter = THeader;

  // 标题与结尾信息
  TTitle = class(TObject)
  public
    Distance: Integer;                                  // 距离页眉页脚高度
    FontSize: Integer;                                  // 字体大小
    FontStyle: TFontStyles;                             // 字体风格
    FontColor: TColor;                                  // 字体颜色
    FontName: string;                                   // 字体名
    Content: string;                                    // 标题内容
    constructor Create;
    procedure LoadFromStream(AFileStream: TFileStream);
    procedure SaveToStream(AFileStream: TFileStream);
    procedure Assign(Source: Pointer);
  end;
  TTail = TTitle;

  // 通用页面信息
  TCommonPageInfo = class(TObject)
  public
    Orientation: TPrinterOrientation;                // 打印方向(横向/纵向)
    StartPageNo: Integer;                            // 起始页号
    PrintAllPages: Boolean;                          // 是否打印所有页
    StartPage, EndPage: Integer;                     // 打印页码范围
    PaperSize: Integer;                              // 纸张大小   
    PageWidth: Integer;                              // 页宽
    PageHeight: Integer;                             // 页高
    Margin: TRect;                                   // 页边距
    Scale: Integer;                                  // 缩放比例
    HeaderExtent, FooterExtent: Integer;             // 页眉页脚高度
    HeaderLineStyle, FooterLineStyle: TPenStyle;     // 页眉页脚线形
    HeaderLineWidth, FooterLineWidth: Integer;       // 页眉页脚线宽
    HeaderDoubleLine, FooterDoubleLine: Boolean;     // 页眉页脚是否打印双线
    ExtraHeaderExtent, ExtraFooterExtent: Integer;   // 正文距离页眉页脚高度
    PrintFirstHeader, PrintFirstFooter: Boolean;     // 是否在第一页打印页眉页脚
    TitleExtent: Integer;                            // 标题区高度
    ExtraTitleExtent: Integer;                       // 额外标题区高度
    TailExtent: Integer;                             // 结尾区高度
    Reserved: array [0..255] of Char;                // 保留空间
    Header1, Header2, Header3: THeader;              // 左、中、右页眉信息
    Footer1, Footer2, Footer3: TFooter;              // 左、中、右页脚信息
    MainTitle, Title1, Title2, Title3: TTitle;       // 标题信息
    Title4, Title5, Title6: TTitle;
    Tail1, Tail2, Tail3, Tail4, Tail5, Tail6: TTail; // 结尾信息
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(AFileStream: TFileStream);
    procedure SaveToStream(AFileStream: TFileStream);
    procedure Assign(Source: Pointer);
  end;

  TPreviewBox = class;

  TPage = class(TCustomControl)
  private
    FBox: TPreviewBox;
    FPageIndex: Integer;
    FZoom: Integer;
    FMousePos: TPoint;
    FPageWidth, FPageHeight: Integer;
    procedure SetZoom(Value: Integer);
    procedure SetPageWidth(Value: Integer);
    procedure SetPageHeight(Value: Integer);
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Zoom: Integer read FZoom write SetZoom default 100;
    property PageWidth: Integer read FPageWidth write SetPageWidth default 50;
    property PageHeight: Integer read FPageHeight write SetPageHeight default 100;
    property Box: TPreviewBox read FBox write FBox;
  end;

  TPages = array of TPage;

  TPreviewBoxState = (pbSelect, pbZoomIn, pbZoomOut);

  TDrawPageEvent = procedure(DrawCanvas: TCanvas; DrawRect: TRect; PageIndex: Integer; Printing: Boolean) of object;

  TPreviewBox = class(TScrollBox)
  private
    FPages: TPages;
    FState: TPreviewBoxState;
    FZoom: Integer;
    FPageCount: Integer;
    FPageIndex: Integer;
    FInitializing: Boolean;
    FOnDrawPage: TDrawPageEvent;
    FCommonPageInfo: TCommonPageInfo;
    procedure SetZoom(Value: Integer);
    procedure SetPageCount(Value: Integer);
    procedure SetPageIndex(Value: Integer);
    procedure SetPreviewBoxState(NewState: TPreviewBoxState);
    procedure SetCommonPageInfo(Value: TCommonPageInfo);
    procedure AdjustPages;    // 调整各页的位置
    procedure RefreshPages;   // 刷新页面显示
    procedure ZoomIn;
    procedure ZoomOut;

    procedure DrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer);
    function  TranslateText(Content: string; PageNo: Integer): string; // 解释页眉页脚文本
    procedure DrawHeader(DrawCanvas: TCanvas; DrawRect: TRect; PageIndex: Integer; Printing: Boolean);
    procedure DrawFooter(DrawCanvas: TCanvas; DrawRect: TRect; PageIndex: Integer; Printing: Boolean);
    procedure DrawTitle(DrawCanvas: TCanvas; DrawRect: TRect; Printing: Boolean);
    procedure DrawTail(DrawCanvas: TCanvas; DrawRect: TRect; Printing: Boolean);

    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawPage(DrawCanvas: TCanvas; DrawRect: TRect; PageIndex: Integer; Printing: Boolean); virtual;
    procedure NextPage;
    procedure PriorPage;
    procedure SwitchZoom;
    property State: TPreviewBoxState read FState write SetPreviewBoxState;
    property Pages: TPages read FPages;
    property CommonPageInfo: TCommonPageInfo read FCommonPageInfo write SetCommonPageInfo;
  published
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PageCount: Integer read FPageCount write SetPageCount default 1;
    property PageIndex: Integer read FPageIndex write SetPageIndex default 0;
    property Zoom: Integer read FZoom write SetZoom default 20;
    property OnDrawPage: TDrawPageEvent read FOnDrawPage write FOnDrawPage;
  end;

  TGetPreviewBoxEvent = procedure (var Box: TPreviewBox) of object;
  TPaintEvent = procedure (Canvas: TCanvas; PageIndex: Integer) of object;

procedure Register;
function HasPrinter: Boolean;
function GetPaperSize(var PaperSize, PaperLength, PaperWidth: Integer): Boolean;
function SetPaperSize(PaperSize, PaperLength, PaperWidth: Integer): Boolean;

var
  ScreenPixelsPerMmX: Integer;      // 屏幕水平方向的每毫米像素数
  ScreenPixelsPerMmY: Integer;      // 屏幕垂直方向的每毫米像素数
  ScreenPixelsPerInchX: Integer;    // 屏幕水平方向的每英寸像素数
  ScreenPixelsPerInchY: Integer;    // 屏幕垂直方向的每英寸像素数
  PrinterPixelsPerMmX: Integer;     // 打印机水平方向的每毫米像素数
  PrinterPixelsPerMmY: Integer;     // 打印机垂直方向的每毫米像素数
  PrinterPixelsPerInchX: Integer;   // 打印机水平方向的每英寸像素数
  PrinterPixelsPerInchY: Integer;   // 打印机垂直方向的每英寸像素数
  ScreenToPrinterX: Double;         // 屏幕与打印机水平方向的缩放比
  ScreenToPrinterY: Double;         // 屏幕与打印机垂直方向的缩放比

implementation

const
  MmsPerInch = 25.4;
  HPageSpace = 20;
  VPageSpace = 20;
  SegmentFlagValue = $FF;

procedure Register;
begin
  RegisterComponents('Discovery', [TPreviewBox]);
end;

function HasPrinter: Boolean;
begin
  Result := Printer.Printers.Count > 0;
end;

function GetPaperSize(var PaperSize, PaperLength, PaperWidth: Integer): Boolean;
var
  ADevice, ADriver, APort: array [0..255] of Char;
  DeviceHandle: THandle;
  DevMode: PDeviceMode;
begin
  Result := False;
  Printer.GetPrinter(ADevice, ADriver, APort, DeviceHandle);
  if (DeviceHandle <> 0) then
  begin
    DevMode := GlobalLock(DeviceHandle);
    PaperSize := DevMode^.dmPaperSize;
    PaperLength := DevMode^.dmPaperLength;
    PaperWidth := DevMode^.dmPaperWidth;
    GlobalUnlock(DeviceHandle);
    Result := True;
  end;
end;

function SetPaperSize(PaperSize, PaperLength, PaperWidth: Integer): Boolean;
var
  ADevice, ADriver, APort: array [0..255] of Char;
  DevMode: PDeviceMode;
  DeviceHandle: THandle;
begin
  Result := False;
  Printer.GetPrinter(ADevice, ADriver, APort, DeviceHandle);
  if (DeviceHandle <> 0) then
  begin
    DevMode := GlobalLock(DeviceHandle);
    with DevMode^ do
    begin
      dmFields := dmFields or DM_PAPERSIZE or DM_PAPERLENGTH or DM_PAPERWIDTH;
      dmPaperSize := PaperSize;
      dmPaperLength := PaperLength;
      dmPaperWidth := PaperWidth;
      Printer.SetPrinter(ADevice, ADriver, APort, DeviceHandle);
      GlobalUnlock(DeviceHandle);
      Result := True;
    end;
  end;
end;

procedure GetResolution;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    ScreenPixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
    ScreenPixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
    ScreenPixelsPerMmX := Round(ScreenPixelsPerInchX / MmsPerInch);
    ScreenPixelsPerMmY := Round(ScreenPixelsPerInchY / MmsPerInch);
    if HasPrinter then
      begin
        PrinterPixelsPerInchX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
        PrinterPixelsPerInchY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
      end
    else
      begin
        PrinterPixelsPerInchX := 180;
        PrinterPixelsPerInchY := 180;
      end;
    PrinterPixelsPerMmX := Round(PrinterPixelsPerInchX / MmsPerInch);
    PrinterPixelsPerMmY := Round(PrinterPixelsPerInchY / MmsPerInch);
    ScreenToPrinterX := ScreenPixelsPerInchX / PrinterPixelsPerInchX;
    ScreenToPrinterY := ScreenPixelsPerInchY / PrinterPixelsPerInchY;
  finally
    ReleaseDC(0, DC);
  end;
end;

//TPage

constructor TPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZoom := 100;
  FPageHeight := 100;
  FPageWidth := 50;
  FMousePos.x := 0; FMousePos.y := 0;
  DoubleBuffered := True;
end;

procedure TPage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FMousePos := Point(X, Y);
  if FBox.State = pbZoomIn then begin FBox.State := pbZoomOut; Exit; end;
  if FBox.State = pbSelect then
    begin
      FBox.PageIndex := FPageIndex;
      FBox.State := pbZoomOut;
    end
  else FBox.State := pbZoomIn;
end;

procedure TPage.Paint;
var
  OldWindowExtent, OldViewPortExtent: TSize;
  OldMapMode: Integer;
  NewZoom, X, Y, Edge, Offset: Integer;
  DrawRect: TRect;
begin
  with Canvas do
  begin
    Brush.Color := clWhite;
    Pen.Width := 1;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;

    // 画页面图形
    if FPageIndex = FBox.PageIndex then
    begin
      Brush.Color := clRed;
      FillRect(Rect(0, 0, Width, Height));
      Pen.Color := clBlack;
      Brush.Color := clWhite;
      Rectangle(2, 2, Width - 2, Height - 2);
      Offset := 2;
    end
    else
    begin
      Brush.Color := FBox.Color;
      FillRect(Rect(0, 0, Width, Height));
      Pen.Color := clGray;
      Brush.Color := clGray;
      Rectangle(4, 4, Width, Height);
      Pen.Color := clBlack;
      Brush.Color := clWhite;
      Rectangle(0, 0, Width - 4, Height - 4);
      Offset := 0;
    end;

    // 画边缘折线
    Pen.Color := clSilver;
    Edge := Round(25 * FZoom / 100);
    with FBox.CommonPageInfo do
    begin
      X := Round((Margin.Left + Offset) * FZoom / 100);
      Y := Round((Margin.Top + Offset) * FZoom / 100);
      PolyLine([Point(X - Edge, Y), Point(X, Y), Point(X, Y - Edge)]);
      X := Round((FPageWidth - Margin.Right + Offset) * FZoom / 100);
      PolyLine([Point(X + Edge, Y), Point(X, Y), Point(X, Y - Edge)]);
      Y := Round((FPageHeight - Margin.Bottom + Offset) * FZoom / 100);
      PolyLine([Point(X + Edge, Y), Point(X, Y), Point(X, Y + Edge)]);
      X := Round((Margin.Left + Offset) * FZoom / 100);
      PolyLine([Point(X - Edge, Y), Point(X, Y), Point(X, Y + Edge)]);
      Pen.Color := clBlack;
      DrawRect := Rect(Margin.Left + Offset, Margin.Top + Offset,
                       FPageWidth - Margin.Right + Offset + 1,
                       FPageHeight - Margin.Bottom + Offset + 1);
    end;

    // 设置映射模式  MM_ANISOTROPIC ( 忽略 Scale 参数 )
    if (FBox.State = pbZoomIn) then NewZoom := 100 else NewZoom := FZoom;
    OldMapMode := SetMapMode(Handle, MM_ANISOTROPIC);
    SetWindowExtEx(Handle,100,100,@OldWindowExtent);
    SetViewPortExtEx(Handle, NewZoom, NewZoom, @OldViewPortExtent);
    // 画页眉、页脚、标题、结尾信息
    FBox.DrawPage(Canvas, DrawRect, FPageIndex, False);
    // 恢复映像模式
    SetViewPortExtEx(Handle,OldViewPortExtent.cx,OldViewPortExtent.cy,nil);
    SetWindowExtEx(Handle,OldWindowExtent.cx,OldWindowExtent.cy,nil);
    SetMapMode(Handle, OldMapMode);

    // 设置映射模式  MM_ANISOTROPIC ( 加入 Scale 参数 )
    if (FBox.State = pbZoomIn) then
      NewZoom := FBox.CommonPageInfo.Scale
    else
      NewZoom := Round(FZoom * FBox.CommonPageInfo.Scale / 100);
    OldMapMode := SetMapMode(Handle, MM_ANISOTROPIC);
    SetWindowExtEx(Handle,100,100,@OldWindowExtent);
    SetViewPortExtEx(Handle, NewZoom, NewZoom, @OldViewPortExtent);
    // 执行自定义绘画过程
    if Assigned(FBox.FOnDrawPage) then
      FBox.FOnDrawPage(Canvas, DrawRect, FPageIndex, False);
    // 恢复映像模式
    SetViewPortExtEx(Handle,OldViewPortExtent.cx,OldViewPortExtent.cy,nil);
    SetWindowExtEx(Handle,OldWindowExtent.cx,OldWindowExtent.cy,nil);
    SetMapMode(Handle, OldMapMode);
  end;
end;

procedure TPage.SetPageHeight(Value: Integer);
begin
  if Value = FPageHeight then Exit;
  FPageHeight := Value;
  Height := Round(Value * Zoom / 100) + 4;
  Invalidate;
end;

procedure TPage.SetPageWidth(Value: Integer);
begin
  if Value = FPageWidth then Exit;
  FPageWidth := Value;
  Width := Round(Value * Zoom / 100) + 4;
  Invalidate;
end;

procedure TPage.SetZoom(Value: Integer);
begin
  if Value = FZoom then Exit;
  FZoom := Value;
  Height := Round(Value * FPageHeight / 100) + 4;
  Width := Round(Value * FPageWidth / 100) + 4;
  Invalidate;
end;

procedure TPage.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FBox.State = pbZoomIn then Exit;
  if (FBox.PageIndex = FPageIndex) then
    FBox.State := pbZoomOut
  else
    FBox.State := pbSelect;
end;

procedure TPage.WMSetCursor(var Msg: TWMSetCursor);
var
  Cur: HCURSOR;
begin
  Cur := Screen.Cursors[crArrow];
  case FBox.State of
    pbSelect : Cur := Screen.Cursors[crHandPoint];
    pbZoomOut: Cur := Screen.Cursors[crZoomIn];
    pbZoomIn : Cur := Screen.Cursors[crZoomOut];
  end;
  if Cur <> 0 then SetCursor(Cur) else inherited;
end;

procedure TPage.CMMouseLeave(var Msg: TMessage);
begin
  if FBox.State = pbZoomIn then Exit;
  FBox.State := pbSelect;
end;

//TPreviewBox

const
  PageSepWidth = 3;
  EdgeWidth = 25;

constructor TPreviewBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonPageInfo := TCommonPageInfo.Create;
  TabStop := True;
  VertScrollBar.Style := ssFlat;
  VertScrollBar.Tracking := True;
  HorzScrollBar.Style := ssFlat;
  HorzScrollBar.Tracking := True;
  Width := 300;
  Height := 300;
  FState := pbSelect;
  FPageIndex := 0;
  FZoom := 20;
  PageCount := 1;
end;

destructor TPreviewBox.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPageCount - 1 do FPages[i].Free;
  inherited Destroy;
end;

procedure TPreviewBox.SetZoom(Value: Integer);
var
  i: Integer;
begin
  if Value < 10 then Value := 10;
  if Value > 100 then Value := 100;
  for i:=0 to FPageCount-1 do
    FPages[i].Zoom := Value;
  FZoom := Value;
  AdjustPages;
end;

procedure TPreviewBox.SetPageCount(Value: Integer);
var
  i: Integer;
begin
  FInitializing := True;
  if FPageCount = Value then Exit;
  if Value < 1 then Value := 1;
  for i := FPageCount - 1 downto Value do
    FPages[i].Free;
  SetLength(FPages, Value);
  for i := FPageCount to Value - 1 do
  begin
    FPages[i] := TPage.Create(Self);
    with FPages[i] do
    begin
      Parent := Self;
      Visible := True;
      Box := Self;
      FPageIndex := i;
      Zoom := Self.Zoom;
    end;
  end;
  FPageCount := Value; FPageIndex := 0;
  FInitializing := False;
  RefreshPages;
end;

procedure TPreviewBox.SetPageIndex(Value: Integer);
var
  APage: TPage;
begin
  if Value >= FPageCount then Value := FPageCount - 1;
  if Value < 0 then Value := 0;
  FPages[FPageIndex].Invalidate;
  FPageIndex := Value;
  FPages[FPageIndex].Invalidate;
  APage := FPages[FPageIndex];
  if (not (FState = pbZoomIn)) and
     (APage.Top >= 0) and (APage.Left >= 0) and
     (APage.Top + APage.Height <= Height) and (APage.Left + APage.Width <= Width) then Exit;
  AdjustPages;
end;

procedure TPreviewBox.CNKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode in [VK_PRIOR..VK_DOWN] then Exit;
  inherited;
end;

procedure TPreviewBox.WMSize(var Message: TWMSize);
begin
  inherited;
  if not HandleAllocated or FInitializing then Exit;
  AdjustPages;
end;

procedure TPreviewBox.AdjustPages;
var
  i, VIndex, HCount, VCount, LeftPos, TopPos, RealWidth, RealHeight: Integer;
  PageLeftPos, PageTopPos, PreHPosition: Integer;
begin
  if PageCount = 0 then Exit;
  // 记住原来的滚动条位置
  PreHPosition := HorzScrollBar.Position;
  // 在调整之前把滚动条位置设为 0
  if HorzScrollBar.Range > 0 then
    HorzScrollBar.Position := 0;
  if VertScrollBar.Range > 0 then
    VertScrollBar.Position := 0;
  if FState = pbZoomIn then
  begin
    FPages[FPageIndex].Zoom := 100;
    if Width > FPages[FPageIndex].Width then
      FPages[FPageIndex].Left := (Width - FPages[FPageIndex].Width) div 2
    else
      FPages[FPageIndex].Left := HPageSpace;
    FPages[FPageIndex].Top := VPageSpace;
    FPages[FPageIndex].Visible := True;
    for i:=0 to FPageCount-1 do
      if i <> FPageIndex then FPages[i].Visible := False;
    // 恢复原来的滚动条位置
    if HorzScrollBar.Range > 0 then
      HorzScrollBar.Position := Round(HorzScrollBar.Range * FPages[FPageIndex].FMousePos.x / FPages[FPageIndex].Width);
    if VertScrollBar.Range > 0 then
      VertScrollBar.Position := Round(VertScrollBar.Range * FPages[FPageIndex].FMousePos.y / FPages[FPageIndex].Height);
    Exit;
  end;
  for i:=0 to FPageCount-1 do FPages[i].Visible := True;
  RealWidth := FPages[0].Width + HPageSpace;
  RealHeight := FPages[0].Height + VPageSpace;
  HCount := Trunc((Width-HPageSpace) / RealWidth);
  if HCount < 1 then HCount := 1;
  if FPageCount = 1 then HCount := 1;
  VCount := Round(FPageCount / HCount) + 1;
  VIndex := FPageIndex div HCount ;
  if (FPageCount mod HCount) = 0 then Dec(VCount);
  i := 0;
  LeftPos := HPageSpace;
  TopPos := VPageSpace;
  if (HCount = 1) and (Width > FPages[0].Width) then
    LeftPos := (Width - FPages[0].Width) div 2;
  if (VCount = 1) and (Height > FPages[0].Height) then
    TopPos := (Height - FPages[0].Height) div 2;
  PageLeftPos := LeftPos; PageTopPos := TopPos;
  while i < FPageCount do
  begin
    FPages[i].Left := PageLeftPos;
    FPages[i].Top := PageTopPos;
    Inc(i);
    Inc(PageLeftPos, RealWidth);
    if (i mod HCount) = 0 then
    begin
      PageLeftPos := LeftPos;
      Inc(PageTopPos, RealHeight);
    end;
  end;
  // 恢复滚动条位置
  if HorzScrollBar.Range > 0 then
    HorzScrollBar.Position := Max(HorzScrollBar.Position, PreHPosition);
  if VertScrollBar.Range > 0 then
    VertScrollBar.Position := Round(VertScrollBar.Range * VIndex / VCount);
end;

procedure TPreviewBox.DrawPage(DrawCanvas: TCanvas; DrawRect: TRect;
  PageIndex: Integer; Printing: Boolean);
begin
  DrawHeader(DrawCanvas, DrawRect, PageIndex, Printing);
  DrawFooter(DrawCanvas, DrawRect, PageIndex, Printing);
  if PageIndex = 0 then
    DrawTitle(DrawCanvas, DrawRect, Printing);
  if PageIndex = PageCount - 1 then
    DrawTail(DrawCanvas, DrawRect, Printing);
end;

procedure TPreviewBox.SetPreviewBoxState(NewState: TPreviewBoxState);
var
  OldState: TPreviewBoxState;
begin
  if FState = NewState then Exit;
  OldState := FState;
  FState := NewState;
  if (NewState = pbZoomIn) then ZoomIn
  else if (OldState = pbZoomIn) then ZoomOut;
end;

procedure TPreviewBox.ZoomIn;
begin
  FPages[FPageIndex].Zoom := 100;
  AdjustPages;
end;

procedure TPreviewBox.ZoomOut;
var
  i: Integer;
begin
  for i:=0 to FPageCount-1 do FPages[i].Zoom := FZoom;
  AdjustPages;
end;

procedure TPreviewBox.WMSetCursor(var Msg: TWMSetCursor);
var
  Cur: HCURSOR;
begin
  Cur := Screen.Cursors[crArrow];
  SetCursor(Cur);
end;

procedure TPreviewBox.SetCommonPageInfo(Value: TCommonPageInfo);
begin
  FCommonPageInfo.Assign(Value);
  RefreshPages;
end;

procedure TPreviewBox.DrawFooter(DrawCanvas: TCanvas; DrawRect: TRect;
  PageIndex: Integer; Printing: Boolean);

  procedure DrawFooterLine;
  var
    LinePos: Integer;
  begin
    if CommonPageInfo.FooterLineWidth <= 0 then Exit;
    with CommonPageInfo, DrawCanvas do
      if FooterDoubleLine then
        begin
          Pen.Width := FooterLineWidth;
          Pen.Style := FooterLineStyle;
          Pen.Mode := pmCopy;
          LinePos := DrawRect.Bottom - FooterExtent + FooterLineWidth Div 2;
          DrawLine(DrawCanvas, DrawRect.Left, LinePos, DrawRect.Right, LinePos);
          Inc(LinePos, FooterLineWidth + 2);
          DrawLine(DrawCanvas, DrawRect.Left, LinePos, DrawRect.Right, LinePos);
        end
      else
        begin
          Pen.Width := FooterLineWidth;
          Pen.Style := FooterLineStyle;
          Pen.Mode := pmCopy;
          LinePos := DrawRect.Bottom - FooterExtent + FooterLineWidth Div 2;
          DrawLine(DrawCanvas, DrawRect.Left, LinePos, DrawRect.Right, LinePos);
        end;
  end;

  procedure DrawFooterText(Footer: TFooter; TextAlign: Cardinal);
  var
    FooterRect: TRect;
    FooterText: string;
  begin
    if Footer.Content = '' then Exit;
    with DrawCanvas, Footer do
    begin
      FooterText := TranslateText(Content, PageIndex);
      FooterRect := DrawRect;
      FooterRect.Top := FooterRect.Bottom - CommonPageInfo.FooterExtent;
      Font.Name := FontName;
      Font.Size := FontSize;
      if Printing then Font.Size := Round(FontSize * ScreenToPrinterX);
      Font.Color := FontColor;
      Font.Style := FontStyle;
      DrawText(DrawCanvas.Handle, PChar(FooterText), Length(FooterText), FooterRect, TextAlign);
    end;
  end;

begin
  if ((PageIndex = 0) and (not CommonPageInfo.PrintFirstFooter)) or
     (CommonPageInfo.FooterExtent = 0) then Exit;
  with CommonPageInfo, DrawCanvas do
  begin
    DrawFooterText(Footer1, DT_LEFT or DT_BOTTOM or DT_SINGLELINE or DT_NOCLIP);
    DrawFooterText(Footer2, DT_CENTER or DT_BOTTOM or DT_SINGLELINE or DT_NOCLIP);
    DrawFooterText(Footer3, DT_RIGHT or DT_BOTTOM or DT_SINGLELINE or DT_NOCLIP);
    DrawFooterLine;
  end;
end;

procedure TPreviewBox.DrawHeader(DrawCanvas: TCanvas; DrawRect: TRect;
  PageIndex: Integer; Printing: Boolean);

  procedure DrawHeaderLine;
  var
    LinePos: Integer;
  begin
    if CommonPageInfo.HeaderLineWidth <= 0 then Exit;
    with CommonPageInfo, DrawCanvas do
      if HeaderDoubleLine then
        begin
          Pen.Width := HeaderLineWidth;
          Pen.Style := HeaderLineStyle;
          Pen.Mode := pmCopy;
          LinePos := DrawRect.Top + HeaderExtent - (HeaderLineWidth + 2 + HeaderLineWidth Div 2);
          DrawLine(DrawCanvas, DrawRect.Left, LinePos, DrawRect.Right, LinePos);
          Inc(LinePos, HeaderLineWidth + 2);
          DrawLine(DrawCanvas, DrawRect.Left, LinePos, DrawRect.Right, LinePos);
        end
      else
        begin
          Pen.Width := HeaderLineWidth;
          Pen.Style := HeaderLineStyle;
          Pen.Mode := pmCopy;
          LinePos := DrawRect.Top + HeaderExtent - HeaderLineWidth Div 2;
          DrawLine(DrawCanvas, DrawRect.Left, LinePos, DrawRect.Right, LinePos);
        end;
  end;

  procedure DrawHeaderText(Header: THeader; TextAlign: Cardinal);
  var
    HeaderRect: TRect;
    HeaderText: string;
  begin
    if Header.Content = '' then Exit;
    with DrawCanvas, Header do
    begin
      HeaderText := TranslateText(Content, PageIndex);
      HeaderRect := DrawRect;
      HeaderRect.Bottom := HeaderRect.Top + CommonPageInfo.HeaderExtent;
      Font.Name := FontName;
      Font.Size := FontSize;
      if Printing then Font.Size := Round(FontSize * ScreenToPrinterX);
      Font.Color := FontColor;
      Font.Style := FontStyle;
      DrawText(DrawCanvas.Handle, PChar(HeaderText), -1, HeaderRect, TextAlign);
    end;
  end;

begin
  if ((PageIndex = 0) and (not CommonPageInfo.PrintFirstHeader)) or
     (CommonPageInfo.HeaderExtent = 0) then Exit;
  with CommonPageInfo, DrawCanvas do
  begin
    DrawHeaderText(Header1, DT_LEFT or DT_TOP or DT_SINGLELINE or DT_NOCLIP);
    DrawHeaderText(Header2, DT_CENTER or DT_TOP or DT_SINGLELINE or DT_NOCLIP);
    DrawHeaderText(Header3, DT_RIGHT or DT_TOP or DT_SINGLELINE or DT_NOCLIP);
    DrawHeaderLine;
  end;
end;

procedure TPreviewBox.DrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer);
begin
  ACanvas.MoveTo(X1, Y1);
  ACanvas.LineTo(X2, Y2);
end;

procedure TPreviewBox.DrawTail(DrawCanvas: TCanvas; DrawRect: TRect; Printing: Boolean);

  procedure DrawTailText(Tail: TTail; TextAlign: Cardinal);
  var
    TailRect, ContentRect, IRect: TRect;
  begin
    if Tail.Content = '' then Exit;
    with CommonPageInfo, DrawCanvas, Tail do
    begin
      TailRect := DrawRect;
      TailRect.Bottom := DrawRect.Bottom - FooterExtent - ExtraFooterExtent;
      TailRect.Top := TailRect.Bottom - TailExtent;
      ContentRect := TailRect;
      ContentRect.Top := TailRect.Top + Distance;
      IntersectRect(IRect, TailRect, ContentRect);
      Font.Name := FontName;
      Font.Size := FontSize;
      if Printing then Font.Size := Round(FontSize * ScreenToPrinterX);
      Font.Color := FontColor;
      Font.Style := FontStyle;
      DrawText(DrawCanvas.Handle, PChar(Tail.Content), Length(Tail.Content), IRect, TextAlign);
    end;
  end;

begin
  with CommonPageInfo, DrawCanvas do
  begin
    DrawTailText(Tail1, DT_LEFT or DT_SINGLELINE);
    DrawTailText(Tail2, DT_CENTER or DT_SINGLELINE);
    DrawTailText(Tail3, DT_RIGHT or DT_SINGLELINE);
    DrawTailText(Tail4, DT_LEFT or DT_SINGLELINE);
    DrawTailText(Tail5, DT_CENTER or DT_SINGLELINE);
    DrawTailText(Tail6, DT_RIGHT or DT_SINGLELINE);
  end;
end;

procedure TPreviewBox.DrawTitle(DrawCanvas: TCanvas; DrawRect: TRect; Printing: Boolean);

  procedure DrawTitleText(Title: TTitle; TextAlign: Cardinal);
  var
    TitleRect, ContentRect, IRect: TRect;
  begin
    if Title.Content = '' then Exit;
    with CommonPageInfo, DrawCanvas, Title do
    begin
      TitleRect := DrawRect;
      TitleRect.Top := DrawRect.Top + HeaderExtent + ExtraHeaderExtent + ExtraTitleExtent;
      TitleRect.Bottom := TitleRect.Top + TitleExtent;
      ContentRect := TitleRect;
      ContentRect.Top := TitleRect.Top + Distance;
      IntersectRect(IRect, TitleRect, ContentRect);
      Font.Name := FontName;
      Font.Size := FontSize;
      if Printing then Font.Size := Round(FontSize * ScreenToPrinterX);
      Font.Color := FontColor;
      Font.Style := FontStyle;
      DrawText(DrawCanvas.Handle, PChar(Title.Content), -1, IRect, TextAlign);
    end;
  end;

begin
  with CommonPageInfo, DrawCanvas do
  begin
    DrawTitleText(MainTitle, DT_CENTER or DT_SINGLELINE);
    DrawTitleText(Title1, DT_LEFT or DT_SINGLELINE);
    DrawTitleText(Title2, DT_CENTER or DT_SINGLELINE);
    DrawTitleText(Title3, DT_RIGHT or DT_SINGLELINE);
    DrawTitleText(Title4, DT_LEFT or DT_SINGLELINE);
    DrawTitleText(Title5, DT_CENTER or DT_SINGLELINE);
    DrawTitleText(Title6, DT_RIGHT or DT_SINGLELINE);
  end;
end;

function TPreviewBox.TranslateText(Content: string; PageNo: Integer): string;
var
  i: Integer;
begin
  i := 1;
  Result := '';
  while i <= Length(Content) do
  begin
    if Content[i] <> '&' then
    begin
      Result := Result + Content[i];
      Inc(i);
      Continue;
    end;
    Inc(i);
    if UpperCase(Content[i]) = 'P' then
      Result := Result + IntToStr(CommonPageInfo.StartPageNo + PageNo)
    else if UpperCase(Content[i]) = 'D' then
      Result := Result + FormatDateTime('yyyy"年"mm"月"dd"日"',Date)
    else if UpperCase(Content[i]) = 'T' then
      Result := Result + FormatDateTime('hh":"mm',Time)
    else if UpperCase(Content[i]) = 'C' then
      Result := Result + IntToStr(FPageCount)
    else
      Result := Result + Content[i];
    Inc(i);
  end;
end;

procedure TPreviewBox.RefreshPages;
var
  i: Integer;
begin
  for i := 0 to FPageCount - 1 do
  begin
    FPages[i].PageWidth := FCommonPageInfo.PageWidth;
    FPages[i].PageHeight := FCommonPageInfo.PageHeight;
  end;
  AdjustPages;
  for i := 0 to FPageCount - 1 do FPages[i].Invalidate;
end;

procedure TPreviewBox.PriorPage;
begin
  if (FPageIndex = 0) then Exit;
  PageIndex := FPageIndex - 1;
end;

procedure TPreviewBox.NextPage;
begin
  if (FPageIndex = FPageCount - 1) then Exit;
  PageIndex := FPageIndex + 1;
end;

procedure TPreviewBox.SwitchZoom;
begin
  if (FState = pbZoomIn) then State := pbZoomOut else State := pbZoomIn; 
end;

procedure TPreviewBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Step: Integer;
begin
  inherited KeyDown(Key, Shift);
  Step := 10;
  case Key of
    VK_LEFT :
      if FState = pbZoomIn then
        begin
          if HorzScrollBar.Range > 0 then
            HorzScrollBar.Position := Max(HorzScrollBar.Position - Step, 0);
        end
      else PriorPage;
    VK_UP :
      if FState = pbZoomIn then
        begin
          if VertScrollBar.Range > 0 then
            VertScrollBar.Position := Max(VertScrollBar.Position - Step, 0);
        end
      else PriorPage;
    VK_RIGHT :
      if FState = pbZoomIn then
        begin
          if HorzScrollBar.Range > 0 then
            HorzScrollBar.Position := Min(HorzScrollBar.Position + Step, HorzScrollBar.Range);
        end
      else NextPage;
    VK_DOWN :
      if FState = pbZoomIn then
        begin
          if VertScrollBar.Range > 0 then
            VertScrollBar.Position := Min(VertScrollBar.Position + Step, VertScrollBar.Range);
        end
      else NextPage;
    VK_PRIOR : 
      if FState = pbZoomIn then
        begin
          if VertScrollBar.Range > 0 then
            VertScrollBar.Position := 0;
        end
      else PriorPage;
    VK_NEXT :
      if FState = pbZoomIn then
        begin
          if VertScrollBar.Range > 0 then
            VertScrollBar.Position := VertScrollBar.Range;
        end
      else NextPage;
    VK_HOME :
      if FState = pbZoomIn then
        begin
          if HorzScrollBar.Range > 0 then
            HorzScrollBar.Position := 0;
        end
      else PageIndex := 0;
    VK_END :
      if FState = pbZoomIn then
        begin
          if HorzScrollBar.Range > 0 then
            HorzScrollBar.Position := HorzScrollBar.Range;
        end
      else PageIndex := FPageCount - 1;
    VK_RETURN: SwitchZoom;
  end;
end;

{ TCommonPageInfo }

procedure TCommonPageInfo.Assign(Source: Pointer);
begin
  Orientation := TCommonPageInfo(Source).Orientation;
  StartPageNo := TCommonPageInfo(Source).StartPageNo;
  PrintAllPages := TCommonPageInfo(Source).PrintAllPages;
  StartPage := TCommonPageInfo(Source).StartPage;
  EndPage := TCommonPageInfo(Source).EndPage;
  PaperSize := TCommonPageInfo(Source).PaperSize;
  PageWidth := TCommonPageInfo(Source).PageWidth;
  PageHeight := TCommonPageInfo(Source).PageHeight;
  Margin := TCommonPageInfo(Source).Margin;
  Scale := TCommonPageInfo(Source).Scale;
  HeaderExtent := TCommonPageInfo(Source).HeaderExtent;
  FooterExtent := TCommonPageInfo(Source).FooterExtent;
  HeaderLineStyle := TCommonPageInfo(Source).HeaderLineStyle;
  FooterLineStyle := TCommonPageInfo(Source).FooterLineStyle;
  HeaderLineWidth := TCommonPageInfo(Source).HeaderLineWidth;
  FooterLineWidth := TCommonPageInfo(Source).FooterLineWidth;
  HeaderDoubleLine := TCommonPageInfo(Source).HeaderDoubleLine;
  FooterDoubleLine := TCommonPageInfo(Source).FooterDoubleLine;
  ExtraHeaderExtent := TCommonPageInfo(Source).ExtraHeaderExtent;
  ExtraFooterExtent := TCommonPageInfo(Source).ExtraFooterExtent;
  PrintFirstHeader := TCommonPageInfo(Source).PrintFirstHeader;
  PrintFirstFooter := TCommonPageInfo(Source).PrintFirstFooter;
  TitleExtent := TCommonPageInfo(Source).TitleExtent;
  ExtraTitleExtent := TCommonPageInfo(Source).ExtraTitleExtent;
  TailExtent := TCommonPageInfo(Source).TailExtent;
  Header1.Assign(TCommonPageInfo(Source).Header1);
  Header2.Assign(TCommonPageInfo(Source).Header2);
  Header3.Assign(TCommonPageInfo(Source).Header3);
  Footer1.Assign(TCommonPageInfo(Source).Footer1);
  Footer2.Assign(TCommonPageInfo(Source).Footer2);
  Footer3.Assign(TCommonPageInfo(Source).Footer3);
  MainTitle.Assign(TCommonPageInfo(Source).MainTitle);
  Title1.Assign(TCommonPageInfo(Source).Title1);
  Title2.Assign(TCommonPageInfo(Source).Title2);
  Title3.Assign(TCommonPageInfo(Source).Title3);
  Title4.Assign(TCommonPageInfo(Source).Title4);
  Title5.Assign(TCommonPageInfo(Source).Title5);
  Title6.Assign(TCommonPageInfo(Source).Title6);
  Tail1.Assign(TCommonPageInfo(Source).Tail1);
  Tail2.Assign(TCommonPageInfo(Source).Tail2);
  Tail3.Assign(TCommonPageInfo(Source).Tail3);
  Tail4.Assign(TCommonPageInfo(Source).Tail4);
  Tail5.Assign(TCommonPageInfo(Source).Tail5);
  Tail6.Assign(TCommonPageInfo(Source).Tail6);
end;

constructor TCommonPageInfo.Create;
begin
  Inherited Create;
  Orientation := poPortrait;
  StartPageNo := 1;
  PrintAllPages := True;
  StartPage := 0;
  EndPage := 9999;
  PaperSize := 0;
  if HasPrinter then
    begin
      PageWidth := Round(Printer.PageWidth * ScreenToPrinterX);
      PageHeight := Round(Printer.PageHeight * ScreenToPrinterY);
    end
  else
    begin
      PageWidth := Round(1488 * ScreenToPrinterX);
      PageHeight := Round(2104 * ScreenToPrinterY);
    end;
  Margin.Left := 19 * ScreenPixelsPerMmX;
  Margin.Right := Margin.Left;
  Margin.Top := 25 * ScreenPixelsPerMmY;
  Margin.Bottom := Margin.Top;
  Scale := 100;
  TitleExtent := 0 * ScreenPixelsPerMmY;
  TailExtent := 0 * ScreenPixelsPerMmY;
  ExtraTitleExtent := 0;
  HeaderExtent := 0 * ScreenPixelsPerMmY;
  FooterExtent := 0 * ScreenPixelsPerMmY;
  ExtraHeaderExtent := 5 * ScreenPixelsPerMmY;
  ExtraFooterExtent := 5 * ScreenPixelsPerMmY;
  HeaderLineStyle := psSolid;
  FooterLineStyle := psSolid;
  HeaderLineWidth := 0;
  FooterLineWidth := 0;
  HeaderDoubleLine := True;
  FooterDoubleLine := True;
  PrintFirstHeader := True;
  PrintFirstFooter := True;
  FillChar(Reserved, 256, 0);

  Header1 := THeader.Create;
  Header2 := THeader.Create;
  Header3 := THeader.Create;
  Footer1 := TFooter.Create;
  Footer2 := TFooter.Create;
  Footer3 := TFooter.Create;
  MainTitle := TTitle.Create;
  Title1 := TTitle.Create;
  Title2 := TTitle.Create;
  Title3 := TTitle.Create;
  Title4 := TTitle.Create;
  Title5 := TTitle.Create;
  Title6 := TTitle.Create;
  Tail1 := TTail.Create;
  Tail2 := TTail.Create;
  Tail3 := TTail.Create;
  Tail4 := TTail.Create;
  Tail5 := TTail.Create;
  Tail6 := TTail.Create;
end;

destructor TCommonPageInfo.Destroy;
begin
  Header1.Free;
  Header2.Free;
  Header3.Free;
  Footer1.Free;
  Footer2.Free;
  Footer3.Free;
  MainTitle.Free;
  Title1.Free;
  Title2.Free;
  Title3.Free;
  Title4.Free;
  Title5.Free;
  Title6.Free;
  Tail1.Free;
  Tail2.Free;
  Tail3.Free;
  Tail4.Free;
  Tail5.Free;
  Tail6.Free;
  Inherited Destroy;
end;

procedure TCommonPageInfo.LoadFromStream(AFileStream: TFileStream);

  procedure ReadSegmentFlag;
  var
    SegmentFlag: Byte;
  begin
    AFileStream.Read(SegmentFlag, SizeOf(SegmentFlag));
    if (SegmentFlag <> SegmentFlagValue) then Raise Exception.Create(BadCommonPageInfo);
  end;
  
begin
  with AFileStream do
  begin
    Read(Orientation, SizeOf(Orientation));
    Read(StartPageNo, SizeOf(StartPageNo));
    Read(PrintAllPages, SizeOf(PrintAllPages));
    Read(StartPage, SizeOf(StartPage));
    Read(EndPage, SizeOf(EndPage));
    Read(PaperSize, SizeOf(PaperSize));
    Read(PageWidth, SizeOf(PageWidth));
    Read(PageHeight, SizeOf(PageHeight));
    Read(Margin, SizeOf(Margin));
    Read(Scale, SizeOf(Scale));
    Read(TitleExtent, SizeOf(TitleExtent));
    Read(ExtraTitleExtent, SizeOf(ExtraTitleExtent));
    Read(TailExtent, SizeOf(TailExtent));
    Read(HeaderExtent, SizeOf(HeaderExtent));
    Read(FooterExtent, SizeOf(FooterExtent));
    Read(ExtraHeaderExtent, SizeOf(ExtraHeaderExtent));
    Read(ExtraFooterExtent, SizeOf(ExtraFooterExtent));
    Read(HeaderLineStyle, SizeOf(HeaderLineStyle));
    Read(FooterLineStyle, SizeOf(FooterLineStyle));
    Read(HeaderLineWidth, SizeOf(HeaderLineWidth));
    Read(FooterLineWidth, SizeOf(FooterLineWidth));
    Read(HeaderDoubleLine, SizeOf(HeaderDoubleLine));
    Read(FooterDoubleLine, SizeOf(FooterDoubleLine));
    Read(PrintFirstHeader, SizeOf(PrintFirstHeader));
    Read(PrintFirstFooter, SizeOf(PrintFirstFooter));
    Read(Reserved, 256);
    ReadSegmentFlag;
  end;
  Header1.LoadFromStream(AFileStream); ReadSegmentFlag;
  Header2.LoadFromStream(AFileStream); ReadSegmentFlag;
  Header3.LoadFromStream(AFileStream); ReadSegmentFlag;
  Footer1.LoadFromStream(AFileStream); ReadSegmentFlag;
  Footer2.LoadFromStream(AFileStream); ReadSegmentFlag;
  Footer3.LoadFromStream(AFileStream); ReadSegmentFlag;
  MainTitle.LoadFromStream(AFileStream); ReadSegmentFlag;
  Title1.LoadFromStream(AFileStream); ReadSegmentFlag;
  Title2.LoadFromStream(AFileStream); ReadSegmentFlag;
  Title3.LoadFromStream(AFileStream); ReadSegmentFlag;
  Title4.LoadFromStream(AFileStream); ReadSegmentFlag;
  Title5.LoadFromStream(AFileStream); ReadSegmentFlag;
  Title6.LoadFromStream(AFileStream); ReadSegmentFlag;
  Tail1.LoadFromStream(AFileStream); ReadSegmentFlag;
  Tail2.LoadFromStream(AFileStream); ReadSegmentFlag;
  Tail3.LoadFromStream(AFileStream); ReadSegmentFlag;
  Tail4.LoadFromStream(AFileStream); ReadSegmentFlag;
  Tail5.LoadFromStream(AFileStream); ReadSegmentFlag;
  Tail6.LoadFromStream(AFileStream); ReadSegmentFlag;
end;

procedure TCommonPageInfo.SaveToStream(AFileStream: TFileStream);
var
  SegmentFlag: Byte;
begin
  SegmentFlag := SegmentFlagValue;
  with AFileStream do
  begin
    Write(Orientation, SizeOf(Orientation));
    Write(StartPageNo, SizeOf(StartPageNo));
    Write(PrintAllPages, SizeOf(PrintAllPages));
    Write(StartPage, SizeOf(StartPage));
    Write(EndPage, SizeOf(EndPage));
    Write(PaperSize, SizeOf(PaperSize));
    Write(PageWidth, SizeOf(PageWidth));
    Write(PageHeight, SizeOf(PageHeight));
    Write(Margin, SizeOf(Margin));
    Write(Scale, SizeOf(Scale));
    Write(TitleExtent, SizeOf(TitleExtent));
    Write(ExtraTitleExtent, SizeOf(ExtraTitleExtent));
    Write(TailExtent, SizeOf(TailExtent));
    Write(HeaderExtent, SizeOf(HeaderExtent));
    Write(FooterExtent, SizeOf(FooterExtent));
    Write(ExtraHeaderExtent, SizeOf(ExtraHeaderExtent));
    Write(ExtraFooterExtent, SizeOf(ExtraFooterExtent));
    Write(HeaderLineStyle, SizeOf(HeaderLineStyle));
    Write(FooterLineStyle, SizeOf(FooterLineStyle));
    Write(HeaderLineWidth, SizeOf(HeaderLineWidth));
    Write(FooterLineWidth, SizeOf(FooterLineWidth));
    Write(HeaderDoubleLine, SizeOf(HeaderDoubleLine));
    Write(FooterDoubleLine, SizeOf(FooterDoubleLine));
    Write(PrintFirstHeader, SizeOf(PrintFirstHeader));
    Write(PrintFirstFooter, SizeOf(PrintFirstFooter));
    Write(Reserved, 256);
    Write(SegmentFlag, SizeOf(SegmentFlag));
  end;
  Header1.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Header2.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Header3.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Footer1.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Footer2.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Footer3.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  MainTitle.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Title1.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Title2.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Title3.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Title4.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Title5.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Title6.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Tail1.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Tail2.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Tail3.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Tail4.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Tail5.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
  Tail6.SaveToStream(AFileStream); AFileStream.Write(SegmentFlag, SizeOf(SegmentFlag));
end;

{ THeader }

procedure THeader.Assign(Source: Pointer);
begin
  FontSize := THeader(Source).FontSize;
  FontStyle := THeader(Source).FontStyle;
  FontColor := THeader(Source).FontColor;
  FontName := THeader(Source).FontName;
  Content := THeader(Source).Content;
end;

constructor THeader.Create;
begin
  FontSize := 9;
  FontStyle := [];
  FontColor := clBlack;
  FontName := '宋体';
  Content := '';
end;

procedure THeader.LoadFromStream(AFileStream: TFileStream);
var
  StrLen: Integer;
begin
  with AFileStream do
  begin
    Read(FontSize, SizeOf(FontSize));
    Read(FontStyle, SizeOf(FontStyle));
    Read(FontColor, SizeOf(FontColor));
    Read(StrLen,SizeOf(Integer));
    SetLength(FontName, StrLen);
    Read(FontName[1],StrLen);
    Read(StrLen,SizeOf(Integer));
    SetLength(Content, StrLen);
    Read(Content[1],StrLen);
  end;
end;

procedure THeader.SaveToStream(AFileStream: TFileStream);
var
  StrLen: Integer;
begin
  with AFileStream do
  begin
    Write(FontSize, SizeOf(FontSize));
    Write(FontStyle, SizeOf(FontStyle));
    Write(FontColor, SizeOf(FontColor));
    StrLen := Length(FontName);
    Write(StrLen,SizeOf(Integer));
    Write(FontName[1],StrLen);
    StrLen := Length(Content);
    Write(StrLen,SizeOf(Integer));
    Write(Content[1],StrLen);
  end;
end;

{ TTitle }

procedure TTitle.Assign(Source: Pointer);
begin
  Distance := TTitle(Source).Distance;
  FontName := TTitle(Source).FontName;
  FontSize := TTitle(Source).FontSize;
  FontColor := TTitle(Source).FontColor;
  FontStyle := TTitle(Source).FontStyle;
  Content := TTitle(Source).Content;
end;

constructor TTitle.Create;
begin
  Distance := 0 * ScreenPixelsPerMmY;
  FontSize := 24;
  FontStyle := [];
  FontColor := clBlack;
  FontName := '宋体';
  Content := '';
end;

procedure TTitle.LoadFromStream(AFileStream: TFileStream);
var
  StrLen: Integer;
begin
  with AFileStream do
  begin
    Read(Distance, SizeOf(Distance));
    Read(FontSize, SizeOf(FontSize));
    Read(FontStyle, SizeOf(FontStyle));
    Read(FontColor, SizeOf(FontColor));
    Read(StrLen,SizeOf(Integer));
    SetLength(FontName, StrLen);
    Read(FontName[1],StrLen);
    Read(StrLen,SizeOf(Integer));
    SetLength(Content, StrLen);
    Read(Content[1],StrLen);
  end;
end;

procedure TTitle.SaveToStream(AFileStream: TFileStream);
var
  StrLen: Integer;
begin
  with AFileStream do
  begin
    Write(Distance, SizeOf(Distance));
    Write(FontSize, SizeOf(FontSize));
    Write(FontStyle, SizeOf(FontStyle));
    Write(FontColor, SizeOf(FontColor));
    StrLen := Length(FontName);
    Write(StrLen,SizeOf(Integer));
    Write(FontName[1],StrLen);
    StrLen := Length(Content);
    Write(StrLen,SizeOf(Integer));
    Write(Content[1],StrLen);
  end;
end;

initialization
  GetResolution;

finalization

end.
