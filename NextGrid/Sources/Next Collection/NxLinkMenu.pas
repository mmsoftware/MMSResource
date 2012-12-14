{$HINTS OFF}
{$WARNINGS OFF}
unit NxLinkMenu;

interface

uses
  Classes, Controls, Windows, Dialogs, SysUtils,
  NxCollection, ImgList, Graphics, Messages, NxThemesSupport,
  NxScrollControl;

const
  spaImgToText = 7;

type
  TNxLinkMenu = class;
  TSectionItems = class;
  TSectionItem = class;
  TShowingItems = set of (siLink, siShowImage);
  TSectionClickEvent = procedure(Sender: TObject; Section: Integer) of object;
  TItemClickEvent = procedure(Sender: TObject; Item: TSectionItem) of object;

  TSection = class(TCollectionItem)
  private
    FExpanded: Boolean;
    FCaption: string;
    FItems: TSectionItems;
    FGlyph: TBitmap;
    FSectionMargins: TNxMargins;
    procedure SetItems(const Value: TSectionItems);
    procedure SetExpanded(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetSectionMargins(const Value: TNxMargins);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Caption: string read FCaption write SetCaption;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Items: TSectionItems read FItems write SetItems;
    property SectionMargins: TNxMargins read FSectionMargins write SetSectionMargins;
  end;

  TSections = class(TCollection)
  private
    FOwner: TNxLinkMenu;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TNxLinkMenu);
    function Add: TSection;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  end;

  TSectionItem = class(TCollectionItem)
  private
    FCaption: string;
    FImageIndex: TImageIndex;
    FOptions: TShowingItems;
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetOptions(const Value: TShowingItems);
    function GetSection: TSection;
  public
    property Section: TSection read GetSection;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property Options: TShowingItems read FOptions write SetOptions;
  end;

  TSectionItems = class(TCollection)
  private
    FOwner: TSection;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TSection);
    function Add: TSectionItem;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  end;

  TNxLinkMenu = class(TNxScrollControl)
  private
    FSections: TSections;
    FInnerMargins: TNxMargins;
    FHederSize: integer;
    FImages: TImageList;
    FHoverSectionItem: TSectionItem;
    FItemSize: Integer;
    FOldHoverItem: TSectionItem;
    FOnItemClick: TItemClickEvent;
    FOnSectionClick: TSectionClickEvent;
    FSpacing: Integer;
    FExpanded: Boolean;
    function GetImagesWidth: Integer;
    procedure Preparing;
    procedure SetSections(const Value: TSections);
    procedure SetInnerMargins(const Value: TNxMargins);
    procedure SetHederSize(const Value: integer);
    procedure SetImage(const Value: TImageList);
    procedure SetItemSize(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
  protected
    procedure CreateWnd; override;
    function GetVertOffset(FromPos, ToPos: Integer): Integer; override;
    procedure DoItemClick(Item: TSectionItem); dynamic;
    procedure DoSectionClick(Section: Integer); dynamic;
    procedure DrawArrow(const X, Y: Integer; Expanded: Boolean); virtual;
    procedure DrawBackground; virtual;
    procedure DrawMarginBkgrnd; virtual;
    procedure DrawRectangle(const Value:TRect); virtual;
    procedure ExpandColapseSection(Expanded: Boolean; index: integer; Pos: integer); virtual;
    procedure Paint; override;
    procedure RefreshItem(Item: TSectionItem; IncludeImage: Boolean); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintSection(index,Pos: integer);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSectionItemAtPos(const X, Y: Integer): TSectionItem;
    function GetSectionItemRect(const Item: TSectionItem; IncludeImage: Boolean): TRect;
    function GetTotalHeigth: Integer;
  published
    property Align;
    property Font;
    property Sections: TSections read FSections write SetSections;
    property InnerMargins: TNxMargins read FInnerMargins write SetInnerMargins;
    property HederSize: integer read FHederSize write SetHederSize;
    property Images: TImageList read FImages write SetImage;
    property ItemSize: Integer read FItemSize write SetItemSize;
    property Spacing: Integer read FSpacing write SetSpacing;
    property VertScrollBar;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnItemClick: TItemClickEvent read FOnItemClick write FOnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSectionClick: TSectionClickEvent read FOnSectionClick write FOnSectionClick;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TNxLinkMenu }



procedure TNxLinkMenu.CMFontChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TNxLinkMenu.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoverSectionItem := nil;
  Invalidate;
end;

constructor TNxLinkMenu.Create(AOwner: TComponent);
begin
  inherited;
  FHederSize := 25;
  FInnerMargins := TNxMargins.Create;
  FItemSize := 20;
  FSections := TSections.Create(Self);
  InnerMargins.Top := 10;
  InnerMargins.Left := 10;
  InnerMargins.Right := 10;
  InnerMargins.Bottom := 10;
  FSpacing := 10;
  Width := 200;
  Height := 350;
  ParentColor := False;
  Color := clWindow;
end;

procedure TNxLinkMenu.CreateWnd;
begin
  inherited;
  HorzScrollBar.Visible := False;
  VertScrollBar.Visible := False;
end;

destructor TNxLinkMenu.Destroy;
begin
  FSections.Free;
  FInnerMargins.Free;
  inherited;
end;

procedure TNxLinkMenu.DoItemClick(Item: TSectionItem);
begin
  if Assigned(FOnItemClick) then FOnItemClick(Self, Item);
end;

procedure TNxLinkMenu.DoSectionClick(Section: Integer);
begin
  if Assigned(FOnSectionClick) then FOnSectionClick(Self, Section);
end;

procedure TNxLinkMenu.DrawArrow(const X, Y: Integer; Expanded: Boolean);
begin
  inherited;
  if Expanded then
  begin
    Canvas.Pen.Color := clBlue;
    Canvas.MoveTo(X,Y);
    Canvas.LineTo(X-3,Y-3);
    Canvas.LineTo(X-7,Y+1);
    Canvas.MoveTo(X,Y+5);
    Canvas.LineTo(X-3,Y+2);
    Canvas.LineTo(X-7,Y+6);
  end else
  begin
    Canvas.Pen.Color := clBlue;
    Canvas.MoveTo(X,Y-3);
    Canvas.LineTo(X-3,Y);
    Canvas.LineTo(X-7,Y-4);
    Canvas.MoveTo(X,Y+2);
    Canvas.LineTo(X-3,Y+5);
    Canvas.LineTo(X-7,Y+1);
  end;
end;


procedure TNxLinkMenu.DrawBackground;
var
  i,j,Pos : integer;
  DrawingSection : TSection;
  ExpandedSection : integer;
begin
  inherited;
  if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(0,0,Width,Height),'explorerbar',0,1)
    else DrawMarginBkgrnd;
  Pos := InnerMargins.Top;
  for i := 0 to FSections.Count-1 do
  begin
    DrawingSection := TSection(FSections.Items[i]);
    ExpandedSection := (FItemSize * TSection(FSections.Items[i]).Items.Count)+ DrawingSection.SectionMargins.Top + DrawingSection.SectionMargins.Bottom;
    if DrawingSection.Expanded then
    begin
      Pos := Pos + (FHederSize + ExpandedSection);
    end else
        begin
          Pos := Pos + FHederSize;
          if i = FSections.Count-1 then Break;
        end;
    if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(FInnerMargins.Left,Pos,Width-FInnerMargins.Right,Pos+FSpacing+2),'explorerbar',1,1)
      else
      begin
        DrawRectangle(Rect(FInnerMargins.Left,Pos,Width-FInnerMargins.Right, Pos+FSpacing));
      end;
    Pos := Pos + FSpacing;
  end;
  if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(FInnerMargins.Left,Pos,Width-FInnerMargins.Right,Height),'explorerbar',1,1)
      else
      begin
        DrawRectangle(Rect(FInnerMargins.Left,Pos,Width-FInnerMargins.Right,Height));
      end;
end;


procedure TNxLinkMenu.DrawMarginBkgrnd;
begin
  if IsThemed then
  begin
    ThemeRect(Handle,Canvas.Handle,Rect(0,0,FInnerMargins.Left,Height),'explorerbar',1,2);
    ThemeRect(Handle,Canvas.Handle,Rect(Width - FInnerMargins.Right,0,Width,Height),'explorerbar',1,2);
    ThemeRect(Handle,Canvas.Handle,Rect(FInnerMargins.Left,0,Width-FInnerMargins.Right,FInnerMargins.Top+2),'explorerbar',1,2);
  end
    else
    begin
      DrawRectangle(Rect(0,0,FInnerMargins.Left,Height));
      DrawRectangle(Rect(Width-FInnerMargins.Right,0,Width,Height));
      DrawRectangle(Rect(FInnerMargins.Left,0,Width-FInnerMargins.Right,FInnerMargins.Top));
    end;
end;

procedure TNxLinkMenu.DrawRectangle(const Value: TRect);
begin
  Canvas.Brush.Color := clWindow;
  Canvas.FillRect(Value);
end;



procedure TNxLinkMenu.ExpandColapseSection(Expanded: Boolean; index: integer; Pos: integer);
var
  SectionExpansion, GlyphPos: integer;
  ClpRect: TRect;
begin
  if TSection(FSections.Items[index]).Items.Count = 0 then SectionExpansion := 0 else
  SectionExpansion := (FItemSize * TSection(FSections.Items[index]).Items.Count) + TSection(Sections.Items[index]).SectionMargins.Top + TSection(Sections.Items[index]).SectionMargins.Bottom;
  ClpRect := Rect(FInnerMargins.Left,Pos+FHederSize,Width-FInnerMargins.Right,Height);
  GlyphPos := (FHederSize div 2)-10;
  if Expanded = True then
  begin
    ScrollWindowEx(Handle,0,-SectionExpansion,nil,@ClpRect,0,@ClpRect,SW_INVALIDATE);
    TSection(FSections.Items[index]).Expanded := not TSection(FSections.Items[index]).Expanded;
    if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(Width-(FInnerMargins.Right+25),Pos+GlyphPos+2,Width-(FInnerMargins.Right+5),Pos+GlyphPos+22),'explorerbar',7,1)
      else
        begin
          Canvas.Brush.Color := clBtnFace;
          FillRect(Canvas.Handle,Rect(Width-(FInnerMargins.Right+20),Pos,Width-(FInnerMargins.Right+5),Pos+FHederSize),Canvas.Brush.Handle);
          DrawArrow(Width-(InnerMargins.Right+10),Pos+((FHederSize div 2)-1),False);
        end;
    FExpanded := True;
    Exit;
  end else
      begin
        ScrollWindowEx(Handle,0,SectionExpansion,nil,@ClpRect,0,@ClpRect,SW_INVALIDATE);
        TSection(FSections.Items[index]).Expanded := not TSection(FSections.Items[index]).Expanded;
        if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(Width-(FInnerMargins.Right+25),Pos+GlyphPos+2,Width-(FInnerMargins.Right+5),Pos+GlyphPos+22),'explorerbar',6,1)
          else
            begin
              Canvas.Brush.Color := clBtnFace;
              FillRect(Canvas.Handle,Rect(Width-(FInnerMargins.Right+20),Pos,Width-(FInnerMargins.Right+5),Pos+FHederSize),Canvas.Brush.Handle);
              DrawArrow(Width-(InnerMargins.Right+10),Pos+((FHederSize div 2)-1),True);
            end;
        FExpanded := True;
        Exit;
      end;

end;

function TNxLinkMenu.GetImagesWidth: Integer;
begin
  Result := 0;
  if Images <> nil then
  begin
    Result := Images.Width;
    Result := Result + spaImgToText;
  end;
end;

function TNxLinkMenu.GetSectionItemAtPos(const X, Y: Integer): TSectionItem;
var
  i, j, index, Pos, AWidth: integer;
  DrawingSection: TSection;
  DrawingItem: TSectionItem;
begin
  inherited;
  Result := nil;
  index := -1;
  Pos := FInnerMargins.Top - VertScrollBar.Position;
  for i := 0 to FSections.Count-1 do
  begin
    DrawingSection := TSection(FSections.Items[i]);
    if DrawingSection.Expanded then
    begin
      index := i;
      Pos := Pos + FHederSize;
      if (X < InnerMargins.Left +DrawingSection.SectionMargins.Left) or (X > Width - (InnerMargins.Right)) then Exit;
      for j := 0 to TSection(FSections.Items[i]).Items.Count-1 do
      begin
        if j = 0 then
        begin
          Pos := Pos + DrawingSection.FSectionMargins.Top;
        end;
        DrawingItem := TSectionItem(DrawingSection.Items.Items[j]);
        if siShowImage in DrawingItem.Options
          then AWidth := InnerMargins.Left + DrawingSection.FSectionMargins.Left + GetImagesWidth + Canvas.TextWidth(DrawingItem.Caption)
          else AWidth := InnerMargins.Left + DrawingSection.FSectionMargins.Left + Canvas.TextWidth(DrawingItem.Caption);

        if (Y > Pos + 5) and (Y < Pos + (FItemSize - 5)) and (X < AWidth) then
        begin
          // Result je ugradjena promenjiva koja ustvari
          // predstavlja rezultat funkcije
          Result := DrawingItem;
          Exit;
        end;
        Pos := Pos + FItemSize;
        if (j = DrawingSection.Items.Count-1) then Pos := Pos + DrawingSection.SectionMargins.Bottom;
      end;
    end else if (i = FSections.Count-1) and (j = DrawingSection.Items.Count-1) then Break else Pos := Pos + HederSize; { expanded }
    Pos := Pos + FSpacing;
  end;
end;


function TNxLinkMenu.GetSectionItemRect(const Item: TSectionItem; IncludeImage: Boolean): TRect;
var
  i, j, Pos, index, ImgX, ResultX, ImgWidth: integer;
  DrawingRect: TRect;
  DrawingItem: TSectionItem;
  DrawingSection: TSection;
begin
  inherited;
  index := -1;
  Pos := FInnerMargins.Top - VertScrollBar.Position;
  ImgWidth := 0;
  if Images <> nil
    then ImgWidth := Images.Width + spaImgToText;

  for i := 0 to FSections.Count-1 do
  begin
    if TSection(FSections.Items[i]).Expanded then
    begin
      DrawingSection := TSection(FSections.Items[i]);
      index := i;
      Pos := Pos + HederSize;
        for j := 0 to TSection(FSections.Items[i]).Items.Count-1 do
        begin
          if j = 0 then
          begin
            Pos := Pos + DrawingSection.SectionMargins.Top;
          end;
          DrawingItem := TSectionItem(DrawingSection.Items.Items[j]);
          if Item = DrawingItem then
          begin
            ResultX := InnerMargins.Left + DrawingSection.SectionMargins.Left+1;
            if siShowImage in DrawingItem.Options then ResultX := ResultX + ImgWidth;
            Result := Rect(ResultX, Pos, ResultX + Canvas.TextWidth(DrawingItem.Caption)+5, Pos + FItemSize);
            Exit;
          end;
          Pos := Pos + FItemSize;
          if j = DrawingSection.Items.Count-1 then Pos := Pos + DrawingSection.FSectionMargins.Bottom;
        end;
        end else Pos := Pos + HederSize;
        Pos := Pos + FSpacing;
  end;
end;

function TNxLinkMenu.GetTotalHeigth: Integer;
var
  i,j: Integer;
begin
  Result := FInnerMargins.Top;
  for i := 0 to FSections.Count-1 do
  begin
    if TSection(FSections.Items[i]).Expanded then
    begin
      Result := Result + FHederSize;
      Result := Result + TSection(FSections.Items[i]).FSectionMargins.Top;
      for j := 0 to TSection(FSections.Items[i]).Items.Count-1 do Result := Result + FItemSize;
      Result := Result + TSection(FSections.Items[i]).FSectionMargins.Bottom;
    end else Result := Result + FHederSize;
    if i = FSections.Count-1 then Result := Result + FInnerMargins.Bottom
      else Result := Result + FSpacing;
  end;
end;

function TNxLinkMenu.GetVertOffset(FromPos, ToPos: Integer): Integer;
begin
  Result := 0;
  case VertScrollBar.ScrollKind of
    rkThumb, rkPage, rkTopBottom: Result := FromPos - ToPos;
    rkLine: Result := FromPos - ToPos;
  end;
  //boki: ovde dakle se kazuje za koliko pixela se pomera
end;

procedure TNxLinkMenu.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TNxLinkMenu.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  TargetItem: TSectionItem;
begin
  inherited;
  TargetItem := GetSectionItemAtPos(X, Y);
  if TargetItem <> nil then
  begin
    FHoverSectionItem := TargetItem;
    if TargetItem <> FOldHoverItem then
    begin
      if FOldHoverItem <> nil then RefreshItem(FOldHoverItem, False);
      RefreshItem(TargetItem, False);
      FOldHoverItem := TargetItem;
    end;
  end else
  begin
    FHoverSectionItem := nil;
    if FOldHoverItem <> nil then RefreshItem(FOldHoverItem, False);
    FOldHoverItem := nil;
  end;
end;

procedure TNxLinkMenu.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i,j, Pos, index : integer;
  DrawingSection: TSection;
  DrawingItem: TSectionItem;
begin
  inherited;
  FExpanded := False;
  index := -1;
  if (x < InnerMargins.Left) or (x > Width-InnerMargins.Right) then Exit;
  Pos := FInnerMargins.Top - VertScrollBar.Position;
  for i := 0 to FSections.Count - 1 do
    begin
      if (y > Pos) and (y < Pos + HederSize) then
        begin
          index := i;
          DoSectionClick(i);
          ExpandColapseSection(TSection(FSections.Items[i]).Expanded,index,Pos);
          if FExpanded = True then Exit;
          Break;
        end;
      DrawingSection := TSection(FSections.Items[i]);
      if DrawingSection.Expanded then
        begin
          Pos := Pos + HederSize;
          for j := 0 to TSection(FSections.Items[i]).Items.Count-1 do
            begin
              if j = 0 then Pos := Pos + DrawingSection.FSectionMargins.Top;
              DrawingItem := TSectionItem(DrawingSection.Items.Items[j]);
              Pos := Pos + FItemSize;
              if DrawingItem = FHoverSectionItem then DoItemClick(DrawingItem);
              if j = DrawingSection.Items.Count-1 then Pos := Pos + DrawingSection.FSectionMargins.Bottom;
            end;
            Pos := Pos + FSpacing;
        end else Pos := Pos + FHederSize + FSpacing;
    end;
  if index > -1 then
  begin
    TSection(FSections.Items[i]).Expanded := not TSection(FSections.Items[i]).Expanded;
    Invalidate;
  end;
end;

procedure TNxLinkMenu.Paint;
const
  ScrollWidth = 10;
var
  I, j, Pos, ExpHeigth, TextY, ImgX, Glyph, MarginEnlargment, GlyphPos, ImagePos: Integer;
  DrawingSection: TSection;
  DrawingItem: TSectionItem;
  TotalHeigth, RightMargin: Integer;
  Bigger: Boolean;
begin
  inherited;
  VertScrollClipRect := ClientRect; //boki: Ovde kazujemo koji deo komponente moze da se skroluje
  Preparing;
  TotalHeigth := GetTotalHeigth;
  RightMargin := FInnerMargins.Right;
  if TotalHeigth > ClientHeight then
  begin
    VertScrollBar.Visible := True;
    VertScrollBar.Max := TotalHeigth - ClientHeight;
    FInnerMargins.Right := 27;
  end
    else
    begin
      VertScrollBar.Visible := False;
      VertScrollBar.Max := GetTotalHeigth - ClientHeight;
      FInnerMargins.Right := 10;
    end;
  Pos := InnerMargins.Top - VertScrollBar.Position;
  for I := 0 to FSections.Count - 1 do
  begin
    Glyph := 0;
    PaintSection(i,pos);     {drawing heder}
    DrawingSection := TSection(FSections.Items[i]);
    if (DrawingSection.FGlyph <> nil) and (not DrawingSection.FGlyph.Empty) then Glyph := DrawingSection.Glyph.Width;
    TextY := FHederSize div 2 - Canvas.TextHeight(DrawingSection.Caption) div 2;
    with Canvas do
    begin
      Draw(FInnerMargins.Left + 5, Pos + FHederSize div 2 - DrawingSection.Glyph.Height div 2, DrawingSection.Glyph);
      Brush.Style := bsClear;
      TextOut(InnerMargins.Left+10+Glyph, Pos+ TextY, DrawingSection.Caption);
      Brush.Style := bsSolid;
    end;
    GlyphPos := (FHederSize div 2)-10;
    if DrawingSection.Expanded then
    begin
      if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(Width-(FInnerMargins.Right+25),Pos+GlyphPos+2,Width-(FInnerMargins.Right+5),Pos+GlyphPos+22),'explorerbar',6,1)
        else DrawArrow(Width-(InnerMargins.Right+10),Pos+((FHederSize div 2)-1),True)
    end else if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(Width-(FInnerMargins.Right+25),Pos+GlyphPos+2,Width-(FInnerMargins.Right+5),Pos+GlyphPos+22),'explorerbar',7,1)
               else DrawArrow(Width-(InnerMargins.Right+10),Pos+((FHederSize div 2)-1),False);

    ExpHeigth := (DrawingSection.Items.Count * FItemSize);

    { Drawing Items }
    if DrawingSection.Expanded then
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Pen.Color := clBtnFace;
      Pos := Pos + HederSize;
      if DrawingSection.Items.Count = 0 then MarginEnlargment := 0 else MarginEnlargment := DrawingSection.SectionMargins.Top + DrawingSection.SectionMargins.Bottom;
      if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(InnerMargins.Left,Pos,Width-InnerMargins.Right,pos+ExpHeigth+MarginEnlargment),'explorerbar',5,1)
        else Canvas.Rectangle(InnerMargins.Left,Pos,Width-InnerMargins.Right,pos+ExpHeigth+MarginEnlargment);
      for j := 0 to DrawingSection.Items.Count-1 do
      begin
        DrawingItem := TSectionItem(DrawingSection.Items.Items[j]);
        TextY := FItemSize div 2 - Canvas.TextHeight(DrawingItem.Caption) div 2;
        ImagePos := (FHederSize-Font.Size) div 3;
        ImgX := FInnerMargins.Left + DrawingSection.SectionMargins.Left + 1;
        if Images <> nil then
        if siShowImage in DrawingItem.Options then       { drawing item image}
        begin
          if j = 0 then Images.Draw(Canvas,imgX, Pos + ImagePos +3, DrawingItem.ImageIndex)
            else Images.Draw(Canvas, ImgX, Pos+ImagePos,DrawingItem.ImageIndex);
          ImgX := ImgX + Images.Width + spaImgToText;
        end;
        if DrawingItem = FHoverSectionItem then
          if siLink in DrawingItem.Options then
            Canvas.Font.Style := [fsUnderline];
        if j = 0 then
        begin
          Pos := Pos + DrawingSection.SectionMargins.Top;
        end;
        Canvas.Brush.Style := bsClear;
        Canvas.TextOut(ImgX, Pos+ TextY, DrawingItem.Caption);
        Canvas.Brush.Style := bsSolid;
        if j = DrawingSection.Items.Count-1 then Pos := Pos + DrawingSection.SectionMargins.Bottom;
        Canvas.Font.Style := [];
        Pos := Pos + FItemSize;
      end;
      if (i = FSections.Count-1) and (j = DrawingSection.Items.Count-1) then Break;
      Pos := Pos + FSpacing;
    end else if (i = FSections.Count-1) and (j = DrawingSection.Items.Count-1) then Break else Pos := Pos + FHederSize + FSpacing;
  end;
  if IsThemed then
  begin
    Pos := FInnerMargins.Top - VertScrollBar.Position;
    for i := 0 to FSections.Count - 1 do
    begin
      DrawingSection := TSection(FSections.Items[i]);

      if DrawingSection.Items.Count = 0
        then ExpHeigth := 0
      else ExpHeigth := (DrawingSection.Items.Count * FItemSize) + DrawingSection.SectionMargins.Top + DrawingSection.SectionMargins.Bottom;

      if DrawingSection.Expanded then
      begin
        ExcludeClipRect(Canvas.Handle, FInnerMargins.Left + 2, Pos, Width - (FInnerMargins.Right + 2), Pos + 1);
        ExcludeClipRect(Canvas.Handle, FInnerMargins.Left + 1, Pos + 1,Width - (FInnerMargins.Right + 1),Pos + 2);
        ExcludeClipRect(Canvas.Handle, FInnerMargins.Left,Pos + 2,Width-(FInnerMargins.Right), Pos + FHederSize + ExpHeigth);
        Pos := Pos + FHederSize + ExpHeigth;
      end else
      begin
        ExcludeClipRect(Canvas.Handle, FInnerMargins.Left + 2, Pos, Width - (FInnerMargins.Right+2), Pos + 1);
        ExcludeClipRect(Canvas.Handle, FInnerMargins.Left + 1, Pos + 1, Width - (FInnerMargins.Right+1), Pos + 2);
        ExcludeClipRect(Canvas.Handle, FInnerMargins.Left, Pos+2,Width-(FInnerMargins.Right),Pos+FHederSize);
        Pos := Pos + FHederSize;
      end;
      Pos := Pos + FSpacing;
    end;
    ThemeRect(Handle,Canvas.Handle,ClientRect,'explorerbar', 1, 0);
  end else DrawBackground;
end;

procedure TNxLinkMenu.PaintSection(index, Pos: integer);
begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Color := clBtnFace;
    if IsThemed then ThemeRect(Handle,Canvas.Handle,Rect(InnerMargins.Left,Pos, Width-InnerMargins.Right, Pos+HederSize),'explorerbar',8,1)
      else Canvas.Rectangle(InnerMargins.Left,Pos, Width-InnerMargins.Right, Pos+HederSize);
end;


procedure TNxLinkMenu.Preparing;
begin
  FHederSize := Font.Size + 17;
  FItemSize := Font.Size + 12;
  Canvas.Font.Assign(Font);
end;

procedure TNxLinkMenu.RefreshItem(Item: TSectionItem; IncludeImage: Boolean);
var
  R: TRect;
begin
  R := GetSectionItemRect(Item, IncludeImage);
  InvalidateRect(Handle, @R, False);
end;

procedure TNxLinkMenu.SetHederSize(const Value: integer);
begin
  FHederSize := Value;
  Invalidate;
end;

procedure TNxLinkMenu.SetImage(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;


procedure TNxLinkMenu.SetInnerMargins(const Value: TNxMargins);
begin
  FInnerMargins := Value;
end;

procedure TNxLinkMenu.SetItemSize(const Value: Integer);
begin
  FItemSize := Value;
  Invalidate;
end;


procedure TNxLinkMenu.SetSections(const Value: TSections);
begin
  FSections := Value;
end;


procedure TNxLinkMenu.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
end;

procedure TNxLinkMenu.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

{ TSections }

function TSections.Add: TSection;
begin
  Result := TSection(inherited Add);
end;

constructor TSections.Create(AOwner: TNxLinkMenu);
begin
  inherited Create(TSection);
  FOwner := AOwner;
end;

function TSections.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSections.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  FOwner.Invalidate;
end;

{ TSection }

constructor TSection.Create(Collection: TCollection);
begin
  inherited;
  FItems := TSectionItems.Create(Self);
  FGlyph := TBitmap.Create;
  FSectionMargins := TNxMargins.Create;
  FSectionMargins.Left := 5;
  FSectionMargins.Right := 5;
  FSectionMargins.Top := 5;
  FSectionMargins.Bottom := 5;
end;

destructor TSection.Destroy;
begin
  FItems.Free;
  FGlyph.Free;
  FSectionMargins.Free;
  inherited;
end;

procedure TSection.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TSection.SetExpanded(const Value: Boolean);
begin
  FExpanded := Value;
end;

procedure TSection.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TSection.SetItems(const Value: TSectionItems);
begin
  FItems := Value;
end;

procedure TSection.SetSectionMargins(const Value: TNxMargins);
begin
  FSectionMargins := Value;
end;

{ TSectionItem }

function TSectionItem.GetSection: TSection;
begin
  Result := TSectionItems(Collection).FOwner;
end;

procedure TSectionItem.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TSectionItem.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
end;

procedure TSectionItem.SetOptions(const Value: TShowingItems);
begin
  FOptions := Value;
end;

{ TSectionItems }

function TSectionItems.Add: TSectionItem;
begin
  Result := TSectionItem(inherited Add);
end;

constructor TSectionItems.Create(AOwner: TSection);
begin
  inherited Create(TSectionItem);
  FOwner := AOwner;
end;

function TSectionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSectionItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  TSections(FOwner.Collection).FOwner.Invalidate;
end;

end.
{$HINTS ON}
{$WARNINGS ON}
