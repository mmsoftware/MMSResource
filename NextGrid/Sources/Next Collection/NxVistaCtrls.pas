{
  Next Collection
  Copyright (C) 1996-2005 by Berg
  All rights reserved.

  $id:NxVistaCtrls.pas bn
}

unit NxVistaCtrls;

interface

uses
  Classes, Types, Messages, Controls, Windows, Graphics, ImgList, Menus, Forms,
  NxClasses;

type
  TNxButtonAlignment = (baLeft, baRight);

  TNxVEditButton = class
  private
    FPosition: Integer;
    FAlignment: TNxButtonAlignment;
    procedure SetPosition(const Value: Integer);
    procedure SetAlignment(const Value: TNxButtonAlignment);
  published
    property Alignment: TNxButtonAlignment read FAlignment write SetAlignment;
    property Position: Integer read FPosition write SetPosition;
  end;

  TNxEditControl = class(TCustomControl)
  private
    FCursorPos: Integer;
    FEditRect: TRect;
    FGlyph: TBitmap;
    FPosition: Integer;
    FText: WideString;
    FTextWidth: Integer;
    FSelStart: Integer;
    FSelLength: Integer;
    FBorderStyle: TBorderStyle;
    FAlignment: TAlignment;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetText(const Value: WideString);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelLength(const Value: Integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetAlignment(const Value: TAlignment);
  protected
    procedure AdjustEditRect(var AEditRect: TRect); virtual;
    procedure CreateWnd; override;
    procedure DeleteSelection(Backspace: Boolean = true);
    procedure DrawBorder; virtual;
    procedure DrawEditText; virtual;
    procedure DrawGlyph; virtual;
    function GetButtonsCount: Integer; virtual;
    function GetCharWidth(AChar: WideChar): Integer; virtual;
//    function GetCharRect(const Index: Integer): TRect;
    function GetEditSize: TSize;
    function GetTextWidth(AText: WideString): Integer; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeleteText(const Index, Count: Integer);
    function GetCharRect(const Index: Integer): TRect;
    procedure InsertText(const Pos: Integer; AText: WideString);
    property SelLength: Integer read FSelLength write SetSelLength;
    property SelStart: Integer read FSelStart write SetSelStart;
  published
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
  end;

  TNxTextBox = class(TNxEditControl)
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Anchors;
    property BidiMode;
    property Canvas;
    property Color default clWindow;
    property Constraints;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Text: WideString read FText write SetText;
  end;

implementation

uses
  Math, Dialogs, SysUtils, NxSharedCommon, NxSharedDraw, NxThemesSupport;

{ Unicode }

function CodePageFromLocale(Language: LCID): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

function KeyUnicode(C: Char): WideChar;
begin
  MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @C, 1, @Result, 1);
end;

{ TNxVEditButton }

procedure TNxVEditButton.SetAlignment(const Value: TNxButtonAlignment);
begin
  FAlignment := Value;
end;

procedure TNxVEditButton.SetPosition(const Value: Integer);
begin
  FPosition := Value;
end;

{ TNxEditControl }

procedure TNxEditControl.AdjustEditRect(var AEditRect: TRect);
const
  spGlyphToText = 2;
begin
  FEditRect := ClientRect;
  if BorderStyle = bsSingle then
    InflateRect(FEditRect, -2, -2);
  if not FGlyph.Empty then Inc(FEditRect.Left, 2 + FGlyph.Width + spGlyphToText);
end;

constructor TNxEditControl.Create(AOwner: TComponent);
begin
  inherited;
  FGlyph := TBitmap.Create;
  FPosition := 0;
  FSelStart := 0;
  FTextWidth := 0;
  ParentColor := False;
  Color := clWindow;
end;

procedure TNxEditControl.CreateWnd;
begin
  inherited;
  AdjustEditRect(FEditRect);
end;

procedure TNxEditControl.DeleteSelection(Backspace: Boolean);
begin

end;

procedure TNxEditControl.DeleteText(const Index, Count: Integer);
var
  W: Integer;
begin
  W := GetTextWidth(Copy(FText, Index, Count));  
end;

procedure TNxEditControl.DrawBorder;
var
  BorderRect: TRect;
begin
  if IsThemed and (BorderStyle = bsSingle) then
  begin
    BorderRect := ClientRect;
    ExcludeClipRect(Canvas.Handle, FEditRect.Left, FEditRect.Top,
      FEditRect.Right, FEditRect.Bottom);
    ThemeRect(Handle, Canvas.Handle, BorderRect, teEdit, 1, 0);
  end;
end;

procedure TNxEditControl.DrawEditText;
var
  i, X, CharWidth: Integer;
  CharRect: TRect;
  AChar: WideChar;
begin
  X := FEditRect.Left + FPosition;
  SetClipRect(Canvas, FEditRect);
  for i := 1 to Length(FText) do
  begin
    AChar := FText[i];
    CharWidth := GetCharWidth(AChar);
    CharRect := Rect(X, FEditRect.Top, X + CharWidth, FEditRect.Bottom);
    DrawText(Canvas, CharRect, taLeftJustify, AChar, True);

    Inc(X, CharWidth);
    if i = FSelStart then
    begin
      Canvas.MoveTo(X, FEditRect.Top);
      Canvas.LineTo(X, FEditRect.Right);
    end;
  end;
  SetClipRect(Canvas, ClientRect);
end;

procedure TNxEditControl.DrawGlyph;
var
  X, Y: Integer;
begin
  if Assigned(FGlyph) and not FGlyph.Empty then
  begin
    X := 2;
    Y := ClientHeight div 2 - FGlyph.Height div 2;
    DrawBitmap(Canvas, X, Y, FGlyph, Color);
  end;
end;

function TNxEditControl.GetButtonsCount: Integer;
begin
  Result := 0;
end;

function TNxEditControl.GetCharRect(const Index: Integer): TRect;
var
  X: Integer;
begin
  X := FEditRect.Left - FPosition;
  Inc(X, GetTextWidth(Copy(FText, 0, Index)));
  Result := Rect(X, 0, X + GetCharWidth(FText[Index]), ClientHeight);
end;

function TNxEditControl.GetCharWidth(AChar: WideChar): Integer;
var
  Size: TSize;
begin
  Size.cX := 0;
  if IsUnicodeSupported then
  begin
    GetTextExtentPoint32W(Canvas.Handle, @AChar, 1, Size);
    Result := Size.cx;
  end else Result := Canvas.TextWidth(AChar);
end;

function TNxEditControl.GetEditSize: TSize;
begin
  with Result do
  begin
    cx := FEditRect.Right - FEditRect.Left;
    cy := FEditRect.Bottom - FEditRect.Top;
  end;
end;

function TNxEditControl.GetTextWidth(AText: WideString): Integer;
var
  Size: TSize;
begin
  Size.cX := 0;
  if IsUnicodeSupported then
  begin
    GetTextExtentPoint32W(Canvas.Handle, PWideChar(AText), Length(AText), Size);
    Result := Size.cx;
  end else Result := Canvas.TextWidth(AText);
end;

procedure TNxEditControl.InsertText(const Pos: Integer; AText: WideString);
var
  ATextWidth: Integer;
begin
  Insert(AText, FText, Pos + 1);
  FSelStart := Pos + Length(AText);
  ATextWidth := GetTextWidth(AText);
  Inc(FTextWidth, ATextWidth);
  FPosition := GetEditSize.cx - (FTextWidth + 4);
  if FPosition > 0 then FPosition := 0;
  Invalidate;
end;

procedure TNxEditControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_LEFT then SelStart := SelStart - 1;
  if Key = VK_RIGHT then SelStart := SelStart + 1;
  if Key = VK_BACK then SelStart := SelStart - 1;
  Invalidate;
end;

procedure TNxEditControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  SetFocus;
end;

procedure TNxEditControl.Paint;
begin
  inherited;
  Canvas.Font := Font;
//  DrawBorder;

  Canvas.Brush.Color := clYellow;
  Canvas.FillRect(FEditRect);

  DrawGlyph;

  DrawEditText;
  Canvas.TextOut(0, 0, IntToStr(FSelStart));
  Canvas.TextOut(50, 0, IntToStr(FPosition));
end;

procedure TNxEditControl.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
end;

procedure TNxEditControl.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
end;

procedure TNxEditControl.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TNxEditControl.SetSelLength(const Value: Integer);
begin
  FSelLength := Value;
end;

procedure TNxEditControl.SetSelStart(const Value: Integer);
begin
  FSelStart := Value;
end;

procedure TNxEditControl.SetText(const Value: WideString);
begin
  FText := Value;
end;

procedure TNxEditControl.WMChar(var Message: TWMChar);
var
  cw: WideChar;
  ch: Char;
begin
  if Message.CharCode > 8 then
  begin
    ch := Chr(Message.CharCode);
    cw := KeyUnicode(Ch);

    InsertText(FSelStart, Cw);
  end;
end;

procedure TNxEditControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
