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

unit Preview;              

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PreviewBox, ToolWin, ComCtrls, ImgList, Printers, ExtCtrls, StdCtrls;

type
  TFormPreview = class(TForm)
    ToolBarEasyGrid: TToolBar;
    ImgColor: TImageList;
    ImgBW: TImageList;
    ToolBtnPrint: TToolButton;
    ToolBtnPageSetup: TToolButton;
    ToolBtnZoom: TToolButton;
    Sep1: TToolButton;
    ToolBtnExit: TToolButton;
    ToolBtnPrior: TToolButton;
    ToolBtnNext: TToolButton;
    Sep2: TToolButton;
    PanelZoom: TPanel;
    CbxZoom: TComboBox;
    procedure ToolBtnPrintClick(Sender: TObject);
    procedure PreviewBoxEasyGridDrawPage(DrawCanvas: TCanvas; DrawRect: TRect;
      PageIndex: Integer; Printing: Boolean);
    procedure ToolBtnPageSetupClick(Sender: TObject);
    procedure ToolBtnExitClick(Sender: TObject);
    procedure ToolBtnZoomClick(Sender: TObject);
    procedure ToolBtnPriorClick(Sender: TObject);
    procedure ToolBtnNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CbxZoomChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    ParentGrid: Pointer;
    PreviewBoxEasyGrid: TPreviewBox;
  end;

implementation
uses EasyGrid;
{$R *.DFM}

procedure TFormPreview.ToolBtnPrintClick(Sender: TObject);
begin
  TEasyGrid(ParentGrid).Print;
end;

procedure TFormPreview.PreviewBoxEasyGridDrawPage(DrawCanvas: TCanvas;
  DrawRect: TRect; PageIndex: Integer; Printing: Boolean);
begin
  TEasyGrid(ParentGrid).PrintPage(DrawCanvas, DrawRect, PageIndex, Printing);
end;

procedure TFormPreview.ToolBtnPageSetupClick(Sender: TObject);
begin
  TEasyGrid(ParentGrid).PageSetup;
end;

procedure TFormPreview.ToolBtnExitClick(Sender: TObject);
begin
  PreviewBoxEasyGrid.PriorPage;
end;

procedure TFormPreview.ToolBtnZoomClick(Sender: TObject);
begin
  PreviewBoxEasyGrid.SwitchZoom;
end;

procedure TFormPreview.ToolBtnPriorClick(Sender: TObject);
begin
  PreviewBoxEasyGrid.NextPage;
end;

procedure TFormPreview.ToolBtnNextClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPreview.FormCreate(Sender: TObject);
begin
  CbxZoom.ItemIndex := 6;
  PreviewBoxEasyGrid := TPreviewBox.Create(Self);
  PreviewBoxEasyGrid.Parent := Self;
  PreviewBoxEasyGrid.Align := alClient;
  PreviewBoxEasyGrid.OnDrawPage := PreviewBoxEasyGridDrawPage;
end;

procedure TFormPreview.CbxZoomChange(Sender: TObject);
var
  S: string;
begin
  S := CbxZoom.Text;
  Delete(S, Length(S), 1);
  if S = '100' then PreviewBoxEasyGrid.State := pbZoomIn
  else
  begin
    PreviewBoxEasyGrid.State := pbZoomOut;
    PreviewBoxEasyGrid.Zoom := StrToInt(S);
  end;
  if Self.Visible then PreviewBoxEasyGrid.SetFocus;
end;

procedure TFormPreview.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not (WS_SYSMENU);
end;

procedure TFormPreview.FormDestroy(Sender: TObject);
begin
  PreviewBoxEasyGrid.Free;
end;

end.
