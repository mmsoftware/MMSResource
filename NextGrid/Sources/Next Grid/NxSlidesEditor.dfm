object SlidesForm: TSlidesForm
  Left = 508
  Top = 387
  Width = 433
  Height = 308
  Caption = 'SlidesForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SlidesDesigner: TNxSlidesDesigner
    Left = 0
    Top = 0
    Width = 425
    Height = 274
    Align = alClient
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnChange = SlidesDesignerChange
    OnSelectSlide = SlidesDesignerSelectSlide
  end
  object PopupMenu1: TPopupMenu
    Left = 76
    Top = 100
    object AlignToGri1: TMenuItem
      Caption = 'Align to &Grid'
      OnClick = AlignToGri1Click
    end
  end
end
