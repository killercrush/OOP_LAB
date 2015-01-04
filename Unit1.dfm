object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 644
  ClientWidth = 678
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 40
    Width = 505
    Height = 489
    AutoSize = False
    Color = clWhite
    ParentColor = False
    PopupMenu = PopupMenu1
    Transparent = False
    OnMouseUp = Label1MouseUp
  end
  object Panel1: TPanel
    Left = 0
    Top = 608
    Width = 678
    Height = 36
    Align = alBottom
    TabOrder = 0
    object Edit1: TEdit
      Left = 5
      Top = 6
      Width = 247
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object Button1: TButton
      Left = 592
      Top = 6
      Width = 81
      Height = 25
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1089#1077
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 258
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 2
    end
    object Button3: TButton
      Left = 339
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Button3'
      TabOrder = 3
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 678
    Height = 19
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 63
    Caption = #1042#1099#1076#1077#1083#1080#1090#1100
    List = True
    ShowCaptions = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100
      Grouped = True
      ImageIndex = 0
      Style = tbsCheck
    end
    object ToolButton2: TToolButton
      Left = 63
      Top = 0
      Caption = #1058#1086#1095#1082#1072
      Grouped = True
      ImageIndex = 1
      Style = tbsCheck
    end
    object ToolButton3: TToolButton
      Left = 126
      Top = 0
      Caption = #1050#1088#1091#1075
      Grouped = True
      ImageIndex = 2
      Style = tbsCheck
    end
    object ToolButton4: TToolButton
      Left = 189
      Top = 0
      Caption = #1050#1074#1072#1076#1088#1072#1090
      Grouped = True
      ImageIndex = 3
      Style = tbsCheck
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 584
    Top = 32
    object N1: TMenuItem
      Caption = #1055#1077#1088#1077#1084#1077#1089#1090#1080#1090#1100' '#1089#1102#1076#1072
      OnClick = N1Click
    end
    object N2: TMenuItem
      Caption = #1053#1072#1088#1080
      OnClick = N2Click
    end
  end
end
