object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #1051#1072#1073#1072#1088#1072#1090#1086#1088#1085#1072#1103' '#1054#1054#1055
  ClientHeight = 466
  ClientWidth = 894
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnMouseUp = Form1MouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 430
    Width = 894
    Height = 36
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 608
    ExplicitWidth = 678
    object Button1: TButton
      Left = 805
      Top = 6
      Width = 81
      Height = 25
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1089#1077
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 894
    Height = 19
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 63
    Caption = #1042#1099#1076#1077#1083#1080#1090#1100
    List = True
    ShowCaptions = True
    TabOrder = 1
    Transparent = False
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
  object Panel2: TPanel
    Left = 709
    Top = 19
    Width = 185
    Height = 411
    Align = alRight
    ParentBackground = False
    TabOrder = 2
    ExplicitTop = 13
    object Label1: TLabel
      Left = 152
      Top = 87
      Width = 25
      Height = 25
      AutoSize = False
      Color = clBlack
      ParentColor = False
      Transparent = False
    end
    object Label2: TLabel
      Left = 16
      Top = 36
      Width = 35
      Height = 13
      Caption = #1056#1072#1079#1084#1077#1088
    end
    object Label3: TLabel
      Left = 26
      Top = 63
      Width = 24
      Height = 13
      Caption = #1059#1075#1086#1083
    end
    object Edit1: TEdit
      Left = 16
      Top = 6
      Width = 161
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 56
      Top = 33
      Width = 121
      Height = 21
      NumbersOnly = True
      TabOrder = 2
      Text = '50'
    end
    object Button2: TButton
      Left = 64
      Top = 87
      Width = 75
      Height = 25
      Caption = #1062#1074#1077#1090
      TabOrder = 1
      OnClick = Button2Click
    end
    object Edit3: TEdit
      Left = 56
      Top = 60
      Width = 121
      Height = 21
      NumbersOnly = True
      TabOrder = 3
      Text = '0'
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
  object ColorDialog1: TColorDialog
    Left = 600
    Top = 88
  end
end
