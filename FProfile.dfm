object ProfileForm: TProfileForm
  Left = 1101
  Top = 487
  Anchors = []
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'NeoLemmix'
  ClientHeight = 114
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 16
    Top = 16
    Width = 54
    Height = 13
    Caption = 'User name:'
  end
  object cbName: TComboBox
    Left = 88
    Top = 13
    Width = 129
    Height = 21
    Sorted = True
    TabOrder = 0
    OnChange = cbNameChange
  end
  object cbAutoStart: TCheckBox
    Left = 16
    Top = 40
    Width = 137
    Height = 17
    Caption = 'Always start as this user'
    TabOrder = 1
    OnClick = cbAutoStartClick
  end
  object btnStart: TButton
    Left = 61
    Top = 81
    Width = 75
    Height = 25
    Caption = 'Start'
    Default = True
    TabOrder = 2
    OnClick = btnStartClick
  end
  object btnQuit: TButton
    Left = 142
    Top = 81
    Width = 75
    Height = 25
    Caption = 'Quit'
    TabOrder = 3
    OnClick = btnQuitClick
  end
  object cbTestStart: TCheckBox
    Left = 16
    Top = 58
    Width = 185
    Height = 17
    Caption = 'Start testplay mode as this user'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
end
