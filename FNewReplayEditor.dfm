object NewReplayEditorForm: TNewReplayEditorForm
  Left = 0
  Top = 0
  Caption = 'NewReplayEditorForm'
  ClientHeight = 354
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    635
    354)
  PixelsPerInch = 96
  TextHeight = 13
  object btnButton3: TButton
    Left = 183
    Top = 321
    Width = 83
    Height = 25
    Anchors = [akBottom]
    Caption = 'Placeholder'
    TabOrder = 2
    OnClick = btnButton3Click
  end
  object btnButton2: TButton
    Left = 94
    Top = 321
    Width = 83
    Height = 25
    Anchors = [akBottom]
    Caption = 'Placeholder'
    TabOrder = 1
    OnClick = btnButton2Click
  end
  object btnButton1: TButton
    Left = 5
    Top = 321
    Width = 83
    Height = 25
    Anchors = [akBottom]
    Caption = 'Placeholder'
    TabOrder = 0
    OnClick = btnButton1Click
  end
  object gbMetadata: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 305
    Caption = 'Metadata'
    TabOrder = 3
    object lblMetadataPlayerName: TLabel
      Left = 16
      Top = 17
      Width = 60
      Height = 13
      Caption = 'Player Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblMetadataLevelID: TLabel
      Left = 16
      Top = 57
      Width = 39
      Height = 13
      Caption = 'Level ID'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblMetadataLevelTitle: TLabel
      Left = 16
      Top = 97
      Width = 48
      Height = 13
      Caption = 'Level Title'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblMetadataLevelAuthor: TLabel
      Left = 16
      Top = 137
      Width = 61
      Height = 13
      Caption = 'Level Author'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblMetadataLevelPack: TLabel
      Left = 16
      Top = 177
      Width = 50
      Height = 13
      Caption = 'Level Pack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblMetadataLevelGroup: TLabel
      Left = 16
      Top = 217
      Width = 79
      Height = 13
      Caption = 'Level Sub-Group'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblMetadataLevelNumber: TLabel
      Left = 16
      Top = 257
      Width = 36
      Height = 13
      Caption = 'Level #'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblMetadataLevelVersion: TLabel
      Left = 96
      Top = 257
      Width = 63
      Height = 13
      Caption = 'Level Version'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object ebMetadataPlayerName: TEdit
      Tag = 1
      Left = 16
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'Placeholder'
    end
    object ebMetadataLevelID: TEdit
      Tag = 2
      Left = 16
      Top = 72
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'Placeholder'
    end
    object ebMetadataLevelTitle: TEdit
      Tag = 3
      Left = 16
      Top = 112
      Width = 153
      Height = 21
      TabOrder = 2
      Text = 'Placeholder'
    end
    object ebMetadataLevelAuthor: TEdit
      Tag = 4
      Left = 16
      Top = 152
      Width = 153
      Height = 21
      TabOrder = 3
      Text = 'Placeholder'
    end
    object ebMetadataLevelPack: TEdit
      Tag = 5
      Left = 16
      Top = 192
      Width = 153
      Height = 21
      TabOrder = 4
      Text = 'Placeholder'
    end
    object ebMetadataLevelGroup: TEdit
      Tag = 6
      Left = 16
      Top = 232
      Width = 153
      Height = 21
      TabOrder = 5
      Text = 'Placeholder'
    end
    object ebMetadataLevelNumber: TEdit
      Tag = 7
      Left = 16
      Top = 272
      Width = 73
      Height = 21
      NumbersOnly = True
      TabOrder = 6
      Text = 'Placeholder'
    end
    object ebMetadataLevelVersion: TEdit
      Tag = 8
      Left = 96
      Top = 272
      Width = 73
      Height = 21
      NumbersOnly = True
      TabOrder = 7
      Text = 'Placeholder'
    end
  end
end
