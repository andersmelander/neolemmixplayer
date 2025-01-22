object FormNXConfig: TFormNXConfig
  Left = 192
  Top = 125
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'NeoLemmix Configuration'
  ClientHeight = 538
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClick = OptionChanged
  DesignSize = (
    382
    538)
  PixelsPerInch = 96
  TextHeight = 13
  object NXConfigPages: TPageControl
    Left = 0
    Top = 0
    Width = 382
    Height = 499
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      object lblUserName: TLabel
        Left = 19
        Top = 22
        Width = 56
        Height = 13
        Caption = 'Your name:'
      end
      object gbReplayNamingOptions: TGroupBox
        Left = 19
        Top = 256
        Width = 336
        Height = 119
        Caption = 'Replay Naming Options'
        TabOrder = 4
        object lblIngameSaveReplay: TLabel
          Left = 28
          Top = 56
          Width = 45
          Height = 13
          Caption = 'In-game:'
        end
        object lblPostviewSaveReplay: TLabel
          Left = 28
          Top = 83
          Width = 48
          Height = 13
          Caption = 'Postview:'
        end
        object cbAutoSaveReplay: TCheckBox
          Left = 12
          Top = 27
          Width = 72
          Height = 17
          Caption = 'Auto-save:'
          TabOrder = 0
          OnClick = cbAutoSaveReplayClick
        end
        object cbAutoSaveReplayPattern: TComboBox
          Left = 90
          Top = 26
          Width = 231
          Height = 21
          ItemIndex = 0
          TabOrder = 1
          Text = 'Position + Timestamp'
          OnChange = OptionChanged
          OnEnter = cbReplayPatternEnter
          Items.Strings = (
            'Position + Timestamp'
            'Title + Timestamp'
            'Position + Title + Timestamp')
        end
        object cbIngameSaveReplayPattern: TComboBox
          Left = 90
          Top = 53
          Width = 231
          Height = 21
          TabOrder = 2
          Text = 'Position + Timestamp'
          OnChange = OptionChanged
          OnEnter = cbReplayPatternEnter
          Items.Strings = (
            'Position + Timestamp'
            'Title + Timestamp'
            'Position + Title + Timestamp'
            '(Show file selector)')
        end
        object cbPostviewSaveReplayPattern: TComboBox
          Left = 90
          Top = 80
          Width = 231
          Height = 21
          TabOrder = 3
          Text = 'Position + Timestamp'
          OnChange = OptionChanged
          OnEnter = cbReplayPatternEnter
          Items.Strings = (
            'Position + Timestamp'
            'Title + Timestamp'
            'Position + Title + Timestamp'
            '(Show file selector)')
        end
      end
      object gbInternetOptions: TGroupBox
        Left = 19
        Top = 165
        Width = 336
        Height = 81
        Caption = 'Internet Options'
        TabOrder = 3
        object cbEnableOnline: TCheckBox
          Left = 12
          Top = 23
          Width = 153
          Height = 17
          Caption = 'Enable Online Features'
          TabOrder = 0
          OnClick = cbEnableOnlineClick
        end
        object cbUpdateCheck: TCheckBox
          Left = 12
          Top = 46
          Width = 169
          Height = 17
          Caption = 'Enable Update Check'
          TabOrder = 1
          OnClick = OptionChanged
        end
      end
      object btnHotkeys: TButton
        Left = 19
        Top = 58
        Width = 336
        Height = 42
        Caption = 'Configure Hotkeys'
        TabOrder = 1
        OnClick = btnHotkeysClick
      end
      object ebUserName: TEdit
        Left = 81
        Top = 19
        Width = 274
        Height = 21
        TabOrder = 0
      end
      object btnStyles: TButton
        Left = 19
        Top = 107
        Width = 336
        Height = 42
        Caption = 'Style Manager'
        TabOrder = 2
        OnClick = btnStylesClick
      end
      object rgGameLoading: TRadioGroup
        Left = 19
        Top = 390
        Width = 336
        Height = 71
        Caption = 'Game Loading Options'
        Items.Strings = (
          'Always Load Next Unsolved Level'
          'Always Load Most Recently Active Level')
        TabOrder = 5
        OnClick = OptionChanged
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Gameplay'
      ImageIndex = 2
      object rgWhenNoLemmings: TRadioGroup
        Left = 19
        Top = 377
        Width = 336
        Height = 85
        Caption = 'When No Lemmings Remain'
        Items.Strings = (
          'Always Exit To Postview'
          'Exit To Postview if Save Requirement Met'
          'Never Exit To Postview')
        TabOrder = 3
        OnClick = OptionChanged
      end
      object gbReplayOptions: TGroupBox
        Left = 19
        Top = 292
        Width = 336
        Height = 70
        Caption = 'Replay Options'
        TabOrder = 2
        object cbNoAutoReplay: TCheckBox
          Left = 19
          Top = 20
          Width = 234
          Height = 17
          Caption = 'Don'#39't Replay After Backwards Frameskips'
          TabOrder = 0
          OnClick = OptionChanged
        end
        object cbReplayAfterRestart: TCheckBox
          Left = 19
          Top = 43
          Width = 177
          Height = 17
          Caption = 'Auto-Replay After Restart'
          TabOrder = 1
        end
      end
      object gbHelperOptions: TGroupBox
        Left = 19
        Top = 161
        Width = 336
        Height = 119
        Caption = 'Helper Options'
        TabOrder = 1
        object cbPauseAfterBackwards: TCheckBox
          Left = 19
          Top = 22
          Width = 173
          Height = 17
          Caption = 'Pause After Backwards Skip'
          TabOrder = 0
          OnClick = OptionChanged
        end
        object cbDeactivateHelpers: TCheckBox
          Left = 19
          Top = 91
          Width = 177
          Height = 17
          Caption = 'Deactivate Helper Overlays'
          TabOrder = 3
          OnClick = OptionChanged
        end
        object cbDeactivateSkillQueue: TCheckBox
          Left = 19
          Top = 68
          Width = 193
          Height = 17
          Caption = 'Deactivate Skill Queueing'
          TabOrder = 2
          OnClick = OptionChanged
        end
        object cbHideShadows: TCheckBox
          Left = 19
          Top = 45
          Width = 153
          Height = 17
          Caption = 'Deactivate Skill Shadows'
          TabOrder = 1
          OnClick = OptionChanged
        end
      end
      object gbInterfaceOptions: TGroupBox
        Left = 19
        Top = 9
        Width = 336
        Height = 142
        Caption = 'Interface Options'
        TabOrder = 0
        object cbNoBackgrounds: TCheckBox
          Left = 19
          Top = 66
          Width = 193
          Height = 17
          Caption = 'Disable Background Images'
          TabOrder = 2
          OnClick = OptionChanged
        end
        object cbEdgeScrolling: TCheckBox
          Left = 19
          Top = 20
          Width = 221
          Height = 17
          Caption = 'Enable Edge Scrolling and Trap Cursor'
          TabOrder = 0
          OnClick = OptionChanged
        end
        object cbSpawnInterval: TCheckBox
          Left = 19
          Top = 112
          Width = 153
          Height = 17
          Caption = 'Use Spawn Interval'
          TabOrder = 4
          OnClick = OptionChanged
        end
        object cbHideAdvanced: TCheckBox
          Left = 19
          Top = 89
          Width = 234
          Height = 17
          Caption = 'Hide Advanced Options in Level Select'
          TabOrder = 3
          OnClick = OptionChanged
        end
        object cbForceDefaultLemmings: TCheckBox
          Left = 19
          Top = 43
          Width = 173
          Height = 17
          Caption = 'Force Default Lemming Sprites'
          TabOrder = 1
          OnClick = OptionChanged
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Graphics'
      ImageIndex = 3
      object gbZoomOptions: TGroupBox
        Left = 19
        Top = 19
        Width = 336
        Height = 142
        Caption = 'Zoom Options'
        TabOrder = 0
        object Label1: TLabel
          Left = 27
          Top = 28
          Width = 32
          Height = 13
          Caption = 'Zoom:'
        end
        object Label2: TLabel
          Left = 27
          Top = 55
          Width = 31
          Height = 13
          Caption = 'Panel:'
        end
        object cbZoom: TComboBox
          Left = 72
          Top = 24
          Width = 177
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = '1x Zoom'
          OnChange = OptionChanged
          Items.Strings = (
            '1x Zoom')
        end
        object cbPanelZoom: TComboBox
          Left = 72
          Top = 51
          Width = 177
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = '1x Zoom'
          OnChange = OptionChanged
          Items.Strings = (
            '1x Zoom')
        end
        object cbIncreaseZoom: TCheckBox
          Left = 28
          Top = 86
          Width = 205
          Height = 17
          Caption = 'Increase Zoom On Small Levels'
          TabOrder = 2
          OnClick = OptionChanged
        end
        object cbCompactSkillPanel: TCheckBox
          Left = 28
          Top = 109
          Width = 153
          Height = 17
          Caption = 'Compact Skill Panel'
          TabOrder = 3
          OnClick = OptionChanged
        end
      end
      object gbResolutionOptions: TGroupBox
        Left = 19
        Top = 183
        Width = 336
        Height = 129
        Caption = 'Resolution Options'
        TabOrder = 1
        object cbLinearResampleMenu: TCheckBox
          Left = 28
          Top = 48
          Width = 205
          Height = 17
          Caption = 'Use Smooth Resampling In Menus'
          TabOrder = 1
          OnClick = OptionChanged
        end
        object cbLinearResampleGame: TCheckBox
          Left = 28
          Top = 71
          Width = 205
          Height = 17
          Caption = 'Use Smooth Resampling In Game'
          TabOrder = 2
          OnClick = OptionChanged
        end
        object cbMinimapHighQuality: TCheckBox
          Left = 28
          Top = 94
          Width = 153
          Height = 17
          Caption = 'High Quality Minimap'
          TabOrder = 3
          OnClick = OptionChanged
        end
        object cbHighResolution: TCheckBox
          Left = 28
          Top = 25
          Width = 205
          Height = 17
          Caption = 'High Resolution'
          TabOrder = 0
          OnClick = OptionChanged
        end
      end
      object gbWindowOptions: TGroupBox
        Left = 19
        Top = 332
        Width = 336
        Height = 120
        Caption = 'Window Options'
        TabOrder = 2
        object cbFullScreen: TCheckBox
          Left = 28
          Top = 26
          Width = 205
          Height = 17
          Caption = 'Full Screen'
          TabOrder = 0
          OnClick = cbFullScreenClick
        end
        object cbResetWindowSize: TCheckBox
          Left = 28
          Top = 89
          Width = 141
          Height = 17
          Caption = 'Reset Window Size'
          TabOrder = 2
          OnClick = OptionChanged
        end
        object cbResetWindowPosition: TCheckBox
          Left = 175
          Top = 90
          Width = 146
          Height = 17
          Caption = 'Reset Window Position'
          TabOrder = 3
          OnClick = OptionChanged
        end
        object btnResetWindow: TButton
          Left = 27
          Top = 54
          Width = 293
          Height = 27
          Caption = 'Reset Window'
          TabOrder = 1
          OnClick = btnResetWindowClick
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Audio'
      ImageIndex = 4
      object gbVolume: TGroupBox
        Left = 19
        Top = 33
        Width = 336
        Height = 105
        Caption = 'Volume'
        TabOrder = 0
        object Label3: TLabel
          Left = 16
          Top = 28
          Width = 34
          Height = 13
          Caption = 'Sound'
        end
        object Label5: TLabel
          Left = 20
          Top = 67
          Width = 30
          Height = 13
          Caption = 'Music'
        end
        object tbSoundVol: TTrackBar
          Left = 56
          Top = 24
          Width = 265
          Height = 33
          Max = 100
          Frequency = 10
          TabOrder = 0
          OnChange = SliderChange
        end
        object tbMusicVol: TTrackBar
          Left = 56
          Top = 63
          Width = 265
          Height = 33
          Max = 100
          Frequency = 10
          TabOrder = 1
          OnChange = SliderChange
        end
      end
      object gbSoundOptions: TGroupBox
        Left = 19
        Top = 160
        Width = 336
        Height = 137
        Caption = 'Sound Options'
        TabOrder = 1
        object rgSoundScheme: TRadioGroup
          Left = 20
          Top = 32
          Width = 293
          Height = 57
          Caption = 'Sound Scheme'
          Columns = 2
          Items.Strings = (
            'NeoLemmix'
            'NeoLemmix CE')
          TabOrder = 0
          OnClick = OptionChanged
        end
        object cbPostviewJingles: TCheckBox
          Left = 20
          Top = 103
          Width = 129
          Height = 17
          Caption = 'Postview Jingles'
          TabOrder = 1
          OnClick = OptionChanged
        end
      end
      object gbMusicOptions: TGroupBox
        Left = 19
        Top = 320
        Width = 336
        Height = 65
        Caption = 'Music Options'
        TabOrder = 2
        object cbDisableTestplayMusic: TCheckBox
          Left = 20
          Top = 26
          Width = 193
          Height = 17
          Caption = 'Disable Music When Testplaying'
          TabOrder = 0
          OnClick = OptionChanged
        end
      end
    end
  end
  object btnOK: TButton
    Left = 23
    Top = 505
    Width = 108
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 137
    Top = 505
    Width = 108
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 251
    Top = 505
    Width = 108
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Apply'
    TabOrder = 3
    OnClick = btnApplyClick
  end
end
