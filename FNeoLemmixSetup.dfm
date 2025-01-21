object FNLSetup: TFNLSetup
  Left = 794
  Top = 419
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'NeoLemmix Setup'
  ClientHeight = 283
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    554
    283)
  PixelsPerInch = 96
  TextHeight = 13
  object SetupPages: TPageControl
    Left = 0
    Top = 0
    Width = 554
    Height = 238
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 473
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      TabVisible = False
      object lblWelcome: TLabel
        Left = 170
        Top = 11
        Width = 217
        Height = 21
        Caption = 'Welcome to NeoLemmix CE!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblOptionsText1: TLabel
        Left = 36
        Top = 48
        Width = 477
        Height = 13
        Caption = 
          'It appears that this is your first time using NeoLemmix, or that' +
          ' your settings.ini file is missing.'
      end
      object lblOptionsText2: TLabel
        Left = 88
        Top = 78
        Width = 366
        Height = 13
        Caption = 
          'Please select the desired options. You can always change them la' +
          'ter on.'
      end
      object lblHotkeys: TLabel
        Left = 112
        Top = 139
        Width = 83
        Height = 13
        Caption = 'Hotkey settings:'
      end
      object lblGraphics: TLabel
        Left = 112
        Top = 166
        Width = 87
        Height = 13
        Caption = 'Graphic settings:'
      end
      object lblUsername: TLabel
        Left = 112
        Top = 112
        Width = 56
        Height = 13
        Caption = 'Your name:'
      end
      object lblOnline: TLabel
        Left = 112
        Top = 193
        Width = 82
        Height = 13
        Caption = 'Online settings:'
      end
      object imgFloater: TImage
        Left = 19
        Top = 97
        Width = 71
        Height = 117
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000580000
          00920803000000F638FDD4000000017352474200AECE1CE90000000467414D41
          0000B18F0BFC610500000024504C5445222255A53333FF4F4FFFFFFFBCBCBCBB
          9999EECCBB11AA000044990066DD00355B000000C80F3C330000000C74524E53
          FFFFFFFFFFFFFFFFFFFFFF0012DFCECE000000097048597300000EC300000EC3
          01C76FA8640000001874455874536F667477617265005061696E742E4E455420
          352E312E3162B5520C000000B66558496649492A000800000005001A01050001
          0000004A0000001B010500010000005200000028010300010000000200000031
          010200100000005A00000069870400010000006A000000000000006000000001
          00000060000000010000005061696E742E4E455420352E312E31000300009007
          00040000003032333001A00300010000000100000005A0040001000000940000
          0000000000020001000200040000005239380002000700040000003031303000
          0000001B74D394D408860B0000030A4944415478DAEDDA8B72A3201400D0EBAA
          AD5DFFFF53D9C618D7E5F24610890AD259E9681C83279DCB3390AA87D844E811
          9FBBBA082681F7C24FE6873959D13FFE0AF0971EBFE831B33BB378ED8B8089A0
          F8B91277E7C5F5ACAEFA8B61627095BA3BCB071677F847F497C13C084BD40F9B
          B81B903CB03F0821783D203960A282B0CE86DE9B2D3A3D1C62C3B09F4E0D87D9
          2DD847A7854D762B105B39249D0FDE0A446C3052C2C4CA583B4C0C5CC164E5EF
          13C278D37EB0713A9CC9A266FAE1F60756F0B28416C6A4F0B28B2694E64967D7
          853B1BFF462306D817F8868B54B07FF86F18EA76A976D7C8FF85D73BF38A64B0
          3B092102EE37735E01FBD3CF98D1DFF00DDFF00DFF1F703C7D3D8C6C4B8F3192
          CE0F13E8ADF393922D9B8EC5D1F9605EA9087CC240AFF4F983D22D9B023C8B82
          1104CAB8679C9A626AF7852211CC59370D828C2FBA5C7098EEE8F1D8DBA4B3C3
          487FD1F37761F0921DD4D5679130618089B6E2EB016F1AA5C1761806816AF67C
          58BAC742218331B061887792B55832381BC6AFFAD32158876310C3268683C14C
          3E0BC6D8B60853B43E009B958D77EB185BFA5A4FF5D4BCDA91C1737437B40EB3
          D8CA7103F6C2E690A4E0E9F71F3922E1507A225C4F32D8BB61B7B28DD0B0951B
          5D9F4F834570C562D06E580744D072B4B7E0F80AB70EF38AAC56AF0EC06E018A
          2CC0FBB8F260BB00F53405D34807FFAA40580744C33C7500C5C23A20725AA5B2
          160B6BDE654B8691EE0C50A6D86E283F4C4420DCAD89B860E486892A38DF8655
          4C304A82E38291172646D3F809B06C18F32A1C537CB960D9F554ECE16E058E09
          467A9877EF2DDB04E42C140D136332558BC259EF86628A2F0FFCA4136CFC59D4
          432CE3950EE344EAC3584E3AD611A586393D5ACB75E1AEE85B5DF517C1EE52F9
          16AC9749FA8BE06522DE29164F58DDCC3598A8DFBB6586ED9FD03DD437564CCF
          4261894BB63CD84F8362616FE165841FECDC9A8FEF6920C960DEC43B83EC410F
          BA984675B70498580F2FB755B6370D73C364D10462B7AE6E3816268B460B6F6C
          5E5D0D4370C02C07D6DBDB98B089BCCBE687250EBBD0A4F03FB56F4FCB448821
          180000000049454E44AE426082}
        Proportional = True
      end
      object cbHotkey: TComboBox
        Left = 216
        Top = 136
        Width = 217
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Grouped by function'
        Items.Strings = (
          'Grouped by function'
          'Traditional layout'
          'Minimalist configuration')
      end
      object cbGraphics: TComboBox
        Left = 216
        Top = 163
        Width = 217
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 2
        Text = 'Low-resolution, enhancements'
        Items.Strings = (
          'Low-resolution, no enhancements'
          'Low-resolution, enhancements'
          'High-resolution, no enhancements'
          'High-resolution, enhancements')
      end
      object ebUserName: TEdit
        Left = 216
        Top = 109
        Width = 217
        Height = 21
        TabOrder = 0
        Text = 'Anonymous'
      end
      object cbOnline: TComboBox
        Left = 216
        Top = 190
        Width = 217
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 3
        Text = 'Online functions disabled'
        Items.Strings = (
          'Online functions disabled'
          'Online functions enabled'
          'Online + update check enabled')
      end
    end
  end
  object btnNext: TButton
    Left = 384
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnExit: TButton
    Left = 304
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Exit'
    TabOrder = 1
    OnClick = btnExitClick
  end
end
