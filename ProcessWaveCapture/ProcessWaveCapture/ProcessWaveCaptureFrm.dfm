object frmWaveCapture: TfrmWaveCapture
  Left = 432
  Top = 265
  Width = 660
  Height = 465
  Caption = 'frmProcessWaveCapture'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    644
    427)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Output'
  end
  object lblStatus: TLabel
    Left = 8
    Top = 405
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Status'
  end
  object lblFmt: TLabel
    Left = 528
    Top = 48
    Width = 28
    Height = 13
    Caption = 'lblFmt'
  end
  object btnOpen: TButton
    Left = 8
    Top = 8
    Width = 65
    Height = 25
    Caption = 'Output'
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object btnStart: TButton
    Left = 72
    Top = 8
    Width = 65
    Height = 25
    Caption = 'Start'
    Enabled = False
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 136
    Top = 8
    Width = 65
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object btnPause: TButton
    Left = 200
    Top = 8
    Width = 65
    Height = 25
    Caption = 'Pause'
    Enabled = False
    TabOrder = 3
    OnClick = btnPauseClick
  end
  object btnResume: TButton
    Left = 264
    Top = 8
    Width = 65
    Height = 25
    Caption = 'Resume'
    Enabled = False
    TabOrder = 4
    OnClick = btnResumeClick
  end
  object btnWebSite: TButton
    Left = 506
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Web Site'
    TabOrder = 5
    OnClick = btnWebSiteClick
  end
  object cboLogLevel: TComboBox
    Left = 587
    Top = 402
    Width = 57
    Height = 21
    Hint = 'LogLevel'
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 5
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    Text = 'Info'
    OnChange = cboLogLevelChange
    Items.Strings = (
      'Quiet'
      'Panic'
      'Fatal'
      'Error'
      'Warning'
      'Info'
      'Verbose'
      'Debug')
  end
  object txtOutput: TStaticText
    Left = 47
    Top = 62
    Width = 8
    Height = 17
    BevelEdges = [beBottom]
    BevelKind = bkSoft
    Caption = '-'
    TabOrder = 7
  end
  object mmoLog: TMemo
    Left = 8
    Top = 85
    Width = 626
    Height = 311
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 8
  end
  object Edit1: TEdit
    Left = 8
    Top = 40
    Width = 449
    Height = 21
    TabOrder = 9
    Text = 'Edit1'
  end
  object btnInject: TButton
    Left = 456
    Top = 8
    Width = 65
    Height = 25
    Caption = 'btnInject'
    TabOrder = 10
    OnClick = btnInjectClick
  end
  object Edit2: TEdit
    Left = 456
    Top = 40
    Width = 65
    Height = 21
    TabOrder = 11
    Text = 'Edit2'
  end
  object btnAskFmt: TButton
    Left = 520
    Top = 8
    Width = 65
    Height = 25
    Caption = 'btnAskFmt'
    TabOrder = 12
    OnClick = btnAskFmtClick
  end
  object btnVoice: TButton
    Left = 328
    Top = 8
    Width = 65
    Height = 25
    Caption = 'btnVoice'
    TabOrder = 13
    OnClick = btnVoiceClick
  end
  object btnNoVoice: TButton
    Left = 392
    Top = 8
    Width = 65
    Height = 25
    Caption = 'btnNoVoice'
    TabOrder = 14
    OnClick = btnNoVoiceClick
  end
  object FFEncoder: TFFEncoder
    OnProgress = FFEncoderProgress
    OnTerminate = FFEncoderTerminate
    Left = 64
    Top = 160
  end
  object FFLogger: TFFLogger
    OnLog = FFLoggerLog
    Left = 96
    Top = 160
  end
  object SaveDialog1: TSaveDialog
    Left = 128
    Top = 160
  end
end
