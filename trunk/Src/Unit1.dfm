object Form1: TForm1
  Left = 388
  Top = 226
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 504
    Top = 104
    Width = 345
    Height = 329
  end
  object lbl1: TLabel
    Left = 352
    Top = 80
    Width = 3
    Height = 13
  end
  object mmo1: TMemo
    Left = 16
    Top = 112
    Width = 473
    Height = 313
    Lines.Strings = (
      'mmo1')
    TabOrder = 0
  end
  object btn1: TButton
    Left = 128
    Top = 8
    Width = 89
    Height = 33
    Caption = 'Start'
    TabOrder = 1
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 128
    Top = 56
    Width = 89
    Height = 33
    Caption = 'Pause'
    TabOrder = 2
    OnClick = btn2Click
  end
  object btn3: TButton
    Left = 240
    Top = 8
    Width = 89
    Height = 33
    Caption = 'Stop'
    TabOrder = 3
    OnClick = btn3Click
  end
  object btn4: TButton
    Left = 240
    Top = 56
    Width = 89
    Height = 33
    Caption = 'Resume'
    TabOrder = 4
    OnClick = btn4Click
  end
  object btn5: TButton
    Left = 344
    Top = 8
    Width = 89
    Height = 33
    Caption = 'Add Handle'
    TabOrder = 5
    OnClick = btn5Click
  end
  object Edit1: TEdit
    Left = 352
    Top = 56
    Width = 377
    Height = 21
    TabOrder = 6
    Text = 
      'offset=0,0;framesize=500,500;framerate=15/1;showframe=1;cursor=1' +
      ';'
  end
  object edt1: TEdit
    Left = 448
    Top = 16
    Width = 105
    Height = 21
    TabOrder = 7
    Text = 'CaptureScreen.mp4'
  end
  object edt2: TEdit
    Left = 560
    Top = 16
    Width = 249
    Height = 21
    TabOrder = 8
  end
  object btn6: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btn6'
    TabOrder = 9
    OnClick = btn6Click
  end
  object Listbox: TListBox
    Left = 16
    Top = 432
    Width = 313
    Height = 97
    ItemHeight = 13
    TabOrder = 10
  end
  object edtPid: TEdit
    Left = 360
    Top = 440
    Width = 121
    Height = 21
    TabOrder = 11
    Text = '0'
  end
  object btn7: TButton
    Left = 8
    Top = 56
    Width = 113
    Height = 33
    Caption = 'ScreenCapture2 start'
    TabOrder = 12
    OnClick = btn7Click
  end
  object btn8: TButton
    Left = 360
    Top = 496
    Width = 75
    Height = 25
    Caption = 'Merger'
    TabOrder = 13
    OnClick = btn8Click
  end
  object edt3: TEdit
    Left = 464
    Top = 496
    Width = 105
    Height = 21
    TabOrder = 14
    Text = 'E:\'#26032#24314#25991#20214#22841'\ScreenCapture1.mp4'
  end
  object edt4: TEdit
    Left = 600
    Top = 496
    Width = 97
    Height = 21
    TabOrder = 15
    Text = 'E:\'#26032#24314#25991#20214#22841'\372_70568024.wav'
  end
  object edt5: TEdit
    Left = 712
    Top = 496
    Width = 121
    Height = 21
    TabOrder = 16
    Text = 'E:\'#26032#24314#25991#20214#22841'\Merger.mp4'
  end
  object btn9: TButton
    Left = 360
    Top = 536
    Width = 75
    Height = 25
    Caption = 'GetStreamIndex'
    TabOrder = 17
    OnClick = btn9Click
  end
  object actlst1: TActionList
    Left = 808
    Top = 64
    object act_point: TAction
      Caption = 'act_point'
      ShortCut = 16496
      OnExecute = act_pointExecute
    end
  end
end
