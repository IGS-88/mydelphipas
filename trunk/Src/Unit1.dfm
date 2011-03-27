object Form1: TForm1
  Left = 298
  Top = 109
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
  end
  object btn5: TButton
    Left = 344
    Top = 8
    Width = 89
    Height = 33
    Caption = 'btn1'
    TabOrder = 5
  end
  object Edit1: TEdit
    Left = 352
    Top = 56
    Width = 377
    Height = 21
    TabOrder = 6
    Text = 
      'offset=0,0;framesize=500,500;framerate=15/1;showframe=1;cursor=1' +
      ';title='#24211
  end
  object FFLogger1: TFFLogger
    Left = 48
    Top = 40
  end
end
