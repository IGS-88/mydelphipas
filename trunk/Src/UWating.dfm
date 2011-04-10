object Waiting: TWaiting
  Left = 567
  Top = 289
  BorderStyle = bsNone
  Caption = 'Waiting'
  ClientHeight = 81
  ClientWidth = 209
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 32
    Width = 36
    Height = 13
    Caption = 'Waiting'
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 24
  end
end
