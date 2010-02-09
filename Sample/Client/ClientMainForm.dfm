object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'DSFC Client Sample'
  ClientHeight = 311
  ClientWidth = 327
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    327
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 263
    Width = 127
    Height = 13
    Caption = 'Password for Cipher filters'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Click Me'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 311
    Height = 202
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 282
    Width = 193
    Height = 21
    TabOrder = 2
    Text = 'mypassword'
  end
  object SQLConnection1: TSQLConnection
    ConnectionName = 'DATASNAPCONNECTION'
    DriverName = 'DataSnap'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=DataSnap'
      'HostName=127.0.0.1')
    Left = 56
    Top = 72
  end
end
