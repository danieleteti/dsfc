object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Server Form'
  ClientHeight = 286
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 237
    Width = 127
    Height = 13
    Caption = 'Password for Cipher filters'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 279
    Height = 25
    Caption = 'Restart with selected filter'
    TabOrder = 0
    OnClick = Action1Execute
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 39
    Width = 279
    Height = 186
    Caption = 'Available Filters'
    Columns = 2
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 256
    Width = 193
    Height = 21
    TabOrder = 2
    Text = 'mypassword'
  end
  object DSServer1: TDSServer
    Left = 168
    Top = 144
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    LifeCycle = 'Invocation'
    Left = 40
    Top = 144
  end
  object DSTCP: TDSTCPServerTransport
    PoolSize = 0
    Server = DSServer1
    Filters = <>
    Left = 168
    Top = 72
  end
end
