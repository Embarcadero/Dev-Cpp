object Form1: TForm1
  Left = 206
  Top = 196
  Width = 696
  Height = 491
  ActiveControl = SynEdit1
  Caption = 'URL Demo'
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 688
    Height = 457
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clBlack
    Gutter.Font.Height = 8
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Pitch = fpVariable
    Gutter.Font.Style = []
    Highlighter = SynURISyn1
    Lines.Strings = (
      
        'This project demonstrates how to use SynURIOpener to make links ' +
        'clickable.'
      ''
      'Under Windows (also CLX when running Windows) this is very easy.'
      'For a Linux demo open URLDemoLinux.dpr.'
      ''
      'Simply assign two properties of SynURIOpener:'
      '- Editor-property must be set to your SynEdit/SynMemo.'
      
        '- URIHighlighter-property should be assigned an instance of a Sy' +
        'nURISyn.'
      ''
      
        'Don'#39't forget to set also SynEdit'#39's Highlighter-property to a Syn' +
        'URISyn.'
      'That'#39's all!'
      ''
      'NOTE:'
      'By default you have to press CTRL to make the links clickable.'
      
        'If you don'#39't like that, set CtrlActivatesLinks-property to false' +
        '.'
      ''
      ''
      'Some test links:'
      '----------------'
      '(not all real ones, just to check if correct app starts)'
      ''
      'http://www.somewhere.org'
      'ftp://superhost.org/downloads/gems.zip'
      'www.w3c.org'
      'mailto:big@lebowski.edu'
      'douglas@adams.lod'
      'news:comp.lang.pascal.borland')
  end
  object SynURIOpener1: TSynURIOpener
    Editor = SynEdit1
    URIHighlighter = SynURISyn1
    Left = 632
    Top = 40
  end
  object SynURISyn1: TSynURISyn
    Left = 632
    Top = 8
  end
end
