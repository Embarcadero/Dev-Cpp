object VerifyForm: TVerifyForm
  Left = 192
  Top = 107
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Verifying package...'
  ClientHeight = 154
  ClientWidth = 298
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 8
    Width = 209
    Height = 49
    AutoSize = False
    Caption = 'The package'#39's contents are now being verified. Please wait...'
    WordWrap = True
  end
  object Panel2: TPanel
    Left = 8
    Top = 8
    Width = 52
    Height = 51
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Image1: TImage
      Left = 2
      Top = 2
      Width = 48
      Height = 47
      AutoSize = True
      Picture.Data = {
        07544269746D6170A61A0000424DA61A00000000000036000000280000003000
        00002F0000000100180000000000701A0000C40E0000C40E0000000000000000
        0000D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D8D8D7D7D7D4D4D4D1D1D1CECECE
        CDCDCDCECECED0D0D0D3D3D3D6D6D6D8D8D8D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D8
        D8D6D6D6D2D2D2CBCBCBC4C4C4BFBFBFBCBCBCBEBEBEC3C3C3CBCBCBD1D1D1D6
        D6D6D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D8D8D8D6D6D6D1D1D1C9C9C9BDBDBDB1B1B1A8A8A8
        A6A6A6A8A8A8B1B1B1BEBEBECBCBCBD3D3D3D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D8D8D6D6D6D1D1
        D1C8C8C8BBBBBBACACAC9E9E9E959595929292959595A0A0A0B1B1B1C3C3C3D0
        D0D0D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D8D8D8D6D6D6D1D1D1C8C8C8BABABA7C7C7C5C5C5C555555515151
        5C5C5C858585969696A8A8A8BEBEBECECECED9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D8D8D6D6D6D1D1D1C8C8C8BABA
        BA7C7C7C5C5C5C323639374046505E644B4D4D5D5D5D939393A7A7A7BDBDBDCE
        CECED9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8
        D8D8D6D6D6D1D1D1C9C9C9BBBBBB7C7C7C5C5C5C323639374046DDE1E2BCC3C7
        2B3034535353979797ABABABC0C0C0CFCFCFD9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D8D8D7D7D7D7D7D7D6D6D6D6D6D6
        D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D3D3D3CBCBCBBDBDBD7D7D7D5C5C
        5C323639374046DDE1E2BCC3C721282C131718575757A3A3A3B5B5B5C6C6C6D2
        D2D2D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D8D8D7D7
        D7D6D6D6D3D3D3D0D0D0CECECECDCDCDCCCCCCCCCCCCCDCDCDCECECED0D0D0D0
        D0D0CBCBCBC0C0C08080805D5D5D323639374046DDE1E2D2D7D921282C131718
        3D3E3E5E5E5EB1B1B1C1C1C1CECECED6D6D6D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D7D7D7D6D6D6D1D1D1CDCDCDC7C7C7C2C2C2BEBEBEBBBBBB
        BABABABABABABBBBBBBEBEBEC0C0C0C2C2C2BFBFBF82828260606033373A3740
        46DDE1E2BCC3C721282C1317183D3E3E5E5E5E979797BFBFBFCCCCCCD3D3D3D7
        D7D7D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D7D7D7D5D5D5CFCFCFC6C6
        C6BEBEBEB4B4B4ADADADA8A8A8A4A4A4A3A3A3A3A3A3A4A4A4A7A7A7ABABABAE
        AEAE7E7E7E61616133383B374046E4E7E9BCC3C721282C1317183D3E3E5E5E5E
        979797BFBFBFCBCBCBD3D3D3D7D7D7D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D7D7D7D5D5D5CECECEC3C3C3B6B6B6AAAAAAA0A0A0989898949494929292
        9292929292929292929494949797979A9A9A5B5B5B33373A374046E4E7E9BCC3
        C721282C1317183D3E3E5E5E5E979797BFBFBFCBCBCBD3D3D3D7D7D7D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D8D8D6D6D6CFCFCFC3C3C3B4B4B49090
        907171715E5E5E5454545353535353535454545B5B5B6868687A7A7A8C8C8C6F
        6F6F535353374046E4E7E988939921282C1317183E3E3E5F5F5F989898BFBFBF
        CBCBCBD3D3D3D7D7D7D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D7
        D7D7D1D1D1C6C6C6A8A8A872727259595953585A525D6152606653636A53636A
        3B4348363A3C41424354545460606059595951595CE6EAEB88939921282C1317
        183F40416161619A9A9AC0C0C0CCCCCCD3D3D3D7D7D7D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D8D8D8D6D6D6CCCCCCA6A6A66666664A4A4A3C45
        4A53636A53636AACB9BDBBC5C8BAC3C797A6AA65777E3C484D353B3D47474853
        595C516065E1E5E6CFD5D6B0B6B98082836464649D9D9DC3C3C3CDCDCDD3D3D3
        D7D7D7D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D7D7D7D2
        D2D2B8B8B86C6C6C5B60622E373B3B464C53636A515C61888989666666666666
        8A8B8BAEB1B2BBC5C85767702F373C4D575D3E484F99A1A4515A5D5656566060
        60979797C5C5C5CFCFCFD5D5D5D7D7D7D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D7D7D7D0D0D086868662656653636A2E373B3C44
        4854585A5A5A5A7474748686868080806868685454547F7F7FB6BCBE53636A2A
        31373B404351575A5E5E5E757575A6A6A6BBBBBBCBCBCBD4D4D4D7D7D7D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D6D6D6B4
        B4B470707057636853636A31363A5353536969699D9D9DBEBEBEC2C2C2BEBEBE
        B8B8B89A9A9A6A6A6A5F5F5FB7BDBF5F6F76373D41545454787878929292A6A6
        A6BCBCBCCDCDCDD6D6D6D8D8D8D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D6D6D6989898666A6D53636A3B464C4242426767
        67A3A3A3B9B9B9CACACAD3D3D3D5D5D5D6D6D6C9C9C9B2B2B2686868828282BB
        C5C853636A414243686868929292A4A4A4BBBBBBCCCCCCD6D6D6D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D6D6D684
        8484959DA143515635393C636363919191AEAEAEBFBFBFCECECED6D6D6D8D8D8
        D9D9D9D7D7D7C5C5C5909090545454AFB3B353636A363A3C5B5B5B929292A3A3
        A3BABABACCCCCCD6D6D6D8D8D8D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D6D6D67B7B7BB8C0C33F4C524141417D7D7DACAC
        ACB7B7B7C2C2C2CECECED6D6D6D8D8D8D9D9D9D9D9D9D0D0D0A9A9A96565658B
        8C8C5F6F7631373C545454929292A3A3A3BABABACCCCCCD6D6D6D8D8D8D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D6D6D67B
        7B7BBAC4C73B464C555555929292B6B6B6C4C4C4C9C9C9CDCDCDD3D3D3D7D7D7
        D9D9D9D9D9D9D3D3D3B4B4B48080806969699FADB22E373B535353929292A4A4
        A4BBBBBBCDCDCDD6D6D6D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D6D6D67B7B7BBAC4C72F393C515152919191B9B9
        B9CCCCCCCECECECCCCCCCECECED3D3D3D6D6D6D6D6D6D2D2D2BDBDBD8686866A
        6A6AACB9BD2E373B535353949494A8A8A8BEBEBECECECED6D6D6D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D0D0D0C5C5C5BBBB
        BBB2B2B2A8A8A89E9E9E9494948B8B8B808080D9D9D9D9D9D9D9D9D9D7D7D77C
        7C7CAEB5B82F393C16181A3030306969698F8F8F858585505050202020151515
        1C1C1C2626262E2E2EBEBEBE7979798E8F8FA3B0B630373B2B2B2B5353536666
        667B7B7B8D8D8D9D9D9DA8A8A8B2B2B2BBBBBBC5C5C5D0D0D0D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D5D6D6D0D3D2CBCFCFC6CCCBC1C9C8BDC6C4B8C2C1B3BFBE
        A2ABAA767676D9D9D9D9D9D9D7D7D7313131AFB5B63741462C33353F49477581
        80A4AEADB7C0BFAAB6B48D9F9D768D8A708A86738B8860706D2B2B2B5F5F5F95
        9E9F5F707623282A565B5A7F8988939D9CA6B0AFB3BDBBBBC4C3C1C9C8C6CCCB
        CBCFCFD0D3D2D4D6D6D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D7D8D8D4D7D7D0D7
        D6CDD5D4CAD4D3C7D4D2C4D3D1C2D2D0AEBCBA767676D9D9D9D9D9D9D8D8D83A
        3A3A9FA3A36C7E852E373B3641415A696797A7A5B7C3C1BDCAC8AEBFBD91AAA7
        829F9B7F9C986579761D1D1D565A5B53636A53636A21232372757598A7A4AAB8
        B6BAC8C5C3D0CEC8D4D2CBD5D4CDD5D4D0D6D5D3D7D7D6D8D8D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D7D8D8D4D8D7D1D7D6CED6D5CBD5D4C8D4D3C4D3D1C2D2D0
        AEBCBA767676D9D9D9D9D9D9D9D9D9454545808585A1ACAE46545B30383B4553
        515C6A688D9C9AA7B6B49FB1AE859C99738D89657C793E4948181818535F6553
        636A525F64282828949897A5B3B1B6C4C2C0CDCCC7D4D2CAD5D4CCD6D4CED6D5
        D1D7D6D3D7D7D6D8D8D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D7D9D8D5D8D8D2D7
        D7CFD7D6CDD6D5CAD5D3C6D4D2C3D3D0AEBCBA767676D9D9D9D9D9D9D9D9D94F
        4F4F9A9F9E86929096A5AB4D5C6430383A3743414F5D5B677674758785677A77
        4E5F5C404E4C37424246535953636A53636A585C5D343434B9BDBDB3C2C0BECD
        CBC6D3D1C9D5D4CCD6D5CDD6D5CFD7D6D2D7D6D4D8D7D6D8D8D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D7D9D9D5D8D8D3D8D7D0D7D6CED7D6CBD6D4C8D5D3C4D4D1
        B0BDBB767676D9D9D9D9D9D9D9D9D94F4F4FD0D6D5717F7E939E9DAEB8BD5161
        682E363B2E393A354140404D4C3D4A48323C3C2D35383D484E53636A7889905B
        60626666664D4D4DCACECDBFCECCC5D3D1C9D6D4CCD7D6CDD7D6CED7D6D0D7D6
        D2D8D7D4D8D7D7D8D8D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D7D9D9D5D9D8D3D8
        D8D1D8D7CFD7D6CCD7D5C9D6D4C6D5D3B1BEBC767676D9D9D9D9D9D9D9D9D94F
        4F4FE5EBEBAABEBB71807E869391B3BBBD86969D3F4C523B464C3B464C3B464C
        4D5C645F6F76B3BEC2A2A7A98282826C6C6CA6A6A65A5A5AD6DBDBC7D5D3CAD8
        D6CDD9D8CDD8D7CED8D7CFD8D7D1D8D7D3D8D7D5D8D8D7D9D8D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D8D9D9D6D9D9D4D9D8D2D8D8D0D8D7CED8D6CBD7D5C8D6D4
        B3BFBD767676D9D9D9D9D9D9D9D9D94F4F4FE5EBEBC6DDDAB3C9C78396936C7D
        7A929E9DACB6B7B6C0C2B4BFC2B4BFC2B5BEC0ABB3B37C85841F1F1F868686B8
        B8B8CDCDCD5F5F5FDEE2E2CBD9D8CEDBD9CFDAD9CFDAD8D0D9D8D1D9D8D2D8D8
        D4D9D8D5D9D8D7D9D8D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D8D9D9D6D9D9D4D9
        D8D2D8D8D0D8D7CED8D6CBD7D5C8D6D4B3BFBD767676D9D9D9D9D9D9D9D9D94F
        4F4FEDEDEDC6DDDAC4DDDAC1DAD7A4BDB9879E9B718784667B78667A77657875
        6576756C7F7D657573313131D0D0D0D3D3D3D6D6D6626262E7E7E7CDDBD9CEDB
        D9CFDAD9CFDAD8D0D9D8D1D9D8D2D8D8D4D9D8D5D9D8D7D9D8D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9DADAD9DBDAD9DBDBD8DCDCD8DDDCD8DDDDD7DEDDD6DEDD
        CBD2D2767676D9D9D9D9D9D9D9D9D94F4F4FDAE6E5EDEEEEDBE8E6DBE8E7DAE9
        E8D7E7E6D9E8E6D4E7E5D3E5E3D2E3E1D1E2DFCFE0DEBBC9C73B3B3BD7D7D7D7
        D7D7D8D8D8636363DAE2E1E5E6E6DAE1E0DAE0DFD9DFDED9DEDDD9DDDDD8DCDC
        D9DBDBD9DADAD9DADAD9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D6D8D7CED0CFBBBB
        BBB2B2B2A8A8A89E9E9E9494948B8B8B808080D9D9D9D9D9D9D9D9D9D9D9D94F
        4F4FE9EDECAABCB9B4CAC7B0C7C5A5BDB96A79770A0A0A0000000A0A0A141414
        1E1E1E282828313131D9D9D9D9D9D9D9D9D9D9D9D9636363E4E7E7BBC7C5C5D1
        D0C7D1D0C6CFCDB8BDBCA8A8A8B2B2B2BBBBBBC5C5C5D0D0D0D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D6D7D7C5C5C5D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9454545DDE2E1CED7D6CDD7D7A5AA
        AA141414D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D96D6D6DDCDEDED3D8D7D4D8D8C6C8C89E9E9ED9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D0D0D0D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D93C3C3C3131312828281E1E1ED9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D97676768080
        808B8B8B949494D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9
        D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9D9}
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 64
    Width = 281
    Height = 46
    Caption = 'Progress (0%)'
    TabOrder = 1
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 16
      Width = 265
      Height = 20
      Smooth = True
      TabOrder = 0
    end
  end
  object BitBtn1: TBitBtn
    Left = 56
    Top = 120
    Width = 91
    Height = 25
    Caption = 'Abort'
    Default = True
    ModalResult = 3
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 153
    Top = 120
    Width = 89
    Height = 25
    Caption = '&Details...'
    Enabled = False
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 256
    Top = 120
  end
end