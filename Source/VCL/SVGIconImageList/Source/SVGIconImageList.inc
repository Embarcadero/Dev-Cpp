// Delphi XE6
{$IFDEF VER270}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
{$ENDIF}

// Delphi XE7
{$IFDEF VER280}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
{$ENDIF}

// Delphi XE8
{$IFDEF VER290}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
{$ENDIF}

// Delphi 10 Seattle
{$IFDEF VER300}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
{$ENDIF}

// Delphi 10.1 Berlin
{$IFDEF VER310}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
{$ENDIF}

// Delphi 10.2 Tokyo
{$IFDEF VER320}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
{$ENDIF}

// Delphi 10.3 Rio
{$IFDEF VER330}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
{$ENDIF}

// Delphi 10.4 Sydney
{$IFDEF VER340}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
{$ENDIF}

// Delphi 11 Alexandria
{$IFDEF VER350}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
  {$DEFINE D11+}
{$ENDIF}

{$IFDEF D10_3+}
  {$Define HiDPISupport}
{$ENDIF}

//Prefer Engine Direct2D by Kiriakos Vlahos
//if supported by Windows Platform (from Windows Creators update)
{.$DEFINE PreferNativeSvgSupport}
{.$DEFINE GPUSupport}
{$IFDEF PreferNativeSvgSupport}
  // Throw an exception if the SVG contains text elements or class attributes
  // which are not supported by Windows SVG. Since it costs some performance,
  // you should only turn it on during debugging or if it's absolutely necessary.
  {.$DEFINE CheckForUnsupportedSvg}
{$ENDIF}

//if PreferNativeSvgSupport not active or not available:
//use Delphi Engine TSVG by Martin Walter (included into SVG folder)
{$DEFINE Delphi_SVGEngine}
//or use Engine Cairo wrapper (included by Lübbe Onken in Cairo folder)
{.$DEFINE Cairo_SVGEngine}

