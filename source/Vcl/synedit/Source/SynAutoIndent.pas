{-------------------------------------------------------------------------------

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: SynAutoIndent.pas, Peter Schraut, released 2003-12-14.
  The Original Code is based on SynEditPythonBehaviour.pas, part
  of the SynEdit component suite.
  
  Contributors to the SynEdit and mwEdit projects are listed in the
  Contributors.txt file.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  $Id:

  You may retrieve the latest version of this file at the SynEdit home page,
  located at http://SynEdit.SourceForge.net


  History:
      14th Dec 2003
        + Used and extended code from SynEditPythonBehaviour.pas to be able to
          specifiy a set of chars to indent and unindent automatically.

      22nd Jan 2004
        + Fixed a bug where the cursor went to the above row when x got s
          smaller than 0


  Known Issues:          
-------------------------------------------------------------------------------}

{$IFNDEF QSYNAUTOINDENT}
unit SynAutoIndent;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  {$IFDEF SYN_CLX}
  Qt, QGraphics, QControls, QForms, QDialogs,
  QSynEdit,
  QSynEditKeyCmds,
  {$ELSE}
  Messages,
  SynEdit,
  SynEditKeyCmds,
  {$ENDIF}
  SysUtils,
  Classes;

type
  TSynCustomAutoIndent = class(TComponent)
  private
    FEnabled: Boolean;
    FEditor: TSynEdit;
    FIndentChars: string;
    FUnIndentChars: string;
  protected
    procedure SetEditor(Value: TSynEdit); virtual;
    procedure doProcessUserCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var AChar: Char; Data: Pointer; HandlerData: pointer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Editor: TSynEdit read FEditor write SetEditor;
    property Enabled: Boolean read FEnabled write FEnabled;
    property IndentChars: string read FIndentChars write FIndentChars;
    property UnIndentChars: string read FUnIndentChars write FUnIndentChars;
  end;

  
  TSynAutoIndent = class(TSynCustomAutoIndent)
  published
    property Editor;
    property Enabled;
    property IndentChars;
    property UnIndentChars;
    property Name;
    property Tag;
  end;

  
  procedure Register;


implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

{$R SynAutoIndent.res}

procedure Register;
begin
  RegisterComponents('SynEdit', [TSynAutoIndent]);
end;


//--------------------------------------------------------------------------------------------------


constructor TSynCustomAutoIndent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEnabled := True;
  FEditor := nil;
  FIndentChars := ':{';
  FUnIndentChars := '}';
end;


//--------------------------------------------------------------------------------------------------


procedure TSynCustomAutoIndent.SetEditor(Value: TSynEdit);
begin
  if FEditor <> Value then
  begin
    if (Editor <> nil) and not (csDesigning in ComponentState) then
      Editor.UnregisterCommandHandler( doProcessUserCommand );
    // Set the new editor
    FEditor := Value;
    if (Editor <> nil) and not (csDesigning in ComponentState) then
      Editor.RegisterCommandHandler( doProcessUserCommand, nil );
  end;
end; 


//--------------------------------------------------------------------------------------------------


procedure TSynCustomAutoIndent.doProcessUserCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: Char; Data: Pointer; HandlerData: pointer);
var
  iEditor: TCustomSynEdit;
  StrPrevLine: string;
  StrCurLine: string;
  i: integer;
begin
  if (not FEnabled) or not (eoAutoIndent in (Sender as TCustomSynEdit).Options) then
    Exit;

  if AfterProcessing then
  begin
    case Command of
      ecLineBreak:
        begin
          iEditor := Sender as TCustomSynEdit;
          { CaretY should never be lesser than 2 right after ecLineBreak, so there's
          no need for a check }
          StrPrevLine := TrimRight(iEditor.Lines[iEditor.CaretY-2]);
          if (StrPrevLine <> '') and (AnsiPos(StrPrevLine[Length(StrPrevLine)], FIndentChars) > 0) then
          begin
            iEditor.UndoList.BeginBlock;
            try
              i := iEditor.DisplayX + iEditor.TabWidth -1;
              iEditor.ExecuteCommand(ecSelLineStart, #0, nil);
              while iEditor.DisplayX <= i do
                iEditor.ExecuteCommand(ecTab, #0, nil);
            finally
              iEditor.UndoList.EndBlock;
            end;
          end;
        end;
    end;
  end
  else
  begin
    case Command of
      ecChar:
        begin
          iEditor := Sender as TCustomSynEdit;
          StrCurLine := Trim(iEditor.Lines[iEditor.CaretY-1]);
          if (StrCurLine = '') and (AnsiPos(AChar, FUnIndentChars) > 0) then
          begin
            iEditor.UndoList.BeginBlock;
            try
              i := iEditor.DisplayX-1 - FEditor.TabWidth;
              iEditor.ExecuteCommand(ecSelLineStart, #0, nil);
              iEditor.ExecuteCommand(ecChar, AChar, nil);
              AChar := #0;
              iEditor.ExecuteCommand(ecLeft, #0, nil);
              while iEditor.DisplayX <= i do
                iEditor.ExecuteCommand(ecTab, #0, nil);
              iEditor.ExecuteCommand(ecRight, #0, nil);
            finally
              iEditor.UndoList.EndBlock;
            end;
          end;
        end;
    end;
  end;
end;



end.


