{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPlugins.pas, released 2001-10-17.

Author of this file is Fl�vio Etrusco.
Portions created by Fl�vio Etrusco are Copyright 2001 Fl�vio Etrusco.
Unicode translation by Ma�l H�rz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditPlugins.pas,v 1.8.2.2 2008/09/14 16:24:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITPLUGINS}
unit SynEditPlugins;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  {$IFDEF SYN_COMPILER_17_UP}
  Types,
  {$ENDIF}
  Windows,
  Menus,
  SynEdit,
  SynEditKeyCmds,
  SynUnicode,
  Classes;

type
  TAbstractSynPlugin = class(TComponent)
  private
    procedure SetEditor(const Value: TCustomSynEdit);
    function GetEditors(aIndex: Integer): TCustomSynEdit;
    function GetEditor: TCustomSynEdit;
    function GetEditorCount: Integer;
  protected
    FEditors: TList;
    procedure Notification(aComponent: TComponent;
      aOperation: TOperation); override;
    procedure DoAddEditor(aEditor: TCustomSynEdit); virtual;
    procedure DoRemoveEditor(aEditor: TCustomSynEdit); virtual;
    function AddEditor(aEditor: TCustomSynEdit): Integer;
    function RemoveEditor(aEditor: TCustomSynEdit): Integer;
  public
    destructor Destroy; override;
    property Editors[aIndex: Integer]: TCustomSynEdit read GetEditors;
    property EditorCount: Integer read GetEditorCount;
  published
    property Editor: TCustomSynEdit read GetEditor write SetEditor;
  end;

  TAbstractSynHookerPlugin = class(TAbstractSynPlugin)
  protected
    procedure HookEditor(aEditor: TCustomSynEdit; aCommandID: TSynEditorCommand;
      aOldShortCut, aNewShortCut: TShortCut);
    procedure UnHookEditor(aEditor: TCustomSynEdit;
      aCommandID: TSynEditorCommand; aShortCut: TShortCut);
    procedure OnCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: Pointer; HandlerData: Pointer); virtual; abstract;
  end;

  TPluginState = (psNone, psExecuting, psAccepting, psCancelling);

  TAbstractSynSingleHookPlugin = class(TAbstractSynHookerPlugin)
  private
    fCommandID: TSynEditorCommand;
    function IsShortCutStored: Boolean;
    procedure SetShortCut(const Value: TShortCut);
  protected
    FState: TPluginState;
    fCurrentEditor: TCustomSynEdit;
    FShortCut: TShortCut;
    class function DefaultShortCut: TShortCut; virtual;
    procedure DoAddEditor(aEditor: TCustomSynEdit); override;
    procedure DoRemoveEditor(aEditor: TCustomSynEdit); override;
    {}
    procedure DoExecute; virtual; abstract;
    procedure DoAccept; virtual; abstract;
    procedure DoCancel; virtual; abstract;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property CommandID: TSynEditorCommand read fCommandID;
    property CurrentEditor: TCustomSynEdit read fCurrentEditor;
    function Executing: Boolean;
    procedure Execute(aEditor: TCustomSynEdit);
    procedure Accept;
    procedure Cancel;
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut
      stored IsShortCutStored;
  end;

  { use TAbstractSynCompletion for non-visual completion }

  TAbstractSynCompletion = class(TAbstractSynSingleHookPlugin)
  protected
    fCurrentString: UnicodeString;
  protected
    procedure SetCurrentString(const Value: UnicodeString); virtual;
    procedure OnCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data, HandlerData: Pointer); override;
    procedure DoExecute; override;
    procedure DoAccept; override;
    procedure DoCancel; override;
    function GetCurrentEditorString: UnicodeString; virtual;
  public
    procedure AddEditor(aEditor: TCustomSynEdit);
    property CurrentString: UnicodeString read fCurrentString write SetCurrentString;
  end;

function NewPluginCommand: TSynEditorCommand;
procedure ReleasePluginCommand(aCmd: TSynEditorCommand);

implementation

uses
  Forms,
  SynEditTypes,
  SynEditMiscProcs,
  SynEditStrConst,
  SysUtils;

const
  ecPluginBase = 64000;

var
  gCurrentCommand: Integer = ecPluginBase;

function NewPluginCommand: TSynEditorCommand;
begin
  Result := gCurrentCommand;
  Inc(gCurrentCommand);
end;

procedure ReleasePluginCommand(aCmd: TSynEditorCommand);
begin
  if aCmd = Pred(gCurrentCommand) then
    gCurrentCommand := aCmd;
end;

{ TAbstractSynPlugin }

function TAbstractSynPlugin.AddEditor(aEditor: TCustomSynEdit): Integer;
begin
  if FEditors = nil then
  begin
    FEditors := TList.Create;
  end
  else
    if FEditors.IndexOf(aEditor) >= 0 then
    begin
      Result := -1;
      Exit;
    end;
  aEditor.FreeNotification(Self);
  Result := FEditors.Add(aEditor);
  DoAddEditor(aEditor);
end;

destructor TAbstractSynPlugin.Destroy;
begin
  { RemoveEditor will free FEditors when it reaches count = 0}
  while Assigned(FEditors) do
    RemoveEditor(Editors[0]);
  inherited;
end;

procedure TAbstractSynPlugin.Notification(aComponent: TComponent;
  aOperation: TOperation);
begin
  inherited;
  if aOperation = opRemove then
  begin
    if (aComponent = Editor) or (aComponent is TCustomSynEdit) then
      RemoveEditor(TCustomSynEdit(aComponent));
  end;
end;

procedure TAbstractSynPlugin.DoAddEditor(aEditor: TCustomSynEdit);
begin

end;

procedure TAbstractSynPlugin.DoRemoveEditor(aEditor: TCustomSynEdit);
begin

end;

function TAbstractSynPlugin.RemoveEditor(aEditor: TCustomSynEdit): Integer;
begin
  if FEditors = nil then
  begin
    Result := -1;
    Exit;
  end;
  Result := FEditors.Remove(aEditor);
  //aEditor.RemoveFreeNotification(Self);
  if FEditors.Count = 0 then
  begin
    FEditors.Free;
    FEditors := nil;
  end;
  if Result >= 0 then
    DoRemoveEditor(aEditor);
end;

procedure TAbstractSynPlugin.SetEditor(const Value: TCustomSynEdit);
var
  iEditor: TCustomSynEdit;
begin
  iEditor := Editor;
  if iEditor <> Value then
  try
    if (iEditor <> nil) and (FEditors.Count = 1) then
      RemoveEditor(iEditor);
    if Value <> nil then
      AddEditor(Value);
  except
    if [csDesigning] * ComponentState = [csDesigning] then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

function TAbstractSynPlugin.GetEditors(aIndex: Integer): TCustomSynEdit;
begin
  Result := TCustomSynEdit(FEditors[aIndex]);
end;

function TAbstractSynPlugin.GetEditor: TCustomSynEdit;
begin
  if FEditors <> nil then
    Result := FEditors[0]
  else
    Result := nil;
end;

function TAbstractSynPlugin.GetEditorCount: Integer;
begin
  if FEditors <> nil then
    Result := FEditors.Count
  else
    Result := 0;
end;

{ TAbstractSynHookerPlugin }

procedure TAbstractSynHookerPlugin.HookEditor(aEditor: TCustomSynEdit;
  aCommandID: TSynEditorCommand; aOldShortCut, aNewShortCut: TShortCut);
var
  iIndex: Integer;
  iKeystroke: TSynEditKeyStroke;
begin
  Assert(aNewShortCut <> 0);
  { shortcurts aren't created while in design-time }
  if [csDesigning] * ComponentState = [csDesigning] then
  begin
    if TSynEdit(aEditor).Keystrokes.FindShortcut(aNewShortCut) >= 0 then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut)
    else
      Exit;
  end;
  { tries to update old Keystroke }
  if aOldShortCut <> 0 then
  begin
    iIndex := TSynEdit(aEditor).Keystrokes.FindShortcut(aOldShortCut);
    if (iIndex >= 0) then
    begin
      iKeystroke := TSynEdit(aEditor).Keystrokes[iIndex];
      if iKeystroke.Command = aCommandID then
      begin
        iKeystroke.ShortCut := aNewShortCut;
        Exit;
      end;
    end;
  end;
  { new Keystroke }
  iKeystroke := TSynEdit(aEditor).Keystrokes.Add;
  try
    iKeystroke.ShortCut := aNewShortCut;
  except
    iKeystroke.Free;
    raise;
  end;
  iKeystroke.Command := aCommandID;
  aEditor.RegisterCommandHandler(OnCommand, Self);
end;

procedure TAbstractSynHookerPlugin.UnHookEditor(aEditor: TCustomSynEdit;
  aCommandID: TSynEditorCommand; aShortCut: TShortCut);
var
  iIndex: Integer;
begin
  aEditor.UnregisterCommandHandler(OnCommand);
  iIndex := TSynEdit(aEditor).Keystrokes.FindShortcut(aShortCut);
  if (iIndex >= 0) and
    (TSynEdit(aEditor).Keystrokes[iIndex].Command = aCommandID) then
    TSynEdit(aEditor).Keystrokes[iIndex].Free;
end;

{ TAbstractSynHookerPlugin }

procedure TAbstractSynSingleHookPlugin.Accept;
begin
  FState := psAccepting;
  try
    DoAccept;
  finally
    fCurrentEditor := nil;
    FState := psNone;
  end;
end;

procedure TAbstractSynSingleHookPlugin.Cancel;
begin
  FState := psCancelling;
  try
    DoCancel;
  finally
    fCurrentEditor := nil;
    FState := psNone;
  end;
end;

constructor TAbstractSynSingleHookPlugin.Create(aOwner: TComponent);
begin
  inherited;
  fCommandID := NewPluginCommand;
  FShortCut := DefaultShortCut;
end;

class function TAbstractSynSingleHookPlugin.DefaultShortCut: TShortCut;
begin
  Result := 0;
end;

destructor TAbstractSynSingleHookPlugin.Destroy;
begin
  if Executing then
    Cancel;
  ReleasePluginCommand(CommandID);
  inherited;
end;

procedure TAbstractSynSingleHookPlugin.DoAddEditor(
  aEditor: TCustomSynEdit);
begin
  if ShortCut <> 0 then
    HookEditor(aEditor, CommandID, 0, ShortCut);
end;

procedure TAbstractSynSingleHookPlugin.Execute(aEditor: TCustomSynEdit);
begin
  if Executing then
    Cancel;
  Assert(fCurrentEditor = nil);
  fCurrentEditor := aEditor;
  Assert(FState = psNone);
  FState := psExecuting;
  try
    DoExecute;
  except
    Cancel;
    raise;
  end;
end;

function TAbstractSynSingleHookPlugin.Executing: Boolean;
begin
  Result := FState = psExecuting;
end;

function TAbstractSynSingleHookPlugin.IsShortCutStored: Boolean;
begin
  Result := FShortCut <> DefaultShortCut;
end;

procedure TAbstractSynSingleHookPlugin.DoRemoveEditor(aEditor: TCustomSynEdit);
begin
  if ShortCut <> 0 then
    UnHookEditor(aEditor, CommandID, ShortCut);
  if Executing and (CurrentEditor = aEditor) then
    Cancel;
end;

procedure TAbstractSynSingleHookPlugin.SetShortCut(const Value: TShortCut);
var
  cEditor: Integer;
begin
  if FShortCut <> Value then
  begin
    if Assigned(FEditors) then
      if Value <> 0 then
      begin
        for cEditor := 0 to FEditors.Count -1 do
          HookEditor(Editors[cEditor], CommandID, FShortCut, Value);
      end
      else
      begin
        for cEditor := 0 to FEditors.Count -1 do
          UnHookEditor(Editors[cEditor], CommandID, FShortCut);
      end;
    FShortCut := Value;
  end;
end;

{ TAbstractSynCompletion }

function TAbstractSynCompletion.GetCurrentEditorString: UnicodeString;
var
  S: UnicodeString;
  Col: Integer;
begin
  S := CurrentEditor.LineText;
  if (CurrentEditor.CaretX > 1) and
    (CurrentEditor.CaretX - 1 <= Length(S)) then
  begin
    for Col := CurrentEditor.CaretX - 1 downto 1 do
      if not CurrentEditor.IsIdentChar(S[Col])then
        Break;
    Result := Copy(S, Col + 1, CurrentEditor.CaretX - Col - 1);
  end;
end;

procedure TAbstractSynCompletion.DoAccept;
begin
  fCurrentString := '';
end;

procedure TAbstractSynCompletion.DoCancel;
begin
  fCurrentString := '';
end;

procedure TAbstractSynCompletion.DoExecute;
begin
  CurrentString := GetCurrentEditorString;
end;

procedure TAbstractSynCompletion.OnCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data,
  HandlerData: Pointer);
var
  S: UnicodeString;
begin  
  if not Executing then
  begin
    if (Command = CommandID) then
    begin
      Execute(Sender as TCustomSynEdit);
      Handled := True;
    end;
  end
  else { Executing }
    if Sender = CurrentEditor then
    begin
      if not AfterProcessing then
      begin
          case Command of
            ecChar:
              if aChar = #27 then
              begin
                Cancel;
                Handled := True;
              end
              else
              begin
                if not(CurrentEditor.IsIdentChar(aChar)) then 
                  Accept;
                {don't handle the char}
              end;
            ecLineBreak:
            begin
              Accept;
              Handled := True;
            end;
            ecLeft, ecSelLeft:
              if CurrentString = '' then
                Handled := True;
            ecDeleteLastChar:
              if CurrentString = '' then
                Handled := True;
            ecTab:
              Accept;
            ecDeleteChar,
            ecRight, ecSelRight,
            ecLostFocus, ecGotFocus:
              ; {processed on AfterProcessing}
            else
              Cancel;
          end;
      end
      else { AfterProcessing }
        case Command of
          ecLostFocus, ecGotFocus,
          ecDeleteChar:
            ;
          ecDeleteLastChar,
          ecLeft, ecSelLeft,
          ecChar:
            CurrentString := GetCurrentEditorString;
          ecRight, ecSelRight: begin
            S := GetCurrentEditorString;
            if S = '' then
              Cancel
            else
              CurrentString := S;
          end;
          else
            if CurrentString <> GetCurrentEditorString then
              Cancel;
        end;
    end; {endif Sender = CurrentEditor}
end;

procedure TAbstractSynCompletion.SetCurrentString(const Value: UnicodeString);
begin
  fCurrentString := Value;
end;

procedure TAbstractSynCompletion.AddEditor(aEditor: TCustomSynEdit);
begin
  inherited AddEditor(aEditor);
end;

end.
