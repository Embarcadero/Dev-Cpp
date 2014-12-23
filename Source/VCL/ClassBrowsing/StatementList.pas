{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit StatementList;

interface

uses
  Classes, CBUtils, Forms;

type
  PStatementNode = ^TStatementNode;
  TStatementNode = record
    PrevNode: PStatementNode;
    NextNode: PStatementNode;
    Data: PStatement;
  end;

  { TStatementList class }
  TStatementList = class(TObject)
  private
    fCount: Integer;
    fFirstNode: PStatementNode;
    fLastNode: PStatementNode;
    fOwnsObjects: boolean;
    procedure DisposeNode(Node: PStatementNode);
    procedure OnNodeAdding(Node: PStatementNode); // call when about to add this node
    procedure OnNodeDeleting(Node: PStatementNode); // call when about to delete this node
  public
    constructor Create;
    destructor Destroy; override;
    function FirstStatement: PStatement;
    function LastStatement: PStatement;
    function Add(Data: PStatement): Integer;
    function DeleteFirst: Integer;
    function DeleteLast: Integer;
    function Delete(Node: PStatementNode): Integer; overload;
    function Delete(Data: PStatement): Integer; overload;
    function DeleteFromTo(FromNode, ToNode: PStatementNode): Integer;
    procedure Clear;
    property FirstNode: PStatementNode read fFirstNode;
    property LastNode: PStatementNode read fLastNode;
    property Count: Integer read fCount;
    property OwnsObjects: boolean read fOwnsObjects write fOwnsObjects;
  end;

implementation

{ TStatementList }

constructor TStatementList.Create;
begin
  fFirstNode := nil;
  fLastNode := nil;
  fCount := 0;
  fOwnsObjects := True;
end;

destructor TStatementList.Destroy;
begin
  Clear;
end;

function TStatementList.FirstStatement: PStatement;
begin
  if Assigned(fFirstNode) then
    Result := fFirstNode^.Data
  else
    Result := nil;
end;

function TStatementList.LastStatement: PStatement;
begin
  if Assigned(fLastNode) then
    Result := fLastNode^.Data
  else
    Result := nil;
end;

procedure TStatementList.OnNodeAdding(Node: PStatementNode);
begin
  // First node to add. It's alone
  if fCount = 0 then begin
    Node^.NextNode := nil;
    Node^.PrevNode := nil;
    fFirstNode := Node;
    fLastNode := Node;
  end else begin
    Node^.NextNode := nil;
    Node^.PrevNode := fLastNode; // previous last node
    Node^.PrevNode.NextNode := Node;
    fLastNode := Node; // new last node
  end;

  // Update count
  Inc(fCount);
end;

function TStatementList.Add(Data: PStatement): Integer;
var
  Node: PStatementNode;
begin
  // Create a new one
  Node := New(PStatementNode);
  Node^.Data := Data;
  OnNodeAdding(Node);
  Result := fCount;
end;

procedure TStatementList.DisposeNode(Node: PStatementNode);
begin
  if OwnsObjects then begin
    PStatement(Node^.Data)._InheritsFromStatements.Free;
    Dispose(PStatement(Node^.Data));
  end;
  Dispose(Node);
end;

procedure TStatementList.OnNodeDeleting(Node: PStatementNode);
begin
  // Special easy cases
  if fCount = 0 then
    Exit;
  if fCount = 1 then begin
    fFirstNode := nil;
    fLastNode := nil;
    fCount := 0;
    Exit;
  end;

  // Otherwise, first and last both exist
  if Node = fFirstNode then begin // update first node referals
    fFirstNode^.NextNode^.PrevNode := nil;
    fFirstNode := fFirstNode^.NextNode;
  end else if Node = fLastNode then begin // update last node referals
    fLastNode^.PrevNode^.NextNode := nil;
    fLastNode := fLastNode^.PrevNode;
  end else begin // update neighbor
    Node^.PrevNode^.NextNode := Node^.NextNode;
    Node^.NextNode^.PrevNode := Node^.PrevNode;
  end;
  Dec(fCount);
end;

function TStatementList.DeleteFirst: Integer;
begin
  Result := Delete(fFirstNode);
end;

function TStatementList.DeleteLast: Integer;
begin
  Result := Delete(fLastNode);
end;

function TStatementList.Delete(Node: PStatementNode): Integer;
begin
  if Assigned(Node) then begin
    OnNodeDeleting(Node); // updates information about linked list
    DisposeNode(Node);
  end;
  Result := fCount;
end;

function TStatementList.Delete(Data: PStatement): Integer;
var
  Node: PStatementNode;
begin
  Node := fFirstNode;
  while Assigned(Node) do begin
    if Node^.Data = Data then begin
      OnNodeDeleting(Node); // updates information about linked list
      DisposeNode(Node);
      break;
    end;
    Node := Node^.NextNode;
  end;
  Result := fCount;
end;

function TStatementList.DeleteFromTo(FromNode, ToNode: PStatementNode): Integer; // TODO: merge with Delete?
var
  Node: PStatementNode;
begin
  Node := FromNode;
  while Assigned(Node) do begin
    OnNodeDeleting(Node); // updates information about linked list
    DisposeNode(Node);
    break;
    if Node = ToNode then
      break;
    Node := Node^.NextNode;
  end;
  Result := fCount;
end;

procedure TStatementList.Clear;
var
  Node, NextNode: PStatementNode;
begin
  // Search all nodes
  Node := fFirstNode;
  while Assigned(Node) do begin
    NextNode := Node^.NextNode;
    // Do not call OnNodeDeleting, because all nodes will be cleared
    DisposeNode(Node);
    Node := NextNode;
  end;
  fFirstNode := nil;
  fLastNode := nil;
  fCount := 0;
end;

end.

