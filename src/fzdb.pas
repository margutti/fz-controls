unit FZDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComponentEditors, PropEdits, LCLIntf,
  LCLType, DB, Grids, DBGrids, fzbase, sqldb;

type

  { TFZSQLQuery }

  TFZSQLQuery = Class(TSQLQuery)
  protected
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
  end;

  { TFZColumn }

  TFZColumn = class(TColumn)
  private
    FEditor: TFZControl;
    procedure SetEditor(AEditor: TFZControl);
  published
    property Editor: TFZControl read FEditor write SetEditor;
  end;

  { TFZColumns }

  TFZColumns = class(TDBGridColumns)
  private
    function GetColumn(Index: Integer): TFZColumn;
    procedure SetColumn(Index: Integer; Value: TFZColumn);
  public
    function  Add: TFZColumn;
    property Items[Index: Integer]: TFZColumn read GetColumn write SetColumn; default;
  end;


  { TFZDBGrid }

  TFZDBGrid = class(TDBGrid)
  private
    function GetColumns: TFZColumns;
    procedure SetColumns(const AValue: TFZColumns);
    procedure InternalSelectEditor(Sender: TObject; Column: TColumn;
      var AEditor: TWinControl);
    procedure InternalKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    function CreateColumns: TGridColumns; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Columns: TFZColumns read GetColumns write SetColumns;
  end;


  { TFZDBEdit }

  TFZDBEdit = Class(TFZDBGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Alignment;
    property ReadOnly;
    property PassWordChar;
  end;

  { TFZDBMemo }

  TFZDBMemo = Class(TFZDBGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Alignment;
    property ReadOnly;
  end;

  { TFZDBCombo }

  TFZDBCombo = Class(TFZDBGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Items;
    property Alignment;
    property ReadOnly;
  end;

  { TFZDBCheckBox }

  TFZDBCheckBox = Class(TFZDBGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Caption;
    property Alignment;
    property ReadOnly;
    property Checked;
    property ValueChecked;
    property ValueUnChecked;
  end;

  { TFZDBLookupCombo }

  TFZDBLookupCombo = class(TFZDBGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Alignment;
    property ReadOnly;
    property ListDataSet;
    property KeyFields;
    property ListFields;
    property LookupFields;
  end;

  { TFZDBLookupMemo }

  TFZDBLookupMemo = Class(TFZDBGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Alignment;
    property ListDataSet;
    property KeyFields;
    property ListFields;
    property LookupFields;
  end;


implementation

{ TFZColumn }

procedure TFZColumn.SetEditor(AEditor: TFZControl);
begin
  if FEditor = AEditor then Exit;
  FEditor:= AEditor;
  if Assigned(FEditor) then
    FEditor.Visible:= False;
end;

{ TFZColumns }

function TFZColumns.GetColumn(Index: Integer): TFZColumn;
begin
  result := TFZColumn( inherited Items[Index] );
end;

procedure TFZColumns.SetColumn(Index: Integer; Value: TFZColumn);
begin
  Items[Index].Assign(Value);
end;

function TFZColumns.Add: TFZColumn;
begin
  result := TFZColumn( inherited add );
end;

{ TFZDBGrid }

function TFZDBGrid.GetColumns: TFZColumns;
begin
  result := TFZColumns( inherited Columns );
end;

procedure TFZDBGrid.SetColumns(const AValue: TFZColumns);
begin
  inherited Columns := TFZColumns(AValue);
end;

procedure TFZDBGrid.InternalSelectEditor(Sender: TObject; Column: TColumn;
  var AEditor: TWinControl);
begin
  if not Assigned(TFZColumn(Column).Editor) then Exit;
  TFZColumn(Column).Editor.BoundsRect:= SelectedFieldRect;
  AEditor:= TFZColumn(Column).Editor;
  AEditor.OnKeyDown:=@InternalKeyDown;
end;

procedure TFZDBGrid.InternalKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(Key, Shift);
  SetFocus;
end;

function TFZDBGrid.CreateColumns: TGridColumns;
begin
  Result := TFZColumns.Create(Self, TFZColumn);
end;

constructor TFZDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnSelectEditor:=@InternalSelectEditor;
end;


{ TFZDBLookupMemo }

constructor TFZDBLookupMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geMemo;
  ReadOnly:= True;
end;

{ TFZDBLookupCombo }

constructor TFZDBLookupCombo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geCombo;
  Width:= 75;
  Height:= 23;
end;

{ TFZSQLQuery }

procedure TFZSQLQuery.InternalOpen;
var
  i: Integer;
  s: String;
begin
  for i:=0 to FieldCount - 1 do begin
    if Fields[i].FieldKind in [fkCalculated, fkLookup] then begin
      Fields[i].LookupCache:= True;
      Fields[i].ProviderFlags:= [];
    end;
    if LowerCase(Fields[i].FieldName) = 'id' then
      Fields[i].ProviderFlags:= Fields[i].ProviderFlags + [pfInKey];
    if Fields[i].LookupDataSet <> nil then Fields[i].LookupDataSet.Open;
  end;
  for i:=0 to Params.Count - 1 do begin
    if Params[i].DataType <> ftUnknown then Continue;
    if (LowerCase(Params[i].Name) = 'awal') or (LowerCase(Params[i].Name) = 'akhir') then
      Params[i].DataType:=ftDate
    else
      Params[i].DataType:=ftString;
  end;

  s:= IndexFieldNames;
  IndexFieldNames:= '';

  inherited InternalOpen;

  IndexFieldNames:= s;
  for i:=0 to FieldCount - 1 do begin
    if Fields[i] is TFloatField then TFloatField(Fields[i]).Currency:= True;
    if Fields[i] is TDateField then TDateField(Fields[i]).EditMask:= '99/99/9999;1;_';
  end;

end;

procedure TFZSQLQuery.InternalRefresh;
var
  i  : integer;
  bm, s : String;
begin
  bm:='';
  for i:=0 to FieldCount - 1 do begin
    if Fields[i].LookupDataSet <> nil then begin
      Fields[i].LookupDataSet.Refresh;
      Fields[i].RefreshLookupList;
    end;
    if LowerCase(Fields[i].FieldName) = 'id' then bm:= Fields[i].AsString;
  end;

  s:= IndexFieldNames;
  IndexFieldNames:= '';

  inherited InternalRefresh;

  IndexFieldNames:= s;
  if bm <> '' then Locate('id', bm, []);
end;

{ TFZDBCheckBox }

constructor TFZDBCheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geChechBox;
  Width:= 75;
  Height:= 23;
end;

{ TFZDBCombo }

constructor TFZDBCombo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geCombo;
  Width:= 75;
  Height:= 23;
end;

{ TFZDBMemo }

constructor TFZDBMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geMemo;
end;

{ TFZDBEdit }

constructor TFZDBEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geEdit;
  Width:= 75;
  Height:= 23;
end;

initialization
{$I fzdb.lrs}

end.

