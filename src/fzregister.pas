unit FZRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComponentEditors, PropEdits, fzcommon, fzdb,
  FZBase, LResources;

type
  { TFZStringProperty }

  TFZStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TFZFieldEditor }

  TFZFieldEditor = class(TFZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;


  { TFZPageControlEditor }

  TFZPageControlEditor = class(TDefaultComponentEditor)
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function FZPageControl: TFZPageControl; virtual;
  end;

  { TFZActivePageEditor }

  TFZActivePageEditor = class(TComponentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TFZCaptionEditor }

  TFZCaptionEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function CreateDlg(s: TCaption): TStringsPropEditorDlg;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterNoIcon([TFZVirtualPage, TFZVirtualTabButton]);
  RegisterComponents('FZ-Common', [TFZEdit, TFZMemo, TFZCombo, TFZCheckBox,
    TFZPanel, TFZDevideLabel, TFZButton, TFZPageControl, TFZStatusBar, TFZShell,
    TFZDropDownButton]);
  RegisterComponentEditor([TFZPageControl], TFZPageControlEditor);
  RegisterPropertyEditor(ClassTypeInfo(TFZVirtualPage), TFZPageControl,
    'ActivePage', TFZActivePageEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TFZButton, 'Caption', TFZCaptionEditor);

  RegisterComponents('FZ-DB', [TFZSQLQuery, TFZDBEdit, TFZDBMemo, TFZDBCombo,
    TFZDBCheckBox, TFZDBLookupCombo, TFZDBLookupMemo, TFZDBGrid]);
  RegisterPropertyEditor(TypeInfo(string), TFZDBLookupCombo, 'KeyFields',
    TFZFieldEditor);
  RegisterPropertyEditor(TypeInfo(string), TFZDBLookupCombo, 'ListFields',
    TFZFieldEditor);
  RegisterPropertyEditor(TypeInfo(string), TFZDBLookupCombo, 'LookupFields',
    TFZFieldEditor);

  RegisterPropertyEditor(TypeInfo(string), TFZDBLookupMemo, 'KeyFields',
    TFZFieldEditor);
  RegisterPropertyEditor(TypeInfo(string), TFZDBLookupMemo, 'ListFields',
    TFZFieldEditor);
  RegisterPropertyEditor(TypeInfo(string), TFZDBLookupMemo, 'LookupFields',
    TFZFieldEditor);

end;

{ TFZCaptionEditor }

function TFZCaptionEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes + [paDialog];
end;

procedure TFZCaptionEditor.Edit;
var
  Dlg: TStringsPropEditorDlg;
begin
  Dlg:= CreateDlg(GetStrValue);
  try
    if (Dlg.ShowModal = mrOK) then
      SetStrValue(Dlg.Memo.Lines.Text);
  finally
    Dlg.Free;
  end;
end;

function TFZCaptionEditor.CreateDlg(s: TCaption): TStringsPropEditorDlg;
begin
  Result := TStringsPropEditorDlg.Create(nil);
  Result.Editor := Self;
  Result.Memo.Text := s;
  Result.MemoChange(nil); // force call OnChange event
end;

{ TFZActivePageEditor }

function TFZActivePageEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=(inherited GetAttributes)-[paMultiSelect];
end;

procedure TFZActivePageEditor.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  //Proc('(none)');
  with TFZPageControl(GetComponent(0)) do begin
    for i:=1 to ControlCount - 1 do
      Proc(Controls[i].Name);
  end;
end;


{ TFZFieldEditor }

procedure TFZFieldEditor.GetValueList(List: TStrings);
var i:integer;
begin
  with TFZDBGeneralEdit(GetComponent(0)) do begin
    if (ListDataSet = nil) then exit;
    for i:=0 to ListDataSet.FieldCount - 1 do
      List.Add(ListDataSet.Fields[i].FieldName);
  end;
end;

{ TFZPageControlEditor }

procedure TFZPageControlEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
  NewPage: TFZVirtualPage;
  NewName: string;
  PageCtrl: TFZPageControl;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;
  if Index > 0 then Exit; // ubah baris jika nanti tambah menu
  PageCtrl := FZPageControl;

  NewName := GetDesigner.CreateUniqueComponentName(PageCtrl.Name+'Pg');
  NewPage := TFZVirtualPage.Create(PageCtrl.Owner);
  NewPage.Name := NewName;
  NewPage.Parent := PageCtrl;
  NewPage.Align:= alClient;
  NewPage.Caption := 'PAGE'+IntToStr(PageCtrl.PageCount);
  PageCtrl.ActivePageIndex:= PageCtrl.PageCount - 1;

  Hook.PersistentAdded(NewPage, True);
  Modified;
end;

function TFZPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Page'; //nbcesAddPage;
  else
    Result := '';
  end;
end;

function TFZPageControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TFZPageControlEditor.FZPageControl: TFZPageControl;
begin
  Result := TFZPageControl(GetComponent);
end;

{ TFZStringProperty }

function TFZStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TFZStringProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for i := 0 to Pred(Values.Count) do
      Proc(Values[i]);
  finally
    Values.Free;
  end;
end;

end.

