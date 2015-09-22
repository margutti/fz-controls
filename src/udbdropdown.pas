unit udbdropdown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, FZCommon, LCLType;

type

  { Tfdbdropdown }

  Tfdbdropdown = class(TForm)
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    FZPanel1: TFZPanel;
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure FilterData(DataSet: TDataSet; var Accept: Boolean);
  public
    { public declarations }
  end;

var
  fdbdropdown: Tfdbdropdown;

implementation

{$R *.lfm}

{ Tfdbdropdown }

procedure Tfdbdropdown.FormShow(Sender: TObject);
begin
  Datasource1.DataSet.OnFilterRecord:=@FilterData;
  Datasource1.DataSet.Filtered:= True;
end;

procedure Tfdbdropdown.FilterData(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept:= (Edit1.Text = '') or (Pos(LowerCase(Edit1.Text),
    LowerCase(Datasource1.DataSet.FieldByName(Self.Caption).AsString)) > 0);
end;

procedure Tfdbdropdown.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
    ModalResult:= mrOK
  else if (Key = #27) then
    ModalResult:= mrCancel;
end;

procedure Tfdbdropdown.FormActivate(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure Tfdbdropdown.Edit1Enter(Sender: TObject);
begin
  edit1.SelStart:=length(edit1.Text);
end;

procedure Tfdbdropdown.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var k:Word;
begin
  if (key = VK_DOWN) or (Key = VK_UP) then begin
    k := Key;
    Key := $0;
    if Datasource1.DataSet.RecordCount = 0 then Exit;
    if (k = VK_DOWN) then begin
      Datasource1.DataSet.Next;
      if Datasource1.DataSet.EOF then
        Datasource1.DataSet.First;
    end;
    if (k = VK_UP) then begin
      Datasource1.DataSet.Prior;
      if Datasource1.DataSet.BOF then
        Datasource1.DataSet.Last;
    end;
  end;
end;

procedure Tfdbdropdown.Edit1Change(Sender: TObject);
begin
  if Datasource1.DataSet = nil then exit;
  Datasource1.DataSet.Filtered:= False;
  Datasource1.DataSet.Filtered:= True;
  DBGrid1.Refresh;
end;

procedure Tfdbdropdown.DBGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((key = VK_DOWN) and (Datasource1.DataSet.EOF)) or
     ((Key = VK_UP)   and (Datasource1.DataSet.BOF)) then
  begin
    key := $0;
    Edit1.SetFocus;
  end;
end;

procedure Tfdbdropdown.DBGrid1DblClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

end.

