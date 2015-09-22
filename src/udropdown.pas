unit udropdown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType;

type

  { Tfdropdown }

  Tfdropdown = class(TForm)
    Edit1: TEdit;
    ListBox1: TListBox;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fdropdown: Tfdropdown;

implementation

uses FZBase;

{$R *.lfm}

{ Tfdropdown }

procedure Tfdropdown.FormActivate(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure Tfdropdown.Edit1Change(Sender: TObject);
var i:integer;
begin
  ListBox1.ItemIndex:=-1;
  for i:=0 to ListBox1.Items.Count -1 do begin
    if Pos(LowerCase(Edit1.Text), LowerCase(ListBox1.Items[i])) > 0 then begin
      ListBox1.ItemIndex:= i;
      ListBox1Click(Sender);
      Exit;
    end;
  end;
end;

procedure Tfdropdown.Edit1Enter(Sender: TObject);
begin
  edit1.SelStart:=length(edit1.Text);
end;

procedure Tfdropdown.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var k:Word;
begin
  if (key = VK_DOWN) or (Key = VK_UP) then begin
    k := Key;
    Key := $0;
    if ListBox1.Items.Count = 0 then Exit;
    if (k = VK_DOWN) then begin
      if (ListBox1.ItemIndex < ListBox1.Items.Count - 1) then
        ListBox1.ItemIndex:= ListBox1.ItemIndex + 1
      else
        ListBox1.ItemIndex:= 0;
    end;
    if (k = VK_UP) then begin
      if (ListBox1.ItemIndex > 0 ) then
        ListBox1.ItemIndex:= ListBox1.ItemIndex - 1
      else
        ListBox1.ItemIndex:= ListBox1.Items.Count - 1;
    end;
    ListBox1Click(Sender);
  end;
end;

procedure Tfdropdown.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) or (Key = #27) then
    Close;
end;

procedure Tfdropdown.ListBox1Click(Sender: TObject);
begin
  TFZGeneralEdit(Owner).ItemIndex:=ListBox1.ItemIndex;
  if ListBox1.Focused then
    Edit1.Text:= ListBox1.Items[ListBox1.ItemIndex];
end;

procedure Tfdropdown.ListBox1DblClick(Sender: TObject);
begin
  Close;
end;

procedure Tfdropdown.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((key = VK_DOWN) and (ListBox1.ItemIndex=ListBox1.Items.Count - 1)) or
     ((Key = VK_UP)   and (ListBox1.ItemIndex=0)) then begin
    key := $0;
    Edit1.SetFocus;
  end;
end;

end.

