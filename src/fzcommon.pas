unit FZCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComponentEditors, ObjInspStrConsts, PropEdits, LMessages,
  Messages, LCLIntf, LCLType, LCLProc, math, fzbase, ComCtrls;

type

  { TFZEdit }

  TFZEdit = Class(TFZGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Text;
    property Alignment;
    property ReadOnly;
    property PassWordChar;
  end;

  { TFZMemo }

  TFZMemo = Class(TFZGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Lines;
    property Alignment;
    property ReadOnly;
  end;

  { TFZCombo }

  TFZCombo = Class(TFZGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Items;
    property ItemIndex;
    property Alignment;
    property ReadOnly;
  end;

  { TFZCheckBox }

  TFZCheckBox = Class(TFZGeneralEdit)
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Caption;
    property Alignment;
    property ReadOnly;
    property Checked;
  end;

  { TFZPanel }

  TFZPanel = class(TFZControl)
  private
    FHotTrack: Boolean;
    FHoverColor: Tcolor;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property ChildSizing;
    property Constraints;
	property HotTrack: Boolean read FHotTrack write FHotTrack;
	property HoverColor: TColor read FHoverColor write FHoverColor;
	property FZBorder: TFZBorder;
  end;

  TFZDevideLabel = class(TFZVirtualDevideLabel)

  end;

  { TFZStatusBar }

  TFZStatusBar = class(TFZVirtualStatusBar)
  end;

  { TFZButton }

  TFZButton = class(TFZVirtualButton)
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Down;
  end;


  { TFZDropDownButton }

  TFZDropDownButton = class(TFZVirtualDropDownButton)

  end;


  { TFZShell }

  TFZShell = class(TFZVirtualShell)
  end;

  { TFZPageControl }

  TFZPageControl = class(TFZVirtualPageControl)
  end;


implementation

{ TFZButton }

constructor TFZButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Appearance.Border.Normal.Color:= clSilver;
  Appearance.Border.Normal.Width:= 1;
end;

{ TFZPanel }

constructor TFZPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Appearance.Border.Normal.Color:= clSilver;
  Appearance.Border.Normal.Width:= 1;
  FHotTrack:= False;
end;


{ TFZCheckBox }

constructor TFZCheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geChechBox;
  Width:= 75;
  Height:= 23;
end;

{ TFZCombo }

constructor TFZCombo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geCombo;
  Width:= 75;
  Height:= 23;
end;

{ TFZMemo }

constructor TFZMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geMemo;
end;

{ TFZEdit }

constructor TFZEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geEdit;
  Width:= 75;
  Height:= 23;
end;

initialization
{$I fzcommon.lrs}

end.
