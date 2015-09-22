unit FZBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, LMessages, math, Menus,
  Forms, Dialogs, LCLIntf, maskedit, DB, DbCtrls, variants, LResources;

type

  { General Type}

  TFZDrawStyle = (fzdDefault);
  TFZMouseState  = (msNone, msHover, msClicked);
  TFZBorderOption = (mboTop, mboLeft, mboBottom, mboRight);
  TFZBorderOptions = set of TFZBorderOption;
  TFZSysButtonStyle = (sbsMinimize, sbsRestore, sbsMaximize, sbsClose,
    sbsDropDown, sbsEllips, sbsUnCheck, sbsCheck, sbsPartialCheck);
  TFZGeneralEditStyle = (geEdit, geMemo, geCombo, geGrid, geChechBox, geButton);
  TFZImageStyle = (isLeft, isRight, isTop, isBottom);

  { TFZColor }

  TFZColor = class(TPersistent)
  private
    FControl: TCustomControl;
    FHotTrack: Boolean;
    FHover: TColor;
    FNormal: TColor;
    procedure SetNormal(AValue: TColor);
  public
    constructor Create(AControl: TCustomControl);
  published
    property Normal: TColor read FNormal write SetNormal;
    property Hover: TColor read FHover write FHover;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
  end;

  { TFZFont }

  TFZFont = class(TPersistent)
  private
    FControl: TCustomControl;
    FHotTrack: Boolean;
    FNormal: TFont;
    FHover: TFont;
    procedure SetNormal(AValue: TFont);
  public
    constructor Create(AControl: TCustomControl);
    destructor Destroy; override;
  published
    property Normal: TFont read FNormal write SetNormal;
    property Hover: TFont read FHover write FHover;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
  end;

  { TFZBorderItem }

  TFZBorderItem = class(TPersistent)
  private
    FControl: TCustomControl;
    FColor: TColor;
    FVisible: Boolean;
    FWidth: Integer;
    FOption: TFZBorderOptions;
    procedure SetColor(AValue: TColor);
    procedure SetOption(AValue: TFZBorderOptions);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create(AControl: TCustomControl);
  published
    property Color: TColor read FColor write SetColor;
    property Width: Integer read FWidth write SetWidth;
    property Option: TFZBorderOptions read FOption write SetOption;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TFZBorder }

  TFZBorder = class(TPersistent)
  private
    FControl: TCustomControl;
    FNormal: TFZBorderItem;
    FHover: TFZBorderItem;
    FHotTrack: Boolean;
    FColor: TColor;
    procedure SetColor(AValue: TColor);
  public
    constructor Create(AControl: TCustomControl);
  published
    property Normal: TFZBorderItem read FNormal write FNormal;
    property Hover: TFZBorderItem read FHover write FHover;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
    property Color: TColor read FColor write SetColor;
  end;

  { TFZIconItem }

  TFZIconItem = class(TPersistent)
  private
    FColor: TColor;
    FControl: TCustomControl;
    FWidth: Integer;
    procedure SetColor(AValue: TColor);
  public
    constructor Create(AControl: TCustomControl);
  published
    property Color: TColor read FColor write SetColor;
    property Width: Integer read FWidth write FWidth;
  end;


  { TFZIcon }

  TFZIcon = class(TPersistent)
  private
    FControl: TCustomControl;
    FHotTrack: Boolean;
    FHover: TFZIconItem;
    FNormal: TFZIconItem;
  public
    constructor Create(AControl: TCustomControl);
    destructor Destroy; override;
  published
    property Normal: TFZIconItem read FNormal write FNormal;
    property Hover: TFZIconItem read FHover write FHover;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
  end;

  { TFZAppearance }

  TFZAppearance = class(TPersistent)
  private
    FControl: TCustomControl;
    FBorder: TFZBorder;
    FColor: TFZColor;
    FFont: TFZFont;
  public
    constructor Create(AControl: TCustomControl); virtual;
    destructor Destroy; override;
  published
    property Border: TFZBorder read FBorder write FBorder;
    property Color: TFZColor read FColor write FColor;
    property Font: TFZFont read FFont write FFont;
  end;

  { TFZAppearanceEx }

  TFZAppearanceEx = class(TFZAppearance)
  private
    FIcon: TFZIcon;
  public
    constructor Create(AControl: TCustomControl); override;
    destructor Destroy; override;
  published
    property Icon: TFZIcon read FIcon write FIcon;
  end;

  { TFZTabAppearance }

  TFZTabAppearance = class(TPersistent)
  private
    FControl: TCustomControl;
    FActive: TFZAppearance;
    FInActive: TFZAppearance;
  public
    constructor Create(Acontrol: TCustomControl);
    destructor Destroy; override;
  published
    property Active: TFZAppearance read FActive write FActive;
    property InActive: TFZAppearance read FInActive write FInActive;
  end;

  { TFZControl }

  TFZControl = class(TCustomControl)
  private
    FAppearance: TFZAppearance;
    FAppearanceDown: TFZAppearance;
    FDown: Boolean;
    FMouseState: TFZMouseState;
    FOnChange: TNotifyEvent;
  protected
    function CreateAppearance: TFZAppearance; virtual;
    procedure PaintBorder; virtual;
    procedure Paint; override;
    procedure SetDown(AValue: Boolean); virtual;
    procedure RealSetText(const AValue: TCaption); override;
    procedure AdjustClientRect(var ARect: TRect); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Change; Virtual;
    procedure DoTheKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoTheKeyPress(var Key: Char); virtual;
    procedure DoClearKey; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Down: Boolean read FDown write SetDown;
    property AppearanceDown: TFZAppearance read FAppearanceDown write FAppearanceDown;
    property MouseState: TFZMouseState read FMouseState write FMouseState;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Appearance: TFZAppearance read FAppearance write FAppearance;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property BorderWidth;
    property Visible;
    property TabStop;
    property TabOrder;
    property Cursor;
    property ShowHint;
    property Hint;
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUnDock;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TFZVirtualDevideLabel }

  TFZVirtualDevideLabel = class(TFZControl)
  protected
    procedure PaintBorder; override;
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Caption;
  end;

  { TFZControlEx }

  TFZControlEx = class(TFZControl)
  private
    function GetAppearance: TFZAppearanceEx;
    function GetAppearanceDown: TFZAppearanceEx;
    procedure SetAppearance(AValue: TFZAppearanceEx);
    procedure SetAppearanceDown(AValue: TFZAppearanceEx);
  protected
    function CreateAppearance: TFZAppearance; override;
    property AppearanceDown: TFZAppearanceEx read GetAppearanceDown write SetAppearanceDown;
  published
    property Appearance: TFZAppearanceEx read GetAppearance write SetAppearance;
  end;

  { TFZSysButton }

  TFZSysButton = class(TFZControlEx)
  private
    FButtonStyle: TFZSysButtonStyle;
    procedure SetButtonStyle(AValue: TFZSysButtonStyle);
  protected
    procedure Paint; override;
    procedure Click; override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property ButtonStyle: TFZSysButtonStyle read FButtonStyle write SetButtonStyle;
  end;


  { TFZVirtualButton }

  TFZVirtualButton = class(TFZControl)
  private
    FIndent: Integer;
    FGroupIndex: Integer;
    FSpacing: Integer;
    FShowCaption: Boolean;
    FImage: TPicture;
    FImageStyle: TFZImageStyle;
    procedure SetSpacing(AValue: integer);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetImageStyle(AValue: TFZImageStyle);
    procedure SetIndent(AValue: Integer);
  protected
    procedure SetDown(AValue: Boolean); override;
    procedure Paint; override;
    procedure Click; override;
    procedure ImageChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Image: TPicture read FImage write FImage;
    property ImageStyle: TFZImageStyle read FImageStyle write SetImageStyle;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property Spacing: Integer read FSpacing write SetSpacing;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property Indent: Integer read FIndent write SetIndent;
    property AppearanceDown;
    property Action;
    property Caption;
    property Constraints;
  end;

  { TMyWinControl }

  TMyWinControl = class(TWinControl)
  published
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnClick;
  end;

  { TFZVirtualEdit }

  TFZVirtualEdit = class(TPersistent)
  private
    FControl: TFZControl;
    FComponent: TMyWinControl;
    FButton: TMyWinControl;
    procedure ControlChange(Sender: TObject);
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ControlKeyPress(Sender: TObject; var Key: Char);
  protected
  public
    constructor Create(AComponent: TComponentClass; AOwner: TFZControl;
      AButton: TComponentClass = Nil; BtnRight: Boolean = True);
    destructor Destroy; override;
  end;

  { TFZGeneralEdit }

  TFZGeneralEdit = class(TFZControlEx)
  private
    FEdit: TFZVirtualEdit;
    FEditStyle: TFZGeneralEditStyle;
    FItems: TStrings;
    FChecked: Boolean;
    function GetLines: TStrings;
    procedure SetEditStyle(AEditStyle: TFZGeneralEditStyle);
    procedure SetItems(AValue: TStrings);
    procedure SetLines(AValue: TStrings);
    function GetAlignment: TAlignment;
    procedure SetAlignment(AValue: TAlignment);
    function GetPassWordChar: Char;
    procedure SetPassWordChar(AChar: Char);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(AValue: Boolean);
    function GetItemIndex: Integer;
    procedure SetItemIndex(AValue: Integer);
  protected
    procedure Paint; override;
    procedure TheMouseEnter(Sender: TObject);
    procedure TheMouseLeave(Sender: TObject);
    procedure TheMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TheMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ButtonClick(Sender: TObject); virtual;
    procedure InternalClick(Sender: TObject); virtual;
    function RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetChecked(AValue: Boolean); virtual;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    property EditStyle: TFZGeneralEditStyle read FEditStyle write SetEditStyle;
    property Lines: TStrings read GetLines write SetLines;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property PassWordChar: Char read GetPassWordChar write SetPassWordChar;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Checked: Boolean read FChecked write SetChecked;
  published
    property Constraints;
    property OnChange;
  end;

  { TFZDBGeneralEdit }

  TFZDBGeneralEdit = class(TFZGeneralEdit)
  private
    FDataLink: TFieldDataLink;
    FLookupDataSet: TDataSet;
    FKeyFields: String;
    FListFields: String;
    FLookupFields: String;
    FValueChecked: String;
    FValueUnChecked: String;
    FOnEditReference: TNotifyEvent;
    function GetDataField: String;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetCheckValue: String;
    procedure SetDataField(const Value: String);
    procedure SetDataSource(const Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DataChange(Sender: TObject); // data changed in table
    procedure ModifyRecord;
    procedure UpdateData(Sender: TObject); // change data in table
    procedure CmExit(var Message: TCmExit); message CM_Exit;
    procedure InternalUpdateRecord;
    procedure SetChecked(AValue: Boolean); override;
    procedure SetListFields(AValue: String);
    procedure SetLookupFields(AValue: String);
    procedure DoClearKey; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
    property ValueChecked: String read FValueChecked write FValueChecked;
    property ValueUnChecked: String read FValueUnChecked write FValueUnChecked;
    property ListDataSet: TDataSet read FLookupDataSet write FLookupDataSet;
    property KeyFields: String read FKeyFields write FKeyFields;
    property ListFields: String read FListFields write SetListFields;
    property LookupFields: String read FLookupFields write FLookupFields;
    property OnEditReference: TNotifyEvent read FOnEditReference write FOnEditReference;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: String read GetDataField write SetDataField;
  end;

  { TFZVirtualDropDownButton }

  TFZVirtualDropDownButton = class(TFZGeneralEdit)
  private
    FDropDownMenu: TPopupMenu;
    function GetImage: TPicture;
    function GetImageStyle: TFZImageStyle;
    function GetShowCaption: Boolean;
    function GetSpacing: Integer;
    procedure SetImage(AValue: TPicture);
    procedure SetImageStyle(AValue: TFZImageStyle);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetSpacing(AValue: Integer);
  protected
    procedure ButtonClick(Sender: TObject); override;
  public
    constructor Create(TheOwner: TComponent);override;
  published
    property Image: TPicture read GetImage write SetImage;
    property ImageStyle: TFZImageStyle read GetImageStyle write SetImageStyle;
    property Spacing: Integer read GetSpacing write SetSpacing;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property Caption;
  end;

  { TFZVirtualStatusBar }

  TFZVirtualStatusBar = class(TFZControl)
  private
    FIndicatorCaption: TCaption;
    procedure SetIndicatorCaption(ACaption: TCaption);
  protected
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property IndicatorCaption: TCaption read FIndicatorCaption write SetIndicatorCaption;
    property Caption;
  end;

  { TFZVirtualShell }

  TFZVirtualShell = class(TFZControl)
  private
    FX, FY: Integer;
    FSettingButton: TFZVirtualButton;
    FHelpButton: TFZVirtualButton;
    FMinButton: TFZSysButton;
    FRestoreButton: TFZSysButton;
    FCloseButton: TFZSysButton;
    FButtonAppearance: TFZAppearanceEx;
    FOnHelpClick: TNotifyEvent;
    FOnSettingClick: TNotifyEvent;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure HelpClicked(Sender: TObject);
    procedure SettingClicked(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ButtonAppearance: TFZAppearanceEx read FButtonAppearance write FButtonAppearance;
    property OnHelpClick: TNotifyEvent read FOnHelpClick write FOnHelpClick;
    property OnSettingClick: TNotifyEvent read FOnSettingClick write FOnSettingClick;
    property Caption;
  end;

  // Page Control definition

  TFZVirtualPageControl = class;
  TFZVirtualPage = class;

  { TFZScrollBtn }

  TFZScrollBtn = class(TFZVirtualButton)
  protected
    procedure Paint; override;
  end;

  { TFZTitlePanel }

  TFZTitlePanel = class(TFZControl)
  private
    FChildWidth: Integer;
    FLeftBtn: TFZScrollBtn;
    FRightBtn: TFZScrollBtn;
    FScrollVisible: Boolean;
    procedure SetScrollVisible(AValue: Boolean);
    procedure LefBtnClick(Sender: TObject);
    procedure RightBtnClick(Sender: TObject);
  protected
    procedure PaintBorder; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ScrollVisible: Boolean read FScrollVisible write SetScrollVisible;
  end;

  { TFZVirtualTabMenu }

  TFZVirtualTabMenu = class(TFZVirtualButton)
  protected
    procedure Paint; override;
  end;

  { TFZVirtualTabButton }

  TFZVirtualTabButton = class(TFZVirtualButton)
  private
    FPage: TFZVirtualPage;
  protected
    procedure Paint; override;
    procedure Click; override;
  public
    constructor Create(TheOwner: TComponent; Container: TFZControl; ThePage: TFZVirtualPage);
    property Page: TFZVirtualPage read FPage write FPage stored True;
  end;

  { TFZVirtualPage }

  TFZVirtualPage = class(TFZControl)
  private
    FControlChanging: Boolean;
    FButton: TFZVirtualTabButton;
    FButtonWidth: Integer;
    procedure SetButtonWidth(Value: Integer);
  protected
    procedure RealSetText(const AValue: TCaption); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetVisible(Value: Boolean); override;
    procedure Paint; override;
    procedure ShowMe;
    procedure HideMe;
  public
    constructor Create(TheOwner: TComponent); override;
    property Button: TFZVirtualTabButton read FButton write FButton stored True;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth Stored True;
  published
    property Caption;
  end;

  { TFZVirtualPageControl }

  TFZVirtualPageControl = class(TFZControl)
  private
    FTitlePanel: TFZTitlePanel;
    FMenuPanel: TFZVirtualTabMenu;
    FTitleHeight: Integer;
    FMenuWidth: Integer;
    FShowMenu: Boolean;
    FShowTabs: Boolean;
    FActivePage: TFZVirtualPage;
    FMenuAppearance: TFZAppearance;
    FTabAppearance: TFZTabAppearance;
    FOnMenuClick: TNotifyEvent;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): TFZVirtualPage;
    function GetActivePageIndex: Integer;
    function GetIndexOfPage(AName: String): Integer;
    procedure SetTitleHeight(AValue: Integer);
    procedure SetMenuWidth(AValue: Integer);
    procedure SetShowMenu(AValue: Boolean);
    procedure SetShowTabs(AValue: Boolean);
    procedure SetActivePage(APage: TFZVirtualPage);
    procedure SetActivePageIndex(APageIndex: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure MenuClick(Sender: TObject);
    procedure CreateItem;
    procedure ArrangeButton;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Pages[Index: Integer]: TFZVirtualPage read GetPage;
    property TitlePanel: TFZTitlePanel read FTitlePanel;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property MenuWidth: Integer read FMenuWidth write SetMenuWidth;
  published
    property ActivePage: TFZVirtualPage read FActivePage write SetActivePage;
    property MenuAppearance: TFZAppearance read FMenuAppearance write FMenuAppearance;
    property TabAppearance: TFZTabAppearance read FTabAppearance write FTabAppearance;
    property PageCount: Integer read GetPageCount;
    property ShowMenu: Boolean read FShowMenu write SetShowMenu;
    property ShowTabs: Boolean read FShowTabs write SetShowTabs;
    property TitleHeight: Integer read FTitleHeight write SetTitleHeight;
    property Caption;
    property OnMenuClick: TNotifyEvent read FOnMenuClick write FOnMenuClick;
    property OnChange;
  end;



implementation

//{$I atterbilang1_icon.lrs}

uses udropdown, udbdropdown;

{ TFZScrollBtn }

procedure TFZScrollBtn.Paint;
begin
  Appearance:= TFZVirtualPageControl(Parent.Parent).TabAppearance.InActive;
  inherited Paint;
end;

{ TFZDevideLabel }

procedure TFZVirtualDevideLabel.PaintBorder;
begin
  //do nothing;
end;

procedure TFZVirtualDevideLabel.Paint;
var x, y, z: integer;
    LineVisible: Boolean;
begin
  inherited Paint;
  if (MouseState=msHover) and Appearance.Font.HotTrack then
    Canvas.Font:= Appearance.Font.Hover
  else
    Canvas.Font:= Appearance.Font.Normal;
  y:= (Height - Canvas.TextHeight(Caption)) div 2;
  Canvas.TextOut(0, y, Caption);
  if (MouseState=msHover) and Appearance.Border.HotTrack then begin
    Canvas.Pen.Color:= Appearance.Border.Hover.Color;
    Canvas.Pen.Width:= Appearance.Border.Hover.Width;
    LineVisible:= Appearance.Border.Hover.Visible;
    z:= Appearance.Border.Hover.Width;
  end else begin
    Canvas.Pen.Color:= Appearance.Border.Normal.Color;
    Canvas.Pen.Width:= Appearance.Border.Normal.Width;
    LineVisible:= Appearance.Border.Normal.Visible;
    z:= Appearance.Border.Normal.Width;
  end;
  if (z = 0) or not LineVisible then Exit;
  y:= (Height - z) div 2;
  x:= Canvas.TextWidth(Caption) + 4;
  Canvas.Line(x, y, Width, y);
end;

constructor TFZVirtualDevideLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAppearance.Border.Normal.Color:= clSilver;
  FAppearance.Border.Normal.Width:= 1;
  Height:= 20;
end;

{ TFZControlEx }

function TFZControlEx.GetAppearance: TFZAppearanceEx;
begin
  Result:= TFZAppearanceEx(inherited Appearance);
end;

function TFZControlEx.GetAppearanceDown: TFZAppearanceEx;
begin
  Result:= TFZAppearanceEx(inherited AppearanceDown);
end;

procedure TFZControlEx.SetAppearance(AValue: TFZAppearanceEx);
begin
  inherited Appearance:= TFZAppearanceEx(AValue);
end;

procedure TFZControlEx.SetAppearanceDown(AValue: TFZAppearanceEx);
begin
  inherited AppearanceDown:= TFZAppearanceEx(AValue);
end;

function TFZControlEx.CreateAppearance: TFZAppearance;
begin
  Result:= TFZAppearanceEx.Create(Self);
end;

{ TFZAppearanceEx }

constructor TFZAppearanceEx.Create(AControl: TCustomControl);
begin
  inherited Create(AControl);
  FIcon:= TFZIcon.Create(AControl);
end;

destructor TFZAppearanceEx.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

{ TFZTitlePanel }

procedure TFZTitlePanel.SetScrollVisible(AValue: Boolean);
begin
  if FScrollVisible=AValue then Exit;
  FScrollVisible:=AValue;
  FLeftBtn.Visible:= AValue;
  FRightBtn.Visible:= AValue;
end;

procedure TFZTitlePanel.LefBtnClick(Sender: TObject);
var i:integer;
begin
  for i:= ControlCount - 1 Downto 0 do begin
    if (Controls[i] is TFZVirtualTabMenu) or (Controls[i].Align = alNone) then Continue;
    if Controls[i].Visible then begin
      Controls[i].Visible:= False;
      Break;
    end;
  end;
  for i:= 0 to ControlCount - 1 do begin
    if (Controls[i] is TFZVirtualTabMenu) or (Controls[i].Align = alNone) then Continue;
    if not Controls[i].Visible then begin
      Controls[i].Visible:= True;
      Break;
    end;
  end;
  if TFZVirtualPageControl(Parent).ActivePageIndex > 0 then
    TFZVirtualPageControl(Parent).ActivePageIndex:=
      TFZVirtualPageControl(Parent).ActivePageIndex - 1;
end;

procedure TFZTitlePanel.RightBtnClick(Sender: TObject);
var i:integer;
begin
  for i:= 0 to ControlCount - 1 do begin
    if (Controls[i] is TFZVirtualTabMenu) or (Controls[i].Align = alNone) then Continue;
    if Controls[i].Visible then begin
      Controls[i].Visible:= False;
      Break;
    end;
  end;
  for i:= ControlCount - 1 Downto 0 do begin
    if (Controls[i] is TFZVirtualTabMenu) or (Controls[i].Align = alNone) then Continue;
    if not Controls[i].Visible then begin
      Controls[i].Visible:= True;
      Break;
    end;
  end;
  if TFZVirtualPageControl(Parent).ActivePageIndex <
    TFZVirtualPageControl(Parent).PageCount - 1 then
    TFZVirtualPageControl(Parent).ActivePageIndex:=
      TFZVirtualPageControl(Parent).ActivePageIndex + 1;
end;

procedure TFZTitlePanel.PaintBorder;
begin
  // do nothing;
end;

procedure TFZTitlePanel.Paint;
begin
  Appearance:= TFZVirtualPageControl(Parent).Appearance;
  inherited Paint;
end;

procedure TFZTitlePanel.Resize;
begin
  inherited Resize;
  if not Assigned(FRightBtn) then Exit;
  FRightBtn.Left:= Width - FRightBtn.Width;
  FLeftBtn.Left:= FRightBtn.Left - FLeftBtn.Width;
  //ScrollVisible:= FChildWidth > Width;
end;

constructor TFZTitlePanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FChildWidth:= 0;

  FRightBtn:= TFZScrollBtn.Create(Self);
  FRightBtn.ControlStyle:= FRightBtn.ControlStyle + [csNoDesignVisible];
  FRightBtn.Width:= 10;
  FRightBtn.Height:= 22;
  FRightBtn.Caption:= '>';
  FRightBtn.Parent:= Self;
  FRightBtn.OnClick:= @RightBtnClick;
  FRightBtn.Visible:= True;

  FLeftBtn:= TFZScrollBtn.Create(Self);
  FLeftBtn.ControlStyle:= FLeftBtn.ControlStyle + [csNoDesignVisible];
  FLeftBtn.Width:= 10;
  FLeftBtn.Height:= 22;
  FLeftBtn.Caption:= '<';
  FLeftBtn.Parent:= Self;
  FLeftBtn.OnClick:= @LefBtnClick;
  FLeftBtn.Visible:= True;

  FScrollVisible:= True; //False;
end;

destructor TFZTitlePanel.Destroy;
begin
  FLeftBtn.Free;
  FRightBtn.Free;
  inherited Destroy;
end;


{ TFZVirtualTabMenu }

procedure TFZVirtualTabMenu.Paint;
begin
  Appearance:= TFZVirtualPageControl(Parent.Parent).MenuAppearance;
  inherited Paint;
end;

{ TFZTabAppearance }

constructor TFZTabAppearance.Create(Acontrol: TCustomControl);
begin
  inherited Create;
  FControl:= Acontrol;
  FActive:= TFZAppearance.Create(Acontrol);
  FInActive:= TFZAppearance.Create(Acontrol);
end;

destructor TFZTabAppearance.Destroy;
begin
  FActive.Free;
  FInActive.Free;
  inherited Destroy;
end;

{ TFZIconItem }

procedure TFZIconItem.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  FControl.Invalidate;
end;

constructor TFZIconItem.Create(AControl: TCustomControl);
begin
  inherited Create;
  FControl:= AControl;
  FWidth:= 1;
end;

{ TFZIcon }

constructor TFZIcon.Create(AControl: TCustomControl);
begin
  inherited Create;
  FControl:= AControl;
  FNormal:= TFZIconItem.Create(AControl);
  FHover:= TFZIconItem.Create(AControl);
end;

destructor TFZIcon.Destroy;
begin
  FNormal.Free;
  FHover.Free;
  inherited Destroy;
end;

{ TFZBorderItem }

procedure TFZBorderItem.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  FControl.Invalidate;
end;

procedure TFZBorderItem.SetOption(AValue: TFZBorderOptions);
begin
  if FOption=AValue then Exit;
  FOption:=AValue;
  FControl.Invalidate;
end;

procedure TFZBorderItem.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  FControl.Invalidate;
end;

procedure TFZBorderItem.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  if FWidth > 0 then
    FControl.Perform(CM_BORDERCHANGED, 0, 0);
  FControl.Invalidate;
end;

constructor TFZBorderItem.Create(AControl: TCustomControl);
begin
  inherited Create;
  FControl:= AControl;
  FOption:= [mboTop, mboLeft, mboBottom, mboRight];
  FVisible:= True;
  FWidth:= 0;
end;

{ TFZFont }

procedure TFZFont.SetNormal(AValue: TFont);
begin
  if FNormal = AValue then Exit;
  FNormal:= AValue;
  FControl.Invalidate;
end;

constructor TFZFont.Create(AControl: TCustomControl);
begin
  inherited Create;
  FControl:= AControl;
  FNormal:= TFont.Create;
  FHover:= TFont.Create;
  FHotTrack:= False;
end;

destructor TFZFont.Destroy;
begin
  FNormal.Free;
  FHover.Free;
  inherited Destroy;
end;

{ TFZColor }

constructor TFZColor.Create(AControl: TCustomControl);
begin
  inherited Create;
  FControl:= AControl;
  FHotTrack:= False;
  FNormal:= clDefault;
  FHover:= clDefault;
end;

procedure TFZColor.SetNormal(AValue: TColor);
begin
  if FNormal=AValue then Exit;
  FNormal:=AValue;
  FControl.Invalidate;
end;

{ TFZAppearance }

constructor TFZAppearance.Create(AControl: TCustomControl);
begin
  inherited Create;
  FControl:= AControl;

  FBorder:= TFZBorder.Create(AControl);
  FColor:= TFZColor.Create(AControl);
  FFont:= TFZFont.Create(AControl);
end;

destructor TFZAppearance.Destroy;
begin
  FBorder.Free;
  FColor.Free;
  FFont.Free;
  inherited Destroy;
end;

{ TFZVirtualDropDownButton }

function TFZVirtualDropDownButton.GetImage: TPicture;
begin
  Result:= TFZVirtualButton(FEdit.FComponent).Image;
end;

function TFZVirtualDropDownButton.GetImageStyle: TFZImageStyle;
begin
  Result:= TFZVirtualButton(FEdit.FComponent).ImageStyle;
end;

function TFZVirtualDropDownButton.GetShowCaption: Boolean;
begin
  Result:= TFZVirtualButton(FEdit.FComponent).ShowCaption;
end;

function TFZVirtualDropDownButton.GetSpacing: Integer;
begin
  Result:= TFZVirtualButton(FEdit.FComponent).Spacing;
end;

procedure TFZVirtualDropDownButton.SetImage(AValue: TPicture);
begin
  TFZVirtualButton(FEdit.FComponent).Image.Assign(AValue);
end;

procedure TFZVirtualDropDownButton.SetImageStyle(AValue: TFZImageStyle);
begin
  TFZVirtualButton(FEdit.FComponent).ImageStyle:= AValue;
end;

procedure TFZVirtualDropDownButton.SetShowCaption(AValue: Boolean);
begin
  TFZVirtualButton(FEdit.FComponent).ShowCaption:= AValue;
end;

procedure TFZVirtualDropDownButton.SetSpacing(AValue: Integer);
begin
  TFZVirtualButton(FEdit.FComponent).Spacing:= AValue;
end;

procedure TFZVirtualDropDownButton.ButtonClick(Sender: TObject);
var p: TPoint;
begin
  if FDropDownMenu <> nil then begin
    p:= Self.ClientToScreen(Point(0, 0));
    FDropDownMenu.PopUp(p.X, p.Y + Height);
  end;
end;

constructor TFZVirtualDropDownButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EditStyle:= geButton;
  Width:= 75;
  Height:= 25;
  Appearance.Border.Normal.Color:= clSilver;
  Appearance.Border.Normal.Width:= 1;
end;

{ TFZVirtualPageControl }

procedure TFZVirtualPageControl.SetTitleHeight(AValue: Integer);
begin
  if FTitleHeight = AValue then Exit;
  FTitleHeight:= AValue;
  if FTitlePanel <> nil then
    FTitlePanel.Height:= AValue;
end;

procedure TFZVirtualPageControl.SetMenuWidth(AValue: Integer);
begin
  if FMenuWidth = AValue then Exit;
  FMenuWidth:= AValue;
  if FMenuPanel <> nil then
    FMenuPanel.Width:= AValue;
end;

procedure TFZVirtualPageControl.SetShowMenu(AValue: Boolean);
begin
  if FShowMenu = AValue then Exit;
  FShowMenu:= AValue;
  if FMenuPanel <> nil then
    FMenuPanel.Visible:= AValue;
end;

procedure TFZVirtualPageControl.SetShowTabs(AValue: Boolean);
begin
  if FShowTabs = AValue then Exit;
  FShowTabs:= AValue;
  if FTitlePanel <> nil then
    FTitlePanel.Visible:= AValue;
end;

function TFZVirtualPageControl.GetPageCount: Integer;
begin
  Result:= ControlCount - 1;
end;

function TFZVirtualPageControl.GetPage(Index: Integer): TFZVirtualPage;
begin
  Result:= nil;
  if (Index > PageCount - 1) or (Index < 0) then Exit;
  if Controls[Index + 1] is TFZVirtualPage then
    Result:= TFZVirtualPage(Controls[Index + 1]);
end;

function TFZVirtualPageControl.GetActivePageIndex: Integer;
begin
  if Assigned(FActivePage) then
    Result:= GetIndexOfPage(FActivePage.Name)
  else
    Result:= -1;
end;

function TFZVirtualPageControl.GetIndexOfPage(AName: String): Integer;
var i: integer;
begin
  Result:= -1;
  for i:=1 to ControlCount - 1 do
    if Controls[i].Name = AName then begin
      Result:= i - 1;
      Exit;
    end;
end;

procedure TFZVirtualPageControl.SetActivePage(APage: TFZVirtualPage);
begin
  if FActivePage = APage then Exit;

  FActivePage:= APage;
  if Assigned(FActivePage) then
    FActivePage.FButton.Click;
  Change;
end;

procedure TFZVirtualPageControl.SetActivePageIndex(APageIndex: Integer);
var APage: TFZVirtualPage;
begin
  if ActivePageIndex = APageIndex then Exit;
  if (APageIndex > PageCount - 1) or (APageIndex < -1) then Exit;

  APage:= GetPage(APageIndex);
  ActivePage:= APage;
end;

procedure TFZVirtualPageControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FActivePage) then begin
    FActivePage:= nil;
    ActivePageIndex:= PageCount - 1;
  end;
end;

procedure TFZVirtualPageControl.RealSetText(const AValue: TCaption);
var c: TBitmap;
begin
  inherited RealSetText(AValue);
  if FMenuPanel <> nil then begin
    FMenuPanel.Caption:= AValue;
    c:= TBitmap.Create;
    try
      c.Canvas.Font:= FMenuPanel.Font;
      MenuWidth:= c.Canvas.TextWidth(AValue) + 24;
    finally
      c.Free;
    end;
  end;
end;

procedure TFZVirtualPageControl.MenuClick(Sender: TObject);
begin
  if Assigned(FOnMenuClick) then FOnMenuClick(Self);
end;

procedure TFZVirtualPageControl.CreateItem;
begin
  if FTitlePanel = nil then begin
    FTitlePanel:= TFZTitlePanel.Create(Self);
    FTitlePanel.ControlStyle:= FTitlePanel.ControlStyle + [csNoDesignVisible];
    FTitlePanel.Parent:= Self;
    FTitlePanel.Align:= alTop;
    TitleHeight:= 22;
  end;
  if FMenuPanel = nil then begin
    FMenuPanel:= TFZVirtualTabMenu.Create(Self);
    FMenuPanel.ControlStyle:= FMenuPanel.ControlStyle + [csNoDesignVisible];
    FMenuPanel.Parent:= FTitlePanel;
    FMenuPanel.Left:= 0;
    FMenuPanel.Align:= alLeft;
    FMenuPanel.BorderSpacing.Right:= 4;
    FMenuPanel.OnClick:= @MenuClick;
    ShowMenu:= True;
    MenuWidth:= 50;
  end;
end;

procedure TFZVirtualPageControl.ArrangeButton;
var i, x:integer;
begin
  if not Assigned(FTitlePanel) then Exit;
  x:= 0;
  for i:= 0 to FTitlePanel.ControlCount - 1 do begin
    if (FTitlePanel.Controls[i].Visible) and (FTitlePanel.Controls[i].Align = alLeft) then begin
      FTitlePanel.Controls[i].Left:= x;
      x:= x + FTitlePanel.Controls[i].Width + FTitlePanel.Controls[i].BorderSpacing.Right;
      if x > Width - 44 then FTitlePanel.Controls[i].Visible:= False;
    end;
  end;
  FTitlePanel.FChildWidth:= x;
end;

constructor TFZVirtualPageControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAppearance.Border.Normal.Color:= clSilver;

  FMenuAppearance:= CreateAppearance;
  FMenuAppearance.Color.Normal:= clSilver;
  FMenuAppearance.Color.Hover:= clSilver;
  FMenuAppearance.Color.HotTrack:= True;

  FTabAppearance:= TFZTabAppearance.Create(Self);
  with FTabAppearance.Active.Border do begin
    HotTrack:= True;
    Normal.Option:= [mboLeft, mboTop, mboRight];
    Normal.Color:= clSilver;
    Normal.Width:= 1;
    Hover.Option:= [mboLeft, mboTop, mboRight];
    Hover.Color:= clSilver;
    Hover.Width:= 2;
  end;
  with FTabAppearance.InActive.Border do begin
    HotTrack:= True;
    Hover.Option:= [mboLeft, mboTop, mboRight];
    Hover.Color:= clSilver;
    Hover.Width:= 1;
  end;

  Height:= 200;
  Width:= 200;
  FShowMenu:= True;
  FShowTabs:= True;
  CreateItem;
end;

destructor TFZVirtualPageControl.Destroy;
begin
  FTabAppearance.Free;
  inherited Destroy;
end;

{ TFZVirtualPage }

procedure TFZVirtualPage.SetButtonWidth(Value: Integer);
begin
  if FButtonWidth = Value then Exit;
  FButtonWidth:= Value;
  if FButton <> nil then
    FButton.Width:= Value;
end;

procedure TFZVirtualPage.RealSetText(const AValue: TCaption);
var c: TBitmap;
begin
  inherited RealSetText(AValue);
  if FButton <> nil then begin
    FButton.Caption:= AValue;
    c:= TBitmap.Create;
    try
      c.Canvas.Font:= FButton.Font;
      ButtonWidth:= c.Canvas.TextWidth(AValue) + 24;
    finally
      c.Free;
    end;
  end;
end;

procedure TFZVirtualPage.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if NewParent = nil then Exit;
  if FButton <> nil then Exit;
  FButton:= TFZVirtualTabButton.Create(Self,
    TFZVirtualPageControl(NewParent).TitlePanel, Self);
  FButton.Align:=alLeft;
  FButton.Visible:= Visible;
  TFZVirtualPageControl(NewParent).ArrangeButton;
end;

procedure TFZVirtualPage.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  if (FButton <> nil) and not FControlChanging then
    FButton.Visible:= Value;
end;

procedure TFZVirtualPage.Paint;
begin
  Appearance.Color.Normal:= TFZVirtualPageControl(Parent).Appearance.Color.Normal;
  Appearance.Border.Normal.Color:= TFZVirtualPageControl(Parent).Appearance.Border.Normal.Color;
  inherited Paint;
end;

procedure TFZVirtualPage.ShowMe;
var i:integer;
begin
  for i:=1 to Parent.ControlCount - 1 do
    TFZVirtualPage(Parent.Controls[i]).HideMe;

  if Align <> alClient then Align:=alClient;
  ControlStyle:= ControlStyle - [csNoDesignVisible];
  FControlChanging:= True;
  Visible:= True;
  FControlChanging:= False;
end;

procedure TFZVirtualPage.HideMe;
begin
  ControlStyle:= ControlStyle + [csNoDesignVisible];
  FControlChanging:= True;
  Visible:= False;
  FControlChanging:= False;
end;

constructor TFZVirtualPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FControlChanging:= True;
  Appearance.Border.Normal.Width:= 1;
  Appearance.Border.HotTrack:= False;
  Appearance.Border.Normal.Option:=[mboTop, mboBottom];
end;

{ TFZVirtualTabButton }

procedure TFZVirtualTabButton.Paint;
begin
  Appearance:= TFZVirtualPageControl(Parent.Parent).TabAppearance.InActive;
  AppearanceDown:= TFZVirtualPageControl(Parent.Parent).TabAppearance.Active;
  inherited Paint;
end;

procedure TFZVirtualTabButton.Click;
begin
  if Page <> nil then Page.ShowMe;
  inherited Click;
end;

constructor TFZVirtualTabButton.Create(TheOwner: TComponent;
  Container: TFZControl; ThePage: TFZVirtualPage);
begin
  inherited Create(TheOwner);
  ControlStyle:= ControlStyle + [csNoDesignSelectable];
  BorderSpacing.Right:= 2;
  Page:= ThePage;
  Parent:= Container;
  GroupIndex:=1;
end;

{ TFZVirtualShell }

procedure TFZVirtualShell.Paint;
var x, y: integer;
begin
  inherited Paint;
  x:= 4;
  y:= 4;
  with Canvas do begin
    StretchDraw(Rect(x, y, x + 20, y + 20), Application.Icon);
    x:= x + 20 + 8;
    y:= y + 18;
    Pen.Color:= Appearance.Border.Normal.Color;
    Line(x, 4, x, y);
    x:= x + 12;
    y:= (Height - TextHeight(Caption)) div 2;
    TextOut(x, y, Caption);
  end;
end;

procedure TFZVirtualShell.Resize;
var l:integer;
begin
  inherited Resize;
  if not Assigned(FCloseButton) then Exit;
  l:= Width - FCloseButton.Width - 1;
  FCloseButton.Top:=0;
  FCloseButton.Left:= l;

  l:= l - FRestoreButton.Width;
  FRestoreButton.Top:= 0;
  FRestoreButton.Left:= l;

  l:= l - FMinButton.Width;
  FMinButton.Top:= 0;
  FMinButton.Left:= l;

  l:= l - FHelpButton.Width;
  FHelpButton.Top:= 0;
  FHelpButton.Left:= l;

  l:= l - FSettingButton.Width;
  FSettingButton.Top:= 0;
  FSettingButton.Left:= l;
end;

procedure TFZVirtualShell.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FX:= X;
  FY:= Y;
end;

procedure TFZVirtualShell.MouseMove(Shift: TShiftState; X, Y: Integer);
var Frm: TForm;
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then begin
    Frm:= TForm(GetTopParent);
    Frm.Left:= Mouse.CursorPos.X - FX;
    Frm.Top:= Mouse.CursorPos.Y - FY;
  end;
end;

procedure TFZVirtualShell.HelpClicked(Sender: TObject);
begin
  if Assigned(FOnHelpClick) then FOnHelpClick(Self);
end;

procedure TFZVirtualShell.SettingClicked(Sender: TObject);
begin
  if Assigned(FOnSettingClick) then FOnSettingClick(Self);
end;

constructor TFZVirtualShell.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FButtonAppearance:= TFZAppearanceEx.Create(Self);
  Align:= alTop;
  Height:= 27;
  with Appearance do begin
    Border.Normal.Option:= [mboBottom];
    Border.Normal.Color:= clSilver;
    Border.Normal.Width:= 1;
  end;
  with ButtonAppearance do begin
    Icon.HotTrack:= True;
    Icon.Normal.Color:= clSilver;
    Icon.Hover.Color:= clSilver;
    Border.HotTrack:= True;
    Border.Hover.Color:= clSilver;
    Border.Hover.Width:=1;
  end;

  FCloseButton:= TFZSysButton.Create(Self);
  FCloseButton.ButtonStyle:= sbsClose;
  FCloseButton.Parent:= Self;

  FRestoreButton:= TFZSysButton.Create(Self);
  FRestoreButton.ButtonStyle:= sbsRestore;
  FRestoreButton.Parent:= Self;

  FMinButton:= TFZSysButton.Create(Self);
  FMinButton.ButtonStyle:= sbsMinimize;
  FMinButton.Parent:= Self;

  FSettingButton:= TFZVirtualButton.Create(Self);
  FSettingButton.Caption:= 'SETTINGS';
  FSettingButton.Width:= 66;
  FSettingButton.Height:= 22;
  FSettingButton.Parent := Self;
  FSettingButton.Appearance.Border.HotTrack:= True;
  FSettingButton.Appearance.Border.Hover.Color:= clSilver;
  FSettingButton.Appearance.Border.Hover.Width:= 1;
  FSettingButton.OnClick:= @SettingClicked;

  FHelpButton:= TFZVirtualButton.Create(Self);
  FHelpButton.Caption:= 'HELP';
  FHelpButton.Width:= 45;
  FHelpButton.Height:= 22;
  FHelpButton.Parent := Self;
  FHelpButton.Appearance.Border.HotTrack:= True;
  FHelpButton.Appearance.Border.Hover.Color:= clSilver;
  FHelpButton.Appearance.Border.Hover.Width:= 1;
  FHelpButton.OnClick:= @HelpClicked;
end;

destructor TFZVirtualShell.Destroy;
begin
  FSettingButton.Free;
  FHelpButton.Free;
  FMinButton.Free;
  FRestoreButton.Free;
  FCloseButton.Free;
  FButtonAppearance.Free;
  inherited Destroy;
end;

{ TFZVirtualStatusBar }

procedure TFZVirtualStatusBar.SetIndicatorCaption(ACaption: TCaption);
begin
  if FIndicatorCaption = ACaption then Exit;
  FIndicatorCaption:= ACaption;
  Invalidate;
end;

procedure TFZVirtualStatusBar.Paint;
var w, h: integer;
begin
  inherited Paint;
  if (MouseState = msHover) and Appearance.Font.HotTrack then
    Canvas.Font:= Appearance.Font.Hover
  else
    Canvas.Font:= Appearance.Font.Normal;
  w:=8;
  h:= (Height - Canvas.TextHeight(Caption)) div 2;
  Canvas.TextOut(w, h, Caption);
  w:= Width - 8 - Canvas.TextWidth(IndicatorCaption);
  Canvas.TextOut(w, h, IndicatorCaption);
end;

constructor TFZVirtualStatusBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIndicatorCaption:= '';
  Align:= alBottom;
  Height:=22;
  Appearance.Border.Normal.Option:= [mboTop];
  Appearance.Border.Normal.Color:= clSilver;
  Appearance.Border.Normal.Width:= 1;
end;

{ TFZVirtualButton }

procedure TFZVirtualButton.SetSpacing(AValue: integer);
begin
  if FSpacing = AValue then Exit;
  FSpacing:= AValue;
  Invalidate;
end;

procedure TFZVirtualButton.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption = AValue then Exit;
  FShowCaption:= AValue;
  Invalidate;
end;

procedure TFZVirtualButton.SetImageStyle(AValue: TFZImageStyle);
begin
  if FImageStyle = AValue then Exit;
  FImageStyle:= AValue;
  Invalidate;
end;

procedure TFZVirtualButton.SetIndent(AValue: Integer);
begin
  if FIndent = AValue then Exit;
  FIndent:= AValue;
  Invalidate;
end;

procedure TFZVirtualButton.SetDown(AValue: Boolean);
var i:integer;
begin
  if FDown = AValue then Exit;
  if FGroupIndex > 0 then begin
    for i:=0 to Parent.ControlCount-1 do
      if (Parent.Controls[i] is TFZVirtualButton) then
        if TFZVirtualButton(Parent.Controls[i]).GroupIndex = FGroupIndex then begin
          TFZVirtualButton(Parent.Controls[i]).FDown:= False;
          Parent.Controls[i].Invalidate;
        end;

    FDown:= AValue;
    Invalidate;
  end;
end;

procedure TFZVirtualButton.Paint;
var w, h, spasi, i, swidth, sheight, j: integer;
    lns: TStrings;
    AprTmp: TFZAppearance;
begin
  if (Parent is TFZVirtualShell) then
    Appearance:= TFZVirtualShell(Parent).ButtonAppearance;
  if Parent is TFZGeneralEdit then begin
    Appearance.Color:= TFZGeneralEdit(Parent).Appearance.Color;
    Appearance.Font:= TFZGeneralEdit(Parent).Appearance.Font;
  end;

  inherited Paint;

  if FDown then AprTmp:= FAppearanceDown else AprTmp:= FAppearance;

  lns:= TStringList.Create;
  if FShowCaption then lns.Text:= Caption else lns.Text:='';
  spasi:= FSpacing;
  if (lns.Text = '') or (FImage.Graphic = nil) then spasi:= 0;
  swidth:=0; sheight:= 0;
  for i:=0 to lns.Count - 1 do begin
    w:= Canvas.TextWidth(lns[i]);
    h:= Canvas.TextHeight(lns[i]);
    if w > swidth then swidth:= w;
    sheight:= sheight + h;
  end;
  if (MouseState = msHover) and (AprTmp.Font.HotTrack) then
    Canvas.Font:= AprTmp.Font.Hover
  else
    Canvas.Font:= AprTmp.Font.Normal;

  if not Enabled then Canvas.Font.Color:= clSilver;

  if FImageStyle = isLeft then begin
    if FIndent <> 0 then
      w:= FIndent
    else
      w:= (ClientWidth - (FImage.Width + spasi + swidth)) div 2;
    h:= (ClientHeight - FImage.Height) div 2;
    if FImage.Graphic <> nil then
      Canvas.Draw(w, h, FImage.Graphic);
    w:= w + FImage.Width + spasi;
    h:= (ClientHeight - sheight) div 2;
    if FShowCaption then
      for i:=0 to lns.Count - 1 do begin
        Canvas.TextOut(w, h, lns[i]);
        h:= h + Canvas.TextHeight(lns[i]);
      end;
  end;

  if FImageStyle = isRight then begin
    if FIndent <> 0 then
      w:= FIndent
    else
      w:= (ClientWidth - (FImage.Width + spasi + swidth)) div 2;
    h:= (ClientHeight - sheight) div 2;
    if FShowCaption then
      for i:=0 to lns.Count - 1 do begin
        j:= (swidth - Canvas.TextWidth(lns[i]));
        Canvas.TextOut(w + j, h, lns[i]);
        h:= h + Canvas.TextHeight(lns[i]);
      end;
    w:= w + swidth + spasi;
    h:= (ClientHeight - FImage.Height) div 2;
    if FImage.Graphic <> nil then
      Canvas.Draw(w, h, FImage.Graphic);
  end;

  if FImageStyle = isTop then begin
    w:= (ClientWidth - FImage.Width) div 2;
    if FIndent <> 0 then
      h:= FIndent
    else
      h:= (ClientHeight - (FImage.Height + spasi + sheight)) div 2;
    if FImage.Graphic <> nil then
      Canvas.Draw(w, h, FImage.Graphic);
    w:= (ClientWidth - swidth) div 2;
    h:= h + FImage.Height + spasi;
    if FShowCaption then
      for i:=0 to lns.Count - 1 do begin
        j:= (swidth - Canvas.TextWidth(lns[i])) div 2;
        Canvas.TextOut(w + j, h, lns[i]);
        h:= h + Canvas.TextHeight(lns[i]);
      end;
  end;

  if FImageStyle = isBottom then begin
    w:= (ClientWidth - swidth) div 2;
    if FIndent <> 0 then
      h:= FIndent
    else
      h:= (ClientHeight - (FImage.Height + spasi + sheight)) div 2;
    if FShowCaption then
      for i:=0 to lns.Count - 1 do begin
        j:= (swidth - Canvas.TextWidth(lns[i])) div 2;
        Canvas.TextOut(w + j, h, lns[i]);
        h:= h + Canvas.TextHeight(lns[i]);
      end;
    w:= (ClientWidth - FImage.Width) div 2;
    h:= h + {sheight +} spasi;
    if FImage.Graphic <> nil then
      Canvas.Draw(w, h, FImage.Graphic);
  end;

  lns.Free;
end;

procedure TFZVirtualButton.Click;
begin
  if FGroupIndex > 0 then
    SetDown(True);
  inherited Click;
end;

procedure TFZVirtualButton.ImageChanged(Sender: TObject);
begin
  Invalidate;
end;

constructor TFZVirtualButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Constraints.MinHeight:= 14;
  Constraints.MinWidth:= 14;
  FImage:= TPicture.Create;
  FImage.OnChange:= @ImageChanged;
  FImageStyle:= isLeft;
  FGroupIndex:= 0;
  FSpacing:= 4;
  FShowCaption:= True;
  FIndent:= 0;
  Height:= 25;
  Width:= 75;
end;

destructor TFZVirtualButton.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

{ TFZDBGeneralEdit }

function TFZDBGeneralEdit.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

function TFZDBGeneralEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TFZDBGeneralEdit.GetField: TField;
begin
  Result:= FDataLink.Field;
end;

function TFZDBGeneralEdit.GetCheckValue: String;
begin
  if Checked then
    Result:= FValueChecked
  else
    Result:= FValueUnChecked;
end;

procedure TFZDBGeneralEdit.SetDataField(const Value: String);
begin
  FDataLink.FieldName := Value;
end;

procedure TFZDBGeneralEdit.SetDataSource(const Value: TDataSource);
begin
  ChangeDataSource(Self, FDataLink, Value);
end;

procedure TFZDBGeneralEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FLookupDataSet) then
    FLookupDataSet:= Nil;
end;

procedure TFZDBGeneralEdit.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.Field) then Exit;
  if FEditStyle = geEdit then begin
    TMaskEdit(FEdit.FComponent).EditMask := FDataLink.Field.EditMask;
    Alignment := FDataLink.Field.Alignment;
  end;

  if FEditStyle = geChechBox then
    inherited SetChecked(FDataLink.Field.AsString = FValueChecked)
  else begin
    if FLookupDataSet = nil then
      Text := FDataLink.Field.AsString
    else begin
      if not FLookupDataSet.Active then FLookupDataSet.Open;
      Text:= VarToStr(FLookupDataSet.Lookup(FKeyFields, Field.AsString, FListFields));
    end;
  end;
end;

procedure TFZDBGeneralEdit.ModifyRecord;
begin
  if FDataLink.Edit then
    FDataLink.Modified
  else
    FDataLink.Reset;
end;

procedure TFZDBGeneralEdit.UpdateData(Sender: TObject);
begin
  if not Assigned(FDataLink.Field) then Exit;
  if FEditStyle = geChechBox then begin
    if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean:=Checked
    else
      FDataLink.Field.AsString:= GetCheckValue;
  end else begin
    if FLookupDataSet = nil then
      FDataLink.Field.AsString := Text
    else
      FDataLink.Field.AsString:= FLookupDataSet.FieldByName(FKeyFields).AsString;
  end;
end;

procedure TFZDBGeneralEdit.CmExit(var Message: TCmExit);
begin
  InternalUpdateRecord;
  inherited;
end;

procedure TFZDBGeneralEdit.InternalUpdateRecord;
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise // re-raise exception
  end;
end;

procedure TFZDBGeneralEdit.DoClearKey;
begin
  if (FEditStyle = geCombo) and Assigned(FDataLink.Field) then begin
    Text:= '';
    FDataLink.Field.Clear;
  end;
end;

procedure TFZDBGeneralEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key in [9, 13, 27] then
    InternalUpdateRecord
  else
    ModifyRecord;
  inherited KeyDown(Key, Shift);
end;

procedure TFZDBGeneralEdit.KeyPress(var Key: char);
var frm: TForm;
    p: TPoint;
    mdlres: TModalResult;
    i: integer;
    s: String;
begin
  if ord(Key) in [9, 13, 27] {(Key = #13) or (Key = #27)} then Exit;

  if (FLookupDataSet = nil) or (FKeyFields = '') or (FListFields = '') or (FLookupFields = '') then begin
    inherited KeyPress(Key);
    InternalUpdateRecord;
    Exit;
  end;
  if FEditStyle = geCombo then begin
    frm:= Tfdbdropdown.Create(Self);
    Tfdbdropdown(frm).Edit1.Height:= ClientHeight;
    Tfdbdropdown(frm).Edit1.Text:= Key;
    Key:= #0;

    frm.Color:= Appearance.Border.Normal.Color;
    Tfdbdropdown(frm).FZPanel1.Color:= Parent.Color;
    frm.Name:='frm'+ Self.Name;
    frm.Caption:= FListFields;
    p:= Self.ClientToScreen(Point(0, 0));
    frm.Width:= Width;
    frm.Height:= 200;
    frm.Top:= p.Y;
    frm.Left:= p.X;

    Tfdbdropdown(frm).Datasource1.DataSet:= FLookupDataSet;
    s:= FLookupFields;
    with Tfdbdropdown(frm).DBGrid1 do begin
      Columns.Clear;
      i:=0;
      while Pos(';', s) > 0 do begin
        Columns.Add;
        Columns[i].FieldName:= Copy(s, 1, Pos(';', s)-1);
        s:= Copy(s, Pos(';', s)+1, Length(s));
        Inc(i);
      end;
      Columns.Add;
      Columns[i].FieldName:= s;
    end;

    FLookupDataSet.First;
    mdlres:= frm.ShowModal;
    if mdlres = mrOK then begin
      if FLookupDataSet.FieldByName(FKeyFields).AsString = '' then begin
        if Assigned(FOnEditReference) then FOnEditReference(Self);
      end else begin
        ModifyRecord;
        FDataLink.Field.AsString:= FLookupDataSet.FieldByName(FKeyFields).AsString;
      end;
    end;
    FLookupDataSet.Filtered:= False;
    FLookupDataSet.OnFilterRecord:= Nil;
    frm.Free;
    SetFocus;
  end;
end;

procedure TFZDBGeneralEdit.SetChecked(AValue: Boolean);
begin
  ModifyRecord;
  inherited SetChecked(AValue);
  InternalUpdateRecord;
end;

procedure TFZDBGeneralEdit.SetListFields(AValue: String);
var s: String;
begin
  if FListFields = AValue then Exit;
  s:= StringReplace(AValue, ' ', ';', [rfReplaceAll]);
  s:= StringReplace(s, ',', ';', [rfReplaceAll]);
  FListFields:= s;
  if FLookupFields = '' then
    FLookupFields:= s;
end;

procedure TFZDBGeneralEdit.SetLookupFields(AValue: String);
var s: String;
begin
  if FLookupFields <> AValue then Exit;
  s:= StringReplace(AValue, ' ', ';', [rfReplaceAll]);
  s:= StringReplace(s, ',', ';', [rfReplaceAll]);
  FLookupFields:= s;
end;

constructor TFZDBGeneralEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FValueChecked:= 'Y';
  FValueUnChecked:= 'N';
end;

destructor TFZDBGeneralEdit.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

{ TFZSysButton }

procedure TFZSysButton.SetButtonStyle(AValue: TFZSysButtonStyle);
begin
  if FButtonStyle = AValue then Exit;
  FButtonStyle:= AValue;
  Invalidate;
end;

procedure TFZSysButton.Paint;
var x, y, w, z: integer;
  cl: TColor;
begin
  if (Parent is TFZVirtualShell) then
    Appearance:= TFZVirtualShell(Parent).ButtonAppearance
  else
    Appearance.Icon := TFZControlEx(Parent).Appearance.Icon;

  if (Parent is TFZGeneralEdit) then
    Appearance.Color:= TFZGeneralEdit(Parent).Appearance.Color;

  inherited Paint;

  x:= (Width - 10) div 2;
  y:= (Height - 10) div 2;
  with Canvas do begin
    w:= Pen.Width;
    cl:= Pen.Color;

    if (MouseState = msHover) and Appearance.Icon.HotTrack then begin
      Pen.Color:= Appearance.Icon.Hover.Color;
      Pen.Width:= Appearance.Icon.Hover.Width;
      z:= Appearance.Icon.Hover.Width;
    end else begin
      Pen.Color:= Appearance.Icon.Normal.Color;
      Pen.Width:= Appearance.Icon.Normal.Width;
      z:= Appearance.Icon.Normal.Width;
    end;

    if (z = 0) or (Pen.Color = clNone) then begin
      Pen.Width:= w;
      Pen.Color:= cl;
      Exit;
    end;

    case FButtonStyle of
      sbsMinimize:
        Rectangle(x, y + 3, x + 10, y + 7);
      sbsMaximize:
        Rectangle(x, y,     x + 10, y + 10);
      sbsRestore:
        begin
          Rectangle(x + 2, y, x + 10, y + 8);
          Rectangle(x, y + 2, x + 8, y + 10);
        end;
      sbsClose:
        begin
          x:= x + 1;
          y:= y + 1;
          MoveTo(x, y + 2);

          LineTo(x + 2, y);
          LineTo(x + 4, y + 2);
          LineTo(x + 6, y);
          LineTo(x + 8, y + 2);

          LineTo(x + 6, y + 4);
          LineTo(x + 8, y + 6);

          LineTo(x + 6, y + 8);
          LineTo(x + 4, y + 6);
          LineTo(x + 2, y + 8);
          LineTo(x, y + 6);

          LineTo(x + 2, y + 4);
          LineTo(x, y + 2);
        end;
      sbsDropDown:
        begin
          x:= x + 1;
          y:= y + 3;
          MoveTo(x, y);
          LineTo(x + 8, y);
          LineTo(x + 4, y + 4);
          LineTo(x, y);
        end;
      sbsEllips:
        begin
          x:= (Width - TextWidth('...')) div 2;
          y:= (Height - TextHeight('...')) div 2;
          TextOut(x, y, '...');
        end;
      sbsUnCheck, sbsCheck, sbsPartialCheck:
        begin
          if Align = alLeft then x:= 0 else x:= 4;
          y:= (Height - 14) div 2;
          Rectangle(x, y, x + 14, y + 14);
          if FButtonStyle <> sbsUnCheck then begin
            if FButtonStyle = sbsPartialCheck then Pen.Color:= clSilver;
            Pen.Width:= 2;
            MoveTo(x + 2, y + 6);
            LineTo(x + 5, y + 10);
            LineTo(x + 12, y + 2);
          end;
        end;
    end;
    Pen.Width:= w;
    Pen.Color:= cl;
  end;

end;

procedure TFZSysButton.Click;
var Frm: TForm;
    Rct: TRect;
begin
  inherited Click;
  Frm:= TForm(GetTopParent);
  case FButtonStyle of
    sbsClose:
      Frm.Close;
    sbsMinimize:
      begin
        if Frm = Application.MainForm then
          Application.Minimize
        else begin
          if Frm.Parent <> nil then begin

          end;
        end;
      end;
    sbsMaximize:
      begin
        if Parent = nil then
          Rct:= Screen.WorkAreaRect
        else
          Parent.ClientRect;
      end;
    sbsRestore: Frm.WindowState:= wsNormal;
  end;
end;

constructor TFZSysButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Height:= 22;
  Width:= 22;
end;


{ TFZGeneralEdit }

procedure TFZGeneralEdit.SetEditStyle(AEditStyle: TFZGeneralEditStyle);
begin
  if Assigned(FEdit) then FEdit.Free;
  FEditStyle:= AEditStyle;
  case AEditStyle of
    geButton:
      begin
        FEdit:= TFZVirtualEdit.Create(TFZVirtualButton, Self, TFZSysButton);
        TFZSysButton(FEdit.FButton).ButtonStyle:= sbsDropDown;
        FEdit.FComponent.OnClick:=@InternalClick;
        FEdit.FButton.OnClick:=@ButtonClick;
      end;
    geEdit:
      begin
        FEdit:= TFZVirtualEdit.Create(TMaskEdit, Self);
        TMaskEdit(FEdit.FComponent).BorderStyle:= bsNone;
      end;
    geMemo:
      begin
        FEdit:= TFZVirtualEdit.Create(TCustomMemo, Self);
        TCustomMemo(FEdit.FComponent).BorderStyle:= bsNone;
      end;
    geCombo:
      begin
        FEdit:= TFZVirtualEdit.Create(TMaskEdit, Self, TFZSysButton);
        TMaskEdit(FEdit.FComponent).BorderStyle:= bsNone;
        TFZSysButton(FEdit.FButton).ButtonStyle:= sbsDropDown;
        FEdit.FButton.OnClick:=@ButtonClick;
      end;
    geChechBox:
      begin
        FEdit:= TFZVirtualEdit.Create(TLabel, Self, TFZSysButton, False);
        FEdit.FButton.Width:= 18;
        TLabel(FEdit.FComponent).Layout:= tlCenter;
        FChecked:= False;
        TFZSysButton(FEdit.FButton).ButtonStyle:= sbsUnCheck;
        Appearance.Border.Normal.Visible:= False;
        FEdit.FButton.OnClick:=@ButtonClick;
        FEdit.FComponent.OnClick:=@ButtonClick;
      end;
  end;
  FEdit.FComponent.OnMouseEnter:= @TheMouseEnter;
  FEdit.FComponent.OnMouseLeave:= @TheMouseLeave;
  if FEditStyle in [geButton, geChechBox] then begin
    FEdit.FComponent.OnMouseDown:=@TheMouseDown;
    FEdit.FComponent.OnMouseUp:=@TheMouseUp;
  end;
end;

procedure TFZGeneralEdit.TheMouseEnter(Sender: TObject);
begin
  MouseEnter;
  if FEditStyle = geChechBox then
    TFZSysButton(FEdit.FButton).MouseEnter;
end;

procedure TFZGeneralEdit.TheMouseLeave(Sender: TObject);
begin
  MouseLeave;
  if FEditStyle = geChechBox then
    TFZSysButton(FEdit.FButton).MouseLeave;
end;

procedure TFZGeneralEdit.TheMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDown(Button, Shift, X, Y);
end;

procedure TFZGeneralEdit.TheMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseUp(Button, Shift, X, Y);
end;

function TFZGeneralEdit.GetLines: TStrings;
begin
  if FEditStyle = geMemo then
    Result:= TCustomMemo(FEdit.FComponent).Lines
  else
    Result:= nil;
end;

procedure TFZGeneralEdit.SetLines(AValue: TStrings);
begin
  if FEditStyle <> geMemo then Exit;
  if TCustomMemo(FEdit.FComponent).Lines <> AValue then
    TCustomMemo(FEdit.FComponent).Lines.Assign(AValue);
end;

function TFZGeneralEdit.RealGetText: TCaption;
begin
  case FEditStyle of
    geMemo: Result:= TCustomMemo(FEdit.FComponent).Lines.Text;
  else
    Result:= FEdit.FComponent.Caption;
  end;
end;

procedure TFZGeneralEdit.RealSetText(const AValue: TCaption);
begin
  if RealGetText = AValue then Exit;
  case FEditStyle of
    geMemo: TCustomMemo(FEdit.FComponent).Lines.Text:= AValue;
  else
    FEdit.FComponent.Caption:= AValue;
  end;
end;

function TFZGeneralEdit.GetAlignment: TAlignment;
begin
  case FEditStyle of
    geEdit: Result:= TMaskEdit(FEdit.FComponent).Alignment;
    geMemo: Result:= TCustomMemo(FEdit.FComponent).Alignment;
    geChechBox: Result:= TLabel(FEdit.FComponent).Alignment;
  else
    Result:= taLeftJustify;
  end;
end;

procedure TFZGeneralEdit.SetAlignment(AValue: TAlignment);
begin
  case FEditStyle of
    geEdit: TMaskEdit(FEdit.FComponent).Alignment:= AValue;
    geMemo: TCustomMemo(FEdit.FComponent).Alignment:= AValue;
    geChechBox:
      begin
        if AValue = taCenter then Exit;
        TLabel(FEdit.FComponent).Alignment:= AValue;
        if AValue = taRightJustify then
          FEdit.FButton.Align:= alRight
        else
          FEdit.FButton.Align:= alLeft;
      end;
  end;
end;

function TFZGeneralEdit.GetPassWordChar: Char;
begin
  if FEditStyle = geEdit then
    Result:= TMaskEdit(FEdit.FComponent).PasswordChar
  else
    Result:= #0;
end;

procedure TFZGeneralEdit.SetPassWordChar(AChar: Char);
begin
  if FEditStyle = geEdit then
    TMaskEdit(FEdit.FComponent).PasswordChar:= AChar;
end;

function TFZGeneralEdit.GetReadOnly: Boolean;
begin
  case FEditStyle of
    geEdit: Result:= TMaskEdit(FEdit.FComponent).ReadOnly;
    geMemo: Result:= TCustomMemo(FEdit.FComponent).ReadOnly;
  else
    Result:= False;
  end;
end;

procedure TFZGeneralEdit.SetReadOnly(AValue: Boolean);
begin
  case FEditStyle of
    geEdit: TMaskEdit(FEdit.FComponent).ReadOnly:= AValue;
    geMemo: TCustomMemo(FEdit.FComponent).ReadOnly:= AValue;
  end;
end;

procedure TFZGeneralEdit.SetItems(AValue: TStrings);
begin
  if FEditStyle = geCombo then begin
    FItems.Assign(AValue);
  end;
end;

function TFZGeneralEdit.GetItemIndex: Integer;
begin
  Result:= FItems.IndexOf(Text);
end;

procedure TFZGeneralEdit.SetItemIndex(AValue: Integer);
begin
  if ItemIndex = AValue then Exit;
  if AValue >=0 then
    Text:= Items[AValue]
  else
    Text:= '';
end;

procedure TFZGeneralEdit.Paint;
begin
  inherited Paint;
  if (FMouseState = msHover) and Appearance.Font.HotTrack then
    FEdit.FComponent.Font:= Appearance.Font.Hover
  else
    FEdit.FComponent.Font:= Appearance.Font.Normal;
end;

procedure TFZGeneralEdit.SetChecked(AValue: Boolean);
begin
  if FChecked = AValue then Exit;
  FChecked:= AValue;
  if FChecked then
    TFZSysButton(FEdit.FButton).ButtonStyle:= sbsCheck
  else
    TFZSysButton(FEdit.FButton).ButtonStyle:= sbsUnCheck;
  Change;
end;

procedure TFZGeneralEdit.KeyPress(var Key: char);
var frm: TForm;
    p: TPoint;
begin
  inherited KeyPress(Key);

  if ord(Key) in [9, 13, 27] {(Key = #13) or (Key = #27)} then Exit;
  if FItems.Count = 0 then Exit;
  if FEditStyle = geCombo then begin
    frm:= Tfdropdown.Create(Self);
    Tfdropdown(frm).Edit1.Height:= ClientHeight;
    Tfdropdown(frm).ListBox1.Items:= FItems;
    if Key = #0 then
      Tfdropdown(frm).Edit1.Text:= Text
    else begin
      Tfdropdown(frm).Edit1.Text:= Key;
      Key:= #0;
    end;
    frm.Color:= Appearance.Border.Normal.Color;
    frm.Name:='frm'+ Self.Name;
    p:= Self.ClientToScreen(Point(0, 0));
    frm.Width:= Width;
    frm.Height:= 200;
    frm.Top:= p.Y;
    frm.Left:= p.X;
    frm.ShowModal;
    frm.Free;
    SetFocus;
  end;
end;

procedure TFZGeneralEdit.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FEdit.FComponent.Enabled:= Value;
end;

procedure TFZGeneralEdit.ButtonClick(Sender: TObject);
var k: Char;
begin
  case FEditStyle of
    geChechBox: Checked:= not Checked;
    geCombo:
      begin
        k:= #0;
        KeyPress(k);
      end;
  end;
end;

procedure TFZGeneralEdit.InternalClick(Sender: TObject);
begin
  Click;
end;

constructor TFZGeneralEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Constraints.MinHeight:= 22;
  Constraints.MinWidth:= 22;
  FItems:= TStringList.Create;

  FAppearance.Border.Normal.Color:= clSilver;
  FAppearance.Border.Normal.Width:= 1;
end;

destructor TFZGeneralEdit.Destroy;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TFZGeneralEdit.SetFocus;
begin
  FEdit.FComponent.SetFocus;
end;


{ TFZVirtualEdit }

procedure TFZVirtualEdit.ControlChange(Sender: TObject);
begin
  TFZControl(FControl).Change;
end;

procedure TFZVirtualEdit.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 46 then
    FControl.DoClearKey
  else
    FControl.DoTheKeyDown(Key, Shift);
end;

procedure TFZVirtualEdit.ControlKeyPress(Sender: TObject; var Key: Char);
begin
  FControl.DoTheKeyPress(Key);
end;

constructor TFZVirtualEdit.Create(AComponent: TComponentClass;
  AOwner: TFZControl; AButton: TComponentClass; BtnRight: Boolean);
begin
  FControl:= AOwner;
  if AButton <> nil then begin
    FButton:= TMyWinControl(AButton.Create(FControl));
    FButton.Parent:= FControl;
    if BtnRight then
      FButton.Align:= alRight
    else
      FButton.Align:= alLeft;
  end;
  FComponent:= TMyWinControl(AComponent.Create(FControl));
  FComponent.Parent:= FControl;
  FComponent.AutoSize:= False;
  FComponent.Align:= alClient;
  if AComponent = TMaskEdit then begin
    TMaskEdit(FComponent).OnChange:=@ControlChange;
    TMaskEdit(FComponent).OnKeyDown:=@ControlKeyDown;
    TMaskEdit(FComponent).OnKeyPress:=@ControlKeyPress;
  end;
  if AComponent = TCustomMemo then TCustomMemo(FComponent).OnChange:=@ControlChange;
end;

destructor TFZVirtualEdit.Destroy;
begin
  FComponent.Free;
  inherited Destroy;
end;


{ TFZControl }

procedure TFZControl.RealSetText(const AValue: TCaption);
begin
  inherited RealSetText(AValue);
  Invalidate;
end;

procedure TFZControl.SetDown(AValue: Boolean);
begin
  if FDown=AValue then Exit;
  FDown:=AValue;
  Invalidate;
end;

function TFZControl.CreateAppearance: TFZAppearance;
begin
  Result:= TFZAppearance.Create(Self);
end;

procedure TFZControl.PaintBorder;
var w, z: integer;
    c: TColor;
    AprTmp: TFZAppearance;
    BorderVisible: Boolean;
    Option: TFZBorderOptions;
begin
  with Canvas do begin
    c:= Pen.Color;
    w:= Pen.Width;
    if FDown then AprTmp:= FAppearanceDown else AprTmp:= FAppearance;

    with AprTmp.Border do begin
      if (FMouseState = msHover) and HotTrack then begin
        BorderVisible:= Hover.Visible;
        Option:= Hover.Option;
        z:= Hover.Width;
        Pen.Color:= Hover.Color;
        Pen.Width:= Hover.Width * 2;
      end else begin
        BorderVisible:= Normal.Visible;
        Option:= Normal.Option;
        z:= Normal.Width;
        Pen.Color:= Normal.Color;
        Pen.Width:= Normal.Width * 2;
      end;
    end;

    if BorderVisible and (z > 0) and (Pen.Color <> clNone) then begin
      if mboTop in Option then Line(Width, 0, 0, 0);
      if mboLeft in Option then Line(0, 0, 0, Height);
      if mboBottom in Option then Line(0, Height, Width, Height);
      if mboRight in Option then Line(Width, Height, Width, 0);
    end;

    Pen.Color:= c;
    Pen.Width:= w;
  end;
end;

procedure TFZControl.Paint;
var AprTmp: TFZAppearance;
begin
  if FDown then AprTmp:= FAppearanceDown else AprTmp:= FAppearance;
  if (FMouseState = msHover) and AprTmp.Color.HotTrack then
    Canvas.Brush.Color:= AprTmp.Color.Hover
  else
    Canvas.Brush.Color:= AprTmp.Color.Normal;

  Canvas.FillRect(0, 0, Width, Height);
  PaintBorder;
  inherited Paint;
end;

procedure TFZControl.AdjustClientRect(var ARect: TRect);
var bvl: Integer;
begin
  inherited AdjustClientRect(ARect);
  bvl := BorderWidth;
  with Appearance.Border do begin
    if Normal.Visible and Hover.Visible and HotTrack then
      inc(bvl, Max(Normal.Width, Hover.Width))
    else begin
      if Normal.Visible then Inc(bvl, Normal.Width);
      if (Hover.Visible) and (HotTrack) then Inc(bvl, Hover.Width);
    end;
  end;
  InflateRect(ARect, -bvl, -bvl);
end;

procedure TFZControl.MouseEnter;
begin
  if csDesigning in ComponentState then
    exit;

  if Enabled and (FMouseState <> msHover) then begin
    FMouseState := msHover;
    Invalidate;
  end;

  inherited MouseEnter;
end;

procedure TFZControl.MouseLeave;
begin
  if csDesigning in ComponentState then
    exit;

  if FMouseState <> msNone then begin
    FMouseState := msNone;
    Invalidate;
  end;

  inherited MouseLeave;
end;

procedure TFZControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if Enabled and (FMouseState <> msClicked) then
  begin
    FMouseState := msClicked;
    Invalidate;
  end;
end;

procedure TFZControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if Enabled and (FMouseState = msClicked) then
  begin
    FMouseState := msHover;
    Invalidate;
  end;
end;

procedure TFZControl.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TFZControl.DoTheKeyDown(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TFZControl.DoTheKeyPress(var Key: Char);
begin
  KeyPress(Key);
end;

procedure TFZControl.DoClearKey;
begin
  //do nothing
end;

constructor TFZControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
    csNoFocus, csAutoSize0x0] - [csOpaque];

  FAppearance    := CreateAppearance;
  FAppearanceDown:= CreateAppearance;
  FDown:= False;

  FMouseState:= msNone;
  Width:=170;
  Height:=50;
end;

destructor TFZControl.Destroy;
begin
  FAppearance:= nil;
  FAppearanceDown:= nil;
  FAppearance.Free;
  FAppearanceDown.Free;
  inherited Destroy;
end;


{ TFZBorder }

procedure TFZBorder.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  FControl.Invalidate;
end;

constructor TFZBorder.Create(AControl: TCustomControl);
begin
  inherited Create;
  FControl:= AControl;
  FNormal:= TFZBorderItem.Create(AControl);
  FHover:= TFZBorderItem.Create(AControl);
  FHotTrack:= False;
  FColor:= clDefault;
end;

end.

