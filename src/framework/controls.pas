{$mode objfpc}{$H+}
unit controls;

interface

uses
  rtl_sys, events, classes;

type
  // Forward-Deklarationen
  TControl = class;
  TWinControl = class;
  
  // Control-Liste
  TControlList = class(TComponentList)
  public
    function GetControl(Index: Integer): TControl;
    property Controls[Index: Integer]: TControl read GetControl;
  end;
  
  // Basis-Control ohne Handle
  TControl = class(TComponent)
  private
    FLeft, FTop, FWidth, FHeight: Integer;
    FVisible, FEnabled: Boolean;
    FParent: TWinControl;
    FOnClick: TNotifyEvent;
  protected
    procedure SetLeft(AValue: Integer); virtual;
    procedure SetTop(AValue: Integer); virtual; 
    procedure SetWidth(AValue: Integer); virtual;
    procedure SetHeight(AValue: Integer); virtual;
    procedure SetVisible(AValue: Boolean); virtual;
    procedure SetEnabled(AValue: Boolean); virtual;
    procedure SetParent(AValue: TWinControl); virtual;
    procedure DoOnClick; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    
    procedure Click; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Parent: TWinControl read FParent write SetParent;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;
  
  // Control mit Handle (Platform Widget)
  TWinControl = class(TControl)
  public
    FHandle: Pointer; // Public für WidgetSet-Zugriff
  private
    FControls: TControlList;
    function GetControl(Index: Integer): TControl;
    function GetControlCount: Integer;
  protected
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure SetVisible(AValue: Boolean); override;
    procedure SetEnabled(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure InsertControl(AControl: TControl);
    procedure RemoveControl(AControl: TControl);
    
    property Handle: Pointer read FHandle;
    property Controls[Index: Integer]: TControl read GetControl;
    property ControlCount: Integer read GetControlCount;
  end;

// Globale Referenz auf WidgetSet wird in ws_intf definiert

implementation

// TControlList Implementation
function TControlList.GetControl(Index: Integer): TControl;
begin
  Result := TControl(inherited Items[Index]);
end;

// TControl Implementation
constructor TControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLeft := 0;
  FTop := 0;
  FWidth := 75;
  FHeight := 25;
  FVisible := True;
  FEnabled := True;
  FParent := nil;
  FOnClick := nil;
end;

procedure TControl.SetLeft(AValue: Integer);
begin
  if FLeft <> AValue then
  begin
    FLeft := AValue;
    SetBounds(FLeft, FTop, FWidth, FHeight);
  end;
end;

procedure TControl.SetTop(AValue: Integer);
begin
  if FTop <> AValue then
  begin
    FTop := AValue;
    SetBounds(FLeft, FTop, FWidth, FHeight);
  end;
end;

procedure TControl.SetWidth(AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    SetBounds(FLeft, FTop, FWidth, FHeight);
  end;
end;

procedure TControl.SetHeight(AValue: Integer);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    SetBounds(FLeft, FTop, FWidth, FHeight);
  end;
end;

procedure TControl.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
  // Subklassen überschreiben dies für Platform-Updates
end;

procedure TControl.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
  // Subklassen überschreiben dies für Platform-Updates
end;

procedure TControl.SetParent(AValue: TWinControl);
begin
  if FParent <> AValue then
  begin
    if Assigned(FParent) then
      FParent.RemoveControl(Self);
      
    FParent := AValue;
    
    if Assigned(FParent) then
      FParent.InsertControl(Self);
  end;
end;

procedure TControl.DoOnClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TControl.Click;
begin
  DoOnClick;
end;

procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
  // Basis-Implementation - Subklassen delegieren an WidgetSet
end;

// TWinControl Implementation
constructor TWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := nil;
  FControls := TControlList.Create;
end;

destructor TWinControl.Destroy;
var
  i: Integer;
begin
  // Entferne alle Child-Controls
  for i := FControls.Count - 1 downto 0 do
    FControls.Controls[i].Parent := nil;
    
  FControls.Free;
  
  DestroyHandle;
  inherited Destroy;
end;

function TWinControl.GetControl(Index: Integer): TControl;
begin
  Result := FControls.Controls[Index];
end;

function TWinControl.GetControlCount: Integer;
begin
  Result := FControls.Count;
end;

procedure TWinControl.CreateHandle;
begin
  // WidgetSet-Aufruf wird in Subklassen implementiert
end;

procedure TWinControl.DestroyHandle;
begin
  // WidgetSet-Aufruf wird in Subklassen implementiert
  FHandle := nil;
end;

procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  
  // WidgetSet-Aufruf wird in Subklassen implementiert
end;

procedure TWinControl.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
  
  // WidgetSet-Aufruf wird in Subklassen implementiert
end;

procedure TWinControl.SetEnabled(AValue: Boolean);
begin
  inherited SetEnabled(AValue);
  
  // WidgetSet-Aufruf wird in Subklassen implementiert
end;

procedure TWinControl.InsertControl(AControl: TControl);
begin
  if not Assigned(AControl) then Exit;
  if FControls.IndexOf(AControl) >= 0 then Exit;
  
  FControls.Add(AControl);
  AControl.FParent := Self;
end;

procedure TWinControl.RemoveControl(AControl: TControl);
var
  Index: Integer;
begin
  Index := FControls.IndexOf(AControl);
  if Index >= 0 then
  begin
    AControl.FParent := nil;
    FControls.Delete(Index);
  end;
end;

end.