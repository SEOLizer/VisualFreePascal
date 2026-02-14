{$mode objfpc}{$H+}
unit ws_intf;

interface

uses
  rtl_sys, events, classes, controls;

type
  // Forward-Deklarationen
  TWidgetSet = class;
  TWSControl = class;
  TWSWinControl = class;
  
  // Control-Klassen für Factory
  TControlClass = class of TControl;
  TWSControlClass = class of TWSControl;
  
  // WS-Control Registry Entry
  TWSControlEntry = record
    ControlClass: TControlClass;
    WSClass: TWSControlClass;
  end;
  
  // Basis WidgetSet-Control
  TWSControl = class(TObject)
  protected
    FControl: TControl;
  public
    constructor Create(AControl: TControl); virtual;
    property Control: TControl read FControl;
  end;
  
  // WidgetSet-WinControl mit Handle
  TWSWinControl = class(TWSControl)
  public
    procedure CreateHandle; virtual; abstract;
    procedure DestroyHandle; virtual; abstract;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual; abstract;
    procedure SetVisible(AVisible: Boolean); virtual; abstract;
    procedure SetEnabled(AEnabled: Boolean); virtual; abstract;
  end;
  
  // WidgetSet-Button
  TWSButton = class(TWSWinControl)
  public
    procedure SetCaption(const ACaption: string); virtual; abstract;
  end;
  
  // WidgetSet-Form
  TWSForm = class(TWSWinControl)
  public
    procedure SetCaption(const ACaption: string); virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Hide; virtual; abstract;
  end;
  
  // WidgetSet-Basis (Factory/Registry)
  TWidgetSet = class(TObject)
  private
    FWSControlEntries: array of TWSControlEntry;
    FWSControlCount: Integer;
  public
    constructor Create; virtual;
    
    // Registry für Control -> WSControl Mapping
    procedure RegisterWSComponent(AControlClass: TControlClass; AWSClass: TWSControlClass);
    function GetWSClass(AControlClass: TControlClass): TWSControlClass;
    function CreateWSControl(AControl: TControl): TWSControl;
    
    // Platform-Lifecycle
    procedure Initialize; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure ProcessMessages; virtual; abstract;
    procedure Terminate; virtual; abstract;
    
    // Direkte Control-Methoden (vereinfacht für PoC)
    procedure CreateControlHandle(AControl: TWinControl); virtual;
    procedure DestroyControlHandle(AControl: TWinControl); virtual;
    procedure SetControlBounds(AControl: TWinControl; ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure SetControlVisible(AControl: TWinControl; AVisible: Boolean); virtual;
    procedure SetControlEnabled(AControl: TWinControl; AEnabled: Boolean); virtual;
    
    // Form-spezifische Methoden
    procedure SetFormCaption(AForm: TObject{TForm}; const ACaption: string); virtual;
    procedure ShowForm(AForm: TObject{TForm}); virtual;
    procedure HideForm(AForm: TObject{TForm}); virtual;
    
    // Button-spezifische Methoden
    procedure SetButtonCaption(AButton: TObject{TButton}; const ACaption: string); virtual;
  end;

implementation

// TWSControl Implementation
constructor TWSControl.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
end;

// TWidgetSet Implementation
constructor TWidgetSet.Create;
begin
  inherited Create;
  SetLength(FWSControlEntries, 0);
  FWSControlCount := 0;
end;

procedure TWidgetSet.RegisterWSComponent(AControlClass: TControlClass; AWSClass: TWSControlClass);
begin
  SetLength(FWSControlEntries, FWSControlCount + 1);
  FWSControlEntries[FWSControlCount].ControlClass := AControlClass;
  FWSControlEntries[FWSControlCount].WSClass := AWSClass;
  Inc(FWSControlCount);
end;

function TWidgetSet.GetWSClass(AControlClass: TControlClass): TWSControlClass;
var
  i: Integer;
  CurrentClass: TClass;
begin
  // Suche exakte Klasse oder Parent-Klassen
  CurrentClass := AControlClass;
  
  while Assigned(CurrentClass) do
  begin
    for i := 0 to FWSControlCount - 1 do
    begin
      if FWSControlEntries[i].ControlClass = CurrentClass then
      begin
        Result := FWSControlEntries[i].WSClass;
        Exit;
      end;
    end;
    
    CurrentClass := CurrentClass.ClassParent;
  end;
  
  Result := nil;
end;

// Globale WidgetSet-Instanz
var
  WidgetSet: TWidgetSet = nil;

function TWidgetSet.CreateWSControl(AControl: TControl): TWSControl;
var
  WSClass: TWSControlClass;
begin
  WSClass := GetWSClass(TControlClass(AControl.ClassType));
  if Assigned(WSClass) then
    Result := WSClass.Create(AControl)
  else
    Result := nil;
end;

// Basis-Implementierung der direkten Control-Methoden
procedure TWidgetSet.CreateControlHandle(AControl: TWinControl);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.DestroyControlHandle(AControl: TWinControl);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.SetControlBounds(AControl: TWinControl; ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.SetControlVisible(AControl: TWinControl; AVisible: Boolean);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.SetControlEnabled(AControl: TWinControl; AEnabled: Boolean);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.SetFormCaption(AForm: TObject; const ACaption: string);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.ShowForm(AForm: TObject);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.HideForm(AForm: TObject);
begin
  // Überschrieben in Platform-WidgetSet
end;

procedure TWidgetSet.SetButtonCaption(AButton: TObject; const ACaption: string);
begin
  // Überschrieben in Platform-WidgetSet
end;

end.