{$mode objfpc}{$H+}
unit stdctrls;

interface

uses
  rtl_sys, events, classes, controls;

type
  // Forward declaration
  TWidgetSet = class(TObject)
  public
    procedure SetButtonCaption(AButton: TObject; const ACaption: string); virtual; abstract;
  end;

var
  WidgetSet: TWidgetSet = nil;

type
  // Button-Control
  TButton = class(TWinControl)
  private
    FCaption: TCaption;
  protected
    procedure SetCaption(const AValue: TCaption); virtual;
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    
    property Caption: TCaption read FCaption write SetCaption;
  end;

implementation

// TButton Implementation
constructor TButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Button';
  
  // Standard-Größe für Buttons
  Width := 75;
  Height := 25;
end;

procedure TButton.SetCaption(const AValue: TCaption);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    
    // Caption wird direkt über GTK4WidgetSet gesetzt
    // if Assigned(WidgetSet) and Assigned(Handle) then
    //   WidgetSet.SetButtonCaption(Self, AValue);
  end;
end;

procedure TButton.CreateHandle;
begin
  if not Assigned(Handle) then
  begin
    inherited CreateHandle;
  end;
end;

end.