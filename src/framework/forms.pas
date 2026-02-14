{$mode objfpc}{$H+}
unit forms;

interface

uses
  rtl_sys, events, classes, controls;

type
  // Forward declaration
  TWidgetSet = class(TObject)
  public
    procedure Initialize; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure ProcessMessages; virtual; abstract;
    procedure SetFormCaption(AForm: TObject; const ACaption: string); virtual; abstract;
    procedure ShowForm(AForm: TObject); virtual; abstract;
    procedure HideForm(AForm: TObject); virtual; abstract;
    procedure Terminate; virtual; abstract;
  end;

var
  WidgetSet: TWidgetSet = nil;

type
  // Forward-Deklarationen
  TApplication = class;
  TForm = class;
  
  // Anwendung (Singleton)
  TApplication = class(TComponent)
  private
    FMainForm: TForm;
    FTerminated: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    
    procedure Initialize;
    procedure Run;
    procedure Terminate;
    procedure ProcessMessages;
    function CreateForm(InstanceClass: TComponentClass; var Reference): TForm;
    
    property MainForm: TForm read FMainForm write FMainForm;
    property Terminated: Boolean read FTerminated;
  end;
  
  // Form-Basis
  TForm = class(TWinControl)
  private
    FCaption: TCaption;
  protected
    procedure SetCaption(const AValue: TCaption); virtual;
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    
    procedure Show;
    procedure Hide;
    procedure Close;
    
    property Caption: TCaption read FCaption write SetCaption;
  end;

// Globale Application-Instanz
var
  Application: TApplication = nil;

implementation

// TApplication Implementation
constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainForm := nil;
  FTerminated := False;
end;

procedure TApplication.Initialize;
begin
  WriteLn('TApplication.Initialize aufgerufen');
  if Assigned(WidgetSet) then
  begin
    WriteLn('Rufe WidgetSet.Initialize auf');
    WidgetSet.Initialize;
  end
  else
  begin
    WriteLn('WARNUNG: Kein WidgetSet verfügbar für Initialize');
  end;
end;

procedure TApplication.Run;
begin
  if Assigned(WidgetSet) then
  begin
    WriteLn('TApplication.Run: Starte WidgetSet.Run');
    WidgetSet.Run;
  end
  else
  begin
    WriteLn('TApplication.Run: Kein WidgetSet verfügbar');
  end;
end;

procedure TApplication.Terminate;
begin
  FTerminated := True;
  WriteLn('Application.Terminate aufgerufen');
  
  if Assigned(WidgetSet) then
  begin
    WriteLn('Rufe WidgetSet.Terminate auf');
    WidgetSet.Terminate;
  end;
end;

procedure TApplication.ProcessMessages;
begin
  // ProcessMessages wird direkt über GTK4WidgetSet aufgerufen
  // if Assigned(WidgetSet) then
  //   WidgetSet.ProcessMessages;
end;

function TApplication.CreateForm(InstanceClass: TComponentClass; var Reference): TForm;
var
  Instance: TComponent;
begin
  Instance := InstanceClass.Create(Self);
  if not (Instance is TForm) then
    raise EComponentError.Create('InstanceClass is not a TForm descendant');
    
  Result := TForm(Instance);
  TForm(Reference) := Result;
  
  // Setze als MainForm, falls noch keine vorhanden
  if not Assigned(FMainForm) then
    FMainForm := Result;
end;

// TForm Implementation  
constructor TForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'Form';
  
  // Standard-Größe für Forms
  Width := 400;
  Height := 300;
end;

procedure TForm.SetCaption(const AValue: TCaption);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    
    // Caption wird direkt über GTK4WidgetSet gesetzt
    // if Assigned(WidgetSet) and Assigned(Handle) then
    //   WidgetSet.SetFormCaption(Self, AValue);
  end;
end;

procedure TForm.CreateHandle;
begin
  if not Assigned(Handle) then
  begin
    inherited CreateHandle;
  end;
end;

procedure TForm.Show;
begin
  Visible := True;
  
  if not Assigned(Handle) then
    CreateHandle;
    
  // Show wird direkt über GTK4WidgetSet aufgerufen
  // if Assigned(WidgetSet) and Assigned(Handle) then
  //   WidgetSet.ShowForm(Self);
end;

procedure TForm.Hide;
begin
  Visible := False;
  
  // Hide wird direkt über GTK4WidgetSet aufgerufen
  // if Assigned(WidgetSet) and Assigned(Handle) then
  //   WidgetSet.HideForm(Self);
end;

procedure TForm.Close;
begin
  WriteLn('TForm.Close aufgerufen für: ', Caption);
  Hide;
  
  // Falls es die MainForm ist, beende Application
  if Application.MainForm = Self then
  begin
    WriteLn('MainForm geschlossen - beende Application');
    Application.Terminate;
  end;
end;

initialization
  // Erstelle globale Application-Instanz
  Application := TApplication.Create(nil);

finalization
  // Cleanup
  if Assigned(Application) then
  begin
    Application.Free;
    Application := nil;
  end;

end.